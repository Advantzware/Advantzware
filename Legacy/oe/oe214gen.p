/*------------------------------------------------------------------------
    File        : oe214gen.p
    Purpose     : 

    Syntax      :

    Description : ASN program to create edi asn records (edshtran ...)

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/***************************************************************************/

{edi/sharedv.i "new"}
DEFINE INPUT PARAMETER bol_rec             AS RECID   NO-UNDO.
DEFINE INPUT PARAMETER original_print      AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER force_asn           AS LOGICAL INITIAL FALSE.

DEFINE VARIABLE run_program AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutFolder  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCnt        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLen        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cResult     AS CHARACTER NO-UNDO.


FIND oe-bolh WHERE RECID(oe-bolh) = bol_rec NO-LOCK NO-ERROR.
IF NOT AVAILABLE oe-bolh THEN RETURN.

ASSIGN
    ws_process_rec = RECID(oe-bolh)
    .

FUNCTION fDtTm RETURNS CHARACTER 
    ( ipdDate AS DATE ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE cResult        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTime          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTimeFormatted AS CHARACTER NO-UNDO.
        
    cTime =  STRING(TIME, "hh:mm:ss").
    cTimeFormatted = SUBSTRING(cTime, 1, 2) + substring(cTime, 4, 2) + substring(cTime, 7, 2).
    cResult = STRING(YEAR(ipdDate)) + STRING(MONTH(ipdDate)) + STRING(DAY(ipdDate))
        + cTimeFormatted.
    RETURN cResult.

        
END FUNCTION.  
EDI-FIND-LOOP:
DO:
    FIND FIRST edmast WHERE edmast.cust EQ oe-bolh.cust-no NO-LOCK NO-ERROR.

    IF NOT AVAILABLE edmast THEN  LEAVE EDI-FIND-LOOP.
    ws_partner = edmast.partner.

    FIND edco WHERE edco.company = oe-bolh.company NO-LOCK NO-ERROR.
    ASSIGN 
        ws_edmast_rec = RECID(edmast) .
    FIND FIRST edcode WHERE edcode.partner EQ edmast.partner
        AND edcode.setid = "214" NO-LOCK NO-ERROR.

    IF NOT AVAILABLE edcode THEN 
        LEAVE EDI-FIND-LOOP.
    /* 214 process (shipment advice) */
    IF original_print OR force_asn THEN
    EDI-214-LOOP: /* 9601 CAH: No ASN on reprinted oe-bolh */
    DO:
        ws_setid = "214".
 
        DO:
            ASSIGN
                cOutFolder = edcode.path-out
                iLen       = LENGTH(cOutFolder)
                .
      
            IF NOT (SUBSTRING(cOutFolder, iLen, 1) EQ "/" OR SUBSTRING(cOutFolder, iLen, 1) EQ "\") THEN 
                cOutFolder = cOutFolder + "\".  
            ASSIGN 
                iCnt          = iCnt + 1
                ws_edcode_rec = RECID(edcode)
                cFile         = cOutFolder + "TAR214" + fDtTm(TODAY) + STRING(iCnt) + ".json"
                .
            /* Generate File */
            RUN ed/asi/target214.p (
                INPUT RECID(oe-bolh), 
                INPUT cFile,
                OUTPUT cResult
                ).   
            /* Transmit */
            IF cResult EQ "" THEN 
                RUN cxml/target.p (
                                   INPUT cFile,
                                   INPUT "",
                                   OUTPUT cResult
                                   ).
        END.
    END. /* end of EDI-214-LOOP */
END. /* end of EDI-FIND-LOOP */
