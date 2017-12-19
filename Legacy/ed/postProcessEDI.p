DEFINE INPUT  PARAMETER ipcSetId AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcInPath AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPartner AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcOutPath AS CHARACTER NO-UNDO.


INPUT from VALUE(ipcInPath).

DEFINE VARIABLE inln           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutLine       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDelimPos      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iElemNum       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cElem          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lISASent       AS LOG       NO-UNDO.
DEFINE VARIABLE iISAControlNum AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSenderDuns    AS CHARACTER NO-UNDO INIT "PREMIER        ". 
DEFINE VARIABLE cISASeg        AS CHARACTER.
DEFINE VARIABLE cGSSeg         AS CHARACTER.
DEFINE VARIABLE cEleDelim      AS CHARACTER INIT "*".
DEFINE VARIABLE cSTSeg         AS CHARACTER.
DEFINE VARIABLE iSTControl     AS INTEGER.
DEFINE VARIABLE iSegmentcount  AS INTEGER.
DEFINE VARIABLE iCurrentElem   AS INTEGER.
DEFINE VARIABLE cSegment       AS CHARACTER.
DEFINE VARIABLE cSetID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQualifier AS CHARACTER NO-UNDO.

lISASent = NO.
iISAControlNum = 1.
iSTControl = 1.

FIND FIRST EDMast NO-LOCK WHERE EDMast.Partner EQ ipcPartner NO-ERROR.
IF NOT AVAILABLE EDMast THEN 
  RETURN.
FIND FIRST edPartnerGrp EXCLUSIVE-LOCK  
   WHERE edPartnerGrp.PartnerGrp EQ EDMast.partnerGrp
   NO-ERROR.
IF NOT AVAILABLE edPartnerGrp THEN  
  RETURN. 
ASSIGN iIsaControlNum = edPartnerGrp.isa + 1
       edPartnerGrp.isa = iISAControlNum
       edPartnerGrp.gs = iISAControlNum
       .
RELEASE edPartnerGrp.
FIND LAST eddoc EXCLUSIVE-LOCK  
    WHERE eddoc.partner EQ ipcPartner
    AND eddoc.setID EQ ipcSetId
    AND eddoc.isa NE ?
    AND eddoc.gs NE ?
    USE-INDEX bySAIStat NO-ERROR.
IF AVAILABLE eddoc THEN 
    ASSIGN     
        eddoc.isa      = iISAControlNum
        eddoc.gs       = iISAControlNum
        eddoc.st       = iSTControl
        .
RELEASE eddoc.

/*DEFINE VARIABLE cOutFileName AS CHARACTER NO-UNDO.
cOutFileName = "c:\temp\o810-" + string(time) + ".edi". */
DEFINE STREAM sOutput.
OUTPUT stream sOutput to value(ipcOutPath). /* c:\temp\o810-clean.txt. */
FIND FIRST EDMast NO-LOCK WHERE EDMast.Partner EQ ipcPartner NO-ERROR.
IF NOT AVAILABLE EDMast THEN 
    RETURN. 
    
GET-NEXT-LINE:
REPEAT:
    inln = "".
    cOutLine = "".
    IMPORT DELIMITER "|" inln.
  
    cSegment = "".
    iCurrentElem = 0.
    cQualifier = "".
    DO iDelimPos = 1 TO NUM-ENTRIES(inln, "*"):
        cElem = "".
    
        IF iDelimPos = 1 OR
            iDelimPos = 2 OR
            iDelimPos = 4 OR 
            iDelimPos = 5 THEN 
            NEXT.
            
        /* Lines start with a sequence number and setid, so start with 3 */
        IF iDelimPos GT 2 THEN 
        DO:
    
            cElem = TRIM(ENTRY(iDelimPos, inln, "*")).
            IF cElem BEGINS "00000000000" THEN 
                cElem = "GS".
        
            IF cSegment EQ "" THEN 
                cSegment = cElem.
            ELSE
                iCurrentElem = iCurrentElem + 1.
        
            /* correct position */
            IF cSegment EQ "IT1" AND iCurrentElem EQ 5 THEN 
                cElem = "*" + cElem.
                
            /* Remove leading zero's */
            IF cSegment EQ "IT1" AND iCurrentElem EQ 2 THEN 
                cElem = STRING(INTEGER(cElem)).
                
            /* Convert from implied decimal to decimal format */    
            IF cSegment EQ "IT1" AND iCurrentElem EQ 4 THEN 
                ASSIGN
                    cElem = SUBSTRING(cElem, 2, 13) + "." + 
                  substring(cElem, 15, 4)
                    cElem = STRING(DECIMAL(cElem)).
            IF cSegment EQ "N1" AND iCurrentElem EQ 1 THEN 
              cQualifier = cElem.
              
            /* First digit is the length */
            IF cSegment EQ "TDS" AND iCurrentElem EQ 1 THEN
                ASSIGN
                    cElem = SUBSTRING(cElem, 2)
                    cElem = STRING(INTEGER(cElem))
                    .
            FIND first ediPartnerSegment NO-LOCK  
                WHERE ediPartnerSegment.partnerGrp EQ EDMast.partnerGrp
                  AND ediPartnerSegment.segmentCode EQ cSegment
                NO-ERROR.
            IF NOT AVAILABLE ediPartnerSegment THEN 
                NEXT GET-NEXT-LINE.
                
            IF cQualifier GT "" THEN 
                FIND FIRST ediPartnerSegment NO-LOCK  
                    WHERE ediPartnerSegment.partnerGrp  EQ EDMast.partnerGrp             
                    AND ediPartnerSegment.segmentCode EQ cSegment
                    AND ediPartnerSegment.elementNum  EQ iCurrentElem
                    AND ediPartnerSegment.qual EQ cQualifier
                    NO-ERROR
                    .     
            /* Either the qualifier matches or the qualifier is blank */  
            IF NOT AVAILABLE ediPartnerSegment THEN 
            FIND FIRST ediPartnerSegment NO-LOCK  
                WHERE ediPartnerSegment.partnerGrp  EQ EDMast.partnerGrp             
                  AND ediPartnerSegment.segmentCode EQ cSegment
                  AND ediPartnerSegment.elementNum  EQ iCurrentElem
                  AND ediPartnerSegment.qual EQ ""
                NO-ERROR
                .       
            IF NOT AVAILABLE ediPartnerSegment AND cSegment NE "it1" 
              and iCurrentElem ne 0 THEN do:
              
              cElem = "".
              
              end.

            IF NOT (cSegment EQ "TDS" AND iCurrentElem GT 1) THEN
                cOutLine = cOutLine + "*" + cElem. 
         
        END.
    
    END. /* examine each element of segment */


        

    
    cOutLine = TRIM(cOutLine, "*") + "~~".
 
    IF NOT lISASEnt THEN 
    DO:
        cISASeg = "ISA" + cEleDelim + 
            "00"  + cEleDelim + 
            fill(" ", 10) + cEleDelim  +
            "00"  + cEleDelim + 
            fill(" ", 10) + cEleDelim  +
            "ZZ"  + cEleDelim + 
            cSenderDuns + cEleDelim + 
            "ZZ"  + cEleDelim + 
            "AMAZON         " + cEleDelim +               
            substring(STRING(YEAR(TODAY), "9999"),3, 2) + 
            string(MONTH(TODAY), "99") + 
            string(DAY(TODAY), "99") + cEleDelim +               
            substring(STRING(TIME, "hh:mm"), 1, 2) 
            + substring(STRING(TIME, "hh:mm"), 4, 2) 
            + cEleDelim +                
            "U" + cEleDelim +               
            "00401" + cEleDelim +               
            string(iISAControlNum, "999999999") + cEleDelim +               
            "0" + cEleDelim +               
            "I" + cEleDelim +           
            ">" +
            "~~".
              
        PUT STREAM sOutput UNFORMATTED cISASeg SKIP.
        cGsSeg = "GS" + cEleDelim + 
            "IN" + cEleDelim + 
            trim(cSenderDuns) + cEleDelim + 
            "AMAZON" + cEleDelim + 
            substring(STRING(YEAR(TODAY), "9999"),1, 4) + 
            string(MONTH(TODAY), "99") + 
            string(DAY(TODAY), "99") + cEleDelim +               
            substring(STRING(TIME, "hh:mm"), 1, 2) 
            + substring(STRING(TIME, "hh:mm"), 4, 2) 
            + cEleDelim + 
            string(iISAControlNum) + cEleDelim +
            "X" + cEleDelim + 
            "004010" + 
            "~~"
            . 
        PUT STREAM sOutput UNFORMATTED cGsSeg SKIP.
    
        cStSeg = "ST" + cEleDelim + 
            "810" + cEleDelim + 
            string(iStControl, "9999") +  
            "~~"
            . 
    
        PUT STREAM sOutput UNFORMATTED cStSeg SKIP.
        lISASent = TRUE.

    END.
     /* ELSE  */
    DO:
        PUT STREAM sOutput UNFORMATTED cOutLine SKIP.  
        iSegmentCount = iSegmentCount + 1.
    END.
END.

IF lISASent THEN 
DO:
    iSegmentCount = iSegmentCount + 2.
    PUT STREAM sOutput UNFORMATTED 
        "SE" + cEleDelim +
        string(iSegmentCount) + cEleDelim
        STRING(iSTControl, "9999") 
        + "~~"
        SKIP
        .
  

    PUT STREAM sOutput UNFORMATTED 
        "GE" + cEleDelim +
        "1" + cEleDelim
        STRING(iISAControlNum) 
        + "~~"
        SKIP
        .
    PUT STREAM sOutput UNFORMATTED 
        "IEA" + cEleDelim +
        "1" + cEleDelim
        STRING(iISAControlNum, "999999999") 
        + "~~"
        SKIP
        .                         
END.
OUTPUT stream sOutput close.
DOS SILENT notepad value(ipcOutPath /* cOutFileName */). 
