
/*------------------------------------------------------------------------
    File        : vendorTagParse.p
    Purpose     : 

    Syntax      :

    Description : Parse vendor tag

    Author(s)   : 
    Created     : Wed Nov 17 12:34:03 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcTagno     AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER opiPOnumber AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opiPOline   AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opiQuantity AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opcErrorStr AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcErrtype  AS CHARACTER NO-UNDO. //will be retried from the sys control Ignore,Warning and Error
/*DEFINE OUTPUT PARAMETER opiUOM      AS CHARACTER NO-UNDO.*/

DEFINE VARIABLE cParseMask     AS CHARACTER NO-UNDO INIT "PPPPPPLLLQQQQQ". /*will be retrieved from sys-ctrl later*/
DEFINE VARIABLE cParseMasklist AS CHARACTER NO-UNDO INIT "P,L,Q".  

DEFINE TEMP-TABLE ttParseVal NO-UNDO
    FIELD cValueFor  AS CHARACTER
    FIELD iStartPos  AS INTEGER
    FIELD iStrlength AS INTEGER .
  
DEFINE VARIABLE iIncrement AS INTEGER   NO-UNDO.
DEFINE VARIABLE ctemp      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPsize     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLsize     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iQsize     AS INTEGER   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

opcErrtype = "I".

IF LENGTH(cParseMask) <> LENGTH(ipcTagno) AND opcErrtype <> "I" THEN 
DO:
        opcErrorStr = "Vendor tag length must be " + STRING(LENGTH(cParseMask)).
        IF opcErrtype = "Error" THEN 
            RETURN.
END.

DO iIncrement = 1 TO LENGTH(cParseMask ):
    ctemp = SUBSTRING(cParseMask ,iIncrement,1).
    CASE ctemp:
        WHEN "P" THEN
            iPsize = iPsize + 1.
        WHEN "L" THEN
            iLsize = iLsize + 1.
        WHEN "Q" THEN 
            iQsize = iQsize + 1.
    END CASE.
END.

DO iIncrement = 1 TO NUM-ENTRIES(cParseMaskList):
    CREATE ttParseVal.
    ASSIGN 
        ttParseVal.cValueFor = ENTRY(iIncrement,cParseMaskList)
        ttParseVal.iStartPos = INDEX(cParseMask,ENTRY(iIncrement,cParseMaskList)).
           
    IF ttParseVal.cValueFor = "P" THEN 
        ttParseVal.iStrlength = iPsize.
    ELSE IF ttParseVal.cValueFor = "L" THEN
        ttParseVal.iStrlength = iLsize.
    ELSE IF ttParseVal.cValueFor = "Q" THEN
        ttParseVal.iStrlength = iQsize.
END.

FOR EACH ttParseVal:
    CASE ttParseVal.cValueFor:
        WHEN "P" THEN 
            opiPOnumber = INT(SUBSTRING(ipcTagno,ttParseVal.iStartPos,ttParseVal.iStrlength)).
        WHEN "L" THEN 
            opiPOline   = INT(SUBSTRING(ipcTagno,ttParseVal.iStartPos,ttParseVal.iStrlength)).
        WHEN "Q" THEN
            opiQuantity = INT(SUBSTRING(ipcTagno,ttParseVal.iStartPos,ttParseVal.iStrlength)).
    END CASE.
END.
