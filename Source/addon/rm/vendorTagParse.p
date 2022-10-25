
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
DEFINE OUTPUT PARAMETER opcAddInfo  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplError    AS LOGICAL NO-UNDO. 


DEFINE VARIABLE cParseMask     AS CHARACTER NO-UNDO INIT "PPPPPPLLLQQQQQ". /*will be retrieved from sys-ctrl later*/
DEFINE VARIABLE cParseMasklist AS CHARACTER NO-UNDO INIT "P,L,Q".  

DEFINE TEMP-TABLE ttParseVal NO-UNDO
    FIELD cValueFor  AS CHARACTER
    FIELD iStartPos  AS INTEGER
    FIELD iStrlength AS INTEGER .
  
DEFINE VARIABLE iIncrement  AS INTEGER   NO-UNDO.
DEFINE VARIABLE ctemp       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPsize      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLsize      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iQsize      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iXsize      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPromptType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorNo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany    AS CHARACTER NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).

FIND FIRST po-ord NO-LOCK
     WHERE po-ord.company EQ cCompany 
       AND (po-ord.po-no EQ INT(SUBSTR(ipcTagno,1,6)) 
        OR  po-ord.po-no EQ INT(SUBSTR(ipcTagno,1,8)))
     NO-ERROR.
     
cVendorNo = IF AVAIL po-ord THEN po-ord.vend-no ELSE "".

RUN spGetSettingByNameAndVendor("SSScanVendorTagMask", cVendorNo, OUTPUT cParseMask) .
RUN spGetSettingByNameAndVendor("SSScanVendorTagPrompt", cVendorNo, OUTPUT cPromptType) .
      
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


IF LENGTH(cParseMask) <> LENGTH(ipcTagno) AND cPromptType BEGINS "Block if Length of Scan not equal to Length" THEN 
DO:
        opcMessage = "Vendor tag length must be " + STRING(LENGTH(cParseMask)).
        oplError = YES. 
        RETURN.
END.
ELSE IF LENGTH(cParseMask) <> LENGTH(ipcTagno) AND cPromptType BEGINS "Warn if Length of Scan not equal to Length" THEN
DO:
        opcMessage = "Vendor tag length must be " + STRING(LENGTH(cParseMask)).            
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
        WHEN "X" THEN 
            iXsize = iXsize + 1.    
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
    ELSE IF ttParseVal.cValueFor = "X" THEN
        ttParseVal.iStrlength = iXsize.    
END.

FOR EACH ttParseVal:
    CASE ttParseVal.cValueFor:
        WHEN "P" THEN 
            opiPOnumber = INT(SUBSTRING(ipcTagno,ttParseVal.iStartPos,ttParseVal.iStrlength)) NO-ERROR.
        WHEN "L" THEN 
            opiPOline   = INT(SUBSTRING(ipcTagno,ttParseVal.iStartPos,ttParseVal.iStrlength)) NO-ERROR.
        WHEN "Q" THEN
            opiQuantity = INT(SUBSTRING(ipcTagno,ttParseVal.iStartPos,ttParseVal.iStrlength)) NO-ERROR.
        WHEN "X" THEN
            opcAddInfo = SUBSTRING(ipcTagno,ttParseVal.iStartPos,ttParseVal.iStrlength).    
    END CASE.
END.
