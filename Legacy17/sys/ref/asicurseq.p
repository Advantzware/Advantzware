DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcSeqName AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiNextVal AS INTEGER     NO-UNDO.
DEF VAR cCompSuffix AS CHAR NO-UNDO.

IF NOT AVAIL company THEN
  FIND company WHERE company.company EQ ipcCompany 
    NO-LOCK NO-ERROR. 
IF NOT AVAIL company THEN
    RETURN ERROR "Company not found".
cCompSuffix = company.spare-char-1.

/* Using same sequence for all companies */
IF ipcSeqName EQ "rm_rcpt_seq" THEN
    cCompSuffix = "".

opiNextVal = 
    DYNAMIC-CURRENT-VALUE(ipcSeqName + cCompSuffix, "ASI") NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN ERROR "Could not obtain next number".
