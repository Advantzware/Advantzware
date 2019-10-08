DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcSeqName AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiNextVal AS INTEGER     NO-UNDO.
DEF VAR cCompSuffix AS CHAR NO-UNDO.
DEF VAR liSeq AS INT NO-UNDO.

IF NOT AVAIL company THEN
  FIND company WHERE company.company EQ ipcCompany 
    NO-LOCK NO-ERROR. 
IF NOT AVAIL company THEN
    RETURN ERROR "Company not found".
cCompSuffix = company.spare-char-1.
/* Using same sequence for all companies */
IF ipcSeqName EQ "rm_rcpt_seq" THEN DO:
      cCompSuffix = "".
      /* Code is included just in case some program is not using the sequence*/
      find last rm-rctd use-index rm-rctd NO-LOCK NO-ERROR.
      IF AVAIL rm-rctd THEN
        liSeq = rm-rctd.r-no.
      find last rm-rcpth use-index r-no NO-LOCK NO-ERROR.

      IF AVAIL rm-rcpth AND rm-rcpth.r-no GT liSeq THEN
        liSeq = rm-rcpth.r-no.

      IF current-value(rm_rcpt_seq) LT liSeq THEN
      current-value(rm_rcpt_seq) = liSeq.
END.


opiNextVal = DYNAMIC-NEXT-VALUE(ipcSeqName + cCompSuffix, "ASI") NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN ERROR "Could not obtain next number".
