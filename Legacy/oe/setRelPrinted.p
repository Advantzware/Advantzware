/* oe/setRelPrinted.p */
DEFINE INPUT  PARAMETER iprOeRelh AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER iplPrintStatus AS LOGICAL     NO-UNDO.

FIND oe-relh WHERE ROWID(oe-relh) EQ iprOeRelh EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAIL oe-relh THEN
  RETURN.


ASSIGN oe-relh.printed = iplPrintStatus.
IF  NOT (iplPrintStatus EQ YES AND oe-relh.spare-char-3 GT "") THEN 
       oe-relh.spare-char-3 = (IF iplPrintStatus EQ YES THEN 
                                 USERID("NOSWEAT")
                               ELSE
                                 "").
RELEASE oe-relh.
