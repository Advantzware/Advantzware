
DEF INPUT  PARAM ip-company AS CHAR NO-UNDO.
DEF INPUT  PARAM ip-date AS DATE NO-UNDO.
DEF OUTPUT PARAM op-period LIKE period.pnum NO-UNDO.

DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR lv-date-format AS CHAR INIT "99/99/9999" NO-UNDO.

{sys/inc/var.i NEW SHARED}


cocode = ip-company.

DO TRANSACTION:
  {sys/inc/postdate.i}
END.
  
FIND FIRST period                   
    WHERE period.company EQ ip-company
      AND period.pst     LE ip-date
      AND period.pend    GE ip-date
    NO-LOCK NO-ERROR.

IF NOT AVAIL period THEN
DO:
  IF ip-date NE ? THEN
     lv-msg = "No defined period exists for " + TRIM(STRING(ip-date,lv-date-format)).
  ELSE
     lv-msg = "No defined period exists for ?".
END.

ELSE IF NOT period.pstat THEN
   lv-msg = "Period for " + TRIM(STRING(ip-date,lv-date-format)) + " is already closed".

ELSE IF postdate-dat NE ? AND ip-date LE postdate-dat THEN
   lv-msg = "Transaction Date must be after " + TRIM(STRING(postdate-dat,lv-date-format)).

IF lv-msg NE "" THEN DO:
  MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

ELSE op-period = period.pnum.
