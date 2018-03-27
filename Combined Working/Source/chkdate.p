DEF VAR lv-date AS DATE NO-UNDO.

IF CONNECTED("ASI") THEN
DO:
  FIND FIRST asi.expiration NO-LOCK NO-ERROR.
  IF NOT AVAIL asi.expiration THEN DO TRANSACTION:
    CREATE asi.expiration.
    ASSIGN
     asi.expiration.this-a-trial = YES
     asi.expiration.expire-date  = 12/31/9999.
  END.

  IF USERID(ldbname(1)) EQ "asi" AND SEARCH("fixexdat") NE ? THEN DO TRANSACTION:
    FIND CURRENT asi.expiration.
    MESSAGE "New Date:" UPDATE asi.expiration.expire-date.
    QUIT.
  END.

  IF asi.expiration.this-a-trial AND TODAY GT asi.expiration.expire-date THEN DO:
    FIND CURRENT asi.expiration.
    DO TRANSACTION:
      ASSIGN
       lv-date = asi.expiration.expire-date
       asi.expiration.expire-date = 01/01/0001.
    END.
    FIND CURRENT asi.expiration NO-LOCK.

    MESSAGE "*** PROCEDURE WAS TERMINATED ABNORMALLY."
            "DATA CORRUPTION HAS INTERRUPTED THE PROCESS,"
            "PLEASE CALL YOUR SOFTWARE VENDOR ****"
            "(" + STRING((MONTH(lv-date) * 100) + DAY(lv-date),"9999") + ")"
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
  END.
END.
