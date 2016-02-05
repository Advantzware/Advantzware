/* tscomplt.i */

  FIND FIRST sys-ctrl NO-LOCK
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "TSCOMPLT" NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "TSCOMPLT"
     sys-ctrl.descrip  = "Close TS Jobs via Completed Flag".
    MESSAGE sys-ctrl.descrip VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE sys-ctrl.log-fld.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
