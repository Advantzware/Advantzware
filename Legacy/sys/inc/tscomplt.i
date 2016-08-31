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
   
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
