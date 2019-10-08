/* fgbrowse.i */

  FIND FIRST sys-ctrl NO-LOCK
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "FGBROWSE" NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "FGBROWSE"
     sys-ctrl.descrip  = "Select colors for stocked boxes on FG browser".
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
