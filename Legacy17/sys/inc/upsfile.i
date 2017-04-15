/* upsfile.i */

  FIND FIRST sys-ctrl NO-LOCK
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name EQ 'UPSFILE' NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = cocode
     sys-ctrl.name     = 'UPSFILE'
     sys-ctrl.module   = 'OS5'
     sys-ctrl.descrip  = 'UPS File Location and Name'
     sys-ctrl.char-fld = '.\upsfile.cvs'.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
