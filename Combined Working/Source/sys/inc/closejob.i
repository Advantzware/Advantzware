
DEF VAR v-close-job AS INT.

DEF BUFFER closejob-tbl FOR sys-ctrl.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CLOSEJOB"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CLOSEJOB"
   sys-ctrl.descrip  = "When/how to Close Job? (Manually, OrdClose, FGPost)"
   sys-ctrl.log-fld  = NO
   sys-ctrl.char-fld = "Manually".
END.

FIND closejob-tbl WHERE ROWID(closejob-tbl) EQ ROWID(sys-ctrl)
    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

IF AVAIL closejob-tbl THEN DO:
  IF closejob-tbl.int-fld  GT 1        THEN closejob-tbl.int-fld = 1.
  IF closejob-tbl.char-fld NE "FGPost" THEN closejob-tbl.int-fld = 0.

  closejob-tbl.descrip = "char: Close Job?, " +
                         "log: Calc Prod Waste?, " + 
                         "int: Ignore Stat? FGPost".

  FIND CURRENT closejob-tbl NO-LOCK NO-ERROR.
END.

FIND CURRENT sys-ctrl NO-LOCK NO-ERROR.

v-close-job = int(sys-ctrl.char-fld eq "{1}") + sys-ctrl.int-fld.
