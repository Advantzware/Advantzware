
DEF VAR cecunit-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR cecunit-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR cecunit-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR cecunit-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CECUNIT"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CECUNIT"
   sys-ctrl.descrip  = "Method for calculating Pallet Count (FluteMtx/AUTOCALC)?"
   sys-ctrl.log-fld  = NO
   sys-ctrl.char-fld = "FluteMtx".
END.

ASSIGN
 cecunit-log = sys-ctrl.log-fld
 cecunit-chr = sys-ctrl.char-fld
 cecunit-int = sys-ctrl.int-fld
 cecunit-dec = sys-ctrl.dec-fld.
