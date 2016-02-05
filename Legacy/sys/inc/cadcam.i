
DEF VAR cadcam-log LIKE sys-ctrl.log-fld INIT NO NO-UNDO.
DEF VAR cadcam-chr LIKE sys-ctrl.char-fld INIT "" NO-UNDO.
DEF VAR cadcam-int LIKE sys-ctrl.int-fld INIT 0 NO-UNDO.
DEF VAR cadcam-dec LIKE sys-ctrl.dec-fld INIT 0 NO-UNDO.

DEF BUFFER cadcam-sys-ctrl FOR sys-ctrl.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CADCAM"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CADCAM"
   sys-ctrl.log-fld = NO
   sys-ctrl.descrip = "Cad Cam Vendor Integrated into Estimating".

  FIND FIRST cadcam-sys-ctrl
      WHERE cadcam-sys-ctrl.company EQ cocode
        AND cadcam-sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.

  IF AVAIL cadcam-sys-ctrl THEN
    sys-ctrl.int-fld = LOOKUP(cadcam-sys-ctrl.char-fld,"Foldware,Corrware,Both").

  MESSAGE TRIM(sys-ctrl.descrip)
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
END.

ASSIGN
 cadcam-log = sys-ctrl.log-fld
 cadcam-chr = sys-ctrl.char-fld.

IF cadcam-log THEN cadcam-int = sys-ctrl.int-fld.
