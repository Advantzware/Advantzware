/* sys/inc/casetag.i*/

DEF VAR casetag-log LIKE sys-ctrl.log-fld INIT NO NO-UNDO.
DEF VAR casetag-chr LIKE sys-ctrl.char-fld INIT "" NO-UNDO.
DEF VAR casetag-int LIKE sys-ctrl.int-fld INIT 0 NO-UNDO.
DEF VAR casetag-desc LIKE sys-ctrl.DESCRIP NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CASETAG"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CASETAG"
   sys-ctrl.log-fld = NO
   sys-ctrl.descrip = "Case Label Format?   Use Case Label as LoadTag?".  
END.

ASSIGN
 casetag-log = sys-ctrl.log-fld
 casetag-chr = sys-ctrl.char-fld
 casetag-desc = sys-ctrl.descrip
 casetag-int = sys-ctrl.int-fld   .

