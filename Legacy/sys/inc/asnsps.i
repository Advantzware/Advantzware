/* sys/inc/asnsps.i*/

 DEF VAR ASNSPS-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR ASNSPS-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR ASNSPS-int LIKE sys-ctrl.int-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "ASNSPS"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "ASNSPS"
   sys-ctrl.log-fld  = NO
   sys-ctrl.char-fld = "c:\temp\"
   sys-ctrl.descrip  = "Generate SPS ASN?".
 
END.
ASSIGN
 ASNSPS-log = sys-ctrl.log-fld
 ASNSPS-cha = sys-ctrl.char-fld
 ASNSPS-int = sys-ctrl.int-fld .

