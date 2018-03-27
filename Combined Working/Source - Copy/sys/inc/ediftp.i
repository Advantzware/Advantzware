/* sys/inc/ediftp.i} */

DEF VAR ediftp-log LIKE sys-ctrl.log-fld   INIT NO NO-UNDO.
DEF VAR ediftp-cha LIKE sys-ctrl.char-fld          NO-UNDO.
DEF VAR ediftp-int LIKE sys-ctrl.int-fld          NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "EDIFTP"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "EDIFTP"
   sys-ctrl.descrip  = "EDI ftp execution?"
   sys-ctrl.char-fld = "".
END.
ASSIGN
 ediftp-log = sys-ctrl.log-fld
 ediftp-cha = sys-ctrl.char-fld
 ediftp-int = sys-ctrl.int-fld.
