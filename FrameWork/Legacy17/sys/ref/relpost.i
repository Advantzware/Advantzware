
DEF VAR relpost-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR relpost-log LIKE sys-ctrl.log-fld  NO-UNDO.
DEF VAR relpost-int LIKE sys-ctrl.int-fld  NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "RELPOST"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "RELPOST"
   sys-ctrl.descrip  = "Release Posting to Create?"
   sys-ctrl.char-fld = "Nothing".
  MESSAGE sys-ctrl.descrip "(Invoice,BOL)"
      UPDATE sys-ctrl.char-fld.
END.
ASSIGN
 relpost-chr = sys-ctrl.char-fld
 relpost-log = sys-ctrl.log-fld
 relpost-int = sys-ctrl.int-fld.
