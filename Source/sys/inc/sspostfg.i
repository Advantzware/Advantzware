  
DEF VAR SSPostFG-log  LIKE sys-ctrl.log-fld   NO-UNDO.
DEF VAR SSPostFG-Char LIKE sys-ctrl.char-fld  NO-UNDO.
DEF VAR SSPostFG-int  LIKE sys-ctrl.int-fld   NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SSPostFG"
    NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "SSPostFG"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Sharp Shooter Finish Goods Post".
END.

ASSIGN SSPostFG-log  = sys-ctrl.log-fld
       SSPostFG-char = sys-ctrl.char-fld
       SSPostFG-int  = sys-ctrl.int-fld.
 
