  
DEF VAR SSPostFGConTags-log  LIKE sys-ctrl.log-fld   NO-UNDO.
DEF VAR SSPostFGConTags-Char LIKE sys-ctrl.char-fld  NO-UNDO.
DEF VAR SSPostFGConTags-int  LIKE sys-ctrl.int-fld   NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SSPostFGConTags"
    NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "SSPostFGConTags"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Sharp Shooter FG Warehouse Trans Transfer Post".
END.

ASSIGN SSPostFGConTags-log  = sys-ctrl.log-fld
       SSPostFGConTags-char = sys-ctrl.char-fld
       SSPostFGConTags-int  = sys-ctrl.int-fld.
 
