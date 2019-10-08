/* sys/inc/SSMOVEFG.i*/  
DEF VAR SSMoveFG-log  LIKE sys-ctrl.log-fld   NO-UNDO.
def var SSMoveFg-cha like sys-ctrl.char-fld no-undo.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
     AND sys-ctrl.name    EQ "SSMoveFG"
    NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "SSMoveFG"
   sys-ctrl.log-fld  = NO
   sys-ctrl.char-fld = "FG Lot#"
   sys-ctrl.descrip  = "Sharp Shooter Move Finish Goods Post".
END.

assign
 SSMoveFG-log = sys-ctrl.log-fld
 SSMoveFG-cha = sys-ctrl.char-fld.
 
