/*sys/inc/f16to32.i*/
DEF VAR v-16-or-32 AS DEC INIT 0.16 NO-UNDO.
DEF VAR li-16-32 AS DEC INIT 16 NO-UNDO.
DEF VAR v-cecscrn-char AS CHAR NO-UNDO.
DEF VAR v-cecscrn-dec AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CECSCRN"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CECSCRN"
   sys-ctrl.log-fld  = NO
   sys-ctrl.char-fld = "16th's"
   sys-ctrl.descrip  = "Show estimate dimension in 16th's or 32nd's".   
END.

v-cecscrn-char = sys-ctrl.char-fld.

IF sys-ctrl.char-fld EQ "16th's" THEN.

ELSE IF sys-ctrl.char-fld EQ "32nd's" THEN
  ASSIGN
   K_FRAC     = 3.125
   v-16-or-32 = 0.32
   li-16-32   = 32.
ELSE
   IF sys-ctrl.char-fld EQ "Decimal" THEN
      ASSIGN
         K_FRAC     = 1
         v-16-or-32 = 1
         li-16-32   = 1
         v-cecscrn-dec = sys-ctrl.log-fld.
