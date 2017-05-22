/* sys/ref/poStatus.i */
DEFINE VARIABLE v-postatus-cha  like sys-ctrl.char-fld no-undo.
DEFINE VARIABLE v-postatus-log LIKE sys-ctrl.log-fld NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "POSTATUS"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "POSTATUS"
   sys-ctrl.descrip  = "Set initial status of POs"
   sys-ctrl.char-fld = ""
   sys-ctrl.log-fld  = no.
  
  MESSAGE "Set initial status of POs?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

assign
 v-postatus-cha  = sys-ctrl.char-fld
 v-postatus-log = sys-ctrl.log-fld.
