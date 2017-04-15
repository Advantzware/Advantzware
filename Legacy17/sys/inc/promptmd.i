/*sys/inc/promptmd.i */
def var modem-log like sys-ctrl.log-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "PROMPTMD"
    no-lock no-error.
if not avail sys-ctrl then DO TRANSACTION:
  create sys-ctrl.
  assign
   sys-ctrl.company = g_company
   sys-ctrl.name    = "PROMPTMD"
   sys-ctrl.descrip = "Prompt where modem is installed?"
   sys-ctrl.log-fld = NO  .
/*
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
*/      
end.
ASSIGN modem-log = sys-ctrl.log-fld
       .
