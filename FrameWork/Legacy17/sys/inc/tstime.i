/*sys/inc/tstime.i */
def var tstime-log like sys-ctrl.log-fld no-undo.
def var tstime-cha like sys-ctrl.char-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "TSTIME"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = g_company
   sys-ctrl.name    = "TSTIME"
   sys-ctrl.descrip = "Touch-Screen Time Source - Server or Workstation"
   sys-ctrl.char-fld = "Server"   .

/*  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.*/
end.
ASSIGN tstime-log = sys-ctrl.log-fld
       tstime-cha = sys-ctrl.char-fld
       .
