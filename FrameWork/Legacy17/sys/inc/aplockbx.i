/*sys/inc/aplockbx.i */
def var aplockbx-log like sys-ctrl.log-fld no-undo.
def var aplockbx-cha like sys-ctrl.char-fld no-undo.
DEF VAR aplockbx-path AS cha NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "APLockBX"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = g_company
   sys-ctrl.name    = "APLockBX"
   sys-ctrl.descrip = "Create text file for AP Bank Lock Box?"
   sys-ctrl.log-fld = no
   sys-ctrl.char-fld = "Simkins".

  
end.
ASSIGN aplockbx-log = sys-ctrl.log-fld
       aplockbx-cha = sys-ctrl.char-fld.

FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = ""
         AND sys-ctrl-ship.ship-id = "" NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl-ship THEN DO:
   CREATE sys-ctrl-shipto.
   ASSIGN sys-ctrl-shipto.company = g_company
          sys-ctrl-shipto.NAME = sys-ctrl.NAME
          sys-ctrl-shipto.char-fld = "c:\tmp\".
END.
aplockbx-path = sys-ctrl-shipto.char-fld.
