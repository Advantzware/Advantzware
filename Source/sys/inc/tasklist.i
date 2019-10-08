/* sys/inc/Tasklist.i*/

def var tasklist-log like sys-ctrl.log-fld no-undo.
def var tasklist-cha like sys-ctrl.char-fld no-undo.
def var tasklist-int like sys-ctrl.int-fld no-undo.
def var tasklist-dec like sys-ctrl.dec-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "TaskList"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = g_company
   sys-ctrl.name     = "TaskList"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "Advantzware Task List".
 
end.
assign
 tasklist-log = sys-ctrl.log-fld
 tasklist-cha = sys-ctrl.char-fld
 tasklist-int = sys-ctrl.int-fld
 tasklist-dec = sys-ctrl.dec-fld.   
    

