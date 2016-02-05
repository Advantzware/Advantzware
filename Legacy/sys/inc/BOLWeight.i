
def var BOLWt-log like sys-ctrl.log-fld no-undo.
def var BOLWt-cha like sys-ctrl.char-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq g_company 
      and sys-ctrl.name    eq "BOLWeight"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = g_company 
   sys-ctrl.name     = "BOLWeight"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Calculate BOL Weight".
     
end.
ASSIGN BOLWt-log  = sys-ctrl.log-fld
       BOLWt-cha  = sys-ctrl.char-fld.
