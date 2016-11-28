/*sys/inc/tslogin.i */
def var tslogin-log like sys-ctrl.log-fld no-undo.
def var tslogin-cha like sys-ctrl.char-fld no-undo.

find first sys-ctrl
    where /*sys-ctrl.company eq g_company
      and */ sys-ctrl.name    eq "TSLOGIN"
    no-lock no-error.
if not avail sys-ctrl then do:
  FIND FIRST company NO-LOCK NO-ERROR.
  create sys-ctrl.
  assign
   sys-ctrl.company = IF AVAIL company THEN company.company ELSE "" /*g_company*/
   sys-ctrl.name    = "TSLOGIN"
   sys-ctrl.descrip = "Prompt Login ID/Password for Touch-Screen"
   sys-ctrl.log-fld = no  .

 
end.
ASSIGN tslogin-log = sys-ctrl.log-fld
       .
