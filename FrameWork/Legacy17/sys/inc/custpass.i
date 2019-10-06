/* sys/inc/custpass.i */
  
DEF VAR v-custpass AS LOG NO-UNDO.
DEF VAR v-custpass-char AS cha NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "CUSTPASS"
      no-lock no-error.
if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "CUSTPASS"
     sys-ctrl.log-fld  = no
     sys-ctrl.descrip  = "Customer File Password?  Update when Updating Customer File?"
     sys-ctrl.char-fld = "yorkie"   .
end.
ASSIGN v-custpass = sys-ctrl.log-fld
       v-custpass-char = sys-ctrl.char-fld.
