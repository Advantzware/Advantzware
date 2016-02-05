/* sys/inc/unappju2.i */
  
DEF VAR v-unapp-security AS LOG NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "UNAPPJU2"
      no-lock no-error.
if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "UNAPPJU2"
     sys-ctrl.module   = "JU2"
     sys-ctrl.log-fld  = no
     sys-ctrl.descrip  = "Unapprove Button Security?".
end.
v-unapp-security = sys-ctrl.log-fld.
