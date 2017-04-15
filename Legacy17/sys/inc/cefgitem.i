/* sys/inc/cefgitem.i */
  
DEF VAR v-cefgitem-log AS LOG NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "CEFGITEM"
      no-lock no-error.
if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "CEFGITEM"
     sys-ctrl.log-fld  = no
     sys-ctrl.char-fld = "BLUE"
     sys-ctrl.module = "OU1"
     sys-ctrl.descrip  = "Change Color of FG Item Code in Estimating".
end.

v-cefgitem-log = sys-ctrl.log-fld.
