/* sys/inc/tsfinish.i */
  
DEF VAR v-tsfinish AS LOG NO-UNDO.
DEF VAR v-tsfinish-char-val AS cha NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "TSFINISH"
      no-lock no-error.
if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "TSFINISH"
     sys-ctrl.log-fld  = no
     sys-ctrl.char-fld = "All Machines"
     sys-ctrl.descrip  = "Complete All Machines or Last Machine?".
   
end.
ASSIGN v-tsfinish = sys-ctrl.log-fld
       v-tsfinish-char-val = sys-ctrl.char-fld.
