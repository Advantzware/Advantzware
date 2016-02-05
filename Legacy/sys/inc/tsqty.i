/* sys/inc/tsqty.i */
  
DEF VAR v-tsqty-log AS LOG NO-UNDO.
DEF VAR v-tsqty-cha AS cha NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "TSQTY"
      no-lock no-error.
if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "TSQTY"
     sys-ctrl.log-fld  = no
     sys-ctrl.char-fld = ""
     sys-ctrl.descrip  = "Validate Touch Screen Counts per Machine?".
end.
ASSIGN v-tsqty-log = sys-ctrl.log-fld
       v-tsqty-cha = sys-ctrl.char-fld.
