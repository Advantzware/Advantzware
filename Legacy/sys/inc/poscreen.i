/* sys/inc/graphic.i */
  
DEF VAR v-poscreen-char AS CHAR NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "POScreen"
      NO-LOCK no-error.

if not avail sys-ctrl then DO:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "POScreen"
     sys-ctrl.log-fld  = no
     sys-ctrl.char-fld = "Item-Job"
     sys-ctrl.descrip  = "PO Cursor Initial Position and Tab Order".
end.
    
v-poscreen-char = sys-ctrl.char-fld.