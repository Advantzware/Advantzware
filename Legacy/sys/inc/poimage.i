/* sys/inc/poimage.i */
  
DEF VAR v-poimage-char AS CHAR NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "POIMAGE"
      NO-LOCK no-error.

if not avail sys-ctrl then DO:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "POIMAGE"
     sys-ctrl.log-fld  = no
     sys-ctrl.char-fld = ""
     sys-ctrl.descrip  = "PO Image File Location".
end.
    
v-poimage-char = sys-ctrl.char-fld.
