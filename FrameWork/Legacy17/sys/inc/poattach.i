/* sys/inc/graphic.i */
  
DEF VAR v-poattach-char AS CHAR NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "POPaperClip"
      NO-LOCK no-error.

if not avail sys-ctrl then DO:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "POPaperClip"
     sys-ctrl.log-fld  = no
     sys-ctrl.char-fld = ""
     sys-ctrl.descrip  = "PO Attach Default Folder".
end.
    
v-poattach-char = sys-ctrl.char-fld.
