/* sys/inc/graphic.i */
  
DEF VAR v-graphic-char AS CHAR NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "GRAPHIC"
      NO-LOCK no-error.

if not avail sys-ctrl then DO:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "GRAPHIC"
     sys-ctrl.log-fld  = no
     sys-ctrl.char-fld = ""
     sys-ctrl.descrip  = "FG Graphic Image Folder".
end.
    
v-graphic-char = sys-ctrl.char-fld.
