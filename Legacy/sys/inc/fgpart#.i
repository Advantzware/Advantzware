
def var fgpart#-cha like sys-ctrl.char-fld no-undo.
def var fgpart#-log like sys-ctrl.log-fld no-undo.
def var fgpart#-int like sys-ctrl.int-fld no-undo.
def var fgpart#-dec like sys-ctrl.dec-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGPART#"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGPART#"
   sys-ctrl.descrip  = "Customer Part# Must be Unique for New FG Items?".

  /*MESSAGE TRIM(sys-ctrl.descrip)
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.*/
end.
assign
 fgpart#-cha = sys-ctrl.char-fld
 fgpart#-log = sys-ctrl.log-fld
 fgpart#-int = sys-ctrl.int-fld
 fgpart#-dec = sys-ctrl.dec-fld.
