/* sys/inc/oecomb.i */
def var v-oecomb like sys-ctrl.log-fld NO-UNDO.
DEF VAR v-oecomb-val AS cha NO-UNDO.
def var v-oecomb-int like sys-ctrl.int-fld NO-UNDO.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECOMB"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OECOMB"
   sys-ctrl.char-fld = "password"
   sys-ctrl.log-fld = yes
   sys-ctrl.descrip = "Delete Combination Estimate? " .
   
 
end.
assign
 v-oecomb     = sys-ctrl.log-fld
 v-oecomb-val = sys-ctrl.char-fld
 v-oecomb-int = sys-ctrl.int-fld.
