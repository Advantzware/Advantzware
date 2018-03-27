
def var prt-copies as   int   format ">9" init 1 no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "INVCOPYS"
    NO-LOCK no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "INVCOPYS"
   sys-ctrl.descrip = "Int Val: # Invoice Copies to Print. " +
                      "Log Val: Sort by Cust Name?".
      
  message "Please enter the default number of invoice copies"
        update sys-ctrl.int-fld.
end.
assign
 prt-copies       = sys-ctrl.int-fld
 v-sort-name      = sys-ctrl.log-fld.
