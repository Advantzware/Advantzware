
DEF VAR poqty-log LIKE sys-ctrl.log-fld NO-UNDO.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name eq "POQTY"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "POQTY"
   sys-ctrl.descrip  = "Auto PO QTY to Use Job QTY or Net Sheets?"
   sys-ctrl.char-fld = "JobQty"
   sys-ctrl.log-fld  = no.
  message "Create Auto PO using JobQty or Net Shts?" 
          update sys-ctrl.char-fld.
end.
assign
 v-po-qty  = if sys-ctrl.char-fld eq "Net Shts" then false else true
 poqty-log = sys-ctrl.log-fld.
