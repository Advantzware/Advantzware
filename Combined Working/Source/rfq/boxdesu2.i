def var v-box-uom like sys-ctrl.char-fld.
def var v-sc-fmt  as   char.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BOXDESUM"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "BOXDESUM"
   sys-ctrl.descrip  = "Box Design Scoring UOM"
   sys-ctrl.char-fld = "Inches".
  message "Sys Ctrl NOT found. "
	  "Box design scoring UOM: Inches, MM (Millimeters)"
	  update sys-ctrl.char-fld.
  if sys-ctrl.char-fld ne "Inches" and
     sys-ctrl.char-fld ne "MM"     and
     sys-ctrl.char-fld ne "Both"   then undo, retry.
end.
v-box-uom = sys-ctrl.char-fld.

if program-name(2) matches "*po/*" then v-box-uom = "Inches".

v-sc-fmt  = if v-box-uom eq "MM"                    /*or
	       (v-box-uom eq "Both" and est.metric)*/ then "->>>>9"
           ELSE IF v-cecscrn-char NE "Decimal" THEN "->9.99"
           else "->9.999999".
