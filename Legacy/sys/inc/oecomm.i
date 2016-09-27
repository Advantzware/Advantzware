
DEF VAR oecomm-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR oecomm-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR oecomm-int LIKE sys-ctrl.int-fld NO-UNDO.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECOMM"
    no-lock no-error.

if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OECOMM"
   sys-ctrl.descrip = "Display Commission Dollars and % " +
		              "in Order Entry & Invoicing?"
   sys-ctrl.log-fld = yes.
 
end.

ASSIGN
 oecomm-log = sys-ctrl.log-fld
 oecomm-cha = sys-ctrl.char-fld
 oecomm-int = sys-ctrl.int-fld.

