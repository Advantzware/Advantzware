
def var browser-log like sys-ctrl.log-fld.
/*
find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BROWSER"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "BROWSER"
   sys-ctrl.descrip = "Show records when first entering OE & CE browsers"
   sys-ctrl.log-fld = YES.
  message sys-ctrl.descrip update sys-ctrl.log-fld.
end.
*/
find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "{1}"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "{1}"
   sys-ctrl.descrip = "Show records when first entering OE & CE browsers"
   sys-ctrl.log-fld = YES
   sys-ctrl.int-fld = 30.
  
end.

assign
 browser-log = sys-ctrl.log-fld.
