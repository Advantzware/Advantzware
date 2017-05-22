
def var v-tax-mand as log.


find first sys-ctrl
    where sys-ctrl.company eq gcompany
      and sys-ctrl.name    eq "TAXCODE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = gcompany
   sys-ctrl.name    = "TAXCODE"
   sys-ctrl.descrip = "Is the Tax Code Mandatory for All Customers?".
  
end.
v-tax-mand = sys-ctrl.log-fld.

