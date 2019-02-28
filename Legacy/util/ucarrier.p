
def input parameter v-recid as recid no-undo.


find carrier where recid(carrier) eq v-recid no-error.

if avail carrier then
  assign
   carrier.chg-method = string(carrier.by-pallet,"P/W")
   carrier.by-pallet  = ?.