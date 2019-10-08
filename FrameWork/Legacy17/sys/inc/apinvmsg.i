
def var apinvmsg-log like sys-ctrl.log-fld  no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "APINVMSG"
    no-error.
do transaction:
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "APINVMSG"
     sys-ctrl.descrip  = "Suppress All Receipts Invoice Message?"
     sys-ctrl.log-fld  = NO.
  end.
end.

apinvmsg-log = sys-ctrl.log-fld.

 
