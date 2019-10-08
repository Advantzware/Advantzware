
def var foamcost-log as log no-undo.
def var foamcost-int as int no-undo.
def var foamcost-cha as cha no-undo.
def var foamcost-dec as dec no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FOAMCOST"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FOAMCOST"
   sys-ctrl.char-fld = "Blank"
   sys-ctrl.descrip  = "Calculate Foam on Estimate using Blank or Sheet Dimensions".
end.
assign
 foamcost-log = sys-ctrl.log-fld
 foamcost-int = sys-ctrl.int-fld   
 foamcost-cha = sys-ctrl.char-fld   
 foamcost-dec = sys-ctrl.dec-fld.
