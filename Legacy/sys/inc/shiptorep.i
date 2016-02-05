/* sys/inc/shiptorep.i */
  
DEF VAR v-shiptorep-log AS log NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "OEShipToRep"
      NO-LOCK no-error.

if not avail sys-ctrl then DO:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "OEShipToRep"
     sys-ctrl.log-fld  = no
     sys-ctrl.char-fld = ""
     sys-ctrl.descrip  = "Transfer Sales Rep to Estimate and Order?".
end.
    
v-shiptorep-log = sys-ctrl.log-fld .
