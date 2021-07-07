/* sys/inc/runAOAVer.i       RunAOAVersion => Reports */

&SCOPED-DEFINE descrip Run AOA Version (Logical = ?)

DEFINE VARIABLE lShowBatchMode as logical NO-UNDO.
DEFINE VARIABLE lShowParameters as logical NO-UNDO.

for FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "Reports":
    for first sys-ctrl-shipto  of sys-ctrl
    where sys-ctrl-shipto.char-fld = "{1}":
        
        case sys-ctrl-shipto.int-fld:
            
            when 0 then
            assign 
            lShowBatchMode = false
            lShowParameters = false
            .
            when 1 then
            assign 
            lShowBatchMode = false
            lShowParameters = true
            .
            when 2 then
            assign 
            lShowBatchMode = true
            lShowParameters = true
            .
            otherwise
            assign 
            lShowBatchMode = false
            lShowParameters = false
            .


end case.
end.
END. 


