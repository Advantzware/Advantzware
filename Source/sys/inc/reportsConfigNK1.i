/* sys/inc/runAOAVer.i       RunAOAVersion => Reports */


DEFINE VARIABLE lShowBatchMode as logical NO-UNDO.
DEFINE VARIABLE lShowParameters as logical NO-UNDO.

FOR FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "Reports":
    FOR FIRST sys-ctrl-shipto  OF sys-ctrl
    WHERE sys-ctrl-shipto.char-fld EQ "{1}":       
        CASE sys-ctrl-shipto.int-fld:         
            WHEN 0 THEN
            ASSIGN 
                lShowBatchMode = FALSE
                lShowParameters = FALSE
            .
            WHEN 1 THEN
            ASSIGN 
                lShowBatchMode = FALSE
                lShowParameters = TRUE
            .
            WHEN 2 THEN
            ASSIGN 
                lShowBatchMode = TRUE
                lShowParameters = TRUE
            .
            OTHERWISE
            ASSIGN 
                lShowBatchMode = FALSE
                lShowParameters = FALSE
            .
        END CASE.
    END.
END. 


