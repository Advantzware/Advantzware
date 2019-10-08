/* sys/inc/overwritejobplan.i */

DEFINE VARIABLE cOverwriteJobPlan-Char LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE VARIABLE lOverwriteJobPlan-Log  LIKE sys-ctrl.log-fld  NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK 
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "OverwriteJobPlan"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "OverwriteJobPlan"
        sys-ctrl.log-fld  = YES
        sys-ctrl.descrip  = "Overwrite Job Plan"
        sys-ctrl.char-fld = "Yes"
        .
END. /* not avail */
ASSIGN
    cOverwriteJobPlan-Char = sys-ctrl.char-fld
    lOverwriteJobPlan-Log  = sys-ctrl.log-fld
    .
