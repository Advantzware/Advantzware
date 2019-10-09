/* sys/inc/capacityPage.i */

DEFINE VARIABLE cCapacityPage LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK 
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "CapacityPage"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "CapacityPage"
        sys-ctrl.log-fld  = YES
        sys-ctrl.descrip  = "Schedule Capacity Page Generation"
        sys-ctrl.char-fld = "No"
        .
END. /* not avail */
cCapacityPage = sys-ctrl.char-fld.
