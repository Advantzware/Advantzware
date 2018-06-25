/* sys/inc/CEMenu.i */

DEFINE VARIABLE cCEMenu LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK 
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "CEMenu"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "CEMenu"
        sys-ctrl.descrip = "Graphics\bigboxes"
        .
END. /* not avail */
cCEMenu = sys-ctrl.char-fld.
