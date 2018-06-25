/* sys/inc/menuLink.i */

DEFINE VARIABLE cMenuLink{1} LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK 
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "MENULINK{1}"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "MENULINK{1}"
        .
END. /* not avail */
cMenuLink{1} = sys-ctrl.char-fld.
