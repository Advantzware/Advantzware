/* sys/inc/menuLinkASI.i */

DEFINE VARIABLE cMenuLinkASI LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK 
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "MENULINKASI"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "MENULINKASI"
        sys-ctrl.descrip = "http://www.advantzware.com"
        sys-ctrl.char-fld = "Graphics\asiicon.ico"
        sys-ctrl.log-fld  = YES
        .
END. /* not avail */
cMenuLinkASI = sys-ctrl.char-fld.
