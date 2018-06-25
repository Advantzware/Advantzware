/* sys/inc/menuLinkZoHo.i */

DEFINE VARIABLE cMenuLinkZoHo LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK 
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "MENULINKZOHO"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "MENULINKZOHO"
        sys-ctrl.descrip = "https://support.zoho.com/portal/advantzware/kb"
        sys-ctrl.char-fld = "Graphics\32x32\question.ico"
        sys-ctrl.log-fld  = YES
        .
END. /* not avail */
cMenuLinkZoHo = sys-ctrl.char-fld.
