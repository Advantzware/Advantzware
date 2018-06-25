/* sys/inc/menuLinkUpgrade.i */

DEFINE VARIABLE cMenuLinkUpgrade LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK 
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "MENULINKUPGRADE"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "MENULINKUPGRADE"
        sys-ctrl.descrip = "https://desk.zoho.com/support/advantzware/ShowHomePage.do#Solutions"
        sys-ctrl.char-fld = "Graphics\32x32\question_and_answer.ico"
        sys-ctrl.log-fld  = YES
        .
END. /* not avail */
cMenuLinkUpgrade = sys-ctrl.char-fld.
