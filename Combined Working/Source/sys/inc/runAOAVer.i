/* sys/inc/runAOAVer.i       RunAOAVersion => Reports */

&SCOPED-DEFINE descrip Run AOA Version (Logical = ?)

DEFINE VARIABLE RunAOAVersion1-log LIKE sys-ctrl.log-fld  NO-UNDO.
DEFINE VARIABLE RunAOAVersion1-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE VARIABLE RunAOAVersion2-log LIKE sys-ctrl.log-fld  NO-UNDO.
DEFINE VARIABLE RunAOAVersion2-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE VARIABLE cAOAFile AS CHARACTER NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "Reports"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN 
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "Reports"
        sys-ctrl.log-fld  = NO
        sys-ctrl.char-fld = ""
        sys-ctrl.descrip  = "{&descrip}"
        .
END. /* not avail */
IF sys-ctrl.descrip NE "{&descrip}" THEN DO:
    FIND CURRENT sys-ctrl EXCLUSIVE-LOCK.
    sys-ctrl.descrip  = "{&descrip}".
    FIND CURRENT sys-ctrl NO-LOCK.
END. 

ASSIGN 
    RunAOAVersion1-log = sys-ctrl.log-fld
    RunAOAVersion1-cha = sys-ctrl.char-fld
    .

FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK
     WHERE sys-ctrl-shipto.char-fld EQ "{1}"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl-shipto THEN DO:
    CREATE sys-ctrl-shipto.
    ASSIGN 
        sys-ctrl-shipto.company  = cocode
        sys-ctrl-shipto.name     = "Reports"
        sys-ctrl-shipto.log-fld  = NO
        sys-ctrl-shipto.char-fld = "{1}"
        sys-ctrl-shipto.descrip  = "{&descrip}"
        .
END. /* not avail */
IF sys-ctrl.descrip NE "{&descrip}" THEN DO:
    FIND CURRENT sys-ctrl EXCLUSIVE-LOCK.
    sys-ctrl.descrip  = "{&descrip}".
    FIND CURRENT sys-ctrl NO-LOCK.
END. 

ASSIGN 
    RunAOAVersion2-log = sys-ctrl-ship.log-fld
    RunAOAVersion2-cha = sys-ctrl.char-fld
    .
