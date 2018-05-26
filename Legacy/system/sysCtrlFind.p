/* sysCtrlFind.p */

TRIGGER PROCEDURE FOR FIND OF sys-ctrl.

DEFINE VARIABLE cProgram AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID  AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx      AS INTEGER   NO-UNDO INITIAL 1.

DO WHILE TRUE:
    idx = idx + 1.
    /* all done with program stack */
    IF PROGRAM-NAME(idx) EQ ? THEN LEAVE.
    /* do nothing if inside sys-ctrl window */
    IF INDEX(PROGRAM-NAME(idx),"sys-ctrl.")  NE 0 THEN RETURN.
    /* skip if a sub routine call */
    IF NUM-ENTRIES(PROGRAM-NAME(idx)," ")    GT 1 THEN NEXT.
    /* any of the main launch programs */
    IF INDEX(PROGRAM-NAME(idx),"asiLogin.")  NE 0 OR
       INDEX(PROGRAM-NAME(idx),"mainMenu2.") NE 0 OR
       INDEX(PROGRAM-NAME(idx),"nosweat.")   NE 0 THEN DO:
        IF cProgram EQ "" THEN
        cProgram = PROGRAM-NAME(idx).
        LEAVE.
    END. /* if ... leave */
    /* grab program making sys-ctrl find */
    cProgram = PROGRAM-NAME(idx).
END. /* while true */

cUserID = USERID("ASI").
IF cUserID EQ "" THEN cUserID = "NoSweat".
/* create .dat for use in system/sysCtrlUsage.w */
OUTPUT TO VALUE("users\" + cUserID + "\sysCtrlFind.dat") APPEND.
EXPORT
    cProgram
    sys-ctrl.company
    sys-ctrl.module
    sys-ctrl.name
    sys-ctrl.char-fld
    sys-ctrl.date-fld
    sys-ctrl.dec-fld
    sys-ctrl.int-fld
    sys-ctrl.log-fld
    sys-ctrl.descrip
    USERID(LDBNAME(1))
    NOW
    .
OUTPUT CLOSE.
