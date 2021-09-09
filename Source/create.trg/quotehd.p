&Scoped-define TABLENAME quotehd

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{custom/globdefs.i}

DEFINE VARIABLE iQNo AS INTEGER NO-UNDO.

DO WHILE TRUE:
    FIND FIRST ce-ctrl EXCLUSIVE-LOCK
         WHERE ce-ctrl.company EQ g_company
           AND ce-ctrl.loc     EQ g_loc
         NO-ERROR NO-WAIT.
    IF AVAILABLE ce-ctrl THEN DO:
        iQNo = ce-ctrl.q-num.
        DO WHILE TRUE:
            iQNo = iQNo + 1.
            IF NOT CAN-FIND(FIRST {&TABLENAME}
                            WHERE {&TABLENAME}.company EQ g_company
                              AND {&TABLENAME}.loc     EQ g_loc
                              AND {&TABLENAME}.q-no    EQ iQNo) THEN
            LEAVE.
        END. /* do while */
        ce-ctrl.q-num = ce-ctrl.q-num.
        RELEASE ce-ctrl.
        LEAVE.
    END. /* if avail */
END. /* do while */
ASSIGN
    {&TABLENAME}.company = g_company
    {&TABLENAME}.loc     = g_loc
    {&TABLENAME}.q-no    = iQNo
    .
