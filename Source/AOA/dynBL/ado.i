/* ado.i - rstark - 11.15.2020 */

PROCEDURE p{&Table}:
    DEFINE INPUT PARAMETER ipiTotal AS INTEGER NO-UNDO.

    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

    DISABLE TRIGGERS FOR LOAD OF {&Table}.

    ASSIGN
        hTable[1] = BUFFER {&Table}:HANDLE
        hTable[2] = BUFFER tt{&Table}:HANDLE
        .
    FOR EACH tt{&Table} NO-LOCK:
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar + " {&Table}", iCount, ipiTotal).
        FIND FIRST {&Table} NO-LOCK
             WHERE {&Table}.company     EQ tt{&Table}.company
               AND {&Table}.{&keyField} EQ tt{&Table}.{&keyField}
             NO-ERROR.
        IF NOT AVAILABLE {&Table} THEN DO:
            RUN pCreatettCompare (
                "Add",
                "{&Table}",
                "{&keyField}",
                tt{&Table}.{&keyField},
                tt{&Table}.{&keyField},
                tt{&Table}.{&dscrField}
                ).
            CREATE {&Table}.
        END. /* if not avail */
        ELSE DO:
            BUFFER-COMPARE tt{&Table} TO {&Table} SAVE RESULT IN cCompare.
            RUN pCompare ("{&Table}", {&Table}.{&keyField}, OUTPUT lNoChange).
            IF lNoChange THEN NEXT.
            FIND CURRENT {&Table} EXCLUSIVE-LOCK.
        END. /* if not avail */
        BUFFER-COPY tt{&Table} EXCEPT rec_key TO {&Table}.
        RELEASE {&Table}.
    END. /* each tt{&Table} */

END PROCEDURE.
