IF fSuperRunning("session.") THEN DO:
    {1}.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey").    
    CREATE rec_key.
    ASSIGN
        rec_key.rec_key    = {1}.rec_key
        rec_key.table_name = "{1}"
        .
END. /* if super running */ 
ELSE DO:
    {1}.rec_key = STRING(YEAR(TODAY),"9999")
                + STRING(MONTH(TODAY),"99")
                + STRING(DAY(TODAY),"99")
                + STRING(TIME,"99999")
                + ".NoSuper"
                .
    RUN pBlankRecKeyLog.
END. /* else */

PROCEDURE pBlankRecKeyLog:
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    OUTPUT TO "blankRecKey.log" APPEND.
    PUT UNFORMATTED
        "Table: {1} - "
        STRING(TODAY,"99/99/9999")
        " @ "
        STRING(TIME,"HH:MM:SS am")
        SKIP.
    DO WHILE TRUE:
        PUT UNFORMATTED
            STRING(idx,"99") " - "
            PROGRAM-NAME(idx)
            SKIP.
        idx = idx + 1.
        IF PROGRAM-NAME(idx) EQ ? THEN LEAVE. 
    END. /* do while true */
    OUTPUT CLOSE.
END PROCEDURE.
