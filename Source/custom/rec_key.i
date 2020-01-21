IF fSuperRunning("session.") THEN DO:
    {1}.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey").    
    CREATE rec_key.
    ASSIGN
        rec_key.rec_key    = {1}.rec_key
        rec_key.table_name = "{1}"
        .
END. /* if super running */ 
ELSE DO:
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

    {1}.rec_key = STRING(YEAR(TODAY),"9999")
                + STRING(MONTH(TODAY),"99")
                + STRING(DAY(TODAY),"99")
                + STRING(TIME,"99999")
                + ".NoSuper"
                .
    OUTPUT TO "blankRecKey.log" APPEND.
    PUT UNFORMATTED
        "Table: {1} - "
        STRING(TODAY,"99/99/9999")
        " @ "
        STRING(TIME,"HH:MM:SS am")
        SKIP.
    DO WHILE TRUE:
        PUT UNFORMATTED
            STRING(iLoop,"99") " - "
            PROGRAM-NAME(iLoop)
            SKIP.
        iLoop = iLoop + 1.
        IF PROGRAM-NAME(iLoop) EQ ? THEN LEAVE. 
    END. /* do while true */
    OUTPUT CLOSE.
END. /* else */
