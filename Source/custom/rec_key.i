IF fSuperRunning("session.") THEN DO:
    {1}.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey").    
/*    CREATE rec_key.                     */
/*    ASSIGN                              */
/*        rec_key.rec_key    = {1}.rec_key*/
/*        rec_key.table_name = "{1}"      */
/*        .                               */
END. /* if super running */ 
ELSE DO:
    {1}.rec_key = STRING(YEAR(TODAY),"9999")
                + STRING(MONTH(TODAY),"99")
                + STRING(DAY(TODAY),"99")
                + STRING(TIME,"99999")
                + ".NoSuper"
                .
    RUN custom/RecKeyLog.p ("{1}").
END. /* else */
/* Resolves hanging EXCLUSIVE lock issue whenever a record is created */
/* FIND CURRENT rec_key NO-LOCK NO-ERROR. */
