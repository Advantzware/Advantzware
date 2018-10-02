IF fSuperRunning("session.") THEN DO:
    {1}.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey").    
    CREATE rec_key.
    ASSIGN
        rec_key.rec_key    = {1}.rec_key
        rec_key.table_name = "{1}"
        .
END. /* if super running */ 
