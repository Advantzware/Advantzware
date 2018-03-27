    ASSIGN 
        cError = ""
        startTime = MTIME
        iCount = 0
        iProcessCount = 0
        iCount = oRefTableMigration:{1}(iRecordLimit) 
        iProcessCount = oRefTableMigration:iProcessCount
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN 
        cError = "Error occured while converting reftable " + {2}.
    
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = {2}
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    OUTPUT STREAM sText TO VALUE(cOutFile2) APPEND.
    PUT STREAM sText UNFORMATTED "Processed " + ttResults.cReftable + " - " + string(iCount) + "total records read." + CHR(10).
    OUTPUT STREAM sText CLOSE.
