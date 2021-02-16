ASSIGN
    hdProc    = SESSION:FIRST-PROCEDURE
    cProcName = "{&procName}"
    cProcName = REPLACE(cProcName, "\", "/")
    cProcName = ENTRY(1, cProcName, ".")
    .

DO WHILE VALID-HANDLE(hdProc):
    IF INDEX(hdProc:FILE-NAME, cProcName) GT 0 THEN DO: 
        {&procHandle} = hdProc.
        LEAVE.
    END.
    
    hdProc = hdProc:NEXT-SIBLING.
END.

IF NOT VALID-HANDLE({&procHandle}) THEN
    RUN {&procName} PERSISTENT SET {&procHandle}.
    
IF {&addToSuperProc} AND VALID-HANDLE({&procHandle}) AND NOT CAN-DO(SESSION:SUPER-PROCEDURES,STRING({&procHandle})) THEN
    SESSION:ADD-SUPER-PROCEDURE({&procHandle}).