/*------------------------------------------------------------------------
    File        : api/CheckTransfer.p
    Purpose     : FTP Transfers the file using the ftpConfig table setup

    Syntax      :

    Description : FTP Transfers the file using the ftpConfig table setup

    Author(s)   : Mithun Porandla
    Created     : Wed Sep 25 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.    
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFilePath  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName  AS CHARACTER NO-UNDO.
        
    FIND FIRST APIOutbound NO-LOCK
         WHERE APIOutbound.apiOutboundID   EQ ipiAPIOutboundID
           AND APIOutbound.requestDataType EQ "FTP" /* This API can only have FTP transfer */ 
         NO-ERROR.
    IF NOT AVAILABLE APIOutbound THEN DO:
        ASSIGN
            opcMessage = "Invalid API ID passed to handler"
            oplSuccess = FALSE
            .
        RETURN.        
    END.
         
    FIND FIRST ttArgs
         WHERE ttArgs.argKey   = "LocalFilePath" NO-ERROR.
    IF NOT AVAILABLE ttArgs THEN DO:
        ASSIGN
            opcMessage = "No valid file path passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
    
    IF ttArgs.argValue EQ "" THEN DO:
        ASSIGN
            opcMessage = "Empty file path passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
    
    FILE-INFO:FILE-NAME = ttArgs.argValue.
    
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
        ASSIGN
            opcMessage = "File path " + ttArgs.argValue + "  passed to handler does not exist"
            oplSuccess = FALSE
            .
        RETURN.    
    END.
            
    cFilePath = FILE-INFO:FULL-PATHNAME.
    
    FIND FIRST ttArgs
         WHERE ttArgs.argKey   = "LocalFileName" NO-ERROR.
    IF NOT AVAILABLE ttArgs THEN DO:
        ASSIGN
            opcMessage = "No valid file name passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.

    IF ttArgs.argValue EQ "" THEN DO:
        ASSIGN
            opcMessage = "Empty file name passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
        
    cFileName = ttArgs.argValue.
    
    RUN FileSys_ValidateFile (
        INPUT  cFilePath + "/" + cFileName,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
    IF NOT oplSuccess THEN
        RETURN.

    COPY-LOB FILE cFilePath + "/" + cFileName TO ioplcRequestData.
    
    ASSIGN   
        opcMessage = "Check file " + cFilePath 
                   + "/" + cFileName + " created successfully"
        oplSuccess = TRUE
        .
