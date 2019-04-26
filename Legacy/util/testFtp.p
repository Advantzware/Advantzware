    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO INIT '001'.
    DEFINE INPUT PARAMETER ipctestNum  AS INTEGER NO-UNDO INIT 4.
    DEFINE INPUT PARAMETER ipcFormat   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpSite  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFolder   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFileSpec AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplSilent   AS LOGICAL   NO-UNDO .
    DEFINE INPUT PARAMETER iplExecute  AS LOGICAL   NO-UNDO INIT YES.


    DEFINE OUTPUT PARAMETER ftpURL      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpUser     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpPassword AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpMode     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpDir      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpGet      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpSoftware AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpScript   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpBinary   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER cCommandFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER cFullCmd    AS CHARACTER NO-UNDO.    

    
    DEFINE  BUFFER ipbf-ftpConfig FOR ftpConfig.
        
    DEFINE VARIABLE hftp AS HANDLE.
    DEFINE BUFFER bfFtpConfig FOR ftpConfig.
    RUN system/ftpProcs.p PERSISTENT SET hftp.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hftp).

    RUN pGetConfigValues (ipcFormat, 
                     ipcFtpSite, 
                     ipcFolder, 
                     ipcFileSpec, 
                     BUFFER bfFtpConfig,
                     OUTPUT ftpURL, 
                     OUTPUT ftpUser, 
                     OUTPUT ftpPassword, 
                     OUTPUT ftpMode, 
                     OUTPUT ftpDir, 
                     OUTPUT ftpGet, 
                     OUTPUT ftpSoftware, 
                     OUTPUT ftpScript, 
                     OUTPUT ftpBinary).
    
    IF ipcTestNum GE 2 THEN 
    RUN pCreateScriptRecords (FtpURL,
                             FtpUser,
                             FtpPassword,
                             FtpMode,
                             FtpDir,
                             FtpGet,
                             FtpSoftware,
                             FtpScript,
                             FtpBinary,
                             ipcFolder,
                             ipcFileSpec).

    cCommandFile = DYNAMIC-FUNCTION("fNK1ConfigFolder", ipcCompany) + "\" + ftpScript.
    
    IF ipcTestNum GE 3 THEN 
    RUN pWriteToFile (INPUT cCommandFile).
    
    IF ipcTestNum GE 4 THEN 
    RUN pExecuteCommand (iplSilent /* silent */, FtpSoftware, cCommandFile, iplExecute /* run cmd */, OUTPUT cFullCmd).
        
    THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hftp). 
