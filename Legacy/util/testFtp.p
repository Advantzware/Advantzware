    define input parameter ipcCompany as character no-undo init '001'.
    define input parameter ipctestNum  as integer no-undo init 4.
    define input parameter ipcFormat   AS CHARACTER NO-UNDO.
    define input parameter ipcFtpSite  AS CHARACTER NO-UNDO.
    define input parameter ipcFolder   AS CHARACTER NO-UNDO.
    define input parameter ipcFileSpec AS CHARACTER NO-UNDO.
    define input parameter iplSilent   as LOGICAL   no-undo .
    define input parameter iplExecute  as logical   no-undo init yes.


    define output parameter ftpURL      AS CHARACTER NO-UNDO.
    define output parameter ftpUser     AS CHARACTER NO-UNDO.
    define output parameter ftpPassword AS CHARACTER NO-UNDO.
    define output parameter ftpMode     AS CHARACTER NO-UNDO.
    define output parameter ftpDir      AS CHARACTER NO-UNDO.
    define output parameter ftpGet      AS CHARACTER NO-UNDO.
    define output parameter ftpSoftware AS CHARACTER NO-UNDO.
    define output parameter ftpScript   AS CHARACTER NO-UNDO.
    define output parameter ftpBinary   AS CHARACTER NO-UNDO.
    define output parameter cCommandFile AS CHARACTER NO-UNDO.
    define output parameter cFullCmd    AS CHARACTER NO-UNDO.    

    
    DEFINE  BUFFER ipbf-ftpConfig FOR ftpConfig.
        
    def var hftp as handle.
    def buffer bfFtpConfig for ftpConfig.
    run custom/ftpProcs.p persistent set hftp.
    this-procedure:add-super-procedure(hftp).
    /* run pExecFtp ("001", "ipaper", "acpi", "c:\temp", "x.p"). */
    
    ipcFormat = "ipaper".
    ipcFtpSite = "acpi".
    ipcFolder = "c:\temp".
    ipcFileSpec = "x.p".
  
    
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
    
    if ipcTestNum ge 2 then 
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
    
    if ipcTestNum ge 3 then 
    RUN pWriteToFile (INPUT cCommandFile).
    
    if ipcTestNum ge 4 then 
    RUN pExecuteCommand (iplSilent /* silent */, FtpSoftware, cCommandFile, iplExecute /* run cmd */, OUTPUT cFullCmd).
        
    this-procedure:remove-super-procedure(hftp). 
