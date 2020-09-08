/*------------------------------------------------------------------------
    File        : FtpProcs.p
    Purpose     : Mulltiple Procedures and Functions for processing WinFtp or other ftp

    Syntax      :

    Description : Persistent Procedure file for all ftp processing and text manipulation

    Author(s)   : WK
    Created     : Wed Nov 14 14:35:41 EDT 2018
    Notes       :
                : Example specifying everything:
                :
                : run pSimpleFtp in h (
                     "URL",
                     "userName",
                     "password",
                     "",     /* remote folder */
                     "put",  /* ftp command */
                     "ftp",  /* ftp or sftp */
                     "WinScp", 
                     "ftpscript.dat",  /* script name */
                     "c:\temp", /* folder for script */
                     "asi",  /* bin or asi */
                     "c:\temp", /* local folder if needed */
                     "xyz.txt", /* filespec to get or put */
                     yes         /* Silent - yes or no */
                     ).
                : Example using ftpConfig for setup 
                RUN pExecFtp IN hFtpProcs (INPUT '001',   /* company */
                                           INPUT "iPaper", /* Ftp Config type */
                                           INPUT "acpi",   /* partner code */
                                           INPUT "c:\temp", 
                                           INPUT  "*.dat" /* filespec */).
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE cReturnChar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE cConfigFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lConfigBased AS LOGICAL NO-UNDO.
DEFINE VARIABLE cConfigFolder AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE cCommandFile AS CHARACTER NO-UNDO. */
DEFINE VARIABLE iPos AS INTEGER NO-UNDO.
DEFINE BUFFER bfFtpConfig FOR ftpConfig.
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
{system/ftpProcs.i}        
{sys/inc/var.i NEW SHARED}

cocode = g_company.
/*RUN sys/ref/nk1look.p (INPUT cocode,  "InboundConfig", "C" /* Character*/,*/
/*    INPUT NO /* check by cust */,                                         */
/*    INPUT YES /* use cust not vendor */,                                  */
/*    INPUT "" /* cust */,                                                  */
/*    INPUT "" /* ship-to*/,                                                */
/*    OUTPUT cReturnChar,                                                   */
/*    OUTPUT lRecFound).                                                    */
/*IF lRecFound THEN                                                         */
/*    cConfigFile = cReturnChar  .                                          */
/*ELSE                                                                      */
/*  RETURN.                                                                 */
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fNK1ConfigFolder RETURNS CHARACTER 
	( ipcCompany AS CHARACTER ) FORWARD.

FUNCTION fGetFtpCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ) FORWARD.

FUNCTION fGetLogFile RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcCommandFile AS CHARACTER ) FORWARD.

FUNCTION fGetScriptCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcCommandFile AS CHARACTER ) FORWARD.

FUNCTION fGetCloseCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ) FORWARD.

FUNCTION fGetConfigFolder RETURNS CHARACTER 
    (ipcConfigFile AS CHARACTER) FORWARD.

FUNCTION fGetPutCmd RETURNS CHARACTER 
	(ipcSoftware AS CHARACTER, ipcFtpGet AS CHARACTER, ipcFileSpec AS CHARACTER) FORWARD.

FUNCTION fGetExitCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ) FORWARD.

FUNCTION fGetFtpExe RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER) FORWARD.

FUNCTION fGetFtpIni RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER) FORWARD.

FUNCTION fGetLocalChgDirCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcLocalFolder AS CHARACTER) FORWARD.

FUNCTION fGetOpenCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcFtpMode AS CHARACTER, ipcFtpUser AS CHARACTER, ipcFtpPassword AS CHARACTER, ipcFtpURL AS CHARACTER ) FORWARD.

FUNCTION fGetPrepCmd1 RETURNS CHARACTER 
	(ipcSoftware AS CHARACTER  ) FORWARD.

FUNCTION fGetPrepCmd2 RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ) FORWARD.

FUNCTION fGetRmtChgDirCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcChdir AS CHARACTER) FORWARD.

FUNCTION fGetWinScpExe RETURNS CHARACTER 
	(  ) FORWARD.

FUNCTION fGetWinScpINI RETURNS CHARACTER 
	( ipcCompany AS CHARACTER  ) FORWARD.

FUNCTION fSetCmdLine RETURNS CHARACTER 
    (INPUT ipiLine AS INTEGER, INPUT ipcCmd AS CHARACTER  ) FORWARD.

/* ************************  Function Implementations ***************** */

FUNCTION fNK1ConfigFolder RETURNS CHARACTER 
	( ipcCompany AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cPoConfigDir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnChar AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
    RUN sys/ref/nk1look.p (INPUT ipcCompany,  "POConfigDir", "C" /* Character*/, 
        INPUT NO /* check by cust */, 
        INPUT YES /* use cust not vendor */,
        INPUT "" /* cust */, 
        INPUT "" /* ship-to*/,
        OUTPUT cReturnChar, 
        OUTPUT lRecFound).
    IF lRecFound THEN 
        cPoConfigDir = cReturnChar  .
    ELSE 
        cPoConfigDir  = ".\custfiles\EDIFiles\POs".
    cPoConfigDir = TRIM(cPoConfigDir, "\").
    cResult = cPoConfigDir.
    RETURN cResult.
		
END FUNCTION.


FUNCTION fNK1ConfigFolderBySysCtrlName RETURNS CHARACTER 
	( ipcCompany AS CHARACTER, ipcSysCtrlName AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cConfigDir   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnChar  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound    AS LOGICAL NO-UNDO.
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,  
        ipcSysCtrlName, 
        "C"       /* Character*/, 
        INPUT NO  /* check by cust */, 
        INPUT YES /* use cust not vendor */,
        INPUT ""  /* cust */, 
        INPUT ""  /* ship-to*/,
        OUTPUT cReturnChar, 
        OUTPUT lRecFound
        ).
        
    IF lRecFound THEN 
        cConfigDir = cReturnChar  .
    ELSE 
        cConfigDir  = ".\custfiles". /* this may need modification */
        
    cConfigDir = TRIM(cConfigDir, "\").
    cResult = cConfigDir.
    RETURN cResult.		
END FUNCTION.


FUNCTION fGetFtpCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR cCmd AS CHAR NO-UNDO.
    /* cExec = fGetFtpExe(ipcSoftware). */
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "Close".
        WHEN "FTP" THEN 
            cCmd = "Close". 
    END CASE.
    
    RETURN cCmd.
		
END FUNCTION.

FUNCTION fGetLogFile RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcCommandFile AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN DO:
            cCmd = "/XMLLOG=" + ipcCommandFile + ".XML" .           
        END.
        WHEN "FTP" THEN 
            cCmd = "". 
    END CASE.
    
    RETURN cCmd.
		
END FUNCTION.

FUNCTION fGetScriptCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcCommandFile AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "/script=" + ipcCommandFile.
        WHEN "FTP" THEN 
            cCmd = "-s:" + ipcCommandFile. 
    END CASE.
    
    RETURN cCmd.

		
END FUNCTION.

FUNCTION fGetCloseCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "Close".
        WHEN "FTP" THEN 
            cCmd = "Close". 
    END CASE.
    
    RETURN cCmd.
		
END FUNCTION.

FUNCTION fGetConfigFolder RETURNS CHARACTER 
	(ipcConfigFile AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
		DEFINE VARIABLE iPos AS INTEGER NO-UNDO.
		DEFINE VARIABLE cConfigFolder AS CHARACTER NO-UNDO.
		
        iPos = R-INDEX(ipcConfigFile, "\").
        IF iPos GT 0 THEN 
            cConfigFolder = SUBSTRING(ipcConfigFile, 1, iPos - 1).
        ELSE 
            cConfigFolder = ".".  /* Current Directory */
        cResult = cConfigFolder.
		RETURN cResult.
		
END FUNCTION.

FUNCTION fGetPutCmd RETURNS CHARACTER 
	( ipcSoftware AS CHARACTER, ipcFtpGet AS CHARACTER, ipcFileSpec AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = ipcFtpGet + " " + ipcFileSpec.
        WHEN "FTP" THEN 
            cCmd = ipcFtpGet + " " + ipcFileSpec. 
    END CASE.
    
    RETURN cCmd.
		
END FUNCTION.

FUNCTION fGetExitCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEF VAR cCmd   AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN cCmd = "Exit".
        WHEN "FTP" THEN cCmd = "Exit". 
    END CASE.
    
    RETURN cCmd.
		
END FUNCTION.

FUNCTION fGetFtpExe RETURNS CHARACTER 
	(ipcSoftware AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Return ftp program executable file
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR cFtpExe AS CHAR NO-UNDO.
    DEF VAR cExec      AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN DO:
            IF SEARCH("C:\Program Files\WinSCP\winscp.com") NE ? 
                OR SEARCH("WinSCP\winscp.com") NE ?
                OR SEARCH("C:\Program Files (x86)\WinSCP\winscp.com") NE ?
            THEN DO:
                                
                cExec = SEARCH("WinSCP\winscp.com").
                IF cExec EQ ? THEN 
                    cExec = SEARCH("C:\Program Files\WinSCP\winscp.com").
                IF cExec EQ ? THEN 
                    cExec = SEARCH("C:\Program Files (x86)\WinSCP\winscp.com").
                  
                FILE-INFO:FILE-NAME = cExec.
                cExec = FILE-INFO:FULL-PATHNAME.
                /* To account for spaces in the path */
                cExec = '"' + cExec + '"'.
                
            END.
       END.
       WHEN "FTP" THEN
         cExec = "FTP".
    END CASE. 
    
    cFtpExe = cExec.          
    RETURN cFtpExe.   /* Function return value. */
		
END FUNCTION.

FUNCTION fGetFtpIni RETURNS CHARACTER 
	(ipcSoftware AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:  Returns INI file for ftp software (if any)
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cIniResult AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFtpIniFile AS CHARACTER NO-UNDO.
        CASE ipcSoftware:
            WHEN "WinScp" THEN DO:
            
                cFtpIniFile = SEARCH(fNK1ConfigFolder(cocode) + "\winscp.ini").

                IF cFtpIniFile EQ ? THEN 
                    cFtpIniFile = "".
                ELSE 
                DO:
                    FILE-INFO:FILE-NAME = cFtpIniFile.
                    cFtpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
                END.

            END.
        END CASE.  
        IF cFtpIniFile NE ? THEN 
          cIniResult = cFtpIniFile.
		RETURN cIniResult.
END FUNCTION.

FUNCTION fGetLocalChgDirCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcLocalFolder AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = IF ipcLocalFolder GT "" THEN "lcd " + ipcLocalFolder ELSE "".
        WHEN "FTP" THEN 
            cCmd = "Exit". 
    END CASE.
    
    RETURN cCmd.
END FUNCTION.

FUNCTION fGetOpenCmd RETURNS CHARACTER 
	(ipcSoftware AS CHARACTER, ipcFtpMode AS CHARACTER, ipcFtpUser AS CHARACTER, ipcFtpPassword AS CHARACTER, ipcFtpURL AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "open " 
            + ipcFtpMode + ":" + ipcFtpUser + ":" + ipcFtpPassword + "@" + ipcFtpURL + " -hostkey=*".
        WHEN "FTP" THEN 
            cCmd = "open " + ipcFtpURL + " " + ipcFtpUser + " " + ipcFtpPassword . 
    END CASE.
    
    RETURN cCmd.
END FUNCTION.

FUNCTION fGetPrepCmd1 RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    
    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "option batch abort".
        WHEN "FTP" THEN 
            cCmd = "". 
    END CASE.
    
    RETURN cCmd.
END FUNCTION.

FUNCTION fGetPrepCmd2 RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
     
    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "option confirm off".
        WHEN "FTP" THEN 
            cCmd = "". 
    END CASE.
    
    RETURN cCmd.
END FUNCTION.

FUNCTION fGetRmtChgDirCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcChdir AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = IF ipcChDir GT "" THEN  "cd " + ipcChDir ELSE "".
        WHEN "FTP" THEN 
            cCmd = IF ipcChDir GT "" THEN  "cd " + ipcChDir ELSE "". 
    END CASE.
    RETURN cCmd.
END FUNCTION.

FUNCTION fGetWinScpExe RETURNS CHARACTER 
	(  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEF VAR cWinScpExe AS CHAR NO-UNDO.
    DEF VAR cExec      AS CHAR NO-UNDO.
    
    IF SEARCH("C:\Program Files\WinSCP\winscp.com") NE ? 
        OR SEARCH("WinSCP\winscp.com") NE ?
        OR SEARCH("C:\Program Files (x86)\WinSCP\winscp.com") NE ?
        THEN 
    DO:
        /* WinSCP folder is in resources */
        cExec = SEARCH("WinSCP\winscp.com").
        IF cExec EQ ? THEN 
            cExec = SEARCH("C:\Program Files\WinSCP\winscp.com").
        IF cExec EQ ? THEN 
            cExec = SEARCH("C:\Program Files (x86)\WinSCP\winscp.com").
          
        FILE-INFO:FILE-NAME = cExec.
        cExec = FILE-INFO:FULL-PATHNAME.
        
        cExec = '"' + cExec + '"'.
    END.
    cWinScpExe = cExec.          
    RETURN cWinScpExe.   /* Function return value. */
END FUNCTION.

FUNCTION fGetWinScpINI RETURNS CHARACTER 
	( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
DEFINE VARIABLE cWinScpIniFile AS CHARACTER NO-UNDO.
    cWinScpIniFile = SEARCH(fNK1configFolder(ipcCompany) + "\winscp.ini").
    IF cWinScpIniFile EQ ? THEN 
        cWinScpIniFile = "".
    ELSE 
    DO:
        FILE-INFO:FILE-NAME = cWinScpIniFile.
        cWinScpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
    END.
END FUNCTION.

FUNCTION fSetCmdLine RETURNS CHARACTER 
    (INPUT ipiLine AS INTEGER, INPUT ipcCmd AS CHARACTER  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
	DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
	
    CREATE ttScriptLines.
    ASSIGN ttScriptLines.scriptLineNum  = ipiLine
           ttScriptLines.scriptLineText = ipcCmd
           .
    RETURN cResult.
END FUNCTION.

/* ***************************  Main Block  *************************** */

   
   
/* **********************  Internal Procedures  *********************** */   

PROCEDURE FTP_SendFileWithCurl:
/*------------------------------------------------------------------------------
 Purpose: Uploads the file to the input host with cURL
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcHost      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFTPType   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPassword  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFileName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLogFile   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplEnableSSH AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcHostKey   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCommand      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponseFile AS CHARACTER NO-UNDO.
        
    IF SEARCH("curl.exe") EQ ? THEN DO:
        ASSIGN 
            opcMessage = "curl not found!".
            oplSuccess = NO
            .
                 
        RETURN. 
    END.
    
    cCommand = SEARCH("curl.exe").
    
    IF ipcFTPType NE "FTP" AND ipcFTPType NE "SFTP" THEN DO:
        ASSIGN 
            opcMessage = "Invalid FTP type. Valid FTP types are 'FTP' and 'SFTP'".
            oplSuccess = NO
            .
                 
        RETURN.         
    END.
    
    cCommand = 'start /min '  /* Start cmd with no-wait option to run cmd asynchronously. /min option starts cmd in minimized state */
             + cCommand + ' -v --ftp-skip-pasv-ip' 
             + (IF iplEnableSSH THEN ' --hostpubmd5 "' + ipcHostKey + '"' ELSE ' --insecure')
             + ' --user "' + ipcUserName + ":" + ipcPassword + '"'
             + ' "' + LC(ipcFTPType) + "://" + ipcHost + '"'
             + ' -T "' + ipcFileName + '"'
             + ' --stderr "' + ipcLogFile + '"'.

    cResponseFile = "ftp_log_" + STRING(MTIME) + ".log".

    RUN OS_RunCommand (
        INPUT  cCommand,              /* Command string to run */
        INPUT  cResponseFile,         /* File name to write the command output */
        INPUT  FALSE,                 /* Run with SILENT option */
        INPUT  TRUE,                  /* Run with NO-WAIT option */
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
    
    OS-DELETE VALUE(cResponseFile).
END PROCEDURE.

PROCEDURE pCreateScriptRecords:
    DEFINE INPUT PARAMETER ipFtpURL      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpUser     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpPassword AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpMode     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpDir      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpGet      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpSoftware AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpScript   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpBinary   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocalFolder AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFileSpec  AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cPrepCmd1       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrepCmd2       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOpenCmd        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocalChgDirCmd AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRmtChgDirCmd   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFtpPutCmd     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCloseCmd       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExitCmd        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLineNumber     AS INTEGER NO-UNDO.
    DEFINE VARIABLE cFtpIniFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExec AS CHARACTER NO-UNDO.    
    /* Winscp is the only option handled currently */
    IF ipFtpSoftware EQ "winSCP" THEN 
    DO:
        
        cFtpIniFile = SEARCH(cConfigFile + "\winscp.ini").
        cExec = fGetFtpExe("WinScp").
        IF cFtpIniFile EQ ? THEN 
            cFtpIniFile = "".
        ELSE 
        DO:
            FILE-INFO:FILE-NAME = cFtpIniFile.
            cFtpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
        END.
        
        ASSIGN
        cPrepCmd1       = fGetPrepCmd1(ipFtpSoftware)    
        cPrepCmd2       = fGetPrepCmd2(ipFtpSoftware)      
        cOpenCmd        = fGetOpenCmd(ipFtpSoftware, ipFtpMode, ipFtpUser, 
                                      ipFtpPassword, ipFtpURL)       
        cLocalChgDirCmd = fGetLocalChgDirCmd(ipFtpSoftware, ipcLocalFolder)
        cRmtChgDirCmd   = fGetRmtChgDirCmd(ipFtpSoftware, ipFtpDir)  
        cFtpPutCmd      = fGetPutCmd(ipFtpSoftware, ipFtpGet, ipcFileSpec)    
        cCloseCmd       = fGetCloseCmd(ipFtpSoftware)      
        cExitCmd        = fGetExitCmd(ipFtpSoftware)
        .       
        iLineNumber = 10.
        fSetCmdLine(iLineNumber, cPrepCmd1).
        
        iLineNumber = iLineNumber + 10.
        fSetCmdLine(iLineNumber, cPrepCmd2).
        
        iLineNumber = iLineNumber + 10.
        fSetCmdLine(iLineNumber, cOpenCmd). 
        
        iLineNumber = iLineNumber + 10.          
        fSetCmdLine(iLineNumber, cLocalChgDirCmd).
        
        iLineNumber = iLineNumber + 10.
        fSetCmdLine(iLineNumber, cRmtChgDirCmd).
        
        iLineNumber = iLineNumber + 10.
        fSetCmdLine(iLineNumber, cFtpPutCmd).
        
        iLineNumber = iLineNumber + 10.
        fSetCmdLine(iLineNumber, cCloseCmd).
        
        iLineNumber = iLineNumber + 10.
        fSetCmdLine(iLineNumber, cExitCmd).
        
    END. /* WinScp */   
END PROCEDURE.

PROCEDURE pGetConfigValues:
    DEFINE INPUT PARAMETER ipcFormat   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpSite  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFolder   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFileSpec AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-ftpConfig FOR ftpConfig.
    DEFINE OUTPUT PARAMETER ftpURL      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpUser     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpPassword AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpMode     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpDir      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpGet      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpSoftware AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpScript   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ftpBinary   AS CHARACTER NO-UNDO.
    
    FIND FIRST ftpConfig 
        WHERE ftpConfig.ftpCode EQ ipcFormat
          AND ftpConfig.partner EQ  ipcFtpSite
        NO-LOCK NO-ERROR.
        
    /* IF format not specified */
    IF NOT AVAILABLE ftpConfig THEN
    FIND FIRST ftpConfig 
        WHERE ftpConfig.partner EQ  ipcFtpSite
        NO-LOCK NO-ERROR.
  
    IF AVAIL ftpConfig THEN DO:
        lConfigBased = TRUE.
        ASSIGN
            ftpURL      = ftpConfig.ftpSite   /* ftp url */
            ftpUser     = ftpConfig.ftpUser
            ftpPassword = ftpConfig.ftpPassword
            ftpMode     = ftpConfig.ftpMode   /* ftp or SCP */
            ftpDir      = ftpConfig.ftpDir    /* Remote Dir */
            ftpGet      = ftpConfig.ftpCommand    /* Command keyword */
            ftpSoftware = ftpConfig.ftpSoftware
            ftpScript   = ftpConfig.ftpScript /* script file name */
            ftpBinary   = ftpConfig.ftpBinary /* Character - ASC or BIN */
            .
                    
        FIND ipbf-ftpConfig  WHERE ROWID(ipbf-ftpConfig) EQ ROWID( ftpConfig).
    END.
    ELSE
        lConfigBased = FALSE.
END PROCEDURE. /* set-config-based */

PROCEDURE pExecuteCommand:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /* run ftp and download the files */
    DEFINE INPUT  PARAMETER iplSilent AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSoftware AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCommandFile AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplRunCmd AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcExecString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogFileCmd AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScriptCmd AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFtpIniFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExec AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullCmd AS CHARACTER NO-UNDO.
    cExec = fGetFtpExe(ipcSoftware).
    cLogFileCmd = fGetLogFile(ipcSoftware, ipcCommandFile).
    cScriptCmd = fGetScriptCmd(ipcSoftware, ipcCommandFile).
    cFtpIniFile = fGetFtpIni(ipcSoftware).
    cFullCmd = cExec + cFtpIniFile + " " + cLogFileCmd + " " + cScriptCmd.
    IF iplRunCmd AND cExec NE ? AND cExec NE "" THEN DO:
        IF iplSilent THEN 
            OS-COMMAND SILENT 
              VALUE(cFullCmd).
        ELSE 
            OS-COMMAND 
              VALUE(cFullCmd).
        
    END.
    opcExecString = cFullCmd.
END PROCEDURE.

PROCEDURE pSimpleFtp:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFtpURL       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpUser      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpPassword  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpDir       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpGet       AS CHARACTER NO-UNDO. /* ftp cmd */
    DEFINE INPUT PARAMETER ipcFtpMode      AS CHARACTER NO-UNDO. /* ftp or sftp */
    DEFINE INPUT PARAMETER ipcFtpSoftware  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpScript    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpScriptDir AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpBinary    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocalFolder  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFileSpec     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplSilent       AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cCommandFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullCmd     AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttScriptLines.
    
    RUN pCreateScriptRecords (ipcFtpURL,
        ipcFtpUser,
        ipcFtpPassword,
        ipcFtpMode,
        ipcFtpDir,
        ipcFtpGet,
        ipcFtpSoftware,
        ipcFtpScript,
        ipcFtpBinary,
        ipcLocalFolder,
        ipcFileSpec).

    cCommandFile = ipcFtpScriptDir + "\" + ipcFtpScript.
    RUN pWriteToFile (INPUT cCommandFile).
    RUN pExecuteCommand (iplSilent, ipcFtpSoftware, cCommandFile, YES /* run cmd */, OUTPUT cFullCmd).
END PROCEDURE.

PROCEDURE pWriteToFile:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCommandFile AS CHARACTER NO-UNDO.
    OUTPUT TO VALUE(ipcCommandFile). 
/*    cCommandFile = fNK1configFolder() + "\" + ftpScript. */
    
    FOR EACH ttScriptLines BY ttScriptLines.scriptLineNum:       
      PUT UNFORMATTED ttScriptLines.scriptLineText SKIP.
    END.
    
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE pExecFtp:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFormat   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFtpSite  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFolder   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFileSpec AS CHARACTER NO-UNDO.
    
    
    DEFINE VARIABLE ftpURL      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpUser     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpPassword AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpType     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpDir      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpGet      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpMode     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpSoftware AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpScript   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpBinary   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCommandFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullCmd    AS CHARACTER NO-UNDO.
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

    /* this is a temporary code and may need to be removed */
    if ipcFormat = "checktransfer" then 
       cCommandFile = fNK1configFolderBySysCtrlName(ipcCompany, "BankTransmittalLocation") + "\" + ftpScript.
    else
       cCommandFile = fNK1configFolder(ipcCompany) + "\" + ftpScript.
       
    RUN pWriteToFile (INPUT cCommandFile).
    RUN pExecuteCommand (YES /* silent */, FtpSoftware, cCommandFile, YES /* run cmd */, OUTPUT cFullCmd).
END PROCEDURE.

PROCEDURE FTP_GetScriptName:
    DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFTPType        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFTPCode        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFTPPartnerCode AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSysCtrlName    AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcScriptFilePath AS CHARACTER NO-UNDO.    
    
    FIND FIRST ftpConfig NO-LOCK
        WHERE ftpConfig.ediType EQ ipcFTPType
          AND ftpConfig.ftpCode EQ ipcFTPCode
          AND ftpConfig.partner EQ ipcFTPPartnerCode
        NO-ERROR.        
    IF AVAIL ftpConfig THEN
        opcScriptFilePath = ftpConfig.ftpScript.
        
    opcScriptFilePath = fNK1configFolderBySysCtrlName(
                            ipcCompany, 
                            ipcSysCtrlName) 
                      + "\" + opcScriptFilePath
                      + ".xml".
END PROCEDURE.

PROCEDURE FTP_GetResponse:
    DEFINE INPUT  PARAMETER ipcResponseFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcResponseXML AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE cLine                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFailureMessageStart AS LOGICAL NO-UNDO.
    
    FILE-INFO:FILE-NAME = ipcResponseFile.
    
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
        ASSIGN
            opcMessage = "Response XML File " + ipcResponseFile + " does not exist"
            oplSuccess = FALSE
            .
        RETURN.    
    END.
    
    COPY-LOB FROM FILE ipcResponseFile TO oplcResponseXML.
    
    INPUT FROM VALUE(ipcResponseFile).
    REPEAT:
        IMPORT UNFORMATTED cLine.
    
        IF INDEX(cLine, "<message>") > 0 THEN
            opcMessage = opcMessage + TRIM(cLine) + "~n".
        
        IF INDEX(cLine, "<failure>") > 0 THEN
            oplSuccess = FALSE.
        
        IF INDEX(cLine, "result") > 0 THEN
            oplSuccess = IF INDEX(cLine, "true") > 0 THEN
                             TRUE
                         ELSE IF INDEX(cLine, "false") > 0 THEN
                             FALSE
                         ELSE
                             FALSE.
    END.
    INPUT CLOSE.
    
    ASSIGN
        opcMessage = REPLACE(opcMessage, "<message>", "")
        opcMessage = REPLACE(opcMessage, "</message>", "")
        .
END PROCEDURE.
