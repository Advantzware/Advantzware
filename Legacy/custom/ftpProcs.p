/*------------------------------------------------------------------------
    File        : FtpProcs.p
    Purpose     : Mulltiple Procedures and Functions for processing WinFtp or other ftp

    Syntax      :

    Description : Persistent Procedure file for all ftp processing and text manipulation

    Author(s)   : WK
    Created     : Wed Nov 14 14:35:41 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcFormat AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFtpSite AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFolder AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFileSpec AS CHARACTER NO-UNDO.

DEFINE VARIABLE ftpURL AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftpUser AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftpPassword AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftpType AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftpDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftpGet AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLFile AS CHARACTER FORMAT 'X(50)' NO-UNDO.
DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.
DEFINE VARIABLE cExec    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFtpIniFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE cConfigFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lConfigBased AS LOGICAL NO-UNDO.
DEFINE VARIABLE cConfigFolder AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCommandFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPos AS INTEGER NO-UNDO.
DEFINE BUFFER bfFtpConfig FOR ftpConfig.
{custom/ftpProcs.i}

        
{sys/inc/var.i SHARED}

RUN sys/ref/nk1look.p (INPUT cocode,  "InboundConfig", "C" /* Character*/, 
    INPUT NO /* check by cust */, 
    INPUT YES /* use cust not vendor */,
    INPUT "" /* cust */, 
    INPUT "" /* ship-to*/,
    OUTPUT cReturnChar, 
    OUTPUT lRecFound).
IF lRecFound THEN 
    cConfigFile = cReturnChar  .
ELSE
  RETURN.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION GetCloseCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ) FORWARD.

FUNCTION getConfigFolder RETURNS CHARACTER 
    (ipcConfigFile AS CHARACTER) FORWARD.

FUNCTION GetExecFtpCmd RETURNS CHARACTER 
	(ipcSoftware AS CHARACTER, ipcFtpGet AS CHARACTER) FORWARD.

FUNCTION GetExitCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ) FORWARD.

FUNCTION getFtpExe RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER) FORWARD.

FUNCTION getFtpINI RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER) FORWARD.

FUNCTION GetLocalChgDirCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcLocalFolder AS CHARACTER) FORWARD.

FUNCTION GetOpenCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcFtpMode AS CHARACTER, ipcFtpUser AS CHARACTER, ipcFtpPassword AS CHARACTER, ipcFtpURL AS CHARACTER ) FORWARD.

FUNCTION GetPrepCmd1 RETURNS CHARACTER 
	(ipcSoftware AS CHARACTER  ) FORWARD.

FUNCTION GetPrepCmd2 RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER  ) FORWARD.

FUNCTION GetRmtChgDirCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcChdir AS CHARACTER) FORWARD.

FUNCTION setCmdLine RETURNS CHARACTER 
    (INPUT ipiLine AS INTEGER, INPUT ipcCmd AS CHARACTER  ) FORWARD.

/* ************************  Function Implementations ***************** */

FUNCTION GetCloseCmd RETURNS CHARACTER 
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

FUNCTION getConfigFolder RETURNS CHARACTER 
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

FUNCTION GetExecFtpCmd RETURNS CHARACTER 
	( ipcSoftware AS CHARACTER, ipcFtpGet AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "PUT".
        WHEN "FTP" THEN 
            cCmd = "PUT". 
    END CASE.
    
    RETURN cCmd.


		
END FUNCTION.

FUNCTION GetExitCmd RETURNS CHARACTER 
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

FUNCTION getFtpExe RETURNS CHARACTER 
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
    END CASE. 
    
    cFtpExe = cExec.          
    RETURN cFtpExe.   /* Function return value. */

		
END FUNCTION.

FUNCTION getFtpINI RETURNS CHARACTER 
	(ipcSoftware AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:  Returns INI file for ftp software (if any)
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cIniResult AS CHARACTER NO-UNDO.

        CASE ipcSoftware:
            WHEN "WinScp" THEN DO:
                
                cFtpIniFile = SEARCH("winScp\winscp.ini").
                IF cFtpIniFile EQ ? THEN 
                    cFtpIniFile = "".
                ELSE 
                DO:
                    FILE-INFO:FILE-NAME = cFtpIniFile.
                    cFtpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
                END.

            END.
        END CASE.  
        
		RETURN cIniResult.

		
END FUNCTION.

FUNCTION GetLocalChgDirCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcLocalFolder AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "lcd " + ipcLocalFolder.
        WHEN "FTP" THEN 
            cCmd = "Exit". 
    END CASE.
    
    RETURN cCmd.


		
END FUNCTION.

FUNCTION GetOpenCmd RETURNS CHARACTER 
	(ipcSoftware AS CHARACTER, ipcFtpMode AS CHARACTER, ipcFtpUser AS CHARACTER, ipcFtpPassword AS CHARACTER, ipcFtpURL AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "open " 
            + ipcFtpMode + ":" + ipcFtpUser + ":" + ipcFtpPassword + "@" + ipcFtpURL.
        WHEN "FTP" THEN 
            cCmd = "open " + ipcFtpURL + " " + ipcFtpUser + " " + ipcFtpPassword . 
    END CASE.
    
    RETURN cCmd.


		
END FUNCTION.

FUNCTION GetPrepCmd1 RETURNS CHARACTER 
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

FUNCTION GetPrepCmd2 RETURNS CHARACTER 
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

FUNCTION GetRmtChgDirCmd RETURNS CHARACTER 
    (ipcSoftware AS CHARACTER, ipcChdir AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR cCmd AS CHAR NO-UNDO.
    
    CASE ipcSoftware:
        WHEN "WinScp" THEN 
            cCmd = "cd " + ipcChDir.
        WHEN "FTP" THEN 
            cCmd = "cd " + ipcChDir. 
    END CASE.
    
    RETURN cCmd.


		
END FUNCTION.

FUNCTION setCmdLine RETURNS CHARACTER 
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


cConfigFolder = getConfigFolder(cConfigFile).
RUN LoadConfig.

RUN set-config-based (BUFFER bfftpConfig).

IF NOT lConfigBased OR NOT AVAILABLE ftpConfig THEN 
  RETURN.
  
  
cCommandFile = cConfigFolder + "\" + ftpConfig.ftpScript.


RUN config-Based-Script.

/* run ftp and download the files */
cExec = getFtpExe("WinScp").


IF cExec NE ? AND cExec NE "" THEN 
    OS-COMMAND SILENT VALUE(cExec + cFtpIniFile + " /XMLLOG=c:\temp\log.txt  /script=" + cCommandFile).
   
   
   
/* **********************  Internal Procedures  *********************** */   
PROCEDURE createScriptRecords:
    DEFINE INPUT PARAMETER ipFtpURL      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpUser     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpPassword AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpMode     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpDir      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpGet      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpSoftware AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpScript   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFtpBinary   AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cPrepCmd1       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrepCmd2       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOpenCmd        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocalChgDirCmd AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRmtChgDirCmd   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExecFtpCmd     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCloseCmd       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExitCmd        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLineNumber     AS INTEGER NO-UNDO.
    
    /* Winscp is the only option handled currently */
    IF ipFtpSoftware EQ "winSCP" THEN 
    DO:
        
        cFtpIniFile = SEARCH(cConfigFile + "\winscp.ini").
        cExec = getFtpExe("WinScp").
        IF cFtpIniFile EQ ? THEN 
            cFtpIniFile = "".
        ELSE 
        DO:
            FILE-INFO:FILE-NAME = cFtpIniFile.
            cFtpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
        END.
        
        ASSIGN
        cPrepCmd1       = GetPrepCmd1(ipFtpSoftware)    
        cPrepCmd2       = GetPrepCmd2(ipFtpSoftware)      
        cOpenCmd        = GetOpenCmd(ipFtpSoftware, ipFtpMode, ipFtpUser, ipFtpPassword, ipFtpURL)       
        cLocalChgDirCmd = GetLocalChgDirCmd(ipFtpSoftware, ipcFolder)
        cRmtChgDirCmd   = GetRmtChgDirCmd(ipFtpSoftware,ipFtpDir)  
        cExecFtpCmd     = GetExecFtpCmd(ipFtpSoftware, ipFtpGet)    
        cCloseCmd       = GetCloseCmd(ipFtpSoftware)      
        cExitCmd        = GetExitCmd(ipFtpSoftware)
        .       
        iLineNumber = 10.
        setCmdLine(iLineNumber, cPrepCmd1).
        
        iLineNumber = iLineNumber + 10.
        setCmdLine(iLineNumber, cPrepCmd2).
        
        iLineNumber = iLineNumber + 10.
        setCmdLine(iLineNumber, cOpenCmd). 
        
        iLineNumber = iLineNumber + 10.
        setCmdLine(iLineNumber, cLocalChgDirCmd).
        
        iLineNumber = iLineNumber + 10.
        setCmdLine(iLineNumber, cRmtChgDirCmd).
        
        iLineNumber = iLineNumber + 10.
        setCmdLine(iLineNumber, cExecFtpCmd).
        
        iLineNumber = iLineNumber + 10.
        setCmdLine(iLineNumber, cCloseCmd).

    END. /* WinScp */

   
END PROCEDURE.


PROCEDURE getConfigValues:
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

PROCEDURE pWriteToFile:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    OUTPUT TO VALUE(cConfigFolder + "\" + ftpConfig.ftpScript). 

    FOR EACH ttScriptLines BY ttScriptLines.scriptLineNum:       

      PUT UNFORMATTED ttScriptLines.scriptLineText SKIP.
    END.
    OUTPUT CLOSE.




END PROCEDURE.
