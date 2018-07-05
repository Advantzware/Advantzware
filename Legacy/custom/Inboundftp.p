/* cXMLftp.p */

DEFINE INPUT PARAMETER ipcFtpSite AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcFolder AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE cWinScpIniFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE cConfigFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lConfigBased AS LOGICAL NO-UNDO.
DEFINE VARIABLE cConfigFolder AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCommandFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPos AS INTEGER NO-UNDO.

DEF TEMP-TABLE ttConfig 
    FIELD importFormat AS CHAR
    FIELD destName     AS CHAR FORMAT "x(20)"
    FIELD ftp-site     AS CHAR FORMAT "x(30)"
    FIELD ftp-user     AS CHAR 
    FIELD ftp-passwd   AS CHAR FORMAT "x(12)"
    FIELD ftp-mode     AS CHAR 
    FIELD ftp-software AS CHAR
    FIELD ftp-dir      AS CHAR 
    FIELD ftp-binary   AS CHAR
    FIELD ftp-script   AS CHAR
    FIELD ftp-cmd      AS CHAR
    INDEX importFormat        importFormat
    INDEX destName IS PRIMARY destName.
    
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
    


FUNCTION getWinScpFile RETURNS CHARACTER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF VAR cWinScpExe AS CHAR NO-UNDO.
    DEF VAR cExec AS CHAR NO-UNDO.
    
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
        
        cExec = '"' + cExec + '"'.
    END.
    cWinScpExe = cExec.          
    RETURN cWinScpExe.   /* Function return value. */

END FUNCTION.

cWinScpIniFile = SEARCH("winScp\winscp.ini").
IF cWinScpIniFile EQ ? THEN 
  cWinScpIniFile = "".
ELSE DO:
        FILE-INFO:FILE-NAME = cWinScpIniFile.
        cWinScpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
END.
iPos = R-INDEX(cConfigFile, "\").
IF iPos GT 0 THEN 
  cConfigFolder = SUBSTRING(cConfigFile, 1, iPos - 1).
ELSE 
  cConfigFolder = ".".  /* Currend Directory */

RUN load-config.
RUN set-config-based (BUFFER ttConfig).

IF NOT lConfigBased OR NOT AVAILABLE ttConfig THEN 
  RETURN.
cCommandFile = cConfigFolder + "\" + ttConfig.ftp-script.
RUN config-Based-Script.

/* run ftp and download the files */
cExec = getWinScpFile().


IF cExec NE ? AND cExec NE "" THEN 
    OS-COMMAND SILENT VALUE(cExec + cWinScpIniFile + " /XMLLOG=c:\temp\log.txt  /script=" + cCommandFile).
   
PROCEDURE config-based-script:
    /* Winscp is the only option handled currently */
    IF ttConfig.ftp-software EQ "winSCP" THEN 
    DO:
        cWinScpIniFile = SEARCH(cConfigFile + "\winscp.ini").
        cExec = getWinScpFile().
        IF cWinScpIniFile EQ ? THEN 
            cWinScpIniFile = "".
        ELSE 
        DO:
            FILE-INFO:FILE-NAME = cWinScpIniFile.
            cWinScpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
        END.
            
        OUTPUT TO VALUE(cConfigFolder + "\" + ttConfig.ftp-script). 
            
    
        PUT UNFORMATTED 
            "option batch abort"                  SKIP.

        PUT UNFORMATTED 
            "option confirm off"                  SKIP.
       
        PUT UNFORMATTED "open " 
            + ttConfig.ftp-mode + ":" + ttConfig.ftp-user + ":" + ttConfig.ftp-passwd + "@" + ttConfig.ftp-site SKIP.

        /* Change to folder where files will be downloaded to */
        PUT UNFORMATTED
            "lcd " + ipcFolder                   SKIP. 

        IF ttConfig.ftp-binary GT "" THEN
            PUT UNFORMATTED
                ttConfig.ftp-binary                   SKIP.

        /* Folder on partner system where files exist to be downloaded */
        IF ttConfig.ftp-dir GT "" THEN 
            PUT UNFORMATTED
                "cd " + ttConfig.ftp-dir          SKIP.            

            PUT UNFORMATTED
                ttConfig.ftp-cmd + " " +  ipcFileSpec  SKIP.            
            
        PUT UNFORMATTED    
            "close"                               SKIP .     

        PUT UNFORMATTED 
            "Exit"                                SKIP.   

        OUTPUT CLOSE.


    END. /* WinScp */

   
END PROCEDURE.

PROCEDURE load-config:
   
    EMPTY TEMP-TABLE ttConfig.

    IF SEARCH(cConfigFile ) NE ? THEN 
    DO:

        INPUT FROM VALUE(cConfigFile).
        REPEAT:
          
            CREATE ttConfig.
            IMPORT ttConfig.importFormat ttConfig.destName ttConfig.ftp-site ttConfig.ftp-user ttConfig.ftp-passwd
                ttConfig.ftp-mode ttConfig.ftp-dir ttConfig.ftp-software
                ttConfig.ftp-binary ttConfig.ftp-script ttConfig.ftp-cmd.

            IF ttConfig.importFormat BEGINS "#" OR ttConfig.importFormat EQ "" THEN
                DELETE ttConfig.
    
        END.
        INPUT CLOSE.
      
    END.
END PROCEDURE. /* load-config */

PROCEDURE set-config-based:
    DEFINE PARAMETER BUFFER ipbf-ttConfig FOR ttConfig.
    FIND FIRST ttConfig WHERE
         ttConfig.destName EQ  ipcFtpSite
        NO-LOCK NO-ERROR.
  

    IF AVAIL ttConfig THEN DO:
        lConfigBased = TRUE.
        FIND ipbf-ttConfig  WHERE ROWID(ipbf-ttConfig) EQ ROWID( ttConfig).
    END.
    ELSE
        lConfigBased = FALSE.

END PROCEDURE. /* set-config-based */