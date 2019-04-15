/* cXMLftp.p */

DEFINE INPUT PARAMETER ipDatFile AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipLocalDir AS CHARACTER NO-UNDO.

DEFINE VARIABLE ftpCommandFile AS CHARACTER NO-UNDO INITIAL 'cXML/cXMLftp.cmd'.
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

/* read in parameters needed for ftp */
INPUT FROM VALUE(SEARCH(ipDatFile)) NO-ECHO.
IMPORT ftpURL.
IMPORT ftpUser.
IMPORT ftpPassword.
IMPORT ftpType.
IMPORT ftpDir.
IMPORT ftpGet.
INPUT CLOSE.

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

/* create command file to get files */
OUTPUT TO VALUE(ftpCommandFile).

PUT UNFORMATTED 
    "option batch abort"   SKIP.
PUT UNFORMATTED 
    "option confirm off"   SKIP.

PUT UNFORMATTED "open ftp://" + ftpUser + ":" + ftpPassword + "@" + ftpUrl SKIP.

PUT UNFORMATTED
  "cd " + ftpDir                 SKIP.       /* test or prod */
PUT UNFORMATTED
  "lcd " + ipLocalDir            SKIP.       /* test or prod */          
PUT UNFORMATTED
   ftpType                       SKIP.       /* test or prod */       
PUT UNFORMATTED
  "mget " + ftpGet               SKIP.       /* test or prod */                
PUT UNFORMATTED    "close"       SKIP .     
PUT UNFORMATTED "Exit"           SKIP.   
        
OUTPUT CLOSE.

/* run ftp and download the files */
cExec = getWinScpFile().
IF cExec NE ? AND cExec NE "" THEN 
    OS-COMMAND SILENT VALUE(cExec + cWinScpIniFile + " /script=" + ftpCommandFile).


OUTPUT TO VALUE(ftpCommandFile).
 
        PUT UNFORMATTED 
            "option batch abort"   SKIP.
        PUT UNFORMATTED 
            "option confirm off"   SKIP.

        PUT UNFORMATTED "open ftp://" + ftpUser + ":" + ftpPassword + "@" + ftpUrl SKIP.
    
        PUT UNFORMATTED
          "cd " + ftpDir                 SKIP.       /* test or prod */
        PUT UNFORMATTED
          "lcd " + ipLocalDir            SKIP.       /* test or prod */          
        PUT UNFORMATTED
           ftpType                       SKIP.       /* test or prod */       

/* get all the downloaded file names */
INPUT FROM OS-DIR(ipLocalDir) NO-ECHO.
REPEAT:
  SET cXMLFile ^ attrList.
  IF attrList NE 'f' OR cXMLFile BEGINS '.' OR
     INDEX(cXMLFile,'.xml') EQ 0 THEN NEXT.
  PUT UNFORMATTED 'mv ./' cXMLFile ' ./downloaded/' cXMLFile SKIP.
END. /* repeat */
INPUT CLOSE.

          
PUT UNFORMATTED "close"          SKIP .     
PUT UNFORMATTED "Exit"           SKIP. 
OUTPUT CLOSE.

/* run ftp to move files on ftp site */

IF cExec NE ? AND cExec NE "" THEN 
    OS-COMMAND SILENT VALUE(cExec + cWinScpIniFile + " /script=" + ftpCommandFile).
