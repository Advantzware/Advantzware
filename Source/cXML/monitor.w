/* monitor.w */

{custom/monitor.w "cXML" "cXML"}

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:    creates monitor, process and response directories and import 
              monitored  files
  Parameters: <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cXMLProcessedDir AS CHARACTER FORMAT 'X(50)' NO-UNDO. 
    DEFINE VARIABLE cXMLResponseDir  AS CHARACTER FORMAT 'X(40)' NO-UNDO.
    DEFINE VARIABLE cXMLMainDir      AS CHARACTER FORMAT 'X(40)' NO-UNDO. 
    DEFINE VARIABLE ccXMLName        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidPath       AS LOGICAL   NO-UNDO. 
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCreated         AS LOGICAL   NO-UNDO. 
    DEFINE VARIABLE cGCompany        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCocode          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyMask     AS CHARACTER NO-UNDO INITIAL "$company$". /* Mask character */
    DEFINE VARIABLE ccXMLOrder       AS CHARACTER NO-UNDO INITIAL "cXMLOrder".
    DEFINE VARIABLE lValidateCompany AS LOGICAL   NO-UNDO. 
    
    FOR EACH cXMLDir:
        ASSIGN
            cXMLMainDir = ENTRY(1,cXMLDir.cXMLDir,cCompanyMask) /* Get main cXML dir (Ord,Ack etc..,) */
            ccXMLName   = cXMLDir.cXMLName
            cGCompany   = g_company /* Store global company value into a temporary variable */
            cCocode     = cocode    /* Store cocode into a temporary variable */
            .
            
        COMPANY-BLK:
        FOR EACH company NO-LOCK:
            lValidateCompany = INDEX(cXMLDir.cXMLDir,cCompanyMask) EQ 0. /* Checks whether NK1 directory has mask and current company is global company */
            
            IF lValidateCompany AND company.company NE g_company THEN 
                NEXT COMPANY-BLK.
            
            ASSIGN
                monitorImportDir = REPLACE(cXMLDir.cXMLDir,cCompanyMask,company.company). /* Replaces mask with company value */
                monitorImportDir = IF SUBSTR(monitorImportDir,LENGTH(monitorImportDir),1) EQ "/" OR   /* Checks for forward or backward slashes at the end of dir. if found removes it */
                                      SUBSTR(monitorImportDir,LENGTH(monitorImportDir),1) EQ "\" THEN
                                       SUBSTR(monitorImportDir,1,LENGTH(monitorImportDir) - 1)
                                   ELSE
                                       monitorImportDir
                .

            /* Checks whether import dir exists */    
            RUN FileSys_ValidateDirectory(
                INPUT monitorImportDir,
                OUTPUT lValidPath,
                OUTPUT cMessage
                ) NO-ERROR.
                
            /* Creates import dir if it does not exists */ 
            IF NOT lValidPath THEN 
                RUN FileSys_CreateDirectory(
                    INPUT monitorImportDir,
                    OUTPUT lCreated,
                    OUTPUT cMessage
                    ) NO-ERROR.
                    
            /* FTP to get Orders. This is commented out as cXMLOrders 
               are not coming through FTP */
            /* IF cXMLDir.cXMLName EQ ccXMLOrder THEN
                RUN cXML/cXMLftp.p (
                    INPUT 'cXML/cXMLftp.dat',
                    INPUT monitorImportDir
                    ). */
                
            /* This assignment is required to populate cocode and g_company variables 
               with company code and global company code since as these variables are being 
               used in cXMLOrderProc.i procedures */    
            ASSIGN
                g_company        = company.company /* assigns global company with current company value */
                cocode           = g_company       /* assigns cocode with current company value */
                cXMLMainDir      = IF SUBSTR(cXMLMainDir,LENGTH(cXMLMainDir),1) EQ "/" OR    /* Checks for forward or backward slashes at the end of dir. if found removes it */
                                      SUBSTR(cXMLMainDir,LENGTH(cXMLMainDir),1) EQ "\" THEN
                                       SUBSTR(cXMLMainDir,1,LENGTH(cXMLMainDir) - 1)
                                   ELSE
                                       cXMLMainDir
                cXMLProcessedDir = cXMLMainDir + '\processed\'
                cXMLResponseDir  = cXMLMainDir + '\response\'
                .

            IF NOT lValidateCompany THEN
                ASSIGN
                    cXMLProcessedDir = cXMLProcessedDir + company.company + '\' /* Path where processed files store */
                    cXMLResponseDir  = cXMLResponseDir  + company.company + '\' /* Path where response files store */
                    .
                
            /* Checks whether processed dir exists */
            RUN FileSys_ValidateDirectory(
                INPUT cXMLProcessedDir,
                OUTPUT lValidPath,
                OUTPUT cMessage
                ) NO-ERROR.
                
            /* Creates processed dir if it does not exists */    
            IF NOT lValidPath THEN    
                RUN FileSys_CreateDirectory(
                    INPUT cXMLProcessedDir,
                    OUTPUT lCreated,
                    OUTPUT cMessage
                    ) NO-ERROR.
                    
            /* Checks whether response dir exists */
            RUN FileSys_ValidateDirectory(
                INPUT cXMLResponseDir,
                OUTPUT lValidPath,
                OUTPUT cMessage
                ) NO-ERROR.
            
            /* Creates response dir if it does not exists */    
            IF NOT lValidPath THEN    
                RUN FileSys_CreateDirectory(
                    INPUT cXMLResponseDir,
                    OUTPUT lCreated,
                    OUTPUT cMessage
                    ) NO-ERROR. 
            
            /* Processes and post xml files present in monitor directory */        
            RUN pPostcXML (
                INPUT monitorImportDir,
                INPUT cXMLProcessedDir,
                INPUT cXMLResponseDir,
                INPUT ccXMLName 
                ).
        END. /* each company */
        
        ASSIGN
            g_company = cGCompany /* Assigns back temporary variable into global company */
            cocode    = cCocode   /* Assigns back temporary variable into cocode */
            . 
    END. /* each cXMLDir */
END PROCEDURE.

PROCEDURE pPostcXML PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose:    process files, post files
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMonitorImportDir AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE INPUT PARAMETER ipcXMLProcessedDir  AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE INPUT PARAMETER ipcXMLResponseDir   AS CHARACTER FORMAT 'X(40)' NO-UNDO.
    DEFINE INPUT PARAMETER ipcXMLName          AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE monitorFile   AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE attrList      AS CHARACTER FORMAT 'X(4)'  NO-UNDO.
    DEFINE VARIABLE cXMLError     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXMLFile      AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE cXMLProcessed AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE cXMLResponse  AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE lReturn       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE returnValue   AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE errorStatus   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ccXMLOrder    AS CHARACTER NO-UNDO INITIAL "cXMLOrder".  
    DEFINE VARIABLE ccXML         AS CHARACTER NO-UNDO INITIAL "cXML".
    
    INPUT FROM OS-DIR(ipcMonitorImportDir) NO-ECHO.
    REPEAT:
        SET monitorFile ^ attrList.
        
        /* skips directories and files which are not xml */
        IF attrList NE 'f' OR monitorFile BEGINS '.' OR
            INDEX(monitorFile,'.xml') EQ 0 THEN NEXT.
        /* pause to give monitor process time to finish write in case we grabbed it too soon */
        PAUSE 3 NO-MESSAGE.
        
        ASSIGN
          cXMLFile      = ipcMonitorImportDir + "\" + monitorFile
          cXMLProcessed = ipcXMLProcessedDir  + monitorFile
          cXMLResponse  = ipcXMLResponseDir   + monitorFile
          .
                
        IF SEARCH(cXMLProcessed) NE ? THEN DO:
            RUN monitorActivity (
                INPUT 'ERROR File: ' + monitorFile + ' already processed',
                INPUT YES,
                INPUT ''
                ).
            cXMLError = ipcMonitorImportDir + '/' + REPLACE(monitorFile,'.xml','.err').
            OS-RENAME VALUE(cXMLFile) VALUE(cXMLError).
            NEXT.
        END. /* if search */
        
        FILE-INFO:FILE-NAME = cXMLFile.
        IF FILE-INFO:FILE-SIZE EQ 0 THEN DO:
            RUN monitorActivity (
                INPUT 'ERROR File: ' + monitorFile + ' is zero size',
                INPUT YES,
                INPUT '')
                .
            cXMLError = ipcMonitorImportDir + '/' + REPLACE(monitorFile,'.xml','.err').
            OS-RENAME VALUE(cXMLFile) VALUE(cXMLError).
            NEXT.
        END. /*if file size is 0 */
        
        RUN monitorActivity (
            INPUT ccXML,
            INPUT YES,
            INPUT monitorFile
            ).
        
        IF ipcXMLName EQ ccXMLOrder THEN DO:
            RUN gencXMLOrder (
                INPUT cXMLFile, 
                INPUT NO /* temptable only*/, 
                OUTPUT returnValue
                ). /* generate order */
        END.
        ELSE 
            RUN cXML/ariba.p (
                INPUT cXMLFile,
                INPUT cXMLResponse,
                OUTPUT returnValue
                ). /* transmit to Ariba */
  
        ASSIGN
          dataLine = FILL(' ',1000)
          SUBSTR(dataLine,1) = RETURN-VALUE + ' ' + cXMLFile
          .
        
        RUN monitorActivity (
            INPUT dataLine,
            INPUT YES,
            INPUT ''
            ).
        
        /* be sure it hasn't been previously processed */
        IF returnValue BEGINS 'Success' THEN DO:
            OS-RENAME VALUE(cXMLFile) VALUE(cXMLProcessed).
            errorStatus = OS-ERROR.
            IF errorStatus NE 0 THEN
            RUN monitorActivity (
                INPUT 'ERROR: Moving ' + monitorFile,
                INPUT YES,
                INPUT ''
                ).
        END. /* success */
        ELSE DO:
            RUN monitorActivity (
                INPUT 'ERROR: ' + returnValue,
                INPUT YES,
                INPUT ''
                ).
            cXMLError = ipcMonitorImportDir + '/' + REPLACE(monitorFile,'.xml','.err').
            OS-RENAME VALUE(cXMLFile) VALUE(cXMLError).
            NEXT.
        END.
    END. /* os-dir repeat */
    INPUT CLOSE. 

END PROCEDURE.

&SCOPED-DEFINE monitorActivity
{cXML/cXMLOrderProc.i}


