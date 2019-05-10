/*------------------------------------------------------------------------
  File:         accordFTP.p
  Description:  Accord FTP Task Business Logic
  Author:       Ron Stark
  Date Created: 5.2.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}

DEFINE VARIABLE hFTP AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD taskFile AS CHARACTER FORMAT "x(60)" LABEL "Task File"
    FIELD ftpFile  AS CHARACTER FORMAT "x(60)" LABEL "FTP File"
    .
RUN system/ftpProcs.p PERSISTENT SET hFTP.

&Scoped-define subjectID 5012
{AOA/includes/subjectID{&subjectID}Defs.i}

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cDateTimeStamp AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFolder        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFTPFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPassword      AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bTaskResult FOR taskResult.
    
    g_company = cCompany.
    OS-CREATE-DIR "ftpTasks".
    FOR EACH ftpConfig NO-LOCK
        WHERE ftpConfig.ediType      EQ "Generic"
          AND ftpConfig.ftpDirection EQ "OUT"
          AND ftpConfig.partner      EQ "AB"
        :
        FOR EACH taskResult NO-LOCK
            WHERE (INDEX(taskResult.folderFile,"InventoryInquiryBL") NE 0
              AND  ftpConfig.ftpCode EQ "846")
               OR (INDEX(taskResult.folderFile,"ProductionScheduleBL") NE 0
              AND  ftpConfig.ftpCode EQ "830")
            :
            IF SEARCH(taskResult.folderFile) NE ? THEN DO:
                ASSIGN
                    cDateTimeStamp = STRING(NOW)
                    cDateTimeStamp = SUBSTRING(cDateTimeStamp,1,23)
                    cDateTimeStamp = REPLACE(cDateTimeStamp,"/","")
                    cDateTimeStamp = REPLACE(cDateTimeStamp," ","")
                    cDateTimeStamp = REPLACE(cDateTimeStamp,":","")
                    cDateTimeStamp = REPLACE(cDateTimeStamp,".","")
                    cDateTimeStamp = SUBSTRING(cDateTimeStamp,5,4)
                                   + SUBSTRING(cDateTimeStamp,1,4)
                                   + SUBSTRING(cDateTimeStamp,9)
                    cFTPFile       = "ftpTasks/AB_US_CSV_IB_" + ftpConfig.ftpCode + "_"
                                   + cDateTimeStamp + ".csv"
                                   .
                CREATE ttTempTable.
                ASSIGN
                    ttTempTable.taskFile = taskResult.folderFile
                    ttTempTable.ftpFile  = cFTPFile
                    .
                OS-RENAME VALUE(taskResult.folderFile) VALUE(cFTPFile).
                /* do this because the FTP call might lag and leave taskResult
                   in a transaction lock state, so we update and get out */
                DO TRANSACTION:
                    FIND FIRST bTaskResult EXCLUSIVE-LOCK
                         WHERE ROWID(bTaskResult) EQ ROWID(taskResult).
                    ASSIGN
                        bTaskResult.folderFile = cFTPFile
                        FILE-INFO:FILE-NAME    = SEARCH(bTaskResult.folderFile)
                        cFile                  = REPLACE(bTaskResult.folderFile,"ftpTasks/","")
                        cFolder                = REPLACE(FILE-INFO:FULL-PATHNAME,cFile,"")
                        cPassword              = REPLACE(ftpConfig.ftpPassword,"@","%40") 
                        .
                    FIND CURRENT bTaskResult NO-LOCK.
                END. /* do trans */
                RUN pSimpleFTP IN hFTP (
                    ftpConfig.ftpSite,
                    ftpConfig.ftpUser,
                    cPassword,
                    ftpConfig.ftpDir,
                    ftpConfig.ftpCommand,
                    ftpConfig.ftpMode,
                    ftpConfig.ftpSoftware,
                    ftpConfig.ftpScript,
                    "c:\tmp",
                    ftpConfig.ftpBinary,
                    cFolder,
                    cFile,
                    YES /* run silent */
                    ).
            END. /* if search */
        END. /* each taskresult */
    END. /* each ftpconfig */
END PROCEDURE.
