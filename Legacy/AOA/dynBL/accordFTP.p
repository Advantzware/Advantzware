/*------------------------------------------------------------------------
  File:         accordFTP.p
  Description:  Accord FTP Task Business Logic
  Author:       Ron Stark
  Date Created: 5.2.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hFTP AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD tempField AS CHARACTER
    .
RUN system/ftpProcs.p PERSISTENT SET hFTP.

{AOA/includes/dynRunBusinessLogicDefs.i}

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cDateTimeStamp AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFolder        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFTPFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPassword      AS CHARACTER NO-UNDO.
    
    OS-CREATE-DIR "ftpTasks".
    FOR EACH ftpConfig NO-LOCK
        WHERE ftpConfig.ediType      EQ "Generic"
          AND ftpConfig.ftpDirection EQ "OUT"
          AND ftpConfig.partner      EQ "AB"
        :
        FOR EACH taskResult EXCLUSIVE-LOCK
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
                OS-RENAME VALUE(taskResult.folderFile) VALUE(cFTPFile).
                ASSIGN
                    taskResult.folderFile = cFTPFile
                    FILE-INFO:FILE-NAME   = SEARCH(taskResult.folderFile)
                    cFile                 = REPLACE(taskResult.folderFile,"ftpTasks/","")
                    cFolder               = REPLACE(FILE-INFO:FULL-PATHNAME,cFile,"")
                    cPassword             = REPLACE(ftpConfig.ftpPassword,"@","%40") 
                    .
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
                    YES
                    ).
            END. /* if search */
        END. /* each taskresult */
    END. /* each ftpconfig */
END PROCEDURE.

PROCEDURE pAssignParamVariables:
    /* dummy procedure holder - do not delete this procedure */
END PROCEDURE.
