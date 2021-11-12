/*------------------------------------------------------------------------
  File:         Importer.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 11.4.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttImportData
{util/ttImport.i NEW SHARED}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 196
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hArtiosProcs  AS HANDLE NO-UNDO.
DEFINE VARIABLE hImportProcs  AS HANDLE NO-UNDO.
DEFINE VARIABLE hValidator    AS HANDLE NO-UNDO.

/* **********************  Internal Functions  ************************ */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTypeToInit   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iProcessed    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lContinue     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lLimitReached AS LOGICAL   NO-UNDO.

    RUN util/ImportProcs.p PERSISTENT SET hImportProcs.
    RUN est/ArtiosProcs.p PERSISTENT SET hArtiosProcs.
    RUN util/Validate.p PERSISTENT SET hValidator.
    SESSION:ADD-SUPER-PROCEDURE (hValidator).

    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    RUN spGetSessionParam ("Location", OUTPUT cLocation).
    RUN pConvertExceltoCSV IN hImportProcs (cImportFile, OUTPUT cImportFile).
    cTypeToInit = ENTRY(LOOKUP(cImportType,gcTypePrograms),gcTypeList).
    RUN pInitializeType IN hImportProcs (cTypeToInit).
    RUN pLoad IN hImportProcs (
        cCompany,
        cLocation,
        cImportFile,
        lFirstRowHeader,
        LOGICAL(cOverwriteSkipDup,"Overwrite/Skip"),
        lFieldValidation,
        cImportType,
        OUTPUT lContinue,
        OUTPUT lLimitReached,
        OUTPUT iProcessed
        ).
    IF lProcessImport THEN
    RUN pRunProcess (lGenerateLogOnly, lOnlyLogErrors, cWriteIgnore EQ "Blank").
    IF VALID-HANDLE(hValidator) THEN DO:
        SESSION:REMOVE-SUPER-PROCEDURE (hValidator).
        DELETE PROCEDURE hValidator.
    END. // if valid-handle
    IF VALID-HANDLE(hArtiosProcs) THEN
    DELETE PROCEDURE hArtiosProcs.
    IF VALID-HANDLE(hImportProcs) THEN
    DELETE PROCEDURE hImportProcs.

END PROCEDURE.

PROCEDURE pRunProcess:
    DEFINE INPUT PARAMETER iplGenerateLogOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplOnlyLogErrors   AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplIgnoreBlanks    AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cBase           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogFile        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSummaryMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTypeToInit     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iAdded          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iUpdated        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lMessage        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lProcess        AS LOGICAL   NO-UNDO INITIAL YES.

    lMessage = CAN-DO("Grid,LocalCSV,View",dynParamValue.outputFormat).
    IF NOT CAN-FIND(FIRST ttImportData WHERE ttImportData.lValid) THEN DO:
        IF lMessage THEN
        MESSAGE "No Valid Data to Import" VIEW-AS ALERT-BOX.       
    END. // not can-find
    ELSE DO:
        IF lMessage THEN
        MESSAGE
            "Process the Import File to Update/Add Records?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE lProcess.
        IF lProcess THEN DO:
            ASSIGN
                cTypeToInit = ENTRY(LOOKUP(cImportType,gcTypePrograms),gcTypeList)
                cBase       = REPLACE(cTypeToInit,"ttImport","")
                cLogFile    = cLogFolder + "\" + cBase + "_"
                            + STRING(YEAR(TODAY)) + "_"
                            + STRING(MONTH(TODAY)) + "_"
                            + STRING(DAY(TODAY)) + "_"
                            + STRING(TIME) + ".log"
                            .
            RUN pGenerateLog IN hImportProcs (cLogFile, iplOnlyLogErrors).
            IF NOT iplGenerateLogOnly THEN DO:
                RUN pProcessImport IN hImportProcs (iplIgnoreBlanks, OUTPUT iUpdated, OUTPUT iAdded).
                RUN GetSummaryMessage IN hImportProcs (OUTPUT cSummaryMessage).
            END.
            IF lMessage THEN
            MESSAGE
                "Import Process Completed." SKIP 
                iUpdated " Records Updated" SKIP 
                iAdded   " Records Added"   SKIP    
                cSummaryMessage             SKIP       
                "View Log File for Details: " cLogFile
                VIEW-AS ALERT-BOX.   
        END. // if lprocess
    END. // else

END PROCEDURE.
