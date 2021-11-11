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

/* **********************  Internal Functions  ************************ */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTypeToInit   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hArtiosProcs  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hImportProcs  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hValidator    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iProcessed    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lContinue     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lLimitReached AS LOGICAL   NO-UNDO.

    RUN util/ImportProcs.p PERSISTENT SET hImportProcs.
    RUN est/ArtiosProcs.p PERSISTENT SET hArtiosProcs.
    RUN util/Validate.p PERSISTENT SET hValidator.
    SESSION:ADD-SUPER-PROCEDURE (hValidator).

    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    RUN spGetSessionParam ("Location", OUTPUT cLocation).
    cTypeToInit = ENTRY(LOOKUP(cImportType,gcTypePrograms),gcTypeList).
    RUN pConvertExceltoCSV IN hImportProcs (cImportFile, OUTPUT cImportFile).
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

    IF VALID-HANDLE(hValidator) THEN DO:
        SESSION:REMOVE-SUPER-PROCEDURE (hValidator).
        DELETE PROCEDURE hValidator.
    END. // if valid-handle
    IF VALID-HANDLE(hArtiosProcs) THEN
    DELETE PROCEDURE hArtiosProcs.
    IF VALID-HANDLE(hImportProcs) THEN
    DELETE PROCEDURE hImportProcs.

END PROCEDURE.
