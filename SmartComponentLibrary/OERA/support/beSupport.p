/*************************************************************/
/* Copyright (c) 2007 by Progress Software Corporation      */
/*************************************************************/
/*------------------------------------------------------------------------------
 File        : beSupport.p
 Description : Business Entity super procedre
 Notes       : Sample
              -  Service as property (default to source in main)
              -  DataAccess start and stop managed by service
              -  This sampel uses hooks/events to separate read and save
                 business logic from the data transport and context to allow
                 future changes as well as replacements of fetchData.
                 SaveChanges has only dataset parameter and can actually serve
                 as a hook future extenstions to save could just call it.
              -  This sample only uses one hook at the dataset level.
              -  This sample has hooks in the instance. Hooks can also
                 be in separate procedures.
------------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.Util.* .

/* property table --
   - Stores instance properties.
    (consider using classes with native properties instead)*/
DEFINE TEMP-TABLE ttProperty NO-UNDO
  FIELD TargetProcedure AS HANDLE
  FIELD EntityName AS CHARACTER
  FIELD Service   AS HANDLE
  FIELD DataAccessObject AS HANDLE
  FIELD HasClient AS LOGICAL
  INDEX targetprocedure AS UNIQUE targetprocedure.

/* ***************************  ProtoTypes **************************** */
FUNCTION findTarget RETURNS LOGICAL PRIVATE
  ( phTarget AS HANDLE ) FORWARD.

/* ************ Functions ****************************************/
/* find the target's properties  - private   */
FUNCTION findTarget RETURNS LOGICAL PRIVATE
  ( phTarget AS HANDLE ):

  FIND ttProperty WHERE ttProperty.targetprocedure = phTarget NO-ERROR.

  RETURN AVAILABLE ttProperty.
END FUNCTION.

FUNCTION getDataAccessObject RETURNS HANDLE
  (  ):
  DEFINE VARIABLE hService AS HANDLE    NO-UNDO.

  findTarget(TARGET-PROCEDURE).
  IF NOT VALID-HANDLE(ttProperty.DataAccessObject) THEN
  DO:
    hService = DYNAMIC-FUNCTION('getService':U IN TARGET-PROCEDURE).
    ttProperty.DataAccessObject = DYNAMIC-FUNCTION('startDataAccess':U IN hService,
                                  TARGET-PROCEDURE).
  END.
  RETURN ttProperty.DataAccessObject.
END FUNCTION.

FUNCTION setDataAccessObject RETURNS LOGICAL PRIVATE
  ( phHandle AS HANDLE ) :
  findTarget(TARGET-PROCEDURE).
  ttProperty.DataAccessObject = phHandle.
END FUNCTION.

/* HasClient is set to true if the Be is directly supporting a rich client
  (This basically means define row level triggers and keep alive BE ) */

FUNCTION getHasClient RETURNS LOGICAL
  (  ) .
  findTarget(TARGET-PROCEDURE).
  RETURN ttProperty.HasClient.
END FUNCTION.

FUNCTION setHasClient RETURNS LOGICAL
  ( plClient AS LOGICAL ) :
  findTarget(TARGET-PROCEDURE).
  ttProperty.HasClient = plClient.
  RETURN TRUE.
END FUNCTION.

/* EntityName is defined here for dynamic instances  */
FUNCTION getEntityName RETURNS CHARACTER
  (  ):
  findTarget(TARGET-PROCEDURE).
  RETURN ttProperty.EntityName.
END FUNCTION.

FUNCTION setEntityName RETURNS LOGICAL
  ( pcName AS CHARACTER ) :
  findTarget(TARGET-PROCEDURE).
  ttProperty.EntityName = pcName.
  RETURN TRUE.
END FUNCTION.

FUNCTION getService RETURNS HANDLE
  (  ) .
  findTarget(TARGET-PROCEDURE).
  RETURN ttProperty.Service.
END FUNCTION.

FUNCTION setService RETURNS LOGICAL
  ( phService AS HANDLE ) :
  findTarget(TARGET-PROCEDURE).
  ttProperty.Service = phService.
  RETURN TRUE.
END FUNCTION.

/* Overridden by static instance */
FUNCTION getStaticDataset RETURNS HANDLE ( ) :
  RETURN ?.
END FUNCTION.

/* To be used by generic super destroy  */
FUNCTION getObjectType RETURNS CHARACTER ( ) :
  IF TARGET-PROCEDURE = THIS-PROCEDURE THEN
    RETURN "super":U.
  ELSE
    RETURN "BusinessEntity":U.
END FUNCTION.

/* ************ Procedures ****************************************/
PROCEDURE createObject:
  CREATE ttProperty.
  ttProperty.TargetProcedure = TARGET-PROCEDURE.
END PROCEDURE .

PROCEDURE destroyObject:

  DEFINE VARIABLE hService AS HANDLE    NO-UNDO.
  hService = DYNAMIC-FUNCTION('getService':U IN TARGET-PROCEDURE).

  IF VALID-HANDLE  (hService) THEN
      DYNAMIC-FUNCTION('stopDataAccess':U IN hService, TARGET-PROCEDURE).

  findTarget(TARGET-PROCEDURE).
  DELETE ttProperty.
  DELETE PROCEDURE TARGET-PROCEDURE.
END PROCEDURE .

PROCEDURE fetchDataset:
/*------------------------------------------------------------------------------
  Purpose:  Fetch dataset
 -----------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER dataset-handle hdataset.
  hDataset = DYNAMIC-FUNCTION('getStaticDataset':U IN TARGET-PROCEDURE).
END PROCEDURE . /* fetchData */

PROCEDURE fetchData:
/*------------------------------------------------------------------------------
  Purpose:  Fetch data and/or definitions

Parameters:
     pcTables       - Tables for which to retrieve data or definitions
     pcQueries      - List of queries that corresponds to the requested tables
     pcRequest      - The actual request for the tables
     pcNumRecords   - Batch sizes
     phDataSet      - Prodataset handle (not dataset-handle)
     pcContext      - context
 -----------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER pcTables       AS CHARACTER NO-UNDO.
  DEFINE INPUT        PARAMETER pcQueries      AS CHARACTER NO-UNDO.
  DEFINE INPUT        PARAMETER pcRequests     AS CHARACTER NO-UNDO.
  DEFINE INPUT        PARAMETER piNumRecords   AS INTEGER   NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcContext      AS CHARACTER NO-UNDO.
  DEFINE OUTPUT       PARAMETER dataset-handle phDataset.

  DEFINE VARIABLE hAccess  AS HANDLE    NO-UNDO.

  /* don't fetch for design time request */
  IF pcTables > '' AND pcRequests <> 'DEFS':U THEN
  DO:
    hAccess = DYNAMIC-FUNCTION('getDataAccessObject':U IN TARGET-PROCEDURE).
    RUN fetchData IN hAccess
                    (pcTables,
                     pcQueries,
                     pcRequests,
                     piNumRecords,
                     INPUT-OUTPUT pcContext,
                     OUTPUT dataset-handle phDataset BY-REFERENCE) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
      RETURN ERROR IF RETURN-VALUE > '' THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1).

    /* receive hook/event */
    IF ProcedureHelper:HasInternalProcedure (TARGET-PROCEDURE, "receiveData":U) THEN
        RUN receiveData IN target-procedure(INPUT-OUTPUT dataset-handle phDataset BY-REFERENCE)
            NO-ERROR.

  END. /* runtime request */
END PROCEDURE . /* fetchData */

PROCEDURE saveChanges:
/*------------------------------------------------------------------------------
   Purpose: Save data
Parameters: phDataSet      - Prodataset with changes
----------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dataset-handle phDataset.

  DEFINE VARIABLE hAccess  AS HANDLE    NO-UNDO.

  hAccess = DYNAMIC-FUNCTION('getDataAccessObject':U IN TARGET-PROCEDURE).

  /* save hook */
  IF ProcedureHelper:HasInternalProcedure (TARGET-PROCEDURE, "validateData":U) THEN
      RUN validateData IN target-procedure(INPUT-OUTPUT dataset-handle phDataset BY-REFERENCE)
          NO-ERROR.
  IF NOT phDataset:error THEN
  DO:
    RUN saveChanges IN hAccess(INPUT-OUTPUT dataset-handle phDataset BY-REFERENCE)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      RETURN ERROR IF RETURN-VALUE > '' THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1).
    /* receive hook */
    IF ProcedureHelper:HasInternalProcedure (TARGET-PROCEDURE, "receiveData":U) THEN
        RUN receiveData IN target-procedure(INPUT-OUTPUT dataset-handle phDataset BY-REFERENCE)
            NO-ERROR.
  END.
END PROCEDURE .

/* used by locateProcedure (see locateprocedure.i) */
PROCEDURE isRunning:
  DEFINE OUTPUT PARAMETER phThis AS HANDLE NO-UNDO.
  phThis = THIS-PROCEDURE.
END PROCEDURE .


