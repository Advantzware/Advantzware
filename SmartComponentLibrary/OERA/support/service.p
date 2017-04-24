/*------------------------------------------------------------------------
    File        : service.p
    Purpose     : Request service
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{Consultingwerk/products.i}

define temp-table ttEntity no-undo
  field ProcHandle as handle
  field DAOHandle  as handle
  field Name       as character
  field NumClients as integer
  index ProcName as unique name
  index ProcHandle as unique Prochandle
  index DAOHandle DAOhandle.

define variable glAllowSource as logical   no-undo.
define variable gcPath        as character no-undo.

/* ************************  Function Prototypes ***************** */

/* ------ Properties ------------------------ */

function getAllowSource returns logical
   ( ) forward:

function setAllowSource returns logical
   (plAllow as logical ) forward:

function getPath returns character
   ( ) forward:

function setPath returns logical
   ( pcPath as character) forward:

function getTypePath returns character
   (pcType as CHARACTER) forward:


/* ------ Function Prototypes ---------------------- */
function getEntity returns handle
   ( pcName   as character) forward:

function getDataAccess returns handle private
   ( pcName as character) forward:

function getProcedure returns handle private
   ( pcType as character,
     pcName as character,
     plNew  as logical) forward:

function getPhysicalName returns character private
    ( pcType as character,
      pcName as character) forward:

function startProcedure returns handle private
   ( pcType as character,
     pcName as character) forward:

FUNCTION validateBusinessEntityName RETURNS LOGICAL
  ( pcEntityName AS CHARACTER ) FORWARD.

/* ***************************  Main Block  *************************** */
/*set default props */
setAllowSource(true).
setPath('service':U).

/* **********************  Internal Procedures  *********************** */
procedure destroyObject:
  delete object this-procedure.
end PROCEDURE .

procedure fetchData:
/*------------------------------------------------------------------------------
   Purpose:  Fetch data and/or definitions for a Business Entity.
Parameters:
     pcEntity       - Business Entity
     pcTables       - Tables for which to retrieve data or definitions
     pcQueries      - List of queries that corresponds to the requested tables
     pcRequest      - The actual request for the tables
     phDataSet      - Prodataset handle
     pcContext      - context
 -----------------------------------------------------------------------------*/
 define input        parameter pcEntity       as character no-undo.
 define input        parameter pcTables       as character no-undo.
 define input        parameter pcQueries      as character no-undo.
 define input        parameter pcRequests     as character no-undo.
 define input        parameter piNumRecords   as integer   no-undo.
 define input-output parameter pcContext      as character no-undo.
 define output       parameter phDataSet      as handle    no-undo.

 define variable hEntity as handle    no-undo.

 hEntity = getEntity(pcEntity).

 if valid-handle(hEntity) then
 do:
   /* Mike Fechner, Consultingwerk Ltd. 12.06.2009
      Bug 1839: Set Context in Business Entity */
   RUN setBusinessEntityContext (pcContext, hEntity) .

   run fetchData in hEntity
                   (pcTables,
                    pcQueries,
                    pcRequests,
                    piNumRecords,
                    input-output pcContext,
                    output dataset-handle phDataset by-reference)
                    no-error.
   if error-status:error then
       return error if return-value > '' then return-value else error-status:get-message(1) .

   /* Mike Fechner, Consultingwerk Ltd. 12.06.2009
      Bug 1839: Return updated context string */
   RUN getBusinessEntityContext (INPUT-OUTPUT pcContext, hEntity) .
 end.
 else
   return error SUBSTITUTE ('Business Entity "&1" does not exist or did not start.'{&TRAN}, pcEntity) .

end PROCEDURE . /* fetchData */

procedure fetchDataset:
/*------------------------------------------------------------------------------
   Purpose:  Fetch definitions for a Business Entity.
Parameters:
     pcEntity -  Business Entity
     Notes:
 -----------------------------------------------------------------------------*/
  define input  parameter pcEntity       as character no-undo.
  define output parameter phDataset      as handle    no-undo.

  define variable hEntity as handle    no-undo.

  hEntity = getEntity(pcEntity).

  if valid-handle(hEntity) then
  do:
    run fetchDataset in hEntity
               (output dataset-handle phDataset by-reference) no-error.
  end.
  else
    return error SUBSTITUTE ('Business Entity "&1" does not exist or did not start.'{&TRAN}, pcEntity) .

  if error-status:error then
      return error if return-value > '' then return-value else error-status:get-message(1) .

end PROCEDURE . /* fetchDataset */



PROCEDURE getBusinessEntityContext:
/*------------------------------------------------------------------------------
  Purpose: Return updated context string
  Notes:   See Bug 1839
------------------------------------------------------------------------------*/

    DEFINE INPUT-OUTPUT PARAMETER pcContext AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER phEntity  AS HANDLE    NO-UNDO.

    DEFINE VARIABLE iPos           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cEntityContext AS CHARACTER NO-UNDO.

    ASSIGN
        iPos = INDEX(pcContext, CHR(1))
        cEntityContext = DYNAMIC-FUNCTION ("getContext":U IN phEntity).

    IF cEntityContext = ? THEN
        ASSIGN cEntityContext = "?":U .

    IF iPos = 0 THEN DO:
        IF cEntityContext > "":U THEN
            ASSIGN pcContext = pcContext + CHR(1) + cEntityContext.
    END.
    ELSE DO:
        ASSIGN pcContext = SUBSTRING (pcContext, 1, iPos, "CHARACTER":U) + CHR(1) + cEntityContext.
    END.

END PROCEDURE.

procedure saveChanges:
 /*-----------------------------------------------------------------------------
  Purpose:  Save changes for a Business Entity

Parameters:
     pcEntity       - Business Entity
     phDataSet      - Prodataset handle (not dataset-handle)
----------------------------------------------------------------------------*/
 define input        parameter pcEntity  as character no-undo.
 define input-output parameter phDataset as HANDLE    NO-UNDO.

 define variable hEntity as handle    no-undo.
 hEntity = getEntity(pcEntity).
 if valid-handle(hEntity) then
   run saveChanges in hEntity (input-output dataset-handle phDataset by-reference).
 else
   return error SUBSTITUTE ('Business Entity "&1" does not exist or did not start.'{&TRAN}, pcEntity) .

 if error-status:error then
     return error if return-value > '' then return-value else error-status:get-message(1) .

end PROCEDURE .


PROCEDURE setBusinessEntityContext:
/*------------------------------------------------------------------------------
  Purpose: Set Context in Business Entity
  Notes:   See Bug 1839
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER pcContext AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER phEntity  AS HANDLE    NO-UNDO.

    DEFINE VARIABLE iPos           AS INTEGER   NO-UNDO.

    ASSIGN iPos = INDEX(pcContext, CHR(1)) .

    IF iPos > 0 THEN
        DYNAMIC-FUNCTION ("setContext":U IN phEntity, SUBSTRING(pcContext, iPos + 1, -1, "CHARACTER":U)) .
    ELSE
        DYNAMIC-FUNCTION ("setContext":U IN phEntity, "":U) .


END PROCEDURE.

procedure stopEntity:
  define input parameter pcName as character no-undo.

  define buffer btEntity for ttEntity.
  find btEntity where btEntity.Name = pcName no-error.
  if available btEntity then
  do:
    run destroyObject in btEntity.ProcHandle.
    delete btEntity.
  end.
end PROCEDURE .

/* ************************  Function Implementations ***************** */
function getEntity returns handle
   ( pcName    as character):

  define buffer btEntity for ttEntity.

  define variable hProc as handle    no-undo.

  find btEntity where btEntity.Name = pcName no-error.
  if not available btEntity or not valid-handle(btEntity.ProcHandle) then
  do:
    hProc = startProcedure('BE':U,pcName).
    if valid-handle(hProc) and not available btEntity then
    do:
      create btEntity.
      assign btEntity.Name       = pcName.
    end.

    IF AVAILABLE btEntity THEN
        btEntity.ProcHandle = hProc.

    IF VALID-HANDLE (hProc) THEN
        dynamic-function("setService":U in hProc,target-procedure).
  end.
  return if available  btEntity then btEntity.ProcHandle else ?.
end function. /* getEntity */

function getDataAccess returns handle private
   ( pcName as character):
  define buffer btEntity for ttEntity.


  find btEntity where btEntity.Name = pcName no-error.
  if available btEntity and not valid-handle(btEntity.DAOHandle) then
    btEntity.DAOHandle = startProcedure('DA':U,pcName).

  return if available btEntity then btEntity.DAOHandle else ?.

end function. /* getDataAccess  */

function getPhysicalName returns character private
   ( pcType as character,
     pcName as character):
  /* don't allow name client name to reference affect casing
     (does not allow multi casing .. ) */
  if getTypePath(pcType) = '' then
    return lc(pcType) + caps(substring(pcName,1,1, "CHARACTER":U))
                      + lc(substring(pcName,2, -1, "CHARACTER":U)).
  else
    return lc(pcName).

end function. /* getPhysicalName */

/***  startDataAceess is to be called from entity with handle
      Could have used name, but that would make it look very public..
      (feel free to add function prototype) **/
function startDataAccess returns handle
   ( phEntity as handle):
  define buffer btEntity for ttEntity.
  find btEntity where btEntity.ProcHandle = phEntity no-error.
  if available btEntity then
    return getDataAccess(btEntity.Name).
END FUNCTION . /* startDataAccess */

/***  stopDataAceess is to be called from entity with handle
      Could have used name, but that would make it look very public..
      (feel free to add function prototype) **/
function stopDataAccess returns logical
   ( phEntity as handle):
  define buffer btEntity for ttEntity.
  define buffer btDAO for ttEntity.
  find btEntity where btEntity.ProcHandle = phEntity no-error.
  if available btEntity then
  do:
    /* allow sharing of data access objects only destroy if not in use */
    if not can-find(first btDAO where btDao.DAOHandle = btEntity.DAOHandle
                                and   btDAo.ProcHandle <> btEntity.DAOHandle) then
      run destroyObject in btEntity.DAOHandle.
    btEntity.DAOHandle = ?.
    return true.
  end.
  return false.
end FUNCTION . /* startDataAccess */

function startProcedure returns handle private
   ( pcType   as character,
     pcName   as character):

  define variable hdl       as handle    no-undo.
  define variable cDir      as character no-undo.
  define variable cSubDir   as character no-undo.

  assign
    cDir    = getPath()
    cSubDir = getTypePath(pcType)
    pcName  = getPhysicalName(pcType,pcName)
    pcName = (if cDir > '' then cDir + '/':U else '':U)
           + (if cSubDir > '' then cSubdir + '/':U else '':U)
           + pcName
           + (if getAllowSource() then '.p':U else '.r':U)
    pcName = search(pcName).

  if pcName <> ? then
    run value(pcName) persistent set hdl.

  return hdl.
end function. /* startProcedure */

/*----------- Properties ----------------------*/
function setPath returns logical
   ( pcPath as character):
  gcPath = pcPath.
  return true.
end FUNCTION .  /* setPath */

function getPath returns character
   ( ) :
   return gcPath.
end FUNCTION . /* getPath */

function getTypePath returns character
   (pcType as CHARACTER) :
   /*
   case pcType.
     when 'be' then
       return 'entity'.
     when 'da' then
       return 'dataaccess'.
     when 'sc' then
       return 'datasource'.
   end.
   */
   return ''.
end FUNCTION . /* getTypePath */

function setAllowSource returns logical
   ( plAllow as logical):
  glAllowSource = plAllow.
  return true.
end FUNCTION . /* setAllowSource */

function getAllowSource returns logical
   ( ) :
   return glAllowSource.
end FUNCTION .  /* getAllowSource */


FUNCTION validateBusinessEntityName RETURNS LOGICAL
    ( pcEntityName AS CHARACTER ):
/*------------------------------------------------------------------------------
  Purpose: Validates a BusinessEntity Name
  Notes:
------------------------------------------------------------------------------*/

    DEFINE VARIABLE hEntity AS HANDLE NO-UNDO.

    ASSIGN hEntity = getEntity (pcEntityName) .

    RETURN VALID-HANDLE (hEntity) .

    @SuppressUnusedWarnings.
    CATCH err AS Progress.Lang.Error :
      RETURN FALSE .
    END CATCH.

END FUNCTION.
