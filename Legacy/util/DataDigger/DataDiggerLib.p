&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
  Name         : DataDigger.p
  Description  : Launcher for DataDigger.
  ---------------------------------------------------------------------- 
  15-10-2009 pti Created
  ----------------------------------------------------------------------*/

/* Var to 'cache' the value of the user since it won't change */  
define variable gcUserName   as character   no-undo initial ?.

/* Buildnr, temp-tables and forward defs */
{ DataDigger.i }

procedure GetUserNameA external "ADVAPI32.DLL":
    define output       parameter chrUserID     as character no-undo.
    define input-output parameter intBufferSize as long      no-undo.
    define return       parameter intResult     as short     no-undo.
end procedure.

procedure GetKeyboardState external "user32.dll":
    define input  parameter KBState as long. /* memptr */ 
    define return parameter RetVal  as long. /* bool   */ 
end procedure.

/* Windows API entry point */
procedure ShowScrollBar external "user32.dll":
    define input  parameter hwnd        as long.
    define input  parameter fnBar       as long.
    define input  parameter fShow       as long.
    define return parameter ReturnValue as long.
end procedure.

procedure SendMessageA external "user32.dll":
    define input  parameter hwnd   as long no-undo.
    define input  parameter wmsg   as long no-undo.
    define input  parameter wparam as long no-undo.
    define input  parameter lparam as long no-undo.
    define return parameter rc     as long no-undo.
end procedure.

procedure RedrawWindow external "user32.dll":
    def input parameter v-hwnd  as long no-undo.
    def input parameter v-rect  as long no-undo.
    def input parameter v-rgn   as long no-undo.
    def input parameter v-flags as long no-undo.
    def return parameter v-ret  as long no-undo.
end procedure.

procedure SetWindowTextA external "user32.dll":
  define input parameter hwnd as long.
  define input parameter txt as character.
end procedure.

procedure GetWindow external "user32.dll" :
  define input parameter hwnd as long.
  define input parameter uCmd as long.
  define return parameter hwndOther as long.
end procedure.

procedure GetParent external "user32.dll" :
  define input parameter hwndChild as long.
  define return parameter hwndParent as long.
end procedure.

procedure GetCursorPos external "user32.dll" : 
  define input-output parameter lRect as memptr. 
end. 

procedure ScreenToClient external "user32.dll" :
  define input  parameter hWnd     as long.
  define input  parameter lpPoint  as memptr.
end procedure.

/* Needed for upgrade of DataDigger */
procedure URLDownloadToFileA external "URLMON.DLL" :
   define input parameter pCaller    as long.
   define input parameter szURL      as character.
   define input parameter szFilename as character.
   define input parameter dwReserved as long.
   define input parameter lpfnCB     as long.
   define return parameter ReturnValue as long.
end procedure.

procedure DeleteUrlCacheEntry external "WININET.DLL" :
   define input parameter lbszUrlName as character.
end procedure.

/* Transparency */
PROCEDURE SetWindowLongA EXTERNAL "user32.dll":
 def INPUT PARAM HWND AS LONG.
 def INPUT PARAM nIndex AS LONG.
 def INPUT PARAM dwNewLong AS LONG.
 DEF RETURN PARAM stat AS LONG.
END.

PROCEDURE SetLayeredWindowAttributes EXTERNAL "user32.dll":
 def INPUT PARAM HWND AS LONG.
 def INPUT PARAM crKey AS LONG.
 def INPUT PARAM bAlpha AS SHORT.
 def INPUT PARAM dwFlagsas AS LONG.
 DEF RETURN PARAM stat AS SHORT.
END.


/* Find out if a file is locked */
&GLOBAL-DEFINE GENERIC_WRITE         1073741824 /* &H40000000 */
&GLOBAL-DEFINE OPEN_EXISTING         3
&GLOBAL-DEFINE FILE_SHARE_READ       1          /* = &H1 */
&GLOBAL-DEFINE FILE_ATTRIBUTE_NORMAL 128        /* = &H80 */

PROCEDURE CreateFileA EXTERNAL "kernel32":
    DEFINE INPUT PARAMETER lpFileName AS CHARACTER.
    DEFINE INPUT PARAMETER dwDesiredAccess AS LONG.
    DEFINE INPUT PARAMETER dwShareMode AS LONG.
    DEFINE INPUT PARAMETER lpSecurityAttributes AS LONG.
    DEFINE INPUT PARAMETER dwCreationDisposition AS LONG.
    DEFINE INPUT PARAMETER dwFlagsAndAttributes AS LONG.
    DEFINE INPUT PARAMETER hTemplateFile AS LONG.
    DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE CloseHandle EXTERNAL "kernel32" :
  DEFINE INPUT  PARAMETER hObject     AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.



define temp-table ttWidget no-undo rcode-information
  field hWidget as handle
  field iPosX   as integer
  field iPosY   as integer
  field iWidth  as integer
  field iHeight as integer
  index iPrim as primary hWidget.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-formatQueryString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD formatQueryString Procedure 
FUNCTION formatQueryString RETURNS CHARACTER
  ( input pcQueryString as character
  , input plExpanded    as logical )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getColumnWidthList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getColumnWidthList Procedure 
FUNCTION getColumnWidthList returns character
  ( input phBrowse as handle ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatabaseList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatabaseList Procedure 
FUNCTION getDatabaseList returns character FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getImagePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getImagePath Procedure 
FUNCTION getImagePath returns character
  ( pcImage as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKeyList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKeyList Procedure 
FUNCTION getKeyList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLinkInfo Procedure 
FUNCTION getLinkInfo returns character
  ( input pcFieldName as character
  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMatchesValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMatchesValue Procedure 
FUNCTION getMatchesValue RETURNS CHARACTER
  ( hFillIn as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMaxLength) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMaxLength Procedure 
FUNCTION getMaxLength RETURNS integer
  ( cFieldList as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOsErrorDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOsErrorDesc Procedure 
FUNCTION getOsErrorDesc returns character
  (input piOsError as integer) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrimaryFields Procedure 
FUNCTION getPrimaryFields returns character
  ( input pcDatabaseName as character
  , input pcTableName    as character  
    )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProgramDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProgramDir Procedure 
FUNCTION getProgramDir RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuery Procedure 
FUNCTION getQuery returns character
  ( input pcDatabase as character
  , input pcTable    as character
  , input piQuery    as integer
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReadableQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReadableQuery Procedure 
FUNCTION getReadableQuery returns character
  ( input pcQuery as character ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRegistry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRegistry Procedure 
FUNCTION getRegistry returns character
    ( pcSection as character
    , pcKey     as character 
    )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTableList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTableList Procedure 
FUNCTION getTableList returns character
  ( input  pcDatabaseFilter   as character
  , input  pcTableFilter      as character
  , input  plShowHiddenTables as logical  
  , input  pcSortField        as character
  , input  plAscending        as logical  
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUserName Procedure 
FUNCTION getUserName returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isDefaultFontsChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isDefaultFontsChanged Procedure 
FUNCTION isDefaultFontsChanged returns logical
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isFileLocked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isFileLocked Procedure 
FUNCTION isFileLocked RETURNS LOGICAL
  ( pcFileName as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isWidgetChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isWidgetChanged Procedure 
FUNCTION isWidgetChanged RETURNS LOGICAL PRIVATE
  ( input phWidget as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resolveOsVars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD resolveOsVars Procedure 
FUNCTION resolveOsVars RETURNS CHARACTER
  ( pcString as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resolveSequence) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD resolveSequence Procedure 
FUNCTION resolveSequence returns character
  ( pcString as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setColumnWidthList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setColumnWidthList Procedure 
FUNCTION setColumnWidthList returns logical
  ( input phBrowse    as handle 
  , input pcWidthList as character) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFilterFieldColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFilterFieldColor Procedure 
FUNCTION setFilterFieldColor returns logical
  ( phWidget as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLinkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLinkInfo Procedure 
FUNCTION setLinkInfo returns logical
  ( input pcFieldName as character
  , input pcValue     as character
  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRegistry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRegistry Procedure 
FUNCTION setRegistry returns character
    ( pcSection as character
    , pcKey     as character
    , pcValue   as character
    )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 25.67
         WIDTH              = 34.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
do:
  define variable cEnvironment as character   no-undo.
  cEnvironment = substitute('DataDigger-&1', getUserName() ).

  unload 'DataDiggerHelp' no-error.
  unload 'DataDigger'     no-error.
  unload cEnvironment     no-error.
end. /* CLOSE OF THIS-PROCEDURE  */

/* Event DiggerLib will provide the handle 
 * of the persistent proc 
 */
subscribe to 'DiggerLib' anywhere.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-applyChoose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyChoose Procedure 
PROCEDURE applyChoose :
/*------------------------------------------------------------------------
  Name         : applyChoose
  Description  : Apply the choose event to a widget. Is used in dynamic
                 creation of widgets.
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/
  define input  parameter pihWidget as handle no-undo.
  
  if valid-handle(pihWidget) then apply 'choose' to pihWidget.

end procedure. /* applyChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyEvent Procedure 
PROCEDURE applyEvent :
/*------------------------------------------------------------------------
  Name         : applyEvent
  Description  : Apply an event to a widget. Is used in dynamic
                 creation of widgets.
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/
  define input  parameter pihWidget as handle no-undo.
  define input  parameter pcEvent   as character   no-undo.
  
  if valid-handle(pihWidget) then apply pcEvent to pihWidget.

end procedure. /* applyEvent */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkDir Procedure 
PROCEDURE checkDir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter pcFileName as character   no-undo.
  define output parameter pcError    as character   no-undo.

  define variable cDumpDir     as character   no-undo.
  define variable cDirToCreate as character   no-undo.
  define variable iDir         as integer     no-undo.

  /* Already exist. Overwrite? */
  file-info:file-name = pcFileName.

  if file-info:full-pathname <> ? then 
  do:
    if file-info:file-type matches '*F*' then
    do:
      run showHelp('OverwriteDumpFile', pcFileName).
      if getRegistry('DataDigger:help', 'OverwriteDumpFile:answer') <> '1' then 
      do:
        pcError = 'Aborted by user.'.
        return. 
      end.
  
      /* Write access to this file? */
      if not file-info:file-type matches '*W*' then 
      do:
        pcError = substitute('Cannot overwrite output file "&1"', pcFileName).
        return. 
      end.
    end.

    /* If a dir already exists with the same name as the output file, we cannot create it */
    if file-info:file-type matches '*D*' then
    do:
      pcError = substitute('A directory named "&1" exists; cannot create a file with the same name.', pcFileName).
      return. 
    end. 
  end.

  /* Check dir */
  cDumpDir = substring(pcFileName, 1, r-index(pcFileName,"\")).
  file-info:file-name = cDumpDir.
  if file-info:full-pathname = ? then
  do:
    run showHelp('CreateDumpDir', cDumpDir).
    if getRegistry('DataDigger:help', 'CreateDumpDir:answer') <> '1' then 
    do:
      pcError = 'Aborted by user.'.
      return. 
    end.
      
  end.

  /* Try to create path + file. Progress will not raise an error if it already exists */
  cDirToCreate = entry(1,cDumpDir,'\').
  do iDir = 2 to num-entries(cDumpDir,'\').

    /* In which dir do we want to create a subdir? */
    if iDir = 2 then
      file-info:file-name = cDirToCreate + '\'.
    else 
      file-info:file-name = cDirToCreate.

    /* Does it even exist? */
    if file-info:full-pathname = ? then
    do:
      pcError = substitute('Directory "&1" does not exist.', cDirToCreate).
      return.
    end.

    /* Check if the dir is writable */
    if file-info:file-type matches '*X*'  /* Happens on CD-ROM drives */
      or (        file-info:file-type matches '*D*'
          and not file-info:file-type matches '*W*' ) then 
    do:
      pcError = substitute('No write-access to directory: "&1"', cDirToCreate).
      return. 
    end.

    /* Seems to exist and to be writable. */
    cDirToCreate = cDirToCreate + '\' + entry(iDir,cDumpDir,'\'). 

    /* If a file already exists with the same name, we cannot create a dir */
    file-info:file-name = cDirToCreate.
    if file-info:file-type matches '*F*' then 
    do:
      pcError = substitute('A file named "&1" exists; cannot create a dir with the same name.', cDirToCreate).
      return. 
    end.

    /* Create the dir. Creating an existing dir gives no error */
    os-create-dir value(cDirToCreate). 
    if os-error <> 0 then
    do:
      pcError = getOsErrorDesc(os-error).
      return.
    end. /* error */

  end. /* iDir */
  
end procedure. /* checkDir */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkForUpgrade) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForUpgrade Procedure 
PROCEDURE checkForUpgrade :
define output parameter pcLatestVersion    as character no-undo. 
  define output parameter pcDownloadLocation as character no-undo. 
  define output parameter pcError            as character no-undo. 

  define variable cTargetLocation as character no-undo.
  define variable cUrl            as character no-undo initial "http://www.oehive.org/project/DataDigger".
  define variable iResult         as integer   no-undo.
  define variable cData           as longchar  no-undo.
  define variable iTag            as integer   no-undo.
  define variable cThisTag        as character no-undo.
  define variable cNextTag        as character no-undo.
  
  /* Save the website from the OEHIVE somewhere local */
  cTargetLocation = session:temp-dir + 'DataDigger.html'.

  /* Download the project page for DataDigger from oehive */
  run urlDownloadToFileA (0, cUrl, cTargetLocation, 0, 0, output iResult).
  
  /* Error? */
  if iResult <> 0 then 
  do:
    pcError = substitute('Cannot retrieve information from "&1".', cUrl).
    return.
  end.
  
  /* Clean up from the cache */
  run DeleteURLCacheEntry (input cUrl). /*remove from IE cache*/
  
  /* Parse the file */
  copy-lob file cTargetLocation to cData.
  
  /* Walk thru all HTML-tags */
  do iTag = 1 to num-entries(cData,'<') - 1:
    cThisTag = trim(entry(iTag,cData,'<')).
    cNextTag  = trim(entry(iTag + 1,cData,'<')).
  
    cThisTag = replace(cThisTag,'>', ' ').
    cNextTag = replace(cNextTag,'>', ' ').
  
    if cThisTag begins '/' then next. 
    if lookup(entry(1,cThisTag,' '), 'TD,LI') = 0 then next. 
  
    if cThisTag matches '*class="release-title"*'
      and num-entries(cNextTag,'"') >= 3 then
      pcLatestVersion = trim(entry(3,cNextTag,'"')).
    else
    if cThisTag matches '*class="first project_release_download"*'
      and num-entries(cNextTag,'"') >= 2 then
      pcDownloadLocation = trim(entry(2,cNextTag,'"')).
  
    /* We are only interested in the first one, so: */
    if pcDownloadLocation <> '' then leave.
  end.

  if pcDownloadLocation = '' then
    pcError = 'No information found on OEHIVE website.'.
end procedure. /* checkForUpgrade */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-collectQueryInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE collectQueryInfo Procedure 
PROCEDURE collectQueryInfo :
/*------------------------------------------------------------------------
  Name         : collectQueryInfo
  Description  : Fill the query temp-table
  ---------------------------------------------------------------------- 
  03-11-2009 pti Created
  ----------------------------------------------------------------------*/
 
 define input  parameter pcDatabase     as character   no-undo.
 define input  parameter pcTable        as character   no-undo.

 define variable iMaxQueryHistory as integer no-undo. 
 define variable iQueryNr         as integer no-undo. 
 define variable iLoop            as integer no-undo. 
 define variable cSetting         as character no-undo. 

 define buffer bQuery for ttQuery.

 /* Delete all known queries in memory of this table */
 for each bQuery 
   where bQuery.cDatabase = pcDatabase
     and bQuery.cTable    = pcTable:
   delete bQuery.
 end. 

 iMaxQueryHistory = integer(getRegistry("DataDigger", "MaxQueryHistory" )).
 if iMaxQueryHistory = 0 then return. /* no query history wanted */

 /* If it is not defined use default setting */
 if iMaxQueryHistory = ? then iMaxQueryHistory = 10. 

 collectQueries:
 do iLoop = 1 to iMaxQueryHistory:
   cSetting = getRegistry ( substitute('DB:&1', pcDatabase )
                          , substitute('&1:query:&2', pcTable, iLoop )
                          ).

   if cSetting = '<Empty>' then next.

   if cSetting <> ? then
   do:
     create bQuery.
     assign iQueryNr         = iQueryNr + 1
            bQuery.cDatabase = pcDatabase 
            bQuery.cTable    = pcTable
            bQuery.iQueryNr  = iQueryNr
            bQuery.cQueryTxt = cSetting.
   end.
   else 
     leave collectQueries.

 end. /* 1 .. MaxQueryHistory */

end procedure. /* collectQueryInfo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DiggerLib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DiggerLib Procedure 
PROCEDURE DiggerLib :
/*------------------------------------------------------------------------
  Name         : DiggerLib
  Description  : Return the handle of the procedure. 
  ----------------------------------------------------------------------*/
  define output parameter phDiggerLib as handle no-undo. 

  phDiggerLib = this-procedure:handle. 

end procedure. /* DiggerLib */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dumpRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpRecord Procedure 
PROCEDURE dumpRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter pcAction   as character   no-undo.
  define input  parameter phSource   as handle      no-undo.
  define output parameter plContinue as logical     no-undo.

  define variable hExportTT          as handle      no-undo.
  define variable hExportTtBuffer    as handle      no-undo.
  define variable hBuffer            as handle      no-undo.
  define variable cFileName          as character   no-undo.
  define variable cError             as character   no-undo.
  define variable cMessage           as character   no-undo.
  define variable iRow               as integer     no-undo.
  define variable cDumpProg          as character   no-undo.
  define variable lContinue          as logical     no-undo.
  define variable lDefaultDump       as logical     no-undo.

  if not valid-handle(phSource) then return.

  /* Determine appropriate buffer and populate an intermediate tt
   * with the data to export
   */
  case phSource:type:
    when 'buffer' then 
    do:
      hBuffer = phSource.

      /* Create temptable-handle... */
      create temp-table hExportTt.
      hExportTt:create-like(substitute("&1.&2", hBuffer:dbname, hBuffer:table)).

      /* Prepare the TempTable... */
      hExportTt:temp-table-prepare(substitute("&1_&2", hBuffer:dbname, hBuffer:table)).
      hExportTtBuffer = hExportTt:default-buffer-handle.
      hExportTtBuffer:buffer-create().
      hExportTtBuffer:buffer-copy(hBuffer).
    end.

    when 'browse' then 
    do:
      hBuffer = phSource:query:get-buffer-handle(1).

      /* Create temptable-handle... */
      create temp-table hExportTt.
      hExportTt:create-like(substitute("&1.&2", hBuffer:dbname, hBuffer:table)).

      /* Prepare the TempTable... */
      hExportTt:temp-table-prepare(substitute("&1_&2", hBuffer:dbname, hBuffer:table)).
      hExportTtBuffer = hExportTt:default-buffer-handle.

      /* Copy the records */
      do iRow = 1 to phSource:num-selected-rows:
        phSource:fetch-selected-row(iRow).
        hExportTtBuffer:buffer-create().
        hExportTtBuffer:buffer-copy(hBuffer).
      end.
    end.

    otherwise return. 
  end case.


  /* Determine the default name to save to */
  run getDumpFileName
    ( input pcAction        /* Dump | Delete | Save */
    , input hBuffer:dbname    
    , input hBuffer:table     
    , input 'XML'
    , output cFileName
    ).

  run checkDir(input cFileName, output cError).
  if cError <> "" then 
  do:
    message cError view-as alert-box info buttons ok.
    return. 
  end.


  /* See if the user has specified his own dump program */
  cDumpProg = getRegistry("DumpAndLoad", substitute("&1Prog", pcAction)).
  if search(cDumpProg) <> ? then
  do:
    run value(cDumpProg) 
      ( input pcAction
      , input hBuffer:dbname 
      , input hBuffer:table
      , input hExportTt
      , input cFileName
      , output cMessage
      , output lDefaultDump
      , output plContinue
      ).
    if cMessage <> "" then 
      message cMessage view-as alert-box info buttons ok.

    if not lDefaultDump then
      return. 
  end.

  hExportTT:write-xml
    ( 'file'        /* TargetType     */
    , cFileName     /* File           */
    , yes           /* Formatted      */
    , ?             /* Encoding       */
    , ?             /* SchemaLocation */
    , no            /* WriteSchema    */
    , no            /* MinSchema      */
    ).

  delete object hExportTt.

end procedure. /* dumpRecord */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dynamicDump) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dynamicDump Procedure 
PROCEDURE dynamicDump :
/*------------------------------------------------------------------------
  Name         : dynamicDump
  Description  : Dump the data to a file that is similar to those of   
                 Progress self. Add a checksum and nr of records at the
                 end of the file. 
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pihBrowse as handle      no-undo.
  define input parameter picFile   as character   no-undo.

  define variable cDataType  as character   no-undo.
  define variable cTimeStamp as character   no-undo.
  define variable hBuffer    as handle      no-undo extent 5.
  define variable hColumn    as handle      no-undo.
  define variable hField     as handle      no-undo.
  define variable hQuery     as handle      no-undo.
  define variable iBack      as integer     no-undo.
  define variable iBuffer    as integer     no-undo.
  define variable iColumn    as integer     no-undo.
  define variable iExtent    as integer     no-undo.
  define variable iRecords   as integer     no-undo.
  define variable iTrailer   as integer     no-undo.
  define variable lFirst     as logical     no-undo.

  hQuery = pihBrowse:query.

  /* Accept max 5 buffers for a query */
  do iBuffer = 1 to min(5, hQuery:num-buffers):
    hBuffer[iBuffer] = hQuery:get-buffer-handle(iBuffer).
  end.

  assign
    iRecords   = 0
    cTimeStamp = string(year( today),"9999":u) + "/":u
               + string(month(today),"99":u  ) + "/":u
               + string(day(  today),"99":u  ) + "-":u
               + string(time,"HH:MM:SS":u).

  hQuery:get-first.

  /* Open outputfile */
  output to value(picFile) no-echo no-map.
  export ?.
  iBack = seek(output) - 1.
  seek output to 0.    

  repeat while not hQuery:query-off-end
  on stop undo, leave:

    assign 
      iRecords = iRecords + 1
      lFirst   = true
      .

    process events.
    
    browseColumn:
    do iColumn = 1 to pihBrowse:num-columns:

      /* Grab the handle */
      hColumn = pihBrowse:get-browse-column(iColumn).

      /* Skip invisible columns */
      if not hColumn:visible then next browseColumn.

      /* Find the buffer the column belongs to */
      SearchLoop:
      do iBuffer = 1 to 5:
        assign hField = hBuffer[iBuffer]:buffer-field(hColumn:name) no-error.
        if error-status:error = false 
          and hField <> ? then 
          leave SearchLoop.
      end.

      /* If no column found, something weird happened */
      if hField = ? then next browseColumn.

      if hField:data-type = "recid":u then next.
  
      if lFirst then
        lFirst = false.
      else
      do:
        seek output to seek(output) - iBack.
        put control ' ':u.
      end.
  
      if hField:extent > 1 then
      do iExtent = 1 to hField:extent:
        if iExtent > 1 then
        do:
          seek output to seek(output) - iBack.
          put control ' ':u.
        end.
  
        export hField:buffer-value(iExtent).
      end.
      else
        export hField:buffer-value.
    end. 

    hQuery:get-next().
  end.
  
  put unformatted ".":u skip.
  iTrailer = seek(output).
  
  put unformatted
         "PSC":u 
    skip "filename=":u hBuffer[1]:table 
    skip "records=":u  string(iRecords,"9999999999999":u) 
    skip "ldbname=":u  hBuffer[1]:dbname 
    skip "timestamp=":u cTimeStamp 
    skip "numformat=":u asc(session:numeric-separator) ",":u asc(session:numeric-decimal-point) 
    skip "dateformat=":u session:date-format "-":u session:year-offset 
    skip "map=NO-MAP":u 
    skip "cpstream=":u session:cpstream 
    skip ".":u 
    skip string(iTrailer,"9999999999":u) 
    skip.
  
  output close.

end procedure. /* dynamicDump */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dynamicLoad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dynamicLoad Procedure 
PROCEDURE dynamicLoad :
/*------------------------------------------------------------------------
  Name         : dynamicLoad
  Description  : Load data from a file into a buffer.
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  DEFINE INPUT  PARAMETER hTable AS HANDLE      NO-UNDO.
  DEFINE INPUT  PARAMETER cDelim AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE cImport AS CHARACTER   NO-UNDO EXTENT 2800.
  DEFINE VARIABLE iImp    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iCnt    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iExtnt  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE hFld    AS HANDLE      NO-UNDO.

  IF hTable:TYPE <> "BUFFER" THEN RETURN.
  
  REPEAT:
    ASSIGN cImport = ""
           iImp = 0.

    IMPORT cImport.
    hTable:BUFFER-CREATE().

    DO iCnt = 1 TO hTable:NUM-FIELDS:
      ASSIGN hFld = hTable:BUFFER-FIELD(iCnt).

      IF hFld:EXTENT = 0 THEN 
      DO:
        ASSIGN iImp = iImp + 1
               hFld:BUFFER-VALUE = cImport[iImp].
      END.
      ELSE 
      DO iExtnt = 1 TO hFld:EXTENT:
        ASSIGN iImp = iImp + 1
               hFld:BUFFER-VALUE(iExtnt) = cImport[iImp].
      END.
    END.

    hTable:BUFFER-VALIDATE().
  END.

END PROCEDURE. /* dynamicLoad */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getColumnSort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getColumnSort Procedure 
PROCEDURE getColumnSort :
/*------------------------------------------------------------------------
  Name         : getColumnSort
  Description  : Return the column nr the browse is sorted on
  ----------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phBrowse    AS HANDLE      NO-UNDO.
  DEFINE OUTPUT PARAMETER pcColumn    AS CHARACTER   NO-UNDO.
  DEFINE OUTPUT PARAMETER plAscending AS LOGICAL     NO-UNDO.
  
  DEFINE VARIABLE hColumn AS HANDLE      NO-UNDO.
  DEFINE VARIABLE iColumn AS INTEGER     NO-UNDO.

  do iColumn = 1 to phBrowse:num-columns:
    hColumn = phBrowse:get-browse-column(iColumn).
    if hColumn:sort-ascending <> ? then 
      assign
        pcColumn    = hColumn:name
        plAscending = hColumn:sort-ascending.
  end.

  if pcColumn = '' then
    assign
      pcColumn    = phBrowse:get-browse-column(1):name
      plAscending = true.

END PROCEDURE. /* getColumnSort */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDumpFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDumpFileName Procedure 
PROCEDURE getDumpFileName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter pcAction    as character   no-undo.
  define input  parameter pcDatabase  as character   no-undo.
  define input  parameter pcTable     as character   no-undo.
  define input  parameter pcExtension as character   no-undo.
  define output parameter pcFileName  as character   no-undo.

  define variable iFileNr     as integer     no-undo.
  define variable cDir        as character   no-undo extent 3.
  define variable cFile       as character   no-undo extent 3.
  define variable iAction     as integer     no-undo.
  define variable cAction     as character   no-undo.
  define variable cLastDir    as character   no-undo.
  define variable cDayOfWeek  as character   no-undo extent 7 initial ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'].

  &global-define action-list "Dump,Delete,Save"

  /* Read dirs and file templates for the various actions */
  do iAction = 1 to num-entries( {&action-list} ):
    cAction = entry(iAction, {&action-list} ).

    /* What directory ( DumpDir / DeleteDir / SaveDir ) */
    cDir[iAction] = getRegistry("DumpAndLoad", substitute("&1Dir", cAction)).
    if cDir[iAction] = ? then
    do:
      cDir[iAction] = lc( getProgramDir() + cAction + '\' ).
      setRegistry( "DumpAndLoad", substitute("&1Dir", cAction), '<PROGDIR>' + cAction ).
    end.
    cDir[iAction] = right-trim(cDir[iAction],'/\') + '\'.
    cDir[iAction] = resolveOsVars(cDir[iAction]).

    /* What file template ( DumpFileTemplate / DeleteFileTemplate / SaveFileTemplate ) */
    cFile[iAction] = getRegistry("DumpAndLoad", substitute("&1FileTemplate", cAction)).
    if cFile[iAction] = ? then
    do:
      cFile[iAction] = '<' + caps(cAction) + 'DIR><DB>\<TABLE>.<TIMESTAMP>.<#>.XML'.
      setRegistry( "DumpAndLoad", substitute("&1FileTemplate", cAction), cFile[iAction]).
    end.
    cFile[iAction] = resolveOsVars(cFile[iAction]).
  end.

  /* Find last used dir. Is saved as something like:
     DumpLastFileName=c:\Data\Progress\Sports\customer.xml
  */
  cLastDir = getRegistry("DumpAndLoad", "DumpLastFileName").
  if cLastDir = ? then cLastDir = "".
  cLastDir = substring(cLastDir,1,r-index(cLastDir,'\')).
  

  /* Set initial filename based on action */
  iAction = lookup(pcAction, {&action-list} ).
  if iAction = 0 then 
    pcFileName = pcAction. 
  else 
    pcFileName = cFile[iAction].

  /* Now resolve all tags */
  pcFileName = replace(pcFileName,"<DUMPDIR>"  , cDir[ 1 ]                   ).
  pcFileName = replace(pcFileName,"<DELETEDIR>", cDir[ 2 ]                   ).
  pcFileName = replace(pcFileName,"<SAVEDIR>"  , cDir[ 3 ]                   ).
  pcFileName = replace(pcFileName,"<LASTDIR>"  , cLastDir                    ).
  pcFileName = replace(pcFileName,"<PROGDIR>"  , getProgramDir()             ).

  pcFileName = replace(pcFileName,"<USERID>"   , userid(ldbname(1))          ).
  pcFileName = replace(pcFileName,"<DB>"       , pcDatabase                  ).
  pcFileName = replace(pcFileName,"<TABLE>"    , pcTable                     ).
  pcFileName = replace(pcFileName,"<EXT>"      , pcExtension                 ).

  pcFileName = replace(pcFileName,"<TIMESTAMP>", "<YEAR><MONTH><DAY>.<HH><MM><SS>"       ).
  pcFileName = replace(pcFileName,"<DATE>"     , "<YEAR>-<MONTH>-<DAY>"      ).
  pcFileName = replace(pcFileName,"<TIME>"     , "<HH>:<MM>:<SS>"            ).
  pcFileName = replace(pcFileName,"<WEEKDAY>"  , string(weekday(today))      ).
  pcFileName = replace(pcFileName,"<DAYNAME>"  , cDayOfWeek[weekday(today)]  ).

  pcFileName = replace(pcFileName,"<YEAR>"     , string(year (today),"9999") ).
  pcFileName = replace(pcFileName,"<MONTH>"    , string(month(today),  "99") ).
  pcFileName = replace(pcFileName,"<DAY>"      , string(day  (today),  "99") ).
  pcFileName = replace(pcFileName,"<HH>"       , entry(1,string(time,"HH:MM:SS"),":" ) ).
  pcFileName = replace(pcFileName,"<MM>"       , entry(2,string(time,"HH:MM:SS"),":" ) ).
  pcFileName = replace(pcFileName,"<SS>"       , entry(3,string(time,"HH:MM:SS"),":" ) ).
  
  pcFileName = resolveSequence(pcFileName).

  /* Make lower */
  pcFileName = lc(pcFileName).

end procedure. /* getDumpFileName */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFields Procedure 
PROCEDURE getFields :
/*------------------------------------------------------------------------
  Name         : getFields
  Description  : Fill the fields temp-table
  ---------------------------------------------------------------------- 
  18-03-2009 pti Created
  ----------------------------------------------------------------------*/
  
  define input parameter pcDatabase    as character   no-undo.
  define input parameter pcTableName   as character   no-undo.
  define output parameter table for ttField.

  define variable hBufferFile      as handle      no-undo.
  define variable hBufferField     as handle      no-undo.
  define variable hQuery           as handle      no-undo.
  define variable cPrimIndexFields as character   no-undo. 
  define variable cQuery           as character   no-undo. 
  define variable cSelectedFields  as character   no-undo. 
  define variable iFieldOrder      as integer     no-undo. 
  define variable cFieldOrder      as character   no-undo. 
  define variable iFieldExtent     as integer     no-undo. 
  define variable lShowRecidField  as logical     no-undo. 
  define variable lShowRowidField  as logical     no-undo. 

  /* Clean up first */
  empty temp-table ttField.

  /* Return if no db connected */
  if num-dbs = 0 then return. 

  /*
  ** Fill the tt with _Fields
  */
  create buffer hBufferFile  for table pcDatabase + "._File".                    
  create buffer hBufferField for table pcDatabase + "._Field".

  create query hQuery.
  hQuery:set-buffers(hBufferFile,hBufferField).

  cQuery = substitute("for each &1._File  where &1._file._file-name = '&2' no-lock, " +
                      "    each &1._Field of &1._File no-lock by _Order" 
                     , pcDatabase
                     , pcTableName
                     ).

  hQuery:query-prepare(cQuery).
  hQuery:query-open().
  hQuery:get-first().

  /* Get list of fields in primary index. */
  cPrimIndexFields = getPrimaryFields(pcDatabase, pcTableName).

  /* Get list of all previously selected fields */
  cSelectedFields = getRegistry( substitute('DB:&1',pcDatabase)
                               , substitute('&1:fields',pcTableName)
                               ).

  /* If none selected, set mask to 'all' */
  if cSelectedFields = ? then cSelectedFields = '*'.

  /* Get field ordering */
  cFieldOrder = getRegistry( substitute('DB:&1',pcDatabase)
                           , substitute('&1:fieldOrder', pcTableName )
                           ).
              
  repeat while not hQuery:query-off-end:

    do iFieldExtent = 0 to hBufferField:buffer-field('_Extent'):buffer-value:

      create ttField.
      assign 
        iFieldOrder         = iFieldOrder + 1
        ttField.lShow       = can-do(cSelectedFields, hBufferField:buffer-field('_field-name'):buffer-value)
        ttField.iOrder      = iFieldOrder
        ttField.iOrderOrg   = iFieldOrder
        ttField.cFieldName  = hBufferField:buffer-field('_field-name'):buffer-value
        ttField.cFullName   = hBufferField:buffer-field('_field-name'):buffer-value
        ttField.cDataType   = hBufferField:buffer-field('_data-type'):buffer-value
        ttField.cInitial    = hBufferField:buffer-field('_initial'):buffer-value   
        ttField.cFormat     = hBufferField:buffer-field('_format'):buffer-value     
        ttField.cFormatOrg  = hBufferField:buffer-field('_format'):buffer-value      
        ttField.cLabel      = hBufferField:buffer-field('_label'):buffer-value
        ttField.lPrimary    = lookup(ttField.cFieldName, cPrimIndexFields) > 0
        ttField.iExtent     = hBufferField:buffer-field('_Extent'):buffer-value
        ttField.lMetaField  = true
        ttField.lDataField  = true
        .

      /* Set the real extent field to not display as data */
      if ttField.iExtent > 0 and iFieldExtent = 0 then
        assign
          ttField.lMetaField = true
          ttField.lDataField = false
          .

      /* Create the individual extent fields */
      if iFieldExtent > 0 then
        assign
          ttField.iExtent    = iFieldExtent
          ttField.cFullName  = ttField.cFullName + substitute('[&1]', iFieldExtent)
          ttField.cDataType  = ttField.cDataType + substitute('[&1]', iFieldExtent)
          ttField.lMetaField = false
          ttField.lDataField = true
          .
  
      /* Some types should not be shown like CLOB BLOB and RAW */
      if lookup(ttField.cDataType, 'clob,blob,raw') > 0 then
        assign
          ttField.lMetaField = true
          ttField.lDataField = false
          .

      /* Restore changed field format. */
      ttField.cFormat = getRegistry( substitute('DB:&1',pcDatabase)
                                   , substitute('&1.&2:format',pcTableName,ttField.cFieldName) 
                                   ).
      if ttField.cFormat = ? then ttField.cFormat = ttField.cFormatOrg.
  
      /* Restore changed field order. */
      ttField.iOrder = lookup(ttField.cFieldName,cFieldOrder).
      if ttField.iOrder = ? then ttField.iOrder = ttField.iOrderOrg.
  
      /* Retrieve a formerly saved filter value */
      if logical(getRegistry ("DataDigger", "SaveDataFilters")) then
      do:
        ttField.cFilterValue = getRegistry( substitute('DB:&1',pcDatabase)
                                          , substitute('&1.&2:filter',pcTableName,ttField.cFullName)
                                          ). 
        if ttField.cFilterValue = ? then ttField.cFilterValue = "".
      end.
    end. /* For each extent nr */

    hQuery:get-next().
  end.
  hQuery:query-close().

  delete object hQuery.
  delete object hBufferField.
  delete object hBufferFile.


  /* Add a column for the recid */
  lShowRecidField = logical(getRegistry ("DataDigger", "AddDataColumnForRecid")).
  lShowRowidField = logical(getRegistry ("DataDigger", "AddDataColumnForRowid")).

  create ttField.
  assign 
    iFieldOrder         = iFieldOrder + 1
    ttField.lShow       = lShowRecidField
    ttField.iOrder      = iFieldOrder
    ttField.iOrderOrg   = iFieldOrder
    ttField.cFieldName  = 'RECID'
    ttField.cFullName   = 'RECID'
    ttField.cDataType   = 'CHARACTER'
    ttField.cInitial    = ''
    ttField.cFormat     = 'X(14)'
    ttField.cFormatOrg  = 'X(14)'
    ttField.cLabel      = 'RECID'
    ttField.lPrimary    = no
    ttField.iExtent     = 0
    ttField.lMetaField  = lShowRecidField
    ttField.lDataField  = lShowRecidField
    .

  /* Add a column for the rowid */
  create ttField.
  assign 
    iFieldOrder         = iFieldOrder + 1
    ttField.lShow       = lShowRowidField
    ttField.iOrder      = iFieldOrder
    ttField.iOrderOrg   = iFieldOrder
    ttField.cFieldName  = 'ROWID'
    ttField.cFullName   = 'ROWID'
    ttField.cDataType   = 'CHARACTER'
    ttField.cInitial    = ''
    ttField.cFormat     = 'X(30)'
    ttField.cFormatOrg  = 'X(30)'
    ttField.cLabel      = 'ROWID'
    ttField.lPrimary    = no
    ttField.iExtent     = 0
    ttField.lMetaField  = lShowRowidField
    ttField.lDataField  = lShowRowidField
    .

end procedure. /* getFields */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLibVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLibVersion Procedure 
PROCEDURE getLibVersion :
/*------------------------------------------------------------------------
  Name         : getLibVersion
  Description  : Get the DataDigger version for this instance
  ----------------------------------------------------------------------
  14-12-2009 pti Created
  ----------------------------------------------------------------------*/
  define output parameter pcBuildnr as character no-undo.
  
  pcBuildNr = '{&buildnr}'.

end procedure. /* setLibVersion */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMouseXY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMouseXY Procedure 
PROCEDURE getMouseXY :
/*------------------------------------------------------------------------
  Name         : getMouseXY
  Description  : Get the position of the mouse relative to the frame
  ----------------------------------------------------------------------
  14-09-2010 pti Created
  ----------------------------------------------------------------------*/
  
  define input  parameter phFrame as handle      no-undo.
  define output parameter piMouseX as integer     no-undo.
  define output parameter piMouseY as integer     no-undo.

  define variable lp as memptr  no-undo. 
  
  set-size( lp ) = 16. 
  
  run GetCursorPos( input-output lp). 
  
  /* Get the location of the mouse relative to the frame */
  run ScreenToClient ( input phFrame:hwnd, input lp ).
  
  piMouseX = get-long( lp, 1 ). 
  piMouseY = get-long( lp, 5 ). 
  
  set-size( lp ) = 0. 
end procedure. /* getMouseXY */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQueryTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQueryTable Procedure 
PROCEDURE getQueryTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 define output parameter table for ttQuery.
    
 /* This procedure just returns the table, no further logic needed. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTables Procedure 
PROCEDURE getTables :
/*------------------------------------------------------------------------
  Name         : getTables
  Description  : Fill the ttTable temptable with all tables of all 
                 currently connected databases.
  ----------------------------------------------------------------------
  29-10-2009 pti Created
  06-01-2010 pti Optimized for large / a lot of databases.
  ----------------------------------------------------------------------*/
  
  define output parameter table for ttTable. 

  define variable cQuery      as character   no-undo.
  define variable hBuffer     as handle      no-undo.
  define variable hQuery      as handle      no-undo.
  define variable iDatabase   as integer     no-undo.
  define variable cDatabase   as character   no-undo. 
  define variable cIniFile    as character   no-undo.
  define variable cLine       as character   no-undo.

  publish "timerCommand" ("start", "CollectTables").

  empty temp-table ttTable.
  create query hQuery.

  do iDatabase = 1 to num-dbs:

    cDatabase = ldbname(iDatabase).
    create buffer hBuffer for table substitute('&1._File', cDatabase).
      
    hQuery:set-buffers(hBuffer).
  
    cQuery = substitute('for each &1._File no-lock where _File-number < 32768', cDatabase).
    if hQuery:query-prepare(cQuery) then 
    do:
      hQuery:query-open().
      hQuery:get-first().

      repeat while not hQuery:query-off-end:
        create ttTable.
        assign 
          ttTable.cDatabase  = cDatabase
          ttTable.cTableName = hBuffer::_file-name
          ttTable.cTableDesc = hBuffer::_desc
          ttTable.lHidden    = hBuffer::_hidden
          .
        hQuery:get-next().
      end.
    end. /* opened */
  
    delete object hBuffer.
  end. /* 1 to num-dbs */

  delete object hQuery.
  publish "timerCommand" ("stop", "CollectTables").


  /* Read the ini file as plain text and parse the lines. Otherwise, if you 
   * have a large database (or a lot of databases), startup becomes slow.
   */
  publish "timerCommand" ("start", "parseIniFile").

  cIniFile = substitute('&1DataDigger-&2.ini', SESSION:TEMP-DIR, getUserName() ).
  input from value(cIniFile).
  repeat:
    import unformatted cLine.
    if cLine matches '[DB:*]' then cDatabase = entry(2,trim(cLine,'[]'),':').

    if cLine matches '*:QueriesServed=*' then
    do:
      find ttTable 
        where ttTable.cDatabase  = cDataBase
          and ttTable.cTableName = entry(1,cLine,':')
              no-error.

      if available ttTable then
      do:
        assign ttTable.iNumQueries = integer(entry(2,cLine,'=')) no-error.
        if ttTable.iNumQueries = ? then ttTable.iNumQueries = 0.
      end.
    end. /* queriesServed */

    if cLine matches '*:LastUsed=*' then
    do:
      find ttTable 
        where ttTable.cDatabase  = cDataBase
          and ttTable.cTableName = entry(1,cLine,':')
              no-error.

      if available ttTable then
      do:
        assign ttTable.tLastUsed = datetime(entry(2,cLine,'=')) no-error.
      end.
    end. /* queriesServed */

  end. /* repeat */
  input close. 

  publish "timerCommand" ("stop", "parseIniFile").

end procedure. /* getTables */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTablesWithField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTablesWithField Procedure 
PROCEDURE getTablesWithField :
/*------------------------------------------------------------------------
  Name         : getTablesWithField
  Description  : Fill the ttTable temptable with all tables of all 
                 currently connected databases.
  ----------------------------------------------------------------------
  29-10-2009 pti Created
  06-01-2010 pti Optimized for large / a lot of databases.
  ----------------------------------------------------------------------*/
  
  define input parameter pcFieldList as character no-undo.
  define output parameter table for ttTable. 

  define variable cQuery      as character   no-undo.
  define variable hFile       as handle      no-undo.
  define variable hField      as handle      no-undo.
  define variable hQuery      as handle      no-undo.
  define variable iDatabase   as integer     no-undo.
  define variable iField      as integer     no-undo.
  define variable cDatabase   as character   no-undo. 
  define variable cIniFile    as character   no-undo.
  define variable cLine       as character   no-undo.
  define variable lHasField   as logical     no-undo.

  publish "timerCommand" ("start", "CollectTablesWithField").

  empty temp-table ttTable.
  create query hQuery.

  do iDatabase = 1 to num-dbs:

    cDatabase = ldbname(iDatabase).
    create buffer hFile for table substitute('&1._File', cDatabase).
    create buffer hField for table substitute('&1._Field', cDatabase).

    hQuery:set-buffers(hFile).
  
    cQuery = substitute('for each &1._File no-lock where _File-number < 32768', cDatabase).

    if hQuery:query-prepare(cQuery) then 
    do:
      hQuery:query-open().

      tableLoop:
      repeat:
        hQuery:get-next().
        if hQuery:query-off-end then leave tableLoop.

        /* Check whether all required fields are in the table */
        do iField = 1 to num-entries(pcFieldList):
          lHasField = hField:find-first(substitute('where _file-recid = &1 and _field-name matches &2'
                                                  , hFile:recid
                                                  , quoter(entry(iField,pcFieldList))
                                                  ),no-lock) no-error.
          if not lHasField then next tableLoop.
        end.

        /* Only accept tables that contain all fields */
        create ttTable.
        assign 
          ttTable.cDatabase  = cDatabase
          ttTable.cTableName = hFile::_file-name
          ttTable.cTableDesc = hFile::_desc
          ttTable.lHidden    = hFile::_hidden
          .
      end.
    end. /* opened */
  
    delete object hFile.
    delete object hField.
  end. /* 1 to num-dbs */

  delete object hQuery.
  publish "timerCommand" ("stop", "CollectTablesWithField").


  /* Read the ini file as plain text and parse the lines. Otherwise, if you 
   * have a large database (or a lot of databases), startup becomes slow.
   */
  publish "timerCommand" ("start", "parseIniFile").

  cIniFile = substitute('&1DataDigger-&2.ini', getProgramDir(), getUserName() ).
  input from value(cIniFile).
  repeat:
    import unformatted cLine.
    if cLine matches '[DB:*]' then cDatabase = entry(2,trim(cLine,'[]'),':').

    if cLine matches '*:QueriesServed=*' then
    do:
      find ttTable 
        where ttTable.cDatabase  = cDataBase
          and ttTable.cTableName = entry(1,cLine,':')
              no-error.

      if available ttTable then
      do:
        assign ttTable.iNumQueries = integer(entry(2,cLine,'=')) no-error.
        if ttTable.iNumQueries = ? then ttTable.iNumQueries = 0.
      end.
    end. /* queriesServed */

    if cLine matches '*:LastUsed=*' then
    do:
      find ttTable 
        where ttTable.cDatabase  = cDataBase
          and ttTable.cTableName = entry(1,cLine,':')
              no-error.

      if available ttTable then
      do:
        assign ttTable.tLastUsed = datetime(entry(2,cLine,'=')) no-error.
      end.
    end. /* queriesServed */

  end. /* repeat */
  input close. 

  publish "timerCommand" ("stop", "parseIniFile").

end procedure. /* getTables */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lockWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lockWindow Procedure 
PROCEDURE lockWindow :
/*------------------------------------------------------------------------
  Name         : lockWindow
  Description  : Lock / unlock updates that Windows does to windows. 
                 
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter phWindow as handle  no-undo.
  define input parameter plLock   as logical no-undo.

  define variable iRet as integer no-undo. 
  define buffer ttWindowLock for ttWindowLock. 

  &GLOBAL-DEFINE WM_SETREDRAW     11
  &GLOBAL-DEFINE RDW_ALLCHILDREN 128
  &GLOBAL-DEFINE RDW_ERASE         4
  &GLOBAL-DEFINE RDW_INVALIDATE    1

  /* Find window in our tt of locked windows */
  find ttWindowLock where ttWindowLock.hWindow = phWindow no-error.
  if not available ttWindowLock then
  do:
    create ttWindowLock.
    ttWindowLock.hWindow = phWindow.
  end.

  /* Because commands to lock or unlock may be nested, keep track
   * of the number of locks/unlocks using a semaphore.
   * 
   * The order of commands may be:
   * lockWindow(yes). -> actually lock the window
   * lockWindow(yes). -> do nothing
   * lockWindow(yes). -> do nothing
   * lockWindow(no).  -> do nothing
   * lockWindow(no).  -> do nothing
   * lockWindow(yes). -> do nothing
   * lockWindow(no).  -> do nothing
   * lockWindow(no).  -> actually unlock the window
   */
  if plLock then 
    ttWindowLock.iLockCounter = ttWindowLock.iLockCounter + 1.
  else 
    ttWindowLock.iLockCounter = ttWindowLock.iLockCounter - 1.

  /* Now, only lock when the semaphore is increased to 1 */
  if plLock and ttWindowLock.iLockCounter = 1 then
  do:

    run SendMessageA( phWindow:hwnd /* {&window-name}:hwnd */
                    , {&WM_SETREDRAW}
                    , 0
                    , 0
                    , output iRet
                    ).
  end.

  /* And only unlock after the last unlock command */
  else if ttWindowLock.iLockCounter = 0 then
  do:
    run SendMessageA( phWindow:hwnd /* {&window-name}:hwnd */
                    , {&WM_SETREDRAW}
                    , 1
                    , 0
                    , output iRet
                    ).
    
    run RedrawWindow( phWindow:hwnd /* {&window-name}:hwnd */
                    , 0
                    , 0
                    , {&RDW_ALLCHILDREN} + {&RDW_ERASE} + {&RDW_INVALIDATE}
                    , output iRet
                    ).

    /* Clean up tt */
    delete ttWindowLock.
  end. 

end procedure. /* lockWindow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-openUrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openUrl Procedure 
PROCEDURE openUrl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter pcUrl as character   no-undo.
  define variable cLocation as character   no-undo.

  /* We want to open a HTML page that is on the local system. 
   * That is not a problem. But.... The page we want to open 
   * contains a named link (with a '#' in the url) to point
   * to a specific tiddler in tiddlywiki. When you open a URL
   * like this on the local filesystem, the named links are
   * not supported, so we have to create a workaround using
   * a redirect from within an HTML page using javascript....
   */
  cLocation = 'file://' + getProgramDir() + pcUrl.
  cLocation = replace(cLocation,'\','/').

  output to value(session:temp-dir + 'datadigger.html').
  put unformatted 
         '<html>'
    skip '<head>'
    skip '<script>'
    skip 'document.location="' + cLocation + '";'
    skip '</script>'
    skip '</head>'
    skip '</html>'.
  output close. 

  os-command no-wait start value(session:temp-dir + 'datadigger.html').
  pause 1 no-message. /* otherwise error 'file not found' */
  os-delete value(session:temp-dir + 'datadigger.html').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resizeFilterFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeFilterFields Procedure 
PROCEDURE resizeFilterFields :
/*------------------------------------------------------------------------
  Name         : resizeFilterFields
  Description  : Generic procedure to redraw the filter fields of the 
                 fields browse and of the index browse. 

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define input  parameter pcFilterFields as character   no-undo.
  define input  parameter pcButtons      as character   no-undo.
  define input  parameter phBrowse       as handle      no-undo.

  define variable iField        as integer no-undo. 
  define variable iButton       as integer no-undo. 
  define variable iCurrentPos   as integer no-undo. 
  define variable iRightEdge    as integer no-undo. 
  define variable iWidth        as integer no-undo. 
  define variable hColumn       as handle  no-undo. 
  define variable hButton       as handle  no-undo. 
  define variable hFilterField  as handle  no-undo. 
  define variable iFilter       as integer no-undo. 
  define variable lChangeDetected as logical no-undo. 

  /* Find out if there has been a change in the browse or in one of 
   * its columns. If no changes, save a little time by not redrawing
   */
  findChange:
  do iField = 1 to phBrowse:num-columns:
    hColumn = phBrowse:get-browse-column(iField):handle.
    if isWidgetChanged(hColumn) then lChangeDetected = true.
  end.

  if not lChangeDetected
    and not isWidgetChanged(phBrowse) then return. 



  /* To prevent drawing error, make all fields small */
  do iField = 1 to num-entries(pcFilterFields):
    hFilterField = handle(entry(iField,pcFilterFields)).
    hFilterField:visible      = no.
    hFilterField:x            = phBrowse:x. 
    hFilterField:width-pixels = 1.
  end.

  /* Start by setting the buttons at the proper place. Do this right to left */
  assign iRightEdge = phBrowse:x + phBrowse:width-pixels.
  do iButton = num-entries(pcButtons) to 1 by -1:
    hButton = handle(entry(iButton,pcButtons)).
    hButton:x = iRightEdge - hButton:width-pixels.
    hButton:y = phBrowse:y - 23. /* filter buttons close to the browse */
    iRightEdge = hButton:x - 2. /* A little margin between buttons */
  end.

  /* The left side of the left button is the maximum point 
   * Fortunately, this value is already in iRightEdge.
   * Resize and reposition the fields from left to right, 
   * use the space between browse:x and iRightEdge
   */

  /* Take the left side of the first visible column as a starting point. */
  firstVisibleColumn:
  do iField = 1 to phBrowse:num-columns:
    hColumn = phBrowse:get-browse-column(iField):handle.

    if hColumn:x > 0 and hColumn:visible then
    do:
      iCurrentPos = phBrowse:x + hColumn:x.
      leave firstVisibleColumn.
    end.
  end.

  fieldLoop:
  do iField = 1 to phBrowse:num-columns:

    hColumn = phBrowse:get-browse-column(iField):handle.
    
    /* Some types cannot have a filter */
    if hColumn:data-type = 'raw' then next. 

    iFilter = iFilter + 1.
    if iFilter > num-entries(pcFilterFields) then leave fieldLoop.

    /* Determine the handle of the filterfield */
    hFilterField = handle(entry(iFilter, pcFilterFields)).

    /* If the column is hidden, make the filter hidden and go to the next */
    if not hColumn:visible then 
    do:
      hFilterField:visible = no.
      next fieldLoop. 
    end.

    /* Where *are* we ?? */
    iCurrentPos = phBrowse:x + hColumn:x.

    /* If the columns have been resized, some columns might have fallen off the screen */
    if hColumn:x < 1 then next. 

    /* Does it fit on the screen? */
    if iCurrentPos >= iRightEdge - 5 then leave fieldLoop. /* accept some margin */

    /* Where will this field end? And does it fit? */
    iWidth = hColumn:width-pixels + 4.
    if iCurrentPos + iWidth > iRightEdge then iWidth = iRightEdge - iCurrentPos.

    /* Ok, seems to fit */
    hFilterField:x            = iCurrentPos.
    hFilterField:width-pixels = iWidth.
    iCurrentPos               = iCurrentPos + iWidth.
    hFilterField:visible      = phBrowse:visible. /* take over the visibility of the browse */
  end.

end procedure. /* resizeFilterFields */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveQuery Procedure 
PROCEDURE saveQuery :
/*------------------------------------------------------------------------
  Name         : saveQuery
  Description  : Save a query to the INI file. Increase the nr of all 
                 other queries and place this one at the top
                 
  ----------------------------------------------------------------------
  18-11-2009 pti Created
  ----------------------------------------------------------------------*/

  define input  parameter pcDatabase     as character   no-undo.
  define input  parameter pcTable        as character   no-undo.
  define input  parameter pcQuery        as character   no-undo.

  define variable cQuery as character no-undo. 
  define variable iQuery as integer   no-undo. 

  define buffer bQuery for ttQuery.

  /* Prepare query for saving in ini-file */
  cQuery = pcQuery.
  cQuery = replace(cQuery,'~n',chr(1)).
  cQuery = replace(cQuery,chr(160),chr(1)).
  if cQuery = '' then cQuery = '<empty>'.

  /* Get the table with queries again, because they might be 
   * changed if the user has more than one window open.
   */
  run collectQueryInfo(pcDatabase, pcTable).

  /* Save current query in the tt. If it already is in the 
   * TT then just move it to the top
   */
  find bQuery 
    where bQuery.cDatabase = pcDatabase
      and bQuery.cTable    = pcTable 
      and bQuery.cQueryTxt = cQuery no-error.

  if available bQuery then 
  do:
    assign bQuery.iQueryNr = 0.
  end.
  else 
  do:
    create bQuery.
    assign bQuery.cDatabase = pcDatabase 
           bQuery.cTable    = pcTable   
           bQuery.iQueryNr  = 0
           bQuery.cQueryTxt = cQuery.
  end.

  /* The ttQuery temp-table is already filled, renumber it */
  iQuery = 0.
  repeat preselect each bQuery 
    where bQuery.cDatabase = pcDatabase
      and bQuery.cTable    = pcTable 
       by bQuery.iQueryNr:

    find next bQuery.
    assign 
      iQuery          = iQuery + 1
      bQuery.iQueryNr = iQuery.
  end.

  /* And save it to the INI-file */
  run saveQueryTable(table bQuery, pcDatabase, pcTable).

end procedure. /* saveQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveQueryTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveQueryTable Procedure 
PROCEDURE saveQueryTable :
/*------------------------------------------------------------------------
  Name         : saveQueryTable
  Description  : Save the queries in the TT to the INI file with a max
                 of MaxQueryHistory
                 
  ----------------------------------------------------------------------
  18-11-2009 pti Created
  ----------------------------------------------------------------------*/

  define input  parameter table for ttQuery.
  define input  parameter pcDatabase     as character   no-undo.
  define input  parameter pcTable        as character   no-undo.

  define variable iMaxQueryHistory as integer no-undo. 
  define variable iQuery           as integer no-undo. 
  define variable cSetting         as character no-undo. 

  define buffer bQuery for ttQuery.
 
  iMaxQueryHistory = integer(getRegistry("DataDigger", "MaxQueryHistory" )).
  if iMaxQueryHistory = 0 then return. /* no query history wanted */
 
  /* If it is not defined use default setting */
  if iMaxQueryHistory = ? then iMaxQueryHistory = 10. 

  iQuery = 1.

  saveQuery:
  for each bQuery 
    where bQuery.cDatabase = pcDatabase
      and bQuery.cTable    = pcTable 
       by bQuery.iQueryNr:
    
    cSetting = bQuery.cQueryTxt.
    if cSetting = '' then next. /* cSetting = '<empty>' */

    setRegistry ( substitute('DB:&1', pcDatabase)
                , substitute('&1:query:&2', pcTable, iQuery )
                , cSetting
                ).
    iQuery = iQuery + 1.
    if iQuery > iMaxQueryHistory then leave saveQuery.
  end.

  /* Delete higher nrs than MaxQueryHistory */
  do while iQuery <= iMaxQueryHistory:
 
    setRegistry ( substitute('DB:&1', pcDatabase)
                , substitute('&1:query:&2', pcTable, iQuery )
                , ?
                ).
    iQuery = iQuery + 1.

  end. /* iQuery .. MaxQueryHistory */

end procedure. /* saveQueryTable */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSortArrow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSortArrow Procedure 
PROCEDURE setSortArrow :
/*------------------------------------------------------------------------
  Name         : setSortArrow
  Description  : Set the sorting arrow on a browse
  ----------------------------------------------------------------------*/

  define input parameter phBrowse    as handle    no-undo. 
  define input parameter pcSortField as character no-undo. 
  define input parameter plAscending as logical   no-undo. 

  define variable iColumn  as integer   no-undo. 
  define variable hColumn  as handle    no-undo. 

  do iColumn = 1 to phBrowse:num-columns:
    hColumn = phBrowse:get-browse-column(iColumn).

    /* If you apply the sort to the same column, the order 
     * of sorting is inverted.
     */
    if hColumn:name = pcSortField then 
    do:
      phBrowse:set-sort-arrow(iColumn, plAscending ).

      /* Setting is one of: ColumnSortFields | ColumnSortIndexes | ColumnSortTables */
      setRegistry( 'DataDigger'
                 , substitute('ColumnSort&1', substring(phBrowse:name,3))  
                 , substitute('&1,&2',iColumn, plAscending)
                 ).
    end.
    else 
      phBrowse:set-sort-arrow(iColumn, ? ). /* erase existing arrow */
  end.

end procedure. /* setSortArrow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTransparency) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTransparency Procedure 
PROCEDURE setTransparency :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter phFrame as handle     no-undo.
  define input  parameter piLevel as integer    no-undo.
  
  &scop GWL_EXSTYLE         -20
  &scop WS_EX_LAYERED       524288
  &scop LWA_ALPHA           2
  &scop WS_EX_TRANSPARENT   32
  
  DEFINE VARIABLE stat AS INTEGER    NO-UNDO.

  /* Set WS_EX_LAYERED on this window  */
  RUN SetWindowLongA(phFrame:HWND, {&GWL_EXSTYLE}, {&WS_EX_LAYERED}, output stat).

  /* Make this window transparent (0 - 255) */
  RUN SetLayeredWindowAttributes(phFrame:HWND, 0, piLevel, {&LWA_ALPHA}, OUTPUT stat).

END PROCEDURE. /* setTransparency */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showHelp Procedure 
PROCEDURE showHelp :
define input  parameter pcTopic   as character   no-undo.
  define input  parameter pcStrings as character   no-undo.

  define variable cButtons       as character   no-undo.
  define variable cMessage       as character   no-undo.
  define variable cMsg           as character   no-undo.
  define variable cPrg           as character   no-undo.
  define variable cTitle         as character   no-undo.
  define variable cType          as character   no-undo.
  define variable cUrl           as character   no-undo.
  define variable cHlp           as character   no-undo.
  define variable iButtonPressed as integer     no-undo.
  define variable lAnswer        as logical     no-undo.
  define variable lDontShowAgain as logical     no-undo.
  define variable lHidden        as logical     no-undo.
  define variable iString        as integer     no-undo.
  define variable cUserString    as character   no-undo extent 9.
  define variable cHelpfile    as character   no-undo.

  /* If no message, then just return */
  cMessage = getRegistry('DataDigger:help', pcTopic + ':message').
  cHelpfile = getProgramDir() + 'DataDigger.chm'.

  /* What to start? */
  cUrl = getRegistry('DataDigger:help', pcTopic + ':url').
  cHlp = getRegistry('DataDigger:help', pcTopic + ':hlp').
  cPrg = getRegistry('DataDigger:help', pcTopic + ':program').
  
  if cMessage = ? then 
  do:
    if cUrl = ? and cPrg = ? and cHlp = ? then return. 
    lHidden        = yes. /* suppress empty text window */
    iButtonPressed = 1.   /* forces to start the url or prog */
  end.

  /* If type is unknown, set to QUESTION if there is a question mark in the message */
  cType    = getRegistry('DataDigger:help', pcTopic + ':type').
  if cType = ? then cType = (if cMessage matches '*?*' then 'Question' else 'Message').

  /* If no button labels defined, set them based on message type */
  cButtons = getRegistry('DataDigger:help', pcTopic + ':buttons').
  if cButtons = ? then cButtons = (if cType = 'Question' then '&Yes,&No,&Cancel' else '&Ok').

  /* If title is empty, set it to the type of the message */
  cTitle   = getRegistry('DataDigger:help', pcTopic + ':title').
  if cTitle = ? then cTitle = cType.
  
  /* If hidden has strange value, set it to NO */
  lHidden = logical(getRegistry('DataDigger:help', pcTopic + ':hidden')) no-error.
  if lHidden = ? then lHidden = no.
  
  /* If ButtonPressed has strange value, set hidden to NO */
  iButtonPressed = integer( getRegistry('DataDigger:help',pcTopic + ':answer') ) no-error.
  if iButtonPressed = ? then lHidden = no.
  
  /* if we have no message, but we do have an URL or prog, then don´t show an 
   * empty message box. 
   */
  if cMessage = ? then
    assign 
      lHidden        = yes /* suppress empty text window */
      iButtonPressed = 1.   /* forces to start the url or prog */

  /* Fill in strings in message */
  do iString = 1 to num-entries(pcStrings):
    cUserString[iString] = entry(iString,pcStrings).
  end.

  cMessage = substitute( cMessage
                       , cUserString[1]
                       , cUserString[2]
                       , cUserString[3]
                       , cUserString[4]
                       , cUserString[5]
                       , cUserString[6]
                       , cUserString[7]
                       , cUserString[8]
                       , cUserString[9]
                       ).

  /* If not hidden, show the message and let the user choose an answer */
  if not lHidden then 
  do:
    run value( getProgramDir() + 'dQuestion.w')
      ( input cTitle
      , input cMessage
      , input cButtons
      , output iButtonPressed
      , output lDontShowAgain
      ).
      
    if lDontShowAgain then 
      setRegistry('DataDigger:help', pcTopic + ':hidden', 'yes').
  end. 
  
  /* Start external things if needed */                                            
  if iButtonPressed = 1 then
  do:
    if cHlp <> ? then system-help cHelpfile context integer(cHlp).
    if cUrl <> ? then run openUrl( cUrl ).
    if cPrg <> ? then run value( cPrg ) no-error.
  end.
  
  /* Save answer */
  setRegistry('DataDigger:help',pcTopic + ':answer', string(iButtonPressed)).
      
end procedure. /* showHelp */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showScrollbars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showScrollbars Procedure 
PROCEDURE showScrollbars :
/*------------------------------------------------------------------------
  Name         : showScrollbars
  Description  : Hide or show scrollbars the hard way
  ----------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER ip-Frame      AS HANDLE  NO-UNDO.
  DEFINE INPUT PARAMETER ip-horizontal AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ip-vertical   AS LOGICAL NO-UNDO.

  DEFINE VARIABLE iv-retint AS INTEGER NO-UNDO.

  &scoped-define SB_HORZ 0
  &scoped-define SB_VERT 1
  &scoped-define SB_BOTH 3
  &scoped-define SB_THUMBPOSITION 4

  RUN ShowScrollBar ( ip-Frame:HWND,
                      {&SB_HORZ},
                      IF ip-horizontal THEN -1 ELSE 0,
                      OUTPUT iv-retint ).
       
  RUN ShowScrollBar ( ip-Frame:HWND, 
                      {&SB_VERT},
                      IF ip-vertical  THEN -1 ELSE 0,
                      OUTPUT iv-retint ).
  &undefine SB_HORZ
  &undefine SB_VERT
  &undefine SB_BOTH
  &undefine SB_THUMBPOSITION
    
END PROCEDURE. /* ShowScrollbars */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startWinHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startWinHelp Procedure 
PROCEDURE startWinHelp :
/*------------------------------------------------------------------------
  Name         : startWinHelp
  Description  : Invoke help
  ----------------------------------------------------------------------
  16-03-2011 pti Created
  ----------------------------------------------------------------------*/

  define input  parameter phFocus as handle no-undo.

  define variable cHelpfile    as character   no-undo.
  define variable iHelpContext as integer     no-undo. 
  define variable cStartedFrom as character   no-undo.

  cHelpfile = getProgramDir() + 'DataDigger.chm'.

  if can-query(phFocus,'context-help-id') then
    iHelpContext = phFocus:context-help-id.

  /* If no help available, find the help-id of the current window */
  if iHelpContext = 0 then
  do:
    cStartedFrom = entry(num-entries(program-name(3),'\'), program-name(3),'\').
    cStartedFrom = entry(1,cStartedFrom,'.').

    case cStartedFrom:
      when 'dAbout'       then iHelpContext = 260.
      when 'dDump'        then iHelpContext = 160.
      when 'dFilter'      then iHelpContext = 260.
      when 'dQueries'     then iHelpContext = 140.
      when 'dQuestion'    then iHelpContext = 260.
      when 'dSettings'    then iHelpContext = 120.
      when 'wConnections' then iHelpContext = 130.
      when 'wEdit'        then iHelpContext = 170.
      when 'wDataDigger'  then iHelpContext = 260.
      when 'wLoadData'    then iHelpContext = 150.
    end case.
  end.

  /* If still nothing found, show help about main window */
  if iHelpContext = 0 then 
    iHelpContext = 260. /* page about main window */ 

  system-help cHelpfile context iHelpContext.

end procedure. /* startWinHelp */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-upgradeDataDigger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upgradeDataDigger Procedure 
PROCEDURE upgradeDataDigger :
define input  parameter pcDownloadLocation as character no-undo. 
  define output parameter pcError            as character no-undo. 

  define variable cTargetLocation as character no-undo.
  define variable iResult         as integer   no-undo.
  define variable cTargetDir      as character no-undo.
  define variable cTempFile       as character no-undo. 
  
  /*   /* Would you like to upgrade to version &1 ? */                            */
  /*   run showHelp('UpgradeNow', cVersionInfo[1] ).                              */
  /*   if getRegistry('DataDigger:help', 'UpgradeNow:answer') <> '1' then return. */

  /* pcDownloadLocation looks something like this:
   * http://www.oehive.org/files/DataDigger-14.zip
   * So chop filename from directory for the download location 
   */
  cTargetDir      = getProgramDir().
  cTargetLocation = cTargetDir + entry(num-entries(pcDownloadLocation,'/'),pcDownloadLocation,'/').
  
  run urlDownloadToFileA (0, pcDownloadLocation, cTargetLocation, 0, 0, output iResult).

  /* Error? */
  if iResult <> 0 then 
  do:
    /*  Download failed. 
     *  Please download from "www.oehive.org" and install manually. 
     */
    pcError = 'Download failed. Please download from www.oehive.org and install manually.'.
    return.
  end.

  /* Build a temporary batchfile */
  cTempFile = session:temp-dir + 'datadigger-install.bat'.
  output to value(cTempFile).
  put unformatted 
    '@echo off'
    '~ncd ' cTargetDir
    '~nunzip -oq ' cTargetLocation
    '~n'.
  output close. 

  /* Execute the temp batchfile */
  os-command silent value(cTempFile).
  os-delete value(cTempFile).

end procedure. /* upgradeDataDigger */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-formatQueryString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION formatQueryString Procedure 
FUNCTION formatQueryString RETURNS CHARACTER
  ( input pcQueryString as character
  , input plExpanded    as logical ) :

/*------------------------------------------------------------------------------
  Purpose: formatQueryString
    Notes: return a properly formatted query string
------------------------------------------------------------------------------*/
  define variable cReturnValue as character   no-undo.

  cReturnValue = pcQueryString.

  if cReturnValue <> '' and cReturnValue <> ? then 
  do:
    /* There might be chr(1) chars in the text (if read from ini, for example)
     * Replace these with normal CRLF, then proceed 
     */
    cReturnValue = replace(cReturnValue,chr(1),'~n').

    if plExpanded then
      cReturnValue = replace(cReturnValue, chr(160), '~n').
    else
      cReturnValue = replace(cReturnValue, '~n', chr(160)).
  end.

  return cReturnValue.

end function. /* formatQueryString */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getColumnWidthList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getColumnWidthList Procedure 
FUNCTION getColumnWidthList returns character
  ( input phBrowse as handle ):

  /* returns a list of all fields and their width like:
   * custnum:12,custname:20,city:12
   */
  define variable cWidthList as character   no-undo.
  define variable hColumn    as handle      no-undo.
  define variable iColumn    as integer     no-undo.
  
  do iColumn = 1 to phBrowse:num-columns:
  
    hColumn = phBrowse:get-browse-column(iColumn).
    cWidthList = substitute('&1,&2:&3'
                           , cWidthList 
                           , hColumn:name
                           , hColumn:width-pixels
                           ).
  end.
  
  return trim(cWidthList,','). 
end function. /* getColumnWidthList */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatabaseList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatabaseList Procedure 
FUNCTION getDatabaseList returns character:

/*------------------------------------------------------------------------
  Name         : getDatabaseList
  Description  : Return a comma separated list of all connected datbases
  ---------------------------------------------------------------------- 
  22-01-2009 pti Created
  ----------------------------------------------------------------------*/
  
  define variable cDatabaseList as character   no-undo.

  define variable iCount as integer     no-undo.

  /* Special options */
  do iCount = 1 to num-dbs:
    cDatabaseList = cDatabaseList + ',' + ldbname(iCount).
  end.

  return trim(cDatabaseList,',').

end function. /* getDatabaseList */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getImagePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getImagePath Procedure 
FUNCTION getImagePath returns character
  ( pcImage as character ) :
  
  define variable cImagePath as character   no-undo.
  define variable cIconSet   as character   no-undo.
  
  cIconSet = 'default'.

  cImagePath = substitute('&1Image/&2_&3'
                         , getProgramDir()
                         , cIconSet
                         , pcImage
                         ).

  /* Fall back to the default icon set when image not found */
  if search(cImagePath) = ? then
    cImagePath = substitute('&1Image/default_&2'
                           , getProgramDir()
                           , pcImage
                           ).

  return cImagePath.
end function. /* getImagePath */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKeyList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKeyList Procedure 
FUNCTION getKeyList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getKeyList
  Description  : Return a list of special keys pressed 
  ---------------------------------------------------------------------- 
  22-01-2009 pti Created
  ----------------------------------------------------------------------*/

  def var L-KBSTATE as memptr no-undo. 
  def var L-RETURNVALUE as integer no-undo. 
  def var L-SHIFTLIST as char no-undo. 
  
  set-size(L-KBSTATE) = 256. 
  
  /* Get the current state of the keyboard */ 
  run GetKeyboardState(get-pointer-value(L-KBSTATE), output L-RETURNVALUE). 
  
  if get-bits(get-byte(L-KBSTATE, 1 + 16), 8, 1) = 1 
  then L-SHIFTLIST = L-SHIFTLIST + ",SHIFT". 
  if get-bits(get-byte(L-KBSTATE, 1 + 17), 8, 1) = 1 
  then L-SHIFTLIST = L-SHIFTLIST + ",CTRL". 
  if get-bits(get-byte(L-KBSTATE, 1 + 18), 8, 1) = 1 
  then L-SHIFTLIST = L-SHIFTLIST + ",ALT". 
  
  SET-SIZE(L-KBSTATE) = 0. 
  
  return L-SHIFTLIST.   /* Function return value. */ 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLinkInfo Procedure 
FUNCTION getLinkInfo returns character
  ( input pcFieldName as character
  ):

/*------------------------------------------------------------------------
  Name         : getLinkInfo
  Description  : Save name/value of a field.
  ----------------------------------------------------------------------
  21-10-2009 pti Created
  ----------------------------------------------------------------------*/

  define buffer bLinkInfo for ttLinkInfo. 

  find bLinkInfo where bLinkInfo.cField = pcFieldName no-error.
  return (if available bLinkInfo then bLinkInfo.cValue else "").

end function. /* getLinkInfo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMatchesValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMatchesValue Procedure 
FUNCTION getMatchesValue RETURNS CHARACTER
  ( hFillIn as handle ) :

/*------------------------------------------------------------------------
  Name         : getMatchesValue
  Description  : Transform the value of a fillin to something we can use
                 with the MATCHES function. 

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cValue as character no-undo. 

  cValue = hFillIn:screen-value. 
  if cValue = hFillIn:private-data then cValue = ''.

  if cValue = ? or cValue = '' then cValue = '*'.
  else 
  if    index(cValue,'*') = 0 
    and index(cValue,'.') = 0 then 
    cValue = '*' + cValue + '*'.

  return cValue.   /* Function return value. */

END FUNCTION. /* getMatchesValue */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMaxLength) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMaxLength Procedure 
FUNCTION getMaxLength RETURNS integer
  ( cFieldList as character ) :

/*------------------------------------------------------------------------
  Name         : getMaxLength
  Description  : Return the length of the longest element in a comma 
                 separated list
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/
  define variable iField     as integer no-undo. 
  define variable iMaxLength as integer no-undo. 

  /* Get max field length */
  do iField = 1 to num-entries(cFieldList):
    iMaxLength = maximum(iMaxLength,length(entry(iField,cFieldList))).
  end.

  return iMaxLength.   /* Function return value. */

end function. /* getMaxLength */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOsErrorDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOsErrorDesc Procedure 
FUNCTION getOsErrorDesc returns character
  (input piOsError as integer):

/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  case piOsError:
    when   0 then return "No error                 ".
    when   1 then return "Not owner                ".
    when   2 then return "No such file or directory".
    when   3 then return "Interrupted system call  ".
    when   4 then return "I/O error                ".
    when   5 then return "Bad file number          ".
    when   6 then return "No more processes        ".
    when   7 then return "Not enough core memory   ".
    when   8 then return "Permission denied        ".
    when   9 then return "Bad address              ".
    when  10 then return "File exists              ".
    when  11 then return "No such device           ".
    when  12 then return "Not a directory          ".
    when  13 then return "Is a directory           ".
    when  14 then return "File table overflow      ".
    when  15 then return "Too many open files      ".
    when  16 then return "File too large           ".
    when  17 then return "No space left on device  ".
    when  18 then return "Directory not empty      ".
    otherwise return "Unmapped error           ".
  end case.

end function. /* getOsErrorDesc */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrimaryFields Procedure 
FUNCTION getPrimaryFields returns character
  ( input pcDatabaseName as character
  , input pcTableName    as character  
    ) :

/*------------------------------------------------------------------------
  Name         : getPrimaryFields
  Description  : Return the primary fields of a table.
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/
  
  define variable cWhere            as character   no-undo.
  define variable hQuery            as handle      no-undo.
  define variable hFieldBuffer      as handle      no-undo.
  define variable hFileBuffer       as handle      no-undo.
  define variable hIndexBuffer      as handle      no-undo.
  define variable hIndexFieldBuffer as handle      no-undo.
  define variable cFieldList        as character   no-undo.
  
  create buffer hFileBuffer       for table pcDatabaseName + "._File".
  create buffer hIndexBuffer      for table pcDatabaseName + "._Index".
  create buffer hIndexFieldBuffer for table pcDatabaseName + "._Index-Field".
  create buffer hFieldBuffer      for table pcDatabaseName + "._Field".
  
  create query hQuery.
  hQuery:set-buffers(hFileBuffer,hIndexBuffer,hIndexFieldBuffer,hFieldBuffer).
  
  cWhere = substitute('for each &1._file  where &1._file._file-name = &2, ~
                          first &1._index where recid(&1._index) = &1._file._prime-index, ~
                           each &1._index-field of &1._index, ~
                           each &1._field of &1._index-field'
                     , pcDatabaseName
                     , quoter(pcTableName)
                     ).
  
  if hQuery:query-prepare (cWhere) then 
  do:
    hQuery:query-open().
    hQuery:get-first(no-lock).
    repeat while not hQuery:query-off-end:
      cFieldList = cFieldList + "," + trim(hFieldBuffer:buffer-field("_field-name"):string-value).
      hQuery:get-next(no-lock).
    end.
  end.
  
  cFieldList = trim(cFieldList, ",").
  
  hQuery:query-close. 
  
  delete object hFileBuffer.
  delete object hIndexBuffer.
  delete object hIndexFieldBuffer.
  delete object hFieldBuffer.
  delete object hQuery.
  
  return cFieldList.   /* Function return value. */

end function. /* getPrimaryFields */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProgramDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProgramDir Procedure 
FUNCTION getProgramDir RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getProgramDir
  Description  : Return the directory in which DataDigger is installed,
                 including a backslash
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cProgDir   as character   no-undo.

  file-info:file-name = this-procedure:file-name.

  /* If you use a compiled version without the .p around
   * the full-pathname is unknown. In that case use the 
   * name of the program with a .r suffix 
   */
  if this-procedure:file-name matches '*.p' 
    and file-info:full-pathname = ? then
    file-info:file-name = replace(this-procedure:file-name,'.p','.r').

  cProgDir = substring(file-info:full-pathname,1,r-index(file-info:full-pathname,'\')).
  if '{&uib_is_running}' <> '' then cProgDir = 'd:\data\progress\datadigger\'.

  RETURN cProgDir. /* Function return value. */

END FUNCTION. /* getProgramDir */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuery Procedure 
FUNCTION getQuery returns character
  ( input pcDatabase as character
  , input pcTable    as character
  , input piQuery    as integer
  ) :

  define buffer bQuery for ttQuery.

  find bQuery 
    where bQuery.cDatabase = pcDatabase
      and bQuery.cTable    = pcTable
      and bQuery.iQueryNr  = piQuery no-error.

  if available bQuery then 
    return bQuery.cQueryTxt.
  else
    return ?.

end function. /* getQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReadableQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReadableQuery Procedure 
FUNCTION getReadableQuery returns character
  ( input pcQuery as character ):

  /* Name: getReadableQuery
   * Desc: Return a query as a string that is readable for humans. 
   * 
   *       message getReadableQuery( hQuery:prepare-string ) view-as alert-box.
   *       
   *       message getReadableQuery( string(hQuery) ) view-as alert-box.
   */
  define variable hQuery as handle      no-undo.
  
  /* Accept query or query-handle */
  hQuery = widget-handle(pcQuery) no-error.
  if valid-handle( hQuery ) then
  do:
    hQuery = widget-handle(pcQuery).
    pcQuery = hQuery:prepare-string.
  end.
  
  pcQuery = replace(pcQuery,' EACH ' ,' EACH ').
  pcQuery = replace(pcQuery,' FIRST ',' FIRST ').
  pcQuery = replace(pcQuery,' WHERE ',  '~n  WHERE ').
  pcQuery = replace(pcQuery,' AND '  ,  '~n    AND ').
  pcQuery = replace(pcQuery,' BY '   ,  '~n     BY ').
  pcQuery = replace(pcQuery,' FIELDS ()','').
  pcQuery = replace(pcQuery,'FOR EACH ' ,'FOR EACH ').
  pcQuery = replace(pcQuery,' NO-LOCK',  ' NO-LOCK').
  
  .pcQuery = pcQuery + '~n'.

  return pcQuery.
end function. /* getReadableQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRegistry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRegistry Procedure 
FUNCTION getRegistry returns character
    ( pcSection as character
    , pcKey     as character 
    ) :

/*------------------------------------------------------------------------
  Name         : getRegistry 
  Description  : Get a value from the registry. 
  ---------------------------------------------------------------------- 
  15-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cRegistryValue as character   no-undo.

  use substitute('DataDigger-&1', getUserName() ).
  get-key-value 
    section pcSection
    key     pcKey
    value   cRegistryValue.

  /* If setting is not in the personal INI file
   * then check the default DataDigger.ini
   */
  if cRegistryValue = ? then
  do:
    use 'DataDigger'.
    get-key-value 
      section pcSection
      key     pcKey
      value   cRegistryValue.
  end.

  /* And if it is still not found, look in 
   * the DataDiggerHelp ini file 
   */
  if cRegistryValue = ? then
  do:
    use 'DataDiggerHelp'.
    get-key-value 
      section pcSection
      key     pcKey
      value   cRegistryValue.
  end. 

  /* Clean up and return */
  use "".
  return cRegistryValue.

end function. /* getRegistry */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTableList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTableList Procedure 
FUNCTION getTableList returns character
  ( input  pcDatabaseFilter   as character
  , input  pcTableFilter      as character
  , input  plShowHiddenTables as logical  
  , input  pcSortField        as character
  , input  plAscending        as logical  
  ) :

/*------------------------------------------------------------------------
  Name         : getTableList
  Description  : Get a list of all tables in the current database that 
                 match a certain filter. 
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  23-01-2009 pti added filter
  08-10-2009 pti added input parm plShowHiddenTables
  17-12-2009 pti added input parm pcSortField / plAscending
  ----------------------------------------------------------------------*/
  
  define variable cTableList  as character   no-undo.
  define variable cQuery      as character   no-undo.

  define buffer bTable for ttTable.
  define query qTable for bTable.

  if pcDatabaseFilter = '' or pcDatabaseFilter = ? then pcDatabaseFilter = '*'.
  if pcSortField = '' or pcSortField = ? then pcSortField = 'cTableName'.
  if plAscending = ? then plAscending = yes.

  /* Build query */
  cQuery = substitute('for each bTable where cDatabase matches &1', quoter(pcDatabaseFilter)).
  cQuery = substitute("&1 and cTableName matches &2", cQuery, quoter(pcTableFilter )).
  if plShowHiddenTables = false then 
    cQuery = substitute('&1 and lHidden = no', cQuery).
  query qTable:query-prepare( substitute('&1 by &2 &3', cQuery, pcSortField, string(plAscending,'/descending')) ).

  query qTable:query-open.
  query qTable:get-first.

  /* All fields */
  repeat while not query qTable:query-off-end:
    cTableList = cTableList + "," + bTable.cTableName.
    query qTable:get-next.
  end.
  query qTable:query-close.

  cTableList = left-trim(cTableList, ",").

  return cTableList.   /* Function return value. */
  
end function. /* getTableList */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUserName Procedure 
FUNCTION getUserName returns character
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getUserName
  Description  : Returns the UserID used to logon to Windows.

  ----------------------------------------------------------------------
  14-10-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable intResult   as integer   no-undo.
  define variable intSize     as integer   no-undo.

  /* Use the cached value of username if it is known */
  if gcUserName <> ? then return gcUserName.

  /* Otherwise determine the value */
  assign gcUserName = fill(' ',256)
         intSize    = 255.

  run GetUserNameA ( output       gcUserName
                   , input-output intSize
                   , output       intResult
                   ).

  if intResult <> 1 then
    gcUserName = "". 

  return gcUserName.   /* Function return value. */

end function. /* getUserName */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isDefaultFontsChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isDefaultFontsChanged Procedure 
FUNCTION isDefaultFontsChanged returns logical
  ( /* parameter-definitions */ ) :


/*------------------------------------------------------------------------
  Name         : isDefaultFontsChanged
  Description  : Returns whether the default fonts 0-7 were changed.

  ----------------------------------------------------------------------
  26-04-2010 pti Created
  ----------------------------------------------------------------------*/

  define variable cFontSize     as character   no-undo extent 8.
  define variable iFont         as integer     no-undo.
  define variable lFontsChanged as logical     no-undo.
  
  /* These are the expected fontsizes of the text 'DataDigger' */
  cFontSize[1] = '70/14'.
  cFontSize[2] = '54/13'.
  cFontSize[3] = '70/14'.
  cFontSize[4] = '70/14'.
  cFontSize[5] = '54/13'.
  cFontSize[6] = '70/16'.
  cFontSize[7] = '65/13'.
  cFontSize[8] = '54/13'.
  
  /* Innocent until proven guilty */
  lFontsChanged = no.

  checkFont:
  do iFont = 0 to 7:
    if cFontSize[iFont + 1] <> substitute('&1/&2'
                                         , font-table:get-text-width-pixels('DataDigger',iFont) 
                                         , font-table:get-text-height-pixels(iFont)
                                         ) then 
    do:
      lFontsChanged = true.
      leave checkFont.
    end.
  end. /* checkFont */

  return lFontsChanged.

end function. /* isDefaultFontsChanged */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isFileLocked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isFileLocked Procedure 
FUNCTION isFileLocked RETURNS LOGICAL
  ( pcFileName as character ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE lpSecurityAtt AS INTEGER NO-UNDO.
  DEFINE VARIABLE iFileHandle   AS INTEGER NO-UNDO.
  DEFINE VARIABLE nReturn       AS INTEGER NO-UNDO.
  
  /* Try to lock the file agains writing */
  RUN CreateFileA ( INPUT pcFileName
                  , INPUT {&GENERIC_WRITE}
                  , {&FILE_SHARE_READ}
                  , lpSecurityAtt
                  , {&OPEN_EXISTING}
                  , {&FILE_ATTRIBUTE_NORMAL}
                  , 0
                  , OUTPUT iFileHandle
                  ).
  
  /* Release file handle */
  RUN CloseHandle ( INPUT iFileHandle
                  , OUTPUT nReturn
                  ).

  RETURN (iFileHandle = -1).

END FUNCTION. /* isFileLocked */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isWidgetChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isWidgetChanged Procedure 
FUNCTION isWidgetChanged RETURNS LOGICAL PRIVATE
  ( input phWidget as handle ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  define variable lChangeDetected as logical no-undo. 

  define buffer ttWidget for ttWidget.

  if not valid-handle(phWidget) then return false. 

  find ttWidget where ttWidget.hWidget = phWidget no-error.
  if not available ttWidget then 
  do:
    create ttWidget.
    assign ttWidget.hWidget = phWidget.
  end.

  if ttWidget.iPosX   <> phWidget:x
  or ttWidget.iPosY   <> phWidget:y
  or ttWidget.iWidth  <> phWidget:width-pixels then 
  do:
    assign
      ttWidget.iPosX   = phWidget:x            
      ttWidget.iPosY   = phWidget:y            
      ttWidget.iWidth  = phWidget:width-pixels 
      ttWidget.iHeight = phWidget:height-pixels
      lChangeDetected = true.
  end.

  return lChangeDetected.

END FUNCTION. /* isWidgetChanged */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resolveOsVars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION resolveOsVars Procedure 
FUNCTION resolveOsVars RETURNS CHARACTER
  ( pcString as character ) :

  define variable iPercStart   as integer no-undo. 
  define variable iPercEnd     as integer no-undo. 
  define variable cEnvVarName  as character no-undo. 
  define variable cEnvVarValue as character no-undo. 
  define variable cReturnValue as character no-undo. 


  /* Support for OS-directives between % eg: %username% will expand
   * to your username, as long as you have an OS-var for that. Type 'set' on the dos prompt for 
   * a list of all environment variables
   */
  cReturnValue = pcString.
  iPercStart = index(cReturnValue,'%').
  
  resolveOsVars:
  do while iPercStart > 0:
    iPercEnd = index(cReturnValue,'%',iPercStart + 1).
    
    if iPercEnd = 0 then leave resolveOsVars. /* single % */
    cEnvVarName = trim( substring(cReturnValue,iPercStart, iPercEnd - iPercStart) ,'%'). /* Grab text between % */
    
    /* Search in the registry */
    load "System" base-key "HKEY_LOCAL_MACHINE".
    use "System".
    get-key-value section "CurrentControlSet~\Control~\Session Manager~\Environment" key cEnvVarName value cEnvVarValue.
    unload "System".
    
    /* If not defined, try our luck in the default env */
    if cEnvVarValue = ? then
      cEnvVarValue = os-getenv(cEnvVarName) . /* try to resolve */
    
    /* If still not found, step to next % */
    if cEnvVarValue = ? then
    do:
      iPercStart = iPercEnd.
      next resolveOsVars.  
    end.
    
    cReturnValue = replace(cReturnValue,'%' + cEnvVarName + '%', cEnvVarValue). /* Replace with value */
    iPercStart = index(cReturnValue,'%'). /* Find next directive */
  end.

  RETURN cReturnValue.

end function. /* resolveOsVars */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resolveSequence) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION resolveSequence Procedure 
FUNCTION resolveSequence returns character
  ( pcString as character ) :

  define variable iFileNr       as integer    no-undo.
  define variable cSeqMask      as character  no-undo .
  define variable cSeqFormat    as character  no-undo .
  define variable cFileName     as character   no-undo.

  cFileName = pcString.

  /* User can specify a sequence for the file. The length of 
   * the tag sets the format: <###> translates to a 3-digit nr
   * Special case is <#> which translates to no leading zeros
   */ 
  if    index(cFileName,'<#') > 0 
    and index(cFileName,'#>') > 0 then
  do:
    cSeqMask = substring(cFileName,index(cFileName,'<#')). /* <#####>tralalala */
    cSeqMask = substring(cSeqMask,1,index(cSeqMask,'>')). /* <#####> */
    cSeqFormat = trim(cSeqMask,'<>'). /* ##### */
    cSeqFormat = replace(cSeqFormat,'#','9').
    if cSeqFormat = '9' then cSeqFormat = '>>>>>>>>>9'.

    setFileNr:
    repeat:
      iFileNr = iFileNr + 1.
      if search(replace(cFileName,cSeqMask,trim(string(iFileNr,cSeqFormat)))) = ? then 
      do:
        cFileName = replace(cFileName,cSeqMask,trim(string(iFileNr,cSeqFormat))).
        leave setFileNr.
      end.
    end.
  end.

  return cFileName.

end function. /* resolveSequence */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setColumnWidthList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setColumnWidthList Procedure 
FUNCTION setColumnWidthList returns logical
  ( input phBrowse    as handle 
  , input pcWidthList as character):

  /* set all specified columns in pcWidthList to a specified width like:
   * custnum:12,custname:20,city:12
   */
  define variable cColumnName  as character   no-undo.
  define variable cListItem    as character   no-undo.
  define variable hColumn      as handle      no-undo.
  define variable iColumn      as integer     no-undo.
  define variable iColumnWidth as integer     no-undo.
  define variable iListItem    as integer     no-undo.
  
  do iListItem = 1 to num-entries(pcWidthList):
    cListItem    = entry(iListItem,pcWidthList).
    cColumnName  = entry(1,cListItem,':') no-error.
    iColumnWidth = integer(entry(2,cListItem,':')) no-error.
    
    do iColumn = 1 to phBrowse:num-columns:
      hColumn = phBrowse:get-browse-column(iColumn).
      if hColumn:name = cColumnName then
        hColumn:width-pixels = iColumnWidth.
    end. /* iColumn */
  end. /* iListItem */
  
  return true. 
end function. /* setColumnWidthList */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFilterFieldColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFilterFieldColor Procedure 
FUNCTION setFilterFieldColor returns logical
  ( phWidget as handle ) :

/*------------------------------------------------------------------------
  Name         : setFilterFieldColor
  Description  : If you enter the field and you have not put in a filter
                 clear out the field so you can type something yourself. 
                 
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  if phWidget:screen-value = phWidget:private-data then 
    phWidget:fgcolor = 7.
  else 
    phWidget:fgcolor = ?.

  return true.

END FUNCTION. /* setFilterFieldColor */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLinkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLinkInfo Procedure 
FUNCTION setLinkInfo returns logical
  ( input pcFieldName as character
  , input pcValue     as character
  ):

/*------------------------------------------------------------------------
  Name         : setLinkInfo
  Description  : Save name/value of a field.
  ----------------------------------------------------------------------
  21-10-2009 pti Created
  ----------------------------------------------------------------------*/

  define buffer bLinkInfo for ttLinkInfo. 

  find bLinkInfo where bLinkInfo.cField = pcFieldName no-error.
  if not available bLinkInfo then
  do:
    create bLinkInfo.
    assign bLinkInfo.cField = pcFieldName.
  end.

  bLinkInfo.cValue = pcValue.

  return true.   /* Function return value. */

end function. /* setLinkInfo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRegistry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRegistry Procedure 
FUNCTION setRegistry returns character
    ( pcSection as character
    , pcKey     as character
    , pcValue   as character
    ) :

  /*------------------------------------------------------------------------
    Name         : setRegistry 
    Description  : Set a value in the registry. 
    ---------------------------------------------------------------------- 
    15-01-2009 pti Created
    ----------------------------------------------------------------------*/

    use substitute('DataDigger-&1', getUserName() ).

    put-key-value 
      section pcSection
      key     pcKey
      value   pcValue
      no-error
      .

    use "".
    return "". /* Function return value. */

end function. /* setRegistry */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

