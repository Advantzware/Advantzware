&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: patchftp.w

  Description: utility to download patches and run them

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 4.25.2005

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppPatcher.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE webDir web
&SCOPED-DEFINE localDir system
&SCOPED-DEFINE commandFile {&localDir}/patches.cmd
&SCOPED-DEFINE patchList patchList.txt
&SCOPED-DEFINE URL advantzware.com
&SCOPED-DEFINE user asi
&SCOPED-DEFINE password yorkie

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE patchDat AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tPatchDat NO-UNDO
  FIELD patch AS INTEGER
  FIELD version AS CHARACTER
  FIELD seq AS CHARACTER
  FIELD id AS INTEGER
  FIELD run-order AS INTEGER
  FIELD utility AS CHARACTER
  FIELD run-once AS LOGICAL
  FIELD descr AS CHARACTER
  FIELD dependancy AS CHARACTER.

CREATE tPatchDat.

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS patchesAvailable currentPatches ~
btnCheckPatch btnRemovePatch btnDownloadPatch btnRunPatch btnShowHistory ~
btnImcomplete 
&Scoped-Define DISPLAYED-OBJECTS patchesAvailable currentPatches 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dependancyOK C-Win 
FUNCTION dependancyOK RETURNS LOGICAL
  (ipPatch AS INTEGER,ipVersion AS CHARACTER,ipSeq AS CHARACTER,
   ipDependancy AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD missingPatchHst C-Win 
FUNCTION missingPatchHst RETURNS LOGICAL
  (ipPatchName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD timeStamp C-Win 
FUNCTION timeStamp RETURNS CHARACTER
  (ipStringValue AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCheckPatch 
     LABEL "&Check for Current Patch" 
     SIZE 37 BY 1.14.

DEFINE BUTTON btnDownloadPatch 
     LABEL "&Download Current Patch" 
     SIZE 37 BY 1.14.

DEFINE BUTTON btnImcomplete 
     LABEL "Re-Run &Incomplete Patches" 
     SIZE 37 BY 1.14.

DEFINE BUTTON btnRemovePatch 
     LABEL "Remove &Selected Patch" 
     SIZE 37 BY 1.14.

DEFINE BUTTON btnRunPatch 
     LABEL "&Run Latest Patch Downloaded" 
     SIZE 37 BY 1.14.

DEFINE BUTTON btnShowHistory 
     LABEL "Show Patch &History" 
     SIZE 37 BY 1.14.

DEFINE VARIABLE currentPatches AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 37 BY 12.14 NO-UNDO.

DEFINE VARIABLE patchesAvailable AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 37 BY 12.14 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     patchesAvailable AT ROW 1.95 COL 2 NO-LABEL
     currentPatches AT ROW 1.95 COL 41 NO-LABEL
     btnCheckPatch AT ROW 14.33 COL 2
     btnRemovePatch AT ROW 14.33 COL 41
     btnDownloadPatch AT ROW 15.76 COL 2
     btnRunPatch AT ROW 15.76 COL 41
     btnShowHistory AT ROW 17.19 COL 2
     btnImcomplete AT ROW 17.19 COL 41
     "Downloaded Patches" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.24 COL 41
     "Patches Available" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.6 BY 17.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Web Patch Download and Run"
         HEIGHT             = 17.48
         WIDTH              = 77.6
         MAX-HEIGHT         = 17.48
         MAX-WIDTH          = 77.6
         VIRTUAL-HEIGHT     = 17.48
         VIRTUAL-WIDTH      = 77.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Web Patch Download and Run */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Web Patch Download and Run */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCheckPatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCheckPatch C-Win
ON CHOOSE OF btnCheckPatch IN FRAME DEFAULT-FRAME /* Check for Current Patch */
DO:
  RUN checkPatch.
  RUN uploadPatchDat.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDownloadPatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDownloadPatch C-Win
ON CHOOSE OF btnDownloadPatch IN FRAME DEFAULT-FRAME /* Download Current Patch */
DO:
  RUN downloadPatch.
  RUN uploadPatchDat.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImcomplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImcomplete C-Win
ON CHOOSE OF btnImcomplete IN FRAME DEFAULT-FRAME /* Re-Run Incomplete Patches */
DO:
  RUN runPatchHst.
  RUN uploadPatchDat.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemovePatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemovePatch C-Win
ON CHOOSE OF btnRemovePatch IN FRAME DEFAULT-FRAME /* Remove Selected Patch */
DO:
  RUN removePatch.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRunPatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunPatch C-Win
ON CHOOSE OF btnRunPatch IN FRAME DEFAULT-FRAME /* Run Latest Patch Downloaded */
DO:
  RUN runPatchDownload.
  RUN uploadPatchDat.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShowHistory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowHistory C-Win
ON CHOOSE OF btnShowHistory IN FRAME DEFAULT-FRAME /* Show Patch History */
DO:
  RUN system/patchhst.w PERSISTENT.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  FIND FIRST clientid NO-LOCK NO-ERROR.
  IF NOT AVAILABLE clientid THEN DO:
    CREATE clientid.
    clientid.client-id = 'UNKNOWN'.
  END.
  patchDat = clientid.client-id + '.patch.dat'.
  RUN getCurrentPatches.
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildPatchDat C-Win 
PROCEDURE buildPatchDat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OUTPUT TO VALUE('{&localDir}' + '/' + patchDat).
  FOR EACH patchhst NO-LOCK:
    EXPORT patchhst.
  END.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkPatch C-Win 
PROCEDURE checkPatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE commandList AS CHARACTER NO-UNDO.

  ASSIGN
    commandList = 'get {&patchList}'
    patchesAvailable:LIST-ITEMS IN FRAME {&FRAME-NAME} = 'Getting List of Available Patches ...'.
  PROCESS EVENTS.
  RUN patchCommandFile ('ascii',commandList).
  RUN runCommandFile.
  RUN patchesAvailable.
  RUN removeCommandFile.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE downloadPatch C-Win 
PROCEDURE downloadPatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE commandList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  RUN checkPatch.
  ASSIGN
    currentPatches:LIST-ITEMS IN FRAME {&FRAME-NAME} = 'Downloading Current Patch ...'
    j = patchesAvailable:NUM-ITEMS.
  PROCESS EVENTS.
  DO i = 1 TO j:
    IF SEARCH('{&localDir}/' + patchesAvailable:ENTRY(i)) NE ? THEN NEXT.
    IF missingPatchHst(patchesAvailable:ENTRY(i)) THEN
    commandList = commandList + (IF commandList NE '' THEN ',' ELSE '') +
                 'mget ' + REPLACE(patchesAvailable:ENTRY(i),'.exe','.*').
  END. /* do i */
  RUN patchCommandFile ('binary',commandList).
  RUN runCommandFile.
  RUN removeCommandFile.
  RUN getCurrentPatches.
  patchesAvailable:SCREEN-VALUE = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY patchesAvailable currentPatches 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE patchesAvailable currentPatches btnCheckPatch btnRemovePatch 
         btnDownloadPatch btnRunPatch btnShowHistory btnImcomplete 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCurrentPatches C-Win 
PROCEDURE getCurrentPatches :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE patchName AS CHARACTER NO-UNDO.

  currentPatches:LIST-ITEMS IN FRAME {&FRAME-NAME} = ''.
  INPUT FROM OS-DIR('{&localDir}').
  REPEAT WITH FRAME {&FRAME-NAME}:
    IMPORT patchName.
    IF NOT patchName BEGINS 'patch' OR INDEX(patchName,'.exe') EQ 0 THEN NEXT.
    currentPatches:ADD-LAST(patchName).
  END.
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPatchKeys C-Win 
PROCEDURE getPatchKeys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPatchName AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opPatch AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opVersion AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opSeq AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lvTmp AS CHARACTER NO-UNDO.

  ASSIGN
    lvTmp = REPLACE(ipPatchName,'.exe','')
    opSeq = SUBSTR(lvTmp,R-INDEX(lvTmp,'.') + 1)
    SUBSTR(lvTmp,R-INDEX(lvTmp,'.'),50) = ''
    opVersion = SUBSTR(lvTmp,R-INDEX(lvTmp,'.') + 1)
    SUBSTR(lvTmp,R-INDEX(lvTmp,'.'),50) = ''
    opPatch = INTEGER(REPLACE(lvTmp,'patch.','')).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadPatchHst C-Win 
PROCEDURE loadPatchHst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPatchDat AS CHARACTER NO-UNDO.

  IF SEARCH(ipPatchDat) EQ ? THEN RETURN.
  SESSION:SET-WAIT-STATE('General':U).
  INPUT FROM VALUE(SEARCH(ipPatchDat)) NO-ECHO.
  REPEAT:
    IMPORT tPatchDat.
    FIND patchhst EXCLUSIVE-LOCK WHERE patchhst.id EQ tPatchDat.id NO-ERROR.
    IF NOT AVAILABLE patchhst THEN DO:
      CREATE patchhst.
      patchhst.id = tPatchDat.id.
    END.
    ASSIGN
      patchhst.patch = tPatchDat.patch
      patchhst.version = tPatchDat.version
      patchhst.seq = tPatchDat.seq
      patchhst.run-order = tPatchDat.run-order
      patchhst.utility = tPatchDat.utility
      patchhst.run-once = tPatchDat.run-once
      patchhst.descr = tPatchDat.descr
      patchhst.dependancy = tPatchDat.dependancy.
  END. /* repeat */
  INPUT CLOSE.
  FIND CURRENT clientid EXCLUSIVE-LOCK.
  ASSIGN
    clientid.patch = tPatchDat.patch
    clientid.version = tPatchDat.version
    clientid.seq = tPatchDat.seq.
  FIND CURRENT clientid NO-LOCK.
  SESSION:SET-WAIT-STATE('':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE patchCommandFile C-Win 
PROCEDURE patchCommandFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipCommand AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  OUTPUT TO '{&commandFile}'.
  PUT UNFORMATTED
    'open {&URL}' SKIP
    '{&user}' SKIP
    '{&password}' SKIP
    'cd {&webDir}' SKIP
    'lcd {&localDir}' SKIP
    ipType SKIP.
  DO i = 1 TO NUM-ENTRIES(ipCommand):
    PUT UNFORMATTED ENTRY(i,ipCommand) SKIP.
  END.
  PUT UNFORMATTED 'quit' SKIP.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE patchesAvailable C-Win 
PROCEDURE patchesAvailable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE patchName AS CHARACTER NO-UNDO.

  patchesAvailable:LIST-ITEMS IN FRAME {&FRAME-NAME} = ''.
  INPUT FROM VALUE('{&localDir}/{&patchList}') NO-ECHO.
  REPEAT WITH FRAME {&FRAME-NAME}:
    IMPORT UNFORMATTED patchName.
    patchesAvailable:ADD-LAST(patchName).
  END.
  INPUT CLOSE.
  IF patchesAvailable:NUM-ITEMS NE 0 THEN
  ENABLE btnDownloadPatch WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processPatchHst C-Win 
PROCEDURE processPatchHst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPatchName AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lvPatch AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvVersion AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvSeq AS CHARACTER NO-UNDO.

  RUN getPatchKeys (ipPatchName,OUTPUT lvPatch,OUTPUT lvVersion,OUTPUT lvSeq).
  FOR EACH patchhst EXCLUSIVE-LOCK
      WHERE patchhst.patch EQ lvPatch
        AND patchhst.version EQ lvVersion
        AND patchhst.seq EQ lvSeq
        AND patchhst.completed EQ NO:
    RUN runUtility.
  END. /* each patchhst */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE removeCommandFile C-Win 
PROCEDURE removeCommandFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OS-DELETE VALUE(REPLACE('{&commandFile}','/','\')).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE removePatch C-Win 
PROCEDURE removePatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE patchName AS CHARACTER NO-UNDO.

  IF currentPatches:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN RETURN.
  MESSAGE 'Remove Selected Patch?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE removePatch AS LOGICAL.
  IF removePatch THEN
  DO:
    patchName = '{&localDir}\' + currentPatches:SCREEN-VALUE.
    OS-DELETE VALUE(patchName).
    OS-DELETE VALUE(REPLACE(patchName,'exe','dat')).
    RUN getCurrentPatches.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runCommandFile C-Win 
PROCEDURE runCommandFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OS-COMMAND NO-CONSOLE VALUE('ftp -v -i -s:{&commandFile}').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runPatchDownload C-Win 
PROCEDURE runPatchDownload :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE patchName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  j = currentPatches:NUM-ITEMS IN FRAME {&FRAME-NAME}.
  IF j EQ 0 THEN RETURN.
  DO i = 1 TO j:
    patchName = '{&localDir}\' + currentPatches:ENTRY(i).
    IF SEARCH(patchName) EQ ? OR
       NOT missingPatchHst(currentPatches:ENTRY(i)) THEN NEXT.
    IF SEARCH('{&localDir}\patchInstalled') NE ? THEN
    OS-DELETE VALUE(SEARCH('{&localDir}\patchInstalled')).
    OS-COMMAND NO-CONSOLE VALUE(patchName).
    IF SEARCH('{&localDir}\patchInstalled') EQ ? THEN NEXT.
    RUN loadPatchHst (REPLACE(patchName,'exe','dat')).
    RUN processPatchHst (currentPatches:ENTRY(i)).
  END. /* do i */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runPatchHst C-Win 
PROCEDURE runPatchHst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH patchhst EXCLUSIVE-LOCK WHERE patchhst.completed EQ NO:
    RUN runUtility.
  END. /* each patchhst */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runUtility C-Win 
PROCEDURE runUtility :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  patchhst.user_id = USERID('NOSWEAT').
  IF SEARCH(patchhst.utility) NE ? THEN DO:
    IF dependancyOK(patchhst.patch,patchhst.version,
                    patchhst.seq,patchhst.dependancy) THEN DO:
      IF patchhst.utility BEGINS 'db_delta' THEN DO:
        IF INDEX(DBPARAM('NOSWEAT'),'-1') NE 0 THEN DO:
          patchhst.returnvalue = timeStamp('Not Yet Implemented').
        END. /* if lookup */
        ELSE patchhst.returnvalue = timeStamp('Multi User Mode').
      END. /* if delta */
      ELSE DO:
        RUN VALUE(SEARCH(patchhst.utility)).
        IF RETURN-VALUE NE '' THEN /* failed */
        patchhst.returnvalue = timeStamp(RETURN-VALUE).
        ELSE /* successful run */
        ASSIGN
          patchhst.returnvalue = ''
          patchhst.patch-date = TODAY
          patchhst.patch-time = TIME
          patchhst.completed = YES.
      END. /* else, not delta */
    END. /* if dependencyOK */
    ELSE patchhst.returnvalue = timeStamp('Dependency Not Completed').
  END. /* if search */
  ELSE patchhst.returnvalue = timeStamp('Utility Does Not Exist').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uploadPatchDat C-Win 
PROCEDURE uploadPatchDat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE commandList AS CHARACTER NO-UNDO.

  RUN buildPatchDat.
  commandList = 'put ' + patchDat.
  RUN patchCommandFile ('ascii',commandList).
  RUN runCommandFile.
  RUN removeCommandFile.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dependancyOK C-Win 
FUNCTION dependancyOK RETURNS LOGICAL
  (ipPatch AS INTEGER,ipVersion AS CHARACTER,ipSeq AS CHARACTER,
   ipDependancy AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE runOrder AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  j = NUM-ENTRIES(ipDependancy).
  DO i = 1 TO j:
    runOrder = INTEGER(ENTRY(i,ipDependancy)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.
    IF CAN-FIND(FIRST patchhst
                WHERE patchhst.patch EQ ipPatch
                  AND patchhst.version EQ ipVersion
                  AND patchhst.seq EQ ipSeq
                  AND patchhst.run-order EQ runOrder
                  AND patchhst.completed EQ NO) THEN
    RETURN FALSE.
  END.
  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION missingPatchHst C-Win 
FUNCTION missingPatchHst RETURNS LOGICAL
  (ipPatchName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lvPatch AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvVersion AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvSeq AS CHARACTER NO-UNDO.

  RUN getPatchKeys (ipPatchName,OUTPUT lvPatch,OUTPUT lvVersion,OUTPUT lvSeq).
  RETURN NOT CAN-FIND(FIRST patchhst WHERE patchhst.patch EQ lvPatch
                                       AND patchhst.version EQ lvVersion
                                       AND patchhst.seq EQ lvSeq).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION timeStamp C-Win 
FUNCTION timeStamp RETURNS CHARACTER
  (ipStringValue AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN ipStringValue + ' (' +
         STRING(TODAY,'99.99.9999') + '-' +
         LEFT-TRIM(STRING(TIME,'HH:MM:SSam')) + ')'.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

