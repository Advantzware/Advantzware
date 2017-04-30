&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wLoadData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wLoadData 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{ datadigger.i }

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) NE 0  &THEN
   &SCOPED-DEFINE invar  VARIABLE 
   &SCOPED-DEFINE iovar  VARIABLE 
   &SCOPED-DEFINE outvar VARIABLE    
&ELSE
   &SCOPED-DEFINE invar   INPUT PARAMETER      
   &SCOPED-DEFINE iovar   INPUT-OUTPUT PARAMETER      
   &SCOPED-DEFINE outvar  OUTPUT PARAMETER
&ENDIF

define {&invar} pcDatabase as character no-undo.
define {&invar} pcTable    as character no-undo.
define {&invar} pcFile     as character no-undo.

/* tt to share settings between viewers */
define temp-table ttData no-undo rcode-info
  field cName  as character 
  field cValue as character 
  index iPrim is primary cName.

/* Local Variable Definitions ---                                       */

define variable gcSessionNumericFormat as character no-undo.
define variable gcSessionDateFormat    as character no-undo.
define variable giSessionYearOffset    as integer   no-undo.

define variable giNumPages             as integer             no-undo.
define variable giPage                 as integer             no-undo.
define variable ghTabFrame             as handle    extent 10 no-undo.
define variable ghTabFolder            as handle    extent 10 no-undo.
define variable gcTabFolder            as character extent 10 no-undo.

gcTabFolder[1] = 'frLoadChooseFile.w'.
gcTabFolder[2] = 'frLoadSettings.w'.
gcTabFolder[3] = 'frLoadMapping.w'.
gcTabFolder[4] = 'frLoadImport.w'.

giNumPages = 4.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frLoadData

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnCancel BtnPrev BtnNext btnFinish 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getData wLoadData 
FUNCTION getData returns character
  ( pcName as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setData wLoadData 
FUNCTION setData returns logical
  ( pcName  as character 
  , pcValue as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wLoadData AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnFinish DEFAULT 
     LABEL "&Finish" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnNext 
     LABEL ">" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnPrev 
     LABEL "<" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frLoadData
     BtnCancel AT ROW 31.48 COL 130 WIDGET-ID 22
     BtnPrev AT ROW 31.48 COL 146 WIDGET-ID 96
     BtnNext AT ROW 31.48 COL 162 WIDGET-ID 94
     btnFinish AT ROW 31.48 COL 178 WIDGET-ID 108
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 196.2 BY 32.05
         DEFAULT-BUTTON BtnNext WIDGET-ID 100.


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
  CREATE WINDOW wLoadData ASSIGN
         HIDDEN             = YES
         TITLE              = "Load Data"
         HEIGHT             = 32.05
         WIDTH              = 196.2
         MAX-HEIGHT         = 45.1
         MAX-WIDTH          = 272.6
         VIRTUAL-HEIGHT     = 45.1
         VIRTUAL-WIDTH      = 272.6
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
/* SETTINGS FOR WINDOW wLoadData
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frLoadData
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wLoadData)
THEN wLoadData:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wLoadData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wLoadData wLoadData
ON END-ERROR OF wLoadData /* Load Data */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wLoadData wLoadData
ON WINDOW-CLOSE OF wLoadData /* Load Data */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFinish
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFinish wLoadData
ON CHOOSE OF btnFinish IN FRAME frLoadData /* Finish */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:34 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNext wLoadData
ON CHOOSE OF BtnNext IN FRAME frLoadData /* > */
DO:
  define variable lOk as logical no-undo. 

  if giPage < giNumPages then 
  do:
    run commitPage in ghTabFolder[giPage] (output lOk) no-error.
    if lOk then run setPage(giPage + 1).
  end.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:34 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnPrev wLoadData
ON CHOOSE OF BtnPrev IN FRAME frLoadData /* < */
DO:

  if giPage > 1 then run setPage(giPage - 1).

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:34 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wLoadData 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:34 am */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  run initializeObject.

  /* Set input data file */
  setRegistry('DumpAndLoad', 'LoadFile',  pcFile).
  run setPage(1).

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:34 am */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* Restore settings */
session:date-format    = gcSessionDateFormat.
session:numeric-format = gcSessionNumericFormat.
session:year-offset    = giSessionYearOffset.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wLoadData  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wLoadData)
  THEN DELETE WIDGET wLoadData.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wLoadData  _DEFAULT-ENABLE
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
  ENABLE BtnCancel BtnPrev BtnNext btnFinish 
      WITH FRAME frLoadData IN WINDOW wLoadData.
  {&OPEN-BROWSERS-IN-QUERY-frLoadData}
  VIEW wLoadData.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wLoadData 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  define variable cLoadReadyAction as character   no-undo.

  /* Save original settings */
  gcSessionDateFormat    = session:date-format.
  gcSessionNumericFormat = session:numeric-format.

  /* Make sure the lib is running */
  run startDiggerLib.

/* debug */
/*  load 'DataDigger-nljrpti' base-key 'ini' no-error. */
/* debug */


  setData('TargetDatabase', pcDatabase).
  setData('TargetTable', pcTable).


END PROCEDURE. /* initializeObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPage wLoadData 
PROCEDURE setPage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter piTab as integer     no-undo.

  define variable iTabHide as integer     no-undo.

  /* Allowed to switch to this tab? */

  /* Hide current tab */
  run hideFrame in ghTabFolder[giPage] no-error.

  /* Set global for page nr */
  giPage = piTab.

  /* Start procedure if needed */
  if not valid-handle(ghTabFolder[piTab]) then
  do:
    run value(getProgramDir() + gcTabFolder[piTab]) persistent set ghTabFolder[piTab] 
      (input this-procedure:handle).

    run setFrame in ghTabFolder[giPage] 
      ( frame frLoadData:x 
      , frame frLoadData:y 
      , frame frLoadData:width-pixels 
      , frame frLoadData:height-pixels - 40 ) no-error.
  end.

  /* Show the frame */
  run viewFrame in ghTabFolder[giPage].

  /* Refresh data on the frame */
  run refreshFrame in ghTabFolder[giPage] no-error.

  /* Enable / disable buttons */
  do with frame {&frame-name}:
    btnPrev:sensitive = (giPage > 1). 
    btnNext:sensitive = (giPage < giNumPages). 
  end.

end procedure. /* setPage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startDiggerLib wLoadData 
PROCEDURE startDiggerLib :
/*------------------------------------------------------------------------
  Name         : startDiggerLib
  Description  : Start DiggerLib if it has not already been started

  ----------------------------------------------------------------------
  21-10-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable hDiggerLib   as handle      no-undo.
  define variable hProcedure   as handle      no-undo.
  define variable cProgDir     as character   no-undo.

  /* Call out to see if the lib has been started */
  publish 'DiggerLib' (output hProcedure). 

  if not valid-handle(hProcedure) then
  do:
    file-info:file-name = this-procedure:file-name.
    cProgDir = substring(file-info:full-pathname,1,r-index(file-info:full-pathname,'\')).
    if '{&uib_is_running}' <> '' then cProgDir = 'd:\data\progress\datadigger\'.

    run value(cProgDir + 'DataDiggerLib.p') persistent set hProcedure. 
    session:add-super-procedure(hProcedure,search-target).
  end.

end procedure. /* startDiggerLib */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getData wLoadData 
FUNCTION getData returns character
  ( pcName as character ) :

  define buffer ttData for ttData. 

  find ttData where ttData.cName = pcName no-error.
  if available ttData then 
    return ttData.cValue.
  else 
    return ?.

end function. /* getData */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setData wLoadData 
FUNCTION setData returns logical
  ( pcName  as character 
  , pcValue as character ) :

  define buffer ttData for ttData. 

  find ttData where ttData.cName = pcName no-error.
  if not available ttData then create ttData. 

  assign 
    ttData.cName  = pcName
    ttData.cValue = pcValue
    .

  return yes.

end function. /* setData */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
