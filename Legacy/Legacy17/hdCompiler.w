&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: hdCompiler.w

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

&SCOPED-DEFINE taskList p:/asi_gui10/pcoDir/compTask/rco
&SCOPED-DEFINE saveInto p:/asi10test/patch/rco

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipVer AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE compiling AS LOGICAL NO-UNDO.
DEF VAR v-oldpropath AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS processingTask compilingProgram 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE compilingProgram AS CHARACTER FORMAT "X(256)":U INITIAL "Waiting ..." 
     LABEL "Compiling" 
     VIEW-AS FILL-IN 
     SIZE 78 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE processingTask AS CHARACTER FORMAT "X(256)":U INITIAL "Waiting ..." 
     LABEL "Processing Task" 
     VIEW-AS FILL-IN 
     SIZE 78 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     processingTask AT ROW 1.95 COL 17 COLON-ALIGNED
     compilingProgram AT ROW 3.14 COL 17 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99 BY 4.


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
         TITLE              = "Help Desk Compiler for"
         HEIGHT             = 4
         WIDTH              = 99
         MAX-HEIGHT         = 4
         MAX-WIDTH          = 99
         VIRTUAL-HEIGHT     = 4
         VIRTUAL-WIDTH      = 99
         MAX-BUTTON         = no
         RESIZE             = no
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
/* SETTINGS FOR FILL-IN compilingProgram IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN processingTask IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME




/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 3.38
       COLUMN          = 2
       HEIGHT          = 1.43
       WIDTH           = 7
       HIDDEN          = yes
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(compilingProgram:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Help Desk Compiler for */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Help Desk Compiler for */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

RUN Check4tasks.

END PROCEDURE.

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
  {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + ' - Version: ' + ipVer.
  RUN enable_UI.

  v-oldpropath = PROPATH.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check4Tasks C-Win 
PROCEDURE check4Tasks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hdTask AS CHARACTER NO-UNDO.

  RUN getTask (OUTPUT hdTask).
  IF hdTask NE '' THEN
  RUN compileTask (hdTask).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE compileTask C-Win 
PROCEDURE compileTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipTaskFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE prgmSCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE prgmRCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE viEntry   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vcError   AS CHARACTER  NO-UNDO.
  DEF VAR v-addon AS LOG NO-UNDO.

  compiling = YES.
  OS-DELETE VALUE(REPLACE(ipTaskFile,'.lst','.log')).
  OUTPUT TO VALUE(REPLACE(ipTaskFile,'.lst','.tmp')).
  INPUT FROM VALUE(ipTaskFile) NO-ECHO.
  REPEAT WITH STREAM-IO:
    IMPORT prgmSCode.
    ASSIGN
      processingTask:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ipTaskFile
      compilingProgram:SCREEN-VALUE = prgmSCode
      prgmRCode = REPLACE(prgmSCode,'pco','rco')
      prgmRCode = SUBSTR(prgmRCode,1,R-INDEX(prgmRCode,'\') - 1).
    PUT UNFORMATTED 'Compiled ' prgmSCode ' ...' SKIP.

    /* dgd 04/09/2007 - BEGIN */ 
    if substring (prgmSCode, r-index (prgmSCode,'.') + 1) = 'w' or
       substring (prgmSCode, r-index (prgmSCode,'.') + 1) = 'p' then
       DO:
          IF INDEX(prgmSCode,"addon\") NE 0 OR
             INDEX(prgmSCode,"addon/") NE 0 THEN
             v-addon = YES.
          ELSE
             v-addon = NO.

          PROPATH = 'p:\asi10test\patch\pco' + ipVer
                  + (IF v-addon THEN ',p:\asi10test\patch\pco' + ipVer + '\addon,'
                     ELSE ",")
                  + 'p:\asi10test\pco' + ipVer
                  + (IF v-addon THEN ',p:\asi10test\pco' + ipVer + '\addon,'
                     ELSE ",")
                  + v-oldpropath.

          COMPILE VALUE(prgmSCode) SAVE INTO VALUE(prgmRCode) no-error. 
       END.
    if compiler:error then
    do:
      vcError = "   Line   : "  + string (compiler:error-row).
      put unformatted vcError skip.
      do viEntry = 1 to error-status:num-messages:
        vcError = "   Problem: " + error-status:get-message  (viEntry).
        put unformatted vcError skip.
      end.
    end.
    /* dgd 04/09/2007 - END */ 

    PROCESS EVENTS.
  END. /* repeat */
  INPUT CLOSE.
  OUTPUT CLOSE.
  OS-DELETE VALUE(ipTaskFile).
  OS-RENAME VALUE(REPLACE(ipTaskFile,'.lst','.tmp')) VALUE(REPLACE(ipTaskFile,'.lst','.log')).
  ASSIGN
    processingTask:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'Waiting ...'
    compilingProgram:SCREEN-VALUE = 'Waiting ...'
    compiling = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "hdCompiler.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "hdCompiler.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  DISPLAY processingTask compilingProgram 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTask C-Win 
PROCEDURE getTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opTaskFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE taskDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE taskFile AS CHARACTER NO-UNDO FORMAT 'X(30)'.
  DEFINE VARIABLE attrList AS CHARACTER NO-UNDO FORMAT 'X(4)'.

  ASSIGN
    taskDir = '{&taskList}' + ipVer
    taskDir = REPLACE(taskDir,'pcoDir','pco' + ipVer).

  /* Validate that the directory is good. */
  os-create-dir value (taskDir). /* dgd - 04/09/2007 */
  if os-error = 0 then           /* dgd - 04/09/2007 */
  do:                            /* dgd - 04/09/2007 */
    INPUT FROM OS-DIR(taskDir) no-echo.
    REPEAT:
      SET taskFile ^ attrList.
      IF attrList NE 'f' OR INDEX(taskFile,'.lst') EQ 0 THEN NEXT.
      opTaskFile = taskDir + '/' + taskFile.
    END.
    INPUT CLOSE.  
  end.                          /* dgd - 04/09/2007 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

