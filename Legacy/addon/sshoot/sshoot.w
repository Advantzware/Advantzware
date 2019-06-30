&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-11 btn-rm btn-fg btn-bol btn-tag ~
btn-wip btn-close 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-bol 
     LABEL "&Bill of Lading" 
     SIZE 35 BY 2
     FONT 6.

DEFINE BUTTON btn-close 
     LABEL "C&lose" 
     SIZE 35 BY 2
     FONT 6.

DEFINE BUTTON btn-fg 
     LABEL "&Finished Goods" 
     SIZE 35 BY 2
     FONT 6.

DEFINE BUTTON btn-rm 
     LABEL "&Material" 
     SIZE 35 BY 2
     FONT 6.

DEFINE BUTTON btn-tag 
     LABEL "L&abel Menu" 
     SIZE 35 BY 2
     FONT 6.

DEFINE BUTTON btn-wip 
     LABEL "&Work In Process" 
     SIZE 35 BY 2
     FONT 6.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 10.95 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btn-rm AT ROW 1.24 COL 2
     btn-fg AT ROW 3.38 COL 2
     btn-bol AT ROW 5.52 COL 2
     btn-tag AT ROW 7.67 COL 2
     btn-wip AT ROW 9.81 COL 2 WIDGET-ID 2
     btn-close AT ROW 11.95 COL 2
     RECT-11 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 37.2 BY 13.27.


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
         TITLE              = "Sharp Shooter Menu"
         HEIGHT             = 13.27
         WIDTH              = 37.2
         MAX-HEIGHT         = 14.43
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 14.43
         VIRTUAL-WIDTH      = 80
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       btn-bol:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btn-close:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btn-fg:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btn-rm:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btn-tag:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btn-wip:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sharp Shooter Menu */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sharp Shooter Menu */
DO:
    RUN system/userLogOut.p (NO, 0).
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-bol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-bol C-Win
ON CHOOSE OF btn-bol IN FRAME DEFAULT-FRAME /* Bill of Lading */
DO:
   RUN addon/bol/boltrans.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-close C-Win
ON CHOOSE OF btn-close IN FRAME DEFAULT-FRAME /* Close */
DO:
   IF INDEX(program-name(4),"asiLogin") <> 0 THEN
       RUN system/userLogOut.p (NO, 0).
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-fg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-fg C-Win
ON CHOOSE OF btn-fg IN FRAME DEFAULT-FRAME /* Finished Goods */
DO:
   RUN addon/fg/fgtransa.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
         

&Scoped-define SELF-NAME btn-rm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-rm C-Win
ON CHOOSE OF btn-rm IN FRAME DEFAULT-FRAME /* Material */
DO:
   RUN addon/rm/rmtransa.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-tag C-Win
ON CHOOSE OF btn-tag IN FRAME DEFAULT-FRAME /* Label Menu */
DO:
   RUN addon/loadtags/ldtagtrs.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-wip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-wip C-Win
ON CHOOSE OF btn-wip IN FRAME DEFAULT-FRAME /* Work In Process */
DO:
   RUN wip/wip-menu.w(g_company, g_loc).
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  ASSIGN
    {&WINDOW-NAME}:X = 20
    {&WINDOW-NAME}:Y = 20.
  RUN enable_UI.
  {methods/nowait.i}
  APPLY "entry" TO btn-rm.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
       WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  ENABLE RECT-11 btn-rm btn-fg btn-bol btn-tag btn-wip btn-close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

