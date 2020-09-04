&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: loadtag-menu.w

  Description: Menu for loadtag option

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Sewa Singh

  Created: 08/27/2020

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
&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipcCompany  AS CHARACTER NO-UNDO INITIAL "001".
DEFINE VARIABLE ipcLocation AS CHARACTER NO-UNDO INITIAL "MAIN".
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE continue AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnJob btnPo  ~
btnRelease btnReturn btnRePrint btnSplit btnClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     LABEL "C&lose" 
     SIZE 35 BY 2
     FONT 36.

DEFINE BUTTON btnJob 
     LABEL "Job " 
     SIZE 35 BY 2
     FONT 36.


DEFINE BUTTON btnPo 
     LABEL "PO" 
     SIZE 35 BY 2
     FONT 36.


DEFINE BUTTON btnRelease 
     LABEL "Release" 
     SIZE 35 BY 2
     FONT 36.


DEFINE BUTTON btnRePrint 
     LABEL "Reprint" 
     SIZE 35 BY 2
     FONT 36.

DEFINE BUTTON btnReturn 
     LABEL "Return" 
     SIZE 35 BY 2
     FONT 36.

DEFINE BUTTON btnSplit 
     LABEL "Split" 
     SIZE 35 BY 2
     FONT 36.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnJob AT ROW 1.48 COL 2.4 WIDGET-ID 2
     
     btnPo AT ROW 3.62 COL 2.4 WIDGET-ID 8     
     
     btnRelease AT ROW 5.86 COL 2.4 WIDGET-ID 10
     btnReturn AT ROW 8 COL 2.4 WIDGET-ID 18
     btnRePrint AT ROW 10.14 COL 2.4 WIDGET-ID 20
     btnSplit AT ROW 12.33 COL 2.4 WIDGET-ID 22
     btnClose AT ROW 15.48 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 37 BY 17.33 WIDGET-ID 100.


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
         TITLE              = "Loadtag Menu"
         HEIGHT             = 18.38
         WIDTH              = 37
         MAX-HEIGHT         = 18.38
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 18.38
         VIRTUAL-WIDTH      = 80
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
       btnClose:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btnJob:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN 
       btnPo:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btnRelease:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btnRePrint:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btnReturn:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btnSplit:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Loadtag Menu */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Loadtag Menu */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
   IF INDEX(program-name(4),"asiLogin") <> 0 THEN
       RUN system/userLogOut.p (NO, 0).
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJob C-Win
ON CHOOSE OF btnJob IN FRAME DEFAULT-FRAME /* Job  */
DO:   
   RUN loadtags\LoadtagJob.w(INPUT ipcCompany,     /* Company Code */
                        INPUT ipcLocation,    /* Location Code */
                        INPUT "",  /* Primary Job number */  
                        INPUT "",  /* FG Item */
                        INPUT 0,        /* Second Job number */
                        INPUT 0,        /* Form number of the Job */
                        INPUT 0).       /* Blank number of the Job */  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME btnPo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPo C-Win
ON CHOOSE OF btnPo IN FRAME DEFAULT-FRAME /* PO */
DO:
    /*RUN loadtags\LoadtagJob.w(INPUT ipcCompany,     /* Company Code */
                        INPUT ipcLocation,    /* Location Code */
                        INPUT "",  /* Primary Job number */                         
                        INPUT 0,        /* Second Job number */
                        INPUT 0,        /* Form number of the Job */
                        INPUT 0).       /* Blank number of the Job */ */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRelease C-Win
ON CHOOSE OF btnRelease IN FRAME DEFAULT-FRAME /* Release */
DO:
  /*  RUN loadtags\LoadtagJob.w(INPUT ipcCompany,     /* Company Code */
                        INPUT ipcLocation,    /* Location Code */
                        INPUT "",  /* Primary Job number */                         
                        INPUT 0,        /* Second Job number */
                        INPUT 0,        /* Form number of the Job */
                        INPUT 0).       /* Blank number of the Job */   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME btnRePrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRePrint C-Win
ON CHOOSE OF btnRePrint IN FRAME DEFAULT-FRAME /* Reprint */
DO:
   /* RUN loadtags\LoadtagJob.w(INPUT ipcCompany,     /* Company Code */
                        INPUT ipcLocation,    /* Location Code */
                        INPUT "",  /* Primary Job number */                         
                        INPUT 0,        /* Second Job number */
                        INPUT 0,        /* Form number of the Job */
                        INPUT 0).       /* Blank number of the Job */   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReturn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReturn C-Win
ON CHOOSE OF btnReturn IN FRAME DEFAULT-FRAME /* Return */
DO:   
  /*  RUN loadtags\LoadtagJob.w(INPUT ipcCompany,     /* Company Code */
                        INPUT ipcLocation,    /* Location Code */
                        INPUT "",  /* Primary Job number */                         
                        INPUT 0,        /* Second Job number */
                        INPUT 0,        /* Form number of the Job */
                        INPUT 0).       /* Blank number of the Job */  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSplit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplit C-Win
ON CHOOSE OF btnSplit IN FRAME DEFAULT-FRAME /* Split */
DO:
  /*  RUN loadtags\LoadtagJob.w(INPUT ipcCompany,     /* Company Code */
                        INPUT ipcLocation,    /* Location Code */
                        INPUT "",  /* Primary Job number */                         
                        INPUT 0,        /* Second Job number */
                        INPUT 0,        /* Form number of the Job */
                        INPUT 0).       /* Blank number of the Job */   */
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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
RUN util/CheckModule.p ("ASI","WIP", YES, OUTPUT continue).
&ELSE
continue = YES.
&ENDIF

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF continue THEN
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  IF NOT continue THEN
  APPLY "CLOSE":U TO THIS-PROCEDURE.
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
  ENABLE btnJob btnPo btnRelease btnReturn 
         btnRePrint btnSplit btnClose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

