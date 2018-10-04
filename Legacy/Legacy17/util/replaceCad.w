&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fi_from_cad fi_to_cad fi_from_fil ~
fi_to_fil btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_from_cad fi_to_cad fi_from_fil ~
fi_to_fil 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fi_from_cad AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Cad#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fi_from_fil AS CHARACTER FORMAT "X(256)":U 
     LABEL "From File#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fi_to_cad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Change to Cad#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fi_to_fil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Change to File#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_from_cad AT ROW 6.24 COL 17 COLON-ALIGNED WIDGET-ID 2
     fi_to_cad AT ROW 6.24 COL 56 COLON-ALIGNED WIDGET-ID 4
     fi_from_fil AT ROW 7.67 COL 17 COLON-ALIGNED WIDGET-ID 6
     fi_to_fil AT ROW 7.67 COL 56 COLON-ALIGNED WIDGET-ID 8
     btn-process AT ROW 10.52 COL 21
     btn-cancel AT ROW 10.52 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 11.67.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 79 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 3
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Replace Cad# and File#"
         HEIGHT             = 11.86
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Replace Cad# and File# */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Replace Cad# and File# */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  v-process  = NO.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) SKIP 
          " for the selected parameters?" SKIP
          " From Cad# begins with " fi_from_cad " replace with " (IF fi_to_cad <> "" THEN fi_to_cad ELSE "<SPACE>") SKIP
          " From File# begins with " fi_from_fil " replace with " (IF fi_to_fil <> "" THEN fi_to_fil ELSE "<SPACE>") SKIP
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_from_cad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_from_cad C-Win
ON LEAVE OF fi_from_cad IN FRAME FRAME-A /* From Cad# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_from_fil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_from_fil C-Win
ON LEAVE OF fi_from_fil IN FRAME FRAME-A /* From File# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_to_cad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_to_cad C-Win
ON LEAVE OF fi_to_cad IN FRAME FRAME-A /* Change to Cad# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_to_fil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_to_fil C-Win
ON LEAVE OF fi_to_fil IN FRAME FRAME-A /* Change to File# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
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

  /*IF access-close THEN DO:
    APPLY "close" TO THIS-PROCEDURE.
    RETURN .
  END.
  */

  RUN enable_UI.

  {methods/nowait.i}
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-process "Start"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY fi_from_cad fi_to_cad fi_from_fil fi_to_fil 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fi_from_cad fi_to_cad fi_from_fil fi_to_fil btn-process 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
def var i AS INTE NO-UNDO.
def var j AS INTE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    fi_from_cad
    fi_to_cad
    fi_from_fil
    fi_to_fil.
END.

SESSION:SET-WAIT-STATE("General").

status default "Processing ...".

ASSIGN i = 0 j = 0.
IF fi_from_cad <> "" THEN
FOR EACH reftable WHERE reftable.reftable EQ "PREPCADFILE"
                    AND reftable.CODE BEGINS fi_from_cad:
    FIND FIRST prep NO-LOCK WHERE prep.company = cocode
                              AND prep.rec_key = reftable.rec_key
        USE-INDEX rec_key NO-ERROR.
    IF AVAIL prep THEN 
        ASSIGN i = i + 1 
               reftable.CODE = fi_to_cad.
END.

IF fi_from_fil <> "" THEN
FOR EACH reftable WHERE reftable.reftable EQ "PREPCADFILE"
                    AND reftable.CODE2 BEGINS fi_from_fil:
    FIND FIRST prep NO-LOCK WHERE prep.company = cocode
                              AND prep.rec_key = reftable.rec_key
        USE-INDEX rec_key NO-ERROR.
    IF AVAIL prep THEN 
        ASSIGN j = j + 1
               reftable.CODE2 = fi_to_fil.
END.


STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

IF i > 0 OR j > 0 THEN
MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." SKIP 
       "Cad# replaced count  ->" i SKIP 
       "File# replaced count ->" j VIEW-AS ALERT-BOX.
ELSE
MESSAGE TRIM(c-win:TITLE) + " No records were processed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

