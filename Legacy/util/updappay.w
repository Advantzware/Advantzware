&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\updappay.w

------------------------------------------------------------------------*/
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

{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_vend-no fi_check-no btn-select RECT-17 
&Scoped-Define DISPLAYED-OBJECTS fi_vend-no fi_check-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-process 
     LABEL "&Update" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-select 
     LABEL "&Select" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fi_check-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Check Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_check-no AS INTEGER FORMAT "99999999":U INITIAL 0 
     LABEL "Check #" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_vend-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_vend-no AT ROW 3.14 COL 19 COLON-ALIGNED
     fi_check-no AT ROW 3.05 COL 52.6 COLON-ALIGNED
     btn-select AT ROW 2.91 COL 69
     fi_check-date AT ROW 4.52 COL 33.8 COLON-ALIGNED
     btn-process AT ROW 7.1 COL 32
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.86 COL 5
     RECT-17 AT ROW 1.38 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 11.67.


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
         TITLE              = "Update Posted Check"
         HEIGHT             = 11.71
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 90.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 90.2
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-select:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR BUTTON btn-process IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_check-date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_check-date:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Posted Check */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Posted Check */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Update */
DO:
  DEF VAR v-process AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN fi_check-date.

     FIND FIRST period WHERE
          period.company EQ cocode AND
          period.pst     LE fi_check-date AND
          period.pend    GE fi_check-date
          NO-LOCK NO-ERROR.

     IF AVAIL period THEN DO:
        IF NOT period.pstat THEN DO:
           MESSAGE "Period Already Closed..." VIEW-AS ALERT-BOX ERROR.
           LEAVE.
        END.
     END.

     ELSE DO:
        MESSAGE "No Defined Period Exists for" fi_check-date
           VIEW-AS ALERT-BOX ERROR.
        LEAVE.
     END.

     MESSAGE "Are you sure you want to update check date?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

     IF v-process THEN
     DO:
        FIND FIRST ap-pay WHERE
             ap-pay.company = cocode AND
             ap-pay.vend-no = fi_vend-no AND
             ap-pay.check-no = fi_check-no
             EXCLUSIVE-LOCK NO-ERROR.

         IF AVAIL ap-pay THEN
         DO:
            ASSIGN
               ap-pay.check-date = fi_check-date
               fi_check-date:HIDDEN = YES
               fi_check-date:SENSITIVE = NO
               btn-process:SENSITIVE = NO
               btn-select:SENSITIVE = YES
               fi_vend-no:SENSITIVE = YES
               fi_check-no:SENSITIVE = YES.

            RELEASE ap-pay.

            MESSAGE "Posted Check Was Updated."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
         END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-select C-Win
ON CHOOSE OF btn-select IN FRAME FRAME-A /* Select */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN fi_vend-no fi_check-no.

      FIND FIRST ap-pay WHERE
           ap-pay.company = cocode AND
           ap-pay.vend-no = fi_vend-no AND
           ap-pay.check-no = fi_check-no
           NO-LOCK NO-ERROR.

      IF AVAIL ap-pay THEN
      DO:
         ASSIGN
            fi_check-date:HIDDEN = NO
            fi_check-date:SENSITIVE = YES
            btn-process:SENSITIVE = YES
            btn-select:SENSITIVE = NO
            fi_vend-no:SENSITIVE = NO
            fi_check-no:SENSITIVE = NO
            fi_check-date = ap-pay.check-date.

         DISPLAY fi_check-date WITH FRAME {&FRAME-NAME}.

         RELEASE ap-pay.
      END.
      ELSE
      DO:
         MESSAGE "Invalid Check# for the Vendor Entered."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY":U TO fi_check-no IN FRAME {&FRAME-NAME}.
      END.
   END.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry":U TO fi_vend-no.
  END.

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
  DISPLAY fi_vend-no fi_check-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE fi_vend-no fi_check-no btn-select RECT-17 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

