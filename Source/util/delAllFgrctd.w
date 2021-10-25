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
{methods/prgsecur.i}

{custom/gcompany.i}    
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.
DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTransList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTransDesc AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fiTransType fiFromDate btnCalendar-1 ~
fiToDate btnCalendar-2 btnCalendar-3 dtTransType fiReceiptDate btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiTransType fiFromDate fiToDate fiReceiptDate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 

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

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".
     
DEFINE BUTTON btnCalendar-3 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".     

DEFINE VARIABLE fiFromDate AS DATE FORMAT "99/99/9999":U 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiToDate AS DATE FORMAT "99/99/9999":U 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiTransType AS CHARACTER FORMAT "x(1)":U 
     LABEL "Transaction Type" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.
 
DEFINE VARIABLE dtTransType AS CHARACTER FORMAT "x(10)":U  INITIAL "Update"
     LABEL "Mode"     
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "Update","Delete" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FGCOLOR 0 FONT 22 NO-UNDO.
  
DEFINE VARIABLE fiReceiptDate AS DATE FORMAT "99/99/9999":U 
     LABEL "New Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     dtTransType AT ROW 6.39 COL 23 COLON-ALIGNED 
     fiReceiptDate AT ROW 6.39 COL 56 COLON-ALIGNED 
     btnCalendar-3 AT ROW 6.39 COL 75
     fiTransType AT ROW 8.19 COL 23 COLON-ALIGNED WIDGET-ID 2
     fiFromDate AT ROW 9.86 COL 23 COLON-ALIGNED WIDGET-ID 4
     btnCalendar-1 AT ROW 9.86 COL 43.4 WIDGET-ID 8
     fiToDate AT ROW 9.86 COL 56 COLON-ALIGNED WIDGET-ID 6
     btnCalendar-2 AT ROW 9.86 COL 75 WIDGET-ID 10         
     btn-process AT ROW 14.1 COL 21
     btn-cancel AT ROW 14.1 COL 52
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "If blank - All Transactions" VIEW-AS TEXT
          SIZE 32 BY .62 AT ROW 8.39 COL 33     
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 15.38.

DEFINE FRAME FRAME-B
     "This process may take up to an hour.  Please let the process complete!" VIEW-AS TEXT
          SIZE 81 BY .95 AT ROW 2.91 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You Should perform a database backup before running this procedure!" VIEW-AS TEXT
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
         TITLE              = "Update/Delete FG Transactions"
         HEIGHT             = 15.57
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

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME FRAME-A
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME FRAME-A
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME FRAME-A
   3                                                                    */   
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Clear unposted FG Transactions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Clear unposted FG Transactions */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  v-process  = NO.
  
  DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
  fiFromDate fiToDate fiTransType dtTransType fiReceiptDate.
  END.
  
  IF fiReceiptDate EQ ? AND dtTransType:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "Update" THEN DO:
    MESSAGE "New date must not be blank.  Please re-enter." VIEW-AS ALERT-BOX.
    APPLY "Entry" TO fiReceiptDate.
    RETURN.
  END.
  
  IF fiTransType NE "" AND LOOKUP(fiTransType, cTransList) EQ 0 THEN DO:
    MESSAGE "Invalid Transaction Type.  Please re-enter." VIEW-AS ALERT-BOX.
    APPLY "Entry" TO fiTransType.
    RETURN.
  END.
  
  IF fiFromDate EQ ? THEN DO:
    MESSAGE "From date must not be blank.  Please re-enter." VIEW-AS ALERT-BOX.
    APPLY "Entry" TO fiFromDate.
    RETURN.
  END.
  
  IF fiToDate EQ ? THEN DO:
    MESSAGE "To date must not be blank.  Please re-enter." VIEW-AS ALERT-BOX.
    APPLY "Entry" TO fiToDate.
    RETURN.
  END.
    
  IF fiTransType :SCREEN-VALUE EQ "" THEN
  MESSAGE "All Unposted Transactions" 
          VIEW-AS ALERT-BOX INFO.

  MESSAGE "Are you sure you want to" (IF dtTransType:SCREEN-VALUE EQ "Update" THEN "update New Date "
           ELSE "delete FG Transactions")
          "within the selected parameters?"       
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME FRAME-A
DO:
  {methods/btnCalendar.i fiFromDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 C-Win
ON CHOOSE OF btnCalendar-2 IN FRAME FRAME-A
DO:
  {methods/btnCalendar.i fiToDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 C-Win
ON CHOOSE OF btnCalendar-3 IN FRAME FRAME-A
DO:
  {methods/btnCalendar.i fiReceiptDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFromDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFromDate C-Win
ON HELP OF fiFromDate IN FRAME FRAME-A /* From Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiToDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiToDate C-Win
ON HELP OF fiToDate IN FRAME FRAME-A /* To Date */
DO:

  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fiReceiptDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiReceiptDate C-Win
ON HELP OF fiReceiptDate IN FRAME FRAME-A /* Trans Date */
DO:

  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTransType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTransType C-Win
ON HELP OF fiTransType IN FRAME FRAME-A /* Transaction Type */
DO:
  RUN windows/l-tranCd.w (fiTransType:SCREEN-VALUE, OUTPUT char-val).
  fiTransType:SCREEN-VALUE = char-val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME dtTransType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dtTransType C-Win
ON VALUE-CHANGED OF dtTransType IN FRAME FRAME-A /* Transaction Mode */
DO:
   IF dtTransType:SCREEN-VALUE EQ "Update" THEN
   DO:
       fiReceiptDate:HIDDEN = NO.
       btnCalendar-3:HIDDEN = NO.
   END.
   ELSE ASSIGN
            fiReceiptDate:HIDDEN   = YES
            btnCalendar-3:HIDDEN = YES.  
  
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

  IF access-close THEN DO:
    APPLY "close" TO THIS-PROCEDURE.
    RETURN .
  END.
  RUN sys/ref/transcodes.p (OUTPUT cTransList, OUTPUT cTransDesc).
  RUN enable_UI.

  {methods/nowait.i}
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
  DISPLAY fiTransType fiFromDate fiToDate dtTransType fiReceiptDate
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fiTransType fiFromDate btnCalendar-1 fiToDate btnCalendar-2 
         btnCalendar-3 dtTransType fiReceiptDate btn-process btn-cancel 
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
DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
SESSION:SET-WAIT-STATE("General").

FOR EACH bf-fg-rctd
    WHERE bf-fg-rctd.company EQ cocode
      AND (bf-fg-rctd.rita-code EQ fiTransType OR fiTransType EQ "")
      AND bf-fg-rctd.rct-date GE fiFromDate
      AND bf-fg-rctd.rct-date LE fiToDate:
    IF dtTransType:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "Delete" THEN
    DO:
       DELETE bf-fg-rctd.
    END.         
    ELSE
    ASSIGN bf-fg-rctd.rct-date = fiReceiptDate.
END.
         
SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

