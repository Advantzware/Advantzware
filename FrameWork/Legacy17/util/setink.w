&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES item

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define QUERY-STRING-FRAME-A FOR EACH item SHARE-LOCK
&Scoped-define OPEN-QUERY-FRAME-A OPEN QUERY FRAME-A FOR EACH item SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-A item
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-A item


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_rm-i-no end_rm-i-no tb_fold tb_corr ~
tb_ink1 tb_var1 tb_ink2 tb_lacq tb_ultr tb_var2 tb_aque tb_offs tb_flex ~
tb_grav tb_lett tb_silk fi_yield fi_min-lbs btn-process btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-i-no end_rm-i-no tb_fold tb_corr ~
tb_ink1 tb_var1 tb_ink2 tb_lacq tb_ultr tb_var2 tb_aque tb_offs tb_flex ~
tb_grav tb_lett tb_silk fi_yield fi_min-lbs 

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

DEFINE VARIABLE begin_rm-i-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-i-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_min-lbs AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "Min Lbs" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE fi_yield AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "SqInches/LB" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 11.43.

DEFINE VARIABLE tb_aque AS LOGICAL INITIAL no 
     LABEL "Aqueous" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_corr AS LOGICAL INITIAL no 
     LABEL "Corrugated" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_flex AS LOGICAL INITIAL no 
     LABEL "Flexo" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fold AS LOGICAL INITIAL no 
     LABEL "Folding Carton" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_grav AS LOGICAL INITIAL no 
     LABEL "Gravure" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ink1 AS LOGICAL INITIAL no 
     LABEL "Ink" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ink2 AS LOGICAL INITIAL no 
     LABEL "Ink" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_lacq AS LOGICAL INITIAL no 
     LABEL "Lacquer" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_lett AS LOGICAL INITIAL no 
     LABEL "Letterpress" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_offs AS LOGICAL INITIAL no 
     LABEL "Offset" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_silk AS LOGICAL INITIAL no 
     LABEL "Silkscreen" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ultr AS LOGICAL INITIAL no 
     LABEL "Ultra Violet" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_var1 AS LOGICAL INITIAL no 
     LABEL "Varnish" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_var2 AS LOGICAL INITIAL no 
     LABEL "Varnish" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FRAME-A FOR 
      item SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rm-i-no AT ROW 6.48 COL 24 COLON-ALIGNED HELP
          "Enter Beginning RM Item Number"
     end_rm-i-no AT ROW 6.48 COL 66 COLON-ALIGNED HELP
          "Enter Ending RM ItemNumber"
     tb_fold AT ROW 9.33 COL 5
     tb_corr AT ROW 10.29 COL 5
     tb_ink1 AT ROW 9.33 COL 29
     tb_var1 AT ROW 10.29 COL 29
     tb_ink2 AT ROW 9.33 COL 52
     tb_lacq AT ROW 10.29 COL 52
     tb_ultr AT ROW 11.24 COL 52
     tb_var2 AT ROW 12.19 COL 52
     tb_aque AT ROW 13.14 COL 52
     tb_offs AT ROW 9.33 COL 71
     tb_flex AT ROW 10.29 COL 71
     tb_grav AT ROW 11.24 COL 71
     tb_lett AT ROW 12.19 COL 71
     tb_silk AT ROW 13.14 COL 71
     fi_yield AT ROW 13.14 COL 19 COLON-ALIGNED HELP
          "Enter Yield Percentage in LB/MSI for Ink Mat'l Consumption"
     fi_min-lbs AT ROW 14.33 COL 19 COLON-ALIGNED HELP
          "Enter minimum Pounds to charge for this Ink or Coating Mat'l."
     btn-process AT ROW 17.19 COL 22
     btn-cancel AT ROW 17.19 COL 54
     RECT-17 AT ROW 4.81 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "Industry Type" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 8.14 COL 5
     "Ink Type" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 8.14 COL 52
     "Material Type" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 8.14 COL 29
     "Press Type" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 8.14 COL 71
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 18.24.

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
         TITLE              = "Set SqFt/LB & MinLB for Ink RMs"
         HEIGHT             = 18.24
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
   Custom                                                               */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".



DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-B:MOVE-BEFORE-TAB-ITEM (begin_rm-i-no:HANDLE IN FRAME FRAME-A)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _TblList          = "asi.item"
     _Query            is OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Set SqFt/LB  MinLB for Ink RMs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Set SqFt/LB  MinLB for Ink RMs */
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
  DEF VAR v-process AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
          " for the selected parameters?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_aque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_aque C-Win
ON VALUE-CHANGED OF tb_aque IN FRAME FRAME-A /* Aqueous */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_corr C-Win
ON VALUE-CHANGED OF tb_corr IN FRAME FRAME-A /* Corrugated */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_flex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_flex C-Win
ON VALUE-CHANGED OF tb_flex IN FRAME FRAME-A /* Flexo */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fold C-Win
ON VALUE-CHANGED OF tb_fold IN FRAME FRAME-A /* Folding Carton */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_grav
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_grav C-Win
ON VALUE-CHANGED OF tb_grav IN FRAME FRAME-A /* Gravure */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ink1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ink1 C-Win
ON VALUE-CHANGED OF tb_ink1 IN FRAME FRAME-A /* Ink */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ink2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ink2 C-Win
ON VALUE-CHANGED OF tb_ink2 IN FRAME FRAME-A /* Ink */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_lacq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_lacq C-Win
ON VALUE-CHANGED OF tb_lacq IN FRAME FRAME-A /* Lacquer */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_lett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_lett C-Win
ON VALUE-CHANGED OF tb_lett IN FRAME FRAME-A /* Letterpress */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_offs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_offs C-Win
ON VALUE-CHANGED OF tb_offs IN FRAME FRAME-A /* Offset */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_silk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_silk C-Win
ON VALUE-CHANGED OF tb_silk IN FRAME FRAME-A /* Silkscreen */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ultr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ultr C-Win
ON VALUE-CHANGED OF tb_ultr IN FRAME FRAME-A /* Ultra Violet */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_var1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_var1 C-Win
ON VALUE-CHANGED OF tb_var1 IN FRAME FRAME-A /* Varnish */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_var2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_var2 C-Win
ON VALUE-CHANGED OF tb_var2 IN FRAME FRAME-A /* Varnish */
DO:
  assign {&self-name}.
/*
  IF {&self-name} THEN spec_codes:HIDDEN = NO.
  ELSE
  IF lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
*/  
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

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO begin_rm-i-no.
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

  {&OPEN-QUERY-FRAME-A}
  GET FIRST FRAME-A.
  DISPLAY begin_rm-i-no end_rm-i-no tb_fold tb_corr tb_ink1 tb_var1 tb_ink2 
          tb_lacq tb_ultr tb_var2 tb_aque tb_offs tb_flex tb_grav tb_lett 
          tb_silk fi_yield fi_min-lbs 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_rm-i-no end_rm-i-no tb_fold tb_corr tb_ink1 tb_var1 tb_ink2 
         tb_lacq tb_ultr tb_var2 tb_aque tb_offs tb_flex tb_grav tb_lett 
         tb_silk fi_yield fi_min-lbs btn-process btn-cancel RECT-17 
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
SESSION:SET-WAIT-STATE("General").

FOR EACH item
    WHERE item.company      EQ cocode
      AND item.i-no         GE begin_rm-i-no
      AND item.i-no         LE end_rm-i-no
      AND ((item.industry   EQ "1" AND tb_fold) OR
           (item.industry   EQ "2" AND tb_corr))
      AND ((item.mat-type   EQ "I" AND tb_ink1) OR
           (item.mat-type   EQ "V" AND tb_var1))
      AND ((item.press-type EQ "F" AND tb_flex) OR
           (item.press-type EQ "G" AND tb_grav) OR
           (item.press-type EQ "L" AND tb_lett) OR
           (item.press-type EQ "O" AND tb_offs) OR
           (item.press-type EQ "S" AND tb_silk))
      AND ((item.ink-type   EQ "I" AND tb_ink2) OR
           (item.ink-type   EQ "L" AND tb_lacq) OR
           (item.ink-type   EQ "U" AND tb_ultr) OR
           (item.ink-type   EQ "V" AND tb_var2) OR
           (item.ink-type   EQ "A" AND tb_aque))
    BY item.i-no
    TRANSACTION
    WITH DOWN:
  DISPLAY TRIM(item.i-no) FORMAT "x(20)" LABEL "RM Item#".
  ASSIGN
   item.yield   = fi_yield
   item.min-lbs = fi_min-lbs.
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

