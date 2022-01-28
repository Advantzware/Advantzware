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

{custom/globdefs.i}

DEF BUFFER b-period FOR period.

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF TEMP-TABLE tt-journal NO-UNDO
         FIELD tt-journal LIKE gl-jrn.journal
                          INDEX tt-journal tt-journal.

DEF VAR v-process AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_run-no new_date fi_fr-year ~
fi_fr-period fi_to-year fi_to-period tb_gl-tot tb_cust-per ~
tb_vend-per tb_verify-gl-tot btn-process btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS begin_run-no old_date new_date fi_fr-year ~
fi_fr-period fi_to-year fi_to-period tb_gl-tot tb_cust-per tb_vend-per ~
tb_verify-gl-tot fi_file 

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

DEFINE VARIABLE begin_run-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Run#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_fr-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fi_fr-year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Year" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_to-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fi_to-year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Year" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE new_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "New Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE old_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Old Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 11.43.

DEFINE VARIABLE tb_cust-per AS LOGICAL INITIAL yes 
     LABEL "Recalculate Customer Period Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE tb_gl-tot AS LOGICAL INITIAL yes 
     LABEL "Recalculate G/L Period Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE tb_vend-per AS LOGICAL INITIAL yes 
     LABEL "Recalculate Vendor Period Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE tb_verify-gl-tot AS LOGICAL INITIAL yes 
     LABEL "Verify G/L Period Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\acct_variance_list.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 BGCOLOR 15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_run-no AT ROW 6.67 COL 18 COLON-ALIGNED HELP
          "Enter Beginning Run Number"
     old_date AT ROW 7.86 COL 18 COLON-ALIGNED
     new_date AT ROW 9.05 COL 18 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     fi_fr-year AT ROW 6.43 COL 50 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     fi_fr-period AT ROW 6.43 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     fi_to-year AT ROW 9.52 COL 50 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     fi_to-period AT ROW 9.52 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     tb_gl-tot AT ROW 10.81 COL 30.2 WIDGET-ID 2
     tb_cust-per AT ROW 11.95 COL 30.2 WIDGET-ID 4
     tb_vend-per AT ROW 13.1 COL 30.2 WIDGET-ID 6
     tb_verify-gl-tot AT ROW 14.20 COL 30.2 WIDGET-ID 8
     fi_file AT ROW 15.05 COL 28.2 COLON-ALIGNED
     btn-process AT ROW 19.81 COL 21
     btn-cancel AT ROW 19.81 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Change" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 5.48 COL 60
          FONT 6
     "To" VIEW-AS TEXT
          SIZE 5 BY .71 AT ROW 8.62 COL 62
          FONT 6
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 20.52.

DEFINE FRAME FRAME-H
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.29
         SIZE 89 BY 2.86
         BGCOLOR 12 FGCOLOR 14 .

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
         TITLE              = "G/L Admin Tool"
         HEIGHT             = 20.57
         WIDTH              = 90.2
         MAX-HEIGHT         = 20.57
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 20.57
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
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE
       FRAME FRAME-H:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-B:MOVE-BEFORE-TAB-ITEM (begin_run-no:HANDLE IN FRAME FRAME-A)
       XXTABVALXX = FRAME FRAME-H:MOVE-AFTER-TAB-ITEM (fi_to-period:HANDLE IN FRAME FRAME-A)
       XXTABVALXX = FRAME FRAME-H:MOVE-BEFORE-TAB-ITEM (btn-process:HANDLE IN FRAME FRAME-A)
/* END-ASSIGN-TABS */.

ASSIGN 
       begin_run-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       new_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN old_date IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       old_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-per:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_gl-tot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_vend-per:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_verify-gl-tot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* SETTINGS FOR FRAME FRAME-H
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* G/L Admin Tool */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* G/L Admin Tool */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_run-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_run-no C-Win
ON HELP OF begin_run-no IN FRAME FRAME-A /* Run# */
DO:
  DEF VAR lv AS CHAR NO-UNDO.
  lv = {&self-name}:SCREEN-VALUE.
  RUN run-no-help (INPUT-OUTPUT lv).
  IF INT({&self-name}:SCREEN-VALUE) NE INT(lv) THEN DO:
    {&self-name}:SCREEN-VALUE = lv.
    APPLY "value-changed" TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_run-no C-Win
ON VALUE-CHANGED OF begin_run-no IN FRAME FRAME-A /* Run# */
DO:
  DEF VAR lv-date AS DATE INIT ? NO-UNDO.


  IF INT({&self-name}:SCREEN-VALUE) EQ 0 THEN
    ASSIGN
     fi_fr-year:SENSITIVE   = YES
     fi_fr-period:SENSITIVE = YES
     fi_to-year:SENSITIVE   = YES
     fi_to-period:SENSITIVE = YES.

  ELSE DO:
    
    FIND FIRST glhist NO-LOCK
      WHERE glhist.company EQ cocode
        AND glhist.tr-num  EQ INT({&self-name}:SCREEN-VALUE)
      NO-ERROR.
    IF AVAIL glhist THEN lv-date = glhist.tr-date.
   

    IF lv-date NE ? THEN DO:
      FIND FIRST b-period NO-LOCK
          WHERE b-period.company EQ cocode
            AND b-period.pst     LE lv-date
            AND b-period.pend    GE lv-date
          NO-ERROR.

      IF AVAIL b-period THEN
        ASSIGN
         fi_fr-year:SCREEN-VALUE   = STRING(b-period.yr)
         fi_fr-period:SCREEN-VALUE = STRING(b-period.pnum).
      ELSE
        ASSIGN
         fi_fr-year:SCREEN-VALUE   = STRING(YEAR(lv-date))
         fi_fr-period:SCREEN-VALUE = "1".

      ASSIGN
       old_date:SCREEN-VALUE     = STRING(lv-date)
       new_date:SCREEN-VALUE     = STRING(lv-date)
       fi_to-year:SCREEN-VALUE   = fi_fr-year:SCREEN-VALUE
       fi_to-period:SCREEN-VALUE = fi_fr-period:SCREEN-VALUE
       fi_fr-year:SENSITIVE      = NO
       fi_fr-period:SENSITIVE    = NO
       fi_to-year:SENSITIVE      = NO
       fi_to-period:SENSITIVE    = NO.
    END.
  END.
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
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  ll = NO.

  RELEASE b-period.

  FIND FIRST b-period NO-LOCK
        WHERE b-period.company EQ cocode
          AND b-period.pst     LE new_date
          AND b-period.pend    GE new_date
        NO-ERROR.

  IF AVAIL b-period AND b-period.pstat EQ NO THEN
  DO:
     MESSAGE "New Date Has To Be In An Open Period." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO new_date.
     RETURN NO-APPLY.
  END.

  RELEASE b-period.

  IF begin_run-no EQ 0 THEN DO:
    FIND FIRST b-period
        WHERE b-period.company EQ cocode
          AND b-period.yr      EQ fi_to-year
          AND b-period.pnum    EQ fi_to-period
        NO-LOCK NO-ERROR.
    IF AVAIL b-period THEN
    MESSAGE "This period already exists, do you want to merge periods?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.

    IF fi_to-period GT 13            OR
       (AVAIL b-period AND ll EQ NO) THEN DO:
      IF NOT AVAIL b-period THEN
        MESSAGE "To Period Invalid..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_to-period.
      RETURN NO-APPLY.
    END.
  END.

  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST b-period NO-LOCK
        WHERE b-period.company EQ cocode
          AND b-period.pst     LE new_date
          AND b-period.pend    GE new_date
        NO-ERROR.
    IF AVAIL b-period THEN
      ASSIGN
       fi_to-year:SCREEN-VALUE   = STRING(b-period.yr)
       fi_to-period:SCREEN-VALUE = STRING(b-period.pnum)
       fi_to-year
       fi_to-period.

    ELSE DO:
      MESSAGE "No Period Exists for New Date..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO new_date.
      RETURN NO-APPLY.
    END.
  END.

  v-process  = NO.

  MESSAGE "Are you sure you want to" TRIM(c-win:TITLE)
          "within the selected parameters?"       
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_fr-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_fr-period C-Win
ON LEAVE OF fi_fr-period IN FRAME FRAME-A /* Period */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_fr-year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_fr-year C-Win
ON LEAVE OF fi_fr-year IN FRAME FRAME-A /* Year */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_to-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_to-period C-Win
ON LEAVE OF fi_to-period IN FRAME FRAME-A /* Period */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_to-year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_to-year C-Win
ON LEAVE OF fi_to-year IN FRAME FRAME-A /* Year */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME new_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL new_date C-Win
ON LEAVE OF new_date IN FRAME FRAME-A /* New Date */
DO:
  assign {&self-name}
         begin_run-no.

  IF begin_run-no NE 0 THEN
  DO:
     FIND FIRST b-period WHERE
          b-period.company EQ cocode AND
          b-period.pst     LE new_date AND
          b-period.pend    GE new_date
          NO-LOCK NO-ERROR.

    IF AVAIL b-period THEN
       ASSIGN
          fi_to-year:SCREEN-VALUE   = STRING(b-period.yr)
          fi_to-period:SCREEN-VALUE = STRING(b-period.pnum).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME old_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL old_date C-Win
ON LEAVE OF old_date IN FRAME FRAME-A /* Old Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-per
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-per C-Win
ON VALUE-CHANGED OF tb_cust-per IN FRAME FRAME-A /* Recalculate Customer Period Totals? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_gl-tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_gl-tot C-Win
ON VALUE-CHANGED OF tb_gl-tot IN FRAME FRAME-A /* Recalculate G/L Period Totals? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_vend-per
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_vend-per C-Win
ON VALUE-CHANGED OF tb_vend-per IN FRAME FRAME-A /* Recalculate Vendor Period Totals? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_verify-gl-tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_verify-gl-tot C-Win
ON VALUE-CHANGED OF tb_verify-gl-tot IN FRAME FRAME-A /* Verify G/L Period Totals? */
DO:
  ASSIGN {&self-name}.
  DO WITH FRAME {&FRAME-NAME}:
    IF tb_verify-gl-tot THEN
        ASSIGN fi_file:HIDDEN = FALSE.
    ELSE
        ASSIGN fi_file:HIDDEN = TRUE.
    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
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

  ASSIGN
   fi_fr-year   = YEAR(TODAY)
   fi_fr-period = 1
   fi_to-year   = fi_fr-year
   fi_to-period = fi_fr-period.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-ap-ledger C-Win 
PROCEDURE do-ap-ledger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

    ap-ledger.period = fi_to-period.

    IF begin_run-no EQ ap-ledger.trnum THEN ap-ledger.tr-date = new_date.
    ELSE
    IF AVAIL b-period THEN
      IF ap-ledger.tr-date LT b-period.pst THEN ap-ledger.tr-date = b-period.pst.
      ELSE
      IF ap-ledger.tr-date GT b-period.pend THEN ap-ledger.tr-date = b-period.pend.
  
    IF ap-ledger.refnum BEGINS "INV# " THEN
    FOR EACH ap-inv
        WHERE ap-inv.company EQ ap-ledger.company
          AND ap-inv.vend-no EQ ap-ledger.vend-no
          AND ap-inv.inv-no  EQ SUBSTR(ap-ledger.refnum,6,20):

      ap-inv.period = fi_to-period.
    END.

    ELSE
    IF ap-ledger.refnum BEGINS "MEMO#" THEN
    FOR EACH ap-payl
        WHERE ap-payl.inv-no EQ SUBSTR(ap-ledger.refnum,6,20)
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company    EQ cocode
          AND ap-pay.c-no       EQ ap-payl.c-no
          AND ap-pay.vend-no    EQ ap-ledger.vend-no
          AND ap-pay.check-date EQ ap-ledger.ref-date
          AND ap-pay.posted     EQ YES
          AND ap-pay.memo       EQ YES:
      ASSIGN
          ap-pay.period           = fi_to-period
          ap-pay.transactionDate  = ap-ledger.tr-date
          .
    END.

    ELSE
    IF ap-ledger.refnum BEGINS "AC" THEN DO:
      li = INT(SUBSTR(ap-ledger.refnum,3,8)) NO-ERROR.

      IF NOT ERROR-STATUS:ERROR THEN
      FOR EACH bank
          WHERE bank.company EQ ap-ledger.company
          NO-LOCK,

          FIRST ap-pay
          WHERE ap-pay.company   EQ ap-ledger.company
            AND ap-pay.check-act EQ bank.actnum
            AND ap-pay.check-no  EQ li:
            
        IF ap-pay.vend-no NE ap-ledger.vend-no
        OR ap-pay.bank-code NE bank.bank-code
        OR NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN NEXT.

        ASSIGN
          ap-pay.period           = fi_to-period
          ap-pay.transactionDate  = ap-ledger.tr-date
          .
      END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-ar-ledger C-Win 
PROCEDURE do-ar-ledger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  IF ar-ledger.ref-num BEGINS "INV#" THEN
  DO:
     li = INT(SUBSTR(ar-ledger.ref-num,6,20)) NO-ERROR.

     IF NOT ERROR-STATUS:ERROR THEN DO:

       FIND FIRST ar-inv
           WHERE ar-inv.company EQ ar-ledger.company
             AND ar-inv.inv-no  EQ li
           NO-ERROR.
       IF AVAIL ar-inv THEN ar-inv.period = fi_to-period.
     END.
  END.

  IF begin_run-no EQ ar-ledger.tr-num THEN ar-ledger.tr-date = new_date.
  ELSE
  IF AVAIL b-period THEN
    IF ar-ledger.tr-date LT b-period.pst THEN ar-ledger.tr-date = b-period.pst.
    ELSE
    IF ar-ledger.tr-date GT b-period.pend THEN ar-ledger.tr-date = b-period.pend.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-gl-jrn C-Win 
PROCEDURE do-gl-jrn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    gl-jrn.period = fi_to-period.

    IF begin_run-no NE 0 THEN gl-jrn.tr-date = new_date.
    ELSE
    IF AVAIL b-period THEN
      IF gl-jrn.tr-date LT b-period.pst THEN gl-jrn.tr-date = b-period.pst.
      ELSE
      IF gl-jrn.tr-date GT b-period.pend THEN gl-jrn.tr-date = b-period.pend.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-glhist C-Win 
PROCEDURE do-glhist :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

      glhist.period = fi_to-period.

      IF begin_run-no EQ glhist.tr-num THEN glhist.tr-date = new_date.
      ELSE
      IF AVAIL b-period THEN
        IF glhist.tr-date LT b-period.pst THEN glhist.tr-date = b-period.pst.
        ELSE
        IF glhist.tr-date GT b-period.pend THEN glhist.tr-date = b-period.pend.

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
  DISPLAY begin_run-no old_date new_date fi_fr-year fi_fr-period fi_to-year 
          fi_to-period tb_gl-tot tb_cust-per tb_vend-per tb_verify-gl-tot
          fi_file
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_run-no new_date fi_fr-year fi_fr-period fi_to-year fi_to-period 
         tb_gl-tot tb_cust-per tb_vend-per tb_verify-gl-tot btn-process 
         btn-cancel RECT-17 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW FRAME FRAME-H IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-H}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-no-help C-Win 
PROCEDURE run-no-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-focus-val AS CHAR NO-UNDO.

  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR look-recid AS RECID NO-UNDO. 


  RUN windows/l-runno.w (g_company, io-focus-val, OUTPUT char-val).
  IF char-val NE "" AND ENTRY(1,char-val) NE io-focus-val THEN
    io-focus-val = ENTRY(1,char-val).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF VAR lv AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DEF VAR start-date AS DATE INIT 01/01/1901 NO-UNDO.
DEF VAR end-date   AS DATE INIT 01/01/1901 NO-UNDO.


FIND FIRST company WHERE company.company EQ cocode NO-LOCK.

SESSION:SET-WAIT-STATE("General").

FIND FIRST b-period
    WHERE b-period.company EQ cocode
      AND b-period.yr      EQ fi_to-year
      AND b-period.pnum    EQ fi_to-period
    NO-LOCK NO-ERROR.

IF begin_run-no EQ 0 THEN
FOR EACH period
    WHERE period.company EQ cocode
      AND period.yr      EQ fi_fr-year
      AND period.pnum    EQ fi_fr-period
    TRANSACTION:

  FOR EACH ap-ledger
      WHERE ap-ledger.company EQ period.company
        AND ap-ledger.tr-date GE period.pst
        AND ap-ledger.tr-date LE period.pend:

    RUN do-ap-ledger.
  END.

  FOR EACH ar-ledger
      WHERE ar-ledger.company EQ period.company
        AND ar-ledger.tr-date GE period.pst
        AND ar-ledger.tr-date LE period.pend
        AND ar-ledger.ref-num BEGINS "INV# ":

    RUN do-ar-ledger.
  END.

  FOR EACH account WHERE account.company EQ cocode NO-LOCK:
    FOR EACH glhist
        WHERE glhist.company eq cocode
          AND glhist.actnum  eq account.actnum
          AND glhist.tr-date GE period.pst
          AND glhist.tr-date LE period.pend:

      RUN do-glhist.
    END.
        
  END.

  FOR EACH gl-jrn
      WHERE gl-jrn.posted  EQ YES
        AND gl-jrn.company EQ period.company
        AND gl-jrn.tr-date GE period.pst
        AND gl-jrn.tr-date LE period.pend:

    RUN do-gl-jrn.
  END.

  IF begin_run-no EQ 0 THEN
    IF AVAIL b-period THEN DELETE period.

    ELSE
      ASSIGN
       period.yr   = fi_to-year
       period.pnum = fi_to-period.
END.

ELSE
DO TRANSACTION:

  FOR EACH ap-ledger
      WHERE ap-ledger.company EQ cocode
      AND ap-ledger.trnum   EQ begin_run-no
      USE-INDEX trnum:

    RUN do-ap-ledger.
  END.

  FOR EACH ar-ledger
      WHERE ar-ledger.company EQ cocode
      AND ar-ledger.tr-num  EQ begin_run-no
      USE-INDEX tr-num:

    RUN do-ar-ledger.
  END.

  FOR EACH glhist
      WHERE glhist.company EQ cocode
        AND glhist.tr-num  EQ begin_run-no
      USE-INDEX tr-num:

    RUN do-glhist.
  END.
   

  EMPTY TEMP-TABLE tt-journal.

  
  FOR EACH glhist NO-LOCK
      WHERE glhist.company EQ cocode
        AND glhist.tr-num  EQ begin_run-no
        AND glhist.jrnl    EQ "GENERAL":
        
    IF glhist.tr-dscr MATCHES "*JRN#" THEN NEXT.

    CREATE tt-journal.
    tt-journal = INT(SUBSTR(glhist.tr-dscr,INDEX(glhist.tr-dscr,"JRN#") + 4,7)).
  END.

  FOR EACH tt-journal,
      EACH gl-jrn
      WHERE gl-jrn.company EQ cocode
        AND gl-jrn.journal EQ tt-journal
        AND gl-jrn.posted  EQ YES
      BREAK BY tt-journal:

    IF FIRST-OF(tt-journal) THEN RUN do-gl-jrn.

    DELETE tt-journal.
  END.
END.


FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode NO-LOCK NO-ERROR.

IF AVAIL gl-ctrl THEN DO TRANSACTION:
  /*FOR EACH account
      WHERE account.company EQ cocode
        AND account.actnum  NE gl-ctrl.contra
        AND account.actnum  NE gl-ctrl.ret
        AND INDEX("ALCT",account.type) GT 0:
    DO li = 1 TO 13:
      account.cyr-open = account.cyr-open - account.lyr[li].
    END.
  END.*/

 IF tb_gl-tot THEN
     RUN util/fxacctg2.p.
     
 IF tb_verify-gl-tot THEN
     RUN util/VerifyAcctGl.p .

  /*FOR EACH account
      WHERE account.company EQ cocode
        AND account.actnum  NE gl-ctrl.contra
        AND account.actnum  NE gl-ctrl.ret
        AND INDEX("ALCT",account.type) GT 0:
    DO li = 1 TO 13:
      account.cyr-open = account.cyr-open + account.lyr[li].
    END.
  END.*/

  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pstat   EQ YES
      NO-LOCK NO-ERROR.

  IF AVAIL period THEN DO:
    IF tb_cust-per THEN
    FOR EACH cust WHERE cust.company EQ cocode:
      {util/reopeny1.i 1 lyytd lyr 6}

      {util/reopeny1.i 0 ytd ytd 5}
    END.

    /* Vend Processing  */
    IF tb_vend-per THEN
    FOR EACH vend WHERE vend.company EQ cocode:
      {util/reopeny2.i 1 lyytd last-year}

      {util/reopeny2.i 0 ytd-msf purch[13]}
    END. /* for each vend */
  END.
END.  /* TRANS */

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

