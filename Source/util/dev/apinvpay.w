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

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_vend end_vend begin_inv end_inv ~
begin_date end_date rd_paid btn-process btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS begin_vend end_vend begin_inv end_inv ~
begin_date end_date rd_paid 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_inv AS CHARACTER FORMAT "X(12)":U 
     LABEL "Beginning Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv AS CHARACTER FORMAT "X(12)":U INITIAL "zzzzzzzzzzzz" 
     LABEL "Ending Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE rd_paid AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Paid", yes,
"Unpaid", no
     SIZE 22 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_vend AT ROW 6.71 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 6.71 COL 68 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     begin_inv AT ROW 7.67 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 7.67 COL 68 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     begin_date AT ROW 8.62 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 8.62 COL 68 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     rd_paid AT ROW 10.52 COL 42 NO-LABEL
     btn-process AT ROW 14.1 COL 21
     btn-cancel AT ROW 14.1 COL 52
     RECT-17 AT ROW 4.81 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Mark as:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 10.52 COL 32
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 15.38.

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
         TITLE              = "Mark AP Invoices as Paid/Unpaid"
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
                                                                        */
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
ON END-ERROR OF C-Win /* Mark AP Invoices as Paid/Unpaid */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mark AP Invoices as Paid/Unpaid */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv C-Win
ON LEAVE OF begin_inv IN FRAME FRAME-A /* Beginning Invoice# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
DO:
  ASSIGN {&self-name}.
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

  MESSAGE "Are you sure you want to" TRIM(c-win:TITLE)
          "within the selected parameters?"       
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv C-Win
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_paid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_paid C-Win
ON VALUE-CHANGED OF rd_paid IN FRAME FRAME-A
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
  DISPLAY begin_vend end_vend begin_inv end_inv begin_date end_date rd_paid 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_vend end_vend begin_inv end_inv begin_date end_date rd_paid 
         btn-process btn-cancel RECT-17 
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
DEF VAR ld-amt-to-pay AS DEC NO-UNDO.
DEF VAR li-check-no AS INT NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DEF BUFFER b-ap-payl FOR ap-payl.


SESSION:SET-WAIT-STATE("General").

FIND FIRST bank WHERE bank.company EQ cocode.

FIND FIRST period
    WHERE period.company EQ cocode
      AND period.pst     LE TODAY
      AND period.pend    GE TODAY
    NO-LOCK.

FOR EACH ap-inv
    WHERE ap-inv.company   EQ cocode
      AND ap-inv.posted    EQ YES
      AND ap-inv.vend-no   GE begin_vend
      AND ap-inv.vend-no   LE end_vend
      AND ap-inv.inv-no    GE begin_inv
      AND ap-inv.inv-no    LE end_inv
      AND ap-inv.inv-date  GE begin_date
      AND ap-inv.inv-date  LE end_date
    USE-INDEX ap-inv:

  ld-amt-to-pay = ap-inv.net.

  FOR EACH ap-payl
      WHERE ap-payl.inv-no   EQ ap-inv.inv-no
        AND ap-payl.vend-no  EQ ap-inv.vend-no
        AND ap-payl.posted   EQ YES
        AND CAN-FIND(FIRST ap-pay WHERE ap-pay.company EQ cocode
                                    AND ap-pay.c-no    EQ ap-payl.c-no
                                  USE-INDEX c-no)
      USE-INDEX inv-no:

    FIND FIRST reftable
        WHERE reftable.reftable EQ "APINVPAY"
          AND reftable.company  EQ cocode
          AND reftable.loc      EQ ap-payl.vend-no
          AND reftable.code     EQ bank.actnum
          AND reftable.code2    EQ STRING(ap-payl.check-no,"999999")
        NO-ERROR.

    IF NOT rd_paid AND AVAIL reftable THEN DO:
      FIND FIRST ap-pay
          WHERE ap-pay.company EQ cocode
            AND ap-pay.c-no    EQ ap-payl.c-no
          USE-INDEX c-no NO-ERROR.

      IF AVAIL ap-pay THEN DO:
        FIND FIRST b-ap-payl
            WHERE b-ap-payl.c-no   EQ ap-pay.c-no
              AND ROWID(b-ap-payl) NE ROWID(ap-invl)
            NO-LOCK NO-ERROR.
        IF AVAIL b-ap-payl THEN ap-pay.check-amt = ap-pay.check-amt - ap-payl.amt-paid.
        ELSE DO:
          DELETE ap-pay.
          DELETE reftable.
        END. 
      END.

      DELETE ap-payl.
    END.

    ELSE DO:
      IF ap-payl.amt-paid NE 0 THEN ld-amt-to-pay = ld-amt-to-pay - ap-payl.amt-paid.
      IF ap-payl.amt-disc NE 0 THEN DO:
        IF NOT ap-payl.memo THEN ld-amt-to-pay = ld-amt-to-pay - ap-payl.amt-disc.
        IF ap-payl.memo THEN ld-amt-to-pay = ld-amt-to-pay + ap-payl.amt-disc.
      END.
    END.
  END. /* for each ap-payl */

  ap-inv.due = ld-amt-to-pay.

  IF rd_paid AND ap-inv.due NE 0 THEN DO:
    FIND FIRST reftable
        WHERE reftable.reftable EQ "APINVPAY"
          AND reftable.company  EQ cocode
          AND reftable.loc      EQ ap-inv.vend-no
          AND reftable.code     EQ bank.actnum
          AND reftable.val[1]   EQ TODAY - 01/01/0001
        NO-ERROR.

    IF AVAIL reftable THEN li-check-no = INT(reftable.code2).
    ELSE DO:
      ASSIGN
       bank.last-chk = bank.last-chk + 1
       li-check-no   = bank.last-chk.

      CREATE reftable.
      ASSIGN
       reftable.reftable = "APINVPAY"
       reftable.company  = cocode
       reftable.loc      = ap-inv.vend-no
       reftable.code     = bank.actnum
       reftable.code2    = STRING(li-check-no,"999999")
       reftable.val[1]   = TODAY - 01/01/0001.
    END.

    FIND FIRST ap-pay
        WHERE ap-pay.company    EQ cocode
          AND ap-pay.check-act  EQ bank.actnum
          AND ap-pay.check-no   EQ li-check-no         
        NO-ERROR.

    IF NOT AVAIL ap-pay THEN DO:
      FIND LAST ap-pay USE-INDEX c-no NO-LOCK NO-ERROR.
      li = (IF AVAIL ap-pay THEN ap-pay.c-no ELSE 0) + 1.

      CREATE ap-pay.
      ASSIGN
       ap-pay.c-no       = li
       ap-pay.company    = cocode
       ap-pay.posted     = YES
       ap-pay.bank-code  = bank.bank-code
       ap-pay.check-act  = bank.actnum
       ap-pay.check-date = TODAY
       ap-pay.vend-no    = ap-inv.vend-no
       ap-pay.period     = period.pnum
       ap-pay.check-no   = li-check-no
       ap-pay.man-check  = YES.
    END.

    ap-pay.check-amt = ap-pay.check-amt + ld-amt-to-pay.

    FIND LAST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no
        USE-INDEX c-no NO-LOCK NO-ERROR.
    li = (IF AVAIL ap-payl THEN ap-payl.line ELSE 0) + 1.

    CREATE ap-payl.
    ASSIGN
     ap-payl.c-no       = ap-pay.c-no
     ap-payl.posted     = ap-pay.posted
     ap-payl.line       = li
     ap-payl.memo       = NO
     ap-payl.vend-no    = ap-pay.vend-no
     ap-payl.check-no   = ap-pay.check-no
     ap-payl.man-check  = ap-pay.man-check
     ap-payl.actnum     = bank.actnum
     ap-payl.inv-no     = ap-inv.inv-no
     ap-payl.due-date   = ap-inv.due-date
     ap-payl.amt-paid   = ld-amt-to-pay
     ap-payl.amt-disc   = 0.

    ap-inv.due  = 0.    
  END.

  ap-inv.paid = ap-inv.net - ap-inv.due.

  IF ap-inv.paid LT 0 THEN ap-inv.paid = 0.
END.

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

