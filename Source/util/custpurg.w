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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust end_cust btn-process btn-cancel ~
RECT-17 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust 

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 7.19 COL 45 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 9.1 COL 45 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     btn-process AT ROW 16.48 COL 21
     btn-cancel AT ROW 16.48 COL 53
     RECT-17 AT ROW 4.81 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-H
     "WARNING: Deleting Customers will delete all associated history!" VIEW-AS TEXT
          SIZE 78 BY 2.62 AT ROW 1 COL 7
          BGCOLOR 12 FGCOLOR 14 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.91
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
         TITLE              = "Purge Customers"
         HEIGHT             = 17.71
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
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE
       FRAME FRAME-H:FRAME = FRAME FRAME-A:HANDLE.

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
/* SETTINGS FOR FRAME FRAME-H
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Customers */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Customers */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
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


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
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

  IF access-close THEN DO:
    APPLY "close" TO THIS-PROCEDURE.
    RETURN .
  END.

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
  DISPLAY begin_cust end_cust 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust end_cust btn-process btn-cancel RECT-17 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF VAR v-char AS CHAR NO-UNDO.


SESSION:SET-WAIT-STATE("General").

FOR EACH cust
    WHERE cust.company EQ cocode
      AND cust.cust-no GE begin_cust
      AND cust.cust-no LE end_cust
      AND cust.cust-no NE "":

  FOR EACH ar-cash
      WHERE ar-cash.company EQ cust.company
        AND ar-cash.cust-no EQ cust.cust-no
      USE-INDEX ar-cash

      TRANSACTION:

    ar-cash.posted = NO.

    DELETE ar-cash.
  END.

  FOR EACH ar-inv
      WHERE ar-inv.company EQ cust.company
        AND ar-inv.cust-no EQ cust.cust-no
      USE-INDEX ar-inv

      TRANSACTION:

    ar-inv.posted = NO.

    DELETE ar-inv.
  END.

  FOR EACH ar-ledger
      WHERE ar-ledger.company EQ cust.company
        AND ar-ledger.cust-no EQ cust.cust-no
      USE-INDEX ar-ledger

      TRANSACTION:

    DELETE ar-ledger.
  END.

  v-char = "".

  FIND FIRST eb
      WHERE eb.company EQ cust.company
      USE-INDEX cust NO-LOCK NO-ERROR.

  DO WHILE AVAIL eb:
    v-char = eb.loc.

    FOR EACH eb
        WHERE eb.company EQ cust.company
          AND eb.loc     EQ v-char
          AND eb.cust-no EQ cust.cust-no
        USE-INDEX cust

        TRANSACTION:

      FOR EACH probeit
          WHERE probeit.company EQ eb.company
            AND probeit.est-no  EQ eb.est-no
            AND probeit.cust-no EQ cust.cust-no
          USE-INDEX probe-item:

        DELETE probeit.
      END.

      FOR EACH quote
          WHERE quote.company EQ cust.company
            AND quote.loc     EQ eb.loc
            AND quote.est-no  EQ eb.est-no
            AND quote.cust-no EQ cust.cust-no
          USE-INDEX qqq:

        DELETE quote.
      END.

      DELETE eb.
    END.

    FIND FIRST eb
        WHERE eb.company EQ cust.company
          AND eb.loc     GT v-char
        USE-INDEX cust NO-LOCK NO-ERROR.
  END.

  FOR EACH EDIVTran
      WHERE EDIVTran.company EQ cust.company
        AND EDIVTran.cust    EQ cust.cust-no

      TRANSACTION:

    DELETE EDIVTran.
  END.

  FOR EACH EDMast WHERE EDMast.cust EQ cust.cust-no

      TRANSACTION:

    DELETE EDMast.
  END.

  FOR EACH EDPD WHERE EDPD.cust EQ cust.cust-no

      TRANSACTION:

    DELETE EDPD.
  END.

  FOR EACH EDPOTran WHERE EDPOTran.cust EQ cust.cust-no

      TRANSACTION:

    DELETE EDPOTran.
  END.

  FOR EACH EDShipto WHERE EDShipto.cust EQ cust.cust-no
      USE-INDEX ByCustShip

      TRANSACTION:

    DELETE EDShipto.
  END.

  IF cust.cust-no NE "" THEN
  FOR EACH fg-rcpts
      WHERE fg-rcpts.company EQ cust.company
        AND fg-rcpts.cust-no EQ cust.cust-no
      USE-INDEX cust-no

      TRANSACTION:

    DELETE fg-rcpts.
  END.

  FOR EACH inv-head
      WHERE inv-head.company EQ cust.company
        AND inv-head.cust-no EQ cust.cust-no
      USE-INDEX cust

      TRANSACTION:

    FOR EACH inv-line
        WHERE inv-line.r-no    EQ inv-head.r-no
          AND inv-line.cust-no EQ cust.cust-no
        USE-INDEX r-no:

      DELETE inv-line.
    END.

    DELETE inv-head.
  END.

  IF cust.cust-no NE "" THEN
  FOR EACH itemfg
      WHERE itemfg.company EQ cust.company
        AND itemfg.cust-no EQ cust.cust-no
      USE-INDEX customer

      TRANSACTION:

    FOR EACH itemfgdtl
        WHERE itemfgdtl.company EQ cust.company
          AND itemfgdtl.i-no    EQ itemfg.i-no
          AND itemfgdtl.cust-no EQ cust.cust-no
        USE-INDEX pi-itemfgdtl:

      itemfgdtl.cust-no = "".
    END.

    itemfg.cust-no = "".
  END.

  FOR EACH job-hdr
      WHERE job-hdr.company eq cust.company
        AND job-hdr.cust-no eq cust.cust-no
      USE-INDEX cust-idx

      TRANSACTION:

    FOR EACH fg-act
        WHERE fg-act.company EQ cust.company
          AND fg-act.job     EQ job-hdr.job
          AND fg-act.job-no  EQ job-hdr.job-no
          AND fg-act.job-no2 EQ job-hdr.job-no2
          AND fg-act.cust-no EQ cust.cust-no
        USE-INDEX job-idx:

      DELETE fg-act.
    END.

    FOR EACH fg-hist
        WHERE fg-hist.company EQ cust.company
          AND fg-hist.i-no    EQ job-hdr.i-no
          AND fg-hist.job-no  EQ job-hdr.job-no
          AND fg-hist.cust-no EQ cust.cust-no
        USE-INDEX ino:

      DELETE fg-hist.
    END.

    DELETE job-hdr.
  END.

  FOR EACH oe-bolh
      WHERE oe-bolh.company eq cust.company
        AND oe-bolh.cust-no eq cust.cust-no
      USE-INDEX cust

      TRANSACTION:

    FOR EACH oe-ship
        WHERE oe-ship.company EQ cust.company
          AND oe-ship.bol-no  EQ STRING(oe-bolh.bol-no,"99999999")
          AND oe-ship.cust-no EQ cust.cust-no
        USE-INDEX pi-oe-ship:

      DELETE oe-ship.
    END.

    DELETE oe-bolh.
  END.

  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ cust.company
        AND oe-ordl.cust-no EQ cust.cust-no
      USE-INDEX cust

      TRANSACTION:

    FOR EACH oe-rel
        WHERE oe-rel.company EQ cust.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
          AND oe-rel.cust-no EQ cust.cust-no
        USE-INDEX ord-item:

      DELETE oe-rel.
    END.

    DELETE oe-ordl.
  END.

  FOR EACH oe-ord
      WHERE oe-ord.company EQ cust.company
        AND oe-ord.cust-no EQ cust.cust-no
      USE-INDEX cust

      TRANSACTION:

    FOR EACH oe-rell
        WHERE oe-rell.company EQ cust.company
          AND oe-rell.ord-no  eq oe-ord.ord-no:

      DELETE oe-rell.
    END.

    DELETE oe-ord.
  END.

  v-char = "".

  IF cust.cust-no NE "" THEN
  FIND FIRST oe-prmtx
      WHERE oe-prmtx.company EQ cust.company
      USE-INDEX custitem NO-LOCK NO-ERROR.

  DO WHILE AVAIL oe-prmtx:
    v-char = oe-prmtx.custype.

    FOR EACH oe-prmtx
        WHERE oe-prmtx.company EQ cust.company
          AND oe-prmtx.custype EQ v-char
          AND oe-prmtx.cust-no EQ cust.cust-no
        USE-INDEX custitem

        TRANSACTION:

      DELETE oe-prmtx.
    END.

    FIND FIRST oe-prmtx
        WHERE oe-prmtx.company EQ cust.company
          AND oe-prmtx.custype GT v-char
        USE-INDEX custitem NO-LOCK NO-ERROR.
  END.
  RELEASE oe-prmtx.

  DO i = 1 TO 4:
    FOR EACH oe-reth
        WHERE oe-reth.company EQ cust.company
          AND oe-reth.posted  EQ (i GE 3)
          AND oe-reth.applied EQ (i MODULO 2 EQ 0)
          AND oe-reth.cust-no EQ cust.cust-no
        USE-INDEX posted

        TRANSACTION:

      DELETE oe-reth.
    END.
  END.

  FOR EACH pdh WHERE pdh.cust eq cust.cust-no

      TRANSACTION:

    DELETE pdh.
  END.

  IF cust.cust-no NE "" THEN
  FOR EACH po-ord
      WHERE po-ord.company EQ cust.company
        AND po-ord.cust-no EQ cust.cust-no

      TRANSACTION:

    FOR EACH po-ordl
        WHERE po-ordl.company EQ cust.company
          AND po-ordl.po-no   EQ po-ord.po-no
          AND po-ordl.cust-no EQ cust.cust-no
        USE-INDEX po-no:

      DELETE po-ordl.
    END.

    DELETE po-ord.
  END.

  IF cust.cust-no NE "" THEN
  FOR EACH po-ordl
      WHERE po-ordl.company EQ cust.company
        AND po-ordl.cust-no EQ cust.cust-no
      USE-INDEX cust

      TRANSACTION:

    DELETE po-ordl.
  END.

  FOR EACH shipto 
      WHERE shipto.company EQ cust.company
        AND shipto.cust-no EQ cust.cust-no
      USE-INDEX ship-id:

    DELETE shipto. 
  END.

  FOR EACH soldto 
      WHERE soldto.company EQ cust.company
        AND soldto.cust-no EQ cust.cust-no
      USE-INDEX sold-id:

    DELETE soldto.
  END.

  DO TRANSACTION:
    DELETE cust.
  END.
END.

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

