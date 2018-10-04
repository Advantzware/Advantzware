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

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

FUNCTION Floor RETURNS INTEGER (INPUT ipdValue AS DECIMAL):
    IF (ipdValue GE 0) OR (TRUNCATE(ipdValue,0) = ipdValue) THEN
        RETURN INTEGER (TRUNCATE(ipdValue,0)).
    ELSE
        RETURN integer(TRUNCATE(ipdValue,0) - 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_po-no end_po-no begin_date ~
end_date btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_po-no end_po-no begin_date end_date 

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
     LABEL "From Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "From PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "To PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_po-no AT ROW 6.95 COL 23 COLON-ALIGNED
     end_po-no AT ROW 6.95 COL 63 COLON-ALIGNED
     begin_date AT ROW 8.62 COL 23 COLON-ALIGNED HELP
          "From Receipt Date"
     end_date AT ROW 8.62 COL 63 COLON-ALIGNED HELP
          "Enter Ending Receipt Date"
     btn-process AT ROW 11.95 COL 22
     btn-cancel AT ROW 11.95 COL 52
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 12.62.

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
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Fix Receipt Cost"
         HEIGHT             = 12.67
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


ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix Receipt Cost */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix Receipt Cost */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* From Receipt Date */
DO:
  assign {&self-name}.
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
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
          " for the selected parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

  IF ll THEN RUN run-process.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* To Receipt Date */
DO:
  assign {&self-name}.
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

  APPLY "entry" TO begin_po-no.

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
  DISPLAY begin_po-no end_po-no begin_date end_date 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_po-no end_po-no begin_date end_date btn-process 
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
DEF BUFFER b-item FOR item.
DEF BUFFER b-rh FOR rm-rcpth.
DEF BUFFER b-rd FOR rm-rdtlh.
DEF BUFFER b-po-ordl FOR po-ordl.
DEF BUFFER b-item2 FOR item.

DEF VAR ld-cst AS DEC NO-UNDO.
DEF VAR ld-cst-iss AS DEC NO-UNDO.
DEF VAR ld-qty AS DEC NO-UNDO.
DEF VAR ld-qty-iss AS DEC NO-UNDO.
DEF VAR ld-qty-rec AS DEC NO-UNDO.
DEF VAR v-rm-issue-cost AS DEC NO-UNDO.
DEF VAR v-total-po-wid AS DEC NO-UNDO.
DEF VAR v-num-items AS INT NO-UNDO.
DEF VAR v-single-item-dev AS INT NO-UNDO.

FOR EACH po-ord NO-LOCK
    WHERE po-ord.company EQ cocode
      AND po-ord.type    EQ "S"
      AND po-ord.po-no   GE begin_po-no
      AND po-ord.po-no   LE end_po-no,
    EACH po-ordl NO-LOCK
    WHERE po-ordl.company EQ po-ord.company
      AND po-ordl.po-no   EQ po-ord.po-no,
    FIRST item NO-LOCK
    WHERE item.company EQ po-ord.company
      AND item.i-no    EQ po-ordl.i-no,
    EACH b-rh NO-LOCK
    WHERE b-rh.company    EQ po-ordl.company
      AND b-rh.po-no      EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>"))
      AND b-rh.i-no       EQ po-ordl.i-no
      AND b-rh.trans-date GE begin_date
      AND b-rh.trans-date LE end_date,
    EACH b-rd
    WHERE b-rd.r-no      EQ b-rh.r-no
      AND b-rd.rita-code EQ b-rh.rita-code:

  ASSIGN
   ld-qty     = po-ordl.cons-qty
   ld-qty-iss = 0
   ld-qty-rec = 0
   ld-cst-iss = 0.

  IF po-ordl.cons-uom NE b-rh.pur-uom THEN
    RUN custom/convquom.p (po-ordl.company,
                           po-ordl.cons-uom,
                           b-rh.pur-uom,
                           item.basis-w,
                           po-ordl.s-len,
                           po-ordl.s-wid,
                           item.s-dep,
                           po-ordl.cons-qty,
                           OUTPUT ld-qty).

  ASSIGN
     b-rd.cost = po-ordl.t-cost / ld-qty
     v-total-po-wid = 0
     v-num-items = 0
     ld-cst = 0.

  FOR EACH b-po-ordl fields(company i-no) WHERE
      b-po-ordl.company EQ po-ordl.company AND
      b-po-ordl.po-no EQ po-ordl.po-no
      NO-LOCK,
      FIRST b-item2 FIELDS(s-wid) WHERE
            b-item2.company EQ b-po-ordl.company AND
            b-item2.i-no    EQ b-po-ordl.i-no AND
            b-item2.mat-type EQ "B"
            NO-LOCK:

      ASSIGN
         v-total-po-wid = v-total-po-wid + b-item2.s-wid
         v-num-items = v-num-items + 1.
  END.

  FOR EACH rm-rcpth NO-LOCK
      WHERE rm-rcpth.company      EQ po-ord.company
        AND rm-rcpth.vend-no      EQ po-ord.vend-no
        AND rm-rcpth.po-no        EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>"))
        AND rm-rcpth.rita-code    EQ "I"
      USE-INDEX vend,
      EACH rm-rdtlh NO-LOCK
      WHERE rm-rdtlh.r-no             EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code        EQ rm-rcpth.rita-code,
      FIRST b-item NO-LOCK
      WHERE b-item.company EQ rm-rcpth.company
        AND b-item.i-no    EQ rm-rcpth.i-no
      BY rm-rcpth.trans-date
      BY ROWID(rm-rdtlh):

    IF NOT(rm-rcpth.trans-date LT b-rh.trans-date OR
       (rm-rcpth.trans-date EQ b-rh.trans-date AND
        SUBSTRING(rm-rcpth.rec_key,5,4) +
        SUBSTRING(rm-rcpth.rec_key,1,2) +
        SUBSTRING(rm-rcpth.rec_key,3,2) +
        SUBSTRING(rm-rcpth.rec_key,9) LT
        SUBSTRING(b-rh.rec_key,5,4) +
        SUBSTRING(b-rh.rec_key,1,2) +
        SUBSTRING(b-rh.rec_key,3,2) +
        SUBSTRING(b-rh.rec_key,9))) THEN
       NEXT.

    ASSIGN
     ld-qty = rm-rdtlh.qty
     v-rm-issue-cost = rm-rdtlh.cost.

    IF rm-rcpth.pur-uom NE "LF" THEN
       RUN custom/convcuom.p (rm-rcpth.company,
                              rm-rcpth.pur-uom,
                              "LF",
                              item.basis-w,
                              po-ordl.s-len,
                              po-ordl.s-wid,
                              item.s-dep,
                              v-rm-issue-cost, OUTPUT v-rm-issue-cost).

    /*result is in EA*/
    v-rm-issue-cost = v-rm-issue-cost * (item.s-len / 12.0) * (item.s-wid / v-total-po-wid).

    IF v-num-items EQ 1 THEN
       v-single-item-dev = FLOOR(b-ITEM.r-wid / ITEM.s-wid).
    ELSE
       v-single-item-dev = 0.

    IF "EA" NE b-rh.pur-uom THEN
       RUN custom/convcuom.p (rm-rcpth.company,
                              "EA",
                              b-rh.pur-uom,
                              item.basis-w,
                              po-ordl.s-len,
                              po-ordl.s-wid,
                              item.s-dep,
                              v-rm-issue-cost, OUTPUT v-rm-issue-cost).

    ld-cst = v-rm-issue-cost.

  END. /* EACH rm-rcpth*/

  IF ld-cst GT 0 THEN
  DO:
     IF v-num-items EQ 1 AND v-single-item-dev NE 0 THEN
        ld-cst = ld-cst / v-single-item-dev.

     b-rd.cost = b-rd.cost + ld-cst.
  END.
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

