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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_i-no end_i-no tb_0 tb_inactive ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no tb_0 tb_inactive 

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

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 6.67.

DEFINE VARIABLE tb_0 AS LOGICAL INITIAL yes 
     LABEL "Fix Cost for ~"0~" in Cost and ~"?~" in Cost only" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE tb_inactive AS LOGICAL INITIAL no 
     LABEL "Include Inactive Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_i-no AT ROW 6.48 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_i-no AT ROW 7.91 COL 28 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     tb_0 AT ROW 9.33 COL 28
     tb_inactive AT ROW 10.29 COL 28 WIDGET-ID 2
     btn-process AT ROW 11.71 COL 21
     btn-cancel AT ROW 11.71 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 12.67.

DEFINE FRAME FRAME-B
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 3
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 79 BY .95 AT ROW 2.91 COL 8
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
         TITLE              = "Fix FG Hist Cost"
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


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix FG Hist Cost */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix FG Hist Cost */
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
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
          " for the selected parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
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
  DISPLAY begin_i-no end_i-no tb_0 tb_inactive 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_i-no end_i-no tb_0 tb_inactive btn-process btn-cancel 
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
DEF VAR lv-po-no AS INT NO-UNDO.
DEF VAR lv-cost LIKE fg-rdtlh.cost NO-UNDO EXTENT 5.
DEF VAR lv-uom LIKE fg-rcpth.pur-uom NO-UNDO.
DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-wid LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DEF BUFFER b-fg-bin FOR fg-bin.


SESSION:SET-WAIT-STATE("General").

STATUS DEFAULT "Searching...".

FOR EACH itemfg NO-LOCK
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    GE begin_i-no
      AND itemfg.i-no    LE end_i-no
      AND (itemfg.stat EQ 'A' OR tb_inactive) :

  STATUS DEFAULT "Processing FG Item#: " + TRIM(itemfg.i-no).

  FOR EACH fg-rcpth
      WHERE fg-rcpth.company EQ itemfg.company
        AND fg-rcpth.i-no    EQ itemfg.i-no
      USE-INDEX i-no,
      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND (fg-rdtlh.cost EQ 0                           OR
             fg-rdtlh.cost EQ ?                           OR
             NOT tb_0
             /*26626 - Premier "S" transactions without Jobs not be corrected*/
/*                                             AND      */
/*              (INDEX("CRT",fg-rdtlh.rita-code) GT 0 OR*/
/*               fg-rcpth.job-no NE ""))                */
               )
      USE-INDEX rm-rdtl

      BREAK BY INT(fg-rcpth.rita-code NE "R")
            BY INT(fg-rcpth.rita-code NE "C")
            BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no
            BY fg-rdtlh.rec_key

      TRANSACTION:

    ASSIGN
     v-bwt      = 0
     v-len      = itemfg.t-len
     v-wid      = itemfg.t-wid
     v-dep      = 0
     lv-uom     = itemfg.prod-uom
     lv-cost[5] = itemfg.std-tot-cost
     lv-cost[1] = itemfg.std-lab-cost
     lv-cost[2] = itemfg.std-mat-cost
     lv-cost[3] = itemfg.std-var-cost
     lv-cost[4] = itemfg.std-fix-cost.

    RELEASE b-fg-bin.
    IF fg-rcpth.rita-code EQ "T" THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rcpth.company
            AND fg-bin.i-no    EQ fg-rcpth.i-no
            AND fg-bin.job-no  EQ fg-rcpth.job-no
            AND fg-bin.job-no2 EQ fg-rcpth.job-no2
            AND fg-bin.loc     EQ fg-rdtlh.loc2
            AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin2
            AND fg-bin.tag     EQ fg-rdtlh.tag2
            AND fg-bin.cust-no EQ fg-rdtlh.cust-no
          NO-ERROR.

      FIND FIRST b-fg-bin NO-LOCK
          WHERE b-fg-bin.company EQ fg-rcpth.company
            AND b-fg-bin.i-no    EQ fg-rcpth.i-no
            AND b-fg-bin.job-no  EQ fg-rcpth.job-no
            AND b-fg-bin.job-no2 EQ fg-rcpth.job-no2
            AND b-fg-bin.loc     EQ fg-rdtlh.loc
            AND b-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
            AND b-fg-bin.tag     EQ fg-rdtlh.tag
            AND b-fg-bin.cust-no EQ fg-rdtlh.cust-no
          NO-ERROR.

      IF AVAIL b-fg-bin             AND
         b-fg-bin.std-tot-cost NE 0 AND
         b-fg-bin.std-tot-cost NE ? THEN
        ASSIGN 
         lv-uom     = b-fg-bin.pur-uom
         lv-cost[5] = b-fg-bin.std-tot-cost
         lv-cost[1] = b-fg-bin.std-lab-cost
         lv-cost[2] = b-fg-bin.std-mat-cost
         lv-cost[3] = b-fg-bin.std-var-cost
         lv-cost[4] = b-fg-bin.std-fix-cost.
    END.

    ELSE DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rcpth.company
            AND fg-bin.i-no    EQ fg-rcpth.i-no
            AND fg-bin.job-no  EQ fg-rcpth.job-no
            AND fg-bin.job-no2 EQ fg-rcpth.job-no2
            AND fg-bin.loc     EQ fg-rdtlh.loc
            AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
            AND fg-bin.tag     EQ fg-rdtlh.tag
            AND fg-bin.cust-no EQ fg-rdtlh.cust-no
          NO-ERROR.
      IF AVAIL fg-bin             AND
         fg-bin.std-tot-cost NE 0 AND
         fg-bin.std-tot-cost NE ? THEN
        ASSIGN 
         lv-uom     = fg-bin.pur-uom
         lv-cost[5] = fg-bin.std-tot-cost
         lv-cost[1] = fg-bin.std-lab-cost
         lv-cost[2] = fg-bin.std-mat-cost
         lv-cost[3] = fg-bin.std-var-cost
         lv-cost[4] = fg-bin.std-fix-cost.
    END.

    RELEASE po-ordl.

    lv-po-no = INT(fg-rcpth.po-no) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN lv-po-no = 0.

    IF lv-po-no NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ fg-rcpth.company
            AND po-ordl.po-no     EQ lv-po-no
            AND po-ordl.item-type EQ NO
            AND po-ordl.i-no      EQ fg-rcpth.i-no
          NO-LOCK NO-ERROR.

/* 26626 - Removing this find since it does not match on FG Item*/
/*      IF NOT AVAIL po-ordl THEN                      */
/*      FIND FIRST po-ordl                             */
/*          WHERE po-ordl.company   EQ fg-rcpth.company*/
/*            AND po-ordl.po-no     EQ lv-po-no        */
/*            AND po-ordl.item-type EQ NO              */
/*          NO-LOCK NO-ERROR.                          */
    END.

    IF itemfg.pur-man AND fg-rcpth.rita-code EQ "R" AND NOT AVAIL po-ordl THEN
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company   EQ fg-rcpth.company
          AND po-ordl.i-no      EQ fg-rcpth.i-no
          AND po-ordl.item-type EQ NO
        BY po-ordl.po-no DESC:
      LEAVE.
    END.

    IF AVAIL po-ordl THEN
      ASSIGN
       v-len      = po-ordl.s-len
       v-wid      = po-ordl.s-wid
       lv-cost[2] = po-ordl.cost
       lv-cost[5] = po-ordl.cost
       lv-uom     = po-ordl.pr-uom.

    ELSE
    IF TRIM(fg-rcpth.job-no) NE "" THEN DO:
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ fg-rcpth.company
            AND job-hdr.i-no    EQ fg-rcpth.i-no
            AND job-hdr.job-no  EQ fg-rcpth.job-no
            AND job-hdr.job-no2 EQ fg-rcpth.job-no2
          NO-LOCK NO-ERROR.

      IF NOT AVAIL job-hdr THEN DO:
        FIND FIRST job
            WHERE job.company EQ fg-rcpth.company
              AND job.job-no  EQ fg-rcpth.job-no
              AND job.job-no2 EQ fg-rcpth.job-no2
          NO-LOCK NO-ERROR.
        IF AVAIL job THEN
        FIND FIRST reftable
            WHERE reftable.reftable EQ "jc/jc-calc.p"
              AND reftable.company  EQ job.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ STRING(job.job,"999999999")
              AND reftable.code2    EQ fg-rcpth.i-no
            NO-LOCK NO-ERROR.
      END.

      IF AVAIL job-hdr AND job-hdr.std-tot-cost GT 0 THEN
        ASSIGN
         lv-uom     = "M"
         lv-cost[5] = job-hdr.std-tot-cost
         lv-cost[1] = job-hdr.std-lab-cost
         lv-cost[2] = job-hdr.std-mat-cost
         lv-cost[3] = job-hdr.std-var-cost
         lv-cost[4] = job-hdr.std-fix-cost.
      ELSE
      IF AVAIL reftable AND reftable.val[5] GT 0 THEN
        ASSIGN
         lv-uom     = "M"
         lv-cost[5] = reftable.val[5]
         lv-cost[1] = reftable.val[1]
         lv-cost[2] = reftable.val[2]
         lv-cost[3] = reftable.val[3]
         lv-cost[4] = reftable.val[4].
    END.

    IF lv-uom NE itemfg.prod-uom THEN DO li = 1 TO 5:
      RUN custom/convcuom.p(fg-rcpth.company,
                            lv-uom, itemfg.prod-uom,                   
                            v-bwt, v-len, v-wid, v-dep,
                            lv-cost[li], OUTPUT lv-cost[li]).
    END.

    IF AVAIL fg-bin THEN
      ASSIGN
       fg-bin.pur-uom      = itemfg.prod-uom
       fg-bin.std-tot-cost = lv-cost[5]
       fg-bin.std-lab-cost = lv-cost[1]
       fg-bin.std-mat-cost = lv-cost[2]
       fg-bin.std-var-cost = lv-cost[3]
       fg-bin.std-fix-cost = lv-cost[4].

    IF lv-cost[5] EQ ? THEN lv-cost[5] = 0.

    IF lv-cost[5] NE 0 THEN DO:  
      ASSIGN 
       fg-rdtlh.cost    = lv-cost[5]
       fg-rcpth.pur-uom = itemfg.prod-uom.

      FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ fg-rcpth.r-no USE-INDEX fg-rctd NO-ERROR.
      IF AVAIL fg-rctd THEN  
        ASSIGN 
         fg-rctd.std-cost = lv-cost[5]
         fg-rctd.cost-uom = itemfg.prod-uom
         fg-rctd.ext-cost = fg-rctd.std-cost *
                            (fg-rctd.t-qty / IF fg-rctd.cost-uom EQ "M" THEN 1000 ELSE 1).
    END.
  END.
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

