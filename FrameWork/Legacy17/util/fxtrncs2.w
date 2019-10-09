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
&Scoped-Define ENABLED-OBJECTS begin_rm-no end_rm-no tb_0 btn-process ~
btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no tb_0 

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

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 6.19.

DEFINE VARIABLE tb_0 AS LOGICAL INITIAL yes 
     LABEL "Fix Cost for ~"0~" in Cost and ~"?~" in Cost only" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rm-no AT ROW 6.48 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 7.91 COL 28 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     tb_0 AT ROW 9.33 COL 28
     btn-process AT ROW 11.71 COL 21
     btn-cancel AT ROW 11.71 COL 53
     RECT-17 AT ROW 4.81 COL 1
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
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
         TITLE              = "Fix RM Hist Cost"
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
ON END-ERROR OF C-Win /* Fix RM Hist Cost */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix RM Hist Cost */
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

  IF access-close THEN DO:
    APPLY "close" TO THIS-PROCEDURE.
    RETURN .
  END.

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
  DISPLAY begin_rm-no end_rm-no tb_0 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_rm-no end_rm-no tb_0 btn-process btn-cancel RECT-17 
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
DEF VAR lv-cost LIKE rm-rdtlh.cost NO-UNDO.
DEF VAR lv-uom LIKE rm-rcpth.pur-uom NO-UNDO.
DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-wid LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DEF BUFFER b-rm-bin FOR rm-bin.


SESSION:SET-WAIT-STATE("General").

STATUS DEFAULT "Searching...".

FOR EACH rm-rcpth
    WHERE rm-rcpth.company EQ cocode
      AND rm-rcpth.i-no    GE begin_rm-no
      AND rm-rcpth.i-no    LE end_rm-no
    USE-INDEX i-no,
    FIRST item
    WHERE item.company EQ rm-rcpth.company
      AND item.i-no    EQ rm-rcpth.i-no
    USE-INDEX i-no NO-LOCK,
    EACH rm-rdtlh
    WHERE rm-rdtlh.r-no EQ rm-rcpth.r-no
      AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
      AND (rm-rdtlh.cost EQ 0                           OR
           rm-rdtlh.cost EQ ?                           OR
           (NOT tb_0                                AND
            INDEX("CRT",rm-rdtlh.rita-code) GT 0))
    USE-INDEX rm-rdtl

    BY rm-rcpth.i-no
    BY rm-rcpth.trans-date
    BY rm-rcpth.r-no
    BY rm-rdtlh.rec_key

    TRANSACTION:

  STATUS DEFAULT "Processing Item#: " + TRIM(rm-rcpth.i-no).

  ASSIGN
   v-bwt   = item.basis-w
   v-len   = IF item.r-wid NE 0 THEN 12         ELSE item.s-len
   v-wid   = IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid
   v-dep   = item.s-dep
   lv-uom  = item.cons-uom
   lv-cost = item.avg-cost.

  FIND FIRST rm-bin
      WHERE rm-bin.company EQ rm-rcpth.company
        AND rm-bin.i-no    EQ rm-rcpth.i-no
        AND rm-bin.loc     EQ rm-rdtlh.loc
        AND rm-bin.loc-bin EQ rm-rdtlh.loc-bin
        AND rm-bin.tag     EQ rm-rdtlh.tag
      NO-ERROR.
  IF AVAIL rm-bin     AND
     rm-bin.cost NE 0 AND
     rm-bin.cost NE ? THEN lv-cost = rm-bin.cost.

  RELEASE b-rm-bin.
  IF rm-rcpth.rita-code EQ "T" THEN
  FIND FIRST b-rm-bin NO-LOCK
      WHERE b-rm-bin.company EQ rm-rcpth.company
        AND b-rm-bin.i-no    EQ rm-rcpth.i-no
        AND b-rm-bin.loc     EQ rm-rdtlh.loc2
        AND b-rm-bin.loc-bin EQ rm-rdtlh.loc-bin2
        AND b-rm-bin.tag     EQ rm-rdtlh.tag2
      NO-ERROR.

  IF AVAIL b-rm-bin     AND
     b-rm-bin.cost NE 0 AND
     b-rm-bin.cost NE ? THEN lv-cost = b-rm-bin.cost.

  RELEASE po-ordl.

  lv-po-no = INT(rm-rcpth.po-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN lv-po-no = 0.

  IF lv-po-no NE 0 THEN DO:
    FIND FIRST po-ordl NO-LOCK
        WHERE po-ordl.company   EQ rm-rcpth.company
          AND po-ordl.po-no     EQ lv-po-no
          AND po-ordl.item-type EQ YES
          AND po-ordl.i-no      EQ rm-rcpth.i-no
        NO-ERROR.

    IF NOT AVAIL po-ordl THEN
    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ rm-rcpth.company
          AND po-ordl.po-no     EQ lv-po-no
          AND po-ordl.item-type EQ YES
        NO-LOCK NO-ERROR.
  END.

  IF AVAIL po-ordl THEN
    ASSIGN
     v-len   = po-ordl.s-len
     v-wid   = po-ordl.s-wid
     lv-cost = po-ordl.cost
     lv-uom  = po-ordl.pr-uom.

  IF lv-uom NE item.cons-uom THEN
    RUN custom/convcuom.p(rm-rcpth.company,
                          lv-uom, item.cons-uom,                   
                          v-bwt, v-len, v-wid, v-dep,
                          lv-cost, OUTPUT lv-cost).

  IF lv-cost EQ ? THEN lv-cost = 0.

  IF AVAIL rm-bin THEN rm-bin.cost = lv-cost.

  IF lv-cost NE 0 THEN DO:  
    ASSIGN 
     rm-rdtlh.cost    = lv-cost
     rm-rcpth.pur-uom = item.cons-uom.

    FIND FIRST rm-rctd WHERE rm-rctd.r-no EQ rm-rcpth.r-no USE-INDEX rm-rctd NO-ERROR.
    IF AVAIL rm-rctd THEN  
      ASSIGN 
       rm-rctd.cost     = lv-cost
       rm-rctd.pur-uom  = item.cons-uom
       rm-rctd.cost-uom = item.cons-uom.
  END.
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

