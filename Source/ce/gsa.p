&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: ce\gsa.p
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-qty AS INT NO-UNDO.
DEF INPUT PARAM ip-rels AS INT NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

{cec/print4.i SHARED SHARED}
{cec/print42.i SHARED}

DEF SHARED BUFFER xest FOR est.
DEF BUFFER probe-ref FOR reftable.

DEF SHARED VAR qty AS INT NO-UNDO.

ASSIGN
 cocode = g_company
 locode = g_loc.

DO TRANSACTION:
  {est/calcpcts.i xest}
  FIND CURRENT calcpcts NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ce-ctrl

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame ce-ctrl.mat-cost[1] ~
ce-ctrl.mat-pct[1] ce-ctrl.lab-cost[1] ce-ctrl.lab-pct[1] ~
ce-ctrl.mat-cost[2] ce-ctrl.mat-pct[2] ce-ctrl.lab-cost[2] ~
ce-ctrl.lab-pct[2] ce-ctrl.mat-cost[3] ce-ctrl.mat-pct[3] ~
ce-ctrl.lab-cost[3] ce-ctrl.lab-pct[3] ce-ctrl.mat-cost[4] ~
ce-ctrl.mat-pct[4] ce-ctrl.lab-cost[4] ce-ctrl.lab-pct[4] ~
ce-ctrl.mat-cost[5] ce-ctrl.mat-pct[5] ce-ctrl.lab-cost[5] ~
ce-ctrl.lab-pct[5] ce-ctrl.mat-cost[6] ce-ctrl.mat-pct[6] ~
ce-ctrl.lab-cost[6] ce-ctrl.lab-pct[6] 
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ce-ctrl ~
      WHERE ce-ctrl.company = g_company and  ~
ASI.ce-ctrl.loc = g_loc SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ce-ctrl ~
      WHERE ce-ctrl.company = g_company and  ~
ASI.ce-ctrl.loc = g_loc SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ce-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ce-ctrl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ld-gsa-brd ld-gsa-mat ld-gsa-lab ld-gsa-war ~
ld-gsa-fm Btn_OK Btn_Cancel RECT-5 
&Scoped-Define DISPLAYED-FIELDS ce-ctrl.mat-cost[1] ce-ctrl.mat-pct[1] ~
ce-ctrl.lab-cost[1] ce-ctrl.lab-pct[1] ce-ctrl.mat-cost[2] ~
ce-ctrl.mat-pct[2] ce-ctrl.lab-cost[2] ce-ctrl.lab-pct[2] ~
ce-ctrl.mat-cost[3] ce-ctrl.mat-pct[3] ce-ctrl.lab-cost[3] ~
ce-ctrl.lab-pct[3] ce-ctrl.mat-cost[4] ce-ctrl.mat-pct[4] ~
ce-ctrl.lab-cost[4] ce-ctrl.lab-pct[4] ce-ctrl.mat-cost[5] ~
ce-ctrl.mat-pct[5] ce-ctrl.lab-cost[5] ce-ctrl.lab-pct[5] ~
ce-ctrl.mat-cost[6] ce-ctrl.mat-pct[6] ce-ctrl.lab-cost[6] ~
ce-ctrl.lab-pct[6] 
&Scoped-define DISPLAYED-TABLES ce-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE ce-ctrl
&Scoped-Define DISPLAYED-OBJECTS lv-head6 lv-head1 ld-qty lv-head7 ~
ld-gsa-brd lv-head2 ld-gsa-mat lv-head3 ld-gsa-lab lv-head5 ld-gsa-war ~
lv-head-fm ld-gsa-fm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ld-gsa-brd AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-fm AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-lab AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-mat AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-war AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-qty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head-fm AS CHARACTER FORMAT "X(256)":U INITIAL "Fold %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head1 AS CHARACTER FORMAT "X(256)":U INITIAL "Overrides for QTY" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head2 AS CHARACTER FORMAT "X(256)":U INITIAL "GS&A Material %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head3 AS CHARACTER FORMAT "X(256)":U INITIAL "GS&A Labor %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head4 AS CHARACTER FORMAT "X(256)":U INITIAL "Commission %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head5 AS CHARACTER FORMAT "X(256)":U INITIAL "Warehousing Mark Up %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head6 AS CHARACTER FORMAT "X(256)":U INITIAL "=====  GS&A MARK UP PERCENTAGES  ====" 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head7 AS CHARACTER FORMAT "X(256)":U INITIAL "GS&A Board %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 8.57.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ce-ctrl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ce-ctrl.mat-cost[1] AT ROW 3.24 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.mat-pct[1] AT ROW 3.14 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.lab-cost[1] AT ROW 3.14 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.lab-pct[1] AT ROW 3.14 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.mat-cost[2] AT ROW 4.24 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.mat-pct[2] AT ROW 4.14 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.lab-cost[2] AT ROW 4.14 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.lab-pct[2] AT ROW 4.14 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.mat-cost[3] AT ROW 5.24 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.mat-pct[3] AT ROW 5.14 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.lab-cost[3] AT ROW 5.14 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.lab-pct[3] AT ROW 5.14 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.mat-cost[4] AT ROW 6.24 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.mat-pct[4] AT ROW 6.14 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.lab-cost[4] AT ROW 6.14 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.lab-pct[4] AT ROW 6.14 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.mat-cost[5] AT ROW 7.24 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.mat-pct[5] AT ROW 7.14 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.lab-cost[5] AT ROW 7.14 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.lab-pct[5] AT ROW 7.14 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.mat-cost[6] AT ROW 8.24 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.mat-pct[6] AT ROW 8.14 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ce-ctrl.lab-cost[6] AT ROW 8.14 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ce-ctrl.lab-pct[6] AT ROW 8.14 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     lv-head6 AT ROW 1.24 COL 3 COLON-ALIGNED NO-LABEL
     lv-head1 AT ROW 1.24 COL 56 COLON-ALIGNED NO-LABEL
     ld-qty AT ROW 1.24 COL 92 COLON-ALIGNED
     lv-head7 AT ROW 2.67 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-brd AT ROW 2.67 COL 92 COLON-ALIGNED NO-LABEL
     lv-head2 AT ROW 3.62 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-mat AT ROW 3.62 COL 92 COLON-ALIGNED NO-LABEL
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     lv-head3 AT ROW 4.57 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-lab AT ROW 4.57 COL 92 COLON-ALIGNED NO-LABEL
     lv-head4 AT ROW 5.52 COL 90 RIGHT-ALIGNED NO-LABEL
     lv-head5 AT ROW 6.48 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war AT ROW 6.48 COL 92 COLON-ALIGNED NO-LABEL
     lv-head-fm AT ROW 7.43 COL 90 RIGHT-ALIGNED NO-LABEL WIDGET-ID 2
     ld-gsa-fm AT ROW 7.52 COL 92 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     Btn_OK AT ROW 10.05 COL 29
     Btn_Cancel AT ROW 10.05 COL 74
     "Cost" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 6
     "Labor%" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 42
     "Cost" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 30
     "Mat'l%" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 19
     RECT-5 AT ROW 1 COL 1
     SPACE(0.00) SKIP(2.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "GS&A Detail"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[1] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[2] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[3] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[4] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[5] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[6] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[1] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[2] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[3] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[4] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[5] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[6] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-qty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-head-fm IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-head2 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head3 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head4 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
/* SETTINGS FOR FILL-IN lv-head5 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-head7 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[1] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[2] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[3] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[4] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[5] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[6] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[1] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[2] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[3] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[4] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[5] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[6] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.ce-ctrl"
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.ce-ctrl.company = g_company and 
ASI.ce-ctrl.loc = g_loc"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* GSA Detail */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME} :
    ASSIGN {&displayed-objects}.
  END.

  RUN update-pcts.

  RUN update-probe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  DEF BUFFER b-probe FOR probe.

  DEF VAR li-qty AS INT NO-UNDO.

  FIND probe NO-LOCK WHERE ROWID(probe) EQ ip-rowid NO-ERROR.

  IF AVAIL probe THEN
  FIND FIRST est NO-LOCK
      WHERE est.company EQ probe.company
        AND est.est-no  EQ probe.est-no
      NO-ERROR.

  IF AVAIL est THEN DO:
    FIND FIRST ce-ctrl NO-LOCK
        WHERE ce-ctrl.company EQ est.company
          AND ce-ctrl.loc     EQ est.loc 
        NO-ERROR.
                           
    ASSIGN
     ld-gsa-brd = calcpcts.val[1]
     ld-gsa-mat = gsa-mat
     ld-gsa-lab = gsa-lab
     ld-gsa-war = gsa-war
     ld-gsa-fm  = gsa-fm
     ld-qty     = qty.

    IF NOT vprint OR est.override THEN
    FOR EACH b-probe NO-LOCK
        WHERE b-probe.company    EQ est.company
          AND b-probe.est-no     EQ est.est-no
          AND ROWID(b-probe)     NE ROWID(probe)
        BY b-probe.est-qty
        BY b-probe.probe-date DESC
        BY b-probe.probe-time DESC:

      ASSIGN
       ld-gsa-mat = b-probe.gsa-mat
       ld-gsa-lab = b-probe.gsa-lab
       ld-gsa-war = b-probe.gsa-war
       ld-gsa-fm  = int(b-probe.gsa-fm).

      FIND FIRST probe-ref NO-LOCK
          WHERE probe-ref.reftable EQ "probe-ref"
            AND probe-ref.company  EQ b-probe.company
            AND probe-ref.loc      EQ ""
            AND probe-ref.code     EQ b-probe.est-no
            AND probe-ref.code2    EQ STRING(b-probe.line,"9999999999")
          NO-ERROR.
      IF AVAIL probe-ref THEN ld-gsa-brd = probe-ref.val[1].

      IF b-probe.est-qty GE ip-qty THEN LEAVE.
    END.

    IF vprint THEN
      IF est.override THEN DO:
        RUN enable_UI.
        WAIT-FOR GO OF FRAME {&FRAME-NAME}.
      END.

      ELSE RUN update-probe.

    ELSE RUN update-pcts.
  END.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY lv-head6 lv-head1 ld-qty lv-head7 ld-gsa-brd lv-head2 ld-gsa-mat 
          lv-head3 ld-gsa-lab lv-head5 ld-gsa-war lv-head-fm ld-gsa-fm 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE ce-ctrl THEN 
    DISPLAY ce-ctrl.mat-cost[1] ce-ctrl.mat-pct[1] ce-ctrl.lab-cost[1] 
          ce-ctrl.lab-pct[1] ce-ctrl.mat-cost[2] ce-ctrl.mat-pct[2] 
          ce-ctrl.lab-cost[2] ce-ctrl.lab-pct[2] ce-ctrl.mat-cost[3] 
          ce-ctrl.mat-pct[3] ce-ctrl.lab-cost[3] ce-ctrl.lab-pct[3] 
          ce-ctrl.mat-cost[4] ce-ctrl.mat-pct[4] ce-ctrl.lab-cost[4] 
          ce-ctrl.lab-pct[4] ce-ctrl.mat-cost[5] ce-ctrl.mat-pct[5] 
          ce-ctrl.lab-cost[5] ce-ctrl.lab-pct[5] ce-ctrl.mat-cost[6] 
          ce-ctrl.mat-pct[6] ce-ctrl.lab-cost[6] ce-ctrl.lab-pct[6] 
      WITH FRAME Dialog-Frame.
  ENABLE ld-gsa-brd ld-gsa-mat ld-gsa-lab ld-gsa-war ld-gsa-fm Btn_OK 
         Btn_Cancel RECT-5 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-pcts Dialog-Frame 
PROCEDURE update-pcts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  ASSIGN
   gsa-mat = ld-gsa-mat
   gsa-lab = ld-gsa-lab
   gsa-war = ld-gsa-war
   gsa-fm  = ld-gsa-fm.

  DO TRANSACTION:
    FIND CURRENT calcpcts NO-ERROR.
    calcpcts.val[1] = ld-gsa-brd.
    FIND CURRENT calcpcts NO-LOCK NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-probe Dialog-Frame 
PROCEDURE update-probe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  FIND CURRENT probe NO-ERROR.

  IF AVAIL probe THEN DO:
    ASSIGN
     probe.gsa-mat = ld-gsa-mat
     probe.gsa-lab = ld-gsa-lab
     probe.gsa-war = ld-gsa-war
     probe.gsa-fm = string(ld-gsa-fm).

    FIND FIRST probe-ref
        WHERE probe-ref.reftable EQ "probe-ref"
          AND probe-ref.company  EQ probe.company
          AND probe-ref.loc      EQ ""
          AND probe-ref.code     EQ probe.est-no
          AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
        NO-ERROR.
    IF NOT AVAIL probe-ref THEN DO:
      CREATE probe-ref.
      ASSIGN
       probe-ref.reftable = "probe-ref"
       probe-ref.company  = probe.company
       probe-ref.loc      = ""
       probe-ref.code     = probe.est-no
       probe-ref.code2    = STRING(probe.line,"9999999999").
    END.
    probe-ref.val[1] = ld-gsa-brd.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

