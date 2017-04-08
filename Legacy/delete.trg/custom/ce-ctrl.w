&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

def var lv-sell-by-list as cha init "NG" no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ce-ctrl

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME ce-ctrl.e-num ~
ce-ctrl.e-range[1] ce-ctrl.e-range[2] ce-ctrl.q-num ce-ctrl.q-range[1] ~
ce-ctrl.q-range[2] ce-ctrl.ls-width ce-ctrl.ls-triml ce-ctrl.fg-rate ~
ce-ctrl.ls-length ce-ctrl.ls-trimw ce-ctrl.rm-rate ce-ctrl.hand-pct ~
ce-ctrl.def-ink ce-ctrl.def-inkcov ce-ctrl.whse-mrkup ce-ctrl.def-case ~
ce-ctrl.def-pal ce-ctrl.spec-l[1] ce-ctrl.spec-%[1] ce-ctrl.def-coat ~
ce-ctrl.comm-mrkup ce-ctrl.spec-l[2] ce-ctrl.spec-%[2] ce-ctrl.r-cost ~
ce-ctrl.spec-l[3] ce-ctrl.spec-%[3] ce-ctrl.hd-net ce-ctrl.mat-cost[1] ~
ce-ctrl.mat-pct[1] ce-ctrl.lab-cost[1] ce-ctrl.lab-pct[1] ce-ctrl.hd-gross ~
ce-ctrl.mat-cost[2] ce-ctrl.mat-pct[2] ce-ctrl.lab-cost[2] ~
ce-ctrl.lab-pct[2] ce-ctrl.sell-by ce-ctrl.mat-cost[3] ce-ctrl.mat-pct[3] ~
ce-ctrl.lab-cost[3] ce-ctrl.lab-pct[3] ce-ctrl.prof-mrkup ~
ce-ctrl.mat-cost[4] ce-ctrl.mat-pct[4] ce-ctrl.lab-cost[4] ~
ce-ctrl.lab-pct[4] ce-ctrl.comm-add ce-ctrl.sho-labor ce-ctrl.mat-cost[5] ~
ce-ctrl.mat-pct[5] ce-ctrl.lab-cost[5] ce-ctrl.lab-pct[5] ce-ctrl.shp-add ~
ce-ctrl.spec-add[1] ce-ctrl.spec-add[2] ce-ctrl.spec-add[3] ~
ce-ctrl.mat-cost[6] ce-ctrl.mat-pct[6] ce-ctrl.lab-cost[6] ~
ce-ctrl.lab-pct[6] ce-ctrl.spec-add[6] ce-ctrl.spec-add[7] ~
ce-ctrl.spec-add[8] 
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ce-ctrl SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ce-ctrl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ce-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ce-ctrl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 RECT-21 RECT-20 RECT-23 Btn_Update ~
Btn_Close 
&Scoped-Define DISPLAYED-FIELDS ce-ctrl.e-num ce-ctrl.e-range[1] ~
ce-ctrl.e-range[2] ce-ctrl.q-num ce-ctrl.q-range[1] ce-ctrl.q-range[2] ~
ce-ctrl.ls-width ce-ctrl.ls-triml ce-ctrl.fg-rate ce-ctrl.ls-length ~
ce-ctrl.ls-trimw ce-ctrl.rm-rate ce-ctrl.hand-pct ce-ctrl.def-ink ~
ce-ctrl.def-inkcov ce-ctrl.whse-mrkup ce-ctrl.def-case ce-ctrl.def-pal ~
ce-ctrl.spec-l[1] ce-ctrl.spec-%[1] ce-ctrl.def-coat ce-ctrl.comm-mrkup ~
ce-ctrl.spec-l[2] ce-ctrl.spec-%[2] ce-ctrl.r-cost ce-ctrl.spec-l[3] ~
ce-ctrl.spec-%[3] ce-ctrl.hd-net ce-ctrl.mat-cost[1] ce-ctrl.mat-pct[1] ~
ce-ctrl.lab-cost[1] ce-ctrl.lab-pct[1] ce-ctrl.hd-gross ce-ctrl.mat-cost[2] ~
ce-ctrl.mat-pct[2] ce-ctrl.lab-cost[2] ce-ctrl.lab-pct[2] ce-ctrl.sell-by ~
ce-ctrl.mat-cost[3] ce-ctrl.mat-pct[3] ce-ctrl.lab-cost[3] ~
ce-ctrl.lab-pct[3] ce-ctrl.prof-mrkup ce-ctrl.mat-cost[4] ~
ce-ctrl.mat-pct[4] ce-ctrl.lab-cost[4] ce-ctrl.lab-pct[4] ce-ctrl.comm-add ~
ce-ctrl.sho-labor ce-ctrl.mat-cost[5] ce-ctrl.mat-pct[5] ~
ce-ctrl.lab-cost[5] ce-ctrl.lab-pct[5] ce-ctrl.shp-add ce-ctrl.spec-add[1] ~
ce-ctrl.spec-add[2] ce-ctrl.spec-add[3] ce-ctrl.mat-cost[6] ~
ce-ctrl.mat-pct[6] ce-ctrl.lab-cost[6] ce-ctrl.lab-pct[6] ~
ce-ctrl.spec-add[6] ce-ctrl.spec-add[7] ce-ctrl.spec-add[8] 
&Scoped-define DISPLAYED-TABLES ce-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE ce-ctrl
&Scoped-Define DISPLAYED-OBJECTS avg_cost rd-sp-1 rd-sp-2 rd-sp-3 ~
ls-mtx-title ls-title1 ls-title2 ls-title3 ls-title4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 ce-ctrl.e-num ce-ctrl.e-range[1] ce-ctrl.e-range[2] ~
ce-ctrl.q-num ce-ctrl.q-range[1] ce-ctrl.q-range[2] ce-ctrl.ls-width ~
ce-ctrl.ls-triml ce-ctrl.fg-rate ce-ctrl.ls-length ce-ctrl.ls-trimw ~
ce-ctrl.rm-rate avg_cost ce-ctrl.hand-pct ce-ctrl.def-ink ~
ce-ctrl.def-inkcov ce-ctrl.whse-mrkup ce-ctrl.def-case ce-ctrl.def-pal ~
ce-ctrl.spec-l[1] ce-ctrl.spec-%[1] ce-ctrl.def-coat ce-ctrl.comm-mrkup ~
ce-ctrl.spec-l[2] ce-ctrl.spec-%[2] ce-ctrl.r-cost ce-ctrl.spec-l[3] ~
ce-ctrl.spec-%[3] ce-ctrl.hd-net ce-ctrl.mat-cost[1] ce-ctrl.mat-pct[1] ~
ce-ctrl.lab-cost[1] ce-ctrl.lab-pct[1] ce-ctrl.hd-gross ce-ctrl.mat-cost[2] ~
ce-ctrl.mat-pct[2] ce-ctrl.lab-cost[2] ce-ctrl.lab-pct[2] ce-ctrl.sell-by ~
ce-ctrl.mat-cost[3] ce-ctrl.mat-pct[3] ce-ctrl.lab-cost[3] ~
ce-ctrl.lab-pct[3] ce-ctrl.prof-mrkup ce-ctrl.mat-cost[4] ~
ce-ctrl.mat-pct[4] ce-ctrl.lab-cost[4] ce-ctrl.lab-pct[4] ce-ctrl.comm-add ~
ce-ctrl.sho-labor ce-ctrl.mat-cost[5] ce-ctrl.mat-pct[5] ~
ce-ctrl.lab-cost[5] ce-ctrl.lab-pct[5] ce-ctrl.shp-add ce-ctrl.spec-add[1] ~
ce-ctrl.spec-add[2] ce-ctrl.spec-add[3] ce-ctrl.mat-cost[6] ~
ce-ctrl.mat-pct[6] ce-ctrl.lab-cost[6] ce-ctrl.lab-pct[6] ~
ce-ctrl.spec-add[6] ce-ctrl.spec-add[7] ce-ctrl.spec-add[8] 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE ls-mtx-title AS CHARACTER FORMAT "X(256)":U INITIAL "GS&&A MARK UP PERCENTAGES" 
      VIEW-AS TEXT 
     SIZE 39 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ls-title1 AS CHARACTER FORMAT "X(256)":U INITIAL "Cost" 
      VIEW-AS TEXT 
     SIZE 8 BY .62 NO-UNDO.

DEFINE VARIABLE ls-title2 AS CHARACTER FORMAT "X(256)":U INITIAL "Mat'l%" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE ls-title3 AS CHARACTER FORMAT "X(256)":U INITIAL "Cost" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE ls-title4 AS CHARACTER FORMAT "X(256)":U INITIAL "Labor%" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE avg_cost AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Roll Feed", yes,
"Sheet Feed", no
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sp-1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "%", 1,
"$", 2
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sp-2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "%", 1,
"$", 2
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sp-3 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "%", 1,
"$", 2
     SIZE 12 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.67.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 8.81.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 8.33.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 8.81.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 8.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ce-ctrl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ce-ctrl.e-num AT ROW 1.24 COL 22 COLON-ALIGNED
          LABEL "Last Estimate Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.e-range[1] AT ROW 1.24 COL 43 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.e-range[2] AT ROW 1.24 COL 58 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.q-num AT ROW 2.43 COL 22 COLON-ALIGNED
          LABEL "Last Quote Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.q-range[1] AT ROW 2.43 COL 43 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.q-range[2] AT ROW 2.43 COL 58 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.ls-width AT ROW 4.33 COL 22 COLON-ALIGNED
          LABEL "Machine Front-Back"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
          BGCOLOR 15 
     ce-ctrl.ls-triml AT ROW 4.33 COL 43 COLON-ALIGNED
          LABEL "Trim"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.fg-rate AT ROW 4.33 COL 86.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.ls-length AT ROW 5.52 COL 22 COLON-ALIGNED
          LABEL "Machine Side-Side"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
          BGCOLOR 15 
     ce-ctrl.ls-trimw AT ROW 5.52 COL 43 COLON-ALIGNED
          LABEL "Trim"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.rm-rate AT ROW 5.52 COL 86.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     avg_cost AT ROW 6.71 COL 24 HELP
          "Select Press Feed Type" NO-LABEL
     ce-ctrl.hand-pct AT ROW 6.71 COL 86.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.def-ink AT ROW 7.91 COL 9 COLON-ALIGNED
          LABEL "Ink #"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 
     ce-ctrl.def-inkcov AT ROW 7.91 COL 38 COLON-ALIGNED
          LABEL "Coverage %"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 
     ce-ctrl.whse-mrkup AT ROW 7.91 COL 86.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.def-case AT ROW 9.1 COL 9 COLON-ALIGNED
          LABEL "Case #"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 
     ce-ctrl.def-pal AT ROW 9.1 COL 38 COLON-ALIGNED
          LABEL "Pallet #"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 
     ce-ctrl.spec-l[1] AT ROW 9.1 COL 56 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
          BGCOLOR 15 
     ce-ctrl.spec-%[1] AT ROW 9.1 COL 86.6 COLON-ALIGNED
          LABEL ""
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     rd-sp-1 AT ROW 9.1 COL 101 NO-LABEL
     ce-ctrl.def-coat AT ROW 10.29 COL 9 COLON-ALIGNED
          LABEL "Coating"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.6 BY 22.43.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     ce-ctrl.comm-mrkup AT ROW 10.29 COL 38 COLON-ALIGNED
          LABEL "Comm. Rate"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.spec-l[2] AT ROW 10.29 COL 56 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
          BGCOLOR 15 
     ce-ctrl.spec-%[2] AT ROW 10.29 COL 86.6 COLON-ALIGNED
          LABEL ""
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     rd-sp-2 AT ROW 10.29 COL 101 NO-LABEL
     ce-ctrl.r-cost AT ROW 11.48 COL 38 COLON-ALIGNED
          LABEL "For REAL Items Cost, use"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     ce-ctrl.spec-l[3] AT ROW 11.48 COL 56 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
          BGCOLOR 15 
     ce-ctrl.spec-%[3] AT ROW 11.48 COL 86.6 COLON-ALIGNED
          LABEL ""
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     rd-sp-3 AT ROW 11.48 COL 101 NO-LABEL
     ce-ctrl.hd-net AT ROW 13.38 COL 85.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-cost[1] AT ROW 14.1 COL 1 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-pct[1] AT ROW 14.1 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-cost[1] AT ROW 14.1 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-pct[1] AT ROW 14.1 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.hd-gross AT ROW 14.57 COL 85.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-cost[2] AT ROW 15.29 COL 1 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-pct[2] AT ROW 15.29 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-cost[2] AT ROW 15.29 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-pct[2] AT ROW 15.29 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.sell-by AT ROW 15.76 COL 93 COLON-ALIGNED
          LABEL "Calculate Sell Price on Net or Gross"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-cost[3] AT ROW 16.48 COL 1 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-pct[3] AT ROW 16.48 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-cost[3] AT ROW 16.48 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.6 BY 22.43.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     ce-ctrl.lab-pct[3] AT ROW 16.48 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.prof-mrkup AT ROW 16.95 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-cost[4] AT ROW 17.67 COL 1 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-pct[4] AT ROW 17.67 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-cost[4] AT ROW 17.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-pct[4] AT ROW 17.67 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.comm-add AT ROW 18.14 COL 58
          LABEL "Show Commissions"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .81
     ce-ctrl.sho-labor AT ROW 18.14 COL 82
          VIEW-AS TOGGLE-BOX
          SIZE 16.6 BY .81
     ce-ctrl.mat-cost[5] AT ROW 18.86 COL 1 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-pct[5] AT ROW 18.86 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-cost[5] AT ROW 18.86 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-pct[5] AT ROW 18.86 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.shp-add AT ROW 19.33 COL 79
          LABEL "Frt"
          VIEW-AS TOGGLE-BOX
          SIZE 8 BY .81
     ce-ctrl.spec-add[1] AT ROW 19.33 COL 90
          LABEL "S1"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ce-ctrl.spec-add[2] AT ROW 19.33 COL 98
          LABEL "S2"
          VIEW-AS TOGGLE-BOX
          SIZE 7 BY .81
     ce-ctrl.spec-add[3] AT ROW 19.33 COL 107
          LABEL "S3"
          VIEW-AS TOGGLE-BOX
          SIZE 7 BY .81
     ce-ctrl.mat-cost[6] AT ROW 20.05 COL 1 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.mat-pct[6] AT ROW 20.05 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-cost[6] AT ROW 20.05 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     ce-ctrl.lab-pct[6] AT ROW 20.05 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ce-ctrl.spec-add[6] AT ROW 20.29 COL 79
          LABEL "GSA"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
     ce-ctrl.spec-add[7] AT ROW 20.29 COL 90
          LABEL "Comm"
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .81
     ce-ctrl.spec-add[8] AT ROW 20.29 COL 103
          LABEL "Royal"
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.6 BY 22.43.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     Btn_Update AT ROW 21.95 COL 69 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 21.95 COL 85 HELP
          "Cancel Update or Close Window"
     ls-mtx-title AT ROW 12.67 COL 3 COLON-ALIGNED NO-LABEL
     ls-title1 AT ROW 13.38 COL 4 COLON-ALIGNED NO-LABEL
     ls-title2 AT ROW 13.38 COL 15 COLON-ALIGNED NO-LABEL
     ls-title3 AT ROW 13.38 COL 34 COLON-ALIGNED NO-LABEL
     ls-title4 AT ROW 13.38 COL 44 COLON-ALIGNED NO-LABEL
     "What If/Print Options" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 12.67 COL 59
          FONT 6
     "Mark Up Options" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 3.62 COL 59
          FONT 6
     "Estimating Defaults" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 3.62 COL 4
          FONT 6
     "Press Feed Type:" VIEW-AS TEXT
          SIZE 16.6 BY 1 AT ROW 6.71 COL 7
     "Add to Fact. Costs" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 19.33 COL 58
     RECT-22 AT ROW 3.86 COL 57
     RECT-21 AT ROW 12.91 COL 2
     RECT-20 AT ROW 3.86 COL 2
     RECT-15 AT ROW 21.71 COL 68
     RECT-23 AT ROW 12.91 COL 57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.6 BY 22.43.


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
         TITLE              = "Cost Estimating Control"
         HEIGHT             = 22.52
         WIDTH              = 124.6
         MAX-HEIGHT         = 22.67
         MAX-WIDTH          = 124.6
         VIRTUAL-HEIGHT     = 22.67
         VIRTUAL-WIDTH      = 124.6
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR RADIO-SET avg_cost IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       Btn_Close:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       Btn_Update:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

/* SETTINGS FOR TOGGLE-BOX ce-ctrl.comm-add IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.comm-mrkup IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.def-case IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.def-coat IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.def-ink IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.def-inkcov IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.def-pal IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.e-num IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.e-range[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.e-range[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.fg-rate IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.hand-pct IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.hd-gross IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.hd-net IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[3] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[4] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[5] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-cost[6] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[3] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[4] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[5] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.lab-pct[6] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.ls-length IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ls-mtx-title IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-title1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-title2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-title3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-title4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.ls-triml IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.ls-trimw IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.ls-width IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[3] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[4] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[5] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-cost[6] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[3] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[4] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[5] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.mat-pct[6] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.prof-mrkup IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.q-num IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.q-range[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.q-range[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.r-cost IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR RADIO-SET rd-sp-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd-sp-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd-sp-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-15 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ce-ctrl.rm-rate IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.sell-by IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX ce-ctrl.sho-labor IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX ce-ctrl.shp-add IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.spec-%[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.spec-%[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.spec-%[3] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX ce-ctrl.spec-add[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX ce-ctrl.spec-add[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX ce-ctrl.spec-add[3] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX ce-ctrl.spec-add[6] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX ce-ctrl.spec-add[7] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX ce-ctrl.spec-add[8] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ce-ctrl.spec-l[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.spec-l[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.spec-l[3] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ce-ctrl.whse-mrkup IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "ASI.ce-ctrl"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cost Estimating Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cost Estimating Control */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON HELP OF FRAME DEFAULT-FRAME
DO:
     def var char-val as cha no-undo.

     case focus:name :
          when 'def-ink' then do:
               run windows/l-item.w (gcompany, "", "I", focus:screen-value, output char-val).
               if char-val <> "" then 
                  assign focus:screen-value = entry(1,char-val).
          end.
          when 'def-case' then do:
               run windows/l-item.w (gcompany, "", "C", focus:screen-value, output char-val).
               if char-val <> "" then 
                  assign focus:screen-value = entry(1,char-val).
          end.
          when 'def-pal' then do:
               run windows/l-item.w (gcompany, "", "D", focus:screen-value, output char-val).
               if char-val <> "" then 
                  assign focus:screen-value = entry(1,char-val).
          end.
          when 'def-coat' then do:
               run windows/l-item.w (gcompany, "", "I,V", focus:screen-value, output char-val).
               if char-val <> "" then 
                  assign focus:screen-value = entry(1,char-val).
          end.


     end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME avg_cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL avg_cost C-Win
ON VALUE-CHANGED OF avg_cost IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}
    ce-ctrl.avg-cscost = IF {&SELF-NAME} THEN 1 ELSE 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* Close */
DO:
  IF {&SELF-NAME}:LABEL = "&Close" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:  
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    RUN enable_UI.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update C-Win
ON CHOOSE OF Btn_Update IN FRAME DEFAULT-FRAME /* Update */
DO:
  IF {&SELF-NAME}:LABEL = "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&LIST-1}.
    {methods/setButton.i Btn_Update "Save"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Close "Cancel"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    APPLY "ENTRY" TO ce-ctrl.e-num.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:

    run validate no-error.
    if error-status:error then return no-apply.

    DISABLE {&LIST-1}.
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    find current ce-ctrl exclusive-lock.  
    ASSIGN {&LIST-1}.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ce-ctrl.sell-by
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ce-ctrl.sell-by C-Win
ON LEAVE OF ce-ctrl.sell-by IN FRAME DEFAULT-FRAME /* Calculate Sell Price on Net or Gross */
DO:

    if lastkey <> -1 and self:screen-value <> "" and
       index(lv-sell-by-list, self:screen-value) = 0 then do:
          if index(lv-sell-by-list,"S") > 0 then 
             message "Must be 'N'et, 'G'ross, 'S'quare-Feet, or 'B'oard."
                     view-as alert-box error.
          else message "Must be 'N'et or 'G'ross." view-as alert-box error.

          return no-apply.           
    end.

    if index("SB",ce-ctrl.sell-by:screen-value) > 0 then 
        assign ls-mtx-title = "MSF SELL PRICE MATRIX"
               ls-title1 = " MSF"
               ls-title2 = "Markup%"
               ls-title3 = " MSF"
               ls-title4 = "Markup%".        
    else assign ls-mtx-title = "GS&&A MARK UP PERCENTAGES"
               ls-title1 = "Cost"
               ls-title2 = "Mat'l%" 
               ls-title3 = "Cost"
               ls-title4 = "Labor%" .        

    DISPLAY ls-mtx-title ls-title1 ls-title2 ls-title3 ls-title4
            WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ce-ctrl.spec-%[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ce-ctrl.spec-%[1] C-Win
ON LEAVE OF ce-ctrl.spec-%[1] IN FRAME DEFAULT-FRAME
DO:
    if dec(self:screen-value) >= 1 then rd-sp-1:screen-value = "2".
    else rd-sp-1:screen-value = "1".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ce-ctrl.spec-%[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ce-ctrl.spec-%[2] C-Win
ON LEAVE OF ce-ctrl.spec-%[2] IN FRAME DEFAULT-FRAME
DO:
      if dec(self:screen-value) >= 1 then rd-sp-2:screen-value = "2".
    else rd-sp-2:screen-value = "1".


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ce-ctrl.spec-%[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ce-ctrl.spec-%[3] C-Win
ON LEAVE OF ce-ctrl.spec-%[3] IN FRAME DEFAULT-FRAME
DO:
      if dec(self:screen-value) >= 1 then rd-sp-3:screen-value = "2".
    else rd-sp-3:screen-value = "1".


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
session:data-entry-return = yes.
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  find first sys-ctrl where sys-ctrl.company = gcompany and
                            sys-ctrl.name = "CEMENU" no-lock no-error.
  if not avail sys-ctrl or 
     (avail sys-ctrl and sys-ctrl.char-fld = "Corrware")
  then lv-sell-by-list = lv-sell-by-list + "SB".

  FIND ce-ctrl WHERE ce-ctrl.company = gcompany
                 AND ce-ctrl.loc = gloc NO-LOCK NO-ERROR.

  RUN enable_UI.
  IF AVAILABLE ce-ctrl THEN do:
     assign  avg_cost = IF ce-ctrl.avg-cscost = 1 THEN yes ELSE no
             .
     if index("SB",ce-ctrl.sell-by) > 0 then 
        assign ls-mtx-title = "MSF SELL PRICE MATRIX"
               ls-title1 = " MSF"
               ls-title2 = "Markup%"
               ls-title3 = " MSF"
               ls-title4 = "Markup%".        
     rd-sp-1 = if ce-ctrl.spec-%[1] >= 1 then 2 /* $ */ else 1 /* % */.
     rd-sp-2 = if ce-ctrl.spec-%[2] >= 1 then 2 else 1.
     rd-sp-3 = if ce-ctrl.spec-%[3] >= 1 then 2 else 1.

  end.           
  DISPLAY avg_cost ls-mtx-title ls-title1 ls-title2 ls-title3 ls-title4
          rd-sp-1 rd-sp-2 rd-sp-3
          WITH FRAME {&FRAME-NAME}.
  find last quote use-index qno no-lock no-error.
  if avail quote then ce-ctrl.q-num:screen-value in frame {&frame-name} = string(quote.q-no).


  {methods/nowait.i}
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
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
  DISPLAY avg_cost rd-sp-1 rd-sp-2 rd-sp-3 ls-mtx-title ls-title1 ls-title2 
          ls-title3 ls-title4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE ce-ctrl THEN 
    DISPLAY ce-ctrl.e-num ce-ctrl.e-range[1] ce-ctrl.e-range[2] ce-ctrl.q-num 
          ce-ctrl.q-range[1] ce-ctrl.q-range[2] ce-ctrl.ls-width 
          ce-ctrl.ls-triml ce-ctrl.fg-rate ce-ctrl.ls-length ce-ctrl.ls-trimw 
          ce-ctrl.rm-rate ce-ctrl.hand-pct ce-ctrl.def-ink ce-ctrl.def-inkcov 
          ce-ctrl.whse-mrkup ce-ctrl.def-case ce-ctrl.def-pal ce-ctrl.spec-l[1] 
          ce-ctrl.spec-%[1] ce-ctrl.def-coat ce-ctrl.comm-mrkup 
          ce-ctrl.spec-l[2] ce-ctrl.spec-%[2] ce-ctrl.r-cost ce-ctrl.spec-l[3] 
          ce-ctrl.spec-%[3] ce-ctrl.hd-net ce-ctrl.mat-cost[1] 
          ce-ctrl.mat-pct[1] ce-ctrl.lab-cost[1] ce-ctrl.lab-pct[1] 
          ce-ctrl.hd-gross ce-ctrl.mat-cost[2] ce-ctrl.mat-pct[2] 
          ce-ctrl.lab-cost[2] ce-ctrl.lab-pct[2] ce-ctrl.sell-by 
          ce-ctrl.mat-cost[3] ce-ctrl.mat-pct[3] ce-ctrl.lab-cost[3] 
          ce-ctrl.lab-pct[3] ce-ctrl.prof-mrkup ce-ctrl.mat-cost[4] 
          ce-ctrl.mat-pct[4] ce-ctrl.lab-cost[4] ce-ctrl.lab-pct[4] 
          ce-ctrl.comm-add ce-ctrl.sho-labor ce-ctrl.mat-cost[5] 
          ce-ctrl.mat-pct[5] ce-ctrl.lab-cost[5] ce-ctrl.lab-pct[5] 
          ce-ctrl.shp-add ce-ctrl.spec-add[1] ce-ctrl.spec-add[2] 
          ce-ctrl.spec-add[3] ce-ctrl.mat-cost[6] ce-ctrl.mat-pct[6] 
          ce-ctrl.lab-cost[6] ce-ctrl.lab-pct[6] ce-ctrl.spec-add[6] 
          ce-ctrl.spec-add[7] ce-ctrl.spec-add[8] 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-22 RECT-21 RECT-20 RECT-23 Btn_Update Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate C-Win 
PROCEDURE validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 do with frame {&frame-name}:


    if ce-ctrl.sell-by:screen-value <> "" and
       index(lv-sell-by-list, ce-ctrl.sell-by:screen-value) = 0 then do:
          if index(lv-sell-by-list,"S") > 0 then 
             message "Must be 'N'et, 'G'ross, 'S'quare-Feet, or 'B'oard."
                     view-as alert-box error.
          else message "Must be 'N'et or 'G'ross." view-as alert-box error.
          apply "entry" to ce-ctrl.sell-by.
          return error.           
    end.
 end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

