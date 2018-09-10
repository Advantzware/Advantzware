&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: cec\gsa.p
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-qty AS INT NO-UNDO.
DEF INPUT PARAM ip-rels AS INT NO-UNDO.
DEF INPUT PARAM ip-show-update-qty AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAM iop-update-qtys AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAM iop-gsa-brd AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAM iop-gsa-mat AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAM iop-gsa-lab AS DEC NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

{cec/print4.i SHARED SHARED}
{cec/print42.i SHARED}

DEF SHARED BUFFER xest FOR est.
DEF BUFFER probe-ref FOR reftable.

DEF SHARED VAR qty AS INT NO-UNDO.

DEF VAR ld-msf-per AS DEC INIT 1 NO-UNDO.
DEF VAR v-dec AS DEC NO-UNDO.
DEF VAR v-orig-ld-gsa-war AS DEC NO-UNDO.
DEF VAR v-orig-ld-gsa-fm AS DEC NO-UNDO.
DEF VAR v-orig-ld-gsa-war-u-c AS INT NO-UNDO.
DEF VAR v-orig-ld-gsa-war-cnt AS INT NO-UNDO.
DEF VAR v-orig-ld-gsa-war-uni AS INT NO-UNDO.
DEF VAR v-orig-ld-gsa-war-u-p AS INT NO-UNDO.
DEF VAR v-orig-ld-gsa-war-pal AS INT NO-UNDO.
DEF VAR v-orig-ld-gsa-war-amt AS DEC NO-UNDO.
DEF VAR v-orig-ld-gsa-war-hdl AS DEC NO-UNDO.
DEF VAR v-orig-ld-gsa-war-per AS INT NO-UNDO.
DEF VAR v-orig-ld-gsa-war-tot AS INT NO-UNDO.

ASSIGN
 cocode = g_company
 locode = g_loc.

DO TRANSACTION:
  {sys/inc/cewhschg.i}
  {cec/msfcalc.i}
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
&Scoped-Define ENABLED-OBJECTS ld-gsa-brd ld-gsa-mat ld-gsa-lab ~
ld-update-qty ld-gsa-war ld-gsa-fm ld-gsa-war-u-p ld-gsa-war-pal ~
ld-gsa-war-amt ld-gsa-war-hdl ld-gsa-war-per ld-gsa-war-tot Btn_OK ~
Btn_Cancel RECT-5 RECT-9 
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
&Scoped-Define DISPLAYED-OBJECTS lv-head6 lv-head1 ld-qty lv-head16 ~
ld-gsa-brd lv-head2 ld-gsa-mat lv-head3 ld-gsa-lab ld-update-qty lv-head5 ~
ld-gsa-war lv-head-fm ld-gsa-fm lv-head14 ld-gsa-war-u-c lv-head11 ~
ld-gsa-war-cnt lv-head13 ld-gsa-war-uni lv-head12 ld-gsa-war-u-p lv-head7 ~
ld-gsa-war-pal lv-head8 ld-gsa-war-amt lv-head15 ld-gsa-war-hdl lv-head9 ~
ld-gsa-war-per lv-head10 ld-gsa-war-tot 

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

DEFINE VARIABLE ld-gsa-war-amt AS DECIMAL FORMAT ">,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-war-cnt AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-war-hdl AS DECIMAL FORMAT ">,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-war-pal AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-war-per AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-war-tot AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-war-u-c AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-war-u-p AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-gsa-war-uni AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ld-qty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head-fm AS CHARACTER FORMAT "X(256)":U INITIAL "Broker Comm %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head1 AS CHARACTER FORMAT "X(256)":U INITIAL "Overrides for QTY" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head10 AS CHARACTER FORMAT "X(256)":U INITIAL "Total Charge" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head11 AS CHARACTER FORMAT "X(256)":U INITIAL "Pallet Count" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head12 AS CHARACTER FORMAT "X(256)":U INITIAL "Units per Pallet" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head13 AS CHARACTER FORMAT "X(256)":U INITIAL "Total Units" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head14 AS CHARACTER FORMAT "X(256)":U INITIAL "Unit Count" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head15 AS CHARACTER FORMAT "X(256)":U INITIAL "Pallet Handling Charge" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head16 AS CHARACTER FORMAT "X(256)":U INITIAL "GS&A Board %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head2 AS CHARACTER FORMAT "X(256)":U INITIAL "GS&A Material %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head3 AS CHARACTER FORMAT "X(256)":U INITIAL "GS&A Labor %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head5 AS CHARACTER FORMAT "X(256)":U INITIAL "Warehousing Mark Up %" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head6 AS CHARACTER FORMAT "X(256)":U INITIAL "=====  GS&A MARK UP PERCENTAGES  ====" 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head7 AS CHARACTER FORMAT "X(256)":U INITIAL "Total Pallets" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head8 AS CHARACTER FORMAT "X(256)":U INITIAL "Cost per Pallet" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE lv-head9 AS CHARACTER FORMAT "X(256)":U INITIAL "Pallets Delivered/Month" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111 BY 19.38.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 13.38.

DEFINE VARIABLE ld-update-qty AS LOGICAL INITIAL no 
     LABEL "Update Same GSA Values on other Quantities" 
     VIEW-AS TOGGLE-BOX
     SIZE 56 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ce-ctrl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-head6 AT ROW 1.24 COL 3 COLON-ALIGNED NO-LABEL
     ce-ctrl.mat-cost[1] AT ROW 3.14 COL 3 COLON-ALIGNED NO-LABEL
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
     ce-ctrl.mat-cost[2] AT ROW 4.14 COL 3 COLON-ALIGNED NO-LABEL
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
     ce-ctrl.mat-cost[3] AT ROW 5.14 COL 3 COLON-ALIGNED NO-LABEL
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
     ce-ctrl.mat-cost[4] AT ROW 6.14 COL 3 COLON-ALIGNED NO-LABEL
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
     ce-ctrl.mat-cost[5] AT ROW 7.14 COL 3 COLON-ALIGNED NO-LABEL
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
     ce-ctrl.mat-cost[6] AT ROW 8.14 COL 3 COLON-ALIGNED NO-LABEL
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
     lv-head1 AT ROW 1.24 COL 56 COLON-ALIGNED NO-LABEL
     ld-qty AT ROW 1.24 COL 92 COLON-ALIGNED
     lv-head16 AT ROW 2.67 COL 90 RIGHT-ALIGNED NO-LABEL
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
     ld-update-qty AT ROW 5.76 COL 54 WIDGET-ID 6
     lv-head5 AT ROW 8.14 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war AT ROW 8.14 COL 92 COLON-ALIGNED NO-LABEL
     lv-head-fm AT ROW 9.24 COL 90 RIGHT-ALIGNED NO-LABEL WIDGET-ID 2
     ld-gsa-fm AT ROW 9.33 COL 92 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     lv-head14 AT ROW 10.57 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war-u-c AT ROW 10.57 COL 92 COLON-ALIGNED HELP
          "Total Cost of Warehousing this Part#" NO-LABEL
     lv-head11 AT ROW 11.52 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war-cnt AT ROW 11.52 COL 92 COLON-ALIGNED HELP
          "Total Cost of Warehousing this Part#" NO-LABEL
     lv-head13 AT ROW 12.48 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war-uni AT ROW 12.48 COL 92 COLON-ALIGNED HELP
          "Total Cost of Warehousing this Part#" NO-LABEL
     lv-head12 AT ROW 13.67 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war-u-p AT ROW 13.67 COL 92 COLON-ALIGNED HELP
          "Total Cost of Warehousing this Part#" NO-LABEL
     lv-head7 AT ROW 14.62 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war-pal AT ROW 14.62 COL 92 COLON-ALIGNED HELP
          "Total Number of Pallets for this Part#" NO-LABEL
     lv-head8 AT ROW 15.57 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war-amt AT ROW 15.57 COL 92 COLON-ALIGNED HELP
          "Cost per Pallet per Month" NO-LABEL
     lv-head15 AT ROW 16.52 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war-hdl AT ROW 16.52 COL 92 COLON-ALIGNED HELP
          "Cost per Pallet per Month" NO-LABEL
     lv-head9 AT ROW 17.48 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war-per AT ROW 17.48 COL 92 COLON-ALIGNED HELP
          "Number of Pallets Shipped per Month" NO-LABEL
     lv-head10 AT ROW 18.67 COL 90 RIGHT-ALIGNED NO-LABEL
     ld-gsa-war-tot AT ROW 18.67 COL 92 COLON-ALIGNED HELP
          "Total Cost of Warehousing this Part#" NO-LABEL
     Btn_OK AT ROW 20.57 COL 30
     Btn_Cancel AT ROW 20.57 COL 67
     "WAREHOUSING" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 7 COL 57
     "Mat'l%" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 19
     "Cost" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 30
     "Labor%" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 42
     "Cost" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 6
     RECT-5 AT ROW 1 COL 1
     RECT-9 AT ROW 6.76 COL 55
     SPACE(2.39) SKIP(1.90)
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
/* SETTINGS FOR FILL-IN ld-gsa-war-cnt IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-gsa-war-u-c IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-gsa-war-uni IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-qty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-head-fm IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-head10 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head11 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head12 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head13 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head14 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head15 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head16 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head2 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head3 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head5 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-head7 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head8 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN lv-head9 IN FRAME Dialog-Frame
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

  ASSIGN
     iop-update-qtys = ld-update-qty
     iop-gsa-brd = ld-gsa-brd
     iop-gsa-mat = ld-gsa-mat
     iop-gsa-lab = ld-gsa-lab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-gsa-war-amt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-gsa-war-amt Dialog-Frame
ON VALUE-CHANGED OF ld-gsa-war-amt IN FRAME Dialog-Frame
DO:
  RUN calc-war-tot(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-gsa-war-hdl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-gsa-war-hdl Dialog-Frame
ON VALUE-CHANGED OF ld-gsa-war-hdl IN FRAME Dialog-Frame
DO:
  RUN calc-war-tot(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-gsa-war-pal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-gsa-war-pal Dialog-Frame
ON VALUE-CHANGED OF ld-gsa-war-pal IN FRAME Dialog-Frame
DO:
  RUN calc-units-p(INPUT NO).
  RUN calc-count(INPUT NO).
  RUN calc-war-tot(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-gsa-war-per
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-gsa-war-per Dialog-Frame
ON VALUE-CHANGED OF ld-gsa-war-per IN FRAME Dialog-Frame
DO:
  RUN calc-war-tot(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-gsa-war-u-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-gsa-war-u-p Dialog-Frame
ON VALUE-CHANGED OF ld-gsa-war-u-p IN FRAME Dialog-Frame
DO:
  RUN calc-count(INPUT NO).
  RUN calc-pallets(INPUT NO).
  RUN calc-war-tot(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-gsa-war-uni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-gsa-war-uni Dialog-Frame
ON VALUE-CHANGED OF ld-gsa-war-uni IN FRAME Dialog-Frame
DO:
  RUN calc-pallets(INPUT NO).
  RUN calc-war-tot(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-update-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-update-qty Dialog-Frame
ON VALUE-CHANGED OF ld-update-qty IN FRAME Dialog-Frame /* Update Same GSA Values on other Quantities */
DO:
   DO WITH FRAME {&FRAME-NAME}:
   
      ASSIGN
         ld-update-qty   
         ld-gsa-war:SENSITIVE = NOT ld-update-qty
         ld-gsa-fm:SENSITIVE = NOT ld-update-qty
         ld-gsa-war-u-c:SENSITIVE = NOT ld-update-qty
         ld-gsa-war-cnt:SENSITIVE = NOT ld-update-qty
         ld-gsa-war-uni:SENSITIVE = NOT ld-update-qty
         ld-gsa-war-u-p:SENSITIVE = NOT ld-update-qty
         ld-gsa-war-pal:SENSITIVE = NOT ld-update-qty
         ld-gsa-war-amt:SENSITIVE = NOT ld-update-qty
         ld-gsa-war-hdl:SENSITIVE = NOT ld-update-qty
         ld-gsa-war-per:SENSITIVE = NOT ld-update-qty
         ld-gsa-war-tot:SENSITIVE = NOT ld-update-qty.
     
      IF ld-update-qty THEN
         ASSIGN 
            ld-gsa-war:SCREEN-VALUE = STRING(v-orig-ld-gsa-war)
            ld-gsa-fm:SCREEN-VALUE = STRING(v-orig-ld-gsa-fm)
            ld-gsa-war-u-c:SCREEN-VALUE = STRING(v-orig-ld-gsa-war-u-c)
            ld-gsa-war-cnt:SCREEN-VALUE = STRING(v-orig-ld-gsa-war-cnt)
            ld-gsa-war-uni:SCREEN-VALUE = STRING(v-orig-ld-gsa-war-uni)
            ld-gsa-war-u-p:SCREEN-VALUE = STRING(v-orig-ld-gsa-war-u-p)
            ld-gsa-war-pal:SCREEN-VALUE = STRING(v-orig-ld-gsa-war-pal)
            ld-gsa-war-amt:SCREEN-VALUE = STRING(v-orig-ld-gsa-war-amt)
            ld-gsa-war-hdl:SCREEN-VALUE = STRING(v-orig-ld-gsa-war-hdl)
            ld-gsa-war-per:SCREEN-VALUE = STRING(v-orig-ld-gsa-war-per)
            ld-gsa-war-tot:SCREEN-VALUE = STRING(v-orig-ld-gsa-war-tot).
   END.
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
     ld-gsa-brd     = calcpcts.val[1]
     ld-gsa-mat     = gsa-mat
     ld-gsa-lab     = gsa-lab
     ld-gsa-war     = gsa-war
     ld-gsa-fm      = gsa-fm
     ld-gsa-war-tot = ctrl2[1]
     ld-qty         = qty
     v-orig-ld-gsa-war = gsa-war
     v-orig-ld-gsa-fm = gsa-fm
     v-orig-ld-gsa-war-tot = ctrl2[1].

    IF cewhschg-cha BEGINS "$" THEN DO:
       FOR EACH brd
           WHERE (brd.form-no EQ v-form-no OR (NOT vmclean2))
             AND CAN-FIND(FIRST ITEM WHERE item.company  EQ cocode
                                       AND item.i-no     EQ brd.i-no
                                       AND item.mat-type EQ "D"):
      
         li-qty = li-qty + brd.qty.
       END.
      
       FIND FIRST eb
           WHERE eb.company  EQ cocode
             AND eb.est-no   EQ xest.est-no
             AND (eb.form-no EQ v-form-no OR (NOT vmclean2))
           NO-LOCK NO-ERROR.
      
       IF cewhschg-cha EQ "$/MSF" THEN DO:
         lv-head8 = "Cost per MSF".
         IF AVAIL eb THEN DO:
           ld-msf-per = eb.t-len * eb.t-wid * eb.tr-cnt.
           IF v-corr THEN ld-msf-per = ld-msf-per * .007.
                     ELSE ld-msf-per = ld-msf-per / 144.
           ld-msf-per = ld-msf-per / 1000.
         END.
       END.
      
       ASSIGN
        ld-gsa-war-u-c = IF AVAIL eb THEN eb.tr-cnt
                                     ELSE ROUND(qty / li-qty,0)
        ld-gsa-war-u-p = 1
        ld-gsa-war-amt = cewhschg-dec
        ld-gsa-war-hdl = cewhschg-int
        ld-gsa-war-per = li-qty
        v-orig-ld-gsa-war-u-c = IF AVAIL eb THEN eb.tr-cnt
                                ELSE ROUND(qty / li-qty,0)
        v-orig-ld-gsa-war-u-p = 1
        v-orig-ld-gsa-war-amt = cewhschg-dec
        v-orig-ld-gsa-war-hdl = cewhschg-int
        v-orig-ld-gsa-war-per = li-qty.
      
       IF ld-gsa-war-u-c NE 0 THEN
          v-dec = ld-qty / ld-gsa-war-u-c.
      
       {sys/inc/roundup.i v-dec}
      
       ASSIGN
          ld-gsa-war-uni = v-dec
          v-orig-ld-gsa-war-uni = v-dec.
    END.

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
       v-orig-ld-gsa-war = b-probe.gsa-war
       ld-gsa-fm = int(b-probe.gsa-fm)
       v-orig-ld-gsa-fm = int(b-probe.gsa-fm).

      FIND FIRST probe-ref NO-LOCK
          WHERE probe-ref.reftable EQ "probe-ref"
            AND probe-ref.company  EQ b-probe.company
            AND probe-ref.loc      EQ ""
            AND probe-ref.code     EQ b-probe.est-no
            AND probe-ref.code2    EQ STRING(b-probe.line,"9999999999")
          NO-ERROR.
      IF AVAIL probe-ref THEN ld-gsa-brd = probe-ref.val[1].

      IF NOT vprint THEN
        ASSIGN
         ld-gsa-war-amt = b-probe.gsa-war-amt
         ld-gsa-war-hdl = b-probe.gsa-war-hdl
         ld-gsa-war-tot = b-probe.gsa-war-tot
         ld-gsa-war-u-c = b-probe.gsa-war-u-c
         ld-gsa-war-cnt = b-probe.gsa-war-cnt
         ld-gsa-war-uni = b-probe.gsa-war-uni
         ld-gsa-war-pal = b-probe.gsa-war-pal
         ld-gsa-war-per = b-probe.gsa-war-per
         ld-gsa-war-u-p = b-probe.gsa-war-u-p
         v-orig-ld-gsa-war-u-c = b-probe.gsa-war-u-c
         v-orig-ld-gsa-war-cnt = b-probe.gsa-war-cnt
         v-orig-ld-gsa-war-uni = b-probe.gsa-war-uni
         v-orig-ld-gsa-war-u-p = b-probe.gsa-war-u-p
         v-orig-ld-gsa-war-pal = b-probe.gsa-war-pal
         v-orig-ld-gsa-war-amt = b-probe.gsa-war-amt
         v-orig-ld-gsa-war-hdl = b-probe.gsa-war-hdl
         v-orig-ld-gsa-war-per = b-probe.gsa-war-per
         v-orig-ld-gsa-war-tot = b-probe.gsa-war-tot.

      IF b-probe.est-qty GE ip-qty THEN LEAVE.
    END.

    IF vprint THEN
       IF est.override THEN DO:
          RUN enable_UI.
         
          DO WITH FRAME {&FRAME-NAME}:
             IF ip-show-update-qty EQ NO THEN
                ld-update-qty:HIDDEN = YES.

             IF cewhschg-cha BEGINS "$" THEN DO:
                DISABLE ld-gsa-war.
            
                RUN calc-count(INPUT YES).
                RUN calc-pallets(INPUT YES).
                RUN calc-war-tot(INPUT YES).
             END.
            
             ELSE
                DISABLE ld-gsa-war-u-p
                        ld-gsa-war-uni
                        ld-gsa-war-pal
                        ld-gsa-war-amt
                        ld-gsa-war-per
                        ld-gsa-war-tot.
          END.
          
          IF NOT iop-update-qtys THEN
             WAIT-FOR GO OF FRAME {&FRAME-NAME}.
          ELSE
          DO:
             ASSIGN
                gsa-mat  = iop-gsa-mat
                gsa-lab  = iop-gsa-lab
                ld-gsa-brd = iop-gsa-brd
                gsa-war  = v-orig-ld-gsa-war
                gsa-fm   = v-orig-ld-gsa-fm
                ctrl2[1] = v-orig-ld-gsa-war-tot.

             FIND CURRENT calcpcts NO-ERROR.
             calcpcts.val[1] = iop-gsa-brd.
             FIND CURRENT calcpcts NO-LOCK NO-ERROR.
  
             FIND CURRENT probe NO-ERROR.

             IF AVAIL probe THEN DO:
                ASSIGN
                 probe.gsa-mat      = iop-gsa-mat
                 probe.gsa-lab      = iop-gsa-lab
                 probe.gsa-war      = v-orig-ld-gsa-war
                 probe.gsa-war-amt  = v-orig-ld-gsa-war-amt
                 probe.gsa-war-hdl  = v-orig-ld-gsa-war-hdl
                 probe.gsa-war-tot  = v-orig-ld-gsa-war-tot
                 probe.gsa-war-u-c  = v-orig-ld-gsa-war-u-c
                 probe.gsa-war-cnt  = v-orig-ld-gsa-war-cnt
                 probe.gsa-war-uni  = v-orig-ld-gsa-war-uni
                 probe.gsa-war-pal  = v-orig-ld-gsa-war-pal
                 probe.gsa-war-per  = v-orig-ld-gsa-war-per
                 probe.gsa-war-u-p  = v-orig-ld-gsa-war-u-p
                 probe.gsa-fm       = string(v-orig-ld-gsa-fm).
             
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
             
                probe-ref.val[1] = iop-gsa-brd.             
                
              END.
          END.
       END.
      
      ELSE RUN update-probe.

    ELSE RUN update-pcts.
  END.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-count Dialog-Frame 
PROCEDURE calc-count :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-main-block AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
    ld-gsa-war-cnt:SCREEN-VALUE = STRING(DEC(ld-gsa-war-u-c:SCREEN-VALUE) *
                                         DEC(ld-gsa-war-u-p:SCREEN-VALUE)).

    IF ip-main-block THEN
       v-orig-ld-gsa-war-cnt = DEC(ld-gsa-war-u-c:SCREEN-VALUE) *
                               DEC(ld-gsa-war-u-p:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-pallets Dialog-Frame 
PROCEDURE calc-pallets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-main-block AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ld-gsa-war-pal:SCREEN-VALUE = STRING(DEC(ld-gsa-war-uni:SCREEN-VALUE) /
                                         DEC(ld-gsa-war-u-p:SCREEN-VALUE)).

    IF ip-main-block THEN
       v-orig-ld-gsa-war-pal = DEC(ld-gsa-war-uni:SCREEN-VALUE) /
                               DEC(ld-gsa-war-u-p:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-units-p Dialog-Frame 
PROCEDURE calc-units-p :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-main-block AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ld-gsa-war-u-p:SCREEN-VALUE = STRING(DEC(ld-gsa-war-uni:SCREEN-VALUE) /
                                         DEC(ld-gsa-war-pal:SCREEN-VALUE)).

   IF ip-main-block THEN
      v-orig-ld-gsa-war-u-p = DEC(ld-gsa-war-uni:SCREEN-VALUE) /
                              DEC(ld-gsa-war-pal:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-war-tot Dialog-Frame 
PROCEDURE calc-war-tot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-main-block AS LOG NO-UNDO.
    
  DEF VAR li-qty AS INT NO-UNDO.
  DEF VAR ld-tot AS DEC NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     li-qty = INT(ld-gsa-war-pal:SCREEN-VALUE)
     ld-tot = li-qty * DEC(ld-gsa-war-hdl:SCREEN-VALUE)
     li-qty = li-qty - INT(ld-gsa-war-per:SCREEN-VALUE).

    DO WHILE li-qty GT 0 AND INT(ld-gsa-war-per:SCREEN-VALUE) GT 0:
      ASSIGN
       ld-tot = ld-tot + (li-qty * DEC(ld-gsa-war-amt:SCREEN-VALUE))
       li-qty = li-qty - INT(ld-gsa-war-per:SCREEN-VALUE).
    END.
        
    IF cewhschg-cha EQ "$/MSF" THEN ld-tot = ld-tot * ld-msf-per.

    ld-gsa-war-tot:SCREEN-VALUE = STRING(ROUND(ld-tot,2)).

    IF ip-main-block THEN
       v-orig-ld-gsa-war-tot = ROUND(ld-tot,2).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY lv-head6 lv-head1 ld-qty lv-head16 ld-gsa-brd lv-head2 ld-gsa-mat 
          lv-head3 ld-gsa-lab ld-update-qty lv-head5 ld-gsa-war lv-head-fm 
          ld-gsa-fm lv-head14 ld-gsa-war-u-c lv-head11 ld-gsa-war-cnt lv-head13 
          ld-gsa-war-uni lv-head12 ld-gsa-war-u-p lv-head7 ld-gsa-war-pal 
          lv-head8 ld-gsa-war-amt lv-head15 ld-gsa-war-hdl lv-head9 
          ld-gsa-war-per lv-head10 ld-gsa-war-tot 
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
  ENABLE ld-gsa-brd ld-gsa-mat ld-gsa-lab ld-update-qty ld-gsa-war ld-gsa-fm 
         ld-gsa-war-u-p ld-gsa-war-pal ld-gsa-war-amt ld-gsa-war-hdl 
         ld-gsa-war-per ld-gsa-war-tot Btn_OK Btn_Cancel RECT-5 RECT-9 
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
   gsa-mat  = ld-gsa-mat
   gsa-lab  = ld-gsa-lab
   gsa-war  = ld-gsa-war
   gsa-fm   = ld-gsa-fm
   ctrl2[1] = ld-gsa-war-tot.

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
     probe.gsa-mat      = ld-gsa-mat
     probe.gsa-lab      = ld-gsa-lab
     probe.gsa-war      = ld-gsa-war
     probe.gsa-war-amt  = ld-gsa-war-amt
     probe.gsa-war-hdl  = ld-gsa-war-hdl
     probe.gsa-war-tot  = ld-gsa-war-tot
     probe.gsa-war-u-c  = ld-gsa-war-u-c
     probe.gsa-war-cnt  = ld-gsa-war-cnt
     probe.gsa-war-uni  = ld-gsa-war-uni
     probe.gsa-war-pal  = ld-gsa-war-pal
     probe.gsa-war-per  = ld-gsa-war-per
     probe.gsa-war-u-p  = ld-gsa-war-u-p
     probe.gsa-fm       = string(ld-gsa-fm).

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

