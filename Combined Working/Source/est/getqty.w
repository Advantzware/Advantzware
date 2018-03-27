&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est\getqty.w
  
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
def input-output parameter io-do-speed as log no-undo.
def input-output parameter io-do-mr as log no-undo.
def input-output parameter io-do-gsa as log no-undo.
def input-output parameter io-v-drop-rc as log no-undo.
def input-output parameter io-v-match-up as dec no-undo.
def input-output parameter io-ink-all-forms AS LOG NO-UNDO.
def input-output parameter io-board-cost-from-blank AS LOG NO-UNDO.
def input parameter lv-mclean as log no-undo.
def output parameter op-error as log no-undo.

{cec/print4.i shared shared}
{cec/print42.i shared}

def shared buffer xest for est.

def shared temp-table tt-qtty field qtty like qtty
                              field rel like rels.

def TEMP-TABLE w-est NO-UNDO field w-est-no like est.est-no
                   field w-row-id as   rowid.

def var i as int no-undo.
def var li-seq as int no-undo.

def shared var cocode as cha no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-qty-1 lv-rels-1 lv-qty-2 lv-rels-2 ~
lv-qty-3 lv-rels-3 lv-qty-4 lv-rels-4 lv-qty-5 lv-rels-5 lv-qty-6 lv-rels-6 ~
lv-qty-7 lv-rels-7 lv-qty-8 lv-rels-8 lv-qty-9 lv-rels-9 lv-qty-10 ~
lv-rels-10 lv-qty-11 lv-rels-11 lv-qty-12 lv-rels-12 lv-qty-13 lv-rels-13 ~
lv-qty-14 lv-rels-14 lv-qty-15 lv-rels-15 lv-qty-16 lv-rels-16 lv-qty-17 ~
lv-rels-17 lv-qty-18 lv-rels-18 lv-qty-19 lv-rels-19 lv-qty-20 lv-rels-20 ~
lv-qty-21 lv-rels-21 lv-qty-22 lv-rels-22 lv-qty-23 lv-rels-23 lv-qty-24 ~
lv-rels-24 lv-qty-25 lv-rels-25 lv-qty-26 lv-rels-26 lv-qty-27 lv-rels-27 ~
lv-qty-28 lv-rels-28 btn-clear btn-upd v-do-speed v-do-mr v-do-gsa ~
v-drop-rc v-ink-all-forms lv-match-up v-est-list Btn_OK Btn_Cancel ~
v-calc-board-cost-on-blank RECT-1 RECT-23 RECT-24 
&Scoped-Define DISPLAYED-OBJECTS lv-qty-1 lv-rels-1 lv-qty-2 lv-rels-2 ~
lv-qty-3 lv-rels-3 lv-qty-4 lv-rels-4 lv-qty-5 lv-rels-5 lv-qty-6 lv-rels-6 ~
lv-qty-7 lv-rels-7 lv-qty-8 lv-rels-8 lv-qty-9 lv-rels-9 lv-qty-10 ~
lv-rels-10 lv-qty-11 lv-rels-11 lv-qty-12 lv-rels-12 lv-qty-13 lv-rels-13 ~
lv-qty-14 lv-rels-14 lv-qty-15 lv-rels-15 lv-qty-16 lv-rels-16 lv-qty-17 ~
lv-rels-17 lv-qty-18 lv-rels-18 lv-qty-19 lv-rels-19 lv-qty-20 lv-rels-20 ~
lv-qty-21 lv-rels-21 lv-qty-22 lv-rels-22 lv-qty-23 lv-rels-23 lv-qty-24 ~
lv-rels-24 lv-qty-25 lv-rels-25 lv-qty-26 lv-rels-26 lv-qty-27 lv-rels-27 ~
lv-qty-28 lv-rels-28 v-do-speed v-do-mr v-do-gsa v-drop-rc v-ink-all-forms ~
lv-match-up v-est-list v-calc-board-cost-on-blank 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-clear 
     LABEL "&Clear Qtys" 
     SIZE 21 BY 1.14.

DEFINE BUTTON btn-upd 
     LABEL "&Update Est Qtys" 
     SIZE 21 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE v-est-list AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 46 BY 6.14
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lv-match-up AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Match Markup Percentage?" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE lv-qty-1 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-10 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-11 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-12 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-13 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-14 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-15 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-16 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-17 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-18 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-19 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-2 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-20 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-21 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-22 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-23 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-24 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-25 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-26 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-27 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-28 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-3 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-4 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-5 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-6 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-7 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-8 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-9 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-1 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-10 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-11 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-12 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-13 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-14 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-15 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-16 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-17 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-18 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-19 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-2 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-20 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-21 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-22 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-23 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-24 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-25 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-26 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-27 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-28 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-3 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-4 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-5 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-6 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-7 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-8 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-9 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 119 BY 10.48.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 8.33.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 8.33.

DEFINE VARIABLE v-calc-board-cost-on-blank AS LOGICAL INITIAL no 
     LABEL "Calc board cost on blank?" 
     VIEW-AS TOGGLE-BOX
     SIZE 63 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-do-gsa AS LOGICAL INITIAL no 
     LABEL "Override GS&&A and/or Warehouse Percentages?" 
     VIEW-AS TOGGLE-BOX
     SIZE 63 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-do-mr AS LOGICAL INITIAL no 
     LABEL "Recalc Machines' MR-Hrs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 62 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-do-speed AS LOGICAL INITIAL no 
     LABEL "Recalc Machines' Speed, Spoil%, && Waste?" 
     VIEW-AS TOGGLE-BOX
     SIZE 62 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-drop-rc AS LOGICAL INITIAL no 
     LABEL "Drop Slitter if MSF > Minimum?" 
     VIEW-AS TOGGLE-BOX
     SIZE 63 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-ink-all-forms AS LOGICAL INITIAL no 
     LABEL "Find Ink Cost For All Forms?" 
     VIEW-AS TOGGLE-BOX
     SIZE 63 BY 1
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     lv-qty-1 AT ROW 2.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-1 AT ROW 2.43 COL 24 COLON-ALIGNED NO-LABEL
     lv-qty-2 AT ROW 2.43 COL 34 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-2 AT ROW 2.43 COL 52 COLON-ALIGNED NO-LABEL
     lv-qty-3 AT ROW 2.43 COL 62 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-3 AT ROW 2.43 COL 80 COLON-ALIGNED NO-LABEL
     lv-qty-4 AT ROW 2.43 COL 90 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-4 AT ROW 2.43 COL 108 COLON-ALIGNED NO-LABEL
     lv-qty-5 AT ROW 3.38 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-5 AT ROW 3.38 COL 24 COLON-ALIGNED NO-LABEL
     lv-qty-6 AT ROW 3.38 COL 34 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-6 AT ROW 3.38 COL 52 COLON-ALIGNED NO-LABEL
     lv-qty-7 AT ROW 3.38 COL 62 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-7 AT ROW 3.38 COL 80 COLON-ALIGNED NO-LABEL
     lv-qty-8 AT ROW 3.38 COL 90 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-8 AT ROW 3.38 COL 108 COLON-ALIGNED NO-LABEL
     lv-qty-9 AT ROW 4.33 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-9 AT ROW 4.33 COL 24 COLON-ALIGNED NO-LABEL
     lv-qty-10 AT ROW 4.33 COL 34 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-10 AT ROW 4.33 COL 52 COLON-ALIGNED NO-LABEL
     lv-qty-11 AT ROW 4.33 COL 62 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-11 AT ROW 4.33 COL 80 COLON-ALIGNED NO-LABEL
     lv-qty-12 AT ROW 4.33 COL 90 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-12 AT ROW 4.33 COL 108 COLON-ALIGNED NO-LABEL
     lv-qty-13 AT ROW 5.29 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-13 AT ROW 5.29 COL 24 COLON-ALIGNED NO-LABEL
     lv-qty-14 AT ROW 5.29 COL 34 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-14 AT ROW 5.29 COL 52 COLON-ALIGNED NO-LABEL
     lv-qty-15 AT ROW 5.29 COL 62 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-15 AT ROW 5.29 COL 80 COLON-ALIGNED NO-LABEL
     lv-qty-16 AT ROW 5.29 COL 90 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-16 AT ROW 5.29 COL 108 COLON-ALIGNED NO-LABEL
     lv-qty-17 AT ROW 6.24 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-17 AT ROW 6.24 COL 24 COLON-ALIGNED NO-LABEL
     lv-qty-18 AT ROW 6.24 COL 34 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-18 AT ROW 6.24 COL 52 COLON-ALIGNED NO-LABEL
     lv-qty-19 AT ROW 6.24 COL 62 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-19 AT ROW 6.24 COL 80 COLON-ALIGNED NO-LABEL
     lv-qty-20 AT ROW 6.24 COL 90 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-20 AT ROW 6.24 COL 108 COLON-ALIGNED NO-LABEL
     lv-qty-21 AT ROW 7.19 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-21 AT ROW 7.19 COL 24 COLON-ALIGNED NO-LABEL
     lv-qty-22 AT ROW 7.19 COL 34 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-22 AT ROW 7.19 COL 52 COLON-ALIGNED NO-LABEL
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     lv-qty-23 AT ROW 7.19 COL 62 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-23 AT ROW 7.19 COL 80 COLON-ALIGNED NO-LABEL
     lv-qty-24 AT ROW 7.19 COL 90 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-24 AT ROW 7.19 COL 108 COLON-ALIGNED NO-LABEL
     lv-qty-25 AT ROW 8.14 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-25 AT ROW 8.14 COL 24 COLON-ALIGNED NO-LABEL
     lv-qty-26 AT ROW 8.14 COL 34 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-26 AT ROW 8.14 COL 52 COLON-ALIGNED NO-LABEL
     lv-qty-27 AT ROW 8.14 COL 62 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-27 AT ROW 8.14 COL 80 COLON-ALIGNED NO-LABEL
     lv-qty-28 AT ROW 8.14 COL 90 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-28 AT ROW 8.14 COL 108 COLON-ALIGNED NO-LABEL
     btn-clear AT ROW 9.81 COL 42
     btn-upd AT ROW 9.81 COL 63
     v-do-speed AT ROW 12.91 COL 6
     v-do-mr AT ROW 13.86 COL 6
     v-do-gsa AT ROW 14.81 COL 6
     v-drop-rc AT ROW 15.76 COL 6
     v-ink-all-forms AT ROW 16.71 COL 6 WIDGET-ID 2
     lv-match-up AT ROW 18.62 COL 41 COLON-ALIGNED
     v-est-list AT ROW 12.95 COL 73 HELP
          "Enter a list of estimates separated by commas" NO-LABEL
     Btn_OK AT ROW 20.52 COL 30
     Btn_Cancel AT ROW 20.52 COL 74
     v-calc-board-cost-on-blank AT ROW 17.67 COL 6 WIDGET-ID 4
     "Rels" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.71 COL 81
          FONT 6
     "Selection" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 11.95 COL 4
          FONT 6
     "Rels" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.71 COL 109
          FONT 6
     "Estimates for Board Cost" VIEW-AS TEXT
          SIZE 33 BY 1 AT ROW 11.95 COL 80
     "Quantity" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.71 COL 95
          FONT 6
     "Quantity" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.71 COL 66
          FONT 6
     "Rels" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.71 COL 26
          FONT 6
     "Quantity" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.71 COL 38
          FONT 6
     "Rels" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.71 COL 53
          FONT 6
     "Quantity" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.71 COL 10
          FONT 6
     RECT-1 AT ROW 1.24 COL 1
     RECT-23 AT ROW 11.71 COL 1
     RECT-24 AT ROW 11.71 COL 72
     SPACE(2.39) SKIP(1.86)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Estimate Analysis"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       v-est-list:RETURN-INSERTED IN FRAME D-Dialog  = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Estimate Analysis */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  op-error = yes.
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-clear D-Dialog
ON CHOOSE OF btn-clear IN FRAME D-Dialog /* Clear Qtys */
DO:
  ASSIGN
   lv-qty-1:SCREEN-VALUE   = ""
   lv-qty-2:SCREEN-VALUE   = ""
   lv-qty-3:SCREEN-VALUE   = ""
   lv-qty-4:SCREEN-VALUE   = ""
   lv-qty-5:SCREEN-VALUE   = ""
   lv-qty-6:SCREEN-VALUE   = ""
   lv-qty-7:SCREEN-VALUE   = ""
   lv-qty-8:SCREEN-VALUE   = ""
   lv-qty-9:SCREEN-VALUE   = ""
   lv-qty-10:SCREEN-VALUE  = ""
   lv-qty-11:SCREEN-VALUE  = ""
   lv-qty-12:SCREEN-VALUE  = ""
   lv-qty-13:SCREEN-VALUE  = ""
   lv-qty-14:SCREEN-VALUE  = ""
   lv-qty-15:SCREEN-VALUE  = ""
   lv-qty-16:SCREEN-VALUE  = ""
   lv-qty-17:SCREEN-VALUE  = ""
   lv-qty-18:SCREEN-VALUE  = ""
   lv-qty-19:SCREEN-VALUE  = ""
   lv-qty-20:SCREEN-VALUE  = ""
   lv-qty-21:SCREEN-VALUE  = ""
   lv-qty-22:SCREEN-VALUE  = ""
   lv-qty-23:SCREEN-VALUE  = ""
   lv-qty-24:SCREEN-VALUE  = ""
   lv-qty-25:SCREEN-VALUE  = ""
   lv-qty-26:SCREEN-VALUE  = ""
   lv-qty-27:SCREEN-VALUE  = ""
   lv-qty-28:SCREEN-VALUE  = ""

   lv-rels-1:SCREEN-VALUE  = ""
   lv-rels-2:SCREEN-VALUE  = ""
   lv-rels-3:SCREEN-VALUE  = ""
   lv-rels-4:SCREEN-VALUE  = ""
   lv-rels-5:SCREEN-VALUE  = ""
   lv-rels-6:SCREEN-VALUE  = ""
   lv-rels-7:SCREEN-VALUE  = ""
   lv-rels-8:SCREEN-VALUE  = ""
   lv-rels-9:SCREEN-VALUE  = ""
   lv-rels-10:SCREEN-VALUE = ""
   lv-rels-11:SCREEN-VALUE = ""
   lv-rels-12:SCREEN-VALUE = ""
   lv-rels-13:SCREEN-VALUE = ""
   lv-rels-14:SCREEN-VALUE = ""
   lv-rels-15:SCREEN-VALUE = ""
   lv-rels-16:SCREEN-VALUE = ""
   lv-rels-17:SCREEN-VALUE = ""
   lv-rels-18:SCREEN-VALUE = ""
   lv-rels-19:SCREEN-VALUE = ""
   lv-rels-20:SCREEN-VALUE = ""
   lv-rels-21:SCREEN-VALUE = ""
   lv-rels-22:SCREEN-VALUE = ""
   lv-rels-23:SCREEN-VALUE = ""
   lv-rels-24:SCREEN-VALUE = ""
   lv-rels-25:SCREEN-VALUE = ""
   lv-rels-26:SCREEN-VALUE = ""
   lv-rels-27:SCREEN-VALUE = ""
   lv-rels-28:SCREEN-VALUE = "".

  APPLY "entry" TO lv-qty-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-upd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-upd D-Dialog
ON CHOOSE OF btn-upd IN FRAME D-Dialog /* Update Est Qtys */
DO:
  DEF VAR lv-estqty-recid AS RECID NO-UNDO.
  DEF VAR lv-hld-eqty LIKE est-qty.eqty NO-UNDO.
  DEF VAR char-val AS CHAR NO-UNDO. 
  DEF VAR char-val2 AS CHAR NO-UNDO.        
  DEF VAR date-val AS CHAR NO-UNDO.
  DEF VAR date-val2 AS CHAR NO-UNDO.
  DEFINE BUFFER bff-eb FOR eb .
  DEFINE BUFFER bff-ef FOR ef .
  DEFINE BUFFER bf-est FOR est  .

  FIND FIRST bf-est NO-LOCK
     WHERE RECID(bf-est) EQ recid(xest) NO-ERROR.
 
  lv-estqty-recid = IF AVAIL est-qty THEN RECID(est-qty) ELSE ?.
  RUN est/estqtyd.w (lv-estqty-recid, RECID(eb), STRING(IF bf-est.est-type LE 4 THEN eb.bl-qty ELSE est-qty.eqty), OUTPUT char-val, OUTPUT char-val2, OUTPUT date-val, OUTPUT date-val2).

  IF char-val NE "?" OR char-val2 NE "?" THEN DO:
     FIND CURRENT bf-est NO-ERROR.
     ASSIGN
      bf-est.est-qty[1] = INT(ENTRY(1,char-val))
      bf-est.est-qty[2] = INT(ENTRY(2,char-val))
      bf-est.est-qty[3] = INT(ENTRY(3,char-val))
      bf-est.est-qty[4] = INT(ENTRY(4,char-val)).
     FIND CURRENT bf-est NO-LOCK NO-ERROR.
    
     lv-hld-eqty = est-qty.eqty.
     FIND CURRENT est-qty NO-ERROR.
     est-qty.eqty = INT(ENTRY(1,char-val)).
     FIND CURRENT est-qty NO-LOCK NO-ERROR.
    
     /*RELEASE eb.*/
     FOR EACH bff-eb
         WHERE bff-eb.company EQ bf-est.company
           AND bff-eb.est-no  EQ bf-est.est-no
           AND bff-eb.eqty    EQ lv-hld-eqty
           AND bff-eb.form-no NE 0:
       IF bf-est.est-type LE 4 THEN bff-eb.bl-qty = INT(ENTRY(1,char-val)).
       bff-eb.eqty = INT(ENTRY(1,char-val)).
     END.
     FOR EACH bff-ef
         WHERE bff-ef.company EQ bf-est.company
           AND bff-ef.est-no  EQ bf-est.est-no
           AND bff-ef.eqty    EQ lv-hld-eqty:
       bff-ef.eqty = INT(ENTRY(1,char-val)).
     END.
    
     FOR EACH est-op
         WHERE est-op.company EQ bf-est.company
           AND est-op.est-no  EQ bf-est.est-no
           AND est-op.qty     EQ lv-hld-eqty:
       est-op.qty = est-qty.eqty.
     END.
    
     FOR EACH est-op
         WHERE est-op.company EQ bf-est.company
           AND est-op.est-no  EQ bf-est.est-no
           AND est-op.qty     EQ est-qty.eqty
           AND est-op.line    GE 500:
       DELETE est-op.
     END.
    
     APPLY "choose" TO btn-clear.
  END. 

  IF char-val NE "?" THEN DO:
     IF INT(ENTRY(1,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-1:SCREEN-VALUE = ENTRY(11,char-val)
        lv-qty-1:SCREEN-VALUE  = ENTRY(1,char-val).
       APPLY "entry" TO lv-qty-2.
     END.
    
     IF INT(ENTRY(2,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-2:SCREEN-VALUE = ENTRY(12,char-val)
        lv-qty-2:SCREEN-VALUE  = ENTRY(2,char-val).
       APPLY "entry" TO lv-qty-3.
     END.
    
     IF INT(ENTRY(3,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-3:SCREEN-VALUE = ENTRY(13,char-val)
        lv-qty-3:SCREEN-VALUE  = ENTRY(3,char-val).
       APPLY "entry" TO lv-qty-4.
     END.
    
     IF INT(ENTRY(4,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-4:SCREEN-VALUE = ENTRY(14,char-val)
        lv-qty-4:SCREEN-VALUE  = ENTRY(4,char-val).
       APPLY "entry" TO lv-qty-5.
     END.
    
     IF INT(ENTRY(5,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-5:SCREEN-VALUE = ENTRY(15,char-val)
        lv-qty-5:SCREEN-VALUE  = ENTRY(5,char-val).
       APPLY "entry" TO lv-qty-6.
     END.
    
     IF INT(ENTRY(6,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-6:SCREEN-VALUE = ENTRY(16,char-val)
        lv-qty-6:SCREEN-VALUE  = ENTRY(6,char-val).
       APPLY "entry" TO lv-qty-7.
     END.
    
     IF INT(ENTRY(7,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-7:SCREEN-VALUE = ENTRY(17,char-val)
        lv-qty-7:SCREEN-VALUE  = ENTRY(7,char-val).
       APPLY "entry" TO lv-qty-8.
     END.
    
     IF INT(ENTRY(8,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-8:SCREEN-VALUE = ENTRY(18,char-val)
        lv-qty-8:SCREEN-VALUE  = ENTRY(8,char-val).
       APPLY "entry" TO lv-qty-9.
     END.
    
     IF INT(ENTRY(9,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-9:SCREEN-VALUE = ENTRY(19,char-val)
        lv-qty-9:SCREEN-VALUE  = ENTRY(9,char-val).
       APPLY "entry" TO lv-qty-10.
     END.
    
     IF INT(ENTRY(10,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-10:SCREEN-VALUE = ENTRY(20,char-val)
        lv-qty-10:SCREEN-VALUE  = ENTRY(10,char-val).
       APPLY "entry" TO lv-qty-11.
     END.
  END.

  IF char-val2 NE "?" THEN DO:
     IF INT(ENTRY(1,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-11:SCREEN-VALUE = ENTRY(11,char-val2)
        lv-qty-11:SCREEN-VALUE  = ENTRY(1,char-val2).
       APPLY "entry" TO lv-qty-12.
     END.
    
     IF INT(ENTRY(2,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-12:SCREEN-VALUE = ENTRY(12,char-val2)
        lv-qty-12:SCREEN-VALUE  = ENTRY(2,char-val2).
       APPLY "entry" TO lv-qty-13.
     END.
    
     IF INT(ENTRY(3,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-13:SCREEN-VALUE = ENTRY(13,char-val2)
        lv-qty-13:SCREEN-VALUE  = ENTRY(3,char-val2).
       APPLY "entry" TO lv-qty-14.
     END.
    
     IF INT(ENTRY(4,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-14:SCREEN-VALUE = ENTRY(14,char-val2)
        lv-qty-14:SCREEN-VALUE  = ENTRY(4,char-val2).
       APPLY "entry" TO lv-qty-15.
     END.
    
     IF INT(ENTRY(5,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-15:SCREEN-VALUE = ENTRY(15,char-val2)
        lv-qty-15:SCREEN-VALUE  = ENTRY(5,char-val2).
       APPLY "entry" TO lv-qty-16.
     END.
    
     IF INT(ENTRY(6,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-16:SCREEN-VALUE = ENTRY(16,char-val2)
        lv-qty-16:SCREEN-VALUE  = ENTRY(6,char-val2).
       APPLY "entry" TO lv-qty-17.
     END.
    
     IF INT(ENTRY(7,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-17:SCREEN-VALUE = ENTRY(17,char-val2)
        lv-qty-17:SCREEN-VALUE  = ENTRY(7,char-val2).
       APPLY "entry" TO lv-qty-18.
     END.
    
     IF INT(ENTRY(8,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-18:SCREEN-VALUE = ENTRY(18,char-val2)
        lv-qty-18:SCREEN-VALUE  = ENTRY(8,char-val2).
       APPLY "entry" TO lv-qty-19.
     END.
                                        
     IF INT(ENTRY(9,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-19:SCREEN-VALUE = ENTRY(19,char-val2)
        lv-qty-19:SCREEN-VALUE  = ENTRY(9,char-val2).
       APPLY "entry" TO lv-qty-20.
     END.
    
     IF INT(ENTRY(10,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-20:SCREEN-VALUE = ENTRY(20,char-val2)
        lv-qty-20:SCREEN-VALUE  = ENTRY(10,char-val2).
       APPLY "entry" TO lv-qty-21.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
    op-error = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DEF VAR lv-est-no LIKE est.est-no NO-UNDO.

  DEF BUFFER b-reft FOR reftable.
  DEF BUFFER op-lock FOR reftable.


  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&displayed-objects}
     v-est-list = TRIM(v-est-list).

     IF v-est-list NE ""                               AND
        SUBSTR(v-est-list,LENGTH(v-est-list),1) EQ "," THEN
       SUBSTR(v-est-list,LENGTH(v-est-list),1) = "".

     v-est-list = TRIM(v-est-list) + "," + TRIM(xest.est-no).

     DISPLAY v-est-list.
     ASSIGN v-est-list.
  END.

  ASSIGN io-do-speed = v-do-speed
         io-do-mr = v-do-mr
         io-do-gsa = v-do-gsa
         io-v-drop-rc = v-drop-rc
         io-ink-all-forms = v-ink-all-forms
         io-board-cost-from-blank = v-calc-board-cost-on-blank
         io-v-match-up = lv-match-up
         tt-qtty.qtty[1] = lv-qty-1
         tt-qtty.qtty[2] = lv-qty-2 
         tt-qtty.qtty[3] = lv-qty-3  
         tt-qtty.qtty[4] = lv-qty-4 
         tt-qtty.qtty[5] = lv-qty-5  
         tt-qtty.qtty[6] = lv-qty-6    
         tt-qtty.qtty[7] = lv-qty-7 
         tt-qtty.qtty[8] = lv-qty-8  
         tt-qtty.qtty[9] = lv-qty-9    
         tt-qtty.qtty[10] = lv-qty-10    
         tt-qtty.qtty[11] = lv-qty-11    
         tt-qtty.qtty[12] = lv-qty-12    
         tt-qtty.qtty[13] = lv-qty-13    
         tt-qtty.qtty[14] = lv-qty-14    
         tt-qtty.qtty[15] = lv-qty-15    
         tt-qtty.qtty[16] = lv-qty-16    
         tt-qtty.qtty[17] = lv-qty-17    
         tt-qtty.qtty[18] = lv-qty-18    
         tt-qtty.qtty[19] = lv-qty-19    
         tt-qtty.qtty[20] = lv-qty-20    
         tt-qtty.qtty[21] = lv-qty-21    
         tt-qtty.qtty[22] = lv-qty-22    
         tt-qtty.qtty[23] = lv-qty-23    
         tt-qtty.qtty[24] = lv-qty-24    
         tt-qtty.qtty[25] = lv-qty-25    
         tt-qtty.qtty[26] = lv-qty-26    
         tt-qtty.qtty[27] = lv-qty-27    
         tt-qtty.qtty[28] = lv-qty-28    
         tt-qtty.rel[1] = lv-rels-1    
         tt-qtty.rel[2] = lv-rels-2    
         tt-qtty.rel[3] = lv-rels-3    
         tt-qtty.rel[4] = lv-rels-4    
         tt-qtty.rel[5] = lv-rels-5    
         tt-qtty.rel[6] = lv-rels-6    
         tt-qtty.rel[7] = lv-rels-7    
         tt-qtty.rel[8] = lv-rels-8    
         tt-qtty.rel[9] = lv-rels-9    
         tt-qtty.rel[10] = lv-rels-10   
         tt-qtty.rel[11] = lv-rels-11   
         tt-qtty.rel[12] = lv-rels-12   
         tt-qtty.rel[13] = lv-rels-13   
         tt-qtty.rel[14] = lv-rels-14   
         tt-qtty.rel[15] = lv-rels-15   
         tt-qtty.rel[16] = lv-rels-16   
         tt-qtty.rel[17] = lv-rels-17   
         tt-qtty.rel[18] = lv-rels-18   
         tt-qtty.rel[19] = lv-rels-19   
         tt-qtty.rel[20] = lv-rels-20   
         tt-qtty.rel[21] = lv-rels-21   
         tt-qtty.rel[22] = lv-rels-22   
         tt-qtty.rel[23] = lv-rels-23  
         tt-qtty.rel[24] = lv-rels-24   
         tt-qtty.rel[25] = lv-rels-25   
         tt-qtty.rel[26] = lv-rels-26   
         tt-qtty.rel[27] = lv-rels-27   
         tt-qtty.rel[28] = lv-rels-28.

  DO i = 1 TO NUM-ENTRIES(v-est-list):
    ASSIGN
     lv-est-no = ENTRY(i,v-est-list)
     lv-est-no = FILL(" ",8 - LENGTH(TRIM(lv-est-no))) + TRIM(lv-est-no).

    FIND FIRST est
        WHERE est.company    EQ xest.company
          AND est.loc        EQ xest.loc
          AND est.est-no     EQ lv-est-no
          AND ((est.est-type LT 5 AND xest.est-type LT 5) OR
               (est.est-type GE 5 AND xest.est-type GE 5))
        NO-LOCK NO-ERROR.

    IF AVAIL est THEN DO:
      FIND FIRST w-est WHERE w-est-no EQ lv-est-no NO-ERROR.
      IF NOT AVAIL w-est THEN DO:
        CREATE w-est.
        ASSIGN
         w-est-no = est.est-no
         w-row-id = ROWID(est).
      END.
    END.
  END.

  FOR EACH w-est,
      EACH b-reft
      WHERE b-reft.reftable EQ "est/getqty.w"
        AND b-reft.company  EQ xest.company
        AND b-reft.loc      EQ xest.loc
        AND b-reft.code     EQ w-est-no
      NO-LOCK:

    FIND reftable WHERE ROWID(reftable) EQ ROWID(b-reft) EXCLUSIVE NO-WAIT NO-ERROR.
    IF AVAIL reftable THEN DELETE reftable.
    ELSE DO:
      MESSAGE "Estimate Record is being changed by someone else, wait a moment and try again..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "choose" TO btn_cancel.
    END.
  END.

  FIND FIRST reftable
      WHERE reftable.reftable EQ "est/getqty.w"
        AND reftable.code2    EQ STRING(li-seq,"9999999999")
      USE-INDEX code2 NO-LOCK NO-ERROR.

  IF AVAIL reftable OR li-seq EQ 0 THEN DO:
    FIND LAST reftable WHERE reftable.reftable EQ "est/getqty.w"
        USE-INDEX code2 NO-LOCK NO-ERROR.
    li-seq = (IF AVAIL reftable THEN INT(reftable.code2) ELSE 0) + 1.
  END.

  FOR EACH w-est,
      FIRST est WHERE ROWID(est) EQ w-row-id NO-LOCK:

    CREATE reftable.
    ASSIGN
     reftable.reftable = "est/getqty.w"
     reftable.company  = est.company
     reftable.loc      = est.loc
     reftable.code     = est.est-no
     reftable.code2    = STRING(li-seq,"9999999999").
  END.

  FIND FIRST reftable
      WHERE reftable.reftable EQ "est/getqty.w2"
        AND reftable.company  EQ xest.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ xest.est-no
      NO-ERROR.

  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "est/getqty.w2"
     reftable.company  = xest.company
     reftable.loc      = ""
     reftable.code     = xest.est-no.
  END.
  reftable.val[1] = io-v-match-up.

  {est/op-lock.i xest}
  ASSIGN
   op-lock.val[1] = INT(io-do-speed)
   op-lock.val[2] = INT(io-do-mr).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
assign v-do-speed = io-do-speed
       v-do-mr = io-do-mr
       v-do-gsa = io-do-gsa
       v-drop-rc = io-v-drop-rc
       lv-match-up = io-v-match-up
       v-ink-all-forms = io-ink-all-forms
       v-calc-board-cost-on-blank = io-board-cost-from-blank
       li-seq = 0.

FIND FIRST eb NO-LOCK
    WHERE eb.company EQ xest.company
      AND eb.est-no  EQ xest.est-no
      AND eb.form-no NE 0
    NO-ERROR.

FIND FIRST est-qty NO-LOCK
    WHERE est-qty.company EQ xest.company
      AND est-qty.est-no  EQ xest.est-no
    NO-ERROR.

FIND FIRST reftable
    WHERE reftable.reftable EQ "est/getqty.w2"
      AND reftable.company  EQ xest.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ xest.est-no
    NO-LOCK NO-ERROR.
IF AVAIL reftable THEN lv-match-up = reftable.val[1].

FOR EACH probe
    WHERE probe.company EQ xest.company
      AND probe.est-no  EQ xest.est-no
    NO-LOCK,
    FIRST reftable
    {est/probreft.i reftable probe}
    TRANSACTION:

  lv-match-up = reftable.val[2].
  DELETE reftable.
END.

FIND FIRST reftable
    WHERE reftable.reftable EQ "est/getqty.w"
      AND reftable.company  EQ xest.company
      AND reftable.loc      EQ xest.loc
      AND reftable.code     EQ xest.est-no
    NO-LOCK NO-ERROR.
IF AVAIL reftable THEN li-seq = INT(reftable.code2).

IF li-seq NE 0 THEN
FOR EACH reftable
    WHERE reftable.reftable EQ "est/getqty.w"
      AND reftable.code2    EQ STRING(li-seq,"9999999999")
      AND reftable.code     NE xest.est-no
    USE-INDEX code2 NO-LOCK
    BREAK BY reftable.code:

  IF FIRST-OF(reftable.code) THEN
    v-est-list = TRIM(v-est-list) + TRIM(reftable.code) + ",".
END.

find first tt-qtty.
assign  lv-qty-1 = tt-qtty.qtty[1]
        lv-qty-2 = tt-qtty.qtty[2]
        lv-qty-3 = tt-qtty.qtty[3]
        lv-qty-4 = tt-qtty.qtty[4]
        lv-qty-5 = tt-qtty.qtty[5]
        lv-qty-6 = tt-qtty.qtty[6]  
        lv-qty-7 = tt-qtty.qtty[7]        
        lv-qty-8 = tt-qtty.qtty[8]
        lv-qty-9 = tt-qtty.qtty[9]
        lv-qty-10 = tt-qtty.qtty[10]
        lv-qty-11 = tt-qtty.qtty[11]
        lv-qty-12 = tt-qtty.qtty[12]
        lv-qty-13 = tt-qtty.qtty[13]
        lv-qty-14 = tt-qtty.qtty[14]
        lv-qty-15 = tt-qtty.qtty[15]
        lv-qty-16 = tt-qtty.qtty[16]
        lv-qty-17 = tt-qtty.qtty[17]
        lv-qty-18 = tt-qtty.qtty[18]
        lv-qty-19 = tt-qtty.qtty[19]
        lv-qty-20 = tt-qtty.qtty[20]
        lv-qty-21 = tt-qtty.qtty[21]
        lv-qty-22 = tt-qtty.qtty[22]
        lv-qty-23 = tt-qtty.qtty[23]
        lv-qty-24 = tt-qtty.qtty[24]
        lv-qty-25 = tt-qtty.qtty[25]
        lv-qty-26 = tt-qtty.qtty[26]
        lv-qty-27 = tt-qtty.qtty[27]
        lv-qty-28 = tt-qtty.qtty[28]
        lv-rels-1 = tt-qtty.rel[1]
        lv-rels-2 = tt-qtty.rel[2]
        lv-rels-3 = tt-qtty.rel[3]
        lv-rels-4 = tt-qtty.rel[4]
        lv-rels-5 = tt-qtty.rel[5]
        lv-rels-6 = tt-qtty.rel[6]
        lv-rels-7 = tt-qtty.rel[7]
        lv-rels-8 = tt-qtty.rel[8]
        lv-rels-9 = tt-qtty.rel[9]
        lv-rels-10 = tt-qtty.rel[10]
        lv-rels-11 = tt-qtty.rel[11]
        lv-rels-12 = tt-qtty.rel[12]
        lv-rels-13 = tt-qtty.rel[13]
        lv-rels-14 = tt-qtty.rel[14]
        lv-rels-15 = tt-qtty.rel[15]
        lv-rels-16 = tt-qtty.rel[16]
        lv-rels-17 = tt-qtty.rel[17]
        lv-rels-18 = tt-qtty.rel[18]
        lv-rels-19 = tt-qtty.rel[19]
        lv-rels-20 = tt-qtty.rel[20]
        lv-rels-21 = tt-qtty.rel[21]
        lv-rels-22 = tt-qtty.rel[22]
        lv-rels-23 = tt-qtty.rel[23]
        lv-rels-24 = tt-qtty.rel[24]
        lv-rels-25 = tt-qtty.rel[25]
        lv-rels-26 = tt-qtty.rel[26]
        lv-rels-27 = tt-qtty.rel[27]
        lv-rels-28 = tt-qtty.rel[28]
         . 
{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY lv-qty-1 lv-rels-1 lv-qty-2 lv-rels-2 lv-qty-3 lv-rels-3 lv-qty-4 
          lv-rels-4 lv-qty-5 lv-rels-5 lv-qty-6 lv-rels-6 lv-qty-7 lv-rels-7 
          lv-qty-8 lv-rels-8 lv-qty-9 lv-rels-9 lv-qty-10 lv-rels-10 lv-qty-11 
          lv-rels-11 lv-qty-12 lv-rels-12 lv-qty-13 lv-rels-13 lv-qty-14 
          lv-rels-14 lv-qty-15 lv-rels-15 lv-qty-16 lv-rels-16 lv-qty-17 
          lv-rels-17 lv-qty-18 lv-rels-18 lv-qty-19 lv-rels-19 lv-qty-20 
          lv-rels-20 lv-qty-21 lv-rels-21 lv-qty-22 lv-rels-22 lv-qty-23 
          lv-rels-23 lv-qty-24 lv-rels-24 lv-qty-25 lv-rels-25 lv-qty-26 
          lv-rels-26 lv-qty-27 lv-rels-27 lv-qty-28 lv-rels-28 v-do-speed 
          v-do-mr v-do-gsa v-drop-rc v-ink-all-forms lv-match-up v-est-list 
          v-calc-board-cost-on-blank 
      WITH FRAME D-Dialog.
  ENABLE lv-qty-1 lv-rels-1 lv-qty-2 lv-rels-2 lv-qty-3 lv-rels-3 lv-qty-4 
         lv-rels-4 lv-qty-5 lv-rels-5 lv-qty-6 lv-rels-6 lv-qty-7 lv-rels-7 
         lv-qty-8 lv-rels-8 lv-qty-9 lv-rels-9 lv-qty-10 lv-rels-10 lv-qty-11 
         lv-rels-11 lv-qty-12 lv-rels-12 lv-qty-13 lv-rels-13 lv-qty-14 
         lv-rels-14 lv-qty-15 lv-rels-15 lv-qty-16 lv-rels-16 lv-qty-17 
         lv-rels-17 lv-qty-18 lv-rels-18 lv-qty-19 lv-rels-19 lv-qty-20 
         lv-rels-20 lv-qty-21 lv-rels-21 lv-qty-22 lv-rels-22 lv-qty-23 
         lv-rels-23 lv-qty-24 lv-rels-24 lv-qty-25 lv-rels-25 lv-qty-26 
         lv-rels-26 lv-qty-27 lv-rels-27 lv-qty-28 lv-rels-28 btn-clear btn-upd 
         v-do-speed v-do-mr v-do-gsa v-drop-rc v-ink-all-forms lv-match-up 
         v-est-list Btn_OK Btn_Cancel v-calc-board-cost-on-blank RECT-1 RECT-23 
         RECT-24 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF xest.est-type LT 5 THEN DISABLE v-est-list.
    APPLY "entry" TO lv-qty-1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT lv-mclean THEN lv-match-up:HIDDEN = YES.
    IF xest.est-type LE 4 THEN v-drop-rc:HIDDEN = YES.
    IF xest.est-type NE 6 THEN v-ink-all-forms:HIDDEN = YES.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

