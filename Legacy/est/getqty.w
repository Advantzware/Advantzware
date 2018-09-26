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
DEF VAR ld-msf AS DEC NO-UNDO.
def shared var cocode as cha no-undo.

{custom/globdefs.i}
cocode = g_company.
{cec/msfcalc.i}

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
btn-clear btn-upd v-do-speed v-do-mr v-do-gsa v-drop-rc v-ink-all-forms ~
lv-match-up v-est-list Btn_OK Btn_Cancel v-calc-board-cost-on-blank RECT-1 ~
RECT-23 RECT-24 tb_run tb_run-2 tb_run-3 tb_run-4 tb_run-5 tb_run-6 ~
tb_run-7 tb_run-8 tb_run-9 tb_run-10 tb_run-11 tb_run-12 tb_run-13 ~
tb_run-14 tb_run-15 tb_run-16 tb_run-17 tb_run-18 tb_run-19 tb_run-20 ~
tb_run-21 
&Scoped-Define DISPLAYED-OBJECTS lv-qty-1 lv-rels-1 lv-qty-2 lv-rels-2 ~
lv-qty-3 lv-rels-3 lv-qty-4 lv-rels-4 lv-qty-5 lv-rels-5 lv-qty-6 lv-rels-6 ~
lv-qty-7 lv-rels-7 lv-qty-8 lv-rels-8 lv-qty-9 lv-rels-9 lv-qty-10 ~
lv-rels-10 lv-qty-11 lv-rels-11 lv-qty-12 lv-rels-12 lv-qty-13 lv-rels-13 ~
lv-qty-14 lv-rels-14 lv-qty-15 lv-rels-15 lv-qty-16 lv-rels-16 lv-qty-17 ~
lv-rels-17 lv-qty-18 lv-rels-18 lv-qty-19 lv-rels-19 lv-qty-20 lv-rels-20 ~
v-do-speed v-do-mr v-do-gsa v-drop-rc v-ink-all-forms lv-match-up ~
v-est-list v-calc-board-cost-on-blank tb_run tb_run-2 tb_run-3 tb_run-4 ~
tb_run-5 tb_run-6 tb_run-7 tb_run-8 tb_run-9 tb_run-10 tb_run-11 tb_run-12 ~
tb_run-13 tb_run-14 tb_run-15 tb_run-16 tb_run-17 tb_run-18 tb_run-19 ~
tb_run-20 tb_run-21 lv-msf-1 lv-msf-2 lv-msf-3 lv-msf-4 lv-msf-5 lv-msf-6 ~
lv-msf-7 lv-msf-8 lv-msf-9 lv-msf-10 lv-msf-11 lv-msf-12 lv-msf-13 ~
lv-msf-14 lv-msf-15 lv-msf-16 lv-msf-17 lv-msf-18 lv-msf-19 lv-msf-20 

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

DEFINE VARIABLE lv-msf-1 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-10 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-11 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-12 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-13 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-14 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-15 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-16 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-17 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-18 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-19 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-2 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-20 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-3 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-4 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-5 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-6 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-7 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-8 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-9 AS DECIMAL FORMAT ">>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE lv-rels-1 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-10 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-11 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-12 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-13 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-14 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-15 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-16 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-17 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-18 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-19 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-2 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-20 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-3 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-4 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-5 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-6 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-7 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-8 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-rels-9 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 24.29.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 8.33.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 8.33.

DEFINE VARIABLE tb_run AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-10 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-11 AS LOGICAL INITIAL no 
    LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-12 AS LOGICAL INITIAL no 
    LABEL ""
    VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-13 AS LOGICAL INITIAL no 
    LABEL ""
    VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-14 AS LOGICAL INITIAL no 
    LABEL ""
    VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-15 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-16 AS LOGICAL INITIAL no
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-17 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-18 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-19 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-2 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-20 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-21 AS LOGICAL INITIAL no 
     LABEL "Select All" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-3 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-4 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-5 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-6 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-7 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-8 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_run-9 AS LOGICAL INITIAL no 
     LABEL ""
     VIEW-AS TOGGLE-BOX
     SIZE 3.8 BY 1
     FONT 6 NO-UNDO.

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
     tb_run AT ROW 2.38 COL 45.2 WIDGET-ID 8 NO-LABEL
     lv-qty-2 AT ROW 3.43 COL 7 HELP
          "Enter Quantity." NO-LABEL
     lv-rels-2 AT ROW 3.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-2 AT ROW 3.33 COL 45.2 WIDGET-ID 10 NO-LABEL
     lv-qty-3 AT ROW 4.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-3 AT ROW 4.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-3 AT ROW 4.38 COL 45.2 WIDGET-ID 12 NO-LABEL
     lv-qty-4 AT ROW 5.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-4 AT ROW 5.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-4 AT ROW 5.38 COL 45.2 WIDGET-ID 14 NO-LABEL
     lv-qty-5 AT ROW 6.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-5 AT ROW 6.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-5 AT ROW 6.43 COL 45.2 WIDGET-ID 16 NO-LABEL
     lv-qty-6 AT ROW 7.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-6 AT ROW 7.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-6 AT ROW 7.48 COL 45.2 WIDGET-ID 18 NO-LABEL
     lv-qty-7 AT ROW 8.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-7 AT ROW 8.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-7 AT ROW 8.52 COL 45.2 WIDGET-ID 20 NO-LABEL
     lv-qty-8 AT ROW 9.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-8 AT ROW 9.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-8 AT ROW 9.43 COL 45.2 WIDGET-ID 22 NO-LABEL
     lv-qty-9 AT ROW 10.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-9 AT ROW 10.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-9 AT ROW 10.48 COL 45.2 WIDGET-ID 24 NO-LABEL
     lv-qty-10 AT ROW 11.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-10 AT ROW 11.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-10 AT ROW 11.48 COL 45.2 WIDGET-ID 26 NO-LABEL
     lv-qty-11 AT ROW 12.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-11 AT ROW 12.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-11 AT ROW 12.38 COL 45.2 WIDGET-ID 28 NO-LABEL
     lv-qty-12 AT ROW 13.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-12 AT ROW 13.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-12 AT ROW 13.38 COL 45.2 WIDGET-ID 30 NO-LABEL
     lv-qty-13 AT ROW 14.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-13 AT ROW 14.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-13 AT ROW 14.43 COL 45.2 WIDGET-ID 32 NO-LABEL
     lv-qty-14 AT ROW 15.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-14 AT ROW 15.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-14 AT ROW 15.38 COL 45.2 WIDGET-ID 34 NO-LABEL
     lv-qty-15 AT ROW 16.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-15 AT ROW 16.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-15 AT ROW 16.38 COL 45.2 WIDGET-ID 36 NO-LABEL
     lv-qty-16 AT ROW 17.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-16 AT ROW 17.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-16 AT ROW 17.43 COL 45.2 WIDGET-ID 38 NO-LABEL
     lv-qty-17 AT ROW 18.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-17 AT ROW 18.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-17 AT ROW 18.48 COL 45.2 WIDGET-ID 40 NO-LABEL
     lv-qty-18 AT ROW 19.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-18 AT ROW 19.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-18 AT ROW 19.52 COL 45.2 WIDGET-ID 42 NO-LABEL
     lv-qty-19 AT ROW 20.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-19 AT ROW 20.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-19 AT ROW 20.48 COL 45.2  NO-LABEL WIDGET-ID 44
     lv-qty-20 AT ROW 21.43 COL 5 COLON-ALIGNED HELP
          "Enter Quantity." NO-LABEL
     lv-rels-20 AT ROW 21.43 COL 24 COLON-ALIGNED NO-LABEL
     tb_run-20 AT ROW 21.43 COL 45.2 NO-LABEL WIDGET-ID 46 
     btn-clear AT ROW 20.86 COL 96.4
     btn-upd AT ROW 20.86 COL 117.4
     v-do-speed AT ROW 3.86 COL 74.8
     v-do-mr AT ROW 4.81 COL 74.8
     v-do-gsa AT ROW 5.76 COL 74.8
     v-drop-rc AT ROW 6.71 COL 74.8
     v-ink-all-forms AT ROW 7.67 COL 74.8 WIDGET-ID 2
     lv-match-up AT ROW 9.57 COL 109.8 COLON-ALIGNED
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     v-est-list AT ROW 13 COL 85.4 HELP
          "Enter a list of estimates separated by commas" NO-LABEL
     Btn_OK AT ROW 23.67 COL 12.6
     Btn_Cancel AT ROW 23.67 COL 45.6
     v-calc-board-cost-on-blank AT ROW 8.62 COL 74.8 WIDGET-ID 4
     
     
     tb_run-21 AT ROW 20.95 COL 77.8 WIDGET-ID 48
     lv-msf-1 AT ROW 2.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     lv-msf-2 AT ROW 3.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     lv-msf-3 AT ROW 4.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     lv-msf-4 AT ROW 5.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     lv-msf-5 AT ROW 6.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     lv-msf-6 AT ROW 7.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     lv-msf-7 AT ROW 8.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     lv-msf-8 AT ROW 9.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     lv-msf-9 AT ROW 10.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     lv-msf-10 AT ROW 11.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     lv-msf-11 AT ROW 12.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     lv-msf-12 AT ROW 13.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     lv-msf-13 AT ROW 14.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     lv-msf-14 AT ROW 15.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     lv-msf-15 AT ROW 16.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     lv-msf-16 AT ROW 17.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 82
     lv-msf-17 AT ROW 18.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     lv-msf-18 AT ROW 19.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     lv-msf-19 AT ROW 20.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     lv-msf-20 AT ROW 21.43 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     "Quantity" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.71 COL 10
          FONT 6
     "Run/Ship" VIEW-AS TEXT
          SIZE 11.4 BY .62 AT ROW 1.71 COL 42.6 WIDGET-ID 6
          FONT 6
     "Qty Msf" VIEW-AS TEXT
          SIZE 11.4 BY .62 AT ROW 1.71 COL 55.6 WIDGET-ID 52
          FONT 6
     "Selection" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 2.91 COL 72.8
          FONT 6
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     "Estimates for Board Cost" VIEW-AS TEXT
          SIZE 33 BY 1 AT ROW 12 COL 92.4
     "Releases" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.71 COL 26
          FONT 6
     RECT-1 AT ROW 1.24 COL 1
     RECT-23 AT ROW 2.67 COL 70.4
     RECT-24 AT ROW 11.76 COL 84.4
     SPACE(11.59) SKIP(6.14)
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

/* SETTINGS FOR FILL-IN lv-msf-1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-1:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-10 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-10:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-11 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-11:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-12 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-12:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-13 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-13:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-14 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-14:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-15 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-15:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-16 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-16:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-17 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-17:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-18 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-18:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-19 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-19:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-2:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-20 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-20:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-3 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-3:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-4:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-5 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-5:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-6 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-6:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-7 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-7:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-8 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-8:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-9 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-msf-9:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-qty-2 IN FRAME D-Dialog
   ALIGN-L                                                              */
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
   .

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
        RUN get-msf(integer(lv-qty-1:SCREEN-VALUE)) .
          lv-msf-1:SCREEN-VALUE = STRING(ld-msf).

       APPLY "entry" TO lv-qty-2.
     END.
    
     IF INT(ENTRY(2,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-2:SCREEN-VALUE = ENTRY(12,char-val)
        lv-qty-2:SCREEN-VALUE  = ENTRY(2,char-val).
       RUN get-msf(integer(lv-qty-2:SCREEN-VALUE)) .
        lv-msf-2:SCREEN-VALUE = STRING(ld-msf).

       APPLY "entry" TO lv-qty-3.
     END.
    
     IF INT(ENTRY(3,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-3:SCREEN-VALUE = ENTRY(13,char-val)
        lv-qty-3:SCREEN-VALUE  = ENTRY(3,char-val).
       RUN get-msf(integer(lv-qty-3:SCREEN-VALUE)) .
         lv-msf-3:SCREEN-VALUE = STRING(ld-msf).

       APPLY "entry" TO lv-qty-4.
     END.
    
     IF INT(ENTRY(4,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-4:SCREEN-VALUE = ENTRY(14,char-val)
        lv-qty-4:SCREEN-VALUE  = ENTRY(4,char-val).
       RUN get-msf(integer(lv-qty-4:SCREEN-VALUE)) .
        lv-msf-4:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-5.
     END.
    
     IF INT(ENTRY(5,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-5:SCREEN-VALUE = ENTRY(15,char-val)
        lv-qty-5:SCREEN-VALUE  = ENTRY(5,char-val).
        RUN get-msf(integer(lv-qty-5:SCREEN-VALUE)) .
        lv-msf-5:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-6.
     END.
    
     IF INT(ENTRY(6,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-6:SCREEN-VALUE = ENTRY(16,char-val)
        lv-qty-6:SCREEN-VALUE  = ENTRY(6,char-val).
        RUN get-msf(integer(lv-qty-6:SCREEN-VALUE)) .
        lv-msf-6:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-7.
     END.
    
     IF INT(ENTRY(7,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-7:SCREEN-VALUE = ENTRY(17,char-val)
        lv-qty-7:SCREEN-VALUE  = ENTRY(7,char-val).
        RUN get-msf(integer(lv-qty-7:SCREEN-VALUE)) .
        lv-msf-7:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-8.
     END.
    
     IF INT(ENTRY(8,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-8:SCREEN-VALUE = ENTRY(18,char-val)
        lv-qty-8:SCREEN-VALUE  = ENTRY(8,char-val).
       RUN get-msf(integer(lv-qty-8:SCREEN-VALUE)) .
        lv-msf-8:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-9.
     END.
    
     IF INT(ENTRY(9,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-9:SCREEN-VALUE = ENTRY(19,char-val)
        lv-qty-9:SCREEN-VALUE  = ENTRY(9,char-val).
        RUN get-msf(integer(lv-qty-9:SCREEN-VALUE)) .
        lv-msf-9:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-10.
     END.
    
     IF INT(ENTRY(10,char-val)) NE 0 THEN DO:
       ASSIGN
        lv-rels-10:SCREEN-VALUE = ENTRY(20,char-val)
        lv-qty-10:SCREEN-VALUE  = ENTRY(10,char-val).
        RUN get-msf(integer(lv-qty-10:SCREEN-VALUE)) .
        lv-msf-10:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-11.
     END.
  END.

  IF char-val2 NE "?" THEN DO:
     IF INT(ENTRY(1,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-11:SCREEN-VALUE = ENTRY(11,char-val2)
        lv-qty-11:SCREEN-VALUE  = ENTRY(1,char-val2).
        RUN get-msf(integer(lv-qty-11:SCREEN-VALUE)) .
        lv-msf-11:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-12.
     END.
    
     IF INT(ENTRY(2,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-12:SCREEN-VALUE = ENTRY(12,char-val2)
        lv-qty-12:SCREEN-VALUE  = ENTRY(2,char-val2).
        RUN get-msf(integer(lv-qty-12:SCREEN-VALUE)) .
        lv-msf-12:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-13.
     END.
    
     IF INT(ENTRY(3,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-13:SCREEN-VALUE = ENTRY(13,char-val2)
        lv-qty-13:SCREEN-VALUE  = ENTRY(3,char-val2).
        RUN get-msf(integer(lv-qty-13:SCREEN-VALUE)) .
        lv-msf-13:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-14.
     END.
    
     IF INT(ENTRY(4,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-14:SCREEN-VALUE = ENTRY(14,char-val2)
        lv-qty-14:SCREEN-VALUE  = ENTRY(4,char-val2).
        RUN get-msf(integer(lv-qty-14:SCREEN-VALUE)) .
        lv-msf-14:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-15.
     END.
    
     IF INT(ENTRY(5,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-15:SCREEN-VALUE = ENTRY(15,char-val2)
        lv-qty-15:SCREEN-VALUE  = ENTRY(5,char-val2).
        RUN get-msf(integer(lv-qty-15:SCREEN-VALUE)) .
        lv-msf-15:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-16.
     END.
    
     IF INT(ENTRY(6,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-16:SCREEN-VALUE = ENTRY(16,char-val2)
        lv-qty-16:SCREEN-VALUE  = ENTRY(6,char-val2).
        RUN get-msf(integer(lv-qty-16:SCREEN-VALUE)) .
        lv-msf-16:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-17.
     END.
    
     IF INT(ENTRY(7,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-17:SCREEN-VALUE = ENTRY(17,char-val2)
        lv-qty-17:SCREEN-VALUE  = ENTRY(7,char-val2).
        RUN get-msf(integer(lv-qty-17:SCREEN-VALUE)) .
        lv-msf-17:SCREEN-VALUE = STRING(ld-msf).
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
        RUN get-msf(integer(lv-qty-19:SCREEN-VALUE)) .
        lv-msf-19:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-20.
     END.
    
     IF INT(ENTRY(10,char-val2)) NE 0 THEN DO:
       ASSIGN
        lv-rels-20:SCREEN-VALUE = ENTRY(20,char-val2)
        lv-qty-20:SCREEN-VALUE  = ENTRY(10,char-val2).
        RUN get-msf(integer(lv-qty-20:SCREEN-VALUE)) .
        lv-msf-20:SCREEN-VALUE = STRING(ld-msf).
       APPLY "entry" TO lv-qty-20.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME tb_run-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_run-21 D-Dialog
ON VALUE-CHANGED OF tb_run-21 IN FRAME D-Dialog /* Select All checkbox */
DO:
    IF tb_run-21:SCREEN-VALUE EQ "YES" THEN
        ASSIGN
        tb_run:SCREEN-VALUE = "YES"
        tb_run-2:SCREEN-VALUE = "YES"
        tb_run-3:SCREEN-VALUE = "YES"
        tb_run-4:SCREEN-VALUE = "YES"
        tb_run-5:SCREEN-VALUE = "YES"
        tb_run-6:SCREEN-VALUE = "YES"
        tb_run-7:SCREEN-VALUE = "YES"
        tb_run-8:SCREEN-VALUE = "YES"
        tb_run-9:SCREEN-VALUE = "YES"
        tb_run-10:SCREEN-VALUE = "YES"
        tb_run-11:SCREEN-VALUE = "YES"
        tb_run-12:SCREEN-VALUE = "YES"
        tb_run-13:SCREEN-VALUE = "YES"
        tb_run-14:SCREEN-VALUE = "YES"
        tb_run-15:SCREEN-VALUE = "YES"
        tb_run-16:SCREEN-VALUE = "YES"
        tb_run-17:SCREEN-VALUE = "YES"
        tb_run-18:SCREEN-VALUE = "YES"
        tb_run-19:SCREEN-VALUE = "YES"
        tb_run-20:SCREEN-VALUE = "YES".
    ELSE IF tb_run-21:SCREEN-VALUE EQ "No" THEN
        ASSIGN
        tb_run:SCREEN-VALUE = "No"
        tb_run-2:SCREEN-VALUE = "NO"
        tb_run-3:SCREEN-VALUE = "NO"
        tb_run-4:SCREEN-VALUE = "NO"
        tb_run-5:SCREEN-VALUE = "NO"
        tb_run-6:SCREEN-VALUE = "NO"
        tb_run-7:SCREEN-VALUE = "NO"
        tb_run-8:SCREEN-VALUE = "NO"
        tb_run-9:SCREEN-VALUE = "NO"
        tb_run-10:SCREEN-VALUE = "NO"
        tb_run-11:SCREEN-VALUE = "NO"
        tb_run-12:SCREEN-VALUE = "NO"
        tb_run-13:SCREEN-VALUE = "NO"
        tb_run-14:SCREEN-VALUE = "NO"
        tb_run-15:SCREEN-VALUE = "NO"
        tb_run-16:SCREEN-VALUE = "NO"
        tb_run-17:SCREEN-VALUE = "NO"
        tb_run-18:SCREEN-VALUE = "NO"
        tb_run-19:SCREEN-VALUE = "NO"
        tb_run-20:SCREEN-VALUE = "NO".
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
  DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
  
  DEF BUFFER b-reft FOR reftable.
  DEF BUFFER op-lock FOR reftable.
  DEFINE BUFFER bf-est-op FOR est-op.

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
  
  IF AVAILABLE xest AND (v-do-speed OR v-do-mr) THEN DO:
      FIND FIRST bf-est-op NO-LOCK 
        WHERE bf-est-op.company EQ xest.company
        AND bf-est-op.est-no EQ xest.est-no
        AND bf-est-op.isLocked
        NO-ERROR. 
      IF AVAILABLE bf-est-op THEN DO:
            MESSAGE "You have opted to recalculate machine standards yet the estimate has locked operations." SKIP 
            "Do you want to continue calculating using standards from machine file?"
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lContinue.
            IF NOT lContinue THEN 
                APPLY "choose" TO btn_cancel.
      END.
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
         .

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


FIND CURRENT xest EXCLUSIVE-LOCK NO-ERROR.  
ASSIGN xest.markupPct = io-v-match-up.  
FIND CURRENT xest NO-LOCK NO-ERROR.
 {est/op-lock.i xest}
  ASSIGN
   op-lock.val[1] = INT(io-do-speed)
   op-lock.val[2] = INT(io-do-mr).
 FIND FIRST est-qty EXCLUSIVE-LOCK
    WHERE est-qty.company EQ xest.company
      AND est-qty.est-no  EQ xest.est-no
    NO-ERROR.
 IF AVAIL est-qty THEN
     ASSIGN
     est-qty.whsed[1]  =   tb_run 
     est-qty.whsed[2]  =   tb_run-2 
     est-qty.whsed[3]  =   tb_run-3 
     est-qty.whsed[4]  =   tb_run-4 
     est-qty.whsed[5]  =   tb_run-5 
     est-qty.whsed[6]  =   tb_run-6 
     est-qty.whsed[7]  =   tb_run-7 
     est-qty.whsed[8]  =   tb_run-8 
     est-qty.whsed[9]  =   tb_run-9 
     est-qty.whsed[10] =   tb_run-10
     est-qty.whsed[11] =   tb_run-11
     est-qty.whsed[12] =   tb_run-12
     est-qty.whsed[13] =   tb_run-13
     est-qty.whsed[14] =   tb_run-14
     est-qty.whsed[15] =   tb_run-15
     est-qty.whsed[16] =   tb_run-16
     est-qty.whsed[17] =   tb_run-17
     est-qty.whsed[18] =   tb_run-18
     est-qty.whsed[19] =   tb_run-19
     est-qty.whsed[20] =   tb_run-20 .

 FIND CURRENT est-qty NO-LOCK NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-1 D-Dialog
ON VALUE-CHANGED OF lv-qty-1 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-1:SCREEN-VALUE)) .
  lv-msf-1:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-10 D-Dialog
ON VALUE-CHANGED OF lv-qty-10 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-10:SCREEN-VALUE)) .
  lv-msf-10:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-11 D-Dialog
ON VALUE-CHANGED OF lv-qty-11 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-11:SCREEN-VALUE)) .
  lv-msf-11:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-12 D-Dialog
ON VALUE-CHANGED OF lv-qty-12 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-12:SCREEN-VALUE)) .
  lv-msf-12:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-13 D-Dialog
ON VALUE-CHANGED OF lv-qty-13 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-13:SCREEN-VALUE)) .
  lv-msf-13:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-14 D-Dialog
ON VALUE-CHANGED OF lv-qty-14 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-14:SCREEN-VALUE)) .
  lv-msf-14:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-15 D-Dialog
ON VALUE-CHANGED OF lv-qty-15 IN FRAME D-Dialog
DO:
 RUN get-msf(integer(lv-qty-15:SCREEN-VALUE)) .
  lv-msf-15:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-16 D-Dialog
ON VALUE-CHANGED OF lv-qty-16 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-16:SCREEN-VALUE)) .
  lv-msf-16:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-17 D-Dialog
ON VALUE-CHANGED OF lv-qty-17 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-17:SCREEN-VALUE)) .
  lv-msf-17:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-18 D-Dialog
ON VALUE-CHANGED OF lv-qty-18 IN FRAME D-Dialog
DO:
 RUN get-msf(integer(lv-qty-18:SCREEN-VALUE)) .
  lv-msf-18:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-19 D-Dialog
ON VALUE-CHANGED OF lv-qty-19 IN FRAME D-Dialog
DO:
 RUN get-msf(integer(lv-qty-19:SCREEN-VALUE)) .
  lv-msf-19:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-2 D-Dialog
ON VALUE-CHANGED OF lv-qty-2 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-2:SCREEN-VALUE)) .
  lv-msf-2:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-20 D-Dialog
ON VALUE-CHANGED OF lv-qty-20 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-20:SCREEN-VALUE)) .
  lv-msf-20:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-3 D-Dialog
ON VALUE-CHANGED OF lv-qty-3 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-3:SCREEN-VALUE)) .
  lv-msf-3:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-4 D-Dialog
ON VALUE-CHANGED OF lv-qty-4 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-4:SCREEN-VALUE)) .
  lv-msf-4:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-5 D-Dialog
ON VALUE-CHANGED OF lv-qty-5 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-5:SCREEN-VALUE)) .
  lv-msf-5:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-6 D-Dialog
ON VALUE-CHANGED OF lv-qty-6 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-6:SCREEN-VALUE)) .
  lv-msf-6:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-7 D-Dialog
ON VALUE-CHANGED OF lv-qty-7 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-7:SCREEN-VALUE)) .
  lv-msf-7:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-8 D-Dialog
ON VALUE-CHANGED OF lv-qty-8 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-8:SCREEN-VALUE)) .
  lv-msf-8:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-9 D-Dialog
ON VALUE-CHANGED OF lv-qty-9 IN FRAME D-Dialog
DO:
  RUN get-msf(integer(lv-qty-9:SCREEN-VALUE)) .
  lv-msf-9:SCREEN-VALUE = STRING(ld-msf).
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

ASSIGN lv-match-up = xest.markupPct.

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
         .  
        IF lv-qty-1 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-1)) .
            lv-msf-1 = (ld-msf).
        END.
        IF lv-qty-2 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-2)) .
            lv-msf-2 = (ld-msf).
        END.
        IF lv-qty-3 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-3)) .
            lv-msf-3 = (ld-msf).
        END.
        IF lv-qty-4 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-4)) .
            lv-msf-4 = (ld-msf).
        END.
        IF lv-qty-5 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-5)) .
            lv-msf-5 = (ld-msf).
        END.
        IF lv-qty-6 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-6)) .
            lv-msf-6 = (ld-msf).
        END.
        IF lv-qty-7 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-7)) .
            lv-msf-7 = (ld-msf).
        END.
        IF lv-qty-8 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-8)) .
            lv-msf-8 = (ld-msf).
        END.
        IF lv-qty-9 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-9)) .
            lv-msf-9 = (ld-msf).
        END.
        IF lv-qty-10 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-10)) .
            lv-msf-10 = (ld-msf).
        END.
        IF lv-qty-11 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-11)) .
            lv-msf-11 = (ld-msf).
        END.
        IF lv-qty-12 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-12)) .
            lv-msf-12 = (ld-msf).
        END.
        IF lv-qty-13 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-13)) .
            lv-msf-13 = (ld-msf).
        END.
        IF lv-qty-14 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-14)) .
            lv-msf-14 = (ld-msf).
        END.
        IF lv-qty-15 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-15)) .
            lv-msf-15 = (ld-msf).
        END.
        IF lv-qty-16 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-16)) .
            lv-msf-16 = (ld-msf).
        END.
        IF lv-qty-17 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-17)) .
            lv-msf-17 = (ld-msf).
        END.
        IF lv-qty-18 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-18)) .
            lv-msf-18 = (ld-msf).
        END.
        IF lv-qty-19 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-19)) .
            lv-msf-19 = (ld-msf).
        END.
        IF lv-qty-20 NE 0 THEN do:
            RUN get-msf(integer(lv-qty-20)) .
            lv-msf-20 = (ld-msf).
        END.
IF AVAIL est-qty THEN
    ASSIGN
    tb_run = est-qty.whsed[1]
    tb_run-2 = est-qty.whsed[2]
    tb_run-3 = est-qty.whsed[3]
    tb_run-4 = est-qty.whsed[4]
    tb_run-5 = est-qty.whsed[5]
    tb_run-6 = est-qty.whsed[6]
    tb_run-7 = est-qty.whsed[7]
    tb_run-8 = est-qty.whsed[8]
    tb_run-9 = est-qty.whsed[9]
    tb_run-10 = est-qty.whsed[10]
    tb_run-11 = est-qty.whsed[11]
    tb_run-12 = est-qty.whsed[12]
    tb_run-13 = est-qty.whsed[13]
    tb_run-14 = est-qty.whsed[14]
    tb_run-15 = est-qty.whsed[15]
    tb_run-16 = est-qty.whsed[16]
    tb_run-17 = est-qty.whsed[17]
    tb_run-18 = est-qty.whsed[18]
    tb_run-19 = est-qty.whsed[19]
    tb_run-20 = est-qty.whsed[20] .
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
          lv-rels-20 v-do-speed v-do-mr v-do-gsa v-drop-rc v-ink-all-forms 
          lv-match-up v-est-list v-calc-board-cost-on-blank tb_run tb_run-2 
          tb_run-3 tb_run-4 tb_run-5 tb_run-6 tb_run-7 tb_run-8 tb_run-9 
          tb_run-10 tb_run-11 tb_run-12 tb_run-13 tb_run-14 tb_run-15 tb_run-16 
          tb_run-17 tb_run-18 tb_run-19 tb_run-20 tb_run-21 lv-msf-1 lv-msf-2 
          lv-msf-3 lv-msf-4 lv-msf-5 lv-msf-6 lv-msf-7 lv-msf-8 lv-msf-9 
          lv-msf-10 lv-msf-11 lv-msf-12 lv-msf-13 lv-msf-14 lv-msf-15 lv-msf-16 
          lv-msf-17 lv-msf-18 lv-msf-19 lv-msf-20 
      WITH FRAME D-Dialog.
  ENABLE lv-qty-1 lv-rels-1 lv-qty-2 lv-rels-2 lv-qty-3 lv-rels-3 lv-qty-4 
         lv-rels-4 lv-qty-5 lv-rels-5 lv-qty-6 lv-rels-6 lv-qty-7 lv-rels-7 
         lv-qty-8 lv-rels-8 lv-qty-9 lv-rels-9 lv-qty-10 lv-rels-10 lv-qty-11 
         lv-rels-11 lv-qty-12 lv-rels-12 lv-qty-13 lv-rels-13 lv-qty-14 
         lv-rels-14 lv-qty-15 lv-rels-15 lv-qty-16 lv-rels-16 lv-qty-17 
         lv-rels-17 lv-qty-18 lv-rels-18 lv-qty-19 lv-rels-19 lv-qty-20 
         lv-rels-20 btn-clear btn-upd v-do-speed v-do-mr v-do-gsa v-drop-rc 
         v-ink-all-forms lv-match-up v-est-list Btn_OK Btn_Cancel 
         v-calc-board-cost-on-blank RECT-1 RECT-23 RECT-24 tb_run tb_run-2 
         tb_run-3 tb_run-4 tb_run-5 tb_run-6 tb_run-7 tb_run-8 tb_run-9 
         tb_run-10 tb_run-11 tb_run-12 tb_run-13 tb_run-14 tb_run-15 tb_run-16 
         tb_run-17 tb_run-18 tb_run-19 tb_run-20 tb_run-21 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-msf D-Dialog 
PROCEDURE get-msf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipiQty AS INTEGER .
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ld-msf = DEC(ipiQty)
     ld-msf = (IF v-corr THEN (ld-msf * eb.t-len * eb.t-wid * .007)
                     ELSE (ld-msf * eb.t-len * eb.t-wid / 144)) *
          (IF eb.est-type EQ 2 THEN
             (IF eb.cust-% LT 0 THEN (-1 / eb.cust-%) ELSE eb.cust-%)
           ELSE
           IF eb.est-type EQ 6 THEN
             (IF eb.quantityPerSet LT 0 THEN -1 / (eb.quantityPerSet) ELSE eb.quantityPerSet)
           ELSE 1) / 1000.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

