&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: ce\v-est2.w

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
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i new shared}
DEF VAR ll-auto-calc-selected AS LOG NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

&scoped-define est-layout layout       /* for method enable */
&scoped-define PROC-ENABLE PROC-ENABLE  
DEF VAR lv-is-roll AS LOG NO-UNDO.
DEF VAR ls-lam-dscr AS cha NO-UNDO.
DEF VAR lv-is-foam AS LOG NO-UNDO.
DEF VAR lv-industry AS cha NO-UNDO.
DEF VAR ld-roll-wid AS DEC NO-UNDO.
DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xef FOR ef.
DEF NEW SHARED BUFFER xeb FOR eb.
DEF VAR uom-list AS cha NO-UNDO.
DEF VAR ll-is-sheet-calc AS LOG NO-UNDO.
DEF VAR ll-is-canceled AS LOG NO-UNDO.
DEF VAR ll-num-lw-changed AS LOG NO-UNDO.  /* if num-len, eb.num-wid changed */
DEF VAR ll-num-out-changed AS LOG NO-UNDO.  /* if n-out, eb.n-out-l changed */
DEF VAR ll-one-eb-on-ef AS LOG NO-UNDO.  
DEF VAR ll-one-ef-on-est AS LOG NO-UNDO.  

DEF BUFFER bf-est FOR est.
/* leaf/film browser */
DEF QUERY q-flm FOR est-flm SCROLLING. 
DEF VAR br-flm AS WIDGET-HANDLE NO-UNDO.
DEF VAR lh-ino AS WIDGET-HANDLE NO-UNDO.
DEF VAR lh-dscr AS WIDGET-HANDLE NO-UNDO.
DEF VAR lh-snum AS WIDGET-HANDLE NO-UNDO.
DEF VAR lh-bnum AS WIDGET-HANDLE NO-UNDO.
DEF VAR lh-len AS WIDGET-HANDLE NO-UNDO.
DEF VAR lh-wid AS WIDGET-HANDLE NO-UNDO.
DEF VAR lv-label AS CHAR EXTENT 10 NO-UNDO.
DEF VAR ll-tandem AS LOG NO-UNDO.
DEFINE VARIABLE gvl-first AS LOGICAL     NO-UNDO INIT TRUE.

DEF TEMP-TABLE w-eb NO-UNDO LIKE eb.
DEF TEMP-TABLE w-ef NO-UNDO LIKE ef.

DEF TEMP-TABLE old-ef NO-UNDO LIKE ef.
DEF VAR cRtnChar AS CHARACTER NO-UNDO.
DEF VAR lRecFound AS LOGICAL NO-UNDO .
DEF VAR lShtcalcWarm-log AS LOGICAL NO-UNDO .
DEFINE VARIABLE dCelayoutDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE lVendItemCost AS LOGICAL NO-UNDO.
DEFINE VARIABLE cCEVersion AS CHARACTER NO-UNDO.

{cec/bestfitc.i NEW SHARED}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fold

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est ef eb
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, ef, eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ef.m-code ef.lsh-wid ef.lsh-len ef.xgrain ~
ef.board ef.cost-uom ef.cost-msh ef.fr-uom ef.fr-msh ef.nc ef.roll ~
ef.gsh-wid ef.gsh-len ef.n-out ef.n-cuts ef.nsh-wid ef.nsh-len ef.n-out-l ~
ef.trim-w ef.trim-l eb.num-wid eb.num-len ef.die-in ef.leaf[1] ~
ef.leaf-dscr[1] ef.leaf-bnum[1] ef.leaf-w[1] ef.leaf-l[1] ef.leaf[2] ~
ef.leaf-dscr[2] ef.leaf-bnum[2] ef.leaf-w[2] ef.leaf-l[2] ef.leaf[3] ~
ef.leaf-dscr[3] ef.leaf-bnum[3] ef.leaf-w[3] ef.leaf-l[3] ef.leaf[4] ~
ef.leaf-dscr[4] ef.leaf-bnum[4] ef.leaf-w[4] ef.leaf-l[4] 
&Scoped-define ENABLED-TABLES ef eb
&Scoped-define FIRST-ENABLED-TABLE ef
&Scoped-define SECOND-ENABLED-TABLE eb
&Scoped-Define ENABLED-OBJECTS btn_board btn_cost RECT-20 RECT-21 RECT-9 
&Scoped-Define DISPLAYED-FIELDS ef.m-code ef.m-dscr ef.lsh-wid ef.lsh-len ~
ef.xgrain ef.board ef.brd-dscr ef.i-code ef.cal ef.cost-uom ef.cost-msh ~
ef.weight ef.fr-uom ef.fr-msh ef.nc ef.roll ef.roll-wid ef.gsh-wid ~
ef.gsh-len ef.n-out ef.n-cuts ef.nsh-wid ef.nsh-len ef.n-out-l ef.trim-w ~
ef.trim-l eb.num-wid eb.num-len eb.num-up ef.die-in eb.t-wid eb.t-len ~
eb.t-sqin ef.leaf[1] ef.leaf-dscr[1] ef.leaf-snum[1] ef.leaf-bnum[1] ~
ef.leaf-w[1] ef.leaf-l[1] ef.leaf[2] ef.leaf-dscr[2] ef.leaf-snum[2] ~
ef.leaf-bnum[2] ef.leaf-w[2] ef.leaf-l[2] ef.leaf[3] ef.leaf-dscr[3] ~
ef.leaf-snum[3] ef.leaf-bnum[3] ef.leaf-w[3] ef.leaf-l[3] ef.leaf[4] ~
ef.leaf-dscr[4] ef.leaf-snum[4] ef.leaf-bnum[4] ef.leaf-w[4] ef.leaf-l[4] 
&Scoped-define DISPLAYED-TABLES ef eb
&Scoped-define FIRST-DISPLAYED-TABLE ef
&Scoped-define SECOND-DISPLAYED-TABLE eb
&Scoped-Define DISPLAYED-OBJECTS dtCost OverrideExist 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS ef.m-code ef.m-dscr ef.brd-dscr ef.i-code ~
ef.cal ef.weight ef.roll-wid eb.num-wid eb.num-up ef.leaf-snum[1] ~
ef.leaf-snum[2] ef.leaf-snum[3] ef.leaf-snum[4] 
&Scoped-define List-5 ef.m-code ef.m-dscr ef.lsh-wid ef.lsh-len ef.xgrain ~
ef.board ef.brd-dscr ef.i-code ef.cal ef.cost-uom ef.cost-msh ef.weight ~
ef.fr-uom ef.fr-msh ef.nc ef.roll ef.roll-wid ef.gsh-wid ef.gsh-len ~
ef.n-out ef.n-cuts ef.nsh-wid ef.nsh-len ef.trim-w ef.trim-l ef.die-in 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAvailVendItemCost V-table-Win 
FUNCTION fAvailVendItemCost RETURNS LOGICAL
  (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetVendItemCostRollWidth V-table-Win 
FUNCTION fGetVendItemCostRollWidth RETURNS DECIMAL
    (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER, ipdRollWidth AS DECIMAL ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetVendItemCostUOM V-table-Win 
FUNCTION fGetVendItemCostUOM RETURNS CHARACTER
    (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER   ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_board 
     LABEL "" 
     SIZE 10 BY 1.

DEFINE BUTTON btn_cost 
     LABEL "Cost By Qty" 
     SIZE 15 BY 1.

DEFINE VARIABLE dtCost AS CHARACTER FORMAT "X(8)":U INITIAL "Cost/" 
     VIEW-AS FILL-IN 
     SIZE 7.4 BY 1 NO-UNDO.

DEFINE VARIABLE OverrideExist AS CHARACTER FORMAT "X(16)":U INITIAL "Overrides Exist" 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 147 BY 15.95.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143 BY 6.19.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 5.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fold
     ef.m-code AT ROW 1.48 COL 12 COLON-ALIGNED
          LABEL "Machine" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ef.m-dscr AT ROW 1.48 COL 30 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     ef.lsh-wid AT ROW 1.24 COL 84 COLON-ALIGNED HELP
          "This is the Machine Length, Pulled from Machine file"
          LABEL "Front-Back"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.lsh-len AT ROW 1.24 COL 111 COLON-ALIGNED HELP
          "This is the Machine Width, Pulled from Machine file"
          LABEL "Side-Side"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.xgrain AT ROW 1.24 COL 132 COLON-ALIGNED
          LABEL "Xgrain" FORMAT "X"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Normal","N",
                     "Blank","B",
                     "Sheet","S"
          DROP-DOWN-LIST
          SIZE 13 BY 1
     ef.board AT ROW 2.43 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ef.brd-dscr AT ROW 2.43 COL 28 COLON-ALIGNED NO-LABEL FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 63 BY 1
     ef.i-code AT ROW 2.19 COL 111 COLON-ALIGNED
          LABEL "Real"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     ef.cal AT ROW 3.38 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.cost-uom AT ROW 3.38 COL 48.9 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.5 BY 1
     ef.cost-msh AT ROW 3.38 COL 53.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     ef.weight AT ROW 3.38 COL 87.2 COLON-ALIGNED
          LABEL "Wt"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     ef.fr-uom AT ROW 3.38 COL 111 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ef.fr-msh AT ROW 3.38 COL 120 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.nc AT ROW 3.38 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     ef.roll AT ROW 5.76 COL 9
          LABEL "Roll"
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .81
     ef.roll-wid AT ROW 5.76 COL 17 COLON-ALIGNED NO-LABEL FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ef.gsh-wid AT ROW 6.71 COL 17 COLON-ALIGNED
          LABEL "Gross Sheet" FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ef.gsh-len AT ROW 6.71 COL 31 COLON-ALIGNED NO-LABEL FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ef.n-out AT ROW 6.71 COL 69 COLON-ALIGNED HELP
          "Enter number out on Gross Sheet"
          LABEL "#Out" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.n-cuts AT ROW 6.71 COL 96 COLON-ALIGNED
          LABEL "Cuts" FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.nsh-wid AT ROW 7.67 COL 17 COLON-ALIGNED
          LABEL "Mach Feed" FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ef.nsh-len AT ROW 7.67 COL 31 COLON-ALIGNED NO-LABEL FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ef.n-out-l AT ROW 7.67 COL 69 COLON-ALIGNED HELP
          "Enter number out on Net Sheet"
          LABEL "#Out" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fold
     ef.trim-w AT ROW 8.62 COL 17 COLON-ALIGNED
          LABEL "Die Size" FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ef.trim-l AT ROW 8.62 COL 31 COLON-ALIGNED NO-LABEL FORMAT ">>9.999999"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     eb.num-wid AT ROW 8.62 COL 69 COLON-ALIGNED
          LABEL "# On" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.num-len AT ROW 8.62 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.num-up AT ROW 8.62 COL 96 COLON-ALIGNED NO-LABEL FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.die-in AT ROW 8.62 COL 110 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     eb.t-wid AT ROW 9.57 COL 17 COLON-ALIGNED
          LABEL "Blank"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     eb.t-len AT ROW 9.57 COL 31 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     eb.t-sqin AT ROW 9.57 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.leaf[1] AT ROW 11.71 COL 4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[1] AT ROW 11.71 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[1] AT ROW 11.71 COL 46 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-bnum[1] AT ROW 11.71 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-w[1] AT ROW 11.71 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     ef.leaf-l[1] AT ROW 11.71 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.4 BY 1
     ef.leaf[2] AT ROW 12.91 COL 4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[2] AT ROW 12.91 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[2] AT ROW 12.91 COL 46 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-bnum[2] AT ROW 12.91 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-w[2] AT ROW 12.91 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     ef.leaf-l[2] AT ROW 12.91 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.4 BY 1
     ef.leaf[3] AT ROW 14.1 COL 4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[3] AT ROW 14.1 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[3] AT ROW 14.1 COL 46 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-bnum[3] AT ROW 14.1 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-w[3] AT ROW 14.1 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fold
     ef.leaf-l[3] AT ROW 14.1 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.4 BY 1
     ef.leaf[4] AT ROW 15.29 COL 4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[4] AT ROW 15.29 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[4] AT ROW 15.29 COL 46 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-bnum[4] AT ROW 15.29 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-w[4] AT ROW 15.29 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     ef.leaf-l[4] AT ROW 15.29 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.4 BY 1
     btn_board AT ROW 2.43 COL 3 WIDGET-ID 16
     btn_cost AT ROW 3.43 COL 68.2 WIDGET-ID 150
     dtCost AT ROW 3.38 COL 41.6 NO-LABEL WIDGET-ID 152
     OverrideExist AT ROW 3.38 COL 47.8 NO-LABEL WIDGET-ID 154
     "Die Inches" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 5.05 COL 110
     "Total Up" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 5.05 COL 95
     "Freight/" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 3.62 COL 111 RIGHT-ALIGNED
     "Width" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11 COL 65
          FGCOLOR 9 
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 5.05 COL 34
          FGCOLOR 1 
     "Width" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 5.05 COL 20
          FGCOLOR 1 
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11 COL 79
          FGCOLOR 9 
     "Sq. Inches" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 5.05 COL 50
     "Leaf/Film" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 11 COL 8
          FGCOLOR 9 
     "Description" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 11 COL 24
          FGCOLOR 9 
     "S  /  B" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 11 COL 50
          FGCOLOR 9 
     RECT-20 AT ROW 1 COL 1
     RECT-21 AT ROW 4.57 COL 3
     RECT-9 AT ROW 10.76 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.est,ASI.ef,ASI.eb
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 20.67
         WIDTH              = 147.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fold
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME fold:SCROLLABLE       = FALSE
       FRAME fold:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ef.board IN FRAME fold
   5                                                                    */
/* SETTINGS FOR FILL-IN ef.brd-dscr IN FRAME fold
   NO-ENABLE 2 5 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN ef.cal IN FRAME fold
   NO-ENABLE 2 5                                                        */
/* SETTINGS FOR FILL-IN ef.cost-msh IN FRAME fold
   5                                                                    */
/* SETTINGS FOR FILL-IN ef.cost-uom IN FRAME fold
   ALIGN-L 5 EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ef.die-in IN FRAME fold
   5                                                                    */
/* SETTINGS FOR FILL-IN dtCost IN FRAME fold
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN ef.fr-msh IN FRAME fold
   5                                                                    */
/* SETTINGS FOR FILL-IN ef.fr-uom IN FRAME fold
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN ef.gsh-len IN FRAME fold
   5 EXP-LABEL EXP-FORMAT                                               */
ASSIGN 
       ef.gsh-len:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.gsh-wid IN FRAME fold
   5 EXP-LABEL EXP-FORMAT                                               */
ASSIGN 
       ef.gsh-wid:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.i-code IN FRAME fold
   NO-ENABLE 2 5 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN ef.leaf-bnum[1] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.leaf-bnum[2] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.leaf-bnum[3] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.leaf-bnum[4] IN FRAME fold
   EXP-FORMAT                                                           */
ASSIGN 
       ef.leaf-l[1]:PRIVATE-DATA IN FRAME fold     = 
                "16th".

ASSIGN 
       ef.leaf-l[2]:PRIVATE-DATA IN FRAME fold     = 
                "16th".

ASSIGN 
       ef.leaf-l[3]:PRIVATE-DATA IN FRAME fold     = 
                "16th".

ASSIGN 
       ef.leaf-l[4]:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.leaf-snum[1] IN FRAME fold
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.leaf-snum[2] IN FRAME fold
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.leaf-snum[3] IN FRAME fold
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.leaf-snum[4] IN FRAME fold
   NO-ENABLE 2 EXP-FORMAT                                               */
ASSIGN 
       ef.leaf-w[1]:PRIVATE-DATA IN FRAME fold     = 
                "16th".

ASSIGN 
       ef.leaf-w[2]:PRIVATE-DATA IN FRAME fold     = 
                "16th".

ASSIGN 
       ef.leaf-w[3]:PRIVATE-DATA IN FRAME fold     = 
                "16th".

ASSIGN 
       ef.leaf-w[4]:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.lsh-len IN FRAME fold
   5 EXP-LABEL EXP-HELP                                                 */
/* SETTINGS FOR FILL-IN ef.lsh-wid IN FRAME fold
   5 EXP-LABEL EXP-HELP                                                 */
/* SETTINGS FOR FILL-IN ef.m-code IN FRAME fold
   2 5 EXP-LABEL EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN ef.m-dscr IN FRAME fold
   NO-ENABLE ALIGN-L 2 5 EXP-LABEL                                      */
/* SETTINGS FOR FILL-IN ef.n-cuts IN FRAME fold
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.n-out IN FRAME fold
   5 EXP-LABEL EXP-FORMAT EXP-HELP                                      */
/* SETTINGS FOR FILL-IN ef.n-out-l IN FRAME fold
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN ef.nc IN FRAME fold
   5                                                                    */
/* SETTINGS FOR FILL-IN ef.nsh-len IN FRAME fold
   5 EXP-LABEL EXP-FORMAT                                               */
ASSIGN 
       ef.nsh-len:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.nsh-wid IN FRAME fold
   5 EXP-LABEL EXP-FORMAT                                               */
ASSIGN 
       ef.nsh-wid:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN eb.num-len IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.num-up IN FRAME fold
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.num-wid IN FRAME fold
   2 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN OverrideExist IN FRAME fold
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       OverrideExist:HIDDEN IN FRAME fold           = TRUE.

/* SETTINGS FOR TOGGLE-BOX ef.roll IN FRAME fold
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN ef.roll-wid IN FRAME fold
   NO-ENABLE 2 5 EXP-FORMAT                                             */
/* SETTINGS FOR FILL-IN eb.t-len IN FRAME fold
   NO-ENABLE                                                            */
ASSIGN 
       eb.t-len:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN eb.t-sqin IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.t-wid IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       eb.t-wid:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.trim-l IN FRAME fold
   5 EXP-LABEL EXP-FORMAT                                               */
ASSIGN 
       ef.trim-l:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.trim-w IN FRAME fold
   5 EXP-LABEL EXP-FORMAT                                               */
ASSIGN 
       ef.trim-w:PRIVATE-DATA IN FRAME fold     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.weight IN FRAME fold
   NO-ENABLE 2 5 EXP-LABEL                                              */
/* SETTINGS FOR COMBO-BOX ef.xgrain IN FRAME fold
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR TEXT-LITERAL "Freight/"
          SIZE 10 BY .62 AT ROW 3.62 COL 111 RIGHT-ALIGNED              */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fold
/* Query rebuild information for FRAME fold
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME fold */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fold V-table-Win
ON HELP OF FRAME fold
DO:
   DEF VAR lv-handle AS WIDGET-HANDLE NO-UNDO.
   DEF VAR lv-ind LIKE style.industry NO-UNDO.
   DEF VAR ls-cur-val AS cha NO-UNDO.
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR lv-rowid AS ROWID NO-UNDO.
   DEF VAR lw-focus AS WIDGET NO-UNDO.


   lw-focus = FOCUS.

   CASE lw-focus:NAME :
     WHEN "Board" THEN DO:
           FIND style WHERE style.company = eb.company AND
                            style.style = eb.style
                            NO-LOCK NO-ERROR.   
           IF AVAIL style THEN lv-ind = style.industry.
           ELSE lv-ind = "".  
           IF AVAILABLE style AND style.type EQ "f" THEN DO: /* foam */
              RUN AOA/dynLookupSetParam.p (70, ROWID(style), OUTPUT char-val).
              ef.board:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("sfDynLookupValue", "i-no", char-val).
              RUN new-board.
              APPLY "ENTRY":U TO ef.board.
           END. /* if foam */
           ELSE DO:
               RUN windows/l-board1.w (eb.company,lv-ind,lw-focus:SCREEN-VALUE, OUTPUT lv-rowid).
               FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
               IF AVAIL ITEM AND ITEM.i-no NE lw-focus:SCREEN-VALUE THEN DO:
                 ef.board:SCREEN-VALUE IN FRAME {&frame-name} = item.i-no.
                 RUN new-board.                       
               END.
           END.  
     END.
     WHEN "leaf" THEN DO:
           FIND style WHERE style.company = eb.company AND
                            style.style = eb.style
                            NO-LOCK NO-ERROR.   
           IF AVAIL style THEN lv-ind = style.industry.
           ELSE lv-ind = "".  
           RUN windows/l-item.w (eb.company,lv-ind,"F,W",lw-focus:SCREEN-VALUE, OUTPUT char-val).
           IF char-val NE "" AND entry(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
              APPLY "value-changed" TO lw-focus.                         
           END.  /* char-val */ 
     END.  /* leaf */
     WHEN "leaf-snum" THEN DO:
       /*  RUN windows/l-form.w (gcompany,ef.est-no,ef.eqty, OUTPUT char-val). */
         RUN windows/l-blank.w (gcompany,ef.est-no,ef.eqty, OUTPUT char-val). 
         IF char-val <> "" THEN DO:
            lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
            CASE lw-focus:INDEX:
                WHEN 1 THEN ef.leaf-bnum[1]:SCREEN-VALUE = ENTRY(2,char-val).
                WHEN 2 THEN ef.leaf-bnum[2]:SCREEN-VALUE = ENTRY(2,char-val).
                WHEN 3 THEN ef.leaf-bnum[3]:SCREEN-VALUE = ENTRY(2,char-val).
                WHEN 4 THEN ef.leaf-bnum[4]:SCREEN-VALUE = ENTRY(2,char-val).
            END CASE.
         END.
     END.
     WHEN "leaf-bnum" THEN DO:                             /*ef.form-no*/
         RUN windows/l-blank.w (gcompany,ef.est-no,ef.eqty, OUTPUT char-val).
         IF char-val <> "" THEN DO:
            lw-focus:SCREEN-VALUE = ENTRY(2,char-val).
            CASE lw-focus:INDEX:
                WHEN 1 THEN ef.leaf-snum[1]:SCREEN-VALUE = ENTRY(1,char-val).
                WHEN 2 THEN ef.leaf-snum[2]:SCREEN-VALUE = ENTRY(1,char-val).
                WHEN 3 THEN ef.leaf-snum[3]:SCREEN-VALUE = ENTRY(1,char-val).
                WHEN 4 THEN ef.leaf-snum[4]:SCREEN-VALUE = ENTRY(1,char-val).
            END CASE.

         END.
     END.
     WHEN "m-code" THEN DO:
          RUN windows/l-mach.w (gcompany,ef.loc, lw-focus:SCREEN-VALUE, OUTPUT char-val).

          IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO: 
             lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
             RUN new-m-code.
          END.        
     END.
     WHEN "cost-uom" THEN DO:
          FIND FIRST item WHERE item.company = gcompany AND
                                item.i-no = ef.board:screen-value
                                NO-LOCK NO-ERROR.
/*          if avail item then find first e-item of item no-lock no-error.*/
          RUN sys/ref/uom-rm.p  (item.mat-type, OUTPUT uom-list).
          RUN windows/l-stduom.w (gcompany,uom-list, lw-focus:SCREEN-VALUE, OUTPUT char-val).
          IF char-val <> "" THEN 
             ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                    .
     END.
     WHEN "fr-uom" THEN DO:
          uom-list = "CWT,MSF,MSH,TON".
          RUN windows/l-stduom.w (gcompany,uom-list, lw-focus:SCREEN-VALUE, OUTPUT char-val).
          IF char-val <> "" THEN 
             ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                    .
     END.
     WHEN "gsh-len" THEN DO:
         IF ef.m-code:SCREEN-VALUE NE "" AND ef.roll:SCREEN-VALUE EQ "Y" THEN DO:
           RUN windows/l-machar.w (gcompany,ef.m-code:SCREEN-VALUE,"Cylinder Diameter (Front-To-Back)",lw-focus:SCREEN-VALUE, OUTPUT char-val).
           IF char-val NE "" THEN
             lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
         END.
     END.
     OTHERWISE DO:
           lv-handle = lw-focus:HANDLE.
           RUN applhelp.p.

           IF g_lookup-var <> "" THEN lw-focus:SCREEN-VALUE = g_lookup-var.
     END.  /* otherwise */
  END CASE.

  APPLY "entry" TO lw-focus.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board V-table-Win
ON LEAVE OF ef.board IN FRAME fold /* Board */
DO:
   IF LASTKEY <> -1 AND SELF:screen-value <> "" 

   THEN DO:
{&methods/lValidateError.i YES}
       FIND FIRST item WHERE item.company = gcompany AND
                             ((INDEX("BPR",item.mat-type) > 0 AND NOT lv-is-foam) OR
                              (INDEX("1234",item.mat-type) > 0 AND lv-is-foam) ) AND
                              item.industry = lv-industry AND
                              item.i-no = SELF:screen-value
                              NO-LOCK NO-ERROR.
       IF NOT AVAIL item THEN DO:
          MESSAGE "Invalid Board. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       IF item.i-code = "R" AND ll-auto-calc-selected THEN DO:
          IF item.r-wid = 0 THEN DO:                     
             IF item.s-wid < eb.t-wid THEN DO:
                MESSAGE "Sheet Width less than Blank Width. " VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
             END.
             IF item.s-len < eb.t-len THEN DO:
                MESSAGE "Sheet Length less than Blank Length. " VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
             END.
             /*IF ITEM.s-wid > Dec(EF.lsh-wid:SCREEN-VALUE IN FRAME {&FRAME-NAME}) OR
                ITEM.s-len > Dec(EF.lsh-len:SCREEN-VALUE IN FRAME {&FRAME-NAME})
             THEN DO:
                 MESSAGE "Sheet would not fit on the machine." VIEW-AS ALERT-BOX.
                 RETURN NO-APPLY.
             END.*/
          END.  /* r-wid = 0 */
          ELSE IF item.r-wid < eb.t-wid THEN DO:
                MESSAGE "Roll Width less than Blank Width. " VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
          END.
       END.

       IF  ll-auto-calc-selected AND ll-one-eb-on-ef THEN DO:
           FIND xef WHERE RECID(xef) = recid(ef).
           FIND xeb WHERE RECID(xeb) = recid(eb).
           ASSIGN xef.m-code = ef.m-code:screen-value
              xef.lsh-lock = NO
              xef.board = ef.board:SCREEN-VALUE
              xef.xgrain = ef.xgrain:screen-value IN FRAME {&frame-name}.
              xef.roll   = ef.roll:screen-value IN FRAME {&frame-name} EQ "Y".
              .

           RUN ce/calc-dim.p.
           FIND xef WHERE RECID(xef) = recid(ef).
           FIND xeb WHERE RECID(xeb) = recid(eb).
           ASSIGN ef.lsh-len:screen-value = STRING( xef.lsh-len )
              ef.lsh-wid:screen-value = STRING(xef.lsh-wid )
              ef.gsh-len:screen-value = STRING(xef.gsh-len )
              ef.gsh-wid:screen-value = STRING( xef.gsh-wid )
              ef.nsh-len:screen-value = STRING( xef.nsh-len )
              ef.nsh-wid:screen-value = STRING( xef.nsh-wid )
              ef.trim-l:screen-value = STRING( xef.trim-l )
              ef.trim-w:screen-value = STRING( xef.trim-w )
              ef.n-out:screen-value = STRING(xef.n-out)
              ef.n-cuts:screen-value = STRING(xef.n-cuts)
              ef.roll:SCREEN-VALUE = IF xef.roll THEN "Y" ELSE "N"
              ef.roll-wid:SCREEN-VALUE = IF xef.roll THEN STRING(xef.roll-wid) ELSE ""
              lv-is-roll = xef.roll
              eb.num-wid:screen-value = STRING(xeb.num-wid)
              eb.num-len:screen-value = STRING(xeb.num-len)
              eb.num-up:screen-value = STRING(xeb.num-up).
       END. /* ll-auto-calc-selected */
{&methods/lValidateError.i NO}
   END.  /* lastkey <> -1 */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board V-table-Win
ON VALUE-CHANGED OF ef.board IN FRAME fold /* Board */
DO:
  RUN new-board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_board V-table-Win
ON CHOOSE OF btn_board IN FRAME fold
DO:
  IF AVAIL eb THEN
   FIND FIRST ITEM WHERE style.company  = cocode
       AND ITEM.i-no = ef.board NO-LOCK NO-ERROR.

   IF AVAIL ITEM THEN
   RUN windows/item-fe.w(RECID(ITEM)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_cost V-table-Win
ON CHOOSE OF btn_cost IN FRAME fold /* Cost By Qty */
DO:
  IF AVAIL eb THEN
   FIND FIRST ITEM WHERE ITEM.company  = cocode
       AND ITEM.i-no = ef.board NO-LOCK NO-ERROR.

   IF AVAIL ITEM THEN do:
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostSourceFrom = "MF"' ).
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostEst# = ' + est.est-no).
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCost = ' + item.i-no).          
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostType = "RM" ' ).      
      /*RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostVendor = ' + item.vend-no).*/
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostCustomer = ""').  
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostForm# = ' + ( IF AVAIL eb THEN string(eb.form-no) ELSE "" ) ).
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostBlank# = 0').
      
      RUN windows/vendcostmtx.w .
      RUN pShowHideCostFiled.
   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.cost-uom V-table-Win
ON LEAVE OF ef.cost-uom IN FRAME fold /* cost-uom */
DO:
    IF LASTKEY <> -1 AND ef.cost-uom:screen-value <> "" THEN DO:
{&methods/lValidateError.i YES}
       FIND FIRST item WHERE item.company = gcompany AND
                                item.i-no = ef.board
                                NO-LOCK NO-ERROR.
/*       if avail item then find first e-item of item no-lock no-error.*/
       RUN sys/ref/uom-rm.p  (item.mat-type, OUTPUT uom-list).
       IF NOT CAN-FIND(FIRST uom WHERE LOOKUP(uom.uom,uom-list) > 0 AND
                            uom.uom = ef.cost-uom:screen-value)
       THEN DO:
            MESSAGE "Invalid Cost UOM. Try Help."   VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
       END. 
{&methods/lValidateError.i NO}                
    END.  
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.fr-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.fr-uom V-table-Win
ON LEAVE OF ef.fr-uom IN FRAME fold /* fr-uom */
DO:
    IF LASTKEY <> -1 AND ef.cost-uom:screen-value <> "" THEN DO:
{&methods/lValidateError.i YES}
       /*find first item where item.company = gcompany and
                                item.i-no = ef.board:screen-value
                                no-lock no-error.
       if avail item then find first e-item of item no-lock no-error.
       run sys/ref/uom-rm.p  (item.mat-type, output uom-list).
       */
       uom-list = "CWT,MSF,MSH,TON".
       IF NOT CAN-FIND(FIRST uom WHERE LOOKUP(uom.uom,uom-list) > 0 AND
                            uom.uom = ef.fr-uom:screen-value)
       THEN DO:
            MESSAGE "Invalid Freight UOM. Try Help." VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
       END.  
{&methods/lValidateError.i NO}               
    END.  

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.gsh-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.gsh-len V-table-Win
ON LEAVE OF ef.gsh-len IN FRAME fold /* Length */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-gsh-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.gsh-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.gsh-wid V-table-Win
ON LEAVE OF ef.gsh-wid IN FRAME fold /* Gross Sheet */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-gsh-wid NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-bnum[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-bnum[1] V-table-Win
ON LEAVE OF ef.leaf-bnum[1] IN FRAME fold /* leaf-bnum */
DO:
  IF LASTKEY NE -1 THEN DO:  
    RUN valid-leaf-bnum (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-bnum[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-bnum[2] V-table-Win
ON LEAVE OF ef.leaf-bnum[2] IN FRAME fold /* leaf-bnum */
DO:
  IF LASTKEY NE -1 THEN DO:   
    RUN valid-leaf-bnum (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-bnum[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-bnum[3] V-table-Win
ON LEAVE OF ef.leaf-bnum[3] IN FRAME fold /* leaf-bnum */
DO:
  IF LASTKEY NE -1 THEN DO:  
    RUN valid-leaf-bnum (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-bnum[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-bnum[4] V-table-Win
ON LEAVE OF ef.leaf-bnum[4] IN FRAME fold /* leaf-bnum */
DO:
  IF LASTKEY NE -1 THEN DO:  
    RUN valid-leaf-bnum (4) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-l[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-l[1] V-table-Win
ON LEAVE OF ef.leaf-l[1] IN FRAME fold /* Length[1] */
DO:
     IF LASTKEY = -1 THEN RETURN.
 {&methods/lValidateError.i YES}
  IF ef.leaf[1]:SCREEN-VALUE <> "" AND
       INT(SELF:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.

{&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-l[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-l[2] V-table-Win
ON LEAVE OF ef.leaf-l[2] IN FRAME fold /* Length[2] */
DO:
      IF LASTKEY = -1 THEN RETURN.
  {&methods/lValidateError.i YES}
  IF ef.leaf[2]:SCREEN-VALUE <> "" AND
       INT(SELF:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
{&methods/lValidateError.i NO}


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-l[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-l[3] V-table-Win
ON LEAVE OF ef.leaf-l[3] IN FRAME fold /* Length[3] */
DO:
       IF LASTKEY = -1 THEN RETURN.
  {&methods/lValidateError.i YES}
  IF ef.leaf[3]:SCREEN-VALUE <> "" AND
       INT(SELF:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.

{&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-l[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-l[4] V-table-Win
ON LEAVE OF ef.leaf-l[4] IN FRAME fold /* Length[4] */
DO:
       IF LASTKEY = -1 THEN RETURN.
  {&methods/lValidateError.i YES}

  IF ef.leaf[4]:SCREEN-VALUE <> "" AND
       INT(SELF:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.

{&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-snum[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-snum[1] V-table-Win
ON LEAVE OF ef.leaf-snum[1] IN FRAME fold /* S/B[1] */
DO:
  IF LASTKEY NE -1 THEN DO:  
    RUN valid-leaf-snum (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.   
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-snum[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-snum[2] V-table-Win
ON LEAVE OF ef.leaf-snum[2] IN FRAME fold /* S/B[2] */
DO:
  IF LASTKEY NE -1 THEN DO:  
    RUN valid-leaf-snum (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-snum[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-snum[3] V-table-Win
ON LEAVE OF ef.leaf-snum[3] IN FRAME fold /* S/B[3] */
DO:
  IF LASTKEY NE -1 THEN DO:
      RUN valid-leaf-snum (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-snum[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-snum[4] V-table-Win
ON LEAVE OF ef.leaf-snum[4] IN FRAME fold /* S/B[4] */
DO:
  IF LASTKEY NE -1 THEN DO:  
    RUN valid-leaf-snum (4) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-w[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-w[1] V-table-Win
ON LEAVE OF ef.leaf-w[1] IN FRAME fold /* Width[1] */
DO:
    IF LASTKEY = -1 THEN RETURN.
   {&methods/lValidateError.i YES}
   IF ef.leaf[1]:SCREEN-VALUE <> "" AND
       INT(SELF:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-w[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-w[2] V-table-Win
ON LEAVE OF ef.leaf-w[2] IN FRAME fold /* Width[2] */
DO:
    IF LASTKEY = -1 THEN RETURN.
  {&methods/lValidateError.i YES}
    IF ef.leaf[2]:SCREEN-VALUE <> "" AND
       INT(SELF:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
{&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-w[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-w[3] V-table-Win
ON LEAVE OF ef.leaf-w[3] IN FRAME fold /* Width[3] */
DO:
  IF LASTKEY = -1 THEN RETURN.
 {&methods/lValidateError.i YES}
  IF ef.leaf[3]:SCREEN-VALUE <> "" AND
       INT(SELF:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
{&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-w[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-w[4] V-table-Win
ON LEAVE OF ef.leaf-w[4] IN FRAME fold /* Width[4] */
DO:
    IF LASTKEY = -1 THEN RETURN.
{&methods/lValidateError.i YES}

  IF ef.leaf[4]:SCREEN-VALUE <> "" AND
       INT(SELF:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
  END.
{&methods/lValidateError.i NO}


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[1] V-table-Win
ON LEAVE OF ef.leaf[1] IN FRAME fold /* Leaf/Film[1] */
DO:
  IF LASTKEY NE -1 THEN DO:
   {&methods/lValidateError.i YES}
    RUN est/val-leaf.p (FRAME {&FRAME-NAME}:HANDLE, 1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[1] V-table-Win
ON VALUE-CHANGED OF ef.leaf[1] IN FRAME fold /* Leaf/Film[1] */
DO:
  {est/new-leaf.i 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[2] V-table-Win
ON LEAVE OF ef.leaf[2] IN FRAME fold /* Leaf/Film[2] */
DO:
  IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}
    RUN est/val-leaf.p (FRAME {&FRAME-NAME}:HANDLE, 2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[2] V-table-Win
ON VALUE-CHANGED OF ef.leaf[2] IN FRAME fold /* Leaf/Film[2] */
DO:
  {est/new-leaf.i 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[3] V-table-Win
ON LEAVE OF ef.leaf[3] IN FRAME fold /* Leaf/Film[3] */
DO:
  IF LASTKEY NE -1 THEN DO:
   {&methods/lValidateError.i YES}
    RUN est/val-leaf.p (FRAME {&FRAME-NAME}:HANDLE, 3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[3] V-table-Win
ON VALUE-CHANGED OF ef.leaf[3] IN FRAME fold /* Leaf/Film[3] */
DO:
  {est/new-leaf.i 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[4] V-table-Win
ON LEAVE OF ef.leaf[4] IN FRAME fold /* Leaf/Film[4] */
DO:
  IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}
    RUN est/val-leaf.p (FRAME {&FRAME-NAME}:HANDLE, 4) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[4] V-table-Win
ON VALUE-CHANGED OF ef.leaf[4] IN FRAME fold /* Leaf/Film[4] */
DO:
  {est/new-leaf.i 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.m-code V-table-Win
ON LEAVE OF ef.m-code IN FRAME fold /* Machine */
DO:

    IF LASTKEY = -1 THEN RETURN.

    IF ll-is-sheet-calc THEN DO:
          DEF VAR source-str AS cha NO-UNDO.   
          RUN get-link-handle IN adm-broker-hdl 
              (THIS-PROCEDURE, 'Tableio-source':U, OUTPUT source-str).
          RUN apply-sheet-calc IN WIDGET-HANDLE(source-str).                 
          RETURN.
    END.
    IF ef.m-code:screen-value = "" THEN DO:
       ASSIGN ef.m-dscr:screen-value = ""
              ef.lsh-wid:screen-value = "0"
              ef.lsh-len:screen-value = "0".
    END.

    IF LASTKEY <> -1 AND ef.m-code:screen-value <> "" AND
       NOT CAN-FIND (FIRST mach WHERE mach.company = gcompany AND
                                      mach.loc = eb.loc AND
                                      mach.m-code = ef.m-code:screen-value)
    THEN DO:
    {&methods/lValidateError.i YES}
         MESSAGE "Invalid Machine Code. Try Help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    {&methods/lValidateError.i NO}
    END.

    IF ll-auto-calc-selected THEN RUN auto-calc2.  /* from ce/uest2.p */
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.m-code V-table-Win
ON VALUE-CHANGED OF ef.m-code IN FRAME fold /* Machine */
DO:
  RUN new-m-code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.n-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out V-table-Win
ON LEAVE OF ef.n-out IN FRAME fold /* #Out */
DO:
    IF LASTKEY = -1 THEN RETURN.
   {&methods/lValidateError.i YES}

    IF AVAIL item AND item.i-code = "R" THEN DO:
       IF dec(SELF:screen-value) > ef.n-out THEN DO:
          MESSAGE "Cannot be greater than what was calculated." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
    END.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out V-table-Win
ON VALUE-CHANGED OF ef.n-out IN FRAME fold /* #Out */
DO:
  RUN n-out-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.nsh-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.nsh-len V-table-Win
ON LEAVE OF ef.nsh-len IN FRAME fold /* Length */
DO:


   IF LASTKEY <> -1 AND 
       ll-auto-calc-selected AND dec(SELF:screen-value) < dec(ef.trim-l:screen-value) 
   THEN DO:
   {&methods/lValidateError.i YES}
      MESSAGE "Net Sheet Size can not be less than Die Size." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY. 
   {&methods/lValidateError.i NO}    
   END.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.nsh-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.nsh-wid V-table-Win
ON LEAVE OF ef.nsh-wid IN FRAME fold /* Mach Feed */
DO:

   IF LASTKEY <> -1 AND 
       ll-auto-calc-selected AND dec(SELF:screen-value) < dec(ef.trim-w:screen-value) 
   THEN DO:
   {&methods/lValidateError.i YES}
      MESSAGE "Net Sheet Size can not be less than Die Size." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY. 
   {&methods/lValidateError.i NO}    
   END.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-len V-table-Win
ON VALUE-CHANGED OF eb.num-len IN FRAME fold /* # on Length */
DO:
  RUN num-wid-len-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-wid V-table-Win
ON VALUE-CHANGED OF eb.num-wid IN FRAME fold /* # On */
DO:
  RUN num-wid-len-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.roll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.roll V-table-Win
ON VALUE-CHANGED OF ef.roll IN FRAME fold /* Roll */
DO:
  FIND FIRST mach
      WHERE mach.company EQ gcompany
        AND mach.loc     EQ eb.loc
        AND mach.m-code  EQ ef.m-code:SCREEN-VALUE
      USE-INDEX m-code NO-LOCK NO-ERROR.
  IF AVAIL mach AND mach.p-type EQ "R" THEN {&self-name}:SCREEN-VALUE = "Y".

  RUN roll-display.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.trim-l
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.trim-l V-table-Win
ON LEAVE OF ef.trim-l IN FRAME fold /* Trim Length */
DO:

   IF LASTKEY <> -1 AND 
       ll-auto-calc-selected AND dec(SELF:screen-value) < dec(eb.t-len:screen-value) 
   THEN DO:
   {&methods/lValidateError.i YES}
      MESSAGE "Die Size can not be less than Blank Size." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.  
   {&methods/lValidateError.i NO}   
   END.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.trim-w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.trim-w V-table-Win
ON LEAVE OF ef.trim-w IN FRAME fold /* Die Size */
DO:


   IF LASTKEY <> -1 AND
       ll-auto-calc-selected AND dec(SELF:screen-value) < dec(eb.t-wid:screen-value) 
   THEN DO:
   {&methods/lValidateError.i YES}
      MESSAGE "Die Size can not be less than Blank Size." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY. 
    {&methods/lValidateError.i NO}
   END.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.xgrain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.xgrain V-table-Win
ON RETURN OF ef.xgrain IN FRAME fold /* Xgrain */
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.xgrain V-table-Win
ON VALUE-CHANGED OF ef.xgrain IN FRAME fold /* Xgrain */
DO:
  IF ll-one-eb-on-ef THEN DO:
     FIND xef WHERE RECID(xef) = recid(ef).
     FIND xeb WHERE RECID(xeb) = recid(eb).
     ASSIGN
      xef.xgrain = ef.xgrain:screen-value IN FRAME {&frame-name}.
      xef.roll   = ef.roll:screen-value IN FRAME {&frame-name} EQ "Y".
     RUN ce/calc-dim.p.
     FIND xef WHERE RECID(xef) = recid(ef).
     FIND xeb WHERE RECID(xeb) = recid(eb).
     ASSIGN ef.lsh-len:screen-value = STRING(xef.lsh-len )
              ef.lsh-wid:screen-value = STRING(xef.lsh-wid )
              ef.gsh-len:screen-value = STRING(xef.gsh-len )
              ef.gsh-wid:screen-value = STRING(xef.gsh-wid )
              ef.nsh-len:screen-value = STRING(xef.nsh-len )
              ef.nsh-wid:screen-value = STRING(xef.nsh-wid )
              ef.trim-l:screen-value = STRING(xef.trim-l )
              ef.trim-w:screen-value = STRING(xef.trim-w )
              ef.n-out:screen-value = STRING(xef.n-out)
              ef.n-cuts:screen-value = STRING(xef.n-cuts)
              eb.num-wid:screen-value = STRING(xeb.num-wid)
              eb.num-len:screen-value = STRING(xeb.num-len)
              eb.num-up:screen-value = STRING(xeb.num-up).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
ASSIGN cocode = gcompany
       locode = gloc.       

RUN sys/ref/nk1look.p (INPUT cocode, "CELAYOUT", "D" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    dCelayoutDec = decimal(cRtnChar) NO-ERROR. 
    
RUN sys/ref/nk1look.p (INPUT cocode, "VendItemCost", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lVendItemCost = logical(cRtnChar) NO-ERROR.
    
RUN sys/ref/nk1look.p (INPUT cocode, "CEVersion", "C" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cCEVersion = cRtnChar NO-ERROR.    
  
{sys/inc/vendItemCost.i}
SESSION:DATA-ENTRY-RETURN = YES.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "est"}
  {src/adm/template/row-list.i "ef"}
  {src/adm/template/row-list.i "eb"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "ef"}
  {src/adm/template/row-find.i "eb"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-calc V-table-Win 
PROCEDURE auto-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  

   {custom/checkuse.i}

   ll-auto-calc-selected = YES.

   FIND FIRST item WHERE item.company = gcompany AND
                         item.i-no = ef.board
                         NO-LOCK NO-ERROR.

   IF NOT lv-is-foam OR item.i-code = "E" THEN DO:
      FIND FIRST mach WHERE mach.company = gcompany AND
                            mach.loc = eb.loc AND
                            mach.m-code = ef.m-code
                            USE-INDEX m-code NO-LOCK NO-ERROR.
        IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
           ASSIGN ef.nsh-wid:screen-value IN FRAME {&frame-name} = STRING(ef.nsh-wid - (2 * mach.min-triml))
                  ef.nsh-len:screen-value = STRING(ef.nsh-len - (2 * mach.min-trimw)).   
        ASSIGN ef.n-out:screen-value   = STRING(trunc(ef.lsh-len / ef.nsh-wid,0))
               .
   END.

   ASSIGN ef.roll:SCREEN-VALUE     = STRING(ITEM.r-wid GT 0,"Y/N")
          ef.roll-wid:SCREEN-VALUE = STRING(ITEM.r-wid)
          ef.n-out:SCREEN-VALUE    = STRING("0")
          ef.gsh-len:SCREEN-VALUE  = STRING("0")
          ef.gsh-wid:SCREEN-VALUE  = STRING("0")
          ef.nsh-len:SCREEN-VALUE  = STRING("0")
          ef.nsh-wid:SCREEN-VALUE  = STRING("0")
          ef.trim-w:SCREEN-VALUE   = STRING("0")
          ef.trim-l:SCREEN-VALUE   = STRING("0")
          eb.num-len:SCREEN-VALUE  = STRING("0")
          eb.num-wid:SCREEN-VALUE  = STRING("0")
          .

   RUN auto-calc2.

   RUN dispatch ('enable-fields').
   DISABLE ef.gsh-wid ef.gsh-len 
           ef.nsh-wid ef.nsh-len 
           ef.trim-w ef.trim-l 
           WITH FRAME {&frame-name}.
   ENABLE ef.m-code  ef.lsh-wid ef.lsh-len ef.xgrain  WITH FRAME {&frame-name}.

   APPLY "entry" TO ef.m-code IN FRAME {&frame-name} .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-calc2 V-table-Win 
PROCEDURE auto-calc2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF ll-one-eb-on-ef THEN DO WITH FRAME {&FRAME-NAME}:
       FIND xef WHERE RECID(xef) = recid(ef).
       FIND xeb WHERE RECID(xeb) = recid(eb).
       ASSIGN xef.m-code = ef.m-code:screen-value
              xef.lsh-lock = NO
              xef.lsh-len = dec(ef.lsh-len:screen-value)
              xef.lsh-wid = dec(ef.lsh-wid:screen-value)
              xef.n-out = 0
              xef.n-out-l = 0
              .

       RUN ce/calc-dim.p.
       FIND xef WHERE RECID(xef) = recid(ef).
       FIND xeb WHERE RECID(xeb) = recid(eb).
       ASSIGN ef.lsh-len:screen-value = STRING( xef.lsh-len )
              ef.lsh-wid:screen-value = STRING(xef.lsh-wid )
              ef.gsh-len:screen-value = STRING(xef.gsh-len )
              ef.gsh-wid:screen-value = STRING(xef.gsh-wid )
              ef.nsh-len:screen-value = STRING(xef.nsh-len)
              ef.nsh-wid:screen-value = STRING(xef.nsh-wid )
              ef.trim-l:screen-value = STRING(xef.trim-l )
              ef.trim-w:screen-value = STRING(xef.trim-w )
              ef.n-out:screen-value = STRING(xef.n-out)
              ef.n-cuts:screen-value = STRING(xef.n-cuts)              
              ef.die-in:screen-value = STRING(xef.die-in)
              eb.num-wid:screen-value = STRING(xeb.num-wid)
              eb.num-len:screen-value = STRING(xeb.num-len)
              eb.num-up:screen-value = STRING(xeb.num-up).
       RUN roll-display.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-dimension V-table-Win 
PROCEDURE calc-dimension :
/*------------------------------------------------------------------------------
  Purpose:     calc all diminsion same as calc-dim.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def new shared temp-table  formule 
                           field formule as de extent 12.

def var tr-l as de.
def var tr-w as de.  /* store mach or ctrl trim defaults */
def var llen like xef.lsh-len.
def var lwid like xef.lsh-wid.
def var aaa as int init 1.
def var bbb as int init 1.  /* for xgrain flip-flop */
def var zzz as dec no-undo.
def var op as ch extent 12.
def var nextop as int.
def var num as de extent 12.
def var curnum as ch.
def var kar as ch format "x".  /* style formula kalk variables */
def var i as int no-undo.
def var j as int no-undo.
def var k_frac as dec init 6.25 no-undo.
def var lv-is-foam as log no-undo.
def var lv-industry as cha no-undo.
 /* Calculate dimension 
     1.  item -> machine -> control table 
         item : ef.board - Is it a real item or estimate  i-code = "R" or "E"
                           roll ?
         machine :  ef.m-code 
         control file : ce-ctrl.
 */
  find first style where style.company = cocode and
                         style.style = xeb.style
                         no-lock no-error.
  if avail style and style.type = "F" then lv-is-foam = yes.
  if avail style then lv-industry = style.industry.

  find first ce-ctrl where ce-ctrl.company = cocode and
                           ce-ctrl.loc     = locode no-lock no-error.
  if xef.m-code ne "" then
      find first mach {sys/ref/machW.i} and mach.m-code = xef.m-code  no-lock no-error.
  if avail mach then do:
     assign tr-l = mach.min-triml
            tr-w = mach.min-trimw
            xef.lsh-len = mach.max-wid
            xef.lsh-wid = mach.max-len
            xef.lam-dscr = "S"
            xef.roll = mach.p-type = "R".   
     find first item where item.company = cocode 
                  /*     and item.mat-type = "A"   ??? */
                      and item.i-no eq xef.board
                   use-index i-no no-lock no-error.
     if not avail item or item.i-code eq "E" then
         assign xef.gsh-wid = ( trunc(mach.max-len / xeb.t-wid,0) * xeb.t-wid +
                                 (2 * mach.min-triml))
                xef.gsh-len = (trunc(mach.max-wid / xeb.t-len,0) * xeb.t-len +
                                 (2 * mach.min-trimw) )
                xef.gsh-dep = if lv-is-foam then (trunc(mach.max-dep / xeb.t-dep,0) * xeb.t-dep ) else 0 .                     
     else if avail item then
          assign xef.gsh-wid = (item.s-wid)
                 xef.gsh-len = (item.s-len)
                 xef.gsh-dep = if lv-is-foam then (item.s-dep) else 0.

  end.
  else assign tr-l = ce-ctrl.ls-triml
              tr-w = ce-ctrl.ls-trimw
              xef.roll = ce-ctrl.avg-cscost <> 0
              xef.lam-dscr = "S"
              xef.lsh-wid = ce-ctrl.ls-length
              xef.lsh-len = (ce-ctrl.ls-width)
              xef.gsh-len = (xeb.t-len)
              xef.gsh-wid = (xeb.t-wid)
              .
  assign tr-l = tr-l * 2
         tr-w = tr-w * 2.

  find first style {sys/ref/styleW.i} and style.style = xeb.style no-lock no-error.

  find first item where item.company eq cocode
                   and item.i-no    eq xef.board
         no-lock no-error.
  if avail item then do:
    find e-item of item no-lock no-error.

    assign xef.i-code = item.i-code
  "        xef.weight = item.basis-w.

    if not xef.lsh-lock then do:
       xef.cal = item.cal.    
       if item.i-code eq "R" then do:
         if xef.roll then assign xef.roll-wid = item.r-wid
                                 xef.gsh-wid  = item.r-wid.
         else do:
            assign xef.gsh-wid  = item.s-wid
                   xef.gsh-len  = item.s-len
                   xef.lam-dscr = "S"          /* feed type */
                   xef.roll     = no
                   xef.roll-wid = 0.
         end.
       end. /* i-code = "R" */
       else if item.i-code eq "E" then do:
          if xef.roll then do:
             do i = 1 to 26:
                if (xef.lam-dscr ne "R" and xef.xgrain ne "S" and
                    e-item.roll-w[i] lt xef.lsh-len) or
                    ((xef.lam-dscr eq "R" or
                    (xef.lam-dscr ne "R" and xef.xgrain eq "S" )) and
                    e-item.roll-w[i] lt xef.lsh-wid)
                then next.
                if e-item.roll-w[i] gt 0 then xef.gsh-wid = e-item.roll-w[i].
                leave.
             end.
          end.
          else assign xef.gsh-wid = xef.lsh-wid.  
          if xef.lam-dscr eq "R" or (xef.lam-dscr ne "R" and xef.xgrain eq "S")
          then do:
                  assign xef.gsh-wid      = xef.lsh-wid
                         xef.gsh-len      = xef.lsh-len
                         xef.nsh-wid = xef.gsh-wid
                         xef.nsh-len = xef.gsh-len.

          end.
          else assign xef.gsh-wid      = xef.lsh-wid
                      xef.gsh-len      = xef.lsh-len
                      xef.nsh-len = xef.gsh-len
                      xef.nsh-wid = xef.gsh-wid.

          xef.roll-wid = xef.gsh-wid.
      end.   /* item.i-code = "E" */
    end. /* lsh-lock */
  end. /* avail item */
  /*  if not avail item -> all vars got values above from ce-ctrl */

  if xef.n-out = 0 then xef.n-out = 1.
  if xef.n-out-l = 0 then xef.n-out-l = 1.
  xef.n-cuts = (xef.n-out - 1) + (xef.n-out-l - 1) .
  if xef.n-cuts lt 0 then xef.n-cuts = 0.

  assign llen = xef.gsh-len / xef.n-out
         lwid = xef.gsh-wid / xef.n-out-l.

  if xef.lam-dscr eq "R" or (xef.lam-dscr ne "R" and xef.xgrain eq "S") then
     assign  zzz  = llen
             llen = lwid
             lwid = zzz.

   /*   run ce/u2kinc1.p.
      run ce/u2kinc2.p.
   */   
  run est/u2kinc1c.p (recid(xeb)).
  run est/u2kinc2c.p (recid(xeb)).
  find first formule .
  num = 0. /* zero array */
       /* Sheet fed, no Xgrain - or Roll w/Blk Xgrain */
  if (xef.lam-dscr ne "R" and index("SB",xef.xgrain) eq 0) or
      (xef.lam-dscr eq "R" and xef.xgrain = "B") then do:
         /* aaa = 2 -> Blk W on Press.Len *** aaa = 1 -> Blk W on Press.Wid */
      assign  aaa = 2
              bbb = 1.
      do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[aaa] + formule[use-l[j] + (use-l[j] - 1)] <= (llen - tr-l)
            or i = 1   /* at least 1 up!!! */
            then assign
                 op[aaa]  = string(i)
                 num[aaa] = num[aaa] + formule[use-l[j] + (use-l[j] - 1)].
            else leave.
      end.
      do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[bbb] + formule[use-w[j] * 2] <= (lwid - tr-w)
            or i = 1
            then assign op[bbb]  = string(i)
                        num[bbb] = num[bbb] + formule[use-w[j] * 2].
            else leave.
      end.
  end.
     /* Roll Fed, no Xgrain - or Sheet w/Xgrain */
  else if (xef.lam-dscr eq "R" or
          (xef.lam-dscr ne "R" and index("SB",xef.xgrain) gt 0)) then do:
       assign aaa = 1
              bbb = 2. /* aaa = # on layout width, bbb = # layout length */
         do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[aaa] + formule[use-l[j] + (use-l[j] - 1)] <= (lwid - tr-w)
            or i = 1
            then assign op[aaa]  = string(i)
                    num[aaa] = num[aaa] + formule[use-l[j] + (use-l[j] - 1)].
            else leave.
         end.
         do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[bbb] + formule[use-w[j] * 2] <= (llen - tr-l)
            or i = 1
            then assign op[bbb]  = string(i)
                        num[bbb] = num[bbb] + formule[use-w[j] * 2].
            else leave.
         end.
  end.
  if xef.xgrain eq "B" then do:
     assign xeb.num-wid = int(op[2])
            xeb.num-len = int(op[1]).

     if xeb.t-len * xeb.num-wid gt num[2] then  num[2] = xeb.t-len * xeb.num-wid.
     if xeb.t-wid * xeb.num-len gt num[1] then  num[1] = xeb.t-wid * xeb.num-len.
  end.
  else do:
     assign xeb.num-wid = int(op[1])
            xeb.num-len = int(op[2]).      
     if xeb.t-len * xeb.num-wid gt num[1] then  num[1] = xeb.t-len * xeb.num-wid.
     if xeb.t-wid * xeb.num-len gt num[2] then  num[2] = xeb.t-wid * xeb.num-len.
  end.

  assign xeb.num-up  = xeb.num-wid * xeb.num-len
         xef.die-in  = formule[12] * xeb.num-up.
  if xef.lam-dscr eq "R" or (xef.lam-dscr ne "R" and xef.xgrain eq "S")
  then do:
         assign   xef.nsh-wid = num[2] + tr-l
                  xef.nsh-len = num[1] + tr-w 
                  xef.trim-w  = num[2]
                  xef.trim-l  = num[1].
         if xef.lsh-wid lt xef.nsh-wid then xef.lsh-wid = xef.nsh-wid.
         if xef.lsh-len lt xef.nsh-len then xef.lsh-len = xef.nsh-len.
  end.
  else do:
         assign   xef.nsh-wid = num[1] + tr-w
                  xef.nsh-len = num[2] + tr-l 
                  xef.trim-w  = num[1]
                  xef.trim-l  = num[2].
         if xef.lsh-wid lt xef.nsh-wid then xef.lsh-wid = xef.nsh-wid.
         if xef.lsh-len lt xef.nsh-len then xef.lsh-len = xef.nsh-len.
  end.
  if xef.gsh-wid lt xef.nsh-wid then xef.gsh-wid = xef.nsh-wid.
  if xef.gsh-len lt xef.nsh-len then xef.gsh-len = xef.nsh-len.
  /* end of u2k.p */

  if not lv-is-foam or item.i-code eq "E" then do:
     if avail mach and mach.dept[1] eq "RC" then
            assign  xef.nsh-wid = xef.nsh-wid - (2 * mach.min-triml)
                    xef.nsh-len = xef.nsh-len - (2 * mach.min-trimw).


     assign xef.n-out   = trunc(xef.lsh-wid / xef.nsh-wid,0)
            xef.n-out-l = trunc(xef.lsh-len / xef.nsh-len,0)

     assign xef.gsh-wid = if not avail item or item.i-code eq "E" then
                               ((xef.n-out   * xef.nsh-wid) +
                          if avail mach and mach.dept[1] eq "RC" then
                                 (2 * mach.min-triml) else 0)
                          else xef.gsh-wid
            xef.gsh-len = if not avail item or item.i-code eq "E" then
                                 ( (xef.n-out-l * xef.nsh-len) +
                                   if avail mach and mach.dept[1] eq "RC" then
                                   (2 * mach.min-trimw) else 0 )
                              else xef.gsh-len.

  end.  

/*message "in u2k Gsh W" xef.gsh-wid  " L:" xef.gsh-len skip
                "Nsh W" xef.nsh-wid " L:" xef.nsh-len skip
                num[1] num[2] tr-l tr-w
                 view-as alert-box.
*/ 

*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME fold.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-film V-table-Win 
PROCEDURE display-film :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN dispatch ('display-fields').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-layout-fields V-table-Win 
PROCEDURE enable-layout-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAIL ef THEN RETURN.

/*  
  find first item where item.company = gcompany and
                        item.i-no = ef.board 
                        no-lock no-error.
  if avail item and item.i-code <> "E" then 
     disable ef.gsh-wid ef.gsh-len with frame {&frame-name}.
*/

  FIND FIRST style WHERE style.company = gcompany AND
                          style.style = eb.style
                          NO-LOCK NO-ERROR.

  IF NOT ll-auto-calc-selected THEN DISABLE ef.lsh-wid ef.lsh-len WITH FRAME {&frame-name}.

  IF ll-one-eb-on-ef THEN ENABLE eb.num-len eb.num-wid WITH FRAME {&FRAME-NAME}.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-foam AS LOG NO-UNDO.
  DEF VAR lv-num-up LIKE eb.num-up NO-UNDO.
  DEF VAR ll-gsh-len AS LOG NO-UNDO.
  DEF VAR ll-auto-calc AS LOG NO-UNDO.

  DEF BUFFER bf-ef FOR ef.


  /* Code placed here will execute PRIOR to standard behavior. */
  lv-num-up = eb.num-up.

  EMPTY TEMP-TABLE old-ef.
  CREATE old-ef.
  BUFFER-COPY ef TO old-ef.

  RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).

  /* Dispatch standard ADM method.         */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST bf-est WHERE bf-est.company = ef.company
                      AND bf-est.est-no = ef.est-no NO-LOCK NO-ERROR.
  IF bf-est.est-type EQ 4 THEN eb.num-up = lv-num-up.

   FIND style WHERE style.company = gcompany AND
                    style.style = eb.style
                    NO-LOCK NO-ERROR.   
   lv-foam = IF AVAIL style AND style.type = "F" THEN YES ELSE NO.
   FIND FIRST item WHERE item.company = gcompany AND
                         item.i-no = ef.board
                         NO-LOCK NO-ERROR.
/*   if avail item then find first e-item of item no-lock no-error.*/

   IF ef.m-code:screen-value IN FRAME {&frame-name} <> "" THEN 
      FIND FIRST mach WHERE mach.company = gcompany AND
                            mach.loc = ef.loc AND
                            mach.m-code = ef.m-code:screen-value IN FRAME {&frame-name}
                            NO-LOCK NO-ERROR.

   /*  need to run ce/u2k.p 
      if not lv-foam or item.i-code = "E" then run ce/u2k.p . 
      run ce/u2k3.p .   
   */  

ll-auto-calc = ll-auto-calc-selected.

IF ll-auto-calc-selected THEN DO:

   IF ll-num-out-changed THEN DO:
      ll-auto-calc-selected = NO.     
      ll-num-out-changed = NO.

     ef.n-cuts = (ef.n-out - 1) /*+ (ef.n-out-l - 1)*/.
     IF ef.n-cuts LT 0 THEN ef.n-cuts = 0.

     ef.gsh-wid = IF NOT AVAIL item OR item.i-code EQ "E" THEN
            ( (INPUT ef.n-out   * IF ef.xgrain EQ "S" THEN ef.nsh-len ELSE ef.nsh-wid) +
                        IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
                          (2 * mach.min-trimw) ELSE 0)
                        ELSE ef.gsh-wid.

     IF AVAIL mach AND mach.p-type EQ "R" THEN DO:
       ll-gsh-len = NO.

       DO i = 1 TO 20:
         IF mach.max-pan-ss[i] / 1000 EQ ef.gsh-len THEN DO:
           ll-gsh-len = YES.
           LEAVE.
         END.
       END.

       IF NOT ll-gsh-len THEN
       DO i = 1 TO 20:
         IF mach.max-pan-ss[i] / 1000 LT ef.nsh-len THEN LEAVE.
         ELSE ef.gsh-len = mach.max-pan-ss[i] / 1000.
       END.
     END.

     ELSE
       ef.gsh-len = IF NOT AVAIL item OR item.i-code EQ "E" THEN
                       ((/*ef.n-out-l **/ IF ef.xgrain EQ "S" THEN ef.nsh-wid ELSE ef.nsh-len) +
                        IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
                          (2 * mach.min-triml) ELSE 0)
                     ELSE ef.gsh-len.

     ASSIGN ef.gsh-len:screen-value = STRING(ef.gsh-len   )
            ef.gsh-wid:screen-value = STRING(ef.gsh-wid  ).
     DISPLAY ef.n-cuts WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
     ll-auto-calc-selected = NO.


       IF ef.m-code <> "" THEN 
          FIND FIRST mach WHERE mach.company = gcompany AND
                            mach.loc = ef.loc AND
                            mach.m-code = ef.m-code
                            NO-LOCK NO-ERROR.

       FIND FIRST item WHERE item.company = gcompany AND
                             item.i-no = ef.board
                             NO-LOCK NO-ERROR.

     ef.n-cuts = (ef.n-out   - 1) /*+ (ef.n-out-l - 1)*/.
     IF ef.n-cuts LT 0 THEN ef.n-cuts = 0.
     DISPLAY ef.n-cuts WITH FRAME {&FRAME-NAME}.

     ef.gsh-wid = IF NOT AVAIL item OR item.i-code EQ "E" THEN
            ( (INPUT ef.n-out   * IF ef.xgrain EQ "S" THEN ef.nsh-len ELSE ef.nsh-wid) +
                        IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
                          (2 * mach.min-trimw) ELSE 0)
                        ELSE ef.gsh-wid.

     IF AVAIL mach AND mach.p-type EQ "R" THEN DO:
       ll-gsh-len = NO.

       DO i = 1 TO 20:
         IF mach.max-pan-ss[i] / 1000 EQ ef.gsh-len THEN DO:
           ll-gsh-len = YES.
           LEAVE.
         END.
       END.

       IF NOT ll-gsh-len THEN
       DO i = 1 TO 20:
         IF mach.max-pan-ss[i] / 1000 LT ef.nsh-len THEN LEAVE.
         ELSE ef.gsh-len = mach.max-pan-ss[i] / 1000.
       END.
     END.

     ELSE
       ef.gsh-len = IF NOT AVAIL item OR item.i-code EQ "E" THEN
                       ((/*ef.n-out-l **/ IF ef.xgrain EQ "S" THEN ef.nsh-wid ELSE ef.nsh-len) +
                        IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
                          (2 * mach.min-triml) ELSE 0)
                     ELSE ef.gsh-len.

     ASSIGN ef.gsh-len:screen-value = STRING(ef.gsh-len   )
            ef.gsh-wid:screen-value = STRING(ef.gsh-wid  )
            .

/*============= not need
     /*     find xest where recid(xest) = recid(est).  */
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).
  /*   xef.lsh-lock = no.  */
     FIND FIRST bf-est WHERE bf-est.company = xef.company
                         AND bf-est.est-no = xef.est-no NO-LOCK NO-ERROR.
     IF bf-est.est-type < 4 THEN DO:
        if ll-num-lw-changed then do: 
           run ce/calc-dim1.p.  /* not to get new value from calc again */
           ll-num-lw-changed = no. 
        end.
        else run ce/calc-dim.p . 

        find xef where recid(xef) = recid(ef).
        find xeb where recid(xeb) = recid(eb).

        assign ef.lsh-len:screen-value = string( xef.lsh-len )
              ef.lsh-wid:screen-value = string(xef.lsh-wid )
              ef.gsh-len:screen-value = string(xef.gsh-len )
              ef.gsh-wid:screen-value = string(xef.gsh-wid )
              ef.nsh-len:screen-value = string(xef.nsh-len )
              ef.nsh-wid:screen-value = string(xef.nsh-wid )
              ef.trim-l:screen-value = string( xef.trim-l )
              ef.trim-w:screen-value = string( xef.trim-w )
              ef.n-out:screen-value = string(xef.n-out)
              ef.n-out-l:screen-value = string(xef.n-out-l)
              ef.n-cuts:screen-value = string(xef.n-cuts)
              eb.num-wid:screen-value = string(xeb.num-wid)
              eb.num-len:screen-value = string(xeb.num-len)
              eb.num-up:screen-value = string(xeb.num-up)
              ef.die-in:screen-value = string(xef.die-in)              
              ef.roll-wid:SCREEN-VALUE = STRING(xef.roll-wid)
              .

     END.
===============*/
   END.  /* not ll-num-out-changed */
   DISABLE ef.m-code ef.lsh-wid ef.lsh-len ef.xgrain WITH FRAME {&frame-name}.
END.
/*  else do:
       find xef where recid(xef) = recid(ef).
       xef.lsh-lock = yes.
  end.
*/

  RUN sys/inc/die-prep.p (ROWID(ef)).
  FIND CURRENT ef.

  IF ef.roll AND ef.roll-wid GT ef.gsh-wid THEN DO:
    ef.gsh-wid = ef.roll-wid.

    IF ef.i-code EQ "E" AND ll-auto-calc THEN
      IF ef.xgrain EQ "S" THEN
        ef.nsh-len = ef.gsh-wid / ef.n-out.
      ELSE
        ef.nsh-wid = ef.gsh-wid / ef.n-out.
  END.

  IF ll-tandem THEN DO:
    BUFFER-COMPARE ef USING {&List-5} TO old-ef SAVE RESULT IN ll-tandem.
    ll-tandem = NOT ll-tandem.

    IF ll-tandem THEN RUN run-form-copy.
  END.

  /*IF ll-tandem THEN
  FOR EACH bf-ef
      WHERE bf-ef.company EQ ef.company
        AND bf-ef.est-no  EQ ef.est-no
        AND bf-ef.eqty    EQ ef.eqty
        AND ROWID(bf-ef)  NE ROWID(ef):
    FOR EACH bf-eb OF bf-ef:
      BUFFER-COPY eb USING num-wid num-len num-up TO bf-eb.
    END.
    BUFFER-COPY ef USING {&List-5} TO bf-ef.
  END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN eb.num-up.
    IF bf-est.est-type <> 4 THEN 
      ASSIGN eb.num-wid eb.num-len.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ll-is-canceled = YES.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE ef.m-code WITH FRAME {&frame-name}.
  ll-is-canceled = NO.
  DISABLE eb.num-len eb.num-wid WITH FRAME {&FRAME-NAME}.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-n-cuts AS INT NO-UNDO.
  DEF VAR lv-tot-len AS INT NO-UNDO.
  DEF VAR lv-tot-wid AS INT NO-UNDO.
  DEF VAR lv-tot-up AS INT NO-UNDO.
      
  IF VALID-HANDLE(br-flm) THEN DELETE WIDGET  br-flm .

  /* To avoid the display for every form unnecessarily */
  /*IF NOT gvl-first THEN   /* task 06221509 */
    RETURN.*/

  DEF BUFFER bf-eb FOR eb.

  IF NOT AVAIL ef OR NOT AVAIL eb THEN RETURN.
  ASSIGN lv-is-foam = NO
         lv-industry = "".

  FIND FIRST style WHERE style.company = gcompany AND
                          style.style = eb.style
                          NO-LOCK NO-ERROR.
  IF AVAIL style AND style.type = "F" THEN lv-is-foam = YES.
  IF AVAIL style THEN lv-industry = style.industry.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN eb.num-len:HIDDEN IN FRAME {&FRAME-NAME} = NO
         eb.num-wid:HIDDEN IN FRAME {&FRAME-NAME} = NO.

  RUN roll-display.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

   btn_board:LABEL = " " + TRIM(ef.board:LABEL) + ":" /*+ TRIM(ef.board)*/ .
   
    IF ef.board = "" THEN
            btn_board:HIDDEN  = TRUE .
    ELSE 
         btn_board:HIDDEN  = FALSE .
         
    IF lVendItemCost AND cCEVersion EQ "New" AND ef.board NE "" THEN
    DO:
      btn_cost:HIDDEN  = FALSE .
      RUN pShowHideCostFiled.      
    END.                                     
    ELSE ASSIGN
         btn_cost:HIDDEN  = TRUE
         OverrideExist:HIDDEN = TRUE.   
     
  RUN one-eb-on-ef (ROWID(ef), OUTPUT ll-one-eb-on-ef).
  RUN one-ef-on-est (ROWID(est), OUTPUT ll-one-ef-on-est).
  /* from uest2.p =======*/
  FIND FIRST ce-ctrl WHERE ce-ctrl.company =gcompany AND
                           ce-ctrl.loc = eb.loc
                           NO-LOCK NO-ERROR.
  FIND FIRST item WHERE item.company = gcompany AND
                        item.i-no = ef.board
                        NO-LOCK NO-ERROR.
  IF AVAIL item THEN FIND FIRST e-item OF item NO-LOCK NO-ERROR.
  ef.brd-dscr:SCREEN-VALUE = IF AVAIL item THEN item.i-name ELSE "".
  FIND FIRST mach WHERE mach.company = gcompany AND
                        mach.loc = eb.loc AND
                        mach.m-code = ef.m-code
                        USE-INDEX m-code NO-LOCK NO-ERROR.
  IF (AVAIL mach AND mach.p-type = "R") OR
     (AVAIL item AND ( (item.i-code = "R" AND item.r-wid NE 0) OR
                       (AVAIL e-item AND e-item.roll-w[1] NE 0) OR ( fGetVendItemCostRollWidth(ITEM.company, ITEM.i-no, 0) NE 0)    )) OR                        
     (NOT AVAIL mach AND ce-ctrl.avg-cscost NE 0) /*and not(xef.lsh-lock)*/      
  THEN ASSIGN lv-is-roll = TRUE. /* xef.lam-dscr = "R". */
  ELSE ASSIGN lv-is-roll = NO.
  ls-lam-dscr = "S".

  IF ef.n-out = 0 THEN ef.n-out:screen-value IN FRAME {&FRAME-NAME} = string("1").

  IF ef.n-cuts EQ 0 THEN DO:
     li-n-cuts = (int(ef.n-out:screen-value) - 1).
     IF li-n-cuts < 0 THEN li-n-cuts = 0.
     ef.n-cuts:screen-value = STRING(li-n-cuts,">>9").    
  END.

  FOR EACH bf-eb
      WHERE bf-eb.company EQ ef.company
        AND bf-eb.est-no  EQ ef.est-no
        AND bf-eb.form-no EQ ef.form-no
      NO-LOCK:
    ASSIGN
     lv-tot-len = lv-tot-len + bf-eb.num-len.
     lv-tot-wid = lv-tot-wid + bf-eb.num-wid.
     lv-tot-up  = lv-tot-up + bf-eb.num-up.
  END.
  DISPLAY lv-tot-up @ eb.num-up WITH FRAME {&FRAME-NAME}.

  /* === for combo ****/
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"container-source",OUTPUT char-hdl).

  RUN get-attribute IN WIDGET-HANDLE(char-hdl) ("current-page").

  IF int(RETURN-VALUE) = 4 THEN DO:
    FIND FIRST bf-est WHERE bf-est.company = ef.company
                        AND bf-est.est-no = ef.est-no NO-LOCK NO-ERROR.

    IF bf-est.est-type = 4 THEN DO:
       RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
       IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
           RUN enable-leaf IN WIDGET-HANDLE(char-hdl) (YES, bf-est.est-type).

       CREATE BROWSE br-flm 
               ASSIGN TITLE = "Leaf/Film"
               FRAME = FRAME {&FRAME-NAME}:HANDLE
               QUERY = QUERY q-flm:HANDLE
               X = 10
               Y = 205
               WIDTH = 130

               DOWN = 5
               VISIBLE = YES 
               SENSITIVE = TRUE  
               BGCOLOR = 8
               SEPARATORS = YES.

        OPEN QUERY q-flm FOR EACH est-flm WHERE est-flm.company = eb.company
                                             AND est-flm.est-no = eb.est-no
                                            NO-LOCK
                                          BY est-flm.snum
                                          BY est-flm.bnum
                                          BY est-flm.line.

        ASSIGN lh-ino = br-flm:ADD-LIKE-COLUMN("est-flm.i-no")
                lh-dscr = br-flm:ADD-LIKE-COLUMN("est-flm.dscr")
                lh-snum = br-flm:ADD-LIKE-COLUMN("est-flm.snum")
                lh-bnum = br-flm:ADD-LIKE-COLUMN("est-flm.bnum")
                lh-len = br-flm:ADD-LIKE-COLUMN("est-flm.len")
                lh-wid = br-flm:ADD-LIKE-COLUMN("est-flm.wid").

         br-flm:REFRESH() NO-ERROR.

        IF NUM-RESULTS("q-flm":U) = ? OR  /* query not opened */
           NUM-RESULTS("q-flm") = 0 /* query's empty */
                /*OR BROWSE br-flm:NUM-SELECTED-ROWS < 1*/  THEN DO: END.                
        ELSE
         ASSIGN lh-dscr:FORMAT = "x(30)"
                lh-dscr:WIDTH = 40
                lh-len:FORMAT = ">>,>>9.99<<<"
                lh-len:WIDTH = 12
                lh-wid:FORMAT = ">>,>>9.99<<<"
                lh-wid:WIDTH = 12.

    END.  /* est-type = 4 */
    ELSE DO:
       RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
       IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
          RUN enable-leaf IN WIDGET-HANDLE(char-hdl) (NO, bf-est.est-type).
    END.

  IF NOT ll-one-eb-on-ef THEN
    ASSIGN eb.num-len:HIDDEN IN FRAME {&FRAME-NAME} = YES
           eb.num-wid:HIDDEN IN FRAME {&FRAME-NAME} = YES.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO: 
     RUN enable-auto-calc IN WIDGET-HANDLE(char-hdl) (ll-one-eb-on-ef).
     RUN enable-copy IN WIDGET-HANDLE(char-hdl) (ll-one-ef-on-est).
  END.

  END.  /* page - 4 */
  gvl-first = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR hd1 AS HANDLE NO-UNDO.
  DEF VAR hd2 AS HANDLE NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER bf-eb FOR eb.

{&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  /* ==== Folding  item validation ======== */
  DO WITH FRAME {&frame-name} :  /* validation */
    IF ef.m-code:screen-value <> "" AND
       NOT CAN-FIND (FIRST mach WHERE mach.company = gcompany AND
                                      mach.loc = eb.loc AND
                                      mach.m-code = ef.m-code:screen-value)
    THEN DO:
         MESSAGE "Invalid Machine Code. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "Entry" TO ef.m-code.
         RETURN NO-APPLY.
    END.
    ELSE IF ef.m-code:screen-value = "" THEN ef.m-dscr:screen-value = "". 

    RUN new-m-code.

    /*========*/
   IF ef.board:MODIFIED
   THEN DO:
       FIND FIRST item WHERE item.company = gcompany AND
                             ((INDEX("BPR",item.mat-type) > 0 AND NOT lv-is-foam) OR
                              (INDEX("1234",item.mat-type) > 0 AND lv-is-foam) ) AND
                              item.industry = lv-industry AND
                              item.i-no = ef.board:SCREEN-VALUE
                              NO-LOCK NO-ERROR.
       IF NOT AVAIL item THEN DO:
          MESSAGE "Invalid Board. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ef.board.
          RETURN NO-APPLY.
       END.
       IF item.i-code = "R" AND ll-auto-calc-selected THEN DO:
          IF item.r-wid = 0 THEN DO:           
             IF (item.s-wid LT eb.t-wid AND xef.xgrain NE "B") OR
                (item.s-wid LT eb.t-len AND xef.xgrain EQ "B") THEN DO:
                MESSAGE "Sheet Width less than Blank..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO ef.board.
                RETURN NO-APPLY.
             END.
             IF (item.s-len LT eb.t-len AND xef.xgrain NE "B") OR
                (item.s-len LT eb.t-wid AND xef.xgrain EQ "B") THEN DO:
                MESSAGE "Sheet Length less than Blank..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO ef.board.
                RETURN NO-APPLY.
             END.
             IF (xef.xgrain NE "S" AND
                 (ITEM.s-wid GT DEC(ef.lsh-wid:SCREEN-VALUE IN FRAME {&FRAME-NAME}) OR
                  ITEM.s-len GT DEC(ef.lsh-len:SCREEN-VALUE IN FRAME {&FRAME-NAME})))   OR
                (xef.xgrain EQ "S" AND
                 (ITEM.s-wid GT DEC(ef.lsh-len:SCREEN-VALUE IN FRAME {&FRAME-NAME}) OR
                  ITEM.s-len GT DEC(ef.lsh-wid:SCREEN-VALUE IN FRAME {&FRAME-NAME})))   THEN DO:
                 MESSAGE "Sheet does not fit on the machine..." VIEW-AS ALERT-BOX.
                 APPLY "entry" TO ef.board.
                 RETURN NO-APPLY.
             END.
          END.  /* r-wid = 0 */
          ELSE
          IF (item.r-wid LT eb.t-wid AND xef.xgrain NE "B") OR
             (item.r-wid LT eb.t-len AND xef.xgrain EQ "B") THEN DO:
             MESSAGE "Roll Width less than Blank Width. " VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO ef.board.
             RETURN NO-APPLY.
          END.
       END.
    END.

    RUN valid-gsh-wid NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-gsh-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    /*=========*/
    IF EF.BOARD:screen-value <> "" AND
       NOT CAN-FIND (FIRST item WHERE item.company = gcompany AND
                                      ((INDEX("BPR",item.mat-type) > 0 AND NOT lv-is-foam) OR
                                      (INDEX("1234",item.mat-type) > 0 AND lv-is-foam) ) AND
                                      item.industry = lv-industry AND
                                      item.i-no = ef.board:screen-value)
    THEN DO:
         MESSAGE "Invalid Board. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO ef.board.
         RETURN NO-APPLY.
    END.

    /*RUN new-board.*/

  IF bf-est.est-type NE 4 THEN DO:
    RUN est/val-leaf.p  (FRAME {&FRAME-NAME}:HANDLE, ?) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    DO li = 1 TO EXTENT(ef.leaf):
      RUN valid-leaf-snum (li) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-leaf-bnum (li) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

    IF ef.leaf[1]:SCREEN-VALUE <> "" AND
       INT(ef.leaf-w[1]:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO ef.leaf-w[1].
       RETURN NO-APPLY.
    END.
    IF ef.leaf[2]:SCREEN-VALUE <> "" AND
       INT(ef.leaf-w[2]:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO ef.leaf-w[2].
       RETURN NO-APPLY.
    END.
    IF ef.leaf[3]:SCREEN-VALUE <> "" AND
          INT(ef.leaf-w[3]:SCREEN-VALUE) = 0 THEN DO:
          MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ef.leaf-w[3].
          RETURN NO-APPLY.
    END.
    IF ef.leaf[4]:SCREEN-VALUE <> "" AND
           INT(ef.leaf-w[4]:SCREEN-VALUE) = 0 THEN DO:
           MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO ef.leaf-w[4].
           RETURN NO-APPLY.
    END.

    IF ef.leaf[1]:SCREEN-VALUE <> "" AND
      INT(ef.leaf-l[1]:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ef.leaf-l[1].
        RETURN NO-APPLY.
    END.
    IF ef.leaf[2]:SCREEN-VALUE <> "" AND
       INT(ef.leaf-l[2]:SCREEN-VALUE) = 0 THEN DO:
         MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO ef.leaf-l[2].
         RETURN NO-APPLY.
    END.
    IF ef.leaf[3]:SCREEN-VALUE <> "" AND
      INT(ef.leaf-l[3]:SCREEN-VALUE) = 0 THEN DO:
      MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ef.leaf-l[3].
      RETURN NO-APPLY.
    END.
    IF ef.leaf[4]:SCREEN-VALUE <> "" AND
       INT(ef.leaf-l[4]:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO ef.leaf-l[4].
       RETURN NO-APPLY.
    END.
  END. /* leaf/file validateion for not combo */

    IF ef.cost-uom:screen-value <> "" THEN DO:
       FIND FIRST item WHERE item.company = gcompany AND
                                item.i-no = ef.board:screen-value
                                NO-LOCK NO-ERROR.
/*       if avail item then find first e-item of item no-lock no-error.*/
       RUN sys/ref/uom-rm.p  (item.mat-type, OUTPUT uom-list).
       IF NOT CAN-FIND(FIRST uom WHERE LOOKUP(uom.uom,uom-list) > 0 AND
                            uom.uom = ef.cost-uom:screen-value)
       THEN DO:
            MESSAGE "Invalid Cost UOM. Try Help." VIEW-AS ALERT-BOX.
            APPLY "entry" TO ef.cost-uom.
            RETURN NO-APPLY.
       END.                 
    END.  
    IF ef.FR-uom:screen-value <> "" THEN DO:
       uom-list = "CWT,MSF,MSH,TON".
       IF NOT CAN-FIND(FIRST uom WHERE LOOKUP(uom.uom,uom-list) > 0 AND
                            uom.uom = ef.fr-uom:screen-value)
       THEN DO:
            MESSAGE "Invalid Freight UOM. Try Help." VIEW-AS ALERT-BOX.
            APPLY "entry" TO ef.fr-uom.
            RETURN NO-APPLY.
       END.                 
    END.  


    /*
    if NOT ll-auto-calc-selected then do:  
      if dec(ef.gsh-wid:screen-value) < dec(ef.nsh-wid:screen-value) 
      then do:
        message "Net Sheet Size can not be greater than Gross Sheet Size."             
            view-as alert-box error.
        apply "entry" to ef.nsh-wid.
        return no-apply.     
      end.
      if dec(ef.gsh-len:screen-value) < dec(ef.nsh-len:screen-value) 
      then do:
        message "Net Sheet Size can not be greater than Gross Sheet Size." view-as alert-box error.
        apply "entry" to ef.nsh-len.
        return no-apply.     
      end.
      if dec(ef.nsh-wid:screen-value) < dec(ef.trim-w:screen-value) 
      then do:
        message "Net Sheet Size can not be less than Die Size." view-as alert-box error.
        apply "entry" to ef.nsh-wid.
        return no-apply.     
      end.
      if dec(ef.nsh-len:screen-value) < dec(ef.trim-l:screen-value) 
      then do:
        message "Net Sheet Size can not be less than Die Size." view-as alert-box error.
        apply "entry" to ef.nsh-len.
        return no-apply.     
      end.
      if dec(ef.trim-w:screen-value) < dec(eb.t-wid:screen-value) 
      then do:
        message "Die Size can not be less than Blank Size." view-as alert-box error.
        apply "entry" to ef.trim-w.
        return no-apply.     
      end.
      if dec(ef.trim-l:screen-value) < dec(eb.t-len:screen-value) 
      then do:
        message "Die Size can not be less than Blank Size." view-as alert-box error.
        apply "entry" to ef.trim-l.
        return no-apply.     
      end.
    end.  /* not auto-calc */
    */
  END.  /* frame {&frame-name} */

  IF ll-is-sheet-calc THEN DO:
     ll-is-sheet-calc = NO.
     RUN sheet-calc2.
     DISABLE ef.m-code WITH FRAME {&frame-name}.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.

  RELEASE mach.
  IF TRIM(ef.m-code) NE "" THEN
  FIND FIRST mach
      WHERE mach.company EQ ef.company
        AND mach.loc     EQ ef.loc
        AND mach.m-code  EQ ef.m-code
      NO-LOCK NO-ERROR.

  IF AVAIL mach                 AND
     (mach.dept[1] EQ "PR" OR
      mach.dept[2] EQ "PR" OR
      mach.dept[3] EQ "PR" OR
      mach.dept[4] EQ "PR")     THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"reset-ink-source",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    FOR EACH bf-eb
        WHERE bf-eb.company EQ ef.company
          AND bf-eb.est-no  EQ ef.est-no
          AND bf-eb.form-no EQ ef.form-no
        NO-LOCK:

      DO li = 1 TO EXTENT(bf-eb.i-code2):
        IF bf-eb.i-code2[li] NE "" THEN DO:
          IF CAN-FIND(FIRST item
                      WHERE item.company    EQ bf-eb.company
                        AND item.i-no       EQ bf-eb.i-code2[li]
                        AND item.press-type NE mach.pr-type)     THEN
            RUN reset-ink1 IN WIDGET-HANDLE(char-hdl) (ROWID(bf-eb)).

          LEAVE.
        END.
      END.
    END.
  END.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(ef), ROWID(eb)).

  DISABLE eb.num-wid eb.num-len WITH FRAME {&FRAME-NAME}.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    RUN enable-leaf IN WIDGET-HANDLE(char-hdl) (bf-est.est-type EQ 4, bf-est.est-type).

    RUN enable-auto-calc IN WIDGET-HANDLE(char-hdl) (ll-one-eb-on-ef).
  END.

  RUN dispatch ("display-fields").
{&methods/lValidateError.i NO}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE n-out-changed V-table-Win 
PROCEDURE n-out-changed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF ll-auto-calc-selected AND SELF:modified THEN DO:
       ll-num-out-changed  = YES.

       DEF VAR ld-gsh-wid AS DEC NO-UNDO.
       DEF VAR ld-gsh-len AS DEC NO-UNDO.
       DEF VAR ld-gsh-dep AS DEC NO-UNDO.
       ef.n-cuts:screen-value = STRING(INPUT ef.n-out - 1).
       IF int(ef.n-cuts:screen-value) LT 0 THEN ef.n-cuts:screen-value = "0".

       IF ef.m-code:screen-value IN FRAME {&frame-name} <> "" THEN 
          FIND FIRST mach WHERE mach.company = gcompany AND
                            mach.loc = ef.loc AND
                            mach.m-code = ef.m-code:screen-value 
                            NO-LOCK NO-ERROR.

       FIND FIRST item WHERE item.company = gcompany AND
                             item.i-no = ef.board:screen-value
                             NO-LOCK NO-ERROR.

       ASSIGN ld-gsh-wid = IF NOT AVAIL item OR item.i-code EQ "E" THEN ((INT(ef.n-out:SCREEN-VALUE) * DEC(ef.nsh-wid:SCREEN-VALUE)) +
                        IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
                          (2 * mach.min-triml) ELSE 0)
                        ELSE DEC(ef.gsh-wid:SCREEN-VALUE)
              ld-gsh-len = IF NOT AVAIL item OR item.i-code EQ "E" THEN (DEC(ef.nsh-len:SCREEN-VALUE) +
                        IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
                          (2 * mach.min-trimw) ELSE 0)
                     ELSE DEC(ef.gsh-len:SCREEN-VALUE).
       ASSIGN ef.gsh-len:screen-value = STRING(ld-gsh-len   )
              ef.gsh-wid:screen-value = STRING(ld-gsh-wid  )
              .
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-board V-table-Win 
PROCEDURE new-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item
        WHERE item.company  EQ gcompany
          AND ((INDEX("BPR",item.mat-type) GT 0 AND NOT lv-is-foam) OR
               (INDEX("1234",item.mat-type) GT 0 AND lv-is-foam))
          AND item.industry EQ lv-industry
          AND item.i-no     EQ ef.board:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL item AND TRIM(ef.board:SCREEN-VALUE) NE "" THEN DO:
      ASSIGN
       ef.brd-dscr:SCREEN-VALUE = item.i-name
       ef.i-code:SCREEN-VALUE   = item.i-code
       ef.weight:SCREEN-VALUE   = STRING(item.basis-w)
       ef.cal:SCREEN-VALUE      = STRING(item.cal).
      RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT uom-list).
      IF uom-list NE "" AND LOOKUP(ef.cost-uom:SCREEN-VALUE,uom-list) LE 0 THEN
        ef.cost-uom:SCREEN-VALUE = ENTRY(1,uom-list).
      IF item.i-code EQ "R" THEN
        IF item.r-wid GT 0 THEN
          ASSIGN
           ef.gsh-wid:SCREEN-VALUE  = STRING(item.r-wid)
           ef.lsh-len:SCREEN-VALUE  = STRING(item.r-wid)
           ef.roll-wid:SCREEN-VALUE = STRING(item.r-wid)
           ef.roll:SCREEN-VALUE     = "Y".
        ELSE DO:
          ASSIGN
           ef.gsh-wid:SCREEN-VALUE  = STRING(item.s-wid)
           ef.gsh-len:SCREEN-VALUE  = STRING(item.s-len)
           ef.lsh-len:SCREEN-VALUE  = STRING(item.s-wid)
           ef.lsh-wid:SCREEN-VALUE  = STRING(item.s-len)
           ef.roll-wid:SCREEN-VALUE = ""
           ef.roll:SCREEN-VALUE     = "N".
          /*IF ef.xgrain:SCREEN-VALUE EQ "S" THEN
            ASSIGN
             lv                      = ef.gsh-len:SCREEN-VALUE
             ef.gsh-len:SCREEN-VALUE = ef.gsh-wid:SCREEN-VALUE
             ef.gsh-wid:SCREEN-VALUE = lv.*/
        END.
      IF lNewVendorItemCost THEN DO:
         ef.cost-uom:SCREEN-VALUE = fGetVendItemCostUOM(ITEM.company, ITEM.i-no). 
      END.
      ELSE DO: 
        FIND FIRST e-item OF item NO-LOCK NO-ERROR.
        IF AVAIL e-item THEN ef.cost-uom:SCREEN-VALUE = e-item.std-uom. 
      END.  
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-m-code V-table-Win 
PROCEDURE new-m-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST mach
        WHERE mach.company EQ gcompany
          AND mach.m-code  EQ ef.m-code:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL mach AND TRIM(ef.m-code:SCREEN-VALUE) NE "" THEN DO:
      ef.m-dscr:SCREEN-VALUE = mach.m-dscr.

      IF mach.p-type EQ "R" THEN DO:
        ef.roll:SCREEN-VALUE = "Y".
        RUN roll-display.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE num-wid-len-changed V-table-Win 
PROCEDURE num-wid-len-changed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DO WITH FRAME {&FRAME-NAME}:                                                      
  IF ll-auto-calc-selected AND ll-one-eb-on-ef THEN DO:
    FOR EACH w-ef:
      DELETE w-ef.
    END.
    FOR EACH w-eb:
      DELETE w-eb.
    END.

    ll-num-lw-changed = YES.

    calc-dim1: DO ON ERROR UNDO, LEAVE.
      FIND xef WHERE ROWID(xef) EQ ROWID(ef).
      FIND xeb WHERE ROWID(xeb) EQ ROWID(eb).

      ASSIGN
       xeb.num-wid = INT(eb.num-wid:SCREEN-VALUE)
       xeb.num-len = INT(eb.num-len:SCREEN-VALUE)
       xef.roll    = STRING(ef.roll:SCREEN-VALUE) EQ "Y".

      RUN ce/calc-dim1.p NO-ERROR.

      IF ERROR-STATUS:ERROR EQ NO THEN DO:
        FIND xef WHERE ROWID(xef) EQ ROWID(ef).
        FIND xeb WHERE ROWID(xeb) EQ ROWID(eb).

        CREATE w-ef.
        BUFFER-COPY xef TO w-ef.
        CREATE w-eb.
        BUFFER-COPY xeb TO w-eb.
      END.

      UNDO calc-dim1, LEAVE calc-dim1.
    END.

    IF AVAIL w-ef AND AVAIL w-eb THEN
      ASSIGN
       ef.lsh-len:SCREEN-VALUE = STRING(w-ef.lsh-len)
       ef.lsh-wid:SCREEN-VALUE = STRING(w-ef.lsh-wid)
       ef.gsh-len:SCREEN-VALUE = STRING(w-ef.gsh-len)
       ef.gsh-wid:SCREEN-VALUE = STRING(w-ef.gsh-wid)
       ef.nsh-len:SCREEN-VALUE = STRING(w-ef.nsh-len)
       ef.nsh-wid:SCREEN-VALUE = STRING(w-ef.nsh-wid)
       ef.trim-l:SCREEN-VALUE  = STRING(w-ef.trim-l)
       ef.trim-w:SCREEN-VALUE  = STRING(w-ef.trim-w)
       ef.n-out:SCREEN-VALUE   = STRING(w-ef.n-out)
       ef.n-cuts:SCREEN-VALUE  = STRING(w-ef.n-cuts)
       eb.num-wid:SCREEN-VALUE = STRING(w-eb.num-wid)
       eb.num-len:SCREEN-VALUE = STRING(w-eb.num-len)
       eb.num-up:SCREEN-VALUE  = STRING(w-eb.num-up)
       ef.die-in:SCREEN-VALUE  = STRING(w-ef.die-in).

    IF AVAIL w-ef THEN DELETE w-ef.
    IF AVAIL w-eb THEN DELETE w-eb.
  END.

  ELSE
    ASSIGN
     ef.die-in:SCREEN-VALUE = STRING(DEC(ef.die-in:SCREEN-VALUE) /
                                     INT(eb.num-up:SCREEN-VALUE))
     eb.num-up:SCREEN-VALUE = STRING((IF INT(eb.num-wid:SCREEN-VALUE) EQ 0 THEN 1 ELSE INT(eb.num-wid:SCREEN-VALUE)) *
                                     (IF INT(eb.num-len:SCREEN-VALUE) EQ 0 THEN 1 ELSE INT(eb.num-len:SCREEN-VALUE)))
     ef.die-in:SCREEN-VALUE = STRING(DEC(ef.die-in:SCREEN-VALUE) *
                                     INT(eb.num-up:SCREEN-VALUE)).
 END.

 RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE one-eb-on-ef V-table-Win 
PROCEDURE one-eb-on-ef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-one-eb AS LOG NO-UNDO.

  DEF BUFFER b-ac-eb FOR eb.
  DEF BUFFER b-ac-ef FOR ef.


  FIND b-ac-ef WHERE ROWID(b-ac-ef) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-ac-ef THEN
  FIND b-ac-eb
      WHERE b-ac-eb.company EQ b-ac-ef.company
        AND b-ac-eb.est-no  EQ b-ac-ef.est-no
        AND b-ac-eb.eqty    EQ b-ac-ef.eqty
        AND b-ac-eb.form-no EQ b-ac-ef.form-no
      NO-LOCK NO-ERROR.

  op-one-eb = AVAIL b-ac-eb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE one-ef-on-est V-table-Win 
PROCEDURE one-ef-on-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-one-ef AS LOG NO-UNDO.

  DEF BUFFER b-ac-est FOR est.
  DEF BUFFER b-ac-ef FOR ef.


  FIND b-ac-est WHERE ROWID(b-ac-est) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-ac-est THEN
  FIND b-ac-ef WHERE b-ac-ef.company EQ b-ac-est.company
                 AND b-ac-ef.est-no  EQ b-ac-est.est-no        NO-LOCK NO-ERROR.

  op-one-ef = AVAIL b-ac-ef.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      btn_board:HIDDEN = TRUE .
      btn_cost:HIDDEN = TRUE .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rawavail-layout V-table-Win 
PROCEDURE rawavail-layout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE "raw:"  PROGRAM-NAME(1) SKIP
          PROGRAM-NAME(2) SKIP
          PROGRAM-NAME(3) SKIP          
          PROGRAM-NAME(4) SKIP          
          PROGRAM-NAME(5) SKIP          
          PROGRAM-NAME(6) SKIP          
          PROGRAM-NAME(7) SKIP          
          PROGRAM-NAME(8) SKIP          
          PROGRAM-NAME(9) SKIP          
  VIEW-AS ALERT-BOX.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-dim V-table-Win 
PROCEDURE recalc-dim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF ll-auto-calc-selected THEN  DO:  /* from ce/uest2.p */
       FIND FIRST ce-ctrl WHERE ce-ctrl.company =gcompany AND
                           ce-ctrl.loc = eb.loc
                           NO-LOCK NO-ERROR.
       FIND FIRST item WHERE item.company = gcompany AND
                             item.i-no = ef.board:screen-value IN FRAME {&frame-name}
                             NO-LOCK NO-ERROR.
       IF AVAIL item THEN FIND FIRST e-item OF item NO-LOCK NO-ERROR.                      
       ASSIGN lv-is-roll = NO.
       FIND FIRST mach WHERE mach.company = gcompany AND
                                      mach.loc = eb.loc AND
                                      mach.m-code = ef.m-code:screen-value
                                      USE-INDEX m-code NO-LOCK NO-ERROR.
       /* if (avail mach and mach.p-type = "R") or
          (avail item and ( (item.i-code = "R" and item.r-wid ne 0) or
          (avail e-item and e-item.roll-w[1] ne 0) )) or
          (not avail mach and ce-ctrl.avg-cscost ne 0) and
          not(xef.lsh-lock)  /** CTS **/
          then assign xef.roll = true. /* xef.lam-dscr = "R". */
          else       not for auto calc */

       IF AVAIL mach THEN DO:   
          ASSIGN ef.lsh-len:screen-value = STRING(mach.max-wid)
                 ef.lsh-wid:screen-value = STRING(mach.max-len)
                 ls-lam-dscr = "S"
                 lv-is-roll = mach.p-type = "R".

          FIND FIRST item WHERE item.company = gcompany 
                         AND item.mat-type = "A"
                         AND item.i-no EQ ef.board
                   USE-INDEX i-no NO-LOCK NO-ERROR.
          IF NOT AVAIL item OR item.i-code EQ "E" THEN
              ASSIGN ef.gsh-wid:screen-value = STRING( trunc(mach.max-len / eb.t-wid,0) * eb.t-wid +
                                 (2 * mach.min-triml))
                     ef.gsh-len:screen-value = STRING(trunc(mach.max-wid / eb.t-len,0) * eb.t-len +
                                 (2 * mach.min-trimw) )
                     .
          ELSE IF AVAIL item THEN
               ASSIGN ef.gsh-wid:screen-value = STRING(item.s-wid)
                      ef.gsh-len:screen-value = STRING(item.s-len)
                      .        

       END.
       ELSE DO:
            ASSIGN lv-is-roll = ce-ctrl.avg-cscost <> 0
                   ls-lam-dscr = "S"
                   ef.lsh-wid:screen-value = STRING(ce-ctrl.ls-length)
                   ef.lsh-len:screen-value = STRING(ce-ctrl.ls-width)
                   ef.gsh-len:screen-value = STRING(eb.t-len)
                   ef.gsh-wid:screen-value = STRING(eb.t-wid)
                   .
       END.  

              /* from uest2a.p */
       IF ef.n-out:screen-value = "0" THEN ef.n-out:screen-value = "1".
       ef.n-cuts:screen-value = STRING(DECIMAL(ef.n-out:screen-value) - 1).

       IF DECIMAL(ef.n-cuts:screen-value) LT 0 THEN ef.n-cuts:screen-value = "0".

    /* maybe not here */
       /*find xest where recid(xest) = recid(est). */
       FIND xef WHERE RECID(xef) = recid(ef).
       FIND xeb WHERE RECID(xeb) = recid(eb).
       IF NOT lv-is-foam OR item.i-code EQ "E" THEN DO:
          xef.lam-dscr = ls-lam-dscr.
          xef.roll = lv-is-roll.
          xef.m-code = ef.m-code:screen-value.
          ASSIGN xef.lsh-wid = trunc(DECIMAL(ef.lsh-wid:screen-value),0) +
                               ((DECIMAL(ef.lsh-wid:screen-value) -
                                 trunc(DECIMAL(ef.lsh-wid:screen-value),0)
                                 ) * k_frac)
                 xef.lsh-len = trunc(DECIMAL(ef.lsh-len:screen-value),0) +
                               ((DECIMAL(ef.lsh-len:screen-value) -
                                 trunc(DECIMAL(ef.lsh-len:screen-value),0)
                                 ) * k_frac)
                 .
          RUN ce/u2k.p .  /* Calculation */
       END.
       FIND xef WHERE RECID(xef) = recid(ef).
       FIND xeb WHERE RECID(xeb) = recid(eb).

       IF NOT lv-is-foam OR item.i-code EQ "E" THEN DO:
         IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
            ASSIGN  xef.nsh-wid = xef.nsh-wid - (2 * mach.min-triml)
                    xef.nsh-len = xef.nsh-len - (2 * mach.min-trimw).


         ASSIGN xef.n-out   = trunc(xef.lsh-wid / xef.nsh-wid,0)
                /*xef.n-out-l = trunc(xef.lsh-len / xef.nsh-len,0)*/
                .           
         ASSIGN xef.gsh-wid = IF NOT AVAIL item OR item.i-code EQ "E" THEN
                                 ((xef.n-out   * xef.nsh-wid) +
                              IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
                                 (2 * mach.min-triml) ELSE 0)
                              ELSE xef.gsh-wid
                xef.gsh-len = IF NOT AVAIL item OR item.i-code EQ "E" THEN
                                 ( (/*xef.n-out-l **/ xef.nsh-len) +
                                   IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
                                   (2 * mach.min-trimw) ELSE 0 )
                              ELSE xef.gsh-len
                .      
       END.  

       ASSIGN ef.lsh-len:screen-value = STRING( xef.lsh-len )
              ef.lsh-wid:screen-value = STRING(xef.lsh-wid )
              ef.gsh-len:screen-value = STRING(xef.gsh-len )
              ef.gsh-wid:screen-value = STRING(xef.gsh-wid )
              ef.nsh-len:screen-value = STRING(xef.nsh-len )
              ef.nsh-wid:screen-value = STRING(xef.nsh-wid )
              ef.trim-l:screen-value = STRING(xef.trim-l )
              ef.trim-w:screen-value = STRING(xef.trim-w )
              ef.n-out:screen-value = STRING(xef.n-out)
              ef.n-cuts:screen-value = STRING(xef.n-cuts)
              eb.num-wid:screen-value = STRING(xeb.num-wid)
              eb.num-len:screen-value = STRING(xeb.num-len)
              eb.num-up:screen-value = STRING(xeb.num-up)
              .

    END. /* ll-auto-calc-selected */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-shared-buffers V-table-Win 
PROCEDURE release-shared-buffers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE xest.
  RELEASE xef.
  RELEASE xeb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE roll-display V-table-Win 
PROCEDURE roll-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF lv-label[1] EQ "" THEN
      ASSIGN
       lv-label[1] = ef.lsh-len:LABEL
       lv-label[2] = ef.lsh-wid:LABEL.

    IF ef.roll:SCREEN-VALUE EQ "Y" THEN
      ASSIGN
       ef.lsh-len:LABEL         = lv-label[2]
       ef.lsh-wid:LABEL         = lv-label[1]
       ef.roll-wid:SCREEN-VALUE = ef.gsh-wid:SCREEN-VALUE.
    ELSE
      ASSIGN
       ef.lsh-len:LABEL         = lv-label[1]
       ef.lsh-wid:LABEL         = lv-label[2]
       ef.roll-wid:SCREEN-VALUE = "0".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-form-copy V-table-Win 
PROCEDURE run-form-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN est/d-copyfm.w (RECID(ef),RECID(eb)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-goto V-table-Win 
PROCEDURE run-goto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF bf-est.est-type <> 4 THEN RETURN.


 DEF VAR lv-rowid AS ROWID NO-UNDO.
 DEF VAR lv-tot-len AS INT NO-UNDO.
 DEF VAR lv-tot-wid AS INT NO-UNDO.
 DEF VAR lv-tot-up AS INT NO-UNDO.
 DEF BUFFER bf-eb FOR eb.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN run-goto IN WIDGET-HANDLE(char-hdl).

  RUN dispatch ("display-fields").

  FIND FIRST bf-est WHERE bf-est.company = ef.company
                      AND bf-est.est-no = ef.est-no NO-LOCK NO-ERROR.
  IF bf-est.est-type = 4 THEN DO:
      ASSIGN lv-tot-up = 0
             lv-tot-len = 0
             lv-tot-wid = 0.
      FOR EACH bf-eb WHERE bf-eb.company = ef.company AND
                           bf-eb.est-no = ef.est-no NO-LOCK.
          lv-tot-len = lv-tot-len + bf-eb.num-len.
          lv-tot-wid = lv-tot-wid + bf-eb.num-wid.
          lv-tot-up =  lv-tot-up + bf-eb.num-up.

      END.
      DISP /*lv-tot-len @ eb.num-len
           lv-tot-wid @ eb.num-wid */
           lv-tot-up @ eb.num-up WITH FRAME {&FRAME-NAME}.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-job-stds V-table-Win 
PROCEDURE run-job-stds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL est THEN
  FOR EACH job-hdr
      WHERE job-hdr.company EQ est.company
        AND job-hdr.est-no  EQ est.est-no
      NO-LOCK,
      FIRST job
      WHERE job.company EQ job-hdr.company
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2
        AND job.est-no  EQ job-hdr.est-no
        AND job.opened  EQ YES
      NO-LOCK
      BREAK BY job.job:

    IF LAST(job.job) OR job-hdr.ord-no EQ est.ord-no THEN DO:
      RUN jc/jobstds.p (ROWID(job)).
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "ef"}
  {src/adm/template/snd-list.i "eb"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-first V-table-Win 
PROCEDURE set-first :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
gvl-first = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sheet-calc V-table-Win 
PROCEDURE sheet-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ll-is-sheet-calc = YES.
  ENABLE ef.m-code WITH FRAME {&frame-name}.
  APPLY "entry" TO ef.m-code IN FRAME {&frame-name}.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sheet-calc2 V-table-Win 
PROCEDURE sheet-calc2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* from ce/bestfitc.p */
    DEFINE BUFFER bf-item FOR ITEM .

     FIND xest WHERE xest.company = ef.company AND
                     xest.est-no = ef.est-no
                     NO-LOCK NO-ERROR.
     FIND xef WHERE RECID(xef) = recid(ef) NO-LOCK NO-ERROR.
     FIND xeb WHERE RECID(xeb) = recid(eb) NO-LOCK NO-ERROR.

     RUN cec/bestfitc.p (ef.m-code:SCREEN-VALUE IN FRAME {&FRAME-NAME}, 0, "","",ef.board).

     FIND FIRST tt-ef NO-ERROR.
     FIND FIRST tt-eb NO-ERROR.

     IF AVAIL tt-ef AND AVAIL tt-eb THEN
       ASSIGN ef.board:screen-value = tt-ef.board
              ef.brd-dscr:screen-value = tt-ef.brd-dscr
              ef.m-code:screen-value = tt-ef.m-code
              ef.weight:screen-value = STRING(tt-ef.weight)
              ef.i-code:screen-value = tt-ef.i-code
              ef.lsh-len:screen-value = STRING( tt-ef.lsh-len )
              ef.lsh-wid:screen-value = STRING( tt-ef.lsh-wid )
              ef.roll-wid:screen-value = STRING(tt-ef.roll-wid)
              ef.roll:screen-value = STRING(tt-ef.roll,"Y/N")
              ef.gsh-len:screen-value = STRING(tt-ef.gsh-len )
              ef.gsh-wid:screen-value = STRING( tt-ef.gsh-wid )
              ef.nsh-len:screen-value = STRING( tt-ef.nsh-len )
              ef.nsh-wid:screen-value = STRING( tt-ef.nsh-wid )
              ef.trim-l:screen-value = STRING( tt-ef.trim-l )
              ef.trim-w:screen-value = STRING(tt-ef.trim-w )
              ef.n-out:screen-value = STRING(tt-ef.n-out)
              ef.n-cuts:screen-value = STRING(tt-ef.n-cuts)
              eb.num-wid:screen-value = STRING(tt-eb.num-wid)
              eb.num-len:screen-value = STRING(tt-eb.num-len)
              eb.num-up:screen-value = STRING(tt-eb.num-up)
              .

     RUN roll-display.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-bom V-table-Win 
PROCEDURE update-bom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL ef THEN RUN ce/d-itmbom.w (ROWID(ef)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-film V-table-Win 
PROCEDURE update-film :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF bf-est.est-type <> 4 THEN RETURN.

  RUN ce/d-estflm.w (ef.company, ef.est-no, ef.eqty).
  RUN dispatch ('display-fields').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-gsh-len V-table-Win 
PROCEDURE valid-gsh-len :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR li AS INT NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF ll-auto-calc-selected                                                    AND
       ((DEC(ef.gsh-len:SCREEN-VALUE) LT DEC(ef.nsh-len:SCREEN-VALUE) AND
         ef.xgrain:SCREEN-VALUE NE "S")                                     OR
        (DEC(ef.gsh-len:SCREEN-VALUE) LT DEC(ef.nsh-wid:SCREEN-VALUE) AND
         ef.xgrain:SCREEN-VALUE EQ "S"))                                        THEN DO:
      MESSAGE "Gross Sheet Size can not be less than Net Sheet Size..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ef.gsh-len.
      RETURN ERROR.     
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-gsh-wid V-table-Win 
PROCEDURE valid-gsh-wid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ld-tons AS DEC INIT -1 NO-UNDO.

  DEF BUFFER bf-eb FOR eb.


  {methods/lValidateError.i YES}      

  IF dCelayoutDec GT 0 THEN RUN est/boardton.p (ROWID(ef), OUTPUT ld-tons).

  DO WITH FRAME {&FRAME-NAME}:
    IF ll-auto-calc-selected                                                    AND
       ((DEC(ef.gsh-wid:SCREEN-VALUE) LT DEC(ef.nsh-wid:SCREEN-VALUE) AND
         ef.xgrain:SCREEN-VALUE NE "S")                                     OR
        (DEC(ef.gsh-wid:SCREEN-VALUE) LT DEC(ef.nsh-len:SCREEN-VALUE) AND
         ef.xgrain:SCREEN-VALUE EQ "S"))                                        THEN DO:
      MESSAGE "Gross Sheet Size can not be less than Net Sheet Size..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ef.gsh-wid.
      RETURN ERROR.     
    END.

    FIND FIRST item
        WHERE item.company EQ gcompany
          AND item.i-no    EQ ef.board:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN DO:
      IF item.i-code EQ "R"                                                             AND
         ((ef.roll:SCREEN-VALUE EQ "Y" AND DEC(ef.gsh-wid:SCREEN-VALUE) NE item.r-wid)  OR
          (ef.roll:SCREEN-VALUE NE "Y" AND DEC(ef.gsh-wid:SCREEN-VALUE) NE item.s-wid)) THEN DO:
        MESSAGE "Gross Sheet Size may not be changed for a Real Material..." VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END.

      IF ef.roll:SCREEN-VALUE EQ "Y" AND ll-auto-calc-selected THEN DO:
        IF item.i-code EQ "E" THEN DO:
          IF lNewVendorItemCost THEN DO:
             ef.gsh-wid:SCREEN-VALUE = string(fGetVendItemCostRollWidth(ITEM.company, ITEM.i-no, DEC(ef.gsh-wid:SCREEN-VALUE)) ). 
          END.
          ELSE DO:  
              FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.
              IF AVAIL e-item AND ld-tons LT dCelayoutDec THEN
              DO li = 1 TO 26:
                IF e-item.roll-w[li] GE DEC(ef.gsh-wid:SCREEN-VALUE) THEN DO:
                  ef.gsh-wid:SCREEN-VALUE = STRING(e-item.roll-w[li]).
                  LEAVE.
                END.
              END.
          END.
          APPLY "value-changed" TO ef.roll.
        END.
      END.

      RUN roll-display.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-leaf-bnum V-table-Win 
PROCEDURE valid-leaf-bnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF BUFFER bf-eb FOR eb.

  DEF VAR lv-leaf AS CHAR NO-UNDO.
  DEF VAR lv-snum AS CHAR NO-UNDO.
  DEF VAR lv-bnum AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    CASE ip-int:
        WHEN 1 THEN
          ASSIGN
           lv-leaf = ef.leaf[1]:SCREEN-VALUE
           lv-snum = ef.leaf-snum[1]:SCREEN-VALUE
           lv-bnum = ef.leaf-bnum[1]:SCREEN-VALUE.
        WHEN 2 THEN
          ASSIGN
           lv-leaf = ef.leaf[2]:SCREEN-VALUE
           lv-snum = ef.leaf-snum[2]:SCREEN-VALUE
           lv-bnum = ef.leaf-bnum[2]:SCREEN-VALUE.
        WHEN 3 THEN
          ASSIGN
           lv-leaf = ef.leaf[3]:SCREEN-VALUE
           lv-snum = ef.leaf-snum[3]:SCREEN-VALUE
           lv-bnum = ef.leaf-bnum[3]:SCREEN-VALUE.
        WHEN 4 THEN
          ASSIGN
           lv-leaf = ef.leaf[4]:SCREEN-VALUE
           lv-snum = ef.leaf-snum[4]:SCREEN-VALUE
           lv-bnum = ef.leaf-bnum[4]:SCREEN-VALUE.
    END CASE.

    IF lv-leaf NE ""                                         AND
       NOT CAN-FIND(FIRST bf-eb
                    WHERE bf-eb.company   EQ eb.company
                      AND bf-eb.est-no    EQ eb.est-no
                      AND bf-eb.form-no   EQ INT(lv-snum)
                      AND (bf-eb.blank-no EQ INT(lv-bnum) OR
                           INT(lv-bnum) EQ 0))               THEN DO:
      MESSAGE "Blank does not exist on this form..." VIEW-AS ALERT-BOX ERROR.
      CASE ip-int:
        WHEN 1 THEN APPLY "entry" TO ef.leaf-bnum[1].
        WHEN 2 THEN APPLY "entry" TO ef.leaf-bnum[2].
        WHEN 3 THEN APPLY "entry" TO ef.leaf-bnum[3].
        WHEN 4 THEN APPLY "entry" TO ef.leaf-bnum[4].
      END CASE.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-leaf-snum V-table-Win 
PROCEDURE valid-leaf-snum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF BUFFER bf-ef FOR ef.

  DEF VAR lv-leaf AS CHAR NO-UNDO.
  DEF VAR lv-snum AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    CASE ip-int:
        WHEN 1 THEN
          ASSIGN
           lv-leaf = ef.leaf[1]:SCREEN-VALUE
           lv-snum = ef.leaf-snum[1]:SCREEN-VALUE.
        WHEN 2 THEN
          ASSIGN
           lv-leaf = ef.leaf[2]:SCREEN-VALUE
           lv-snum = ef.leaf-snum[2]:SCREEN-VALUE.
        WHEN 3 THEN
          ASSIGN
           lv-leaf = ef.leaf[3]:SCREEN-VALUE
           lv-snum = ef.leaf-snum[3]:SCREEN-VALUE.
        WHEN 4 THEN
          ASSIGN
           lv-leaf = ef.leaf[4]:SCREEN-VALUE
           lv-snum = ef.leaf-snum[4]:SCREEN-VALUE.
    END CASE.

    IF lv-leaf NE ""                                     /*AND
       NOT CAN-FIND(FIRST bf-ef
                    WHERE bf-ef.company EQ eb.company
                      AND bf-ef.est-no  EQ eb.est-no
                      AND bf-ef.form-no EQ INT(lv-snum))*/ THEN DO:
      /*MESSAGE "Form does not exist on this estimate..." VIEW-AS ALERT-BOX ERROR.
      CASE ip-int:
        WHEN 1 THEN APPLY "entry" TO ef.leaf-snum[1].
        WHEN 2 THEN APPLY "entry" TO ef.leaf-snum[2].
        WHEN 3 THEN APPLY "entry" TO ef.leaf-snum[3].
        WHEN 4 THEN APPLY "entry" TO ef.leaf-snum[4].
      END CASE.
      RETURN ERROR.*/
      CASE ip-int:
        WHEN 1 THEN ef.leaf-snum[1]:SCREEN-VALUE = STRING(ef.form-no).
        WHEN 2 THEN ef.leaf-snum[2]:SCREEN-VALUE = STRING(ef.form-no).
        WHEN 3 THEN ef.leaf-snum[3]:SCREEN-VALUE = STRING(ef.form-no).
        WHEN 4 THEN ef.leaf-snum[4]:SCREEN-VALUE = STRING(ef.form-no).
      END CASE.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShowHideCostFiled V-table-Win 
PROCEDURE pShowHideCostFiled :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hdVendorCostProcs     AS HANDLE    NO-UNDO.
  DEFIN VARIABLE lHideCostMsh AS LOGICAL NO-UNDO.
  RUN system\VendorCostProcs.p PERSISTENT SET hdVendorCostProcs. 
  
  DO WITH FRAME {&FRAME-NAME}:
    lHideCostMsh = DYNAMIC-FUNCTION("fVendCostHasEstimateOverride" IN hdVendorCostProcs,ef.company,ef.est-no,ef.form-no, ef.board ).
    IF lHideCostMsh THEN
    DO:
      OverrideExist:HIDDEN = FALSE .
      dtCost:HIDDEN = TRUE.
      ef.cost-uom:HIDDEN = TRUE.
      ef.cost-msh:HIDDEN = TRUE.
      FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR .
      IF AVAIL ef THEN
       ASSIGN ef.cost-msh = 0.
       FIND CURRENT ef NO-LOCK NO-ERROR .
    END.
    ELSE do:
      OverrideExist:HIDDEN = TRUE .
      dtCost:HIDDEN = FALSE.
      ef.cost-uom:HIDDEN = FALSE.
      ef.cost-msh:HIDDEN = FALSE.
    END.      
  END.
  DELETE OBJECT hdVendorCostProcs.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME 

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAvailVendItemCost V-table-Win 
FUNCTION fAvailVendItemCost RETURNS LOGICAL
  (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/       
        DEFINE VARIABLE lResult AS LOGICAL  NO-UNDO.

    FIND FIRST vendItemCost NO-LOCK WHERE vendItemCost.company = ipcCompany
                                      AND vendItemCost.itemID = ipcItemID
                                      NO-ERROR.
    lResult = IF AVAIL vendItemCost THEN YES ELSE NO.                                  
        RETURN lResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetVendItemCostRollWidth V-table-Win 
FUNCTION fGetVendItemCostRollWidth RETURNS DECIMAL
    (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER, ipdRollWidth AS DECIMAL ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dResult AS DECIMAL NO-UNDO.
    DEF VAR iCount AS INT NO-UNDO.
    
    FIND FIRST vendItemCost NO-LOCK WHERE vendItemCost.company = ipcCompany
                                     AND vendItemCost.itemID = ipcItemID
/*                                     AND vendItemCost.validWidth[1] > 0*/
                                     NO-ERROR .
    IF AVAIL vendItemCost THEN DO:
       IF ipdRollWidth = 0 THEN dResult = vendItemCost.validWidth[1].
       ELSE DO iCount = 1 TO 30:            
           IF vendItemCost.validWidth[iCount] >= ipdRollWidth THEN DO:
              dResult = vendItemCost.validWidth[iCount].
              LEAVE. 
           END. 
       END.     
     
    END.
                                                                  
        RETURN dResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetVendItemCostUOM V-table-Win 
FUNCTION fGetVendItemCostUOM RETURNS CHARACTER
    (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER   ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        DEFINE VARIABLE cResult AS CHARACTER  NO-UNDO.
        
    FIND FIRST vendItemCost NO-LOCK WHERE vendItemCost.company = ipcCompany
                                      AND vendItemCost.itemID = ipcItemID
                                      NO-ERROR.
    cResult = IF AVAIL vendItemCost THEN vendItemCost.vendorUOM ELSE "".  
    
    RETURN cResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

