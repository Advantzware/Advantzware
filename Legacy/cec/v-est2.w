&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: cec\v-est2.w

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
{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i new shared}
def var ll-auto-calc-selected as log no-undo.
def var k_frac as dec init 6.25 no-undo.
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

&scoped-define est-layout layout       /* for method enable */
&scoped-define proc-enable proc-enable       /* for method enable */
def var lv-is-roll as log no-undo.
def var ls-lam-dscr as cha no-undo.
def var lv-is-foam as log no-undo.
def var lv-industry as cha no-undo.
def var ld-roll-wid as dec no-undo.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def var uom-list as cha no-undo.
def var ll-is-sheet-calc as log no-undo.
def var ll-is-canceled as log no-undo.
def var ll-num-lw-changed as log no-undo.  /* if num-len, eb.num-wid changed */
def var ll-num-out-changed as log no-undo.  /* if n-out, eb.n-out-l changed */
DEF VAR ll-one-eb-on-ef AS LOG NO-UNDO.  
DEF VAR ll-one-ef-on-est AS LOG NO-UNDO.
DEF VAR ll-part-style AS LOG NO-UNDO.

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
DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR lv-n-out LIKE ef.n-out NO-UNDO.
DEF VAR lv-n-out-d LIKE ef.n-out NO-UNDO.
DEF VAR lv-n-out-l LIKE ef.n-out NO-UNDO.
DEF VAR cRtnChar AS CHARACTER NO-UNDO.
DEF VAR lRecFound AS LOGICAL NO-UNDO .
DEF VAR lShtcalcWarm-log AS LOGICAL NO-UNDO .

{cec/bestfitc.i NEW SHARED}

assign cocode = g_company
       locode = g_loc.
{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Corr

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est ef eb
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, ef, eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ef.spare-int-2 ef.spare-int-3 ef.spare-int-4 ~
ef.lsh-wid ef.lsh-len ef.xgrain ef.board ef.cost-uom ef.cost-msh ef.fr-uom ~
ef.fr-msh ef.nc ef.gsh-wid ef.gsh-len ef.gsh-dep ef.nsh-wid ef.nsh-len ~
ef.nsh-dep ef.trim-w ef.trim-l ef.trim-d eb.num-len eb.num-wid ef.n-out ~
ef.n-out-l ef.n-out-d eb.num-up ef.n-cuts ef.die-in ef.adder[1] ef.adder[7] ~
ef.adder[2] ef.adder[8] ef.adder[3] ef.adder[9] ef.adder[4] ef.adder[10] ~
ef.adder[5] ef.adder[11] ef.adder[6] ef.adder[12] ef.leaf[1] ~
ef.leaf-dscr[1] ef.leaf-bnum[1] ef.leaf-w[1] ef.leaf-l[1] ef.leaf[2] ~
ef.leaf-dscr[2] ef.leaf-bnum[2] ef.leaf-w[2] ef.leaf-l[2] ef.leaf[3] ~
ef.leaf-dscr[3] ef.leaf-bnum[3] ef.leaf-w[3] ef.leaf-l[3] ef.leaf[4] ~
ef.leaf-dscr[4] ef.leaf-bnum[4] ef.leaf-w[4] ef.leaf-l[4] ef.roll ~
ef.spare-int-1 
&Scoped-define ENABLED-TABLES ef eb
&Scoped-define FIRST-ENABLED-TABLE ef
&Scoped-define SECOND-ENABLED-TABLE eb
&Scoped-Define ENABLED-OBJECTS btn_board ls-d-up-label RECT-20 RECT-21 RECT-7 RECT-9 
&Scoped-Define DISPLAYED-FIELDS ef.spare-int-2 ef.spare-int-3 ~
ef.spare-int-4 ef.m-code ef.m-dscr ef.lsh-wid ef.lsh-len ef.xgrain ef.board ~
ef.brd-dscr ef.i-code ef.flute ef.test ef.cost-uom ef.cost-msh ef.weight ~
ef.fr-uom ef.fr-msh ef.nc ef.gsh-wid ef.gsh-len ef.gsh-dep ef.nsh-wid ~
ef.nsh-len ef.nsh-dep ef.trim-w ef.trim-l ef.trim-d eb.num-len eb.num-wid ~
eb.num-dep ef.n-out ef.n-out-l ef.n-out-d eb.num-up ef.n-cuts ef.die-in ~
eb.t-wid eb.t-len eb.t-dep eb.t-sqin ef.adder[1] ef.adder[7] ef.adder[2] ~
ef.adder[8] ef.adder[3] ef.adder[9] ef.adder[4] ef.adder[10] ef.adder[5] ~
ef.adder[11] ef.adder[6] ef.adder[12] ef.leaf[1] ef.leaf-dscr[1] ~
ef.leaf-snum[1] ef.leaf-bnum[1] ef.leaf-w[1] ef.leaf-l[1] ef.leaf[2] ~
ef.leaf-dscr[2] ef.leaf-snum[2] ef.leaf-bnum[2] ef.leaf-w[2] ef.leaf-l[2] ~
ef.leaf[3] ef.leaf-dscr[3] ef.leaf-snum[3] ef.leaf-bnum[3] ef.leaf-w[3] ~
ef.leaf-l[3] ef.leaf[4] ef.leaf-dscr[4] ef.leaf-snum[4] ef.leaf-bnum[4] ~
ef.leaf-w[4] ef.leaf-l[4] ef.roll ef.spare-int-1 
&Scoped-define DISPLAYED-TABLES ef eb
&Scoped-define FIRST-DISPLAYED-TABLE ef
&Scoped-define SECOND-DISPLAYED-TABLE eb
&Scoped-Define DISPLAYED-OBJECTS ls-dep-label ls-d-up-label 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS ef.m-code ef.m-dscr ef.brd-dscr ef.flute ~
ef.test eb.num-up ef.leaf-snum[1] ef.leaf-snum[2] ef.leaf-snum[3] ~
ef.leaf-snum[4] 
&Scoped-define List-5 ef.roll 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotalUp V-table-Win 
FUNCTION getTotalUp RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE ls-d-up-label AS CHARACTER FORMAT "X(256)":U INITIAL "Depth" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE ls-dep-label AS CHARACTER FORMAT "X(256)":U INITIAL "Depth" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FGCOLOR 1  NO-UNDO.

DEFINE BUTTON btn_board
     LABEL "" 
     SIZE 11 BY 1.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 147 BY 16.19.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143 BY 5.24.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 7.14.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 7.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Corr
     ef.spare-int-2 AT ROW 8.62 COL 71.2 COLON-ALIGNED WIDGET-ID 10
          LABEL "# Hits" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     ef.spare-int-3 AT ROW 8.62 COL 94.4 COLON-ALIGNED WIDGET-ID 12
          LABEL "# Out/Hit: W" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     ef.spare-int-4 AT ROW 8.62 COL 103.4 COLON-ALIGNED WIDGET-ID 14
          LABEL "L" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     ef.m-code AT ROW 1.48 COL 12 COLON-ALIGNED
          LABEL "Machine" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     ef.m-dscr AT ROW 1.48 COL 25 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
     ef.lsh-wid AT ROW 1.48 COL 80.4 COLON-ALIGNED HELP
          "This is the Machine Length, Pulled from Machine file"
          LABEL "Front-Back" FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     ef.lsh-len AT ROW 1.48 COL 107 COLON-ALIGNED HELP
          "This is the Machine Width, Pulled from Machine file"
          LABEL "Side-Side" FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     ef.xgrain AT ROW 1.48 COL 134 COLON-ALIGNED
          LABEL "Rev. Corr" FORMAT "X"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "None","N",
                     "Sheet","S",
                     "Blank","B"
          DROP-DOWN-LIST
          SIZE 11.5 BY 1
     ef.board AT ROW 2.43 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     btn_board AT ROW 2.43 COL 3 WIDGET-ID 16
     ef.brd-dscr AT ROW 2.43 COL 28 COLON-ALIGNED NO-LABEL FORMAT "X(50)"
          VIEW-AS FILL-IN 
          SIZE 70 BY 1
     ef.i-code AT ROW 2.43 COL 111 COLON-ALIGNED
          LABEL "Real"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.flute AT ROW 3.38 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     ef.test AT ROW 3.38 COL 35 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.cost-uom AT ROW 3.38 COL 60 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.cost-msh AT ROW 3.38 COL 65 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     ef.weight AT ROW 3.38 COL 86 COLON-ALIGNED
          LABEL "Wt"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     ef.fr-uom AT ROW 3.38 COL 111 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ef.fr-msh AT ROW 3.38 COL 120 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ef.nc AT ROW 3.14 COL 140 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     ef.gsh-wid AT ROW 5.52 COL 17.6 COLON-ALIGNED
          LABEL "Gross Sheet" FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     ef.gsh-len AT ROW 5.52 COL 33.2 COLON-ALIGNED NO-LABEL FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     ef.gsh-dep AT ROW 5.52 COL 48.4 COLON-ALIGNED NO-LABEL FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     ef.nsh-wid AT ROW 6.48 COL 17.6 COLON-ALIGNED
          LABEL "Net Sheet" FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     ef.nsh-len AT ROW 6.48 COL 33.2 COLON-ALIGNED NO-LABEL FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Corr
     ef.nsh-dep AT ROW 6.48 COL 48.4 COLON-ALIGNED NO-LABEL FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     ef.trim-w AT ROW 7.43 COL 17.6 COLON-ALIGNED
          LABEL "Die Size" FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     ef.trim-l AT ROW 7.43 COL 33.2 COLON-ALIGNED NO-LABEL FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     ef.trim-d AT ROW 7.43 COL 48.4 COLON-ALIGNED NO-LABEL FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     eb.num-len AT ROW 7.43 COL 70 COLON-ALIGNED
          LABEL "# On" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.num-wid AT ROW 7.43 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.num-dep AT ROW 7.43 COL 88 COLON-ALIGNED NO-LABEL FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.n-out AT ROW 5.76 COL 70 COLON-ALIGNED
          LABEL "#Out" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.n-out-l AT ROW 5.76 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.n-out-d AT ROW 5.76 COL 88 COLON-ALIGNED NO-LABEL FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.num-up AT ROW 7.43 COL 104.8 COLON-ALIGNED NO-LABEL FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.n-cuts AT ROW 5.76 COL 97 COLON-ALIGNED NO-LABEL FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.die-in AT ROW 6.71 COL 129 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     eb.t-wid AT ROW 8.62 COL 17.6 COLON-ALIGNED
          LABEL "Blank" FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     eb.t-len AT ROW 8.62 COL 33.2 COLON-ALIGNED NO-LABEL FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     eb.t-dep AT ROW 8.62 COL 48.4 COLON-ALIGNED NO-LABEL FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     eb.t-sqin AT ROW 8.62 COL 117.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.adder[1] AT ROW 10.76 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.adder[7] AT ROW 10.76 COL 21.6 COLON-ALIGNED NO-LABEL FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 36.4 BY 1
     ef.adder[2] AT ROW 11.76 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.adder[8] AT ROW 11.76 COL 21.6 COLON-ALIGNED NO-LABEL FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 36.4 BY 1
     ef.adder[3] AT ROW 12.76 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.adder[9] AT ROW 12.76 COL 21.6 COLON-ALIGNED NO-LABEL FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 36.4 BY 1
     ef.adder[4] AT ROW 13.76 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.adder[10] AT ROW 13.76 COL 21.6 COLON-ALIGNED NO-LABEL FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 36.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Corr
     ef.adder[5] AT ROW 14.76 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.adder[11] AT ROW 14.81 COL 21.6 COLON-ALIGNED NO-LABEL FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 36.4 BY 1
     ef.adder[6] AT ROW 15.76 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.adder[12] AT ROW 15.76 COL 21.6 COLON-ALIGNED NO-LABEL FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 36.4 BY 1
     ef.leaf[1] AT ROW 11 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[1] AT ROW 11 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[1] AT ROW 11 COL 103 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-bnum[1] AT ROW 11 COL 110 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-w[1] AT ROW 11 COL 117.8 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.4 BY 1
     ef.leaf-l[1] AT ROW 11 COL 130.4 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.4 BY 1
     ef.leaf[2] AT ROW 12.19 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[2] AT ROW 12.19 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ls-dep-label AT ROW 4.91 COL 50.4 COLON-ALIGNED NO-LABEL
     ef.leaf-snum[2] AT ROW 12.19 COL 103 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ls-d-up-label AT ROW 5.05 COL 88 COLON-ALIGNED NO-LABEL
     ef.leaf-bnum[2] AT ROW 12.19 COL 110 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-w[2] AT ROW 12.19 COL 117.8 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.4 BY 1
     ef.leaf-l[2] AT ROW 12.19 COL 130.4 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.4 BY 1
     ef.leaf[3] AT ROW 13.38 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[3] AT ROW 13.38 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.leaf-snum[3] AT ROW 13.38 COL 103 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-bnum[3] AT ROW 13.38 COL 110 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-w[3] AT ROW 13.38 COL 117.8 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.4 BY 1
     ef.leaf-l[3] AT ROW 13.38 COL 130.4 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.4 BY 1
     ef.leaf[4] AT ROW 14.57 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     ef.leaf-dscr[4] AT ROW 14.57 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Corr
     ef.leaf-snum[4] AT ROW 14.57 COL 103 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-bnum[4] AT ROW 14.57 COL 110 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ef.leaf-w[4] AT ROW 14.57 COL 117.8 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.4 BY 1
     ef.leaf-l[4] AT ROW 14.57 COL 130.4 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.4 BY 1
     ef.roll AT ROW 4.81 COL 9
          LABEL "Roll"
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .81
     ef.spare-int-1 AT ROW 5.76 COL 105 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 2 FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     "Description" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 10.05 COL 32
          FGCOLOR 9 
     "Adders" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 10.05 COL 5
          FGCOLOR 9 
     "S  /  B" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 10.29 COL 106
          FGCOLOR 9 
     "Cost/" VIEW-AS TEXT
          SIZE 6.4 BY .95 AT ROW 3.38 COL 53
     "Freight/" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 3.62 COL 112 RIGHT-ALIGNED
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.91 COL 37
          FGCOLOR 1 
     "Width" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 10.29 COL 120.2
          FGCOLOR 9 
     "Length" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 5.05 COL 81
          FGCOLOR 1 
     "Die Inches" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 5.05 COL 131
     "Total Up" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 5.05 COL 107
     "Sq. Feet" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 5.05 COL 119
     "Width" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 5.05 COL 72
          FGCOLOR 1 
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 10.29 COL 132.8
          FGCOLOR 9 
     "Width" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 4.91 COL 21.8
          FGCOLOR 1 
     "Cut" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 5.05 COL 99
     "Wax / Label" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 10.29 COL 63
          FGCOLOR 9 
     "Description" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 10.29 COL 80
          FGCOLOR 9 
     RECT-20 AT ROW 1 COL 1
     RECT-21 AT ROW 4.57 COL 3
     RECT-7 AT ROW 9.81 COL 3
     RECT-9 AT ROW 9.81 COL 61
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
         HEIGHT             = 17
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
/* SETTINGS FOR FRAME Corr
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME Corr:SCROLLABLE       = FALSE
       FRAME Corr:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ef.adder[10] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.adder[11] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.adder[12] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.adder[7] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.adder[8] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.adder[9] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.brd-dscr IN FRAME Corr
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.cost-uom IN FRAME Corr
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN ef.flute IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ef.fr-uom IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.gsh-dep IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.gsh-dep:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.gsh-len IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.gsh-len:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.gsh-wid IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       ef.gsh-wid:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.i-code IN FRAME Corr
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ef.leaf-bnum[1] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.leaf-bnum[2] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.leaf-bnum[3] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.leaf-bnum[4] IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.leaf-l[1] IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.leaf-l[1]:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.leaf-l[2] IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.leaf-l[2]:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.leaf-l[3] IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.leaf-l[3]:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.leaf-l[4] IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.leaf-l[4]:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.leaf-snum[1] IN FRAME Corr
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.leaf-snum[2] IN FRAME Corr
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.leaf-snum[3] IN FRAME Corr
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.leaf-snum[4] IN FRAME Corr
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ef.leaf-w[1] IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.leaf-w[1]:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.leaf-w[2] IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.leaf-w[2]:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.leaf-w[3] IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.leaf-w[3]:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.leaf-w[4] IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.leaf-w[4]:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ls-dep-label IN FRAME Corr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.lsh-len IN FRAME Corr
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN ef.lsh-wid IN FRAME Corr
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN ef.m-code IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN ef.m-dscr IN FRAME Corr
   NO-ENABLE ALIGN-L 2 EXP-LABEL                                        */
/* SETTINGS FOR FILL-IN ef.n-cuts IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ef.n-out IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ef.n-out-d IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.n-out-l IN FRAME Corr
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.nsh-dep IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.nsh-dep:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.nsh-len IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       ef.nsh-len:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.nsh-wid IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       ef.nsh-wid:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN eb.num-dep IN FRAME Corr
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.num-len IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.num-up IN FRAME Corr
   2 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.num-wid IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX ef.roll IN FRAME Corr
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN ef.spare-int-1 IN FRAME Corr
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN ef.spare-int-2 IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ef.spare-int-3 IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ef.spare-int-4 IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.t-dep IN FRAME Corr
   NO-ENABLE EXP-FORMAT                                                 */
ASSIGN 
       eb.t-dep:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN eb.t-len IN FRAME Corr
   NO-ENABLE EXP-FORMAT                                                 */
ASSIGN 
       eb.t-len:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN eb.t-sqin IN FRAME Corr
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.t-wid IN FRAME Corr
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       eb.t-wid:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.test IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ef.trim-d IN FRAME Corr
   EXP-FORMAT                                                           */
ASSIGN 
       ef.trim-d:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.trim-l IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       ef.trim-l:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.trim-w IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       ef.trim-w:PRIVATE-DATA IN FRAME Corr     = 
                "16th".

/* SETTINGS FOR FILL-IN ef.weight IN FRAME Corr
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR COMBO-BOX ef.xgrain IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TEXT-LITERAL "Freight/"
          SIZE 10 BY .62 AT ROW 3.62 COL 112 RIGHT-ALIGNED              */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Corr
/* Query rebuild information for FRAME Corr
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME Corr */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Corr V-table-Win
ON HELP OF FRAME Corr
DO:
   def var lv-handle as widget-handle no-undo.
   def var lv-ind like style.industry no-undo.
   def var ls-cur-val as cha no-undo.
   def var char-val as cha no-undo.
   def var lv-rowid as rowid no-undo.
   DEF VAR lw-focus AS WIDGET NO-UNDO.


   lw-focus = FOCUS.

   case lw-focus:name :
     when "Board" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  /* foam */
                 run windows/l-boardf.w (eb.company,lv-ind,lw-focus:screen-value,output char-val).
           else run windows/l-board1.w (eb.company,lv-ind,lw-focus:screen-value, output lv-rowid).
           IF lv-rowid NE ? THEN DO:
             FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
             IF AVAIL ITEM AND ITEM.i-no NE lw-focus:SCREEN-VALUE THEN DO:
               ef.board:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.i-no.
               RUN new-board.
             END.
           END.   
           ELSE
           IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
             ef.board:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,char-val).
             RUN new-board. 
           END.
           return no-apply.   
     end.
     when "leaf" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (eb.company,lv-ind,"F,W",lw-focus:screen-value, output char-val).
           IF char-val NE "" AND entry(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
              APPLY "value-changed" TO lw-focus.                         
           end.  /* char-val */                   
     end.  /* leaf */
     when "m-code" then do:
          run windows/l-mach.w (gcompany,ef.loc, lw-focus:screen-value, output char-val).

          IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO: 
             lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
             RUN new-m-code.
          END.
          return no-apply.          
     end.
     when "flute" then do:
           run windows/l-flute.w (gcompany,output char-val).
           if char-val <> "" then
              lw-focus:screen-value =  entry(1,char-val).
           return no-apply.
     end.
     when "test" then do:
           ls-cur-val = ef.flute:screen-value.
           run windows/l-test.w (gcompany,ef.loc,ls-cur-val,output char-val).
           if char-val <> "" then
              lw-focus:screen-value  =  entry(1,char-val).
           return no-apply.       
     end.
     when "adder" then do:
          find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
          if avail style then lv-ind = style.industry.
          else lv-ind = "".  
          run windows/l-boarda.w (eb.company,lv-ind,lw-focus:screen-value, output char-val).
          if char-val <> "" AND lw-focus:screen-value NE entry(1,char-val) then do:
              lw-focus:screen-value = entry(1,char-val).
              RUN new-adder (lw-focus).
          end.    
          return no-apply.       
     end.
     when "cost-uom" then do:
          find first item where item.company = gcompany and
                                item.i-no = ef.board:screen-value
                                no-lock no-error.
          if avail item then find first e-item of item no-lock no-error.
          run sys/ref/uom-rm.p  (item.mat-type, output uom-list).
          run windows/l-stduom.w (gcompany,uom-list, lw-focus:screen-value, output char-val).
          if char-val <> "" then 
             assign lw-focus:screen-value = entry(1,char-val)
                    .
     end.
     when "fr-uom" then do:
          uom-list = "CWT,MSF,MSH,TON".
          run windows/l-stduom.w (gcompany,uom-list, lw-focus:screen-value, output char-val).
          if char-val <> "" then 
             assign lw-focus:screen-value = entry(1,char-val)
                    .
     end.
     otherwise do:
           lv-handle = lw-focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then lw-focus:screen-value = g_lookup-var.
     end.  /* otherwise */
  end case.

  apply "entry" to lw-focus.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.adder[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[1] V-table-Win
ON LEAVE OF ef.adder[1] IN FRAME Corr /* Adder[1] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adder (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[1] V-table-Win
ON VALUE-CHANGED OF ef.adder[1] IN FRAME Corr /* Adder[1] */
DO:
  RUN new-adder (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.adder[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[2] V-table-Win
ON LEAVE OF ef.adder[2] IN FRAME Corr /* Adder[2] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adder (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[2] V-table-Win
ON VALUE-CHANGED OF ef.adder[2] IN FRAME Corr /* Adder[2] */
DO:
  RUN new-adder (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.adder[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[3] V-table-Win
ON LEAVE OF ef.adder[3] IN FRAME Corr /* Adder[3] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adder (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[3] V-table-Win
ON VALUE-CHANGED OF ef.adder[3] IN FRAME Corr /* Adder[3] */
DO:
  RUN new-adder (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.adder[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[4] V-table-Win
ON LEAVE OF ef.adder[4] IN FRAME Corr /* Adder[4] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adder (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[4] V-table-Win
ON VALUE-CHANGED OF ef.adder[4] IN FRAME Corr /* Adder[4] */
DO:
  RUN new-adder (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.adder[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[5] V-table-Win
ON LEAVE OF ef.adder[5] IN FRAME Corr /* Adder[5] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adder (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[5] V-table-Win
ON VALUE-CHANGED OF ef.adder[5] IN FRAME Corr /* Adder[5] */
DO:
  RUN new-adder (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.adder[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[6] V-table-Win
ON LEAVE OF ef.adder[6] IN FRAME Corr /* Adder[6] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adder (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adder[6] V-table-Win
ON VALUE-CHANGED OF ef.adder[6] IN FRAME Corr /* Adder[6] */
DO:
  RUN new-adder (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_board V-table-Win
ON CHOOSE OF btn_board IN FRAME Corr
DO:
  IF AVAIL eb THEN
   FIND FIRST ITEM WHERE ITEM.company  = cocode
       AND ITEM.i-no = ef.board NO-LOCK NO-ERROR.

   IF AVAIL ITEM THEN
   RUN cec/w-itemc.w(RECID(ITEM)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board V-table-Win
ON LEAVE OF ef.board IN FRAME Corr /* Board */
DO:
   if lastkey <> -1 and self:screen-value <> "" 
   then do:
  {&methods/lValidateError.i YES}
       find first item where item.company = gcompany and
                             ((index("BPR",item.mat-type) > 0 and not lv-is-foam) or
                              (index("1234",item.mat-type) > 0 and lv-is-foam) ) and
                              item.industry = lv-industry and
                              item.i-no = self:screen-value
                              no-lock no-error.
       if not avail item then do:
          message "Invalid Board. Try Help." view-as alert-box error.
          return no-apply.
       end.
       if item.i-code = "R" and ll-auto-calc-selected then do:
          if item.r-wid = 0 then do:
             if item.s-wid < eb.t-wid then do:
                message "Sheet Width less than Blank Width. " view-as alert-box error.
                return no-apply.
             end.
             if item.s-len < eb.t-len then do:
                message "Sheet Length less than Blank Length. " view-as alert-box error.
                return no-apply.
             end.
          end.  /* r-wid = 0 */
          else if item.r-wid < eb.t-wid then do:
                message "Roll Width less than Blank Width. " view-as alert-box error.
                return no-apply.
          end.
       end.

       IF ll-auto-calc-selected AND (ll-one-eb-on-ef OR ll-part-style) THEN DO:
         find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).
         assign xef.m-code = ef.m-code:screen-value
                xef.lsh-lock = no
                xef.board = ef.board:screen-value
                xef.roll   = ef.roll:screen-value in frame {&frame-name} EQ "Y"
                .

         run cec/calc-dim.p .
         find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).
         assign ef.lsh-len:screen-value = string({sys/inc/k16.i xef.lsh-len} )
                ef.lsh-wid:screen-value = string({sys/inc/k16.i xef.lsh-wid} )
                ef.gsh-len:screen-value = string({sys/inc/k16.i xef.gsh-len} )
                ef.gsh-wid:screen-value = string({sys/inc/k16.i xef.gsh-wid} )
                ef.nsh-len:screen-value = string({sys/inc/k16.i xef.nsh-len} )
                ef.nsh-wid:screen-value = string({sys/inc/k16.i xef.nsh-wid} )
                ef.trim-l:screen-value = string({sys/inc/k16.i xef.trim-l} )
                ef.trim-w:screen-value = string({sys/inc/k16.i xef.trim-w} )
                ef.n-out:screen-value = string(xef.n-out)
                ef.n-out-l:screen-value = string(xef.n-out-l)
                ef.n-cuts:screen-value = string(xef.n-cuts)
                ef.roll:SCREEN-VALUE = IF xef.roll THEN "Y" ELSE "N"
                lv-is-roll = xef.roll
                ef.adder[01]:SCREEN-VALUE = xef.adder[01]
                ef.adder[02]:SCREEN-VALUE = xef.adder[02]
                ef.adder[03]:SCREEN-VALUE = xef.adder[03]
                ef.adder[04]:SCREEN-VALUE = xef.adder[04]
                ef.adder[05]:SCREEN-VALUE = xef.adder[05]
                ef.adder[06]:SCREEN-VALUE = xef.adder[06]
                ef.adder[07]:SCREEN-VALUE = xef.adder[07]
                ef.adder[08]:SCREEN-VALUE = xef.adder[08]
                ef.adder[09]:SCREEN-VALUE = xef.adder[09]
                ef.adder[10]:SCREEN-VALUE = xef.adder[10]
                ef.adder[11]:SCREEN-VALUE = xef.adder[11]
                ef.adder[12]:SCREEN-VALUE = xef.adder[12]
                eb.num-wid:screen-value = string(xeb.num-wid)
                eb.num-len:screen-value = string(xeb.num-len)
                eb.num-up:screen-value = string(xeb.num-up)
                .
         RUN roll-display.

         if lv-is-foam then       
           assign ef.gsh-dep:screen-value = string({sys/inc/k16.i xef.gsh-dep} )
                  ef.nsh-dep:screen-value = string({sys/inc/k16.i xef.nsh-dep} )
                  ef.trim-d:screen-value = string({sys/inc/k16.i xef.trim-d} )
                  ef.n-out-d:screen-value = string(xef.n-out-d)
                  .
       END.
{&methods/lValidateError.i NO}
   end.  /* lastkey <> -1 */
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board V-table-Win
ON VALUE-CHANGED OF ef.board IN FRAME Corr /* Board */
DO:
  RUN new-board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.cost-uom V-table-Win
ON LEAVE OF ef.cost-uom IN FRAME Corr /* cost-uom */
DO:
    if lastkey <> -1 and ef.cost-uom:screen-value <> "" then do:
{&methods/lValidateError.i YES}
       find first item where item.company = gcompany and
                                item.i-no = ef.board
                                no-lock no-error.
       if avail item then find first e-item of item no-lock no-error.
       run sys/ref/uom-rm.p  (item.mat-type, output uom-list).
       if not can-find(first uom where lookup(uom.uom,uom-list) > 0 and
                            uom.uom = ef.cost-uom:screen-value)
       then do:
            message "Invalid Cost UOM. Try Help."   view-as alert-box.
            return no-apply.
       end.                 
    end.  
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.fr-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.fr-uom V-table-Win
ON LEAVE OF ef.fr-uom IN FRAME Corr /* fr-uom */
DO:
    if lastkey <> -1 and ef.fr-uom:screen-value <> "" then do:
 {&methods/lValidateError.i YES}
       /*find first item where item.company = gcompany and
                                item.i-no = ef.board:screen-value
                                no-lock no-error.
       if avail item then find first e-item of item no-lock no-error.
       run sys/ref/uom-rm.p  (item.mat-type, output uom-list).
       */
       uom-list = "CWT,MSF,MSH,TON".
       if not can-find(first uom where lookup(uom.uom,uom-list) > 0 and
                            uom.uom = ef.fr-uom:screen-value)
       then do:
            message "Invalid Freight UOM. Try Help." view-as alert-box.
            return no-apply.
       end. 
{&methods/lValidateError.i NO}                
    end.  

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.gsh-dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.gsh-dep V-table-Win
ON LEAVE OF ef.gsh-dep IN FRAME Corr /* Depth */
DO:
/*   IF LASTKEY NE -1 THEN DO:                     */
/*     RUN valid-gsh-dep NO-ERROR.                 */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*   END.                                          */

  IF lv-is-foam THEN
    RUN get-ef-nsh.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.gsh-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.gsh-len V-table-Win
ON LEAVE OF ef.gsh-len IN FRAME Corr /* Length */
DO:
/*   IF LASTKEY NE -1 THEN DO:                     */
/*     RUN valid-gsh-len NO-ERROR.                 */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*   END.                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.gsh-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.gsh-wid V-table-Win
ON LEAVE OF ef.gsh-wid IN FRAME Corr /* Gross Sheet */
DO:
/*   IF LASTKEY NE -1 THEN DO:                     */
/*     RUN valid-gsh-wid NO-ERROR.                 */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*   END.                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-bnum[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-bnum[1] V-table-Win
ON LEAVE OF ef.leaf-bnum[1] IN FRAME Corr /* leaf-bnum */
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
ON LEAVE OF ef.leaf-bnum[2] IN FRAME Corr /* leaf-bnum */
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
ON LEAVE OF ef.leaf-bnum[3] IN FRAME Corr /* leaf-bnum */
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
ON LEAVE OF ef.leaf-bnum[4] IN FRAME Corr /* leaf-bnum */
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
ON LEAVE OF ef.leaf-l[1] IN FRAME Corr /* Length[1] */
DO:
  IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-l[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-l[2] V-table-Win
ON LEAVE OF ef.leaf-l[2] IN FRAME Corr /* Length[2] */
DO:
  IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-l[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-l[3] V-table-Win
ON LEAVE OF ef.leaf-l[3] IN FRAME Corr /* Length[3] */
DO:
  IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
  {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-l[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-l[4] V-table-Win
ON LEAVE OF ef.leaf-l[4] IN FRAME Corr /* Length[4] */
DO:
  IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-snum[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-snum[1] V-table-Win
ON LEAVE OF ef.leaf-snum[1] IN FRAME Corr /* S/B[1] */
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
ON LEAVE OF ef.leaf-snum[2] IN FRAME Corr /* S/B[2] */
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
ON LEAVE OF ef.leaf-snum[3] IN FRAME Corr /* S/B[3] */
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
ON LEAVE OF ef.leaf-snum[4] IN FRAME Corr /* S/B[4] */
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
ON LEAVE OF ef.leaf-w[1] IN FRAME Corr /* Width[1] */
DO:
  IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
    {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-w[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-w[2] V-table-Win
ON LEAVE OF ef.leaf-w[2] IN FRAME Corr /* Width[2] */
DO:
   IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-w[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-w[3] V-table-Win
ON LEAVE OF ef.leaf-w[3] IN FRAME Corr /* Width[3] */
DO:
   IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf-w[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf-w[4] V-table-Win
ON LEAVE OF ef.leaf-w[4] IN FRAME Corr /* Width[4] */
DO:
  IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[1] V-table-Win
ON LEAVE OF ef.leaf[1] IN FRAME Corr /* Leaf/Film[1] */
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
ON VALUE-CHANGED OF ef.leaf[1] IN FRAME Corr /* Leaf/Film[1] */
DO:
  {est/new-leaf.i 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[2] V-table-Win
ON LEAVE OF ef.leaf[2] IN FRAME Corr /* Leaf/Film[2] */
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
ON VALUE-CHANGED OF ef.leaf[2] IN FRAME Corr /* Leaf/Film[2] */
DO:
  {est/new-leaf.i 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[3] V-table-Win
ON LEAVE OF ef.leaf[3] IN FRAME Corr /* Leaf/Film[3] */
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
ON VALUE-CHANGED OF ef.leaf[3] IN FRAME Corr /* Leaf/Film[3] */
DO:
  {est/new-leaf.i 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.leaf[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.leaf[4] V-table-Win
ON LEAVE OF ef.leaf[4] IN FRAME Corr /* Leaf/Film[4] */
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
ON VALUE-CHANGED OF ef.leaf[4] IN FRAME Corr /* Leaf/Film[4] */
DO:
  {est/new-leaf.i 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.lsh-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.lsh-wid V-table-Win
ON ENTRY OF ef.lsh-wid IN FRAME Corr /* Front-Back */
DO:
   do with frame {&frame-name} :

     IF not ll-auto-calc-selected AND ef.i-code:SCREEN-VALUE = "R" THEN 
        ASSIGN
          ef.gsh-wid:SENSITIVE = NO
          ef.gsh-len:SENSITIVE = NO.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.m-code V-table-Win
ON LEAVE OF ef.m-code IN FRAME Corr /* Machine */
DO:

    if lastkey = -1 then return.
{&methods/lValidateError.i YES}

    if ll-is-sheet-calc then do:
          def var source-str as cha no-undo.   
          RUN get-link-handle IN adm-broker-hdl 
              (THIS-PROCEDURE, 'Tableio-source':U, OUTPUT source-str).
          run apply-sheet-calc in widget-handle(source-str).                 
          RETURN.
    end.
    if ef.m-code:screen-value = "" then do:
       assign ef.m-dscr:screen-value = ""
              ef.lsh-wid:screen-value = "0"
              ef.lsh-len:screen-value = "0".
    end.

    if lastkey <> -1 and ef.m-code:screen-value <> "" and
       not can-find (first mach where mach.company = gcompany and
                                      mach.loc = eb.loc and
                                      mach.m-code = ef.m-code:screen-value)
    then do:
         message "Invalid Machine Code. Try Help." view-as alert-box error.
         return no-apply.
    end.

    IF ll-auto-calc-selected AND (ll-one-eb-on-ef OR ll-part-style) THEN DO:

       find xef where recid(xef) = recid(ef).
       find xeb where recid(xeb) = recid(eb).
       assign xef.m-code = ef.m-code:screen-value
              xef.lsh-lock = no
              xef.lsh-len = dec(ef.lsh-len:screen-value)
              xef.lsh-wid = dec(ef.lsh-wid:screen-value)
              xef.n-out = 0
              xef.n-out-l = 0
              xef.n-out-d = 0.
       {sys/inc/k16bb.i xef.lsh-len }
       {sys/inc/k16bb.i xef.lsh-wid }
       run cec/calc-dim.p .

       find xef where recid(xef) = recid(ef).
       find xeb where recid(xeb) = recid(eb).
       assign ef.lsh-len:screen-value = string({sys/inc/k16.i xef.lsh-len} )
              ef.lsh-wid:screen-value = string({sys/inc/k16.i xef.lsh-wid} )
              ef.gsh-len:screen-value = string({sys/inc/k16.i xef.gsh-len} )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i xef.gsh-wid} )
              ef.nsh-len:screen-value = string({sys/inc/k16.i xef.nsh-len} )
              ef.nsh-wid:screen-value = string({sys/inc/k16.i xef.nsh-wid} )
              ef.trim-l:screen-value = string({sys/inc/k16.i xef.trim-l} )
              ef.trim-w:screen-value = string({sys/inc/k16.i xef.trim-w} )
              ef.n-out:screen-value = string(xef.n-out)
              ef.n-out-l:screen-value = string(xef.n-out-l)
              ef.n-cuts:screen-value = string(xef.n-cuts)
              ef.adder[01]:SCREEN-VALUE = xef.adder[01]
              ef.adder[02]:SCREEN-VALUE = xef.adder[02]
              ef.adder[03]:SCREEN-VALUE = xef.adder[03]
              ef.adder[04]:SCREEN-VALUE = xef.adder[04]
              ef.adder[05]:SCREEN-VALUE = xef.adder[05]
              ef.adder[06]:SCREEN-VALUE = xef.adder[06]
              ef.adder[07]:SCREEN-VALUE = xef.adder[07]
              ef.adder[08]:SCREEN-VALUE = xef.adder[08]
              ef.adder[09]:SCREEN-VALUE = xef.adder[09]
              ef.adder[10]:SCREEN-VALUE = xef.adder[10]
              ef.adder[11]:SCREEN-VALUE = xef.adder[11]
              ef.adder[12]:SCREEN-VALUE = xef.adder[12]
              eb.num-wid:screen-value = string(xeb.num-wid)
              eb.num-len:screen-value = string(xeb.num-len)
              eb.num-up:screen-value = string(xeb.num-up)
              ef.die-in:screen-value = string(xef.die-in)
              .
       RUN roll-display.

       if lv-is-foam then       
          assign ef.gsh-dep:screen-value = string({sys/inc/k16.i xef.gsh-dep} )
                 ef.nsh-dep:screen-value = string({sys/inc/k16.i xef.nsh-dep} )
                 ef.trim-d:screen-value = string({sys/inc/k16.i xef.trim-d} )
                 ef.n-out-d:screen-value = string(xef.n-out-d)
                 eb.num-dep:screen-value = string(xeb.num-dep)
                 .
    end. /* ll-auto-calc-selected */
{&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.m-code V-table-Win
ON VALUE-CHANGED OF ef.m-code IN FRAME Corr /* Machine */
DO:
  RUN new-m-code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.n-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out V-table-Win
ON ENTRY OF ef.n-out IN FRAME Corr /* #Out */
DO:
  lv-n-out = DECIMAL(EF.n-out:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out V-table-Win
ON LEAVE OF ef.n-out IN FRAME Corr /* #Out */
DO:
  IF LASTKEY NE -1 THEN DO:
{&methods/lValidateError.i YES}
    if avail item and item.i-code = "R" /*AND ll-auto-calc-selected*/ then do:
       if dec(self:screen-value) > ef.n-out AND dec(self:screen-value) > lv-n-out then do:
          message "Cannot be greater than what was calculated." view-as alert-box error.
          return no-apply.
       end.
    end.
/*       
    if ef.m-code:screen-value in frame {&frame-name} <> "" then do:
       find first mach where mach.company = gcompany and
                             mach.loc = ef.loc and
                             mach.m-code = ef.m-code:screen-value in frame {&frame-name}
                             no-lock no-error.
       IF AVAIL mach AND mach.num-wid NE 0 AND DEC(SELF:SCREEN-VALUE) GT mach.num-wid THEN DO:
         MESSAGE "#Out on width may not be greater than " +
                 TRIM(STRING(mach.num-wid,SELF:FORMAT)) + "..."
             VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
       END.
    end.

    if ll-auto-calc-selected and self:modified then do:
       ll-num-out-changed  = yes.

       def var ld-gsh-wid as dec no-undo.
       def var ld-gsh-len as dec no-undo.
       def var ld-gsh-dep as dec no-undo.

       find first item where item.company = gcompany and
                             item.i-no = ef.board:screen-value
                             no-lock no-error.

       assign ld-gsh-wid = if not avail item or item.i-code eq "E" then ((input ef.n-out   * ef.nsh-wid) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-trimw) else 0)
                        else ef.gsh-wid
              ld-gsh-len = if not avail item or item.i-code eq "E" then
                       ((input ef.n-out-l * ef.nsh-len) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-triml) else 0)
                     else ef.gsh-len
              ld-gsh-dep = if not avail item or item.i-code eq "E" then
                       (input ef.n-out-d * ef.nsh-dep)
                     else ef.gsh-dep.
     assign ef.gsh-len:screen-value = string({sys/inc/k16.i ld-gsh-len    } )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i ld-gsh-wid   } )
              ef.gsh-dep:screen-value = string({sys/inc/k16.i ld-gsh-dep } )
              .
    end.*/
    /*if dec(ef.gsh-len:screen-value) > dec(ef.lsh-len:screen-value) or
       dec(ef.gsh-wid:screen-value) > dec(ef.lsh-wid:screen-value) 
    then do:
       message "Invalid Number Out. " view-as alert-box.
       return no-apply.
    end.*/
{&methods/lValidateError.i NO}
  END.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out V-table-Win
ON VALUE-CHANGED OF ef.n-out IN FRAME Corr /* #Out */
DO:
  RUN n-out-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.n-out-d
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out-d V-table-Win
ON ENTRY OF ef.n-out-d IN FRAME Corr /* Number Out/Depth */
DO:
  lv-n-out-d = DECIMAL(EF.n-out-d:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out-d V-table-Win
ON LEAVE OF ef.n-out-d IN FRAME Corr /* Number Out/Depth */
DO:
  IF LASTKEY NE -1 THEN DO:
 {&methods/lValidateError.i YES}
    if avail item and item.i-code = "R" /*AND ll-auto-calc-selected*/ then do:
       if dec(self:screen-value) > ef.n-out-d AND dec(self:screen-value) > lv-n-out-d then do:
          message "Cannot be greater than what was calculated." view-as alert-box error.
          return no-apply.
       end.
    end.
{&methods/lValidateError.i NO}
/*
    if ll-auto-calc-selected and self:modified then do:
       ll-num-out-changed  = yes.

       def var ld-gsh-wid as dec no-undo.
       def var ld-gsh-len as dec no-undo.
       def var ld-gsh-dep as dec no-undo.

       if ef.m-code:screen-value in frame {&frame-name} <> "" then 
          find first mach where mach.company = gcompany and
                            mach.loc = ef.loc and
                            mach.m-code = ef.m-code:screen-value in frame {&frame-name}
                            no-lock no-error.

       find first item where item.company = gcompany and
                             item.i-no = ef.board:screen-value
                             no-lock no-error.

       assign ld-gsh-wid = if not avail item or item.i-code eq "E" then ((input ef.n-out   * ef.nsh-wid) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-trimw) else 0)
                        else ef.gsh-wid
              ld-gsh-len = if not avail item or item.i-code eq "E" then
                       ((input ef.n-out-l * ef.nsh-len) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-triml) else 0)
                     else ef.gsh-len
              ld-gsh-dep = if not avail item or item.i-code eq "E" then
                       (input ef.n-out-d * ef.nsh-dep)
                     else ef.gsh-dep.
       assign ef.gsh-len:screen-value = string({sys/inc/k16.i ld-gsh-len    } )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i ld-gsh-wid   } )
              ef.gsh-dep:screen-value = string({sys/inc/k16.i ld-gsh-dep } )
              .

    end.*/
    /*if dec(ef.gsh-len:screen-value) > dec(ef.lsh-len:screen-value) or
       dec(ef.gsh-wid:screen-value) > dec(ef.lsh-wid:screen-value) 
    then do:
       message "Invalid Number Out. " view-as alert-box.
       return no-apply.
    end.*/
  END.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out-d V-table-Win
ON VALUE-CHANGED OF ef.n-out-d IN FRAME Corr /* Number Out/Depth */
DO:
  RUN n-out-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.n-out-l
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out-l V-table-Win
ON ENTRY OF ef.n-out-l IN FRAME Corr /* # out on Length */
DO:
  lv-n-out-l = DECIMAL(EF.n-out-l:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out-l V-table-Win
ON LEAVE OF ef.n-out-l IN FRAME Corr /* # out on Length */
DO:
  IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}
    if avail item and item.i-code = "R" /*AND ll-auto-calc-selected*/ then do:
         if dec(self:screen-value) > ef.n-out-l AND dec(self:screen-value) > lv-n-out-l then do:
            message "Cannot be greater than what was calculated." view-as alert-box error.
            return no-apply.
         end.
    end.
 {&methods/lValidateError.i NO}
/*       
    if ef.m-code:screen-value in frame {&frame-name} <> "" then do:
       find first mach where mach.company = gcompany and
                             mach.loc = ef.loc and
                             mach.m-code = ef.m-code:screen-value in frame {&frame-name}
                             no-lock no-error.
       IF AVAIL mach AND mach.num-len NE 0 AND DEC(SELF:SCREEN-VALUE) GT mach.num-len THEN DO:
         MESSAGE "#Out on length may not be greater than " +
                 TRIM(STRING(mach.num-len,SELF:FORMAT)) + "..."
             VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
       END.
    end.

    if ll-auto-calc-selected and self:modified then do:
       ll-num-out-changed  = yes.

       def var ld-gsh-wid as dec no-undo.
       def var ld-gsh-len as dec no-undo.
       def var ld-gsh-dep as dec no-undo.

       find first item where item.company = gcompany and
                             item.i-no = ef.board:screen-value
                             no-lock no-error.

       assign ld-gsh-wid = if not avail item or item.i-code eq "E" then ((input ef.n-out   * ef.nsh-wid) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-trimw) else 0)
                        else ef.gsh-wid
              ld-gsh-len = if not avail item or item.i-code eq "E" then
                       ((input ef.n-out-l * ef.nsh-len) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-triml) else 0)
                     else ef.gsh-len
              ld-gsh-dep = if not avail item or item.i-code eq "E" then
                       (input ef.n-out-d * ef.nsh-dep)
                     else ef.gsh-dep.
       assign ef.gsh-len:screen-value = string({sys/inc/k16.i ld-gsh-len    } )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i ld-gsh-wid   } )
              ef.gsh-dep:screen-value = string({sys/inc/k16.i ld-gsh-dep } )
              .

    end.*/
    /*if dec(ef.gsh-len:screen-value) > dec(ef.lsh-len:screen-value) or
       dec(ef.gsh-wid:screen-value) > dec(ef.lsh-wid:screen-value) 
    then do:
       message "Invalid Number Out. " view-as alert-box.
       return no-apply.
    end.*/
  END.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.n-out-l V-table-Win
ON VALUE-CHANGED OF ef.n-out-l IN FRAME Corr /* # out on Length */
DO:
  RUN n-out-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.nsh-dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.nsh-dep V-table-Win
ON ENTRY OF ef.nsh-dep IN FRAME Corr /* Depth */
DO:
  IF lv-is-foam THEN DO:
/*     APPLY "tab" TO {&self-name}.  */
/*     RETURN NO-APPLY.              */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.nsh-dep V-table-Win
ON LEAVE OF ef.nsh-dep IN FRAME Corr /* Depth */
DO:
if lastkey = -1 then return.

{&methods/lValidateError.i YES}
   IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   if lastkey <> -1 and dec(self:screen-value) < dec(ef.trim-d:screen-value) 
       /*AND ll-auto-calc-selected*/
   then do:
      message "Net Sheet Size can not be less than Die Size." view-as alert-box error.
      return no-apply.     
   end.
{&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.nsh-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.nsh-len V-table-Win
ON ENTRY OF ef.nsh-len IN FRAME Corr /* Length */
DO:
  IF lv-is-foam THEN DO:
/*     APPLY "tab" TO {&self-name}.  */
/*     RETURN NO-APPLY.              */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.nsh-len V-table-Win
ON LEAVE OF ef.nsh-len IN FRAME Corr /* Length */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
   IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.

   if lastkey <> -1 and dec(self:screen-value) < dec(ef.trim-l:screen-value) 
       /*AND ll-auto-calc-selected*/
   then do:
      message "Net Sheet Size can not be less than Die Size." view-as alert-box error.
      return no-apply.     
   end.
{&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.nsh-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.nsh-wid V-table-Win
ON ENTRY OF ef.nsh-wid IN FRAME Corr /* Net Sheet */
DO:
/*   IF lv-is-foam THEN DO:                 */
/*     RUN get-ef-nsh.                      */
/* /*     APPLY "tab" TO {&self-name}.  */  */
/* /*     RETURN NO-APPLY.              */  */
/*   END.                                   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.nsh-wid V-table-Win
ON LEAVE OF ef.nsh-wid IN FRAME Corr /* Net Sheet */
DO:
if lastkey = -1 then return.

{&methods/lValidateError.i YES}
   IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   if lastkey <> -1 and dec(self:screen-value) < dec(ef.trim-w:screen-value)
       AND ll-auto-calc-selected
   then do:
      message "Net Sheet Size can not be less than Die Size." view-as alert-box error.
      return no-apply.     
   end.
{&methods/lValidateError.i NO}


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-dep V-table-Win
ON VALUE-CHANGED OF eb.num-dep IN FRAME Corr /* # on Depth */
DO:
  RUN num-wid-len-dep-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-len V-table-Win
ON LEAVE OF eb.num-len IN FRAME Corr /* # On */
DO:
  /* run cec/u2k3.p.   */
  if self:modified and ll-auto-calc-selected then do:
     ll-num-lw-changed = yes.
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).
     xeb.num-len = int(self:screen-value).
     run cec/calc-dim1.p no-error.
     if error-status:error then do:

     end.
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).

     assign ef.lsh-len:screen-value = string({sys/inc/k16.i xef.lsh-len} )
              ef.lsh-wid:screen-value = string({sys/inc/k16.i xef.lsh-wid} )
              ef.gsh-len:screen-value = string({sys/inc/k16.i xef.gsh-len} )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i xef.gsh-wid} )
              ef.nsh-len:screen-value = string({sys/inc/k16.i xef.nsh-len} )
              ef.nsh-wid:screen-value = string({sys/inc/k16.i xef.nsh-wid} )
              ef.trim-l:screen-value = string({sys/inc/k16.i xef.trim-l} )
              ef.trim-w:screen-value = string({sys/inc/k16.i xef.trim-w} )
              ef.n-out:screen-value = string(xef.n-out)
              ef.n-out-l:screen-value = string(xef.n-out-l)
              ef.n-cuts:screen-value = string(xef.n-cuts)
              eb.num-wid:screen-value = string(xeb.num-wid)
              eb.num-len:screen-value = string(xeb.num-len)
              eb.num-up:screen-value = string(xeb.num-up)
              ef.die-in:screen-value = string(xef.die-in)
              .

  end.
  else if self:modified then do:
      assign eb.num-up:screen-value = string(
                                 int(eb.num-len:screen-value) * int(eb.num-wid:screen-value)
                               )
/*              ef.die-in:screen-value = string(                                    */
/*                                      ( dec(ef.die-in:screen-value) / eb.num-up ) */
/*                                      * int(eb.num-up:screen-value)               */
/*                                      )                                           */
             .                        
  end.                                     


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-len V-table-Win
ON VALUE-CHANGED OF eb.num-len IN FRAME Corr /* # On */
DO:
  RUN num-wid-len-dep-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-wid V-table-Win
ON LEAVE OF eb.num-wid IN FRAME Corr /* # on Width */
DO:
  /* run cec/u2k3.p.   */
  if self:modified and ll-auto-calc-selected then do:   
     ll-num-lw-changed = yes.
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).
     xeb.num-wid = int(self:screen-value).
     run cec/calc-dim1.p no-error.
     if error-status:error then do:

     end.
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).
     assign ef.lsh-len:screen-value = string({sys/inc/k16.i xef.lsh-len} )
              ef.lsh-wid:screen-value = string({sys/inc/k16.i xef.lsh-wid} )
              ef.gsh-len:screen-value = string({sys/inc/k16.i xef.gsh-len} )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i xef.gsh-wid} )
              ef.nsh-len:screen-value = string({sys/inc/k16.i xef.nsh-len} )
              ef.nsh-wid:screen-value = string({sys/inc/k16.i xef.nsh-wid} )
              ef.trim-l:screen-value = string({sys/inc/k16.i xef.trim-l} )
              ef.trim-w:screen-value = string({sys/inc/k16.i xef.trim-w} )
              ef.n-out:screen-value = string(xef.n-out)
              ef.n-out-l:screen-value = string(xef.n-out-l)
              ef.n-cuts:screen-value = string(xef.n-cuts)
              eb.num-wid:screen-value = string(xeb.num-wid)
              eb.num-len:screen-value = string(xeb.num-len)
              eb.num-up:screen-value = string(xeb.num-up)
              ef.die-in:screen-value = string(xef.die-in)
              .

  end.
  else if self:modified then do:
      assign eb.num-up:screen-value = string(
                                 int(eb.num-len:screen-value) * int(eb.num-wid:screen-value)
                               )
/*              ef.die-in:screen-value = string(                                    */
/*                                      ( dec(ef.die-in:screen-value) / eb.num-up ) */
/*                                      * int(eb.num-up:screen-value)               */
/*                                      )                                           */
             .                        
  end.                                     


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-wid V-table-Win
ON VALUE-CHANGED OF eb.num-wid IN FRAME Corr /* # on Width */
DO:
  RUN num-wid-len-dep-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.roll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.roll V-table-Win
ON VALUE-CHANGED OF ef.roll IN FRAME Corr /* Roll */
DO:
  FIND FIRST mach
      WHERE mach.company EQ gcompany
        AND mach.loc     EQ eb.loc
        AND mach.m-code  EQ ef.m-code:SCREEN-VALUE
      USE-INDEX m-code NO-LOCK NO-ERROR.
  IF AVAIL mach and mach.p-type EQ "R" THEN {&self-name}:SCREEN-VALUE = "Y".

  RUN roll-display.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.spare-int-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.spare-int-1 V-table-Win
ON VALUE-CHANGED OF ef.spare-int-1 IN FRAME Corr /* spare-int-1 */
DO:
  FIND xef WHERE ROWID(xef) = ROWID(ef) EXCLUSIVE-LOCK.
  xef.spare-int-1 = INTEGER(ef.spare-int-1:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.t-dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-dep V-table-Win
ON LEAVE OF eb.t-dep IN FRAME Corr /* Blank Depth */
DO:
  IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.t-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-len V-table-Win
ON LEAVE OF eb.t-len IN FRAME Corr /* Blank Length */
DO:
  IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
    {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.t-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-wid V-table-Win
ON LEAVE OF eb.t-wid IN FRAME Corr /* Blank */
DO:
 IF lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   {&methods/lValidateError.i NO}
   end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.trim-d
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.trim-d V-table-Win
ON LEAVE OF ef.trim-d IN FRAME Corr /* Trim Depth */
DO:
/*   IF LASTKEY NE -1 THEN DO:                     */
/*     RUN valid-trim-d NO-ERROR.                  */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*   END.                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.trim-l
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.trim-l V-table-Win
ON LEAVE OF ef.trim-l IN FRAME Corr /* Trim Length */
DO:
/*   IF LASTKEY NE -1 THEN DO:                     */
/*     RUN valid-trim-l NO-ERROR.                  */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*   END.                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.trim-w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.trim-w V-table-Win
ON LEAVE OF ef.trim-w IN FRAME Corr /* Die Size */
DO:
/*   IF LASTKEY NE -1 THEN DO:                     */
/*     RUN valid-trim-w NO-ERROR.                  */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*   END.                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.xgrain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.xgrain V-table-Win
ON return OF ef.xgrain IN FRAME Corr /* Rev. Corr */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.xgrain V-table-Win
ON VALUE-CHANGED OF ef.xgrain IN FRAME Corr /* Rev. Corr */
DO:
  /* if self:screen-value = "B" then
              assign ef.gsh-wid:screen-value = if ef.lsh-wid:screen-value = "0" then eb.t-len:Screen-value else ef.gsh-wid:screen-value              
                     ef.gsh-len:screen-value = if ef.lsh-len:screen-value = "0" then eb.t-wid:screen-value else ef.gsh-len:screen-value.
   else  assign ef.gsh-wid:screen-value = if ef.lsh-wid:screen-value = "0" then eb.t-wid:Screen-value else ef.gsh-wid:screen-value
                ef.gsh-len:screen-value = if ef.lsh-len:screen-value = "0" then eb.t-len:screen-value else ef.gsh-len:screen-value.

   run recalc-dim.                
 */

  IF ll-one-eb-on-ef OR ll-part-style THEN DO:
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).
     assign
      xef.xgrain = ef.xgrain:screen-value in frame {&frame-name}
      xef.roll   = ef.roll:screen-value in frame {&frame-name} EQ "Y".
     run cec/calc-dim.p .
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).
     assign ef.lsh-len:screen-value = string({sys/inc/k16.i xef.lsh-len} )
              ef.lsh-wid:screen-value = string({sys/inc/k16.i xef.lsh-wid} )
              ef.gsh-len:screen-value = string({sys/inc/k16.i xef.gsh-len} )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i xef.gsh-wid} )
              ef.nsh-len:screen-value = string({sys/inc/k16.i xef.nsh-len} )
              ef.nsh-wid:screen-value = string({sys/inc/k16.i xef.nsh-wid} )
              ef.trim-l:screen-value = string({sys/inc/k16.i xef.trim-l} )
              ef.trim-w:screen-value = string({sys/inc/k16.i xef.trim-w} )
              ef.n-out:screen-value = string(xef.n-out)
              ef.n-out-l:screen-value = string(xef.n-out-l)
              ef.n-cuts:screen-value = string(xef.n-cuts)
              ef.adder[01]:SCREEN-VALUE = xef.adder[01]
              ef.adder[02]:SCREEN-VALUE = xef.adder[02]
              ef.adder[03]:SCREEN-VALUE = xef.adder[03]
              ef.adder[04]:SCREEN-VALUE = xef.adder[04]
              ef.adder[05]:SCREEN-VALUE = xef.adder[05]
              ef.adder[06]:SCREEN-VALUE = xef.adder[06]
              ef.adder[07]:SCREEN-VALUE = xef.adder[07]
              ef.adder[08]:SCREEN-VALUE = xef.adder[08]
              ef.adder[09]:SCREEN-VALUE = xef.adder[09]
              ef.adder[10]:SCREEN-VALUE = xef.adder[10]
              ef.adder[11]:SCREEN-VALUE = xef.adder[11]
              ef.adder[12]:SCREEN-VALUE = xef.adder[12]
              eb.num-wid:screen-value = string(xeb.num-wid)
              eb.num-len:screen-value = string(xeb.num-len)
              eb.num-up:screen-value = string(xeb.num-up)
              .

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

SESSION:DATA-ENTRY-RETURN = YES.
ef.spare-int-1:SENSITIVE = NO.

ASSIGN
 lv-frame-hdl = FRAME {&frame-name}:HANDLE
 lv-group-hdl = lv-frame-hdl:FIRST-CHILD.

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

   ll-auto-calc-selected = yes.

   find first item where item.company = gcompany and
                         item.i-no = ef.board
                         no-lock no-error.

   if not lv-is-foam or item.i-code = "E" then do:
      find first mach where mach.company = gcompany and
                            mach.loc = eb.loc and
                            mach.m-code = ef.m-code
                            use-index m-code no-lock no-error.
        if avail mach and mach.dept[1] eq "RC" then
           assign ef.nsh-wid:screen-value in frame {&frame-name} = string(ef.nsh-wid - (2 * mach.min-trimw))
                  ef.nsh-len:screen-value = string(ef.nsh-len - (2 * mach.min-triml)).   
        assign ef.n-out:screen-value   = string(trunc(ef.lsh-len / ef.nsh-wid,0))
               ef.n-out-l:screen-value = string(trunc(ef.lsh-wid / ef.nsh-len,0))
               ef.n-out-d:screen-value = string("1").
   end.

   assign ef.roll:SCREEN-VALUE    = STRING(ITEM.r-wid GT 0,"Y/N")
          ef.n-out:screen-value   = string("0")
          ef.n-out-l:screen-value = string("0")
          ef.n-out-d:screen-value = string("0")
          ef.gsh-len:screen-value = string("0")
          ef.gsh-wid:screen-value = string("0")
          ef.gsh-dep:screen-value = string("0")
          ef.nsh-len:screen-value = string("0")
          ef.nsh-wid:screen-value = string("0")
          ef.nsh-dep:screen-value = string("0")
          ef.trim-w:screen-value  = string("0")
          ef.trim-l:screen-value  = string("0")
          ef.trim-d:screen-value  = string("0")
          eb.num-len:screen-value = string("0")
          eb.num-wid:screen-value = string("0")
          eb.num-dep:screen-value = string("0").

   run dispatch ('enable-fields').
   disable ef.gsh-wid ef.gsh-len ef.gsh-dep
           ef.nsh-wid ef.nsh-len ef.nsh-dep
           ef.trim-w ef.trim-l ef.trim-d
           with frame {&frame-name}.
   enable ef.m-code ef.lsh-wid ef.lsh-len ef.xgrain  with frame {&frame-name}.

   IF NOT ll-auto-calc-selected THEN
     ef.spare-int-1:SENSITIVE IN frame {&frame-name} = TRUE.
   ELSE DO:
     /* On auto-calc, blank out override value to avoid confusion */
     FIND xef WHERE ROWID(xef) = ROWID(ef) NO-LOCK NO-ERROR.
     IF AVAIL xef THEN
       IF xef.spare-int-1 NE 0 THEN
         ASSIGN
           ef.spare-int-1:SCREEN-VALUE = "".

   END.
   apply "entry" to ef.m-code in frame {&frame-name} .

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
  if xef.n-out-d = 0 then xef.n-out-d = 1.
  xef.n-cuts = (xef.n-out - 1) + (xef.n-out-l - 1) + (xef.n-out-d - 1).
  if xef.n-cuts lt 0 then xef.n-cuts = 0.

  assign llen = xef.gsh-len / xef.n-out
         lwid = xef.gsh-wid / xef.n-out-l.

  if xef.lam-dscr eq "R" or (xef.lam-dscr ne "R" and xef.xgrain eq "S") then
     assign  zzz  = llen
             llen = lwid
             lwid = zzz.

   /*   run cec/u2kinc1.p.
      run cec/u2kinc2.p.
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
            assign  xef.nsh-wid = xef.nsh-wid - (2 * mach.min-trimw)
                    xef.nsh-len = xef.nsh-len - (2 * mach.min-triml).


     assign xef.n-out   = trunc(xef.lsh-wid / xef.nsh-wid,0)
            xef.n-out-l = trunc(xef.lsh-len / xef.nsh-len,0)
            xef.n-out-d = 1
            xef.nsh-dep = xeb.t-dep
            xef.trim-d  = xeb.t-dep.

     assign xeb.num-dep = 1
            xef.gsh-wid = if not avail item or item.i-code eq "E" then
                               ((xef.n-out   * xef.nsh-wid) +
                          if avail mach and mach.dept[1] eq "RC" then
                                 (2 * mach.min-trimw) else 0)
                          else xef.gsh-wid
            xef.gsh-len = if not avail item or item.i-code eq "E" then
                                 ( (xef.n-out-l * xef.nsh-len) +
                                   if avail mach and mach.dept[1] eq "RC" then
                                   (2 * mach.min-triml) else 0 )
                              else xef.gsh-len
            xef.gsh-dep = if not avail item or item.i-code eq "E" then
                                (xef.n-out-d * xef.nsh-dep)
                              else xef.gsh-dep.

     if xef.n-out-d eq ? then xef.n-out-d = 0.
     if xef.gsh-dep eq ? then xef.gsh-dep = 0.
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
  HIDE FRAME Corr.
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

  if not avail ef then return.


  find first item where item.company = gcompany and
                        item.i-no = ef.board 
                        no-lock no-error.
  if avail item and item.i-code <> "E" then 
     disable ef.gsh-wid ef.gsh-len ef.gsh-dep with frame {&frame-name}.

  if not ll-auto-calc-selected then disable ef.lsh-wid ef.lsh-len with frame {&frame-name}.

  IF ll-one-eb-on-ef THEN ENABLE eb.num-len eb.num-wid /*eb.num-dep*/ WITH FRAME {&FRAME-NAME}.

  find first style where style.company = gcompany and
                          style.style = eb.style
                          no-lock no-error.
  if NOT AVAIL style OR style.type <> "F" then 
     disable ef.gsh-dep ef.nsh-dep ef.trim-d /*eb.num-dep*/ ef.n-out-d
            with frame {&frame-name}.
  if NOT AVAIL style OR style.type EQ "R" then 
     disable ef.roll with frame {&frame-name}.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-ef-nsh V-table-Win 
PROCEDURE get-ef-nsh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       sys/inc/k16.i:
               Formula to display fractions as 16ths of an inch 
------------------------------------------------------------------------------*/
  DEFINE VARIABLE v-n-out-l LIKE ef-nsh.n-out-l NO-UNDO INIT 0.
  DEFINE VARIABLE v-n-out-w LIKE ef-nsh.n-out-w NO-UNDO INIT 0.
  DEFINE VARIABLE v-n-out-d LIKE ef-nsh.n-out-d NO-UNDO INIT 0.
  DEFINE VARIABLE lUpdateNetSheet AS LOGICAL INIT TRUE NO-UNDO.
  DEFINE VARIABLE lResetEfNsh AS LOGICAL INIT TRUE NO-UNDO.
  DEFINE VARIABLE dNewWid AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dNewLen AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dNewDep AS DECIMAL NO-UNDO.

  DEFINE BUFFER bf-ef-nsh FOR ef-nsh.

 DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST ef-nsh OF ef NO-LOCK 
        WHERE ef-nsh.pass-no EQ 1 
        NO-ERROR.
    IF AVAILABLE ef-nsh 
        AND  (TRIM(ef.gsh-wid:SCREEN-VALUE) NE TRIM(STRING({sys/inc/k16.i ef-nsh.wid-in},">>>>9.99"))
              OR TRIM(ef.gsh-len:SCREEN-VALUE) NE TRIM(STRING({sys/inc/k16.i ef-nsh.len-in},">>>>9.99"))
              OR TRIM(ef.gsh-dep:SCREEN-VALUE) NE TRIM(STRING({sys/inc/k16.i ef-nsh.dep-in},">>>>9.99"))) THEN DO:

        MESSAGE "Gross Sheet has changed. Reset Net Sheet Passes?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lResetEfNsh.
        IF lResetEfNsh THEN DO: 
            FOR EACH ef-nsh OF ef EXCLUSIVE-LOCK:
                IF ef-nsh.pass-no EQ 1 THEN DO: 
                    ASSIGN 
                        dNewWid = DEC(ef.gsh-wid:SCREEN-VALUE)
                        dNewLen = DEC(ef.gsh-len:SCREEN-VALUE)
                        dNewDep = DEC(ef.gsh-dep:SCREEN-VALUE)
                        .
                    {sys/inc/k16bb.i dNewWid}
                    {sys/inc/k16bb.i dNewLen}
                    {sys/inc/k16bb.i dNewDep}

                    ASSIGN 
                        ef-nsh.wid-in = dNewWid
                        ef-nsh.len-in = dNewLen
                        ef-nsh.dep-in = dNewDep
                        ef-nsh.wid-out = dNewWid
                        ef-nsh.len-out = dNewLen
                        ef-nsh.dep-out = dNewDep
                        ef-nsh.n-out-w = 1
                        ef-nsh.n-out-l = 1
                        ef-nsh.n-out-d = 1
                        . 

                END.
                ELSE 
                    DELETE ef-nsh.
            END.
        END.
    END.
    RUN est/d-ef-nsh.w (ROWID(ef)) NO-ERROR.

    /* Find first pass and transfer IN values to screen gross sheet values */
    FIND FIRST ef-nsh OF ef WHERE ef-nsh.pass-no EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL ef-nsh THEN
      ASSIGN
       ef.gsh-wid:SCREEN-VALUE = STRING({sys/inc/k16.i ef-nsh.wid-in})
       ef.gsh-len:SCREEN-VALUE = STRING({sys/inc/k16.i ef-nsh.len-in})
       ef.gsh-dep:SCREEN-VALUE = STRING({sys/inc/k16.i ef-nsh.dep-in}).
/*        ef.nsh-wid:SCREEN-VALUE = STRING({sys/inc/k16.i ef-nsh.wid-out})   */
/*        ef.nsh-len:SCREEN-VALUE = STRING({sys/inc/k16.i ef-nsh.len-out})   */
/*        ef.nsh-dep:SCREEN-VALUE = STRING({sys/inc/k16.i ef-nsh.dep-out}).  */


    /* Find last pass and transfer OUT values to screen net sheet values. */
    FOR EACH ef-nsh OF ef NO-LOCK BREAK BY ef-nsh.pass-no:

        /* If last pass, then prompt user if they wish to update the net sheet values. */
        IF LAST(ef-nsh.pass-no) THEN DO:
            MESSAGE "Update net sheet values from pass number " ef-nsh.pass-no
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lUpdateNetSheet.
            IF lUpdateNetSheet = YES THEN
                ASSIGN ef.nsh-wid:SCREEN-VALUE = STRING({sys/inc/k16.i ef-nsh.wid-out})
                       ef.nsh-len:SCREEN-VALUE = STRING({sys/inc/k16.i ef-nsh.len-out})
                       ef.nsh-dep:SCREEN-VALUE = STRING({sys/inc/k16.i ef-nsh.dep-out}).
        END. /* IF LAST(ef-nsh.pass-no) */
    END. /* FOR EACH ef-nsh OF ef  */

    /* Multiply all WLD numbers and update the screen Width, Length, Depth values. */
    FOR EACH ef-nsh OF ef NO-LOCK BY ef-nsh.pass-no:
        IF ef-nsh.pass-no = 1 THEN
            ASSIGN v-n-out-l = ef-nsh.n-out-l
                   v-n-out-w = ef-nsh.n-out-w
                   v-n-out-d = ef-nsh.n-out-d.
        ELSE
        ASSIGN v-n-out-l = (v-n-out-l * ef-nsh.n-out-l)
               v-n-out-w = (v-n-out-w * ef-nsh.n-out-w)
               v-n-out-d = (v-n-out-d * ef-nsh.n-out-d).
    END. /* FOR EACH ef-nsh OF ef */

    /* Set num out values. */
    ASSIGN ef.n-out:SCREEN-VALUE   = STRING(v-n-out-w)
           ef.n-out-l:SCREEN-VALUE = STRING(v-n-out-l)
           ef.n-out-d:SCREEN-VALUE = STRING(v-n-out-d).

  END. /* DO WITH FRAME {&FRAME-NAME} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-foam as log no-undo.
  DEF VAR lv-num-up LIKE eb.num-up NO-UNDO.
  DEF VAR ll-auto-calc AS LOG NO-UNDO. 
  DEF VAR lv-hld-board LIKE ef.board NO-UNDO.
  DEF VAR lv-hld-cust like eb.cust-no no-undo.
  DEF VAR lv-hld-ship like eb.ship-id no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN lv-num-up = eb.num-up
         lv-hld-board = ef.board
         lv-hld-cust  = eb.cust-no
         lv-hld-ship  = eb.ship-id.

  IF ll-auto-calc-selected THEN DO:
    ef.spare-int-1:SENSITIVE IN frame {&frame-name} = TRUE.
    ef.spare-int-1:SCREEN-VALUE = "0".
  END.

  /* Dispatch standard ADM method.         */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  IF lv-hld-board NE ef.board THEN DO:
     {ce/uship-id.i no}
  END.
  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST bf-est
      WHERE bf-est.company EQ ef.company
        AND bf-est.est-no  EQ ef.est-no
      NO-LOCK NO-ERROR.

  IF bf-est.est-type EQ 8 AND NOT lv-is-foam THEN eb.num-up = lv-num-up.

      {sys/inc/k16bb.i ef.lsh-len }
      {sys/inc/k16bb.i ef.lsh-wid }
      {sys/inc/k16bb.i ef.gsh-len }
      {sys/inc/k16bb.i ef.gsh-wid }
      {sys/inc/k16bb.i ef.gsh-dep }
      {sys/inc/k16bb.i ef.nsh-len }
      {sys/inc/k16bb.i ef.nsh-wid }
      {sys/inc/k16bb.i ef.nsh-dep }
      {sys/inc/k16bb.i ef.trim-l }
      {sys/inc/k16bb.i ef.trim-w }
      {sys/inc/k16bb.i ef.trim-d }

      {sys/inc/k16bb.i ef.leaf-w[1]}
      {sys/inc/k16bb.i ef.leaf-w[2]}
      {sys/inc/k16bb.i ef.leaf-w[3]}
      {sys/inc/k16bb.i ef.leaf-w[4]}
      {sys/inc/k16bb.i ef.leaf-l[1]}
      {sys/inc/k16bb.i ef.leaf-l[2]}
      {sys/inc/k16bb.i ef.leaf-l[3]}
      {sys/inc/k16bb.i ef.leaf-l[4]}
   find style where style.company = gcompany and
                    style.style = eb.style
                    no-lock no-error.   
   lv-foam = if avail style and style.type = "F" then yes else no.
   find first item where item.company = gcompany and
                         item.i-no = ef.board
                         no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.

   /*  need to run cec/u2k.p 
      if not lv-foam or item.i-code = "E" then run cec/u2k.p . 
      run cec/u2k3.p .   
   */  

ll-auto-calc = ll-auto-calc-selected. 

if ll-auto-calc-selected then do:
   if ll-num-out-changed then
      ASSIGN
         ll-auto-calc-selected = NO
         ll-num-out-changed = no.

   else do:
       if ef.m-code:screen-value in frame {&frame-name} <> "" then 
          find first mach where mach.company = gcompany and
                            mach.loc = ef.loc and
                            mach.m-code = ef.m-code:screen-value in frame {&frame-name}
                            no-lock no-error.

       find first item where item.company = gcompany and
                             item.i-no = ef.board:screen-value
                             no-lock no-error.

       IF ef.xgrain:SCREEN-VALUE <> "S" THEN DO:
          assign ef.gsh-wid = if not avail item or item.i-code eq "E" then ((input ef.n-out   * ef.nsh-wid) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-trimw) else 0)
                        else ef.gsh-wid
              ef.gsh-len = if not avail item or item.i-code eq "E" then
                       ((input ef.n-out-l * ef.nsh-len) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-triml) else 0)
                     else ef.gsh-len
              ef.gsh-dep = if not avail item or item.i-code eq "E" then
                       (input ef.n-out-d * ef.nsh-dep)
                     else ef.gsh-dep.


          IF item.i-code EQ "E" OR NOT AVAIL ITEM THEN
             ef.roll-wid = ef.gsh-wid.

          assign ef.gsh-len:screen-value = string({sys/inc/k16.i ef.gsh-len    } )
                 ef.gsh-wid:screen-value = string({sys/inc/k16.i ef.gsh-wid   } )
                 ef.gsh-dep:screen-value = string({sys/inc/k16.i ef.gsh-dep } ).
       END.
     ll-auto-calc-selected = no.

   end.  /* not ll-num-out-changed */ 
   disable ef.m-code ef.lsh-wid ef.lsh-len ef.xgrain with frame {&frame-name}.
end.

  FIND FIRST mach WHERE
       mach.company EQ gcompany AND
       mach.m-code  EQ ef.m-code:SCREEN-VALUE
       NO-LOCK NO-ERROR.

  IF ef.roll AND ef.roll-wid GT ef.gsh-wid THEN DO:
     ef.gsh-wid = ef.roll-wid.

     IF AVAIL mach AND ef.gsh-wid LT mach.min-len THEN
        ef.gsh-wid = mach.min-len.

     IF ef.i-code EQ "E" AND ll-auto-calc THEN
        IF ef.xgrain EQ "S" THEN
           ef.nsh-len = ef.gsh-wid / ef.n-out.
        ELSE
           ef.nsh-wid = ef.gsh-wid / ef.n-out.
  END.

  ELSE
  DO:
     IF AVAIL mach AND ef.gsh-wid LT mach.min-len THEN
     DO:
        ef.gsh-wid = mach.min-len.

        IF ef.roll AND ef.i-code EQ "E" AND ll-auto-calc THEN
        DO:
           IF ef.xgrain EQ "S" THEN
              ef.nsh-len = ef.gsh-wid / ef.n-out.
           ELSE
              ef.nsh-wid = ef.gsh-wid / ef.n-out.
        END.
     END.

     ef.roll-wid = ef.gsh-wid.
  END.

  ASSIGN
     eb.flute = ef.flute
     eb.test = ef.test.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-total-up-value AS CHAR NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  v-total-up-value = ef.spare-int-1:SCREEN-VALUE  IN frame {&frame-name}.
  FIND xef WHERE ROWID(xef) = ROWID(ef) EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL xef THEN
    IF xef.spare-int-1 EQ 0 THEN
      ASSIGN
        ef.spare-int-1:SCREEN-VALUE = "".
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN eb.num-up.
    IF est.est-type NE 8 THEN 
      ASSIGN eb.num-wid eb.num-len.
  END.

   ef.spare-int-1:SCREEN-VALUE IN frame {&frame-name} = v-total-up-value.

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
  ll-is-canceled = yes.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  disable ef.m-code with frame {&frame-name}.
  ll-is-canceled = no.

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
  DEF VAR lv-tot-len AS INT NO-UNDO.
  DEF VAR lv-tot-wid AS INT NO-UNDO.
  DEF VAR lv-tot-up AS INT NO-UNDO.
  def var li-n-cuts as int no-undo.

  DEF BUFFER bf-eb FOR eb.

  IF VALID-HANDLE(br-flm) THEN DELETE WIDGET br-flm.

  {cec/msfcalc.i}

  if not avail ef then return.

  DO WITH FRAME {&FRAME-NAME}:

     IF v-cecscrn-char EQ "Decimal" THEN
        ASSIGN
           ef.gsh-wid:FORMAT = ">>>9.999999"
           ef.gsh-len:FORMAT = ">>>9.999999"
           ef.gsh-dep:FORMAT = ">>>9.999999"
           ef.nsh-wid:FORMAT = ">>>9.999999"
           ef.nsh-len:FORMAT = ">>>9.999999"
           ef.nsh-dep:FORMAT = ">>>9.999999"
           ef.trim-w:FORMAT = ">>>9.999999"
           ef.trim-l:FORMAT = ">>>9.999999"
           ef.trim-d:FORMAT = ">>>9.999999"
           eb.t-wid:FORMAT = ">>>9.999999"
           eb.t-len:FORMAT = ">>>9.999999"
           eb.t-dep:FORMAT = ">>>9.999999"
           ef.lsh-wid:FORMAT = ">>9.999999"
           ef.lsh-len:FORMAT = ">>9.999999"
           ef.leaf-w[1]:FORMAT = ">>9.999999"
           ef.leaf-w[2]:FORMAT = ">>9.999999"
           ef.leaf-w[3]:FORMAT = ">>9.999999"
           ef.leaf-w[4]:FORMAT = ">>9.999999"
           ef.leaf-l[1]:FORMAT = ">>9.999999"
           ef.leaf-l[2]:FORMAT = ">>9.999999"
           ef.leaf-l[3]:FORMAT = ">>9.999999"
           ef.leaf-l[4]:FORMAT = ">>9.999999".
  END.

  assign lv-is-foam = no
         lv-industry = "".

  btn_board:LABEL = " " + TRIM(ef.board:LABEL) + ": " /*+ TRIM(ef.board) */ .

    IF ef.board = "" THEN
            btn_board:HIDDEN  = TRUE .
    ELSE 
         btn_board:HIDDEN  = FALSE .

  find first style where style.company = gcompany and
                          style.style = eb.style
                          no-lock no-error.
  IF AVAIL style THEN
  DO:
     IF style.type = "F" then lv-is-foam = yes.
     lv-industry = style.industry.
  END.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   eb.num-len:HIDDEN IN FRAME {&FRAME-NAME} = NO
   eb.num-wid:HIDDEN IN FRAME {&FRAME-NAME} = NO.

  RUN roll-display.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN one-eb-on-ef (ROWID(ef), OUTPUT ll-one-eb-on-ef).
  RUN one-ef-on-est (ROWID(est), OUTPUT ll-one-ef-on-est).
  RUN part-style (ROWID(ef), OUTPUT ll-part-style).
  ef.spare-int-1:SENSITIVE = TRUE.
  ef.spare-int-1:SCREEN-VALUE = STRING(getTotalUp()).
  ef.spare-int-1:SENSITIVE = FALSE.
  run reset-fields.

  /* from uest2.p =======*/
  DO WITH FRAME {&FRAME-NAME}:

  find first ce-ctrl where ce-ctrl.company =gcompany and
                           ce-ctrl.loc = eb.loc
                           no-lock no-error.
  find first item where item.company = gcompany and
                        item.i-no = ef.board
                        no-lock no-error.
  if avail item then DO:
    find first e-item of item no-lock no-error.
    ef.brd-dscr:SCREEN-VALUE = ITEM.i-name.
  END.
  find first mach where mach.company = gcompany and
                        mach.loc = eb.loc and
                        mach.m-code = ef.m-code
                        use-index m-code no-lock no-error.
  if (avail mach and mach.p-type = "R") or
     (avail item and ( (item.i-code = "R" and item.r-wid ne 0) or
                       (avail e-item and e-item.roll-w[1] ne 0) )) or
     (not avail mach and ce-ctrl.avg-cscost ne 0) /*and not(xef.lsh-lock)*/ 
  then assign lv-is-roll = true. /* xef.lam-dscr = "R". */
  else assign lv-is-roll = no.
  ls-lam-dscr = "S".
  /* ======= */

  if ef.lam-dscr = "R" or
     (ef.lam-dscr <> "R" and ef.xgrain = "S" )
  then assign ef.lsh-len:screen-value in frame {&frame-name} = string(ef.gsh-len)
              ef.lsh-wid:screen-value = string(ef.gsh-wid)
              .
  else assign ef.lsh-len:screen-value = string(ef.gsh-wid)
              ef.lsh-wid:screen-value = string(ef.gsh-len)
              .

  assign ef.lsh-len:screen-value in frame {&frame-name} = string( {sys/inc/k16.i ef.lsh-len } ) 
         ef.lsh-wid:screen-value = string( {sys/inc/k16.i ef.lsh-wid } )
         ef.gsh-len:screen-value = string({sys/inc/k16.i ef.gsh-len    } )
         ef.gsh-wid:screen-value = string({sys/inc/k16.i ef.gsh-wid   } )
         ef.gsh-dep:screen-value = string({sys/inc/k16.i ef.gsh-dep } )
         ef.nsh-len:screen-value = string({sys/inc/k16.i ef.nsh-len   } )
         ef.nsh-wid:screen-value = string({sys/inc/k16.i ef.nsh-wid  } )
         ef.nsh-dep:screen-value = string({sys/inc/k16.i ef.nsh-dep  } )
         ef.trim-w:screen-value = string({sys/inc/k16.i ef.trim-w   } )
         ef.trim-l:screen-value = string({sys/inc/k16.i ef.trim-l  } )
         ef.trim-d:screen-value = string({sys/inc/k16.i ef.trim-d } )
         eb.t-len:screen-value = string({sys/inc/k16.i eb.t-len  } )
         eb.t-wid:screen-value = string({sys/inc/k16.i eb.t-wid  } )
         eb.t-dep:screen-value = string({sys/inc/k16.i eb.t-dep  } )
         ef.leaf-w[1]:screen-value = string({sys/inc/k16.i ef.leaf-w[1]  } )
         ef.leaf-w[2]:screen-value = string({sys/inc/k16.i ef.leaf-w[2]  } )
         ef.leaf-w[3]:screen-value = string({sys/inc/k16.i ef.leaf-w[3]  } )
         ef.leaf-w[4]:screen-value = string({sys/inc/k16.i ef.leaf-w[4]  } )
         ef.leaf-l[1]:screen-value = string({sys/inc/k16.i ef.leaf-l[1]  } )
         ef.leaf-l[2]:screen-value = string({sys/inc/k16.i ef.leaf-l[2]  } )
         ef.leaf-l[3]:screen-value = string({sys/inc/k16.i ef.leaf-l[3]  } )
         ef.leaf-l[4]:screen-value = string({sys/inc/k16.i ef.leaf-l[4]  } )
         eb.t-sqin:screen-value = if v-corr then string(eb.t-sqin * .007)
                                  else string(eb.t-sqin / 144) . 

  if ef.n-out = 0 then ef.n-out:screen-value = string("1").
  if ef.n-out-l = 0 then ef.n-out-l:screen-value = string("1").
  if ef.n-out-d = 0 then ef.n-out-d:screen-value = string("1").
  /*if int(ef.n-cuts:screen-value) eq 0 then do:
    li-n-cuts = (int(ef.n-out:screen-value) - 1) + (int(ef.n-out-l:screen-value) - 1) + (int(ef.n-out-d:screen-value) - 1).
    if li-n-cuts < 0 then li-n-cuts = 0.
    ef.n-cuts:screen-value = string(li-n-cuts,">>9").    
  end.*/

  END. /*DO WITH FRAME {&FRAME-NAME}:*/

  FOR EACH bf-eb FIELDS(num-len num-wid num-up)
      WHERE bf-eb.company EQ ef.company
        AND bf-eb.est-no  EQ ef.est-no
        AND bf-eb.form-no EQ ef.form-no
      NO-LOCK:
      ASSIGN
         lv-tot-len = lv-tot-len + bf-eb.num-len.
         lv-tot-wid = lv-tot-wid + bf-eb.num-wid.
         lv-tot-up  = lv-tot-up + bf-eb.num-up.
  END.
  DISPLAY /*lv-tot-len @ eb.num-len
          lv-tot-wid @ eb.num-wid */
          lv-tot-up @ eb.num-up WITH FRAME {&FRAME-NAME}.

  IF NOT ll-one-eb-on-ef THEN
    ASSIGN
     eb.num-len:HIDDEN IN FRAME {&FRAME-NAME} = YES
     eb.num-wid:HIDDEN IN FRAME {&FRAME-NAME} = YES
     eb.num-dep:HIDDEN IN FRAME {&FRAME-NAME} = YES.

  IF NOT AVAIL style OR style.type NE "R" THEN
    ef.roll:HIDDEN IN FRAME {&FRAME-NAME} = YES.

  FIND FIRST bf-est WHERE bf-est.company = ef.company
                      AND bf-est.est-no = ef.est-no NO-LOCK NO-ERROR.

  IF bf-est.est-type EQ 8 THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"container-source",OUTPUT char-hdl).

    RUN GET-ATTRIBUTE IN WIDGET-HANDLE(char-hdl) ("current-page").

    IF INT(RETURN-VALUE) EQ 4 THEN DO:
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).

      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN enable-leaf IN WIDGET-HANDLE(char-hdl) (YES, bf-est.est-type).

      CREATE BROWSE br-flm 
             ASSIGN TITLE = "Wax/Label"
             FRAME = FRAME {&FRAME-NAME}:HANDLE
             QUERY = QUERY q-flm:HANDLE
             X = ef.leaf[1]:X - 8
             Y = ef.leaf[1]:Y - 20
             WIDTH = 84

             DOWN = 5
             VISIBLE = YES 
             SENSITIVE = TRUE  
             READ-ONLY = YES
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

      br-flm:refresh() NO-ERROR.

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
    END.
  END.

  ELSE DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN enable-leaf IN WIDGET-HANDLE(char-hdl) (NO, bf-est.est-type).
  END.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO: 
    RUN enable-auto-calc IN WIDGET-HANDLE(char-hdl) (ll-one-eb-on-ef OR ll-part-style).
    RUN enable-copy IN WIDGET-HANDLE(char-hdl) (ll-one-ef-on-est).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var hd1 as handle no-undo.
  def var hd2 as handle no-undo.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR l-fit-len AS DEC NO-UNDO.
  DEF VAR l-fit-wid AS DEC NO-UNDO.
  def buffer bf-eb for eb.
{&methods/lValidateError.i YES}
IF NOT ll-auto-calc-selected THEN
  ef.spare-int-1:SENSITIVE IN frame {&frame-name} = TRUE.

 IF NOT ll-is-sheet-calc THEN DO:
   /* Code placed here will execute PRIOR to standard behavior. */
  /* ==== Corrugated item validation ======== */
  ASSIGN
  hd1 = frame {&frame-name}:HANDLE
  hd1 = hd1:first-child
  hd2 = hd1:first-child.     
  do while valid-handle(hd2):
     if hd2:type = "fill-in" and 
           hd2:data-type = "decimal" and
           hd2:private-data = "16th" and
           decimal(hd2:screen-value) - trunc(decimal(hd2:screen-value),0) >= v-16-or-32 
     then do:
           message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "               
                  view-as alert-box error.
           apply "entry" to hd2.
           return no-apply.
     end. 
     hd2 = hd2:next-sibling.
  end.       

  do with frame {&frame-name} :  /* validation */
      btn_board:HIDDEN = TRUE .
    if ef.m-code:screen-value <> "" and
       not can-find (first mach where mach.company = gcompany and
                                      mach.loc = eb.loc and
                                      mach.m-code = ef.m-code:screen-value)
    then do:
         message "Invalid Machine Code. Try Help." view-as alert-box error.
         apply "Entry" to ef.m-code.
         return no-apply.
    end.
    else if ef.m-code:screen-value = "" then ef.m-dscr:screen-value = "". 

    RUN new-m-code.

    if EF.BOARD:screen-value <> "" and
       not can-find (first item where item.company = gcompany and
                                      ((index("BPR",item.mat-type) > 0 and not lv-is-foam) or
                                      (index("1234",item.mat-type) > 0 and lv-is-foam) ) and
                                      item.industry = lv-industry and
                                      item.i-no = ef.board:screen-value)
    then do:
         message "Invalid Board. Try Help." view-as alert-box error.
         apply "entry" to ef.board.
         return no-apply.
    end.

    /*RUN new-board.*/

    RUN valid-adder (ef.adder[1]:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-adder (ef.adder[2]:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-adder (ef.adder[3]:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-adder (ef.adder[4]:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-adder (ef.adder[5]:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-adder (ef.adder[6]:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF bf-est.est-type NE 8 THEN DO:
    IF LASTKEY NE -1 THEN DO:
      RUN est/val-leaf.p (FRAME {&FRAME-NAME}:HANDLE, ?) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

    DO li = 1 TO EXTENT(ef.leaf):
      RUN valid-leaf-snum (li) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-leaf-bnum (li) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

    IF ef.leaf[1]:SCREEN-VALUE <> "" and
       INT(ef.leaf-w[1]:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO ef.leaf-w[1].
       RETURN NO-APPLY.
    END.
    IF ef.leaf[2]:SCREEN-VALUE <> "" and
       INT(ef.leaf-w[2]:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO ef.leaf-w[2].
       RETURN NO-APPLY.
    END.
    IF ef.leaf[3]:SCREEN-VALUE <> "" and
          INT(ef.leaf-w[3]:SCREEN-VALUE) = 0 THEN DO:
          MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ef.leaf-w[3].
          RETURN NO-APPLY.
    END.
    IF ef.leaf[4]:SCREEN-VALUE <> "" and
           INT(ef.leaf-w[4]:SCREEN-VALUE) = 0 THEN DO:
           MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO ef.leaf-w[4].
           RETURN NO-APPLY.
    END.

    IF ef.leaf[1]:SCREEN-VALUE <> "" and
      INT(ef.leaf-l[1]:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ef.leaf-l[1].
        RETURN NO-APPLY.
    END.
    IF ef.leaf[2]:SCREEN-VALUE <> "" and
       INT(ef.leaf-l[2]:SCREEN-VALUE) = 0 THEN DO:
         MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO ef.leaf-l[2].
         RETURN NO-APPLY.
    END.
    IF ef.leaf[3]:SCREEN-VALUE <> "" and
      INT(ef.leaf-l[3]:SCREEN-VALUE) = 0 THEN DO:
      MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ef.leaf-l[3].
      RETURN NO-APPLY.
    END.
    IF ef.leaf[4]:SCREEN-VALUE <> "" and
       INT(ef.leaf-l[4]:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO ef.leaf-l[4].
       RETURN NO-APPLY.
    END.
    END.

    if ef.cost-uom:screen-value <> "" then do:
       find first item where item.company = gcompany and
                                item.i-no = ef.board:screen-value
                                no-lock no-error.
       if avail item then find first e-item of item no-lock no-error.
       run sys/ref/uom-rm.p  (item.mat-type, output uom-list).
       if not can-find(first uom where lookup(uom.uom,uom-list) > 0 and
                            uom.uom = ef.cost-uom:screen-value)
       then do:
            message "Invalid Cost UOM. Try Help." view-as alert-box.
            apply "entry" to ef.cost-uom.
            return no-apply.
       end.                 
    end.  
    if ef.FR-uom:screen-value <> "" then do:
       uom-list = "CWT,MSF,MSH,TON".
       if not can-find(first uom where lookup(uom.uom,uom-list) > 0 and
                            uom.uom = ef.fr-uom:screen-value)
       then do:
            message "Invalid Freight UOM. Try Help." view-as alert-box.
            apply "entry" to ef.fr-uom.
            return no-apply.
       end.                 
    end.  
    if not ll-auto-calc-selected then do:
      RUN valid-gsh-wid NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-gsh-len NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-gsh-dep NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      /* Taken out per Joe 1/26/12 during override mode - RESTORED per joe 4/18/13*/
      if dec(ef.nsh-wid:screen-value) < dec(ef.trim-w:screen-value)
      then do:
        message "Net Sheet Size should not be less than Die Size." view-as alert-box error.
/*         apply "entry" to ef.nsh-wid. */
/*         return no-apply.             */
      end.
      ELSE
      if dec(ef.nsh-len:screen-value) < dec(ef.trim-l:screen-value)
      then do:
        message "Net Sheet Size should not be less than Die Size." view-as alert-box error.
/*         apply "entry" to ef.nsh-len. */
/*         return no-apply.             */
      end.

      RUN valid-trim-w NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-trim-l NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-trim-d NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    end.  /* not auto-calc */
  end.  /* frame {&frame-name} */
 END. /* not  ll-is-sheet-calc */

  if ll-is-sheet-calc then do:
     ll-is-sheet-calc = no.
     run sheet-calc2.
     disable ef.m-code with frame {&frame-name}.
  end.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  IF NOT AVAIL xef THEN
    find xef where recid(xef) = recid(ef).
  if xef.xgrain eq "S" then 
    assign
     l-fit-len = xef.gsh-wid
     l-fit-wid = xef.gsh-len.
  else
    assign
     l-fit-len = xef.gsh-len
     l-fit-wid = xef.gsh-wid.
  ef.spare-int-1:SENSITIVE = FALSE.

  find first mach {sys/ref/machW.i} and mach.m-code = xef.m-code  no-lock no-error.
  IF AVAIL(mach) AND xef.i-code = "R" AND
        ((xef.xgrain NE "B" AND (l-fit-len GT mach.max-wid 
     OR l-fit-wid GT mach.max-len))
     OR (xef.xgrain EQ "B" AND (l-fit-wid GT mach.max-wid 
     OR l-fit-len GT mach.max-len))
     OR decimal(eb.num-len:screen-value) EQ 0 
     OR DECIMAL(eb.num-wid:screen-value) EQ 0) THEN
       MESSAGE "The BOARD dimensions will not fit the Maximum Machine Dimensions." SKIP
               "Please check the Blank or Sheet Cross Grain Parameter."
               VIEW-AS ALERT-BOX.
  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(eb)).

  DISABLE eb.num-wid eb.num-len /*eb.num-dep*/ WITH FRAME {&FRAME-NAME}.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    RUN enable-leaf IN WIDGET-HANDLE(char-hdl) (bf-est.est-type EQ 8, bf-est.est-type).

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

def var ld-gsh-wid as dec no-undo.
def var ld-gsh-len as dec no-undo.
def var ld-gsh-dep as dec no-undo.
def var ld-nsh-wid as dec no-undo.
def var ld-nsh-len as dec no-undo.
def var ld-nsh-dep as dec no-undo.

  DO WITH FRAME {&FRAME-NAME}:

    ef.n-cuts:SCREEN-VALUE = STRING((DEC(ef.n-out:SCREEN-VALUE)   - 1) +
                                    (DEC(ef.n-out-l:SCREEN-VALUE) - 1) +
                                    ((IF DEC(ef.n-out-d:SCREEN-VALUE) NE 0 THEN DEC(ef.n-out-d:SCREEN-VALUE) ELSE 1) - 1)).
    ef.spare-int-1:SENSITIVE = TRUE.
    ef.spare-int-1:SCREEN-VALUE = STRING(getTotalUp()).
    ef.spare-int-1:SENSITIVE = FALSE.

    IF DEC(ef.n-cuts:SCREEN-VALUE) LT 0 THEN ef.n-cuts:SCREEN-VALUE = "0".

    if ll-auto-calc-selected then do:
       assign
        ld-gsh-wid = DEC(ef.gsh-wid:SCREEN-VALUE)
        ld-gsh-len = DEC(ef.gsh-len:SCREEN-VALUE)
        ld-gsh-dep = DEC(ef.gsh-dep:SCREEN-VALUE)
        ld-nsh-wid = DEC(ef.nsh-wid:SCREEN-VALUE)
        ld-nsh-len = DEC(ef.nsh-len:SCREEN-VALUE)
        ld-nsh-dep = DEC(ef.nsh-dep:SCREEN-VALUE)
        ll-num-out-changed = yes.

       {sys/inc/k16bb.i ld-gsh-wid}
       {sys/inc/k16bb.i ld-gsh-len}
       {sys/inc/k16bb.i ld-gsh-dep}
       {sys/inc/k16bb.i ld-nsh-wid}
       {sys/inc/k16bb.i ld-nsh-len}
       {sys/inc/k16bb.i ld-nsh-dep}

       if ef.m-code:screen-value in frame {&frame-name} <> "" then 
          find first mach where mach.company = gcompany and
                            mach.loc = ef.loc and
                            mach.m-code = ef.m-code:screen-value 
                            no-lock no-error.

       find first item where item.company = gcompany and
                             item.i-no = ef.board:screen-value
                             no-lock no-error.

       assign ld-gsh-wid = if not avail item or item.i-code eq "E" THEN ((INT(ef.n-out:SCREEN-VALUE) *
                                                                          DEC(IF ef.xgrain:SCREEN-VALUE EQ "S" THEN ld-nsh-len ELSE ld-nsh-wid)) +
                           if avail mach and mach.dept[1] eq "RC" then
                             (2 * mach.min-trimw) else 0)
                           else ld-gsh-wid
              ld-gsh-len = if not avail item or item.i-code eq "E" THEN ((INT(ef.n-out-l:SCREEN-VALUE) *
                                                                          DEC(IF ef.xgrain:SCREEN-VALUE EQ "S" THEN ld-nsh-wid ELSE ld-nsh-len)) +
                           if avail mach and mach.dept[1] eq "RC" then
                             (2 * mach.min-triml) else 0)
                           else ld-gsh-len
              ld-gsh-dep = if not avail item or item.i-code eq "E" then
                            (input ef.n-out-d * ef.nsh-dep)
                           else ld-gsh-dep

              ef.gsh-len:screen-value = string({sys/inc/k16.i ld-gsh-len})
              ef.gsh-wid:screen-value = string({sys/inc/k16.i ld-gsh-wid})
              ef.gsh-dep:screen-value = string({sys/inc/k16.i ld-gsh-dep}).
       FIND xef WHERE ROWID(xef) = ROWID(ef) EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL xef THEN
          xef.roll-wid = if item.i-code eq "E" OR not avail item then
                           ld-gsh-wid
                        ELSE ef.roll-wid.

    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-adder V-table-Win 
PROCEDURE new-adder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  DO WITH FRAME {&frame-name}:
    RELEASE item.
    IF ip-focus:SCREEN-VALUE NE "" THEN
    FIND FIRST item NO-LOCK
        WHERE item.company  EQ gcompany
          AND item.mat-type EQ "A"
          AND item.i-no     EQ ip-focus:SCREEN-VALUE 
        NO-ERROR.

    lv-field-hdl = lv-group-hdl:FIRST-CHILD.

    DO WHILE VALID-HANDLE(lv-field-hdl):
      IF lv-field-hdl:NAME  EQ "adder"            AND
         lv-field-hdl:INDEX EQ ip-focus:INDEX + 6 THEN DO:
        lv-field-hdl:SCREEN-VALUE = IF AVAIL item THEN item.i-name ELSE "".
        LEAVE.
      END.

      lv-field-hdl = lv-field-hdl:NEXT-SIBLING. 
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
       ef.test:SCREEN-VALUE     = item.reg-no
       ef.brd-dscr:SCREEN-VALUE = item.i-name
       ef.i-code:SCREEN-VALUE   = item.i-code
       ef.flute:SCREEN-VALUE    = item.flute
       ef.weight:SCREEN-VALUE   = STRING(item.basis-w).
      RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT uom-list).
      IF uom-list NE "" AND LOOKUP(ef.cost-uom:SCREEN-VALUE,uom-list) LE 0 THEN
        ef.cost-uom:SCREEN-VALUE = ENTRY(1,uom-list).
      IF item.i-code EQ "R" THEN
        IF item.r-wid GT 0 THEN
           ASSIGN
              ef.gsh-wid:SCREEN-VALUE = STRING({sys/inc/k16.i item.r-wid})
              ef.lsh-len:SCREEN-VALUE = STRING({sys/inc/k16.i item.r-wid})
              ef.roll:SCREEN-VALUE    = "Y".
        ELSE DO:
          ASSIGN
             ef.gsh-wid:SCREEN-VALUE = STRING({sys/inc/k16.i item.s-wid})
             ef.gsh-len:SCREEN-VALUE = STRING({sys/inc/k16.i item.s-len})
             ef.lsh-len:SCREEN-VALUE = STRING({sys/inc/k16.i item.s-wid})
             ef.lsh-wid:SCREEN-VALUE = STRING({sys/inc/k16.i item.s-len})
             ef.roll:SCREEN-VALUE    = "N".

          IF lv-is-foam THEN
             ef.gsh-dep:SCREEN-VALUE = STRING({sys/inc/k16.i item.s-dep}).

          IF ef.xgrain:SCREEN-VALUE EQ "S" THEN
            ASSIGN
             lv                      = ef.gsh-len:SCREEN-VALUE
             ef.gsh-len:SCREEN-VALUE = ef.gsh-wid:SCREEN-VALUE
             ef.gsh-wid:SCREEN-VALUE = lv.
        END.

      FIND FIRST e-item OF item NO-LOCK NO-ERROR.
      IF AVAIL e-item THEN ef.cost-uom:SCREEN-VALUE = e-item.std-uom.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE num-wid-len-dep-changed V-table-Win 
PROCEDURE num-wid-len-dep-changed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ef.die-in:SCREEN-VALUE = STRING(DEC(ef.die-in:SCREEN-VALUE) /
                                     INT(eb.num-up:SCREEN-VALUE))
     eb.num-up:SCREEN-VALUE = STRING((IF INT(eb.num-wid:SCREEN-VALUE) EQ 0 THEN 1 ELSE INT(eb.num-wid:SCREEN-VALUE)) *
                                     (IF INT(eb.num-len:SCREEN-VALUE) EQ 0 THEN 1 ELSE INT(eb.num-len:SCREEN-VALUE)) *
                                     (IF INT(eb.num-dep:SCREEN-VALUE) EQ 0 THEN 1 ELSE INT(eb.num-dep:SCREEN-VALUE)))
     ef.die-in:SCREEN-VALUE = STRING(DEC(ef.die-in:SCREEN-VALUE) *
                                     INT(eb.num-up:SCREEN-VALUE)).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE part-style V-table-Win 
PROCEDURE part-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-part-style AS LOG NO-UNDO.

  DEF BUFFER b-ac-eb FOR eb.
  DEF BUFFER b-ac-ef FOR ef.


  FIND b-ac-ef WHERE ROWID(b-ac-ef) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-ac-ef THEN
  FOR EACH b-ac-eb NO-LOCK
      WHERE b-ac-eb.company EQ b-ac-ef.company
        AND b-ac-eb.est-no  EQ b-ac-ef.est-no
        AND b-ac-eb.eqty    EQ b-ac-ef.eqty
        AND b-ac-eb.form-no EQ b-ac-ef.form-no,
      FIRST style NO-LOCK
      WHERE style.company EQ b-ac-eb.company
        AND style.style   EQ b-ac-eb.style:
    IF NOT CAN-DO("P,R",style.type)  OR
       b-ac-eb.t-wid NE eb.t-wid     OR
       b-ac-eb.num-len NE eb.num-len THEN LEAVE.
  END.

  op-part-style = NOT AVAIL b-ac-eb.

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
message "raw:"  program-name(1) skip
          program-name(2) skip
          program-name(3) skip          
          program-name(4) skip          
          program-name(5) skip          
          program-name(6) skip          
          program-name(7) skip          
          program-name(8) skip          
          program-name(9) skip          
  view-as alert-box.          
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

    if ll-auto-calc-selected then  do:  /* from cec/uest2.p */
       find first ce-ctrl where ce-ctrl.company =gcompany and
                           ce-ctrl.loc = eb.loc
                           no-lock no-error.
       find first item where item.company = gcompany and
                             item.i-no = ef.board:screen-value in frame {&frame-name}
                             no-lock no-error.
       if avail item then find first e-item of item no-lock no-error.                      
       assign lv-is-roll = no.
       find first mach where mach.company = gcompany and
                                      mach.loc = eb.loc and
                                      mach.m-code = ef.m-code:screen-value
                                      use-index m-code no-lock no-error.
       /* if (avail mach and mach.p-type = "R") or
          (avail item and ( (item.i-code = "R" and item.r-wid ne 0) or
          (avail e-item and e-item.roll-w[1] ne 0) )) or
          (not avail mach and ce-ctrl.avg-cscost ne 0) and
          not(xef.lsh-lock)  /** CTS **/
          then assign xef.roll = true. /* xef.lam-dscr = "R". */
          else       not for auto calc */

       if avail mach then do:   
          assign ef.lsh-len:screen-value = string(mach.max-wid)
                 ef.lsh-wid:screen-value = string(mach.max-len)
                 ls-lam-dscr = "S"
                 lv-is-roll = mach.p-type = "R".

          find first item where item.company = gcompany 
                         and item.mat-type = "A"
                         and item.i-no eq ef.board
                   use-index i-no no-lock no-error.
          if not avail item or item.i-code eq "E" then
              assign ef.gsh-wid:screen-value = string( trunc(mach.max-len / eb.t-wid,0) * eb.t-wid +
                                 (2 * mach.min-trimw))
                     ef.gsh-len:screen-value = string(trunc(mach.max-wid / eb.t-len,0) * eb.t-len +
                                 (2 * mach.min-triml) )
                     ef.gsh-dep:screen-value = if lv-is-foam then string(trunc(mach.max-dep / eb.t-dep,0) * eb.t-dep ) else "" .                     
          else if avail item then
               assign ef.gsh-wid:screen-value = string(item.s-wid)
                      ef.gsh-len:screen-value = string(item.s-len)
                      ef.gsh-dep:screen-value = if lv-is-foam then string(item.s-dep) else "".

       end.
       else do:
            assign lv-is-roll = ce-ctrl.avg-cscost <> 0
                   ls-lam-dscr = "S"
                   ef.lsh-wid:screen-value = string(ce-ctrl.ls-length)
                   ef.lsh-len:screen-value = string(ce-ctrl.ls-width)
                   ef.gsh-len:screen-value = string(eb.t-len)
                   ef.gsh-wid:screen-value = string(eb.t-wid)
                   .
       end.  

       assign ef.lsh-len:screen-value = string({sys/inc/k16.i decimal(ef.lsh-len:screen-value) } )
              ef.lsh-wid:screen-value = string({sys/inc/k16.i decimal(ef.lsh-wid:screen-value)} )
              ef.gsh-len:screen-value = string({sys/inc/k16.i decimal(ef.gsh-len:screen-value)} )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i decimal(ef.gsh-wid:screen-value)} ).
       if lv-is-foam then       
          ef.gsh-dep:screen-value = string({sys/inc/k16.i decimal(ef.gsh-dep:screen-value)} ).

       /* from uest2a.p */
       if ef.n-out:screen-value = "0" then ef.n-out:screen-value = "1".
       if ef.n-out-l:screen-value = "0" then ef.n-out-l:screen-value = "1".
       if ef.n-out-d:screen-value = "0" then ef.n-out-d:screen-value = "1".
       ef.n-cuts:screen-value = string((decimal(ef.n-out:screen-value) - 1) + 
                                       (decimal(ef.n-out-l:Screen-value) - 1) + 
                                       (decimal(ef.n-out-d:screen-value) - 1) ).

       if decimal(ef.n-cuts:screen-value) lt 0 then ef.n-cuts:screen-value = "0".

    /* maybe not here */
       /*find xest where recid(xest) = recid(est). */
       find xef where recid(xef) = recid(ef).
       find xeb where recid(xeb) = recid(eb).
       if not lv-is-foam or item.i-code eq "E" then do:
          ASSIGN
          xef.lam-dscr = ls-lam-dscr
          xef.roll = lv-is-roll
          xef.m-code = ef.m-code:SCREEN-VALUE
          xef.lsh-wid = trunc(decimal(ef.lsh-wid:screen-value),0) +
                               ((decimal(ef.lsh-wid:screen-value) -
                                 trunc(decimal(ef.lsh-wid:screen-value),0)
                                 ) * k_frac)
          xef.lsh-len = trunc(decimal(ef.lsh-len:screen-value),0) +
                               ((decimal(ef.lsh-len:screen-value) -
                                 trunc(decimal(ef.lsh-len:screen-value),0)
                                 ) * k_frac)
                 .
          run cec/u2k.p .  /* Calculation */
       end.
       find xef where recid(xef) = recid(ef).
       find xeb where recid(xeb) = recid(eb).

       if not lv-is-foam or item.i-code eq "E" then do:
         if avail mach and mach.dept[1] eq "RC" then
            assign  xef.nsh-wid = xef.nsh-wid - (2 * mach.min-trimw)
                    xef.nsh-len = xef.nsh-len - (2 * mach.min-triml).


         assign xef.n-out   = trunc(xef.lsh-wid / xef.nsh-wid,0)
                xef.n-out-l = trunc(xef.lsh-len / xef.nsh-len,0)
                xef.n-out-d = 1
                xef.nsh-dep = xeb.t-dep
                xef.trim-d  = xeb.t-dep
                xeb.num-dep = 1
                xef.gsh-wid = if not avail item or item.i-code eq "E" then
                                 ((xef.n-out   * xef.nsh-wid) +
                              if avail mach and mach.dept[1] eq "RC" then
                                 (2 * mach.min-trimw) else 0)
                              else xef.gsh-wid
                xef.gsh-len = if not avail item or item.i-code eq "E" then
                                 ( (xef.n-out-l * xef.nsh-len) +
                                   if avail mach and mach.dept[1] eq "RC" then
                                   (2 * mach.min-triml) else 0 )
                              else xef.gsh-len
                xef.gsh-dep = if not avail item or item.i-code eq "E" then
                                (xef.n-out-d * xef.nsh-dep)
                              else xef.gsh-dep.

         if xef.n-out-d eq ? then xef.n-out-d = 0.
         if xef.gsh-dep eq ? then xef.gsh-dep = 0.
       end.  

       assign ef.lsh-len:screen-value = string({sys/inc/k16.i xef.lsh-len} )
              ef.lsh-wid:screen-value = string({sys/inc/k16.i xef.lsh-wid} )
              ef.gsh-len:screen-value = string({sys/inc/k16.i xef.gsh-len} )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i xef.gsh-wid} )
              ef.nsh-len:screen-value = string({sys/inc/k16.i xef.nsh-len} )
              ef.nsh-wid:screen-value = string({sys/inc/k16.i xef.nsh-wid} )
              ef.trim-l:screen-value = string({sys/inc/k16.i xef.trim-l} )
              ef.trim-w:screen-value = string({sys/inc/k16.i xef.trim-w} )
              ef.n-out:screen-value = string(xef.n-out)
              ef.n-out-l:screen-value = string(xef.n-out-l)
              ef.n-cuts:screen-value = string(xef.n-cuts)
              eb.num-wid:screen-value = string(xeb.num-wid)
              eb.num-len:screen-value = string(xeb.num-len)
              eb.num-up:screen-value = string(xeb.num-up)
              .


       if lv-is-foam then       
          assign ef.gsh-dep:screen-value = string({sys/inc/k16.i xef.gsh-dep} )
                 ef.nsh-dep:screen-value = string({sys/inc/k16.i xef.nsh-dep} )
                 ef.trim-d:screen-value = string({sys/inc/k16.i xef.trim-d} )
                 ef.n-out-d:screen-value = string(xef.n-out-d)
                 .
/*     all with running u2k.p */

  /*     assign ef.lsh-len:screen-value = string({sys/inc/k16.i decimal(ef.lsh-len:screen-value)} )
              ef.lsh-wid:screen-value = string({sys/inc/k16.i decimal(ef.lsh-wid:Screen-value)} )
              ef.gsh-len:screen-value = string({sys/inc/k16.i decimal(ef.gsh-len:Screen-value)} )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i decimal(ef.gsh-wid:Screen-value)} )              
              ef.nsh-len:screen-value = string({sys/inc/k16.i decimal(ef.nsh-len:Screen-value)} )
              ef.nsh-wid:screen-value = string({sys/inc/k16.i decimal(ef.nsh-wid:Screen-value)} )
              .
   */  
    end. /* ll-auto-calc-selected */

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

  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-fields V-table-Win 
PROCEDURE reset-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  lltes:       
------------------------------------------------------------------------------*/
   DEF VAR ll AS LOG NO-UNDO.


   FIND FIRST style
       WHERE style.company EQ gcompany
         AND style.style   EQ eb.style
       NO-LOCK NO-ERROR.
   ll = NOT AVAIL style OR style.type NE "F".

   DO WITH FRAME {&frame-name}:
     ASSIGN
      ef.board:LABEL      = IF ll THEN "Board" ELSE "Foam"
      ls-dep-label:HIDDEN = ll
      ef.gsh-dep:HIDDEN   = ll
      ef.nsh-dep:HIDDEN   = ll
      ef.trim-d:HIDDEN    = ll
      eb.t-dep:HIDDEN     = ll
      ef.n-out-d:HIDDEN   = ll
      eb.num-dep:HIDDEN   = ll
      ef.spare-int-2:HIDDEN = ll
      ef.spare-int-3:HIDDEN = ll
      ef.spare-int-4:HIDDEN = ll .

     IF AVAIL style AND style.TYPE = "W" THEN
        ef.board:LABEL      =  "Wood" .
     ELSE IF AVAIL style AND style.TYPE = "C" THEN
        ef.board:LABEL      =  "PolyBag" .

   END.

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
       ef.lsh-wid:LABEL         = lv-label[1].
    ELSE
      ASSIGN
       ef.lsh-len:LABEL         = lv-label[1]
       ef.lsh-wid:LABEL         = lv-label[2].
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
IF bf-est.est-type NE 8 THEN RETURN.


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
  IF bf-est.est-type EQ 8 THEN DO:
      ASSIGN lv-tot-up = 0
             lv-tot-len = 0
             lv-tot-wid = 0.
      FOR EACH bf-eb WHERE bf-eb.company = ef.company AND
                           bf-eb.est-no = ef.est-no NO-LOCK.
          ASSIGN
          lv-tot-len = lv-tot-len + bf-eb.num-len
          lv-tot-wid = lv-tot-wid + bf-eb.num-wid
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
      USE-INDEX est-no NO-LOCK,
      FIRST job
      where job.company EQ job-hdr.company
        and job.job     EQ job-hdr.job
        and job.job-no  EQ job-hdr.job-no
        and job.job-no2 EQ job-hdr.job-no2
        and job.est-no  EQ job-hdr.est-no
        AND job.opened  EQ YES
      USE-INDEX job NO-LOCK
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sheet-calc V-table-Win 
PROCEDURE sheet-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ll-is-sheet-calc = yes.
  enable ef.m-code with frame {&frame-name}.
  apply "entry" to ef.m-code in frame {&frame-name}.
  return no-apply.

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
  /* from cec/bestfitc.p */
    DEFINE BUFFER bf-item FOR ITEM .

     find xest where xest.company = ef.company and
                     xest.est-no = ef.est-no
                     no-lock no-error.
     find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
     find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

     run cec/bestfitc.p (ef.m-code:SCREEN-VALUE IN FRAME {&FRAME-NAME}, 0, "","",ef.board).

     FIND FIRST tt-ef NO-ERROR.
     FIND FIRST tt-eb NO-ERROR.

     IF AVAIL tt-ef AND AVAIL tt-eb THEN DO:
       assign ef.board:screen-value = tt-ef.board
              ef.flute:screen-value = tt-ef.flute
              ef.test:screen-value = tt-ef.test
              ef.brd-dscr:screen-value = tt-ef.brd-dscr
              ef.m-code:screen-value = tt-ef.m-code
              ef.weight:screen-value = string(tt-ef.weight)
              ef.i-code:screen-value = tt-ef.i-code
              ef.lsh-len:screen-value = string({sys/inc/k16.i tt-ef.lsh-len} )
              ef.lsh-wid:screen-value = string({sys/inc/k16.i tt-ef.lsh-wid} )
              ef.roll:screen-value = string(tt-ef.roll,"Y/N")
              ef.gsh-len:screen-value = string({sys/inc/k16.i tt-ef.gsh-len} )
              ef.gsh-wid:screen-value = string({sys/inc/k16.i tt-ef.gsh-wid} )
              ef.nsh-len:screen-value = string({sys/inc/k16.i tt-ef.nsh-len} )
              ef.nsh-wid:screen-value = string({sys/inc/k16.i tt-ef.nsh-wid} )
              ef.trim-l:screen-value = string({sys/inc/k16.i tt-ef.trim-l} )
              ef.trim-w:screen-value = string({sys/inc/k16.i tt-ef.trim-w} )
              ef.n-out:screen-value = string(tt-ef.n-out)
              ef.n-out-l:screen-value = string(tt-ef.n-out-l)
              ef.n-cuts:screen-value = string(tt-ef.n-cuts)
              eb.num-wid:screen-value = string(tt-eb.num-wid)
              eb.num-len:screen-value = string(tt-eb.num-len)
              eb.num-up:screen-value = string(tt-eb.num-up)
              .

       RUN roll-display.

       if lv-is-foam then       
          assign ef.gsh-dep:screen-value = string({sys/inc/k16.i tt-ef.gsh-dep} )
                 ef.nsh-dep:screen-value = string({sys/inc/k16.i tt-ef.nsh-dep} )
                 ef.trim-d:screen-value = string({sys/inc/k16.i tt-ef.trim-d} )
                 ef.n-out-d:screen-value = string(tt-ef.n-out-d)
                 .
     END.

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
  IF NOT lv-is-foam THEN eb.num-up:SENSITIVE IN frame {&frame-name} = FALSE.
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
  def var lv-item-recid as recid no-undo.

  find item where item.company = gcompany and
                  item.i-no = ef.board
                  no-lock no-error.
  lv-item-recid =  if avail item then recid(item) else ?.

  if lv-item-recid <> ? then run cec/d-itmbom.w (recid(item), ROWID(ef)).

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
  IF bf-est.est-type NE 8 THEN RETURN.

  RUN ce/d-estflm.w (ef.company, ef.est-no, ef.eqty).
  RUN dispatch ('display-fields').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-adder V-table-Win 
PROCEDURE valid-adder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.

  DEF VAR lv-msg AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF ip-focus:SCREEN-VALUE NE "" THEN DO:
      IF lv-msg EQ ""                                               AND
         NOT CAN-FIND(FIRST item
                      WHERE item.company  EQ gcompany
                        AND item.mat-type EQ "A"
                        AND item.i-no     EQ ip-focus:SCREEN-VALUE) THEN
        lv-msg = "Invalid Adder, try help".

      lv-field-hdl = lv-group-hdl:FIRST-CHILD.

      IF lv-msg EQ "" THEN
      DO WHILE VALID-HANDLE(lv-field-hdl):
        IF lv-field-hdl:NAME         EQ "adder"               AND
           lv-field-hdl:INDEX        NE ip-focus:INDEX        AND
           lv-field-hdl:INDEX        LE 6                     AND
           lv-field-hdl:SCREEN-VALUE EQ ip-focus:SCREEN-VALUE THEN DO:
          lv-msg = "Adder already on this form, please try again".
          LEAVE.
        END.

        lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
      END.

      IF lv-msg NE "" THEN DO:
        MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ip-focus.
        RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-gsh-dep V-table-Win 
PROCEDURE valid-gsh-dep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ef.gsh-dep:SCREEN-VALUE) - TRUNC(DEC(ef.gsh-dep:SCREEN-VALUE),0) GE v-16-or-32 THEN
      lv-msg = "Should not have more than " + string(v-16-or-32 - 0.01) + " as decimal, field is (inches.16ths/32nd's). ".

    IF lv-msg EQ "" THEN
      IF DEC(ef.gsh-dep:SCREEN-VALUE) LT DEC(ef.nsh-dep:SCREEN-VALUE) 
        /*AND ll-auto-calc-selected*/ THEN
        lv-msg = "Gross Sheet Size should not be less than Net Sheet Size...".

    IF lv-msg NE "" THEN DO:
      MESSAGE lv-msg VIEW-AS ALERT-BOX ERROR.
/*       APPLY "entry" TO ef.gsh-dep. */
/*       RETURN ERROR.                */
    END.
  END.

  {methods/lValidateError.i NO}
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
  DEF VAR lv-msg AS CHAR NO-UNDO.
  DEF VAR v-s-len LIKE ITEM.s-len NO-UNDO.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ef.gsh-len:SCREEN-VALUE) - TRUNC(DEC(ef.gsh-len:SCREEN-VALUE),0) GE v-16-or-32 THEN
      lv-msg = "Should not have more than " + string(v-16-or-32 - 0.01) + " as decimal, field is (inches.16ths/32nd's). ".

    IF lv-msg EQ "" THEN
      IF DEC(ef.gsh-len:SCREEN-VALUE) LT (IF ef.xgrain:SCREEN-VALUE EQ "S" THEN
                                            DEC(ef.nsh-wid:SCREEN-VALUE)
                                          ELSE
                                            DEC(ef.nsh-len:SCREEN-VALUE))
        /*AND ll-auto-calc-selected*/ THEN
        lv-msg = "Gross Sheet Size should not be less than Net Sheet Size...".
    IF lv-msg NE "" THEN DO:
      MESSAGE lv-msg VIEW-AS ALERT-BOX ERROR.
/*       APPLY "entry" TO ef.gsh-len. */
/*       RETURN ERROR.                */
    END.
/*     FIND FIRST item                                                                                   */
/*         WHERE item.company EQ gcompany                                                                */
/*           AND item.i-no    EQ ef.board:SCREEN-VALUE                                                   */
/*         NO-LOCK NO-ERROR.                                                                             */
/*                                                                                                       */
/*     IF AVAIL item THEN DO:                                                                            */
/*                                                                                                       */
/*       ASSIGN v-s-len = {sys/inc/k16.i ITEM.s-len}.                                                    */
/* MESSAGE v-s-len ef.gsh-len ITEM.i-code                                                                */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                */
/*       IF item.i-code EQ "R" /*AND ll-auto-calc-selected*/ AND                                         */
/*          (NOT lv-is-foam AND v-s-len NE 0 AND DEC(ef.gsh-len:SCREEN-VALUE) NE v-s-len) THEN DO:       */
/*         MESSAGE "Gross Sheet Size may not be changed for a Real Material..." VIEW-AS ALERT-BOX ERROR. */
/*         RETURN ERROR.                                                                                 */
/*       END.                                                                                            */
/*     END.                                                                                              */
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
  DEF VAR lv-msg AS CHAR NO-UNDO.    
  DEF VAR li AS INT NO-UNDO.
  DEF VAR v-r-wid LIKE ITEM.r-wid NO-UNDO.
  DEF VAR v-s-wid LIKE ITEM.s-wid NO-UNDO.
  DEF VAR v-roll-w LIKE ITEM.r-wid NO-UNDO.
  DEF VAR v-tt-wid LIKE ITEM.s-wid NO-UNDO.
  DEF VAR v-tt-len LIKE ITEM.s-len NO-UNDO.
  DEF VAR v-tt-dep LIKE ITEM.s-dep NO-UNDO.
  DEF VAR v-item-wid LIKE ITEM.s-wid NO-UNDO.
  DEF VAR v-item-len LIKE ITEM.s-len NO-UNDO.
  DEF VAR v-item-dep LIKE ITEM.s-dep NO-UNDO.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ef.gsh-wid:SCREEN-VALUE) - TRUNC(DEC(ef.gsh-wid:SCREEN-VALUE),0) GE v-16-or-32 THEN
      lv-msg = "Can not have more than " + string(v-16-or-32 - 0.01) + " as decimal, field is (inches.16ths/32nd's). ".

    IF lv-msg EQ "" THEN
      IF DEC(ef.gsh-wid:SCREEN-VALUE) LT (IF ef.xgrain:SCREEN-VALUE EQ "S" THEN
                                            DEC(ef.nsh-len:SCREEN-VALUE)
                                          ELSE
                                            DEC(ef.nsh-wid:SCREEN-VALUE)) 
                                          /*AND ll-auto-calc-selected*/ THEN

        lv-msg = "Gross Sheet Size should not be less than Net Sheet Size...".

    IF lv-msg NE "" THEN DO:
      MESSAGE lv-msg VIEW-AS ALERT-BOX ERROR.
/*       APPLY "entry" TO ef.gsh-wid. */
/*       RETURN ERROR.                */
    END.

    FIND FIRST item
        WHERE item.company EQ gcompany
          AND item.i-no    EQ ef.board:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN DO:

      ASSIGN v-r-wid = {sys/inc/k16.i ITEM.r-wid}
             v-s-wid = {sys/inc/k16.i ITEM.s-wid}.

      IF item.i-code EQ "R" /*AND ll-auto-calc-selected*/ AND 
         ((ef.roll:SCREEN-VALUE EQ "Y" AND DEC(ef.gsh-wid:SCREEN-VALUE) NE v-r-wid)  OR
          (NOT lv-is-foam AND ef.roll:SCREEN-VALUE NE "Y" AND DEC(ef.gsh-wid:SCREEN-VALUE) NE v-s-wid)) THEN DO:
        MESSAGE "Gross Sheet Size may not be changed for a Real Material..." VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END.
      ELSE IF lv-is-foam AND item.i-code EQ "R" AND ef.roll:SCREEN-VALUE NE "Y" THEN
      DO:
         ASSIGN
            v-item-wid = {sys/inc/k16.i ITEM.s-wid}
            v-item-len = {sys/inc/k16.i ITEM.s-len}
            v-item-dep = {sys/inc/k16.i ITEM.s-dep}
            v-tt-wid = DEC(ef.gsh-wid:SCREEN-VALUE)
            v-tt-len = DEC(ef.gsh-len:SCREEN-VALUE)
            v-tt-dep = DEC(ef.gsh-dep:SCREEN-VALUE).

         IF v-item-wid EQ v-tt-wid THEN
            v-tt-wid = -1.
         ELSE IF v-item-wid EQ v-tt-len THEN
            v-tt-len = -1.
         ELSE IF v-item-wid EQ v-tt-dep THEN
            v-tt-dep = -1.

         IF v-item-len EQ v-tt-len THEN
            v-tt-len = -1.
         ELSE IF v-item-len EQ v-tt-wid THEN
            v-tt-wid = -1.
         ELSE IF v-item-len EQ v-tt-dep THEN
            v-tt-dep = -1.

         IF v-item-dep EQ v-tt-len THEN
            v-tt-len = -1.
         ELSE IF v-item-dep EQ v-tt-wid THEN
            v-tt-wid = -1.
         ELSE IF v-item-dep EQ v-tt-dep THEN
            v-tt-dep = -1.

         IF v-tt-wid NE -1 OR v-tt-len NE -1 OR
            v-tt-dep NE -1 THEN
            DO:
               MESSAGE "Gross Sheet Size may not be changed for a Real Material..."
                  VIEW-AS ALERT-BOX ERROR.
               RETURN ERROR.
            END.
      END.

      IF ef.roll:SCREEN-VALUE EQ "Y" AND ll-auto-calc-selected THEN DO:
        IF item.i-code EQ "E" THEN DO:
          FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.
          IF AVAIL e-item THEN
          DO li = 1 TO 26:
             v-roll-w = {sys/inc/k16.i e-item.roll-w[li]}.
             IF v-roll-w GE DEC(ef.gsh-wid:SCREEN-VALUE) THEN DO:
                ef.gsh-wid:SCREEN-VALUE = STRING(v-roll-w).
                LEAVE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-trim-d V-table-Win 
PROCEDURE valid-trim-d :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

    IF ef.trim-d:HIDDEN = NO THEN
    DO:
       IF DEC(ef.trim-d:SCREEN-VALUE) - TRUNC(DEC(ef.trim-d:SCREEN-VALUE),0) GT v-16-or-32 THEN DO:
          message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                 VIEW-AS ALERT-BOX ERROR.
          RETURN ERROR.
       END.

       IF DEC(ef.trim-d:SCREEN-VALUE) < DEC(eb.t-dep:SCREEN-VALUE) THEN DO:
          MESSAGE "Die Size should not be less than Blank Size." VIEW-AS ALERT-BOX ERROR.
/*           APPLY "entry" TO ef.trim-l. */
/*           RETURN ERROR.               */
       END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-trim-l V-table-Win 
PROCEDURE valid-trim-l :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ef.trim-l:SCREEN-VALUE) - TRUNC(DEC(ef.trim-l:SCREEN-VALUE),0) GT v-16-or-32
    THEN DO:
    message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
              VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.

    IF ll-auto-calc-selected AND DEC(ef.trim-l:SCREEN-VALUE) <
       (IF ef.xgrain:SCREEN-VALUE EQ "B" THEN DEC(eb.t-wid:SCREEN-VALUE) ELSE DEC(eb.t-len:SCREEN-VALUE))
    THEN DO:
      MESSAGE "Die Size should not be less than Blank Size." VIEW-AS ALERT-BOX ERROR.
/*       APPLY "entry" TO ef.trim-l. */
/*       RETURN ERROR.               */
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-trim-w V-table-Win 
PROCEDURE valid-trim-w :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ef.trim-w:SCREEN-VALUE) - TRUNC(DEC(ef.trim-w:SCREEN-VALUE),0) GT v-16-or-32
    THEN DO:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
              VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.

    IF ll-auto-calc-selected AND DEC(ef.trim-w:SCREEN-VALUE) <
       (IF ef.xgrain:SCREEN-VALUE EQ "B" THEN DEC(eb.t-len:SCREEN-VALUE) ELSE DEC(eb.t-wid:SCREEN-VALUE))
    then do:
      MESSAGE "Die Size should not be less than Blank Size." VIEW-AS ALERT-BOX ERROR.
/*       APPLY "entry" TO ef.trim-w. */
/*       RETURN ERROR.               */
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotalUp V-table-Win 
FUNCTION getTotalUp RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR opi-total-up AS INT NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
IF AVAIL ef THEN
opi-total-up = IF asi.ef.spare-int-1 = 0 THEN
       INT(asi.ef.n-out:SCREEN-VALUE) *
       INT(asi.ef.n-out-l:SCREEN-VALUE) *
       int(asi.ef.n-out-d:SCREEN-VALUE)
    ELSE
        ef.spare-int-1.
ELSE
    opi-total-up = 0.

END.

RETURN opi-total-up.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

