&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est\custfly.w
  
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
def input parameter ip-cust-no like cust.cust-no no-undo.
{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}
{methods/defines/hndldefs.i}
{methods/defines/cust.i &NEW="NEW"}

assign gcompany = g_company
       gloc = g_loc.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.


def var v-secur as log initial no no-undo.
def var v-secur-fld as char format "x(8)" no-undo.
def var v-flag   as log no-undo format "Y/N" init no extent 8.
def var v-prompt as log no-undo format "Yes/No".
def var v-log-fld like sys-ctrl.log-fld no-undo.
def var v-char-fld like sys-ctrl.char-fld no-undo.

DEF VAR v-valid AS LOG NO-UNDO.
DEF VAR v-custsize AS cha NO-UNDO.
DEF VAR v-cust-length AS INT NO-UNDO.

DEF BUFFER bf-cust FOR cust.
DEF VAR ll-secure AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cust

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define FIELDS-IN-QUERY-D-Dialog cust.cust-no cust.active cust.name ~
cust.addr[1] cust.addr[2] cust.city cust.state cust.zip cust.terms ~
cust.cr-use cust.cr-rating cust.cr-lim cust.ord-lim cust.disc ~
cust.curr-code cust.cr-hold-invdays cust.cust-level cust.cr-hold ~
cust.fin-chg cust.auto-reprice cust.inv-meth cust.an-edi-cust cust.sort ~
cust.tax-gr cust.tax-id cust.date-field[1] cust.type cust.contact cust.sman ~
cust.area-code cust.phone cust.fax cust.email cust.frt-pay cust.fob-code ~
cust.ship-part cust.loc cust.carrier cust.del-zone cust.terr cust.under-pct ~
cust.over-pct cust.markup cust.ship-days cust.pallet cust.case-bundle ~
cust.int-field[1] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-D-Dialog cust.active cust.name ~
cust.addr[1] cust.addr[2] cust.city cust.state cust.zip cust.terms ~
cust.cr-use cust.cr-rating cust.cr-lim cust.ord-lim cust.disc ~
cust.curr-code cust.cr-hold-invdays cust.cust-level cust.cr-hold ~
cust.fin-chg cust.auto-reprice cust.inv-meth cust.an-edi-cust cust.sort ~
cust.tax-gr cust.tax-id cust.date-field[1] cust.type cust.contact cust.sman ~
cust.area-code cust.phone cust.fax cust.email cust.frt-pay cust.fob-code ~
cust.ship-part cust.loc cust.carrier cust.del-zone cust.terr cust.under-pct ~
cust.over-pct cust.markup cust.ship-days cust.pallet cust.case-bundle ~
cust.int-field[1] 
&Scoped-define ENABLED-TABLES-IN-QUERY-D-Dialog cust
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-D-Dialog cust
&Scoped-define QUERY-STRING-D-Dialog FOR EACH cust SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH cust SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog cust
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog cust


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cust.active cust.name cust.addr[1] ~
cust.addr[2] cust.city cust.state cust.zip cust.terms cust.cr-use ~
cust.cr-rating cust.cr-lim cust.ord-lim cust.disc cust.curr-code ~
cust.cr-hold-invdays cust.cust-level cust.cr-hold cust.fin-chg ~
cust.auto-reprice cust.inv-meth cust.an-edi-cust cust.sort cust.tax-gr ~
cust.tax-id cust.date-field[1] cust.type cust.contact cust.sman ~
cust.area-code cust.phone cust.fax cust.email cust.frt-pay cust.fob-code ~
cust.ship-part cust.loc cust.carrier cust.del-zone cust.terr cust.under-pct ~
cust.over-pct cust.markup cust.ship-days cust.pallet cust.case-bundle ~
cust.int-field[1] 
&Scoped-define ENABLED-TABLES cust
&Scoped-define FIRST-ENABLED-TABLE cust
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel RECT-2 RECT-26 RECT-3 ~
RECT-4 
&Scoped-Define DISPLAYED-FIELDS cust.cust-no cust.active cust.name ~
cust.addr[1] cust.addr[2] cust.city cust.state cust.zip cust.terms ~
cust.cr-use cust.cr-rating cust.cr-lim cust.ord-lim cust.disc ~
cust.curr-code cust.cr-hold-invdays cust.cust-level cust.cr-hold ~
cust.fin-chg cust.auto-reprice cust.inv-meth cust.an-edi-cust cust.sort ~
cust.tax-gr cust.tax-id cust.date-field[1] cust.type cust.contact cust.sman ~
cust.area-code cust.phone cust.fax cust.email cust.frt-pay cust.fob-code ~
cust.ship-part cust.loc cust.carrier cust.del-zone cust.terr cust.under-pct ~
cust.over-pct cust.markup cust.ship-days cust.pallet cust.case-bundle ~
cust.int-field[1] 
&Scoped-define DISPLAYED-TABLES cust
&Scoped-define FIRST-DISPLAYED-TABLE cust
&Scoped-Define DISPLAYED-OBJECTS terms_dscr stax_tax-dscr custype_dscr ~
sman_sname loc_dscr carrier_dscr carr-mtx_del-dscr terr_dscr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cust.cust-no 
&Scoped-define List-4 cust.name cust.addr[1] cust.addr[2] cust.city ~
cust.state cust.zip cust.terms cust.cr-use cust.cr-rating cust.cr-lim ~
cust.ord-lim cust.disc cust.curr-code cust.cr-hold-invdays cust.cust-level ~
cust.cr-hold cust.fin-chg cust.auto-reprice cust.inv-meth cust.an-edi-cust ~
cust.sort cust.tax-gr cust.tax-id cust.type cust.contact cust.sman ~
cust.area-code cust.phone cust.fax cust.email cust.frt-pay cust.fob-code ~
cust.loc cust.carrier cust.del-zone cust.terr cust.under-pct cust.over-pct ~
cust.markup cust.ship-days cust.pallet cust.case-bundle cust.int-field[1] 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-GO 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE carr-mtx_del-dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE carrier_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE custype_dscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE loc_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE sman_sname AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE stax_tax-dscr AS CHARACTER FORMAT "x(25)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE terms_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE terr_dscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 8.1.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 18.57.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 3.57.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 11.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      cust SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cust.cust-no AT ROW 1.24 COL 17 COLON-ALIGNED
          LABEL "Customer"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     cust.active AT ROW 1.24 COL 42 COLON-ALIGNED
          LABEL "Status" FORMAT "x(11)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "(A)ctive","(I)nactive","(X) Inhouse","(S)tatement" 
          DROP-DOWN-LIST
          SIZE 19 BY 1 TOOLTIP "Active, Inactive, Inhouse, Statement"
     cust.name AT ROW 2.19 COL 17 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
          BGCOLOR 15 FONT 4
     cust.addr[1] AT ROW 3.14 COL 17 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
          BGCOLOR 15 FONT 4
     cust.addr[2] AT ROW 4.1 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
          BGCOLOR 15 FONT 4
     cust.city AT ROW 5.05 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
          BGCOLOR 15 FONT 4
     cust.state AT ROW 5.05 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     cust.zip AT ROW 5.05 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     cust.terms AT ROW 7.67 COL 19 COLON-ALIGNED
          LABEL "Terms"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 8 FONT 4
     terms_dscr AT ROW 7.67 COL 29 COLON-ALIGNED NO-LABEL
     cust.cr-use AT ROW 8.62 COL 19 COLON-ALIGNED
          LABEL "Cr. Acct #"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 8 FONT 4
     cust.cr-rating AT ROW 9.57 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 8 FONT 4
     cust.cr-lim AT ROW 10.52 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 8 FONT 4
     cust.ord-lim AT ROW 11.48 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 8 FONT 4
     cust.disc AT ROW 12.43 COL 19 COLON-ALIGNED
          LABEL "Discount %"
          VIEW-AS FILL-IN 
          SIZE 10.2 BY 1
          BGCOLOR 8 FONT 4
     cust.curr-code AT ROW 13.38 COL 19 COLON-ALIGNED
          LABEL "Currency"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 8 
     cust.cr-hold-invdays AT ROW 8.86 COL 58 COLON-ALIGNED
          LABEL "Grace Days"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 8 FONT 4
     cust.cust-level AT ROW 9.81 COL 58 COLON-ALIGNED
          LABEL "Price Level"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 8 FONT 4
     cust.cr-hold AT ROW 11 COL 48
          LABEL "Credit Hold"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     cust.fin-chg AT ROW 11.71 COL 48
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     cust.auto-reprice AT ROW 12.52 COL 48
          VIEW-AS TOGGLE-BOX
          SIZE 23.2 BY .81
     cust.inv-meth AT ROW 13.38 COL 48.2
          LABEL "Invoice Per PO"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .81
     cust.an-edi-cust AT ROW 14.1 COL 48.2
          LABEL "EDI"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     cust.sort AT ROW 16 COL 24 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Yes", "Y":U,
"No", "N":U
          SIZE 16.4 BY .81
     cust.tax-gr AT ROW 16.95 COL 22 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 8 FONT 4
     stax_tax-dscr AT ROW 16.95 COL 28 COLON-ALIGNED NO-LABEL
     cust.tax-id AT ROW 17.95 COL 22 COLON-ALIGNED
          LABEL "Tax Resale ID#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 8 FONT 4
     cust.date-field[1] AT ROW 1.24 COL 89 COLON-ALIGNED
          LABEL "Date Added" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 8 FONT 4
     cust.type AT ROW 2.24 COL 89 COLON-ALIGNED
          LABEL "Type"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 8 FONT 4
     custype_dscr AT ROW 2.24 COL 101 COLON-ALIGNED NO-LABEL
     cust.contact AT ROW 3.19 COL 89 COLON-ALIGNED
          LABEL "Contact"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
          BGCOLOR 8 FONT 4
     cust.sman AT ROW 4.14 COL 89 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 8 FONT 4
     sman_sname AT ROW 4.14 COL 95 COLON-ALIGNED NO-LABEL
     cust.area-code AT ROW 5.14 COL 89 COLON-ALIGNED
          LABEL "Phone#" FORMAT "(xxx)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 8 FONT 4
     cust.phone AT ROW 5.1 COL 97 COLON-ALIGNED NO-LABEL FORMAT "xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 8 FONT 4
     cust.fax AT ROW 5.05 COL 121 COLON-ALIGNED FORMAT "(xxx)xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
          BGCOLOR 8 FONT 4
     cust.email AT ROW 6.1 COL 89 COLON-ALIGNED
          LABEL "Email"
          VIEW-AS FILL-IN 
          SIZE 53 BY 1
          BGCOLOR 8 
     cust.frt-pay AT ROW 8.19 COL 94.2 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Bill", "B":U,
"Collect", "C":U,
"Prepaid", "P":U,
"3rd Party", "T":U
          SIZE 49 BY .81
     cust.fob-code AT ROW 9.14 COL 94.2 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Destination", "DEST":U,
"Origin", "ORIG":U
          SIZE 28 BY .81
     cust.ship-part AT ROW 9.1 COL 124
          LABEL "Partial Ship"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .81
     cust.loc AT ROW 10.52 COL 90 COLON-ALIGNED
          LABEL "Location"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 8 FONT 4
     loc_dscr AT ROW 10.52 COL 102 COLON-ALIGNED NO-LABEL
     cust.carrier AT ROW 11.52 COL 90.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 8 FONT 4
     carrier_dscr AT ROW 11.48 COL 102 COLON-ALIGNED NO-LABEL
     cust.del-zone AT ROW 12.52 COL 90.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 8 FONT 4
     carr-mtx_del-dscr AT ROW 12.43 COL 102 COLON-ALIGNED NO-LABEL
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     cust.terr AT ROW 13.52 COL 90.2 COLON-ALIGNED
          LABEL "Territory"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 8 FONT 4
     terr_dscr AT ROW 13.38 COL 102 COLON-ALIGNED NO-LABEL
     cust.under-pct AT ROW 14.52 COL 90.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 FONT 4
     cust.over-pct AT ROW 15.48 COL 90.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 FONT 4
     cust.markup AT ROW 16.43 COL 90.4 COLON-ALIGNED FORMAT "->9.99"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     cust.ship-days AT ROW 17.38 COL 90.4 COLON-ALIGNED
          LABEL "Whse Days"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 FONT 4
     cust.pallet AT ROW 15 COL 120.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 8 FONT 4
     cust.case-bundle AT ROW 15.95 COL 120.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 8 FONT 4
     cust.int-field[1] AT ROW 16.91 COL 120.8 COLON-ALIGNED
          LABEL "No. Load Tags" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
          BGCOLOR 8 FONT 4
     Btn_OK AT ROW 20.29 COL 37
     Btn_Cancel AT ROW 20.29 COL 90
     RECT-2 AT ROW 7.43 COL 3
     RECT-26 AT ROW 1 COL 1
     RECT-3 AT ROW 15.52 COL 3
     RECT-4 AT ROW 7.43 COL 73.6
     "Taxable:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 15.76 COL 13
     "Tax Information" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 15.05 COL 5
          FGCOLOR 9 FONT 4
     "Credit Information" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 6.95 COL 6
          FGCOLOR 9 FONT 4
     "FOB:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 9.14 COL 85.2
     "Freight Payment:" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 8.14 COL 74
     "Shipping Information" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 7.19 COL 77
          FGCOLOR 9 FONT 4
     SPACE(44.59) SKIP(13.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Customer Maintenance"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
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
   Custom                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cust.active IN FRAME D-Dialog
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.addr[1] IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.addr[2] IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR TOGGLE-BOX cust.an-edi-cust IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.area-code IN FRAME D-Dialog
   4 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR TOGGLE-BOX cust.auto-reprice IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN carr-mtx_del-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.carrier IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN carrier_dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.case-bundle IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.city IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.contact IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX cust.cr-hold IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.cr-hold-invdays IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.cr-lim IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.cr-rating IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.cr-use IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.curr-code IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.cust-level IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.cust-no IN FRAME D-Dialog
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN custype_dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.date-field[1] IN FRAME D-Dialog
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.del-zone IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.disc IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.email IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.fax IN FRAME D-Dialog
   4 EXP-FORMAT                                                         */
/* SETTINGS FOR TOGGLE-BOX cust.fin-chg IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR RADIO-SET cust.fob-code IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR RADIO-SET cust.frt-pay IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.int-field[1] IN FRAME D-Dialog
   4 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR TOGGLE-BOX cust.inv-meth IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.loc IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN loc_dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.markup IN FRAME D-Dialog
   4 EXP-FORMAT                                                         */
/* SETTINGS FOR FILL-IN cust.name IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.ord-lim IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.over-pct IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.pallet IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.phone IN FRAME D-Dialog
   4 EXP-FORMAT                                                         */
/* SETTINGS FOR FILL-IN cust.ship-days IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX cust.ship-part IN FRAME D-Dialog
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.sman IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN sman_sname IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET cust.sort IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.state IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN stax_tax-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.tax-gr IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.tax-id IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.terms IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN terms_dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.terr IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN terr_dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.type IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.under-pct IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.zip IN FRAME D-Dialog
   4 EXP-LABEL                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "ASI.cust"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON HELP OF FRAME D-Dialog /* Customer Maintenance */
DO:
   def var lv-handle as handle no-undo.
   def var char-val as cha no-undo.
   
   CASE Focus:name :
       when "del-zone" then do:
           run windows/l-delzon.w 
              (gcompany,gloc,cust.carrier:screen-value in frame {&frame-name},focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "pallet" then do:
           run windows/l-itemp.w 
              (gcompany,"",focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "case-bundle" then do:
           run windows/l-item.w 
              (gcompany,"","C",focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "cust-no" then do:
           run windows/l-cust.w 
              (gcompany,focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "sman" then do:
           run windows/l-sman.w 
              (gcompany,output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "terms" then do:
           run windows/l-terms.w 
              (gcompany,focus:screen-value in frame {&frame-name},output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "custype" then do:
           run windows/l-csttyp.w 
              (gcompany,focus:screen-value in frame {&frame-name},output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "state" then do:
           run windows/l-state.w 
              (focus:screen-value in frame {&frame-name},output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     .
           return no-apply.  
     end.
     when "terr" then do:
           run windows/l-terr.w 
              (gcompany,focus:screen-value in frame {&frame-name},output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "type" then do:
           run windows/l-csttyp.w 
              (gcompany,focus:screen-value in frame {&frame-name},output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "loc" then do:
           run windows/l-loc.w 
              (gcompany,focus:screen-value in frame {&frame-name},output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "carrier" then do:
           run windows/l-carrie.w 
              (gcompany,gloc,focus:screen-value in frame {&frame-name},output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "tax-gr" then do:
           run windows/l-stax.w 
              (gcompany,focus:screen-value in frame {&frame-name},output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.

     otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.
             
           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
        
           end.   /* g_lookup-var <> "" */
           apply "entry" to lv-handle.
           return no-apply.

     end.  /* otherwise */
  end case.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Customer Maintenance */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.active D-Dialog
ON return OF cust.active IN FRAME D-Dialog /* Status */
DO:
  apply "tab" to self.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.an-edi-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.an-edi-cust D-Dialog
ON return OF cust.an-edi-cust IN FRAME D-Dialog /* EDI */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.auto-reprice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.auto-reprice D-Dialog
ON return OF cust.auto-reprice IN FRAME D-Dialog /* Auto Reprice */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
    MESSAGE "Do you want to delete a created customer? "
          VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
    
    IF ll-ans THEN DO:
       find cust where cust.company = gcompany and cust.cust-no = ip-cust-no no-error.       
       DELETE cust.
    END.
    ELSE DO:
         APPLY "choose" TO btn_ok.
         RETURN NO-APPLY.
    END.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Save */
DO:
 

   RUN cust-zip.
    
   /*run validate-cust no-error.
   if error-status:error then return no-apply. */
  do with frame {&frame-name}:
     IF cust.cust-no:SCREEN-VALUE = "" THEN DO:
           MESSAGE "Customer number must be entered." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO cust.cust-no.
           RETURN NO-APPLY.
     END.
     FIND FIRST bf-cust WHERE bf-cust.company = gcompany 
                             AND bf-cust.cust-no = cust.cust-no:SCREEN-VALUE
                             AND RECID(bf-cust) <> RECID(cust)
                             NO-LOCK NO-ERROR.
     IF AVAIL bf-cust THEN DO:
           MESSAGE "Customer already exist. Try other number." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO cust.cust-no.
           RETURN NO-APPLY.
     END.
     IF v-custsize = "Hughes" and
           v-cust-length <> LENGTH(cust.cust-no:SCREEN-VALUE) THEN DO:
           MESSAGE "Customer Number must be " v-cust-length "digits character." 
                   VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO cust.cust-no.
           RETURN NO-APPLY.
     END.
    
    
     if cust.sman:screen-value <> "" and
        not can-find(first sman where sman.sman = cust.sman:screen-value)
     then do:
        message "Invalid Sales Rep. Try Help." view-as alert-box error.
        apply "entry" to cust.sman.
        return  NO-APPLY.
     end.
     if cust.pallet:screen-value <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "D" and
                                      item.i-no = cust.pallet:screen-value)
     then do:
        message "Invalid Pallet Code. Try Help." view-as alert-box error.
        apply "entry" to cust.pallet.
        return  NO-APPLY.     
     end.
     if cust.terr:screen-value <> "" and
        not can-find(first terr where terr.company = gcompany and
                                      terr.terr = cust.terr:screen-value)
     then do:
        message "Invalid Territory Code. Try Help." view-as alert-box error.
        apply "entry" to cust.terr.
        return no-apply.     
     end.
     if cust.case-bundle:screen-value <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "C" and
                                      item.i-no = cust.case-bundle:screen-value)
     then do:
        message "Invalid Case/Bundle Code. Try Help." view-as alert-box error.
        apply "entry" to cust.case-bundle.
        return no-apply.     
     end.
     if cust.loc:screen-value <> "" and
        not can-find(first loc where loc.company = gcompany and 
                                     loc.loc = cust.loc:screen-value)
     then do:
        message "Invalid Ord. Loc. Code. Try Help." view-as alert-box error.
        apply "entry" to cust.loc.
        return no-apply.     
     end.
     if /*cust.carrier:screen-value <> "" and*/
        not can-find(first carrier where carrier.company = gcompany and 
                                     carrier.loc = gloc and
                                     carrier.carrier = cust.carrier:screen-value)
     then do:
        message "Invalid Carrier Code. Try Help." view-as alert-box error.
        apply "entry" to cust.carrier.
        return no-apply.     
     end.
     if /*cust.del-zone:screen-value <> "" and*/
        not can-find(first carr-mtx where carr-mtx.company = gcompany and 
                                     carr-mtx.loc = gloc and
                                     carr-mtx.carrier = cust.carrier:screen-value and
                                     carr-mtx.del-zone = cust.del-zone:screen-value)
     then do:
        message "Invalid Delivey Zone. Try Help." view-as alert-box error.
        apply "entry" to cust.del-zone.
        return no-apply.     
     end.
     if cust.sort:screen-value = "Y" and 
        (
        /* not can-find(first stax where stax.tax-group begins gcompany and
                                   substring(stax.tax-group,1,10) = gcompany and
                                   substring(stax.tax-group,11,length(trim(stax.tax-group)) - 10) = cust.tax-gr:screen-value )
        */
          not can-find(first stax-group where stax-group.tax-group = cust.tax-gr:screen-value)         
         or
         cust.tax-gr:screen-value = ""                    
        )    
     then do:
        message "Invalid Tax Code. Try Help." view-as alert-box error.
        apply "entry" to cust.tax-gr.
        return no-apply.
     end.                                     
     if cust.state:screen-value <> "" and
        not can-find(first state where state.state = cust.state:screen-value )
     then do:
       message "Invalid State Code. Try Help." view-as alert-box error.
       apply "entry" to cust.state.
       return no-apply.
     end.                                     
     if decimal(cust.cust-level:screen-value) > 10 then 
     do:
        message "Price level can not exceed 10." view-as alert-box error.
        apply "entry" to cust.cust-level.
        return no-apply.
     end.
     /* validate cr hold */
    v-valid = YES.
    IF STRING(cust.cr-hold,"yes/no") NE cust.cr-hold:SCREEN-VALUE AND
       NOT ll-secure                                              THEN DO:

       RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

       IF NOT ll-secure THEN
         ASSIGN
         v-valid                   = NO
         cust.cr-hold:SCREEN-VALUE = STRING(cust.cr-hold,"yes/no").
    END.
    IF NOT v-valid THEN do:
       APPLY "entry" TO cust.cr-hold.
       RETURN NO-APPLY.
    END.
  /* end cr hold validateion */
     /* cr hold invdays */
    v-valid = YES.
    IF v-secur                                                        AND
       SUBSTR(v-secur-fld,1,1) EQ "Y"                                 AND
       cust.cr-hold-invdays NE DEC(cust.cr-hold-invdays:SCREEN-VALUE) AND
       NOT ll-secure                                                  THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                           = NO
         cust.cr-hold-invdays:SCREEN-VALUE = STRING(cust.cr-hold-invdays).
     END.
     IF NOT v-valid THEN do:
         APPLY "entry" TO cust.cr-hold-invdays.
         RETURN NO-APPLY.
     END.
     /* cr limit */
     v-valid = YES.
     IF v-secur                                      AND
       SUBSTR(v-secur-fld,2,1) EQ "Y"               AND
       cust.cr-lim NE DEC(cust.cr-lim:SCREEN-VALUE) AND
       NOT ll-secure                                THEN DO:

        RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
      IF NOT ll-secure THEN
        ASSIGN
         v-valid                  = NO
         cust.cr-lim:SCREEN-VALUE = STRING(cust.cr-lim).
    END.

    IF NOT v-valid THEN do:
        APPLY "entry" TO cust.cr-lim.
        RETURN NO-APPLY.
    END.
    /* ord limit */
     v-valid = YES.
     IF v-secur                                        AND
       SUBSTR(v-secur-fld,3,1) EQ "Y"                 AND
       cust.ord-lim NE DEC(cust.ord-lim:SCREEN-VALUE) AND
       NOT ll-secure                                  THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
      IF NOT ll-secure THEN
        ASSIGN
         v-valid                   = NO
         cust.ord-lim:SCREEN-VALUE = STRING(cust.ord-lim).
    END.

    IF NOT v-valid THEN do:
        APPLY "entry" TO cust.ord-lim.
        RETURN NO-APPLY.
    END.
    /* inv meth */
    v-valid = YES.
    IF v-secur                                                      AND
      SUBSTR(v-secur-fld,4,1) EQ "Y"                               AND
      STRING(cust.inv-meth,"yes/no") NE cust.inv-meth:SCREEN-VALUE AND
      NOT ll-secure                                                THEN DO:

     RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

     IF NOT ll-secure THEN
       ASSIGN
        v-valid                    = NO
        cust.inv-meth:SCREEN-VALUE = STRING(cust.inv-meth,"yes/no").
   END.

   IF NOT v-valid THEN do:
       APPLY "entry" TO cust.inv-meth.
       RETURN NO-APPLY.
   END.
 
     /* validate terms */
     v-valid = YES.
     IF v-secur                               AND
          SUBSTR(v-secur-fld,5,1) EQ "Y"        AND
          cust.terms NE cust.terms:SCREEN-VALUE AND
          NOT ll-secure                         THEN DO:

         RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
         IF NOT ll-secure THEN
           ASSIGN
            v-valid                   = NO
            cust.terms:SCREEN-VALUE = cust.terms.
       END. 
       IF v-valid                                                                 AND
          NOT CAN-FIND(FIRST terms WHERE terms.t-code EQ cust.terms:SCREEN-VALUE) THEN DO:        
         v-valid = NO.
       END.

       IF NOT v-valid THEN do:
           MESSAGE "Invalid Terms. Try Help." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO cust.terms.
           RETURN NO-APPLY.
       END.
       /* terms validateion */
       /* valid disc */
        v-valid = YES.
        IF v-secur                                  AND
           SUBSTR(v-secur-fld,6,1) EQ "Y"           AND
           cust.disc NE DEC(cust.disc:SCREEN-VALUE) AND
           NOT ll-secure                            THEN DO:

           RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
           IF NOT ll-secure THEN
              ASSIGN v-valid                = NO
                     cust.disc:SCREEN-VALUE = STRING(cust.disc).
         END.

         IF NOT v-valid THEN do:
             APPLY "entry" TO cust.disc.
             RETURN NO-APPLY.
         END.
         /* valid fin, charge */
         v-valid = YES.
        IF v-secur                                                    AND
           SUBSTR(v-secur-fld,7,1) EQ "Y"                             AND
           STRING(cust.fin-chg,"yes/no") NE cust.fin-chg:SCREEN-VALUE AND
           NOT ll-secure                                              THEN DO:

           RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

           IF NOT ll-secure THEN
              ASSIGN v-valid                   = NO
                     cust.fin-chg:SCREEN-VALUE = STRING(cust.fin-chg,"yes/no").
         END.

         IF NOT v-valid THEN do:
             APPLY "entry" TO cust.fin-chg.
             RETURN NO-APPLY.
         END.
         /* valid markup */
          v-valid = YES.
          IF v-secur                                      AND
             SUBSTR(v-secur-fld,8,1) EQ "Y"               AND
             cust.markup NE DEC(cust.markup:SCREEN-VALUE) AND
             NOT ll-secure                                THEN DO:

             RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
             IF NOT ll-secure THEN
             ASSIGN v-valid                  = NO
                    cust.markup:SCREEN-VALUE = STRING(cust.markup).
           END.

           IF NOT v-valid THEN do:
               APPLY "entry" TO cust.markup.
               RETURN NO-APPLY.
           END.
  end.
/* validation for existing customer 
  RUN valid-cr-hold.
  IF NOT v-valid THEN RETURN NO-APPLY.

  RUN valid-cr-hold-invdays.
  IF NOT v-valid THEN RETURN NO-APPLY.

  RUN valid-cr-lim.
  IF NOT v-valid THEN RETURN NO-APPLY.

  RUN valid-ord-lim.
  IF NOT v-valid THEN RETURN NO-APPLY.
  
  RUN valid-inv-meth.
  IF NOT v-valid THEN RETURN NO-APPLY.

  RUN valid-terms.
  IF NOT v-valid THEN RETURN NO-APPLY.
  
  RUN valid-disc.
  IF NOT v-valid THEN RETURN NO-APPLY.

  RUN valid-fin-chg.
  IF NOT v-valid THEN RETURN NO-APPLY.

  RUN valid-markup.
  IF NOT v-valid THEN RETURN NO-APPLY.
*/
  /* ============== end of validations ==================*/
  
  ASSIGN {&list-4}.

  run assign-cust.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.carrier D-Dialog
ON ENTRY OF cust.carrier IN FRAME D-Dialog /* Carrier */
DO:
  s-loc = cust.loc:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.carrier D-Dialog
ON LEAVE OF cust.carrier IN FRAME D-Dialog /* Carrier */
DO:
     if lastkey <> -1 /*and cust.carrier:screen-value <> "" */ and
        not can-find(first carrier where carrier.company = gcompany and 
                                     carrier.loc = gloc and
                                     carrier.carrier = cust.carrier:screen-value)
     then do:
        message "Invalid Carrier Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.case-bundle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.case-bundle D-Dialog
ON LEAVE OF cust.case-bundle IN FRAME D-Dialog /* Case/Bundle */
DO:
     if lastkey <> -1 and cust.case-bundle:screen-value <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "C" and
                                      item.i-no = cust.case-bundle:screen-value)
     then do:
        message "Invalid Case/Bundle Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cr-hold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cr-hold D-Dialog
ON LEAVE OF cust.cr-hold IN FRAME D-Dialog /* Credit Hold */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cr-hold.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cr-hold D-Dialog
ON return OF cust.cr-hold IN FRAME D-Dialog /* Credit Hold */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cr-hold-invdays
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cr-hold-invdays D-Dialog
ON LEAVE OF cust.cr-hold-invdays IN FRAME D-Dialog /* Grace Days */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cr-hold-invdays.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cr-lim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cr-lim D-Dialog
ON LEAVE OF cust.cr-lim IN FRAME D-Dialog /* Credit Limit */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cr-lim.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cust-level
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cust-level D-Dialog
ON LEAVE OF cust.cust-level IN FRAME D-Dialog /* Price Level */
DO:
    if lastkey <> -1 and 
       decimal(cust.cust-level:screen-value) > 10 then 
    do:
        message "Price level can not exceed 10." view-as alert-box error.
        return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.del-zone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.del-zone D-Dialog
ON ENTRY OF cust.del-zone IN FRAME D-Dialog /* Delivery Zone */
DO:
  ASSIGN
    s-loc = cust.loc:SCREEN-VALUE
    s-carrier = cust.carrier:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.del-zone D-Dialog
ON LEAVE OF cust.del-zone IN FRAME D-Dialog /* Delivery Zone */
DO:
     if lastkey <> -1 and /*cust.del-zone:screen-value <> "" and*/
        not can-find(first carr-mtx where carr-mtx.company = gcompany and 
                                     carr-mtx.loc = gloc and
                                     carr-mtx.carrier = cust.carrier:screen-value and
                                     carr-mtx.del-zone = cust.del-zone:screen-value)
     then do:
        message "Invalid Delivey Zone. Try Help." view-as alert-box error.
        return no-apply.     
     end.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.disc D-Dialog
ON LEAVE OF cust.disc IN FRAME D-Dialog /* Discount % */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-disc.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.fin-chg D-Dialog
ON LEAVE OF cust.fin-chg IN FRAME D-Dialog /* Finance Charges */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-fin-chg.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.fin-chg D-Dialog
ON return OF cust.fin-chg IN FRAME D-Dialog /* Finance Charges */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.fob-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.fob-code D-Dialog
ON return OF cust.fob-code IN FRAME D-Dialog /* FOB Code */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.frt-pay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.frt-pay D-Dialog
ON return OF cust.frt-pay IN FRAME D-Dialog /* FR PAY CD */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.inv-meth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.inv-meth D-Dialog
ON LEAVE OF cust.inv-meth IN FRAME D-Dialog /* Invoice Per PO */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-inv-meth.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.loc D-Dialog
ON LEAVE OF cust.loc IN FRAME D-Dialog /* Location */
DO:
     if lastkey <> -1 and cust.loc:screen-value <> "" and
        not can-find(first loc where loc.company = gcompany and 
                                     loc.loc = cust.loc:screen-value)
     then do:
        message "Invalid Ord. Loc. Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.markup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.markup D-Dialog
ON LEAVE OF cust.markup IN FRAME D-Dialog /* Mark-Up */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-markup.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.ord-lim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.ord-lim D-Dialog
ON LEAVE OF cust.ord-lim IN FRAME D-Dialog /* Order Limit */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ord-lim.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.pallet D-Dialog
ON LEAVE OF cust.pallet IN FRAME D-Dialog /* Pallet */
DO:
  if lastkey <> -1 and cust.pallet:screen-value <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "D" and
                                      item.i-no = cust.pallet:screen-value)
     then do:
        message "Invalid Pallet Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.ship-part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.ship-part D-Dialog
ON return OF cust.ship-part IN FRAME D-Dialog /* Partial Ship */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.sman D-Dialog
ON LEAVE OF cust.sman IN FRAME D-Dialog /* SalesRep */
DO:
   if lastkey <> -1 and cust.sman:screen-value <> "" and
      not can-find(first sman where sman.sman = cust.sman:screen-value)
   then do:
        message "Invalid Sales Rep. Try Help." view-as alert-box error.
        return no-apply.
   end.
   
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.sort D-Dialog
ON return OF cust.sort IN FRAME D-Dialog /* Taxable? */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.state D-Dialog
ON LEAVE OF cust.state IN FRAME D-Dialog /* State */
DO:
    if lastkey <> -1 and cust.state:screen-value <> "" and
       not can-find(first state where state.state = cust.state:screen-value )
    then do:
       message "Invalid State Code. Try Help." view-as alert-box error.
       return no-apply.
    end.                                     

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.tax-gr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.tax-gr D-Dialog
ON LEAVE OF cust.tax-gr IN FRAME D-Dialog /* Tax Code */
DO:

  if lastkey <> -1 and cust.sort:screen-value = "Y" and 
     (
    /* old
      not can-find(first stax where stax.tax-group begins gcompany and
                                   substring(stax.tax-group,1,10) = gcompany and
                                   substring(stax.tax-group,11,length(trim(stax.tax-group)) - 10) = cust.tax-gr:screen-value )
    */
       not can-find(first stax-group where stax-group.tax-group = cust.tax-gr:screen-value)                                   
     or
     cust.tax-gr:screen-value = ""                    
     )    
  then do:
     message "Invalid Tax Code. Try Help." self:screen-value view-as alert-box error.
     return no-apply.
  end.                                     
                                   
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.terms D-Dialog
ON LEAVE OF cust.terms IN FRAME D-Dialog /* Terms */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-terms.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.terr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.terr D-Dialog
ON LEAVE OF cust.terr IN FRAME D-Dialog /* Territory */
DO:
    if lastkey <> -1 and cust.terr:screen-value <> "" and
        not can-find(first terr where terr.company = gcompany and
                                      terr.terr = cust.terr:screen-value)
     then do:
        message "Invalid Territory Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.type D-Dialog
ON LEAVE OF cust.type IN FRAME D-Dialog /* Type */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.zip D-Dialog
ON HELP OF cust.zip IN FRAME D-Dialog /* Zip Code */
DO:
  RUN applhelp.p.
  {&self-name}:SCREEN-VALUE = g_lookup-var.
  RUN cust-zip.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.zip D-Dialog
ON LEAVE OF cust.zip IN FRAME D-Dialog /* Zip Code */
DO:
  IF LASTKEY NE -1 THEN RUN cust-zip.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.

find cust where cust.company = gcompany and cust.cust-no = ip-cust-no no-error.
if not avail cust then DO TRANSACTION:
   CREATE cust.
   cust.cust-no = ip-cust-no.
end.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CUSTOMER"
    no-lock no-error.

if not avail sys-ctrl then DO TRANSACTION:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CUSTOMER"
   sys-ctrl.descrip  =
          "Security? HoldDays,CrLmt,OrdLmt,Inv/PO,Terms,FinChg,Disc%,Markup%?"
   sys-ctrl.log-fld  = no
   v-flag            = no
   v-prompt          = NO
   sys-ctrl.log-fld  = v-prompt
   sys-ctrl.char-fld = "".
   
  do i = 1 to 8:
    sys-ctrl.char-fld = sys-ctrl.char-fld + string(v-flag[i],"Y/N").
  end. 
end.
assign
 v-secur     = sys-ctrl.log-fld
 v-secur-fld = sys-ctrl.char-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CUSTSIZE"
    no-lock no-error.

if not avail sys-ctrl then DO TRANSACTION:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CUSTSIZE"
   sys-ctrl.descrip  = "Customer number's length"         
   sys-ctrl.log-fld  = no
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
   sys-ctrl.int-fld = 8.
END.
ASSIGN v-custsize = sys-ctrl.char-fld
       v-cust-length = sys-ctrl.int-fld.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-cust D-Dialog 
PROCEDURE assign-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CREATE shipto.
  ASSIGN
    shipto.company = cust.company
    shipto.cust-no = cust.cust-no
    shipto.ship-addr[1] = cust.addr[1]
    shipto.ship-addr[2] = cust.addr[2]
    shipto.ship-city = cust.city
    shipto.ship-id = cust.cust-no
    shipto.ship-name = cust.name
    shipto.ship-no = 1
    shipto.ship-state = cust.state
    shipto.ship-zip = cust.zip
    shipto.carrier = cust.carrier
    shipto.dest-code = cust.del-zone
    shipto.loc = cust.loc
    shipto.tax-code = cust.tax-gr.
  CREATE soldto.
  ASSIGN
    soldto.company = cust.company
    soldto.cust-no = cust.cust-no
    soldto.sold-addr[1] = cust.addr[1]
    soldto.sold-addr[2] = cust.addr[2]
    soldto.sold-city = cust.city
    soldto.sold-id = cust.cust-no
    soldto.sold-name = cust.name
    soldto.sold-no = 1
    soldto.sold-state = cust.state
    soldto.sold-zip = cust.zip.

 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cust-zip D-Dialog 
PROCEDURE cust-zip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
    IF cust.zip:SCREEN-VALUE NE "" THEN
    FIND FIRST nosweat.zipcode
        WHERE nosweat.zipcode.zipcode EQ cust.zip:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL nosweat.zipcode THEN do:
      cust.state:SCREEN-VALUE = nosweat.zipcode.state.
      IF cust.city:SCREEN-VALUE EQ "" THEN
        cust.city:SCREEN-VALUE = nosweat.zipcode.city.
    END.
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-field D-Dialog 
PROCEDURE display-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*&IF "{&DISPLAY-FIELD}" NE "" &THEN */
&IF "{&list-4}" NE "" &THEN 
  DEFINE INPUT PARAMETER widget-name AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    CASE widget-name:
       /* {methods/dispflds/cust.i} */
         {est/cust-dis.i}
/*      {custom/dispflds.i} */
    END CASE.
    
  END.
&ENDIF

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
  DISPLAY terms_dscr stax_tax-dscr custype_dscr sman_sname loc_dscr carrier_dscr 
          carr-mtx_del-dscr terr_dscr 
      WITH FRAME D-Dialog.
  IF AVAILABLE cust THEN 
    DISPLAY cust.cust-no cust.active cust.name cust.addr[1] cust.addr[2] cust.city 
          cust.state cust.zip cust.terms cust.cr-use cust.cr-rating cust.cr-lim 
          cust.ord-lim cust.disc cust.curr-code cust.cr-hold-invdays 
          cust.cust-level cust.cr-hold cust.fin-chg cust.auto-reprice 
          cust.inv-meth cust.an-edi-cust cust.sort cust.tax-gr cust.tax-id 
          cust.date-field[1] cust.type cust.contact cust.sman cust.area-code 
          cust.phone cust.fax cust.email cust.frt-pay cust.fob-code 
          cust.ship-part cust.loc cust.carrier cust.del-zone cust.terr 
          cust.under-pct cust.over-pct cust.markup cust.ship-days cust.pallet 
          cust.case-bundle cust.int-field[1] 
      WITH FRAME D-Dialog.
  ENABLE cust.active cust.name cust.addr[1] cust.addr[2] cust.city cust.state 
         cust.zip cust.terms cust.cr-use cust.cr-rating cust.cr-lim 
         cust.ord-lim cust.disc cust.curr-code cust.cr-hold-invdays 
         cust.cust-level cust.cr-hold cust.fin-chg cust.auto-reprice 
         cust.inv-meth cust.an-edi-cust cust.sort cust.tax-gr cust.tax-id 
         cust.date-field[1] cust.type cust.contact cust.sman cust.area-code 
         cust.phone cust.fax cust.email cust.frt-pay cust.fob-code 
         cust.ship-part cust.loc cust.carrier cust.del-zone cust.terr 
         cust.under-pct cust.over-pct cust.markup cust.ship-days cust.pallet 
         cust.case-bundle cust.int-field[1] Btn_OK Btn_Cancel RECT-2 RECT-26 
         RECT-3 RECT-4 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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

  
  case cust.active :
        when "A" then cust.active:screen-value in frame {&frame-name} = "(A)ctive".
        when "I" then cust.active:screen-value in frame {&frame-name} = "(I)nactive".
        when "X" then cust.active:screen-value in frame {&frame-name} = "(X) Inhouse".
        when "S" then cust.active:screen-value in frame {&frame-name} = "(S)tatement".     
  end case.

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "cust"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cr-hold D-Dialog 
PROCEDURE valid-cr-hold :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
  v-valid = YES.

  DO WITH FRAME {&frame-name}:    
    IF NOT adm-new-record                                         AND
       STRING(cust.cr-hold,"yes/no") NE cust.cr-hold:SCREEN-VALUE AND
       NOT ll-secure                                              THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                   = NO
         cust.cr-hold:SCREEN-VALUE = STRING(cust.cr-hold,"yes/no").
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.cr-hold.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cr-hold-invdays D-Dialog 
PROCEDURE valid-cr-hold-invdays :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
  v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF v-secur                                                        AND
       SUBSTR(v-secur-fld,1,1) EQ "Y"                                 AND
       NOT adm-new-record                                             AND
       cust.cr-hold-invdays NE DEC(cust.cr-hold-invdays:SCREEN-VALUE) AND
       NOT ll-secure                                                  THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                           = NO
         cust.cr-hold-invdays:SCREEN-VALUE = STRING(cust.cr-hold-invdays).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.cr-hold-invdays.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cr-lim D-Dialog 
PROCEDURE valid-cr-lim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF v-secur                                      AND
       SUBSTR(v-secur-fld,2,1) EQ "Y"               AND
       NOT adm-new-record                           AND
       cust.cr-lim NE DEC(cust.cr-lim:SCREEN-VALUE) AND
       NOT ll-secure                                THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
      IF NOT ll-secure THEN
        ASSIGN
         v-valid                  = NO
         cust.cr-lim:SCREEN-VALUE = STRING(cust.cr-lim).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.cr-lim.
  END.

  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-disc D-Dialog 
PROCEDURE valid-disc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF v-secur                                  AND
       SUBSTR(v-secur-fld,6,1) EQ "Y"           AND
       NOT adm-new-record                       AND
       cust.disc NE DEC(cust.disc:SCREEN-VALUE) AND
       NOT ll-secure                            THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
      IF NOT ll-secure THEN
        ASSIGN
         v-valid                = NO
         cust.disc:SCREEN-VALUE = STRING(cust.disc).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.disc.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fin-chg D-Dialog 
PROCEDURE valid-fin-chg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF v-secur                                                    AND
       SUBSTR(v-secur-fld,7,1) EQ "Y"                             AND
       STRING(cust.fin-chg,"yes/no") NE cust.fin-chg:SCREEN-VALUE AND
       NOT ll-secure                                              THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
      IF NOT ll-secure THEN
        ASSIGN
         v-valid                   = NO
         cust.fin-chg:SCREEN-VALUE = STRING(cust.fin-chg,"yes/no").
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.fin-chg.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-meth D-Dialog 
PROCEDURE valid-inv-meth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF v-secur                                                      AND
       SUBSTR(v-secur-fld,4,1) EQ "Y"                               AND
       NOT adm-new-record                                           AND
       STRING(cust.inv-meth,"yes/no") NE cust.inv-meth:SCREEN-VALUE AND
       NOT ll-secure                                                THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
      IF NOT ll-secure THEN
        ASSIGN
         v-valid                    = NO
         cust.inv-meth:SCREEN-VALUE = STRING(cust.inv-meth,"yes/no").
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.inv-meth.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-markup D-Dialog 
PROCEDURE valid-markup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF v-secur                                      AND
       SUBSTR(v-secur-fld,8,1) EQ "Y"               AND
       NOT adm-new-record                           AND
       cust.markup NE DEC(cust.markup:SCREEN-VALUE) AND
       NOT ll-secure                                THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
      IF NOT ll-secure THEN
        ASSIGN
         v-valid                  = NO
         cust.markup:SCREEN-VALUE = STRING(cust.markup).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.markup.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-lim D-Dialog 
PROCEDURE valid-ord-lim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF v-secur                                        AND
       SUBSTR(v-secur-fld,3,1) EQ "Y"                 AND
       NOT adm-new-record                             AND
       cust.ord-lim NE DEC(cust.ord-lim:SCREEN-VALUE) AND
       NOT ll-secure                                  THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
      IF NOT ll-secure THEN
        ASSIGN
         v-valid                   = NO
         cust.ord-lim:SCREEN-VALUE = STRING(cust.ord-lim).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.ord-lim.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-terms D-Dialog 
PROCEDURE valid-terms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF v-secur                               AND
       SUBSTR(v-secur-fld,5,1) EQ "Y"        AND
       NOT adm-new-record                    AND
       cust.terms NE cust.terms:SCREEN-VALUE AND
       NOT ll-secure                         THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
      IF NOT ll-secure THEN
         ASSIGN
         v-valid                   = NO
         cust.terms:SCREEN-VALUE = cust.terms.
    END. 
   
    IF v-valid                                                                 AND
       NOT CAN-FIND(FIRST terms WHERE terms.t-code EQ cust.terms:SCREEN-VALUE) THEN DO:

      MESSAGE "Invalid Terms. Try Help." VIEW-AS ALERT-BOX ERROR.
      v-valid = NO.
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.terms.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-cust D-Dialog 
PROCEDURE validate-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 do with frame {&frame-name}:
     if cust.sman:screen-value <> "" and
        not can-find(first sman where sman.sman = cust.sman:screen-value)
     then do:
        message "Invalid Sales Rep. Try Help." view-as alert-box error.
        apply "entry" to cust.sman.
        return error.
     end.
     if cust.terms:screen-value <> "" and
        not can-find(first terms where terms.t-code = cust.terms:screen-value)
     then do:
        message "Invalid Terms. Try Help." view-as alert-box error.
        apply "entry" to cust.terms.
        return error.     
     end.
     if cust.pallet:screen-value <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "D" and
                                      item.i-no = cust.pallet:screen-value)
     then do:
        message "Invalid Pallet Code. Try Help." view-as alert-box error.
        apply "entry" to cust.pallet.
        return error.     
     end.
     if cust.terr:screen-value <> "" and
        not can-find(first terr where terr.company = gcompany and
                                      terr.terr = cust.terr:screen-value)
     then do:
        message "Invalid Territory Code. Try Help." view-as alert-box error.
        apply "entry" to cust.terr.
        return error.     
     end.
     if cust.case-bundle:screen-value <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "C" and
                                      item.i-no = cust.pallet:screen-value)
     then do:
        message "Invalid Case/Bundle Code. Try Help." view-as alert-box error.
        apply "entry" to cust.case-bundle.
        return error.     
     end.
     if cust.loc:screen-value <> "" and
        not can-find(first loc where loc.company = gcompany and 
                                     loc.loc = cust.loc:screen-value)
     then do:
        message "Invalid Ord. Loc. Code. Try Help." view-as alert-box error.
        apply "entry" to cust.loc.
        return error.     
     end.
     if cust.carrier:screen-value <> "" and
        not can-find(first carrier where carrier.company = gcompany and 
                                     carrier.loc = gloc and
                                     carrier.carrier = cust.carrier:screen-value)
     then do:
        message "Invalid Carrier Code. Try Help." view-as alert-box error.
        apply "entry" to cust.carrier.
        return error.     
     end.
     if cust.del-zone:screen-value <> "" and
        not can-find(first carr-mtx where carr-mtx.company = gcompany and 
                                     carr-mtx.loc = gloc and
                                     carr-mtx.carrier = cust.carrier:screen-value and
                                     carr-mtx.del-zone = cust.del-zone:screen-value)
     then do:
        message "Invalid Delivey Zone. Try Help." view-as alert-box error.
        apply "entry" to cust.del-zone.
        return error.     
     end.
     if cust.sort:screen-value = "Y" and 
        (
         not can-find(first stax where stax.tax-group begins gcompany and
                                   substring(stax.tax-group,1,10) = gcompany and
                                   substring(stax.tax-group,11,length(trim(stax.tax-group)) - 10) = cust.tax-gr:screen-value )
         or
         cust.tax-gr:screen-value = ""                    
        )    
     then do:
        message "Invalid Tax Code. Try Help." view-as alert-box error.
        apply "entry" to cust.tax-gr.
        return error.
     end.                                     
     if cust.state:screen-value <> "" and
        not can-find(first state where state.state = cust.state:screen-value )
     then do:
       message "Invalid State Code. Try Help." view-as alert-box error.
       apply "entry" to cust.state.
       return error.
     end.                                     
 

  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

