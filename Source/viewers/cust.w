&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/cust.w

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
{methods/defines/cust.i &NEW="NEW"}

DEF VAR ll-secure AS LOG NO-UNDO.

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
DEF VAR v-inv-fmt AS CHAR NO-UNDO.
DEF VAR v-cust-fmt AS CHAR NO-UNDO.
DEF VAR v-cust-log AS LOGICAL NO-UNDO.
DEFINE VARIABLE cAccount AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShift   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRouting AS INTEGER NO-UNDO.
DEFINE VARIABLE lCheckMessage AS LOGICAL NO-UNDO.
DEFINE VARIABLE lQuotePriceMatrix AS LOGICAL NO-UNDO.
DEFINE VARIABLE cRtnChar          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound         AS LOGICAL NO-UNDO.
DEFINE VARIABLE lInterCompanyBilling     AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdCustomerProcs          AS HANDLE  NO-UNDO.

/* gdm - 05050903 */
DEF BUFFER bf-cust FOR cust.

/* gdm - 08240904 */
DEF VAR v-inv-fmt2 AS CHAR NO-UNDO.

DO :
   {sys/inc/custpass.i}

    find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "INVPRINT"
      no-lock no-error.
    if not avail sys-ctrl then
    do:
      create sys-ctrl.
      assign
       sys-ctrl.company = g_company
       sys-ctrl.name    = "INVPRINT"
       sys-ctrl.descrip = "Print Invoice Headers on Invoice Form?".
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
    end.
    v-inv-fmt  = sys-ctrl.char-fld.

    /* gdm - 08240904 */
    ASSIGN v-inv-fmt2 = v-inv-fmt
           v-inv-fmt  = IF v-inv-fmt = "Hughes2" 
                          THEN  "Hughes" 
                          ELSE v-inv-fmt.
     /* gdm - 08240904 end */
  /* gdm - 12221003 */
   find first sys-ctrl
           where sys-ctrl.company eq g_company
             and sys-ctrl.name    eq "CustXfer"
           no-lock no-error.
         if not avail sys-ctrl then
         do:
           create sys-ctrl.
           assign
            sys-ctrl.company = g_company
            sys-ctrl.name    = "CustXfer"
            sys-ctrl.descrip = "Transfer Customer Information to Sister Plants?".
         end.
         ASSIGN
             v-cust-log = sys-ctrl.log-fld 
             v-cust-fmt = sys-ctrl.char-fld.
   /* gdm - 12221003 end */

END.

/* gdm - 11190903 */
DEF VAR v-zipflg AS LOG NO-UNDO.

DEFINE VARIABLE hdSalesManProcs AS HANDLE    NO-UNDO.

RUN salrep/SalesManProcs.p PERSISTENT SET hdSalesManProcs.

 RUN sys/ref/nk1look.p (INPUT g_company, "QuotePriceMatrix", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lQuotePriceMatrix = logical(cRtnChar) NO-ERROR. 
    
RUN system/CustomerProcs.p PERSISTENT SET hdCustomerProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES cust
&Scoped-define FIRST-EXTERNAL-TABLE cust


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cust.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cust.pricingMethod cust.active cust.name ~
cust.addr[1] cust.addr[2] cust.spare-char-3 cust.city cust.state cust.zip ~
cust.fax-country cust.spare-char-2 cust.type cust.date-field[1] ~
cust.contact cust.sman cust.area-code cust.phone cust.fax-prefix ~
cust.accountant cust.csrUser_id cust.scomm cust.terms cust.cr-use ~
cust.cr-hold-invdays cust.cr-hold-invdue cust.cr-rating cust.cust-level ~
cust.cr-lim cust.ord-lim cust.disc cust.curr-code cust.cr-hold cust.fin-chg ~
cust.auto-reprice cust.an-edi-cust cust.factored cust.sort cust.tax-gr ~
cust.tax-id cust.date-field[2] cust.frt-pay cust.fob-code cust.ship-part ~
cust.loc cust.carrier cust.del-zone cust.terr cust.under-pct cust.over-pct ~
cust.markup cust.ship-days cust.manf-day cust.classID cust.spare-int-1 ~
cust.pallet cust.case-bundle cust.int-field[1] cust.po-mandatory ~
cust.imported cust.show-set cust.nationalAcct cust.log-field[1] ~
cust.tagStatus cust.internal cust.emailPreference
&Scoped-define ENABLED-TABLES cust
&Scoped-define FIRST-ENABLED-TABLE cust
&Scoped-Define ENABLED-OBJECTS btn_bank-info RECT-5 RECT-6 
&Scoped-Define DISPLAYED-FIELDS cust.pricingMethod cust.cust-no cust.active ~
cust.name cust.addr[1] cust.addr[2] cust.spare-char-3 cust.city cust.state ~
cust.zip cust.fax-country cust.spare-char-2 cust.type cust.date-field[1] ~
cust.contact cust.sman cust.area-code cust.phone cust.fax-prefix ~
cust.accountant cust.csrUser_id cust.scomm cust.terms cust.cr-use ~
cust.cr-hold-invdays cust.cr-hold-invdue cust.cr-rating cust.cust-level ~
cust.cr-lim cust.ord-lim cust.disc cust.curr-code cust.cr-hold cust.fin-chg ~
cust.auto-reprice cust.an-edi-cust cust.factored cust.sort cust.tax-gr ~
cust.tax-id cust.date-field[2] cust.frt-pay cust.fob-code cust.ship-part ~
cust.loc cust.carrier cust.del-zone cust.terr cust.under-pct cust.over-pct ~
cust.markup cust.ship-days cust.manf-day cust.classID cust.spare-int-1 ~
cust.pallet cust.case-bundle cust.int-field[1] cust.po-mandatory ~
cust.imported cust.show-set cust.nationalAcct cust.log-field[1] ~
cust.tagStatus cust.internal cust.emailPreference
&Scoped-define DISPLAYED-TABLES cust
&Scoped-define FIRST-DISPLAYED-TABLE cust
&Scoped-Define DISPLAYED-OBJECTS cbMatrixPrecision cbMatrixRounding ~
fl_custemail custype_dscr faxAreaCode faxNumber sman_sname fi_flat-comm ~
terms_dscr rd_inv-meth loc_dscr carrier_dscr carr-mtx_del-dscr terr_dscr ~
stax_tax-dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,faxFields,F1 */
&Scoped-define ADM-CREATE-FIELDS cust.cust-no 
&Scoped-define ADM-ASSIGN-FIELDS fl_custemail fi_flat-comm rd_inv-meth ~
cust.po-mandatory cust.imported cust.show-set 
&Scoped-define DISPLAY-FIELD cust.state cust.type fl_custemail cust.sman ~
cust.csrUser_id cust.terms cust.tax-gr cust.loc cust.carrier cust.del-zone ~
cust.terr cust.po-mandatory cust.imported cust.show-set 
&Scoped-define faxFields faxAreaCode faxNumber 

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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnTags 
     IMAGE-UP FILE "Graphics/16x16/question.png":U
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Show Details".

DEFINE BUTTON btn_bank-info 
     LABEL "Bank Info" 
     SIZE 16.4 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE cbMatrixPrecision AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Matrix Precision" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "         0","         1","         2","         3","         4","         5","         6" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE cbMatrixRounding AS CHARACTER FORMAT "X(256)":U 
     LABEL "Matrix Rounding" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Normal Round","N",
                     "Round Up","U",
                     "Round Down","D",
                     "<None>","None"
     DROP-DOWN-LIST
     SIZE 18 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE carr-mtx_del-dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE carrier_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE custype_dscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE faxAreaCode AS CHARACTER FORMAT "(xxx)":U 
     LABEL "Fax #" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE faxNumber AS CHARACTER FORMAT "xxx-xxxx":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fi_flat-comm AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Flat Comm%" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fl_custemail AS CHARACTER FORMAT "X(60)":U 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE loc_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE sman_sname AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE stax_tax-dscr AS CHARACTER FORMAT "x(25)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE terms_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE terr_dscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE rd_inv-meth AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "BOL", no,
"PO", yes,
"User Select", ?
     SIZE 37 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 71 BY 8.33.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 71 BY 3.57.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80.4 BY 11.91.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 71 BY 2.57.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80 BY 2.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cust.pricingMethod AT ROW 21.48 COL 94 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS " ","Type","Customer","Ship To" 
          DROP-DOWN-LIST
          SIZE 20 BY 1
     cbMatrixPrecision AT ROW 20.29 COL 20 COLON-ALIGNED WIDGET-ID 38
     cbMatrixRounding AT ROW 21.48 COL 20 COLON-ALIGNED WIDGET-ID 40
     btnTags AT ROW 11.57 COL 64 WIDGET-ID 26
     cust.cust-no AT ROW 1 COL 12 COLON-ALIGNED
          LABEL "Customer"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     cust.active AT ROW 1 COL 43 COLON-ALIGNED
          LABEL "Status" FORMAT "x(11)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "(A)ctive","(I)nactive","(X) Inhouse","(E)-Service" 
          DROP-DOWN-LIST
          SIZE 19 BY 1 TOOLTIP "Active, Inactive, Inhouse, Statement"
          BGCOLOR 15 
     cust.name AT ROW 1.95 COL 12 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
          BGCOLOR 15 FONT 4
     cust.addr[1] AT ROW 2.95 COL 12 COLON-ALIGNED
          LABEL "Address" FORMAT "x(45)"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
          BGCOLOR 15 FONT 4
     cust.addr[2] AT ROW 3.86 COL 12 COLON-ALIGNED NO-LABEL FORMAT "x(45)"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
          BGCOLOR 15 FONT 4
     cust.spare-char-3 AT ROW 4.81 COL 12 COLON-ALIGNED NO-LABEL FORMAT "x(45)"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
          BGCOLOR 15 FONT 4
     cust.city AT ROW 5.76 COL 12 COLON-ALIGNED FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
          BGCOLOR 15 FONT 4
     cust.state AT ROW 5.76 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     cust.zip AT ROW 5.76 COL 46 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FONT 4
     cust.fax-country AT ROW 6.71 COL 12 COLON-ALIGNED
          LABEL "Country"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 
     cust.spare-char-2 AT ROW 1 COL 79.4 COLON-ALIGNED WIDGET-ID 14
          LABEL "Group" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17.2 BY 1
          BGCOLOR 15 FONT 4
     cust.type AT ROW 1.95 COL 79.4 COLON-ALIGNED
          LABEL "Type"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust.date-field[1] AT ROW 1 COL 134.4 COLON-ALIGNED
          LABEL "Date Added" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4 NO-TAB-STOP 
     cust.contact AT ROW 2.91 COL 79.4 COLON-ALIGNED
          LABEL "Contact"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
          BGCOLOR 15 FONT 4
     fl_custemail AT ROW 3.86 COL 79.4 COLON-ALIGNED WIDGET-ID 2
     cust.sman AT ROW 4.81 COL 79.4 COLON-ALIGNED
          LABEL "Sales Grp"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     custype_dscr AT ROW 1.95 COL 96.4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     cust.area-code AT ROW 5.81 COL 79.4 COLON-ALIGNED AUTO-RETURN 
          LABEL "Phone#" FORMAT "(xxx)"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cust.phone AT ROW 5.81 COL 87.4 COLON-ALIGNED NO-LABEL FORMAT "xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 4
     faxAreaCode AT ROW 6.71 COL 79.4 COLON-ALIGNED AUTO-RETURN 
     faxNumber AT ROW 6.71 COL 87.4 COLON-ALIGNED NO-LABEL
     cust.fax-prefix AT ROW 6.71 COL 112.4 COLON-ALIGNED
          LABEL "Prefix"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 
     cust.accountant AT ROW 6.71 COL 136.8 COLON-ALIGNED WIDGET-ID 18
          LABEL "Accountant"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
          BGCOLOR 15 
     cust.csrUser_id AT ROW 2.67 COL 134.4 COLON-ALIGNED
          LABEL "CSR"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     btn_bank-info AT ROW 3.62 COL 136.4
     sman_sname AT ROW 4.81 COL 87.4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi_flat-comm AT ROW 4.71 COL 141.4 COLON-ALIGNED
     cust.scomm AT ROW 5.71 COL 141.4 COLON-ALIGNED HELP
          "Enter Salesman Flat Commission Percentage" WIDGET-ID 8
          LABEL "Broker Comm%"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     cust.terms AT ROW 8.62 COL 17 COLON-ALIGNED
          LABEL "Terms"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FONT 4
     cust.cr-use AT ROW 9.57 COL 17 COLON-ALIGNED
          LABEL "Cr. Acct #"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     cust.cr-hold-invdays AT ROW 9.57 COL 46 COLON-ALIGNED
          LABEL "Grace Days"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     cust.cr-hold-invdue AT ROW 9.57 COL 55 COLON-ALIGNED
          LABEL "$"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 1
     cust.cr-rating AT ROW 10.52 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FONT 4
     cust.cust-level AT ROW 10.52 COL 56 COLON-ALIGNED
          LABEL "Price Level"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     terms_dscr AT ROW 8.62 COL 27 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     cust.cr-lim AT ROW 11.48 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 4
     cust.ord-lim AT ROW 12.43 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 4
     cust.disc AT ROW 13.38 COL 17 COLON-ALIGNED
          LABEL "Discount %"
          VIEW-AS FILL-IN 
          SIZE 10.2 BY 1
          BGCOLOR 15 FONT 4
     cust.curr-code AT ROW 14.33 COL 17 COLON-ALIGNED
          LABEL "Currency"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     rd_inv-meth AT ROW 15.43 COL 19 NO-LABEL
     cust.cr-hold AT ROW 11.71 COL 47
          LABEL "Credit Hold"
     cust.internal AT ROW 7.1 COL 47
          LABEL "Internal"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .81
     cust.fin-chg AT ROW 12.43 COL 47
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cust.auto-reprice AT ROW 13.24 COL 47
          VIEW-AS TOGGLE-BOX
          SIZE 23.2 BY .81
     cust.an-edi-cust AT ROW 13.95 COL 47
          LABEL "EDI"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
     cust.factored AT ROW 14.67 COL 47
          LABEL "Factored"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     cust.sort AT ROW 16.95 COL 19 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Yes", "Y":U,
"No", "N":U
          SIZE 20 BY .62
     cust.tax-gr AT ROW 17.86 COL 16 COLON-ALIGNED
          LABEL "Tax Group"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     loc_dscr AT ROW 10.52 COL 106.4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     cust.tax-id AT ROW 18.81 COL 16 COLON-ALIGNED
          LABEL "Tax Resale#"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 FONT 4
     cust.date-field[2] AT ROW 18.81 COL 52.2 COLON-ALIGNED
          LABEL "Exp."
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     cust.frt-pay AT ROW 8.62 COL 100.4 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Bill", "B":U,
"Collect", "C":U,
"Prepaid", "P":U,
"3rd Party", "T":U
          SIZE 49 BY .81
     cust.fob-code AT ROW 9.57 COL 100.4 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Destination", "DEST":U,
"Origin", "ORIG":U
          SIZE 28 BY .81
     cust.ship-part AT ROW 9.57 COL 132.4
          LABEL "Partial Ship"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .81
     carrier_dscr AT ROW 11.48 COL 106.4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     cust.loc AT ROW 10.52 COL 94.4 COLON-ALIGNED
          LABEL "Location"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 FONT 4
     cust.carrier AT ROW 11.48 COL 94.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust.del-zone AT ROW 12.48 COL 94.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust.terr AT ROW 13.43 COL 94.4 COLON-ALIGNED
          LABEL "Territory"
          VIEW-AS FILL-IN 
          SIZE 5.8 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx_del-dscr AT ROW 12.48 COL 106.4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     cust.under-pct AT ROW 14.33 COL 94.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     cust.over-pct AT ROW 15.29 COL 94.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     cust.markup AT ROW 16.24 COL 94.4 COLON-ALIGNED FORMAT "->9.99"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 
     cust.ship-days AT ROW 17.19 COL 94.4 COLON-ALIGNED
          LABEL "Whse Days"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     cust.manf-day AT ROW 18.14 COL 94.4 COLON-ALIGNED HELP
          "" WIDGET-ID 6
          LABEL "Pallet Positions" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cust.classID AT ROW 18.95 COL 94.4 COLON-ALIGNED
          LABEL "AR ClassID" FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 FONT 4
     cust.emailPreference AT ROW 18.95 COL 135 COLON-ALIGNED
          LABEL "Email"
          VIEW-AS COMBO-BOX INNER-LINES 3
          LIST-ITEM-PAIRS "Ask","0",
                          "Combined","1",
                          "Separate","2"
          DROP-DOWN-LIST
          SIZE 16 BY 1    
     cust.spare-int-1 AT ROW 13.43 COL 135 COLON-ALIGNED WIDGET-ID 12
          LABEL "Pallet ID" FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 12.2 BY .95
          BGCOLOR 15 FONT 4
     terr_dscr AT ROW 13.43 COL 100 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     cust.pallet AT ROW 14.33 COL 128.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
          BGCOLOR 15 FONT 4
     cust.case-bundle AT ROW 15.29 COL 128.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
          BGCOLOR 15 FONT 4
     cust.int-field[1] AT ROW 16.24 COL 133.4 COLON-ALIGNED
          LABEL "# of Labels per Skid" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
          BGCOLOR 15 FONT 4
     cust.po-mandatory AT ROW 17.43 COL 111.4
          LABEL "PO Req'd"
          VIEW-AS TOGGLE-BOX
          SIZE 14.4 BY .81
     cust.imported AT ROW 17.43 COL 129.4
          LABEL "Contract Pricing"
          VIEW-AS TOGGLE-BOX
          SIZE 22.4 BY .81
     cust.show-set AT ROW 18.14 COL 111.4
          LABEL "Show Sets"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .81
     cust.nationalAcct AT ROW 18.14 COL 129.4
          LABEL "National Account"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     cust.log-field[1] AT ROW 18.86 COL 111.4 HELP
          "" WIDGET-ID 16
          LABEL "Paperless"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .81
     stax_tax-dscr AT ROW 17.86 COL 28 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     cust.tagStatus AT ROW 20.29 COL 94 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 3
          LIST-ITEM-PAIRS "Only tags that are not on hold"," ",
                     "Only on Hold tags","H",
                     "Any tag status","A"
          DROP-DOWN-LIST
          SIZE 40 BY 1
     " Credit Information" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 7.91 COL 5
          FGCOLOR 9 FONT 4
     "Taxable:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 16.14 COL 11
     " Tax Information" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 16.24 COL 4
          FGCOLOR 9 FONT 4
     "Invoice Per:" VIEW-AS TEXT
          SIZE 14 BY .81 AT ROW 15.43 COL 4
     " Other Information" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 7.91 COL 78.4
          FGCOLOR 9 FONT 4
     "Freight Terms:" VIEW-AS TEXT
          SIZE 20 BY .71 AT ROW 8.62 COL 79.4
     "FOB:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 9.57 COL 90.4
     "Taxable:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 16.95 COL 7 WIDGET-ID 24
     "Price Matrix" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 19.81 COL 5.8 WIDGET-ID 30
          FGCOLOR 9 FONT 4
     RECT-2 AT ROW 8.14 COL 1
     RECT-3 AT ROW 16.48 COL 1
     RECT-4 AT ROW 8.14 COL 73
     RECT-5 AT ROW 20.05 COL 1 WIDGET-ID 28
     RECT-6 AT ROW 20.05 COL 73 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.cust
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
         HEIGHT             = 21.62
         WIDTH              = 152.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer4.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cust.accountant IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX cust.active IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.addr[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.addr[2] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR TOGGLE-BOX cust.an-edi-cust IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.area-code IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR BUTTON btnTags IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN carr-mtx_del-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.carrier IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN carrier_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbMatrixPrecision IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbMatrixRounding IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.city IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust.classID IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.contact IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX cust.cr-hold IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.cr-hold-invdays IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.cr-hold-invdue IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.cr-use IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.csrUser_id IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.curr-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.cust-level IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.cust-no IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN custype_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.date-field[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.date-field[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.del-zone IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.disc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX cust.factored IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.fax-country IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.fax-prefix IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN faxAreaCode IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN faxNumber IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi_flat-comm IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fl_custemail IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR TOGGLE-BOX cust.imported IN FRAME F-Main
   2 4 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN cust.int-field[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.loc IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN loc_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX cust.log-field[1] IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN cust.manf-day IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN cust.markup IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust.name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX cust.nationalAcct IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.phone IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR TOGGLE-BOX cust.po-mandatory IN FRAME F-Main
   2 4 EXP-LABEL                                                        */
/* SETTINGS FOR RADIO-SET rd_inv-meth IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RECT-3:PRIVATE-DATA IN FRAME F-Main     = 
                "Date".

/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.scomm IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN cust.ship-days IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX cust.ship-part IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX cust.show-set IN FRAME F-Main
   2 4 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN cust.sman IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN sman_sname IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.spare-char-2 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.spare-char-3 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.spare-int-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.state IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN stax_tax-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.tax-gr IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.tax-id IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.terms IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN terms_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.terr IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN terr_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.type IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.zip IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   def var lv-handle as handle no-undo.
   def var char-val as cha no-undo.
   DEFINE VARIABLE cFieldsValue AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cFoundValue AS CHARACTER NO-UNDO.
   DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

   CASE Focus:name :
     when "del-zone" then do:
           run windows/l-delzon.w 
              (gcompany,cust.loc:screen-value,cust.carrier:screen-value in frame {&frame-name},focus:screen-value in frame {&frame-name}, output char-val).
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
     WHEN "spare-char-2" THEN DO:
           RUN windows/l-usrgrp.w (INPUT "CUSTOMER GROUPS", OUTPUT char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
       END.
     when "case-bundle" then do:
           run windows/l-item.w 
              (gcompany,"","C",focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "terms" then do:
           run windows/l-terms.w 
               (gcompany,focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     terms_dscr:SCREEN-VALUE = ENTRY(2,char-val).
           return no-apply.  
     end.
     when "csrUser_id" then do:
         run windows/l-users.w (cust.csrUser_id:SCREEN-VALUE in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign cust.csrUser_id:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.
     end.
     when "accountant" then do:
         run windows/l-users.w (cust.accountant:SCREEN-VALUE in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign cust.accountant:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.
     end.
     when "classId" then do:
         RUN system/openLookup.p (
            INPUT  gcompany, 
            INPUT  "", /* Lookup ID */
            INPUT  110,  /* Subject ID */
            INPUT  "", /* User ID */
            INPUT  0,  /* Param Value ID */
            OUTPUT cFieldsValue, 
            OUTPUT cFoundValue, 
            OUTPUT recRecordID ).  
       
       IF cFoundValue NE "" THEN    
         cust.classId:SCREEN-VALUE IN FRAME {&frame-name} = cFoundValue.
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


&Scoped-define SELF-NAME cust.accountant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.accountant V-table-Win
ON LEAVE OF cust.accountant IN FRAME F-Main /* Accountant */
DO:
   DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
  IF LASTKEY <> -1 THEN DO:
     RUN valid-bill-owner(OUTPUT lReturnError) NO-ERROR.
     IF lReturnError THEN RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.active V-table-Win
ON return OF cust.active IN FRAME F-Main /* Status */
DO:
  apply "tab" to self.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.active V-table-Win
ON VALUE-CHANGED OF cust.active IN FRAME F-Main /* Status */
DO:
    RUN check-cr-bal .
    IF NOT v-valid THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.an-edi-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.an-edi-cust V-table-Win
ON return OF cust.an-edi-cust IN FRAME F-Main /* EDI */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.auto-reprice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.auto-reprice V-table-Win
ON return OF cust.auto-reprice IN FRAME F-Main /* Auto Reprice */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTags
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTags V-table-Win
ON CHOOSE OF btnTags IN FRAME F-Main
DO:
    RUN system/d-TagViewer.w(
        INPUT cust.rec_key,
        INPUT "HOLD",
        INPUT ""
        ). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_bank-info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_bank-info V-table-Win
ON CHOOSE OF btn_bank-info IN FRAME F-Main /* Bank Info */
DO:
   IF AVAIL cust THEN
    ASSIGN cAccount = cust.bank-acct
           cShift   = cust.SwiftBIC
           cRouting = cust.Bank-RTN .

    IF cust.NAME:SENSITIVE IN FRAME {&FRAME-NAME} EQ NO THEN
     RUN custom/d-bankinfo.w ("View" ,INPUT-OUTPUT cAccount, INPUT-OUTPUT cShift, INPUT-OUTPUT cRouting).
    ELSE
     RUN custom/d-bankinfo.w ("update" ,INPUT-OUTPUT cAccount, INPUT-OUTPUT cShift, INPUT-OUTPUT cRouting).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.carrier V-table-Win
ON ENTRY OF cust.carrier IN FRAME F-Main /* Carrier */
DO:
  s-loc = cust.loc:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.carrier V-table-Win
ON LEAVE OF cust.carrier IN FRAME F-Main /* Carrier */
DO:
    IF LASTKEY NE -1 THEN DO:
        RUN valid-carrier NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  {methods/dispflds.i}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.case-bundle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.case-bundle V-table-Win
ON LEAVE OF cust.case-bundle IN FRAME F-Main /* Case/Bundle */
DO:
     {&methods/lValidateError.i YES}
     if lastkey <> -1 and cust.case-bundle:screen-value <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "C" and
                                      item.i-no = cust.case-bundle:screen-value)
     then do:
        message "Invalid Case/Bundle Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.
     {&methods/lValidateError.i NO}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbMatrixPrecision
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbMatrixPrecision V-table-Win
ON VALUE-CHANGED OF cbMatrixPrecision IN FRAME F-Main /* Matrix Precision */
DO:
    ASSIGN 
        {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbMatrixRounding
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbMatrixRounding V-table-Win
ON VALUE-CHANGED OF cbMatrixRounding IN FRAME F-Main /* Matrix Rounding */
DO:
    ASSIGN 
        {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.city
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.city V-table-Win
ON HELP OF cust.city IN FRAME F-Main /* City */
DO:
  RUN applhelp.p.
  {&self-name}:SCREEN-VALUE = g_lookup-var.
  RUN cust-city.  

  IF cust.zip:SCREEN-VALUE NE cust.zip THEN
     RUN zip-carrier.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.city V-table-Win
ON LEAVE OF cust.city IN FRAME F-Main /* City */
DO:
  IF LASTKEY NE -1 THEN
  DO:
     RUN cust-city.     
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.classID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.classID V-table-Win
ON LEAVE OF cust.classID IN FRAME F-Main /* AR ClassID */
DO:
  
  IF LASTKEY <> -1 THEN DO:
     RUN valid-ClassId NO-ERROR.
     IF NOT v-valid THEN RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.classID V-table-Win
ON VALUE-CHANGED OF cust.classID IN FRAME F-Main /* AR ClassID */
DO:
  lCheckMessage = NO .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cr-hold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cr-hold V-table-Win
ON LEAVE OF cust.cr-hold IN FRAME F-Main /* Credit Hold */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cr-hold.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cr-hold V-table-Win
ON return OF cust.cr-hold IN FRAME F-Main /* Credit Hold */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cr-hold-invdays
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cr-hold-invdays V-table-Win
ON LEAVE OF cust.cr-hold-invdays IN FRAME F-Main /* Grace Days */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cr-hold-invdays.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cr-lim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cr-lim V-table-Win
ON LEAVE OF cust.cr-lim IN FRAME F-Main /* Credit Limit */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cr-lim.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.csrUser_id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.csrUser_id V-table-Win
ON LEAVE OF cust.csrUser_id IN FRAME F-Main /* CSR */
DO:
  
  IF LASTKEY <> -1 THEN DO:
     RUN valid-custcsr NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.curr-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.curr-code V-table-Win
ON LEAVE OF cust.curr-code IN FRAME F-Main /* Currency */
DO:
   IF cust.curr-code:SCREEN-VALUE <> "" AND LASTKEY <> -1 THEN DO:
      RUN valid-currency NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cust-level
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cust-level V-table-Win
ON LEAVE OF cust.cust-level IN FRAME F-Main /* Price Level */
DO:
    {&methods/lValidateError.i YES}
    if lastkey <> -1 and 
       decimal(cust.cust-level:screen-value) > 10 then 
    do:
        message "Price level can not exceed 10." view-as alert-box error.
        return no-apply.
    end.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cust-no V-table-Win
ON LEAVE OF cust.cust-no IN FRAME F-Main /* Customer */
DO:
   {&methods/lValidateError.i YES}
   IF LASTKEY = -1 THEN RETURN.
   IF cust.cust-no:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Customer number must be entered." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO cust.cust-no.
       RETURN NO-APPLY.
    END.
   
   IF adm-new-record AND 
      CAN-FIND(FIRST cust WHERE cust.company = gcompany 
                            AND cust.cust-no = cust.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME})
   THEN DO:
      MESSAGE "Customer already exists. Try other number." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cust.cust-no.
      RETURN NO-APPLY.
   END.
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.del-zone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.del-zone V-table-Win
ON ENTRY OF cust.del-zone IN FRAME F-Main /* Delivery Zone */
DO:
  ASSIGN
    s-loc = cust.loc:SCREEN-VALUE
    s-carrier = cust.carrier:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.del-zone V-table-Win
ON LEAVE OF cust.del-zone IN FRAME F-Main /* Delivery Zone */
DO:
     {&methods/lValidateError.i YES}
     if lastkey <> -1 and /*cust.del-zone:screen-value <> "" and*/
        not can-find(first carr-mtx where carr-mtx.company = gcompany and 
                                     carr-mtx.loc = cust.loc:screen-value and
                                     carr-mtx.carrier = cust.carrier:screen-value and
                                     carr-mtx.del-zone = cust.del-zone:screen-value)
     then do:
        message "Invalid Delivey Zone. Try Help." view-as alert-box error.
        return no-apply.     
     end.

  {methods/dispflds.i}
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.disc V-table-Win
ON LEAVE OF cust.disc IN FRAME F-Main /* Discount % */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-disc.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faxAreaCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faxAreaCode V-table-Win
ON LEAVE OF faxAreaCode IN FRAME F-Main /* Fax # */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faxNumber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faxNumber V-table-Win
ON LEAVE OF faxNumber IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.fin-chg V-table-Win
ON LEAVE OF cust.fin-chg IN FRAME F-Main /* Finance Charges */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-fin-chg.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.fin-chg V-table-Win
ON return OF cust.fin-chg IN FRAME F-Main /* Finance Charges */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fl_custemail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fl_custemail V-table-Win
ON LEAVE OF fl_custemail IN FRAME F-Main /* Email */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.fob-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.fob-code V-table-Win
ON return OF cust.fob-code IN FRAME F-Main /* FOB Code */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.frt-pay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.frt-pay V-table-Win
ON return OF cust.frt-pay IN FRAME F-Main /* FR PAY CD */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.loc V-table-Win
ON LEAVE OF cust.loc IN FRAME F-Main /* Location */
DO:
     {&methods/lValidateError.i YES}
     if lastkey <> -1 and cust.loc:screen-value <> "" and
        not can-find(first loc where loc.company = gcompany and 
                                     loc.loc = cust.loc:screen-value)
     then do:
        message "Invalid Ord. Loc. Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.

  {methods/dispflds.i}
  {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.markup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.markup V-table-Win
ON LEAVE OF cust.markup IN FRAME F-Main /* Mark-Up */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-markup.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.ord-lim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.ord-lim V-table-Win
ON LEAVE OF cust.ord-lim IN FRAME F-Main /* Order Limit */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ord-lim.
    IF NOT v-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.pallet V-table-Win
ON LEAVE OF cust.pallet IN FRAME F-Main /* Pallet */
DO:
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and cust.pallet:screen-value <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "D" and
                                      item.i-no = cust.pallet:screen-value)
     then do:
        message "Invalid Pallet Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.
     {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.ship-part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.ship-part V-table-Win
ON return OF cust.ship-part IN FRAME F-Main /* Partial Ship */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.sman V-table-Win
ON LEAVE OF cust.sman IN FRAME F-Main /* Sales Grp */
DO:
 IF LASTKEY = -1 THEN RETURN.
  RUN valid-sman NO-ERROR. 
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.sort V-table-Win
ON return OF cust.sort IN FRAME F-Main /* Taxable? */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.state V-table-Win
ON LEAVE OF cust.state IN FRAME F-Main /* State */
DO:
    {&methods/lValidateError.i YES}
    if lastkey <> -1 and cust.state:screen-value <> "" and
       not can-find(first statecod where statecod.statecod = cust.state:screen-value )
    then do:
       message "Invalid State Code. Try Help." view-as alert-box error.
       return no-apply.
    end.  
    IF cust.state:MODIFIED THEN
    DO:
         RUN pNewCountry.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.tax-gr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.tax-gr V-table-Win
ON LEAVE OF cust.tax-gr IN FRAME F-Main /* Tax Group */
DO:
  {&methods/lValidateError.i YES}
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
     message "Invalid Tax Group. Try Help." self:screen-value view-as alert-box error.
     return no-apply.
  end.                                     

  {methods/dispflds.i}
  {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.terms V-table-Win
ON LEAVE OF cust.terms IN FRAME F-Main /* Terms */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.terr V-table-Win
ON LEAVE OF cust.terr IN FRAME F-Main /* Territory */
DO:
    {&methods/lValidateError.i YES}
    if lastkey <> -1 and cust.terr:screen-value <> "" and
        not can-find(first terr where terr.company = gcompany and
                                      terr.terr = cust.terr:screen-value)
     then do:
        message "Invalid Territory Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.

  {methods/dispflds.i}
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.type V-table-Win
ON LEAVE OF cust.type IN FRAME F-Main /* Type */
DO:
  {methods/dispflds.i}
  IF LASTKEY <> -1 THEN DO:
     RUN valid-custtype NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.zip V-table-Win
ON HELP OF cust.zip IN FRAME F-Main /* Zip Code */
DO:
  /*RUN applhelp.p.
  {&self-name}:SCREEN-VALUE = g_lookup-var.
  RUN cust-zip.  */

  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR city-val AS cha NO-UNDO.
  DEF VAR state-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.

       RUN windows/l-zipcod.w (FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT city-val,OUTPUT state-val,OUTPUT rec-val).
       IF char-val NE "" THEN cust.zip:SCREEN-VALUE = ENTRY(1,char-val).
       IF city-val NE "" THEN cust.city:SCREEN-VALUE = ENTRY(1,city-val).
       IF state-val NE "" THEN cust.state:SCREEN-VALUE = ENTRY(1,state-val).


  IF cust.zip:SCREEN-VALUE NE cust.zip THEN
     RUN zip-carrier.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.zip V-table-Win
ON LEAVE OF cust.zip IN FRAME F-Main /* Zip Code */
DO:
  IF LASTKEY NE -1 THEN
  DO:
     RUN cust-zip.

     IF cust.zip:SCREEN-VALUE NE cust.zip THEN
        RUN zip-carrier.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.

DO :
     {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""AF1"" }
END.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CUSTOMER"
    no-lock no-error.

if not avail sys-ctrl then DO :
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

if not avail sys-ctrl then DO :
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

session:data-entry-return = yes.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "cust"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cust"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-cr-bal V-table-Win 
PROCEDURE check-cr-bal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cInvoiceList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOrderList AS CHARACTER NO-UNDO.
    {methods/lValidateError.i YES}
IF AVAIL cust AND cust.active:SCREEN-VALUE IN FRAME {&FRAME-NAME} BEGINS "(I)" 
  THEN do: 
    IF AVAIL cust AND cust.acc-bal GT 0 THEN DO:
      MESSAGE 
        "Customer " + cust.cust-no + " - " + cust.NAME 
        " has a non-zero Account Balance ." SKIP 
        "You can not make it Inactive. Please select another status."
       VIEW-AS ALERT-BOX ERROR.

        APPLY "entry" TO cust.active .
      RETURN ERROR.
    END.

    cInvoiceList = "".
    FOR EACH ar-inv NO-LOCK 
        WHERE ar-inv.company EQ cust.company
          AND ar-inv.cust-no EQ cust.cust-no 
          AND ar-inv.posted EQ NO BREAK BY ar-inv.inv-no :

        cInvoiceList = cInvoiceList + string(ar-inv.inv-no) + ",".
        IF LAST(ar-inv.inv-no) THEN DO:
            MESSAGE 
                "Customer " + cust.cust-no + " - " + cust.NAME 
                " has at least one Open Invoice ." SKIP 
                "You can not make it Inactive. Please select another status." SKIP
                "Inv List: " cInvoiceList
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cust.active .
            RETURN ERROR.
        END.
    END.

    cOrderList = "".
    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company EQ cust.company
          AND oe-ord.cust-no EQ cust.cust-no
          AND INDEX("CZ",oe-ord.stat) EQ 0 BREAK BY oe-ord.ord-no:

        cOrderList = cOrderList + STRING(oe-ord.ord-no) + ",".

        IF LAST(oe-ord.ord-no) THEN DO:
            MESSAGE "Customer " + cust.cust-no + " - " + cust.NAME 
                " has at least one Open Order ." SKIP 
                "You can not make it Inactive. Please select another status." SKIP
                "Order List: " cOrderList
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cust.active .
            RETURN ERROR.
        END.
    END.
END.


  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cust-city V-table-Win 
PROCEDURE cust-city :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF cust.city:SCREEN-VALUE NE "" THEN
    FIND FIRST zipcode
        WHERE zipcode.city EQ cust.city:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL zipcode THEN do:
      ASSIGN
        cust.state:SCREEN-VALUE = zipcode.state
        cust.zip:SCREEN-VALUE = zipcode.zipcode  .      
    END.   
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cust-new-log V-table-Win 
PROCEDURE cust-new-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR thisOne AS CHAR NO-UNDO.
 DEFINE BUFFER buff-cust FOR cust .
 DEFINE BUFFER buff-shipto FOR shipto .
 DEFINE BUFFER buff-soldto FOR soldto .
 FIND CURRENT cust NO-LOCK.
 DO I = 1 TO NUM-ENTRIES(v-cust-fmt):
     ASSIGN thisOne = ENTRY(i,v-cust-fmt).
     CREATE buff-cust .
     ASSIGN buff-cust.company = thisone.
     BUFFER-COPY cust EXCEPT company rec_key  TO buff-cust.
     for each shipto of cust NO-LOCK BY shipto.ship-no:
        create buff-shipto.
        ASSIGN buff-shipto.company = thisOne .
        buffer-copy shipto except shipto.company shipto.rec_key to buff-shipto.
     END.
     for each soldto of cust NO-LOCK BY soldto.sold-no:
        create buff-soldto.
        assign buff-soldto.company = thisOne .
        buffer-copy soldto except soldto.company soldto.rec_key to buff-soldto.
     END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cust-update-log V-table-Win 
PROCEDURE cust-update-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 FIND CURRENT cust NO-LOCK.
 DEF VAR thisOne AS CHAR NO-UNDO.
 DEFINE BUFFER buff-cust FOR cust .
 DEFINE BUFFER buff-shipto FOR shipto .
 DEFINE BUFFER buff-soldto FOR soldto .

   DO I = 1 TO NUM-ENTRIES(v-cust-fmt):
     ASSIGN thisOne = ENTRY(i,v-cust-fmt).
     FIND FIRST buff-cust WHERE buff-cust.cust-no = cust.cust-no 
                          AND buff-cust.company = thisOne EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL buff-cust THEN do:
     BUFFER-COPY cust EXCEPT cust-no company ytd-sales lyr-sales cost comm ytd-msf lyytd-msf hibal hibal-date num-inv 
         lpay lpay-date avg-pay ord-bal acc-bal on-account rec_key TO buff-cust.
     END.
     ELSE DO:
         CREATE buff-cust .
         ASSIGN buff-cust.company = thisone.
         BUFFER-COPY cust EXCEPT company ytd-sales lyr-sales cost comm ytd-msf lyytd-msf hibal hibal-date num-inv 
         lpay lpay-date avg-pay ord-bal acc-bal on-account rec_key TO buff-cust.
        for each shipto of cust NO-LOCK BY shipto.ship-no:
            create buff-shipto.
            ASSIGN buff-shipto.company = thisOne .
            buffer-copy shipto except shipto.company shipto.rec_key to buff-shipto.
         END.
         for each soldto of cust NO-LOCK BY soldto.sold-no:
             create buff-soldto.
             assign buff-soldto.company = thisOne .
             buffer-copy soldto except soldto.company soldto.rec_key to buff-soldto.
          END.
      END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cust-zip V-table-Win 
PROCEDURE cust-zip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF cust.zip:SCREEN-VALUE NE "" THEN
    FIND FIRST zipcode
        WHERE zipcode.zipcode EQ cust.zip:SCREEN-VALUE 
        AND (zipcode.city EQ cust.city:SCREEN-VALUE OR cust.city:SCREEN-VALUE = "" )
        NO-LOCK NO-ERROR.
    IF AVAIL zipcode THEN do:
      ASSIGN
        cust.state:SCREEN-VALUE = zipcode.state
        cust.city:SCREEN-VALUE = zipcode.city.    
    END.
    ELSE IF NOT AVAIL zipcode  THEN DO:
        FIND FIRST zipcode
            WHERE zipcode.zipcode EQ cust.zip:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
        IF AVAIL zipcode THEN do:
            ASSIGN
                cust.state:SCREEN-VALUE = zipcode.state
                cust.city:SCREEN-VALUE = zipcode.city.    
        END.
    END.
   /* gdm - 11190903 */
    ELSE 
    IF NOT AVAIL zipcode THEN ASSIGN v-zipflg = TRUE.   
   /* gdm - 11190903 end */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-fields V-table-Win 
PROCEDURE disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}.
        DISABLE 
            {&faxFields} 
            rd_inv-meth 
            fi_flat-comm  
            fl_custemail
            cbMatrixPrecision
            cbMatrixRounding
            .
    END.

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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-active V-table-Win 
PROCEDURE display-active :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    if not avail cust then return.

    case cust.active:
      when "A" then cust.active:screen-value in frame {&frame-name} = "(A)ctive".
      when "I" then cust.active:screen-value in frame {&frame-name} = "(I)nactive".
      when "X" then cust.active:screen-value in frame {&frame-name} = "(X) Inhouse".
      when "S" then cust.active:screen-value in frame {&frame-name} = "(S)tatement".     
      when "E" then cust.active:screen-value in frame {&frame-name} = "(E)-Service".     
    end case.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayTagStatus V-table-Win 
PROCEDURE pDisplayTagStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    if not avail cust then return.

    case cust.tagStatus:
      when "" THEN cust.tagStatus:screen-value in frame {&frame-name} = " ".
      when "H" THEN cust.tagStatus:screen-value in frame {&frame-name} = "H".
      when "A" then cust.tagStatus:screen-value in frame {&frame-name} = "A".           
    end case.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-cust-no V-table-Win 
PROCEDURE get-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER op-cust-no AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER op-cust-x AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
         op-cust-no = cust.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         op-cust-x  = cust.ACTIVE:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "(X) Inhouse".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-cust-recid-prev AS RECID NO-UNDO.
  DEF BUFFER bf-cust FOR cust.
  DEF BUFFER bf-shipto FOR shipto.
  DEF BUFFER bf-soldto FOR soldto.
  DEF BUFFER bf-usercust FOR usercust .
  DEF BUFFER bff-cust FOR cust.

  /* Code placed here will execute PRIOR to standard behavior. */
  v-cust-recid-prev = RECID(cust).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reftable-values (NO).

  {methods/viewers/assign/cust.i}
  assign
   cust.active   = substr(cust.active,2,1)
   cust.inv-meth = rd_inv-meth
   cust.email    = TRIM(fl_custemail)
   cust.matrixPrecision = cbMatrixPrecision
   cust.matrixRounding  = IF cbMatrixRounding EQ "None" THEN "" ELSE cbMatrixRounding
   .  /* gdm - 05180924 */

  /* gdm - 11190903 */
  IF v-zipflg THEN DO:
    IF cust.zip:SCREEN-VALUE NE "" THEN
    DO:
       FIND FIRST zipcode NO-LOCK 
           WHERE zipcode.zipcode EQ cust.zip NO-ERROR.
       IF NOT AVAIL zipcode THEN DO:
         CREATE zipcode.
         ASSIGN zipcode.zipcode   = cust.zip
                zipcode.pref_type = "P"
                zipcode.pref#     = 01
                zipcode.city      = cust.city
                zipcode.state     = cust.state
                zipcode.area_code = IF cust.area-code EQ "" 
                                              THEN 000 
                                              ELSE INT(cust.area-code) 
                zipcode.carrier   = cust.carrier
                zipcode.del-zone  = cust.del-zone.
       END.
       v-zipflg = FALSE.
       RELEASE zipcode.
    END.
  END.
  /* gdm - 11190903 end */


  if adm-new-record and not adm-adding-record then do:  /* copy */
    FIND FIRST bf-cust WHERE RECID(bf-cust) = v-cust-recid-prev NO-LOCK NO-ERROR.

    for each bf-shipto of bf-cust NO-LOCK BY bf-shipto.ship-no:
        create shipto.
        buffer-copy bf-shipto except bf-shipto.rec_key bf-shipto.cust-no to shipto.
        assign shipto.cust-no = cust.cust-no
                shipto.company = cust.company
                shipto.ship-addr[1] = cust.addr[1]
                shipto.ship-addr[2] = cust.addr[2]
                shipto.ship-city = cust.city
                shipto.ship-id = cust.cust-no
                shipto.ship-name = cust.name
                shipto.ship-state = cust.state
                shipto.ship-zip = cust.zip
                shipto.carrier = cust.carrier
                shipto.dest-code = cust.del-zone
                shipto.loc = cust.loc
                shipto.tax-code = cust.tax-gr
                shipto.tax-mandatory = cust.sort EQ "Y"
                shipto.spare-char-1  = cust.sman.
         LEAVE.  /* just copy first shipto only Task 05250421*/
    end.
    for each bf-soldto of bf-cust NO-LOCK BY bf-soldto.sold-no:
        create soldto.
        buffer-copy bf-soldto EXCEPT bf-soldto.rec_key bf-soldto.cust-no to soldto.
        assign soldto.cust-no = cust.cust-no
                soldto.company = cust.company
                soldto.sold-addr[1] = cust.addr[1]
                soldto.sold-addr[2] = cust.addr[2]
                soldto.sold-city = cust.city
                soldto.sold-id = cust.cust-no
                soldto.sold-name = cust.name
                soldto.sold-state = cust.state
                soldto.sold-zip = cust.zip.
        LEAVE. /* just copy first shipto only Task 05250421*/
    end.
  END.


   IF adm-new-record THEN DO:

       FOR EACH bf-usercust WHERE bf-usercust.company = cocode
                 AND bf-usercust.USER_id EQ USERID("nosweat") NO-LOCK,
            FIRST bff-cust WHERE bff-cust.company EQ bf-usercust.company 
                          AND bff-cust.cust-no EQ bf-usercust.cust-no NO-LOCK :
           LEAVE.
       END.
       IF AVAIL bff-cust  THEN
       IF ou-log AND NOT CAN-FIND(FIRST usercust WHERE
             usercust.user_id EQ USERID("nosweat") AND
             usercust.company EQ cocode AND
             usercust.cust-no EQ cust.cust-no) THEN
             DO:
                CREATE usercust.
                ASSIGN
                   usercust.user_id = USERID("nosweat")
                   usercust.company = cocode
                   usercust.cust-no = cust.cust-no.
                RELEASE usercust.
             END.
     IF v-cust-log THEN 
         RUN cust-new-log.
   END.
   ELSE DO:
     IF v-cust-log THEN 
         RUN cust-update-log.
   END.

   ASSIGN
    cust.bank-acct =  cAccount
    cust.SwiftBIC  =  cShift  
    cust.Bank-RTN  =  cRouting .

   
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-fields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/cust.i}
  IF cust.date-field[2] EQ TODAY THEN
      cust.date-field[2] = ?.       
  FIND FIRST company NO-LOCK WHERE company.company EQ cocode NO-ERROR.     
  cust.fax-country = company.countryCode.
  DO WITH FRAME {&FRAME-NAME}:

     if adm-new-record and adm-adding-record THEN /*adding, not copying*/
     DO: /*ESP need both statements due to how cust.fax is populated*/
        ASSIGN
        faxnumber:SCREEN-VALUE = ""
        faxnumber = ""
        rd_inv-meth:SCREEN-VALUE  = STRING(cust.inv-meth)
        fi_flat-comm:SCREEN-VALUE = "" 
        fl_custemail:SCREEN-VALUE = ""
        cbMatrixPrecision:SCREEN-VALUE = STRING(cust.matrixPrecision)
        cbMatrixRounding:SCREEN-VALUE  = STRING(cust.matrixRounding)
        cbMatrixPrecision
        cbMatrixRounding        
        .
     END.
  END.
  RUN display-active.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR thisOne AS CHAR NO-UNDO.
  DEFINE BUFFER buff-cust FOR cust .

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

    /* Code placed here will execute BEFORE standard behavior.    */ 
   IF v-cust-log THEN do:
      FIND CURRENT cust NO-LOCK NO-ERROR.
      DO I = 1 TO NUM-ENTRIES(v-cust-fmt):
          ASSIGN thisOne = ENTRY(i,v-cust-fmt).
          FIND FIRST buff-cust WHERE buff-cust.cust-no = cust.cust-no 
                                  AND buff-cust.company = thisOne EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL buff-cust THEN
              DELETE buff-cust .
      END.
   END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lAvailable AS LOGICAL NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL cust AND NOT adm-new-record THEN DO:
    ASSIGN
     rd_inv-meth  = cust.inv-meth
     fi_flat-comm = 0.

    IF cust.cust-no NE "" THEN RUN reftable-values (YES).
        ASSIGN 
            fl_custemail = cust.email
            fl_custemail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fl_custemail
            .
  END.

    /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE  fi_flat-comm.

    cust.cust-no:BGCOLOR = 3.

    IF NOT adm-new-record THEN
    DO:
      ASSIGN
        faxAreaCode = SUBSTR(cust.fax,1,3)  when avail cust
        faxNumber   = SUBSTR(cust.fax,4)    when avail cust.
      DISPLAY {&faxFields}.
    END.
    IF AVAILABLE cust AND NOT adm-new-record THEN DO:
        ASSIGN 
            cbMatrixPrecision = cust.matrixPrecision
            cbMatrixRounding  = IF cust.matrixRounding EQ "" THEN "None" ELSE cust.matrixRounding
            .
        DISPLAY
            cbMatrixPrecision
            cbMatrixRounding
            . 
                   
    END.             
  END.

  RUN display-active.
  
  RUN pDisplayTagStatus.

  IF AVAIL cust THEN DO:
       ASSIGN
           cAccount = cust.bank-acct   
           cShift   = cust.SwiftBIC    
           cRouting = cust.Bank-RTN  
           .
       RUN Tag_IsTagRecordAvailable(
           INPUT cust.rec_key,
           INPUT "cust",
           OUTPUT lAvailable
           ).
         IF lAvailable THEN  
             btnTags:SENSITIVE = TRUE
             .
         ELSE 
             btnTags:SENSITIVE = FALSE.           
      
      IF cust.pricingMethod EQ "" THEN
          cust.pricingMethod:SCREEN-VALUE = " ".
      IF NOT lQuotePriceMatrix THEN
      cust.pricingMethod:HIDDEN IN FRAME {&FRAME-NAME} = YES. 
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
  def var ls-prev-cust-no like cust.cust-no no-undo.
  DEF VAR ll-prev-cr-hold LIKE cust.cr-hold NO-UNDO.
  DEF VAR ls-prev-sman LIKE cust.sman NO-UNDO.
  DEF VAR ll-new-record AS LOG NO-UNDO.
  DEFINE VARIABLE cOld-fob    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOld-freight AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lError         AS LOGICAL NO-UNDO.

  def buffer bf-cust for cust.
  DEFINE BUFFER bf-shipto for shipto.
  /*def buffer bf-soldto for soldto.*/

  /* Code placed here will execute PRIOR to standard behavior. */
  assign
   ls-prev-cust-no = cust.cust-no
   ll-prev-cr-hold = cust.cr-hold
   ls-prev-sman = cust.sman  
   ll-new-record = FALSE .

  RUN cust-zip.
  {&methods/lValidateError.i YES}
  do with frame {&frame-name}:
     IF adm-new-record THEN DO:
        ll-new-record = TRUE.
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
           MESSAGE "Your NK1 Setting for customer length is set to" v-cust-length "characters, so you" SKIP
               "must change your setting or use a" v-cust-length "character length customer number."
               VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO cust.cust-no.
           RETURN NO-APPLY.
        END.
     END.
     {&methods/lValidateError.i NO}
     /*if /*cust.sman:screen-value <> "" and */
        not can-find(first sman where sman.sman = cust.sman:screen-value)
     then do:
        message "Invalid Sales Rep. Try Help." view-as alert-box error.
        apply "entry" to cust.sman.
        return .
     end.*/

     /* 33482 - Ensure blank record is not saved - MYT - 08/28/18 */
        IF adm-new-record 
        AND cust.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN DO:
            RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
            RETURN NO-APPLY.
        END.
     
     RUN check-cr-bal NO-ERROR .
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-carrier NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-custtype NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-custcsr NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. 
     
     RUN valid-bill-owner(OUTPUT lReturnError) NO-ERROR.
     IF lReturnError THEN RETURN NO-APPLY.

     RUN valid-sman NO-ERROR. 
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-status NO-ERROR. 
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
     {&methods/lValidateError.i YES}
     if cust.pallet:screen-value <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "D" and
                                      item.i-no = cust.pallet:screen-value)
     then do:
        message "Invalid Pallet Code. Try Help." view-as alert-box error.
        apply "entry" to cust.pallet.
        return .     
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
     if /*cust.del-zone:screen-value <> "" and*/
        not can-find(first carr-mtx where carr-mtx.company = gcompany and 
                                     carr-mtx.loc = cust.loc:screen-value and
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
        message "Invalid Tax Group. Try Help." view-as alert-box error.
        apply "entry" to cust.tax-gr.
        return no-apply.
     end.                                     
     if cust.state:screen-value <> "" and
        not can-find(first statecod where statecod.statecod = cust.state:screen-value )
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
     IF cust.curr-code:SCREEN-VALUE <> "" THEN DO:
        RUN valid-currency NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
     END.
  end.
  {&methods/lValidateError.i NO}
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
  
  RUN valid-ClassId .
  IF NOT v-valid THEN RETURN NO-APPLY.


  /* ============== end of validations ==================*/
  ASSIGN cOld-fob = cust.fob-code
         cOld-freight = cust.frt-pay  .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN disable-fields.
  RUN pGetInterCompanyBilling(INPUT cocode, INPUT cust.cust-no, OUTPUT lInterCompanyBilling).
  
  IF lInterCompanyBilling THEN
  DO:
      FOR EACH bf-shipto NO-LOCK
          WHERE bf-shipto.company EQ cust.company
          AND bf-shipto.cust-no EQ cust.cust-no:
      
        RUN Customer_InterCompanyTrans IN hdCustomerProcs(
                                           INPUT cocode,
                                           INPUT cust.cust-no,
                                           INPUT bf-shipto.ship-id,
                                           INPUT "",
                                           OUTPUT lError,
                                           OUTPUT cMessage
                                           ).
      END.                                  
  END.
  
  IF cust.cr-hold NE ll-prev-cr-hold THEN DO:
      RUN ClearTagsHold (cust.rec_key).
      CASE cust.cr-hold:

          WHEN NO THEN
          RUN AddTagHold (
              INPUT cust.rec_key,
              INPUT "cust",
              INPUT "Released from Hold by " + USERID("ASI"),
              INPUT "",
              INPUT ""
              ).
          WHEN YES THEN
          RUN AddTagHold (
              INPUT cust.rec_key,
              INPUT "cust",
              INPUT "Placed on Hold by " + USERID("ASI"),
              INPUT "",
              INPUT ""
              ).
      END CASE.
  END. /* if credit hold changed */

  IF NOT adm-new-record             AND
    cust.cr-hold NE ll-prev-cr-hold THEN
  FOR EACH oe-ord
      WHERE oe-ord.company             EQ cocode
        AND oe-ord.cust-no             EQ cust.cust-no
        AND INDEX("CDZWV",oe-ord.stat) EQ 0:
    oe-ord.stat = IF cust.cr-hold THEN "H" ELSE "A".
  END.
  IF NOT adm-new-record AND ls-prev-sman <> cust.sman  THEN DO:
      RUN windows/w-updsmn.w (cust.cust-no).
  END.

  IF NOT adm-new-record AND (cOld-fob NE cust.fob-code OR  cOld-freight NE cust.frt-pay) THEN DO:
      RUN fg/custfobudt.w(ROWID(cust)) .
  END.


  IF ll-new-record THEN DO:
    /* Reposition browse to new record so other tabs are refreshed */
    {methods/run_link.i "RECORD-SOURCE" "repo-query2" "(INPUT ROWID(cust))"} 
  END.
  lCheckMessage = NO .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy B-table-Win
PROCEDURE local-destroy:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    IF VALID-HANDLE (hdSalesManProcs) THEN
        DELETE PROCEDURE hdSalesManProcs.
        
    IF VALID-HANDLE (hdCustomerProcs) THEN
            DELETE PROCEDURE hdCustomerProcs.        

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-display AS LOG NO-UNDO.

  IF AVAIL cust THEN DO:

    IF ip-display THEN
      fi_flat-comm = cust.flatCommPct.
    ELSE
      cust.flatCommPct = fi_flat-comm.

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
  {src/adm/template/snd-list.i "cust"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetInterCompanyBilling V-table-Win 
PROCEDURE pGetInterCompanyBilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oplReturnValue AS LOGICAL NO-UNDO.
  
  DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.      
      
  RUN sys/ref/nk1look.p (INPUT ipcCompany, "InterCompanyBilling", "L" /* Logical */, YES /* check by cust */, 
      INPUT YES /* use cust not vendor */, ipcCustomer /* cust */, "" /* ship-to*/,
      OUTPUT cReturn, OUTPUT lRecFound).
  oplReturnValue = LOGICAL(cReturn) NO-ERROR.       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNewCountry V-table-Win 
PROCEDURE pNewCountry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&frame-name}:
      FIND FIRST statecod NO-LOCK
           WHERE statecod.statecod EQ cust.state:SCREEN-VALUE NO-ERROR.
      IF AVAIL statecod THEN
      DO:
         cust.fax-country:SCREEN-VALUE = statecod.countryCode. 
      END.
  END.            

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-sman V-table-Win 
PROCEDURE update-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-markup AS DEC NO-UNDO.


  /* SESSION:SET-WAIT-STATE("general").
   FOR EACH eb WHERE eb.company = cust.company 
                 AND eb.cust-no = cust.cust-no:
       IF eb.sman <> cust.sman THEN do:
           eb.sman = cust.sman.

           RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).

           run sys/inc/getsmncm.p (eb.cust-no,
                                   INPUT-OUTPUT eb.sman,
                                   eb.procat,
                                   ld-markup,
                                   OUTPUT eb.comm).
       END.
   END.
   FOR EACH quotehd WHERE quotehd.company = cust.company
                      AND quotehd.cust-no = cust.cust-no:
       IF quotehd.sman <> cust.sman THEN quotehd.sman = cust.sman.
   END.
   SESSION:SET-WAIT-STATE("").
   
   RUN windows/w-updsmn.w (cust.cust-no). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bill-owner V-table-Win 
PROCEDURE valid-bill-owner :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
  {methods/lValidateError.i YES}

   IF cust.accountant:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
       IF NOT CAN-FIND(FIRST users WHERE users.USER_ID EQ cust.accountant:SCREEN-VALUE IN FRAME {&FRAME-NAME})
       THEN DO:
           MESSAGE "Invalid customer Accountant. Try help." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO cust.accountant.
           oplReturnError = YES.
       END.
   END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-carrier V-table-Win 
PROCEDURE valid-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&frame-name}:
    cust.carrier:SCREEN-VALUE = CAPS(cust.carrier:SCREEN-VALUE).

     FIND FIRST carrier  
        WHERE carrier.company EQ g_company 
        AND carrier.loc     EQ cust.loc:SCREEN-VALUE
        AND carrier.carrier EQ cust.carrier:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL carrier THEN DO:
        IF NOT DYNAMIC-FUNCTION("IsActive", carrier.rec_key) THEN do: 
            MESSAGE "Please note: Carrier " cust.carrier:SCREEN-VALUE " is valid but currently inactive"
            VIEW-AS ALERT-BOX INFO.
            APPLY "entry" TO cust.carrier.
            RETURN ERROR.
        END.
    END.

    IF NOT AVAIL carrier THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cust.carrier.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-classId V-table-Win 
PROCEDURE valid-classId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO.
   DEFINE VARIABLE lCheckAccount AS LOGICAL NO-UNDO.
   DEFINE BUFFER bf-arclass FOR arclass.
  {methods/lValidateError.i YES}
  v-valid = YES.

  DO WITH FRAME {&frame-name}:
     IF cust.classId:SCREEN-VALUE NE "" THEN do:
       FIND FIRST arclass NO-LOCK
            WHERE arclass.classID EQ INTEGER(cust.classId:SCREEN-VALUE) NO-ERROR.
       IF AVAIL arclass AND arclass.inActive EQ YES THEN do:
        MESSAGE "AR ClassId is inactive. Try Help." VIEW-AS ALERT-BOX INFO.  
        ASSIGN
          v-valid = NO  .         
         APPLY "entry" TO cust.classId.
       END.     
            
       IF NOT AVAIL arclass THEN do:
        MESSAGE "Invalid AR ClassId. Try Help." VIEW-AS ALERT-BOX INFO.  
        ASSIGN
          v-valid = NO  .          
         APPLY "entry" TO cust.classId.
       END.
     END.
        
     IF NOT adm-new-record AND cust.classID:SCREEN-VALUE NE ""
        AND INTEGER(cust.classID:SCREEN-VALUE) NE cust.classID AND cust.acc-bal <> 0 THEN DO:
        FIND FIRST bf-arclass NO-LOCK
            WHERE bf-arclass.classID EQ INTEGER(cust.classId) NO-ERROR.
        IF AVAIL arclass AND AVAIL bf-arclass AND bf-arclass.receivablesAcct NE arclass.receivablesAcct THEN DO:
          MESSAGE "A/R Class can only be changed with a customer with no balance or with the same G/L Account number"
                   VIEW-AS ALERT-BOX INFO.
           v-valid = NO  .
           lCheckAccount = TRUE .
         APPLY "entry" TO cust.classId.         
        END. 
        IF AVAIL arclass AND AVAIL bf-arclass AND bf-arclass.receivablesAcct EQ arclass.receivablesAcct THEN lCheckAccount = TRUE .          
     END.
          
     IF AVAIL arclass AND AVAIL cust AND NOT lCheckAccount AND cust.classId:SCREEN-VALUE NE "" AND cust.acc-bal <> 0 AND NOT lCheckMessage 
         AND integer(cust.classId:SCREEN-VALUE) NE cust.classId THEN do:
         RUN displayMessageQuestionLOG("35",OUTPUT lCheckError).          
         
       IF NOT lCheckError THEN do:         
        ASSIGN
          v-valid = NO  
          lCheckMessage = NO.          
          cust.classId:SCREEN-VALUE = "".
         APPLY "entry" TO cust.classId.
       END. 
       ELSE
         lCheckMessage = YES .
     END.    
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cr-hold V-table-Win 
PROCEDURE valid-cr-hold :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  v-valid = YES.

  DO WITH FRAME {&frame-name}:    
    IF NOT ll-secure                                              AND
       v-secur                                                    AND
       /*NOT adm-new-record                                         AND*/
       STRING(cust.cr-hold,"yes/no") NE cust.cr-hold:SCREEN-VALUE AND
       v-custpass                                                 THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                   = NO
         cust.cr-hold:SCREEN-VALUE = STRING(cust.cr-hold,"yes/no").
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.cr-hold.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cr-hold-invdays V-table-Win 
PROCEDURE valid-cr-hold-invdays :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF NOT ll-secure                                                  AND
       v-secur                                                        AND
       SUBSTR(v-secur-fld,1,1) EQ "Y"                                 AND
       /*NOT adm-new-record                                             AND*/
       cust.cr-hold-invdays NE DEC(cust.cr-hold-invdays:SCREEN-VALUE) AND
       v-custpass                                                     THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                           = NO
         cust.cr-hold-invdays:SCREEN-VALUE = STRING(cust.cr-hold-invdays).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.cr-hold-invdays.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cr-lim V-table-Win 
PROCEDURE valid-cr-lim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF NOT ll-secure                                AND
       v-secur                                      AND
       SUBSTR(v-secur-fld,2,1) EQ "Y"               AND
       /*NOT adm-new-record                           AND*/
       cust.cr-lim NE DEC(cust.cr-lim:SCREEN-VALUE) AND
       v-custpass                                   THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                  = NO
         cust.cr-lim:SCREEN-VALUE = STRING(cust.cr-lim).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.cr-lim.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-currency V-table-Win 
PROCEDURE valid-currency :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
 FIND FIRST currency WHERE currency.company = g_company
                       AND currency.c-code = cust.curr-code:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                       NO-LOCK NO-ERROR.
 IF NOT AVAIL currency THEN DO:
    MESSAGE "Invalid Currency. Try Help." VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO cust.curr-code.
    RETURN error.
 END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-custcsr V-table-Win 
PROCEDURE valid-custcsr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}

   IF cust.csrUser_id:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
       IF NOT CAN-FIND(FIRST users WHERE users.USER_ID EQ cust.csrUser_id:SCREEN-VALUE IN FRAME {&FRAME-NAME})
       THEN DO:
           MESSAGE "Invalid customer CSR. Try help." VIEW-AS ALERT-BOX ERROR.
           RETURN ERROR.
       END.
   END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-custtype V-table-Win 
PROCEDURE valid-custtype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  IF NOT CAN-FIND(FIRST custype WHERE custype.company = cocode
                       AND custype.custype = cust.TYPE:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                        )
  THEN DO:
     MESSAGE "Invalid customer type. Try help." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-disc V-table-Win 
PROCEDURE valid-disc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF NOT ll-secure                            AND
       v-secur                                  AND
       SUBSTR(v-secur-fld,6,1) EQ "Y"           AND
       /*NOT adm-new-record                       AND*/
       cust.disc NE DEC(cust.disc:SCREEN-VALUE) AND
       v-custpass                               THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                = NO
         cust.disc:SCREEN-VALUE = STRING(cust.disc).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.disc.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fin-chg V-table-Win 
PROCEDURE valid-fin-chg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  v-valid = YES.
  DO WITH FRAME {&frame-name}:
    IF NOT ll-secure                                              AND
       v-secur                                                    AND
       SUBSTR(v-secur-fld,7,1) EQ "Y"                             AND
       /*NOT adm-new-record                                         AND*/
       STRING(cust.fin-chg,"yes/no") NE cust.fin-chg:SCREEN-VALUE AND
       v-custpass                                                 THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                   = NO
         cust.fin-chg:SCREEN-VALUE = STRING(cust.fin-chg,"yes/no").
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.fin-chg.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-meth V-table-Win 
PROCEDURE valid-inv-meth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF NOT ll-secure                                   AND
       v-secur                                         AND
       SUBSTR(v-secur-fld,4,1) EQ "Y"                  AND
       /*NOT adm-new-record                              AND*/
       ((rd_inv-meth NE ? AND STRING(rd_inv-meth) NE rd_inv-meth:SCREEN-VALUE) OR
       (rd_inv-meth EQ ? AND rd_inv-meth:SCREEN-VALUE NE "?")) AND
       v-custpass                                      THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                  = NO
         rd_inv-meth:SCREEN-VALUE = STRING(rd_inv-meth).
    END.

    IF NOT v-valid THEN APPLY "entry" TO rd_inv-meth.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-markup V-table-Win 
PROCEDURE valid-markup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF NOT ll-secure                                AND
       v-secur                                      AND
       SUBSTR(v-secur-fld,8,1) EQ "Y"               AND
       /*NOT adm-new-record                           AND*/
       cust.markup NE DEC(cust.markup:SCREEN-VALUE) AND
       v-custpass                                   THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                  = NO
         cust.markup:SCREEN-VALUE = STRING(cust.markup).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.markup.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-lim V-table-Win 
PROCEDURE valid-ord-lim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF NOT ll-secure                                  AND
       v-secur                                        AND
       SUBSTR(v-secur-fld,3,1) EQ "Y"                 AND
       /*NOT adm-new-record                             AND*/
       cust.ord-lim NE DEC(cust.ord-lim:SCREEN-VALUE) AND
       v-custpass                                     THEN DO:

      RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         v-valid                   = NO
         cust.ord-lim:SCREEN-VALUE = STRING(cust.ord-lim).
    END.

    IF NOT v-valid THEN APPLY "entry" TO cust.ord-lim.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sman V-table-Win 
PROCEDURE valid-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
   
  {methods/lValidateError.i YES}
    RUN SalesMan_ValidateSalesRep IN hdSalesManProcs(  
        INPUT  cocode,
        INPUT  cust.sman:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).  
    IF NOT lSuccess THEN DO:
       MESSAGE cMessage
       VIEW-AS ALERT-BOX ERROR.
       APPLY "ENTRY" TO cust.sman.
       RETURN ERROR.  
    END. 
    sman_sname:SCREEN-VALUE = DYNAMIC-FUNCTION("SalesMan_GetSalesmanName" IN hdSalesManProcs,cocode,cust.sman:SCREEN-VALUE).
   
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-status V-table-Win 
PROCEDURE valid-status :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
IF cust.active:SCREEN-VALUE IN FRAME {&FRAME-NAME} BEGINS "(X)" 
  THEN 
    FIND FIRST bf-cust NO-LOCK
      WHERE bf-cust.company EQ cocode
        AND bf-cust.active = "X"
        AND bf-cust.cust-no NE cust.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
      NO-ERROR.
    IF AVAIL bf-cust THEN DO:
      MESSAGE 
        "Customer " + bf-cust.cust-no + " - " + bf-cust.NAME 
        " is already marked as the inhouse account." SKIP 
        "You can only have 1 inhouse account. Please select another status."
       VIEW-AS ALERT-BOX ERROR.

        APPLY "entry" TO cust.active .
      RETURN ERROR.
    END.


  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-terms V-table-Win 
PROCEDURE valid-terms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  v-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF NOT ll-secure                         AND
       v-secur                               AND
       SUBSTR(v-secur-fld,5,1) EQ "Y"        AND
       /*NOT adm-new-record                    AND*/
       cust.terms NE cust.terms:SCREEN-VALUE AND
       v-custpass                            THEN DO:

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

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE zip-carrier V-table-Win 
PROCEDURE zip-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:

   /* gdm - 10010913 */
   FIND FIRST zipcode
        WHERE zipcode.zipcode EQ cust.zip:SCREEN-VALUE
        NO-LOCK NO-ERROR.

   ASSIGN
      cust.carrier:SCREEN-VALUE = IF AVAIL zipcode AND 
                                     TRIM(zipcode.carrier) NE "" 
                                    THEN zipcode.carrier 
                                    ELSE cust.carrier:SCREEN-VALUE
      cust.del-zone:SCREEN-VALUE = IF AVAIL zipcode AND 
                                      TRIM(zipcode.del-zone) NE "" 
                                     THEN zipcode.del-zone            
                                     ELSE cust.del-zone:SCREEN-VALUE.
      /* gdm - 10010913 end*/
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

