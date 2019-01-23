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

/* gdm - 05050903 */
DEF BUFFER bf-cust FOR cust.

/* gdm - 08240904 */
DEF VAR v-inv-fmt2 AS CHAR NO-UNDO.

DO TRANSACTION:
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
&Scoped-Define ENABLED-FIELDS cust.active cust.name cust.addr[1] ~
cust.addr[2] cust.city cust.state cust.zip cust.terms cust.cr-use ~
cust.cr-rating cust.cr-lim cust.ord-lim cust.disc cust.curr-code ~
cust.cr-hold-invdays cust.cr-hold-invdue cust.cust-level cust.cr-hold ~
cust.fin-chg cust.auto-reprice cust.an-edi-cust cust.factored cust.sort ~
cust.spare-char-1 cust.tax-gr cust.tax-id cust.date-field[2] ~
cust.spare-char-2 cust.date-field[1] cust.type cust.csrUser_id cust.contact ~
cust.sman cust.area-code cust.phone cust.scomm cust.fax-prefix ~
cust.fax-country cust.frt-pay cust.fob-code cust.ship-part cust.loc ~
cust.carrier cust.del-zone cust.terr cust.under-pct cust.over-pct ~
cust.markup cust.ship-days cust.manf-day cust.spare-int-1 cust.pallet ~
cust.case-bundle cust.int-field[1] cust.po-mandatory cust.show-set ~
cust.log-field[1] cust.imported
&Scoped-define ENABLED-TABLES cust
&Scoped-define FIRST-ENABLED-TABLE cust
&Scoped-Define ENABLED-OBJECTS btn_bank-info RECT-2 RECT-3 RECT-4 
&Scoped-Define DISPLAYED-FIELDS cust.cust-no cust.active cust.name ~
cust.addr[1] cust.addr[2] cust.city cust.state cust.zip cust.terms ~
cust.cr-use cust.cr-rating cust.cr-lim cust.ord-lim cust.disc ~
cust.curr-code cust.cr-hold-invdays cust.cr-hold-invdue cust.cust-level ~
cust.cr-hold cust.fin-chg cust.auto-reprice cust.an-edi-cust cust.factored ~
cust.sort cust.spare-char-1 cust.tax-gr cust.tax-id cust.date-field[2] ~
cust.spare-char-2 cust.date-field[1] cust.type cust.csrUser_id cust.contact ~
cust.sman cust.area-code cust.phone cust.scomm cust.fax-prefix ~
cust.fax-country cust.frt-pay cust.fob-code cust.ship-part cust.loc ~
cust.carrier cust.del-zone cust.terr cust.under-pct cust.over-pct ~
cust.markup cust.ship-days cust.manf-day cust.spare-int-1 cust.pallet ~
cust.case-bundle cust.int-field[1] cust.po-mandatory cust.show-set ~
cust.log-field[1] cust.imported
&Scoped-define DISPLAYED-TABLES cust
&Scoped-define FIRST-DISPLAYED-TABLE cust
&Scoped-Define DISPLAYED-OBJECTS fl_custemail terms_dscr rd_inv-meth ~
stax_tax-dscr custype_dscr sman_sname fi_flat-comm faxAreaCode faxNumber ~
loc_dscr carrier_dscr carr-mtx_del-dscr terr_dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,faxFields,F1 */
&Scoped-define ADM-CREATE-FIELDS cust.cust-no 
&Scoped-define ADM-ASSIGN-FIELDS fl_custemail rd_inv-meth fi_flat-comm ~
cust.po-mandatory cust.show-set 
&Scoped-define DISPLAY-FIELD cust.state fl_custemail cust.terms cust.tax-gr ~
cust.type cust.csrUser_id cust.sman cust.loc cust.carrier cust.del-zone ~
cust.terr cust.po-mandatory cust.show-set 
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
DEFINE BUTTON btn_bank-info 
     LABEL "Bank Info" 
     SIZE 16.4 BY 1
     BGCOLOR 15 FONT 4.

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
     SIZE 9 BY 1 NO-UNDO.

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
"Group by Date", ?
     SIZE 46 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 8.57.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 3.82.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.4 BY 12.30.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cust.cust-no AT ROW 1 COL 12 COLON-ALIGNED
          LABEL "Customer"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     cust.active AT ROW 1 COL 43 COLON-ALIGNED
          LABEL "Status" FORMAT "x(11)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "(A)ctive","(I)nactive","(X) Inhouse","(S)tatement","(E)-Service" 
          DROP-DOWN-LIST
          SIZE 19 BY 1 TOOLTIP "Active, Inactive, Inhouse, Statement"
          BGCOLOR 15 
     cust.name AT ROW 1.95 COL 12 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 15 FONT 4
     cust.addr[1] AT ROW 2.95 COL 12 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 15 FONT 4
     cust.addr[2] AT ROW 3.86 COL 12 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 15 FONT 4
     cust.city AT ROW 4.86 COL 12 COLON-ALIGNED FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 15 FONT 4
     cust.state AT ROW 4.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     cust.zip AT ROW 4.86 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FONT 4
     fl_custemail AT ROW 5.76 COL 12 COLON-ALIGNED WIDGET-ID 2
     cust.terms AT ROW 7.67 COL 17 COLON-ALIGNED
          LABEL "Terms"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FONT 4
     terms_dscr AT ROW 7.67 COL 27 COLON-ALIGNED NO-LABEL
     cust.cr-use AT ROW 8.62 COL 17 COLON-ALIGNED
          LABEL "Cr. Acct #"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     cust.cr-rating AT ROW 9.57 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FONT 4
     cust.cr-lim AT ROW 10.52 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 4
     cust.ord-lim AT ROW 11.48 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 4
     cust.disc AT ROW 12.43 COL 17 COLON-ALIGNED
          LABEL "Discount %"
          VIEW-AS FILL-IN 
          SIZE 10.2 BY 1
          BGCOLOR 15 FONT 4
     cust.curr-code AT ROW 13.38 COL 17 COLON-ALIGNED
          LABEL "Currency"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     cust.cr-hold-invdays AT ROW 8.86 COL 46.4 COLON-ALIGNED
          LABEL "Grace Days"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     cust.cr-hold-invdue AT ROW 8.86 COL 55.4 COLON-ALIGNED
          LABEL "$"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 1
     cust.cust-level AT ROW 9.81 COL 56 COLON-ALIGNED
          LABEL "Price Level"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     cust.cr-hold AT ROW 10.76 COL 47
          LABEL "Credit Hold"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     cust.fin-chg AT ROW 11.48 COL 47
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     cust.auto-reprice AT ROW 12.29 COL 47
          VIEW-AS TOGGLE-BOX
          SIZE 23.2 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cust.an-edi-cust AT ROW 13 COL 47
          LABEL "EDI"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
     cust.factored AT ROW 13.71 COL 47
          LABEL "Factored"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     rd_inv-meth AT ROW 14.48 COL 19 NO-LABEL
     cust.sort AT ROW 16.14 COL 22 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Yes", "Y":U,
"No", "N":U
          SIZE 16.4 BY .62
     cust.spare-char-1 AT ROW 15.95 COL 58 COLON-ALIGNED WIDGET-ID 10
          LABEL "Tax Prep Code" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     cust.tax-gr AT ROW 16.95 COL 16 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     stax_tax-dscr AT ROW 16.95 COL 28 COLON-ALIGNED NO-LABEL
     cust.tax-id AT ROW 17.91 COL 16 COLON-ALIGNED
          LABEL "Tax Resale#"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 FONT 4
     cust.date-field[2] AT ROW 17.91 COL 52.2 COLON-ALIGNED
          LABEL "Exp."
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     cust.spare-char-2 AT ROW 1 COL 73 COLON-ALIGNED WIDGET-ID 14
          LABEL "Group" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17.2 BY 1
          BGCOLOR 15 FONT 4
     cust.date-field[1] AT ROW 1 COL 127 COLON-ALIGNED
          LABEL "Date Added" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust.type AT ROW 1.95 COL 73 COLON-ALIGNED
          LABEL "Type"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust.csrUser_id AT ROW 1.95 COL 127 COLON-ALIGNED
          LABEL "CSR"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     custype_dscr AT ROW 1.95 COL 90 COLON-ALIGNED NO-LABEL
     cust.contact AT ROW 2.95 COL 73 COLON-ALIGNED
          LABEL "Contact"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
          BGCOLOR 15 FONT 4
     btn_bank-info AT ROW 2.95 COL 129.2
     cust.sman AT ROW 3.86 COL 73 COLON-ALIGNED
          LABEL "SalesGrp"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     sman_sname AT ROW 3.86 COL 81 COLON-ALIGNED NO-LABEL
     fi_flat-comm AT ROW 3.86 COL 135 COLON-ALIGNED
     cust.area-code AT ROW 4.86 COL 73 COLON-ALIGNED AUTO-RETURN 
          LABEL "Phone#" FORMAT "(xxx)"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     cust.phone AT ROW 4.86 COL 81 COLON-ALIGNED NO-LABEL FORMAT "xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 4
     cust.scomm AT ROW 4.86 COL 135 COLON-ALIGNED HELP
          "Enter Salesman Flat Commission Percentage" WIDGET-ID 8
          LABEL "Broker Comm%"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     faxAreaCode AT ROW 5.76 COL 73 COLON-ALIGNED AUTO-RETURN 
     faxNumber AT ROW 5.76 COL 81 COLON-ALIGNED NO-LABEL
     cust.fax-prefix AT ROW 5.76 COL 106 COLON-ALIGNED
          LABEL "Prefix"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cust.fax-country AT ROW 5.76 COL 123 COLON-ALIGNED
          LABEL "Country"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 
     cust.frt-pay AT ROW 7.86 COL 92.2 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Bill", "B":U,
"Collect", "C":U,
"Prepaid", "P":U,
"3rd Party", "T":U
          SIZE 49 BY .81
     cust.fob-code AT ROW 8.81 COL 92.2 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Destination", "DEST":U,
"Origin", "ORIG":U
          SIZE 28 BY .81
     cust.ship-part AT ROW 8.76 COL 122
          LABEL "Partial Ship"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .81
     cust.loc AT ROW 9.76 COL 88 COLON-ALIGNED
          LABEL "Location"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 FONT 4
     loc_dscr AT ROW 9.76 COL 100 COLON-ALIGNED NO-LABEL
     cust.carrier AT ROW 10.71 COL 88.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     carrier_dscr AT ROW 10.71 COL 100 COLON-ALIGNED NO-LABEL
     cust.del-zone AT ROW 11.71 COL 88.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx_del-dscr AT ROW 11.71 COL 100 COLON-ALIGNED NO-LABEL
     cust.terr AT ROW 12.67 COL 88.2 COLON-ALIGNED
          LABEL "Territory"
          VIEW-AS FILL-IN 
          SIZE 5.8 BY 1
          BGCOLOR 15 FONT 4
     terr_dscr AT ROW 12.67 COL 94 COLON-ALIGNED NO-LABEL
     cust.under-pct AT ROW 13.62 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     cust.over-pct AT ROW 14.57 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     cust.markup AT ROW 15.52 COL 88 COLON-ALIGNED FORMAT "->9.99"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 
     cust.ship-days AT ROW 16.48 COL 88 COLON-ALIGNED
          LABEL "Whse Days"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     cust.manf-day AT ROW 17.43 COL 90.2 COLON-ALIGNED HELP
          "" WIDGET-ID 6
          LABEL "Pallet Positions" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 8.8 BY 1
          FONT 4
    cust.imported AT ROW 18.43 COL 80.6
          LABEL "Contract Pricing?"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81 
     cust.spare-int-1 AT ROW 12.67 COL 129 COLON-ALIGNED WIDGET-ID 12
          LABEL "Pallet ID" FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 12.2 BY .95
          BGCOLOR 15 FONT 4
     cust.pallet AT ROW 13.62 COL 122.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
          BGCOLOR 15 FONT 4
     cust.case-bundle AT ROW 14.57 COL 122.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
          BGCOLOR 15 FONT 4
     cust.int-field[1] AT ROW 15.52 COL 127.6 COLON-ALIGNED
          LABEL "# of Labels per Skid" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
          BGCOLOR 15 FONT 4
     cust.po-mandatory AT ROW 16.62 COL 114.6
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81   
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cust.show-set AT ROW 17.38 COL 114.6
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     cust.log-field[1] AT ROW 18.14 COL 114.6 HELP
          "" WIDGET-ID 16
          LABEL "Paperless Invoice?"
          VIEW-AS TOGGLE-BOX
          SIZE 26.4 BY .81
     "Taxable:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 16.14 COL 11
     "Tax Information" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 15.33 COL 4
          FGCOLOR 9 FONT 4
     "Credit Information" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 6.95 COL 4
          FGCOLOR 9 FONT 4
     "FOB:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 8.67 COL 83.2
     "Freight Terms:" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 7.81 COL 72
     "Shipping Information" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 6.95 COL 75
          FGCOLOR 9 FONT 4
     "Invoice Per:" VIEW-AS TEXT
          SIZE 14 BY .81 AT ROW 14.38 COL 4
     RECT-2 AT ROW 7.19 COL 1
     RECT-3 AT ROW 15.67 COL 1
     RECT-4 AT ROW 7.19 COL 71.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


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
         HEIGHT             = 26.14
         WIDTH              = 201.6.
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

/* SETTINGS FOR COMBO-BOX cust.active IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.addr[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX cust.an-edi-cust IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.area-code IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN carr-mtx_del-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.carrier IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN carrier_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.city IN FRAME F-Main
   EXP-FORMAT                                                           */
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
/* SETTINGS FOR FILL-IN cust.phone IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR TOGGLE-BOX cust.po-mandatory IN FRAME F-Main
   2 4                                                                  */
/* SETTINGS FOR TOGGLE-BOX cust.imported IN FRAME F-Main
   2 4                                                                  */
/* SETTINGS FOR RADIO-SET rd_inv-meth IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       RECT-3:PRIVATE-DATA IN FRAME F-Main     = 
                "Date".

/* SETTINGS FOR FILL-IN cust.scomm IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN cust.ship-days IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX cust.ship-part IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX cust.show-set IN FRAME F-Main
   2 4                                                                  */
/* SETTINGS FOR FILL-IN cust.sman IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN sman_sname IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.spare-char-2 IN FRAME F-Main
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
     {&methods/lValidateError.i YES}
     if lastkey <> -1 /*and cust.carrier:screen-value <> "" */ and
        not can-find(first carrier where carrier.company = gcompany and 
                                     carrier.loc = cust.loc:screen-value and
                                     carrier.carrier = cust.carrier:screen-value)
     then do:
        message "Invalid Carrier Code. Try Help." view-as alert-box error.
        return no-apply.     
     end.

  {methods/dispflds.i}
   {&methods/lValidateError.i NO}
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
   IF LASTKEY = -1 THEN  RETURN.
   IF adm-new-record AND 
      CAN-FIND(FIRST cust WHERE cust.company = gcompany 
                            AND cust.cust-no = cust.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME})
   THEN DO:
      MESSAGE "Customer already exists. Try other number." VIEW-AS ALERT-BOX ERROR.
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
ON LEAVE OF cust.sman IN FRAME F-Main /* Salesman */
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


&Scoped-define SELF-NAME cust.spare-char-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.spare-char-1 V-table-Win
ON LEAVE OF cust.spare-char-1 IN FRAME F-Main /* Tax Prep Code */
DO:
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and SELF:screen-value <> "" and 
     (
    /* old
      not can-find(first stax where stax.tax-group begins gcompany and
                                   substring(stax.tax-group,1,10) = gcompany and
                                   substring(stax.tax-group,11,length(trim(stax.tax-group)) - 10) = cust.tax-gr:screen-value )
    */
       not can-find(first stax-group where stax-group.tax-group = self:screen-value) 
     )    
  then do:
     message "Invalid Tax Prep Code. Try Help." self:screen-value view-as alert-box error.
     return no-apply.
  end.   
  {&methods/lValidateError.i NO}                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.state V-table-Win
ON LEAVE OF cust.state IN FRAME F-Main /* State */
DO:
    {&methods/lValidateError.i YES}
    if lastkey <> -1 and cust.state:screen-value <> "" and
       not can-find(first state where state.state = cust.state:screen-value )
    then do:
       message "Invalid State Code. Try Help." view-as alert-box error.
       return no-apply.
    end.                                     
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.tax-gr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.tax-gr V-table-Win
ON LEAVE OF cust.tax-gr IN FRAME F-Main /* Tax Code */
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
     message "Invalid Tax Code. Try Help." self:screen-value view-as alert-box error.
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

DO TRANSACTION:
     {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""AF1"" }
END.

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
    FIND FIRST ar-inv NO-LOCK 
        WHERE ar-inv.company EQ cust.company
          AND ar-inv.cust-no EQ cust.cust-no 
          AND ar-inv.posted EQ NO NO-ERROR .
    IF AVAIL ar-inv THEN DO:
        MESSAGE 
        "Customer " + cust.cust-no + " - " + cust.NAME 
        " has at least one Open Invoice ." SKIP 
        "You can not make it Inactive. Please select another status."
       VIEW-AS ALERT-BOX ERROR.

        APPLY "entry" TO cust.active .
      RETURN ERROR.
    END.      
    FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ cust.company
          AND oe-ord.cust-no EQ cust.cust-no
          AND INDEX("CZ",oe-ord.stat) EQ 0 NO-ERROR.
    IF AVAIL oe-ord THEN DO:
        MESSAGE 
        "Customer " + cust.cust-no + " - " + cust.NAME 
        " has at least one Open Order ." SKIP 
        "You can not make it Inactive. Please select another status."
       VIEW-AS ALERT-BOX ERROR.

        APPLY "entry" TO cust.active .
      RETURN ERROR.
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
     DISABLE {&faxFields} rd_inv-meth fi_flat-comm  fl_custemail.
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
   cust.email    = TRIM(fl_custemail).  /* gdm - 05180924 */

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
                shipto.tax-mandatory = cust.sort EQ "Y".
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
  DO WITH FRAME {&FRAME-NAME}:

     if adm-new-record and adm-adding-record THEN /*adding, not copying*/
     DO: /*ESP need both statements due to how cust.fax is populated*/
        ASSIGN
        faxnumber:SCREEN-VALUE = ""
        faxnumber = ""
        rd_inv-meth:SCREEN-VALUE  = STRING(cust.inv-meth)
        fi_flat-comm:SCREEN-VALUE = "" 
        fl_custemail:SCREEN-VALUE = "".
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
  END.

  RUN display-active.

  IF AVAIL cust THEN
      ASSIGN
    cAccount = cust.bank-acct   
    cShift   = cust.SwiftBIC    
    cRouting = cust.Bank-RTN  .

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

  def buffer bf-cust for cust.
  /*def buffer bf-shipto for shipto.
  def buffer bf-soldto for soldto.*/

  /* Code placed here will execute PRIOR to standard behavior. */
  assign
   ls-prev-cust-no = cust.cust-no
   ll-prev-cr-hold = cust.cr-hold
   ls-prev-sman = cust.sman  
   ll-new-record = FALSE .

    /* 33482 - Ensure blank record is not saved - MYT - 08/28/18 */
    IF adm-new-record 
    AND cust.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN DO:
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
        RETURN NO-APPLY.
    END.

  RUN cust-zip.
  {&methods/lValidateError.i YES}
  do with frame {&frame-name}:
     IF adm-new-record THEN DO:
        ll-new-record = TRUE.
        IF cust.cust-no:SCREEN-VALUE = "" THEN DO:
           MESSAGE "Customer number must be entered." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO cust.cust-no.
           RETURN.
        END.
        FIND FIRST bf-cust WHERE bf-cust.company = gcompany 
                             AND bf-cust.cust-no = cust.cust-no:SCREEN-VALUE
                             AND RECID(bf-cust) <> RECID(cust)
                             NO-LOCK NO-ERROR.
        IF AVAIL bf-cust THEN DO:
           MESSAGE "Customer already exist. Try other number." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO cust.cust-no.
           RETURN.
        END.
        IF v-custsize = "Hughes" and
           v-cust-length <> LENGTH(cust.cust-no:SCREEN-VALUE) THEN DO:
           MESSAGE "Your NK1 Setting for customer length is set to" v-cust-length "characters, so you" SKIP
               "must change your setting or use a" v-cust-length "character length customer number."
               VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO cust.cust-no.
           RETURN.
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

     RUN check-cr-bal NO-ERROR .
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-custtype NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-custcsr NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

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
     if /*cust.carrier:screen-value <> "" and*/
        not can-find(first carrier where carrier.company = gcompany and 
                                     carrier.loc = cust.loc:screen-value and
                                     carrier.carrier = cust.carrier:screen-value)
     then do:
        message "Invalid Carrier Code. Try Help." view-as alert-box error.
        apply "entry" to cust.carrier.
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


  /* ============== end of validations ==================*/
  ASSIGN cOld-fob = cust.fob-code
         cOld-freight = cust.frt-pay  .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN disable-fields.

 
  IF NOT adm-new-record             AND
    cust.cr-hold NE ll-prev-cr-hold THEN
  FOR EACH oe-ord
      WHERE oe-ord.company             EQ cocode
        AND oe-ord.cust-no             EQ cust.cust-no
        AND INDEX("CDZWV",oe-ord.stat) EQ 0:
    oe-ord.stat = IF cust.cr-hold THEN "H" ELSE "A".
  END.
  IF NOT adm-new-record AND ls-prev-sman <> cust.sman  THEN DO:
     MESSAGE "Update Sales Rep on Estimate, Quotes and Ship Tos ? " VIEW-AS ALERT-BOX QUESTION
         BUTTON YES-NO UPDATE ll-ans AS LOG .
     IF ll-ans THEN 
         RUN update-sman.
  END.

  IF NOT adm-new-record AND (cOld-fob NE cust.fob-code OR  cOld-freight NE cust.frt-pay) THEN DO:
      RUN fg/custfobudt.w(ROWID(cust)) .
  END.


  IF ll-new-record THEN DO:
    /* Reposition browse to new record so other tabs are refreshed */
    {methods/run_link.i "RECORD-SOURCE" "repo-query2" "(INPUT ROWID(cust))"} 
  END.

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

    FIND FIRST reftable {&where-flat-comm} NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.reftable = "cust.flat-comm"
       reftable.company  = cust.company
       reftable.loc      = ""
       reftable.code     = cust.cust-no.
    END.
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
   */
   RUN windows/w-updsmn.w (cust.cust-no).

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
  {methods/lValidateError.i YES}
  FIND FIRST sman
        WHERE sman.company EQ cocode
          AND sman.sman    EQ cust.sman:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        NO-LOCK NO-ERROR.

    IF NOT AVAIL sman THEN DO:
       MESSAGE "Invalid Sales Rep. Try help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO cust.sman.
       RETURN ERROR.
    END.
    sman_sname:SCREEN-VALUE = sman.sNAME.

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

