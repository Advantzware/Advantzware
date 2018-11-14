&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-report NO-UNDO LIKE oe-ordl
       field rec-id as recid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: oe\b-ordrel.w

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

&SCOPED-DEFINE programVersion
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i new shared }
ASSIGN cocode = g_company
       locode = g_loc.

DEF BUFFER ref-lot-no FOR reftable.
DEF BUFFER ref-sell-price FOR reftable.

DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR ls-rel-stat AS cha LABEL "" FORM "x" NO-UNDO.
DEF VAR lv-rel-recid AS RECID NO-UNDO.
DEF NEW SHARED BUFFER xoe-ordl FOR oe-ordl.
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.
DEF NEW SHARED VAR out-recid AS RECID NO-UNDO.
DEF NEW SHARED VAR relh-recid AS RECID NO-UNDO.
DEF NEW SHARED VAR v-auto AS LOG NO-UNDO.
DEF NEW SHARED VAR nufile AS LOG NO-UNDO.   /* for jc-calc.p */
DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF VAR li-ship-no AS INT NO-UNDO.  /* if ship-to is changed */
DEF VAR v-inv-ship AS LOG INIT NO NO-UNDO .  /* Invoice only release */
DEF VAR v-qty-inv-only AS INT NO-UNDO.
DEF VAR v-totqty-inv-only AS INT NO-UNDO.

DEF VAR ll-unposted AS LOG NO-UNDO.
DEF VAR ls-po AS cha NO-UNDO.
DEF VAR ll-canceled AS LOG NO-UNDO.
DEF VAR lv-stat AS cha NO-UNDO.
DEF VAR ld-date AS DATE NO-UNDO.
DEF VAR ll-skip AS LOG NO-UNDO.
DEF VAR lv-s-codes AS CHAR NO-UNDO.
DEF VAR lv-s-dscrs AS CHAR NO-UNDO.
DEF VAR lv-cust-x LIKE cust.cust-no NO-UNDO.
DEF VAR ll-transfer AS LOG NO-UNDO.
DEF VAR v-browse-in-update AS LOG NO-UNDO.
DEF VAR v-cust-no AS CHAR NO-UNDO.
DEF VAR v-last-shipto AS CHAR NO-UNDO.
DEF VAR l-update-reason-perms AS LOG NO-UNDO.
DEF VAR adm-cur-state AS CHAR NO-UNDO.
DEF VAR oeDateChange-log AS LOG NO-UNDO.
DEFINE VARIABLE oeDateChange-char AS CHARACTER   NO-UNDO.
DEF VAR v-rtn-char AS CHAR NO-UNDO.
DEF VAR v-rec-found AS LOG NO-UNDO.
DEF VAR v-disp-rel-qty AS DEC NO-UNDO.
DEF VAR v-scr-s-code AS CHAR NO-UNDO.
DEF VAR vrRElh       AS ROWID NO-UNDO.
DEF VAR oeDateAuto-log AS LOG NO-UNDO.
DEF VAR oeDateAuto-char AS CHAR NO-UNDO.
DEFINE VARIABLE iTTReportLine AS INTEGER NO-UNDO.

DEF VAR iocPrompt AS CHAR NO-UNDO.
DEF VAR lr-rel-lib AS HANDLE NO-UNDO.

 DEFINE VARIABLE run-proc AS CHARACTER.
DEFINE SHARED VARIABLE Persistent-Handle AS HANDLE.
DEFINE VARIABLE is-running AS LOGICAL NO-UNDO.
DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE whColumn  AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE colNumber AS INTEGER       NO-UNDO.
DEFINE VARIABLE dtPrevDueDate AS DATE NO-UNDO.
DEFINE VARIABLE cDueDateChgReason AS CHARACTER NO-UNDO.
DEFINE VARIABLE lJustDeletedLine AS LOGICAL NO-UNDO.
DEFINE VARIABLE oeBolPrompt-char AS CHARACTER NO-UNDO .
DEFINE VARIABLE oeBolPrompt-log AS LOGICAL NO-UNDO .
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO .
DEFINE VARIABLE clvtext AS CHARACTER NO-UNDO .


RUN sys/ref/s-codes.p (OUTPUT lv-s-codes, OUTPUT lv-s-dscrs).

/* DEF TEMP-TABLE tt-report NO-UNDO                                 */
/*     LIKE report FIELD phantom AS LOG                             */
/*     FIELD po-no LIKE oe-rel.po-no                                */
/*     FIELD qty LIKE oe-rel.qty                                    */
/*     FIELD printed AS LOG                                         */
/*     FIELD s-code AS CHAR                                         */
/*     FIELD lot-no AS CHAR                                         */
/*     FIELD sell-price AS DEC                                      */
/*     FIELD freight-pay AS CHAR                                    */
/*     FIELD fob AS CHAR                                            */
/*     FIELD zero-sprice AS LOG                                     */
/*     FIELD release# LIKE oe-relh.release#                         */
/*     FIELD bol-date AS CHAR                                       */
/*     FIELD due-date AS DATE COLUMN-LABEL "Due Date"               */
/*     FIELD due-date-user AS CHAR COLUMN-LABEL "Due Dt Chg User"   */
/*     FIELD due-date-reason AS CHAR COLUMN-LABEL "Due Dt Chg Rsn". */

{oe/chkordl.i NEW}
{oe/relemail.i NEW}

DO TRANSACTION:
  {sys/inc/oeship.i}
  {sys/inc/oereleas.i}
  {sys/ref/relpost.i}
  {sys/inc/addxfer.i}
  {sys/inc/reltype.i}
END.
{sys/inc/funcToWorkDay.i}
DEF VAR v-access-close AS LOG NO-UNDO.
DEF VAR v-access-list AS CHAR NO-UNDO.

RUN methods/prgsecur.p
    (INPUT "OEDateChg",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT l-update-reason-perms, /* Allowed? Yes/NO */
     OUTPUT v-access-close, /* used in template/windows.i  */
     OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */


RUN sys/ref/nk1look.p (cocode, "oeDateChange", "L", NO, NO, "", "", 
                          OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateChange-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (cocode, "oeDateChange", "C", NO, NO, "", "", 
                          OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateChange-char = v-rtn-char NO-ERROR.
    
RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
oeDateAuto-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "C" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
oeDateAuto-char = v-rtn-char NO-ERROR.    

RUN sys/ref/nk1look.p (INPUT cocode, "OEBOLPrompt", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
     oeBolPrompt-log = LOGICAL(cRtnChar) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEBOLPrompt", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeBolPrompt-char = cRtnChar NO-ERROR. 

/* Could be implemented later */
/* DEF VAR bt_addnote AS HANDLE.                                                       */
/* CREATE BUTTON bt_addnote ASSIGN LABEL = "Notes" VISIBLE = TRUE SENSITIVE = TRUE     */
/*     TRIGGERS:                                                                       */
/*        ON 'choose'                                                                  */
/*        DO:                                                                          */
/*            IF AVAIL(oe-rel) THEN                                                    */
/*               RUN windows/specnot5.w (INPUT oe-rel.rec_key, INPUT PROGRAM-NAME(1)). */
/*            RETURN.                                                                  */
/*        END.                                                                         */
/*     END TRIGGERS.                                                                   */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-rel tt-report

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-report.opened oe-rel.s-code ~
oe-rel.ship-id oe-rel.stat oe-rel.carrier oe-rel.tot-qty oe-rel.qty ~
tt-report.po-no tt-report.lot-no tt-report.prom-date tt-report.stat ~
oe-rel.ship-addr[1] oe-rel.ship-city oe-rel.ship-state tt-report.price ~
tt-report.whsed oe-ordl.disc oe-ordl.t-price tt-report.frt-pay ~
tt-report.flute oe-rel.spare-char-1 oe-rel.spare-char-2 oe-rel.spare-char-3 ~
tt-report.q-rel oe-rel.r-no oe-rel.link-no tt-report.job-start-date ~
tt-report.qty tt-report.prom-code tt-report.pr-uom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table oe-rel.s-code ~
oe-rel.ship-id oe-rel.stat oe-rel.carrier oe-rel.tot-qty oe-rel.qty ~
tt-report.po-no tt-report.lot-no tt-report.prom-date tt-report.stat ~
oe-rel.ship-addr[1] tt-report.price tt-report.whsed tt-report.frt-pay ~
tt-report.flute oe-rel.spare-char-1 oe-rel.spare-char-2 tt-report.q-rel 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table oe-rel tt-report
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table oe-rel
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br_table tt-report
&Scoped-define QUERY-STRING-br_table FOR EACH oe-rel WHERE oe-rel.company = oe-ordl.company ~
  AND oe-rel.ord-no = oe-ordl.ord-no ~
  AND oe-rel.i-no = oe-ordl.i-no ~
  AND oe-rel.line = oe-ordl.line NO-LOCK, ~
      EACH tt-report WHERE tt-report.rec-id = recid(oe-rel) NO-LOCK ~
    BY tt-report.del-zone ~
       BY oe-rel.po-no ~
        BY oe-rel.ship-no ~
         BY oe-rel.qty
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH oe-rel WHERE oe-rel.company = oe-ordl.company ~
  AND oe-rel.ord-no = oe-ordl.ord-no ~
  AND oe-rel.i-no = oe-ordl.i-no ~
  AND oe-rel.line = oe-ordl.line NO-LOCK, ~
      EACH tt-report WHERE tt-report.rec-id = recid(oe-rel) NO-LOCK ~
    BY tt-report.del-zone ~
       BY oe-rel.po-no ~
        BY oe-rel.ship-no ~
         BY oe-rel.qty.
&Scoped-define TABLES-IN-QUERY-br_table oe-rel tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-br_table oe-rel
&Scoped-define SECOND-TABLE-IN-QUERY-br_table tt-report


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br_table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|ASI.oe-rel.company
Carrier||y|ASI.oe-rel.Carrier
r-no||y|ASI.oe-rel.r-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,Carrier,r-no"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-rel-qty B-table-Win 
FUNCTION get-rel-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-rel-stat B-table-Win 
FUNCTION get-rel-stat RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-tot-qty B-table-Win 
FUNCTION get-tot-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY .86 TOOLTIP "PopUp Calendar".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      oe-rel, 
      tt-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      tt-report.opened COLUMN-LABEL "Prt" FORMAT "Y/N":U WIDTH 5.4
            COLUMN-FONT 0
      oe-rel.s-code COLUMN-LABEL "S/I" FORMAT "X(12)":U
      VIEW-AS COMBO-BOX INNER-LINES 4 
          LIST-ITEM-PAIRS "B-Both","B",
                     "S-Ship","S",
                     "I-Invoice","I",
                     "T-Transfer","T"
          DROP-DOWN-LIST
      oe-rel.ship-id COLUMN-LABEL "Ship To" FORMAT "x(8)":U COLUMN-FONT 0
      oe-rel.stat COLUMN-LABEL "S" FORMAT "X(15)":U  WIDTH 28
      VIEW-AS COMBO-BOX INNER-LINES 8 
          LIST-ITEM-PAIRS "S-Scheduled","S",
                     "L-Late","L",
                     "I-Invoice Per Terms","I",
                     "A-Actual","A",
                     "P-Posted","P",
                     "B-Backorder","B",
                     "Z-Posted BOL","Z",
                     "C-Completed","C"
          DROP-DOWN-LIST
      oe-rel.carrier COLUMN-LABEL "Via" FORMAT "x(5)":U COLUMN-FONT 0
      oe-rel.tot-qty COLUMN-LABEL "Sched Qty" FORMAT "->>,>>>,>>9":U
      oe-rel.qty COLUMN-LABEL "Actual Qty" FORMAT "->>,>>>,>>9":U
            WIDTH 16 COLUMN-FONT 0
      tt-report.po-no COLUMN-LABEL "Customer PO#" FORMAT "x(15)":U
            COLUMN-FONT 0
      tt-report.lot-no COLUMN-LABEL "Customer Lot #" FORMAT "x(15)":U
            WIDTH 21.8 COLUMN-FONT 0
      tt-report.prom-date COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
      tt-report.stat COLUMN-LABEL "Rel Date" FORMAT "99/99/9999":U
            WIDTH 18.4 COLUMN-FONT 0
      oe-rel.ship-addr[1] COLUMN-LABEL "Ship To Address" FORMAT "x(26)":U
            COLUMN-FONT 0
      oe-rel.ship-city FORMAT "x(15)":U COLUMN-FONT 0
      oe-rel.ship-state FORMAT "x(2)":U COLUMN-FONT 0
      tt-report.price COLUMN-LABEL "Sell Price" FORMAT ">>,>>>,>>9.99<<<<":U
            WIDTH 16 COLUMN-FONT 0
      tt-report.whsed COLUMN-LABEL "$0" FORMAT "Y/N":U COLUMN-FONT 0
      oe-ordl.disc FORMAT "(>>>,>>9.99)":U
      oe-ordl.t-price COLUMN-LABEL "Ext Order Price" FORMAT "->>,>>>,>>9.99":U
      tt-report.frt-pay COLUMN-LABEL "Frt Pay" FORMAT "x(8)":U
            COLUMN-FONT 0
      tt-report.flute COLUMN-LABEL "FOB" FORMAT "x(1)":U COLUMN-FONT 0
      oe-rel.spare-char-1 COLUMN-LABEL "Ship From" FORMAT "x(5)":U
      oe-rel.spare-char-2 COLUMN-LABEL "Dt Chg Reason" FORMAT "x(30)":U
            WIDTH 23
      oe-rel.spare-char-3 COLUMN-LABEL "Dt Chg User" FORMAT "x(8)":U
      tt-report.q-rel COLUMN-LABEL "Release #" FORMAT ">>>>>>9":U
            WIDTH 13.2 COLUMN-FONT 0
      oe-rel.r-no COLUMN-LABEL "Seq. #" FORMAT ">>>>>>>>9":U WIDTH 15
      oe-rel.link-no COLUMN-LABEL "Int. Release" FORMAT ">>>>>>>9":U
            WIDTH 16.6
      tt-report.job-start-date COLUMN-LABEL "Shp Date" FORMAT "99/99/9999":U
            WIDTH 14.4
      tt-report.qty FORMAT "->>,>>>,>>9.9<<":U
      tt-report.prom-code COLUMN-LABEL "Due Dt Chg Usr" FORMAT "x(5)":U
      tt-report.pr-uom COLUMN-LABEL "Due Dt Chg Rsn" FORMAT "x(4)":U
  ENABLE
      oe-rel.s-code HELP "B=Bill and Ship, I=Invoice Only, S=Ship Only, T=Transfer"
      oe-rel.ship-id
      oe-rel.stat
      oe-rel.carrier
      oe-rel.tot-qty
      oe-rel.qty
      tt-report.po-no
      tt-report.lot-no
      tt-report.prom-date HELP "Enter the Due Date."
      tt-report.stat
      oe-rel.ship-addr[1]
      tt-report.price HELP "Enter Sell Price"
      tt-report.whsed
      tt-report.frt-pay
      tt-report.flute
      oe-rel.spare-char-1
      oe-rel.spare-char-2
      tt-report.q-rel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 147 BY 13.57
         FONT 0 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnCalendar AT ROW 1.91 COL 125
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 0.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.oe-ordl
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-report T "?" NO-UNDO asi oe-ordl
      ADDITIONAL-FIELDS:
          field rec-id as recid
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 15.33
         WIDTH              = 147.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br_table btnCalendar F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnCalendar:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.oe-rel WHERE ASI.oe-ordl ...,Temp-Tables.tt-report WHERE ASI.oe-rel ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ",,"
     _OrdList          = "Temp-Tables.tt-report.del-zone|yes,ASI.oe-rel.po-no|yes,ASI.oe-rel.ship-no|yes,ASI.oe-rel.qty|yes"
     _JoinCode[1]      = "ASI.oe-rel.company = ASI.oe-ordl.company
  AND ASI.oe-rel.ord-no = ASI.oe-ordl.ord-no
  AND ASI.oe-rel.i-no = ASI.oe-ordl.i-no
  AND ASI.oe-rel.line = ASI.oe-ordl.line"
     _JoinCode[2]      = "Temp-Tables.tt-report.rec-id = recid(oe-rel)"
     _FldNameList[1]   > Temp-Tables.tt-report.opened
"Temp-Tables.tt-report.opened" "Prt" "Y/N" "logical" ? ? 0 ? ? ? no ? no no "5.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.oe-rel.s-code
"asi.oe-rel.s-code" "S/I" "!" "character" ? ? ? ? ? ? yes "B=Bill and Ship, I=Invoice Only, S=Ship Only, T=Transfer" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-rel.ship-id
"ASI.oe-rel.ship-id" "Ship To" ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-rel.stat
"ASI.oe-rel.stat" "S" ? "character" ? ? ? ? ? ? yes ? no no "2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-rel.carrier
"ASI.oe-rel.carrier" "Via" ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-rel.tot-qty
"ASI.oe-rel.tot-qty" "Sched Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-rel.qty
"ASI.oe-rel.qty" "Actual Qty" ? "decimal" ? ? 0 ? ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tt-report.po-no
"Temp-Tables.tt-report.po-no" "Customer PO#" ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tt-report.lot-no
"Temp-Tables.tt-report.lot-no" "Customer Lot #" ? "character" ? ? 0 ? ? ? yes ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.tt-report.prom-date
"Temp-Tables.tt-report.prom-date" "Due Date" ? "date" ? ? ? ? ? ? yes "Enter the Due Date." no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.tt-report.stat
"Temp-Tables.tt-report.stat" "Rel Date" "99/99/9999" "character" ? ? 0 ? ? ? yes ? no no "18.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-rel.ship-addr[1]
"ASI.oe-rel.ship-addr[1]" "Ship To Address" "x(26)" "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.oe-rel.ship-city
"ASI.oe-rel.ship-city" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.oe-rel.ship-state
"ASI.oe-rel.ship-state" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.tt-report.price
"Temp-Tables.tt-report.price" "Sell Price" ? "decimal" ? ? 0 ? ? ? yes "Enter Sell Price" no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.tt-report.whsed
"Temp-Tables.tt-report.whsed" "$0" "Y/N" "logical" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   = ASI.oe-ordl.disc
     _FldNameList[18]   > ASI.oe-ordl.t-price
"ASI.oe-ordl.t-price" "Ext Order Price" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.tt-report.frt-pay
"Temp-Tables.tt-report.frt-pay" "Frt Pay" "x(8)" "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Temp-Tables.tt-report.flute
"Temp-Tables.tt-report.flute" "FOB" "x(1)" "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.oe-rel.spare-char-1
"ASI.oe-rel.spare-char-1" "Ship From" "x(5)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.oe-rel.spare-char-2
"ASI.oe-rel.spare-char-2" "Dt Chg Reason" "x(30)" "character" ? ? ? ? ? ? yes ? no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.oe-rel.spare-char-3
"ASI.oe-rel.spare-char-3" "Dt Chg User" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > Temp-Tables.tt-report.q-rel
"Temp-Tables.tt-report.q-rel" "Release #" ">>>>>>9" "integer" ? ? 0 ? ? ? yes ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.oe-rel.r-no
"ASI.oe-rel.r-no" "Seq. #" ">>>>>>>>9" "integer" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > ASI.oe-rel.link-no
"ASI.oe-rel.link-no" "Int. Release" ">>>>>>>9" "integer" ? ? ? ? ? ? no ? no no "16.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > Temp-Tables.tt-report.job-start-date
"Temp-Tables.tt-report.job-start-date" "Shp Date" ? "date" ? ? ? ? ? ? no "Enter the Ship Date" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   = Temp-Tables.tt-report.qty
     _FldNameList[29]   > Temp-Tables.tt-report.prom-code
"Temp-Tables.tt-report.prom-code" "Due Dt Chg Usr" "x(5)" "character" ? ? ? ? ? ? no "Enter code the Due Date Reason Code" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > Temp-Tables.tt-report.pr-uom
"Temp-Tables.tt-report.pr-uom" "Due Dt Chg Rsn" ? "character" ? ? ? ? ? ? no "Enter the Due Date Change Reason" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
 DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
    
  RUN oe/d-ordrel.w (ROWID(oe-rel),ROWID(oe-ordl), "update", OUTPUT lv-rowid) .
  RUN reopen-query .
  RUN repo-query (ROWID(oe-rel)).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON RETURN OF br_table IN FRAME F-Main
ANYWHERE
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO: 
  DEF BUFFER bfOeRel FOR oe-rel.
  DEF VAR v-stat AS CHAR NO-UNDO.
  v-stat = get-rel-stat().
  IF v-stat NE oe-rel.stat AND v-stat GT "" THEN
  DO:
    /*FIND oe-rel WHERE
         ROWID(oe-rel) EQ ROWID(b-oe-rel) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      */
    IF AVAIL oe-rel THEN
    DO:
       FIND bfOeRel WHERE ROWID(bfOeRel) EQ  ROWID(oe-rel) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       IF AVAIL bfOeRel THEN
       bfOeRel.stat = v-stat.
       FIND CURRENT bfOeRel NO-LOCK NO-ERROR.
    END.
    oe-rel.stat:SCREEN-VALUE IN BROWSE br_table =  v-stat.

  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
/*   {src/adm/template/brsleave.i} */
        {est/brsleave.i }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON SCROLL-NOTIFY OF br_table IN FRAME F-Main
DO:
   RUN calendarPlacement.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  IF NOT v-browse-in-update THEN RUN set-buttons.
  DEF VAR v-stat AS CHAR NO-UNDO.


  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

  IF v-stat NE oe-rel.stat THEN
  DO:
    /*FIND oe-rel WHERE
         ROWID(oe-rel) EQ ROWID(b-oe-rel) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      */
    IF AVAIL oe-rel THEN
    DO:
       FIND CURRENT oe-rel EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       IF AVAIL oe-rel THEN
         oe-rel.stat = v-stat.
       FIND CURRENT oe-rel NO-LOCK NO-ERROR.
    END.

  END.

/* lJustDeletedLine needed because adm-adding-record not accurate after and add and delete */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"ordrel-source", OUTPUT char-hdl) NO-ERROR.
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
      IF  ( adm-new-record AND adm-adding-record AND NOT lJustDeletedLine) THEN
        RUN reset-button IN WIDGET-HANDLE(char-hdl) (NO).
      ELSE
        RUN reset-button IN WIDGET-HANDLE(char-hdl) (YES).
  END.
  
  ASSIGN 
    lJustDeletedLine = NO    
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


SESSION:DATA-ENTRY-RETURN = YES.
{sys/inc/f3help.i}
{sys/inc/oeinq.i}
{sa/sa-sls01.i}


 RUN-PROC = "sbo/oerel-recalc-act.p".
{methods/smartrun.i}
   lr-rel-lib = phandle.    
lv-cust-x = "".



FOR EACH cust NO-LOCK
    WHERE cust.company EQ cocode
      AND cust.active  EQ "X":
  lv-cust-x = cust.cust-no.
  LEAVE.
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

IF USERID("nosweat") <> "ASI" THEN
  ASSIGN
  oe-rel.stat:READ-ONLY = YES
  oe-rel.qty:READ-ONLY = YES.
  
IF NOT (oeDateAuto-log AND OeDateAuto-Char EQ "Colonial") THEN DO:
    DO colNumber = 1 TO BROWSE {&browse-name}:NUM-COLUMNS:
 
        whColumn = BROWSE {&browse-name}:GET-BROWSE-COLUMN(colNumber) NO-ERROR.

        IF whColumn:LABEL = "Due Date"
          OR whColumn:LABEL BEGINS "Due Dt Chg"
          THEN DO:
 
          ASSIGN whColumn:VISIBLE = FALSE.
     
        END.
    END.
END.
{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-set-releases B-table-Win 
PROCEDURE add-set-releases :
/*------------------------------------------------------------------------------
Purpose:
Parameters:  <none>
Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcShipTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipiRelQty   AS INT  NO-UNDO.
DEFINE INPUT PARAMETER ipdtRelDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER iplRelease  AS LOG  NO-UNDO.
DEFINE INPUT PARAMETER ipdtDelDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcPoNo     AS CHAR NO-UNDO.

/* Create new releases */
/* ********** code from d-oeitem ********* */

DEFINE VARIABLE v-qty-sum    AS INTEGER        NO-UNDO.
DEFINE VARIABLE v-nxt-r-no   AS INTEGER        INIT 1 NO-UNDO.
DEFINE VARIABLE v-lst-rel    AS DATE           NO-UNDO.
DEFINE VARIABLE v-pct-chg    AS DECIMAL        NO-UNDO.
DEFINE VARIABLE v-ship-id    LIKE oe-rel.ship-id NO-UNDO.
DEFINE VARIABLE v-num-shipto AS INTEGER        NO-UNDO.
DEFINE VARIABLE v-relType    AS cha            NO-UNDO.
DEFINE VARIABLE v-ship-from  AS CHARACTER      NO-UNDO.
DEFINE VARIABLE v-relflg2    AS LOGICAL        INIT YES NO-UNDO.

DEF BUFFER bf-ordl FOR oe-ordl.
DEF BUFFER bf-rel FOR oe-rel.
DEF BUFFER xoe-ord FOR oe-ord.

IF NOT AVAIL oe-ordl THEN
RETURN.

IF NOT AVAIL oe-ord THEN
FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
    AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  
IF AVAIL oe-ord THEN
  FIND xoe-ord WHERE ROWID(xoe-ord) = ROWID(oe-ord) NO-LOCK NO-ERROR.

FOR EACH fg-set WHERE fg-set.set-no = oe-ordl.i-no
  AND fg-set.company = oe-ordl.company NO-LOCK.
  FIND FIRST bf-ordl WHERE bf-ordl.company EQ oe-ordl.company
    AND bf-ordl.ord-no EQ oe-ordl.ord-no
    AND bf-ordl.i-no   EQ fg-set.part-no
  NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-ordl THEN
  NEXT.
     
  ASSIGN v-qty-sum  = 0
         v-ship-id = "".

  /* Defaults taken from d-oeitem.w */
  v-ship-id = ipcShipTo.
  v-relflg2 = YES.
  
  /* prompt is in ask-release-questions */
  
  IF v-relflg2 THEN DO:
    {oe/oe-rel.a &fil="bf-ordl"}.
    
  END.
    
  ASSIGN lv-qty = bf-ordl.qty.
  /* If not multi ship-to */
  
  FIND FIRST shipto WHERE shipto.company EQ cocode 
    AND shipto.cust-no EQ xoe-ord.cust-no 
    AND shipto.ship-id EQ v-ship-id
  NO-LOCK NO-ERROR.
  IF NOT AVAIL shipto THEN
      FIND FIRST shipto WHERE shipto.company EQ cocode
        AND shipto.cust-no EQ xoe-ord.cust-no
      NO-LOCK NO-ERROR.

  IF AVAIL shipto THEN DO:
    
   
    IF v-relflg2 THEN DO:
        ASSIGN  
          oe-rel.ship-no      = shipto.ship-no
          oe-rel.ship-id      = shipto.ship-id
          oe-rel.ship-addr[1] = shipto.ship-addr[1]
          oe-rel.ship-addr[2] = shipto.ship-addr[2]
          oe-rel.ship-city    = shipto.ship-city
          oe-rel.ship-state   = shipto.ship-state
          oe-rel.ship-zip     = shipto.ship-zip
          oe-rel.ship-i[1]    = shipto.notes[1]
          oe-rel.ship-i[2]    = shipto.notes[2]
          oe-rel.ship-i[3]    = shipto.notes[3]
          oe-rel.ship-i[4]    = shipto.notes[4]
          oe-rel.spare-char-1 = shipto.loc.
        RUN CopyShipNote (shipto.rec_key, oe-rel.rec_key).
    END.  
    /* check that itemfg-loc exists */
    IF oe-rel.spare-char-1 GT "" THEN
    RUN fg/chkfgloc.p (INPUT oe-rel.i-no, INPUT oe-rel.spare-char-1).
    
    
    /* if add mode then use default carrier */
    IF TRUE THEN DO:
      FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "OECARIER"
      NO-LOCK NO-ERROR.
      IF NOT AVAIL sys-ctrl THEN DO:
        CREATE sys-ctrl.
        ASSIGN sys-ctrl.company  = cocode
        sys-ctrl.NAME     = "OECARIER"
        sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
        sys-ctrl.char-fld = "ShipTo".
        
        DO WHILE TRUE:
          MESSAGE "Default Shipping Carrier from Header or Shipto?"
          UPDATE sys-ctrl.char-fld.
          IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "ShipTo" THEN LEAVE.
        END.
      END.
     
      IF v-relflg2 THEN
      oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto.carrier
                         ELSE xoe-ord.carrier.
    END. /* If true */
    
  END. /* avail shipto */


  oe-rel.s-code          = "S".     

  /* Set oe-rel quantity based on set part quantity */
  oe-rel.tot-qty = ipiRelQty * fg-set.qtyPerSet.
  oe-rel.rel-date = ipdtRelDate.
  IF ipcPoNo GT "" THEN
      oe-rel.po-no = ipcPoNo.

  IF iplRelease THEN DO:
    FOR EACH bf-rel WHERE bf-rel.company EQ oe-rel.company
      AND bf-rel.ord-no EQ oe-rel.ord-no
      AND bf-rel.LINE   EQ oe-rel.LINE
      AND bf-rel.i-no   EQ oe-rel.i-no
      AND bf-rel.rel-date EQ ipdtDelDate
      AND LOOKUP(bf-rel.stat, "S,I,L") GT 0
      EXCLUSIVE-LOCK:

      DELETE bf-rel.

    END.
  END.

  /* Run Freight calculation  */
  /*       RUN oe/oe-frtcl.p. */
END. /* each set part-no to create releases for */
/* ********* end code from d-oeitem *******/
/* Remove old releases for this date */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "oe-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-report-file B-table-Win 
PROCEDURE build-report-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-nxt-r-no LIKE oe-rel.r-no NO-UNDO.
DEF VAR lv-qty AS INT NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-s-code LIKE oe-rell.s-code EXTENT 2 NO-UNDO.


DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-rell-exc FOR oe-rell.
DEF BUFFER b-oe-rel  FOR oe-rel.


  ll-transfer = CAN-FIND(FIRST oe-ord
                         WHERE oe-ord.company EQ oe-ordl.company
                           AND oe-ord.ord-no  EQ oe-ordl.ord-no
                           AND oe-ord.type    EQ "T").

  RUN delete-phantoms.

  FOR EACH tt-report:
    DELETE tt-report.
  END.

  /* RUN oe/cleanrel.p (ROWID(oe-ordl)). */

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item
      
      BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.po-no

      TRANSACTION:

    IF LAST-OF(oe-rel.po-no) OR oe-rel.rel-no EQ 0 THEN DO:
        RUN create-report-record (ROWID(oe-rel), NO).        
    END.
  END.

  FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company  EQ oe-ordl.company
        AND oe-boll.ord-no   EQ oe-ordl.ord-no
        AND oe-boll.i-no     EQ oe-ordl.i-no
        AND oe-boll.line     EQ oe-ordl.line
      USE-INDEX ord-no,

      FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK,
      
      FIRST oe-rell NO-LOCK
      WHERE oe-rell.company  EQ oe-boll.company
        AND oe-rell.ord-no   EQ oe-boll.ord-no
        AND oe-rell.line     EQ oe-boll.line
        AND oe-rell.i-no     EQ oe-boll.i-no
        AND oe-rell.r-no     EQ oe-boll.r-no
        AND oe-rell.rel-no   EQ oe-boll.rel-no
        AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
        AND oe-rell.po-no    EQ oe-boll.po-no
      USE-INDEX ord-no,

      FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-boll.r-no
      
      BREAK BY oe-boll.r-no
            BY oe-boll.rel-no
            BY oe-boll.b-ord-no
            BY oe-boll.po-no

      TRANSACTION:

    IF FIRST-OF(oe-boll.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-boll.qty.

    IF LAST-OF(oe-boll.po-no) AND lv-qty NE 0 THEN DO:
      RELEASE oe-rel.
      IF oe-rell.link-no NE 0 THEN
      FIND oe-rel NO-LOCK
          WHERE oe-rel.r-no EQ oe-rell.link-no
          USE-INDEX seq-no NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.link-no  EQ oe-rell.r-no
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
          USE-INDEX link NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
          USE-INDEX ord-item NO-ERROR.

      IF AVAIL oe-rel THEN
      FIND CURRENT oe-rel EXCLUSIVE NO-ERROR NO-WAIT.

      IF AVAIL oe-rel THEN DO:
        FIND CURRENT oe-rel.
        ASSIGN
         oe-rel.link-no  = oe-rell.r-no
         oe-rel.rel-no   = oe-rell.rel-no
         oe-rel.b-ord-no = oe-rell.b-ord-no
         oe-rel.po-no    = oe-rell.po-no
                    /* oe-rel.qty      = lv-qty - out per Joe */.

        FOR EACH b-oe-rell NO-LOCK
            WHERE b-oe-rell.company  EQ oe-rel.company
              AND b-oe-rell.r-no     EQ oe-rel.link-no
              AND b-oe-rell.ord-no   EQ oe-rel.ord-no
              AND b-oe-rell.i-no     EQ oe-rel.i-no
              AND b-oe-rell.line     EQ oe-rel.line
              AND b-oe-rell.rel-no   EQ oe-rel.rel-no
              AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
              AND b-oe-rell.po-no    EQ oe-rel.po-no
            USE-INDEX r-no:
          FIND b-oe-rell-exc WHERE ROWID(b-oe-rell-exc) EQ ROWID(b-oe-rell)
              EXCLUSIVE NO-ERROR NO-WAIT.
          IF AVAIL b-oe-rell-exc THEN b-oe-rell-exc.link-no = oe-rel.r-no.
        END.
      END.

      ELSE DO:
/*         10051225 */
/*         FIND FIRST oe-rel NO-LOCK USE-INDEX seq-no NO-ERROR.      */
/*         v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1. */
        RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
        CREATE oe-rel.
        ASSIGN
         oe-rel.company   = oe-relh.company
         oe-rel.r-no      = v-nxt-r-no
         oe-rel.link-no   = oe-rell.r-no
         oe-rel.cust-no   = oe-relh.cust-no
         oe-rel.ord-no    = oe-rell.ord-no
         oe-rel.i-no      = oe-rell.i-no
         oe-rel.line      = oe-rell.line
         oe-rel.rel-no    = oe-rell.rel-no
         oe-rel.b-ord-no  = oe-rell.b-ord-no
         oe-rel.rel-date  = oe-relh.rel-date
         oe-rel.carrier   = oe-relh.carrier
         oe-rel.ship-no   = oe-relh.ship-no
         oe-rel.ship-id   = oe-relh.ship-id
         oe-rel.ship-i[1] = oe-relh.ship-i[1]
         oe-rel.ship-i[2] = oe-relh.ship-i[2]
         oe-rel.ship-i[3] = oe-relh.ship-i[3]
         oe-rel.ship-i[4] = oe-relh.ship-i[4]
         oe-rel.po-no     = oe-boll.po-no
         oe-rel.lot-no    = oe-boll.lot-no
         oe-rel.spare-char-1 = oe-rell.loc
         oe-rel.qty       = lv-qty.
        
        RUN CopyShipNote (oe-relh.rec_key, oe-rel.rec_key).
        RUN set-lot-from-boll (INPUT ROWID(oe-rel), INPUT ROWID(oe-rell),
                               INPUT ROWID(oe-boll)).
        RUN oe/custxship.p (oe-rel.company,
                            oe-rel.cust-no,
                            oe-rel.ship-id,
                            BUFFER shipto).

        IF AVAIL shipto THEN
          ASSIGN
           oe-rel.ship-addr[1] = shipto.ship-addr[1]
           oe-rel.ship-addr[2] = shipto.ship-addr[2]
           oe-rel.ship-city    = shipto.ship-city
           oe-rel.ship-state   = shipto.ship-state
           oe-rel.ship-zip     = shipto.ship-zip.

        /* Assign qty to itemfg-loc */
        RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).

        RUN create-report-record (ROWID(oe-rel), NO).
      END.
    END.
  END.

  FOR EACH oe-rell
      WHERE oe-rell.company  EQ cocode
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.line     EQ oe-ordl.line
        AND NOT CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-rell.company
                           AND oe-boll.r-no     EQ oe-rell.r-no
                           AND oe-boll.ord-no   EQ oe-rell.ord-no
                           AND oe-boll.i-no     EQ oe-rell.i-no
                           AND oe-boll.line     EQ oe-rell.line
                           AND oe-boll.rel-no   EQ oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ oe-rell.po-no
                         USE-INDEX ord-no)
      USE-INDEX ord-no NO-LOCK,

      FIRST oe-relh NO-LOCK
      WHERE oe-relh.r-no    EQ oe-rell.r-no
        AND (oe-relh.posted EQ NO OR relpost-chr EQ "Nothing")
      
      BREAK BY oe-rell.r-no
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no

      TRANSACTION:

    IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-rell.qty.

    IF LAST-OF(oe-rell.po-no) AND lv-qty NE 0 THEN DO:
      RELEASE b-oe-rell.
      IF oe-relh.posted THEN
      FOR EACH b-oe-rell
          WHERE b-oe-rell.company EQ oe-rell.company
            AND b-oe-rell.r-no    EQ oe-rell.r-no
            AND ROWID(b-oe-rell)  NE ROWID(oe-rell)
            AND CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ b-oe-rell.company
                           AND oe-boll.ord-no   EQ b-oe-rell.ord-no
                           AND oe-boll.i-no     EQ b-oe-rell.i-no
                           AND oe-boll.line     EQ b-oe-rell.line
                           AND oe-boll.r-no     EQ b-oe-rell.r-no
                           AND oe-boll.rel-no   EQ b-oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ b-oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ b-oe-rell.po-no
                         USE-INDEX ord-no)
          USE-INDEX r-no NO-LOCK:

        LEAVE.
      END.

      IF NOT AVAIL b-oe-rell THEN DO:
        RELEASE oe-rel.
        IF oe-rell.link-no NE 0 AND oe-relh.posted THEN
        FIND oe-rel NO-LOCK
            WHERE oe-rel.r-no EQ oe-rell.link-no
            USE-INDEX seq-no NO-ERROR.
        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.link-no  EQ oe-rell.r-no
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX link NO-ERROR.
        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX ord-item NO-ERROR.

        IF NOT AVAIL oe-rel THEN DO:
/*           10051225 */
/*           FIND FIRST oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR.      */
/*           v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1. */
          RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
          CREATE oe-rel.
          ASSIGN
           oe-rel.company   = oe-relh.company
           oe-rel.r-no      = v-nxt-r-no
           oe-rel.link-no   = IF oe-relh.posted THEN oe-rell.r-no ELSE 0
           oe-rel.cust-no   = oe-relh.cust-no
           oe-rel.ord-no    = oe-rell.ord-no
           oe-rel.i-no      = oe-rell.i-no
           oe-rel.line      = oe-rell.line
           oe-rel.rel-no    = oe-rell.rel-no
           oe-rel.b-ord-no  = oe-rell.b-ord-no
           oe-rel.rel-date  = oe-relh.rel-date
           oe-rel.carrier   = oe-relh.carrier
           oe-rel.ship-no   = oe-relh.ship-no
           oe-rel.ship-id   = oe-relh.ship-id
           oe-rel.ship-i[1] = oe-relh.ship-i[1]
           oe-rel.ship-i[2] = oe-relh.ship-i[2]
           oe-rel.ship-i[3] = oe-relh.ship-i[3]
           oe-rel.ship-i[4] = oe-relh.ship-i[4]
           oe-rel.po-no     = oe-rell.po-no
           oe-rel.lot-no    = oe-rell.lot-no
           oe-rel.spare-char-1 = oe-rell.loc
           oe-rel.qty       = lv-qty.
           
          RUN CopyShipNote (oe-relh.rec_key, oe-rel.rec_key). 
          RUN oe/custxship.p (oe-rel.company,
                              oe-rel.cust-no,
                              oe-rel.ship-id,
                              BUFFER shipto).

          IF AVAIL shipto THEN
            ASSIGN
             oe-rel.ship-addr[1] = shipto.ship-addr[1]
             oe-rel.ship-addr[2] = shipto.ship-addr[2]
             oe-rel.ship-city    = shipto.ship-city
             oe-rel.ship-state   = shipto.ship-state
             oe-rel.ship-zip     = shipto.ship-zip
             .

          /* Update qty in itemfg-loc */
          RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).

          /* Make sure lot # is updated in reftable entry */
          RUN set-lot-from-boll (INPUT ROWID(oe-rel), INPUT ROWID(oe-rell),
                                 INPUT ROWID(oe-boll)).
          RUN create-report-record (ROWID(oe-rel), NO).
        END.

        ELSE DO:
          FIND CURRENT oe-rel EXCLUSIVE NO-ERROR NO-WAIT.

          IF AVAIL oe-rel THEN DO:
            IF oe-relh.posted THEN DO:
                
              ASSIGN
               oe-rel.link-no  = oe-rell.r-no
               oe-rel.rel-no   = oe-rell.rel-no
               oe-rel.b-ord-no = oe-rell.b-ord-no
               oe-rel.po-no    = oe-rell.po-no
               oe-rel.qty      = lv-qty.

              FOR EACH b-oe-rell NO-LOCK
                  WHERE b-oe-rell.company  EQ oe-rel.company
                    AND b-oe-rell.r-no     EQ oe-rel.link-no
                    AND b-oe-rell.ord-no   EQ oe-rel.ord-no
                    AND b-oe-rell.i-no     EQ oe-rel.i-no
                    AND b-oe-rell.line     EQ oe-rel.line
                    AND b-oe-rell.rel-no   EQ oe-rel.rel-no
                    AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
                    AND b-oe-rell.po-no    EQ oe-rel.po-no
                  USE-INDEX r-no:
                FIND b-oe-rell-exc WHERE ROWID(b-oe-rell-exc) EQ ROWID(b-oe-rell)
                    EXCLUSIVE NO-ERROR NO-WAIT.
                IF AVAIL b-oe-rell-exc THEN b-oe-rell-exc.link-no = oe-rel.r-no.
              END.
            END.

            ELSE DO:
              IF oe-rel.link-no NE 0 THEN oe-rel.link-no = 0.

              FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(oe-rel) NO-ERROR.
              IF AVAIL tt-report THEN tt-report.qty = lv-qty.
            END.
          END.
        END.
      END.
    END.
  END.

  

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item:

      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
      /* no calculations per Joe 
      IF INDEX("SIL",lv-stat) GT 0 OR 
         (INDEX("CZ",lv-stat) LE 0 AND oe-rel.qty EQ 0) THEN
      DO:
         REPEAT:
            FIND FIRST b-oe-rel WHERE
                 ROWID(b-oe-rel) EQ ROWID(oe-rel)
                 EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

            IF AVAIL b-oe-rel THEN
            DO:
               b-oe-rel.qty = b-oe-rel.tot-qty.
               FIND CURRENT b-oe-rel NO-LOCK.
               RELEASE b-oe-rel.
               LEAVE.
            END.
         END.
      END.
      */
      FIND FIRST tt-report WHERE 
         tt-report.rec-id  = RECID(oe-rel) NO-ERROR.
      IF NOT AVAIL tt-report THEN DO:          
          RUN create-report-record (ROWID(oe-rel), NO).
      END.
      FIND FIRST tt-report WHERE 
         tt-report.rec-id  = RECID(oe-rel) NO-ERROR.
          
      IF NOT VALID-HANDLE(lr-rel-lib)  THEN DO:
         RUN-PROC = "sbo/oerel-recalc-act.p".

          lr-rel-lib = phandle. 
         RUN VALUE(RUN-PROC) PERSISTENT SET phandle.
         lr-rel-lib = phandle. 

      END.

      IF AVAIL tt-report AND AVAIL oe-rel AND VALID-HANDLE(lr-rel-lib) THEN DO:
             RUN get-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT tt-report.qty).
             IF tt-report.qty NE oe-rel.qty THEN DO:
                 FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ ROWID(oe-rel)
                     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                 IF AVAIL b-oe-rel AND b-oe-rel.stat = "P" THEN
                     b-oe-rel.qty = tt-report.qty.
                 FIND CURRENT b-oe-rel NO-LOCK.

             END.
      END.

  END.

  RELEASE oe-rel.
  RELEASE b-oe-rell.
  RELEASE oe-rell.
  RELEASE oe-boll.
  RELEASE tt-report.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calendarPlacement B-table-Win 
PROCEDURE calendarPlacement :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
   
      IF btnCalendar:VISIBLE AND
         tt-report.stat:X IN BROWSE {&browse-name} + 75 + BROWSE {&browse-name}:X GT 0 AND
         tt-report.stat:Y IN BROWSE {&browse-name} + BROWSE {&browse-name}:Y GT 0 AND
         tt-report.stat:X IN BROWSE {&browse-name} + 75 + BROWSE {&browse-name}:X LE BROWSE {&browse-name}:WIDTH-PIXELS AND
         tt-report.stat:Y IN BROWSE {&browse-name} + BROWSE {&browse-name}:Y LE BROWSE {&browse-name}:HEIGHT-PIXELS THEN
         DO: /*do end needs to be here*/
            ASSIGN
               btnCalendar:X  = tt-report.stat:X IN BROWSE {&browse-name} + 75 + BROWSE {&browse-name}:X.
               btnCalendar:Y  = tt-report.stat:Y IN BROWSE {&browse-name} + BROWSE {&browse-name}:Y.
         END.
   END.

   END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-release B-table-Win 
PROCEDURE check-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {oe/check-release.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bol B-table-Win 
PROCEDURE create-bol :
/*------------------------------------------------------------------------------
  Purpose:     from oe/oe-ordlr.i
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF adm-brs-in-update THEN RETURN.

DEF VAR choice AS LOG NO-UNDO.

DEF VAR v-all-items AS LOG NO-UNDO.
DEF VAR v-first AS LOG NO-UNDO.
DEF VAR lv-save-recid AS RECID NO-UNDO.
DEF VAR v-invoice AS LOG NO-UNDO.
DEFINE BUFFER bf-notes FOR notes .

FIND xoe-ord WHERE xoe-ord.company = g_company AND
                   xoe-ord.ord-no = oe-rel.ord-no NO-LOCK.
FIND FIRST oe-ctrl WHERE oe-ctrl.company = xoe-ord.company NO-LOCK .

{sys/inc/addrelse.i}
{sys/inc/oereordr.i}

v-scr-s-code = oe-rel.s-code:SCREEN-VALUE IN BROWSE {&browse-name}.

FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ xoe-ord.company
          AND cust.cust-no EQ xoe-ord.cust-no NO-ERROR.

ASSIGN clvtext = "Notes: " .
IF oeBolPrompt-log AND AVAILABLE cust THEN DO:
    
        FOR EACH notes NO-LOCK WHERE notes.rec_key = cust.rec_key AND
                              LOOKUP(notes.note_code,oeBolPrompt-char) <> 0 :
            clvtext = clvtext + notes.note_text + "  " + CHR(13) .
        END.
    
    IF clvtext NE "Notes: " THEN
    MESSAGE clvtext VIEW-AS ALERT-BOX INFORMATION BUTTONS OK  .
END.

IF v-scr-s-code NE "I" THEN DO:

  RUN check-release NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  /* gdm - 02020902 */
  RUN credit-check NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.
END.



ASSIGN
lv-save-recid = RECID(oe-rel)
v-first = YES.

SESSION:SET-WAIT-STATE("general").

     
    FIND FIRST xoe-ordl OF oe-rel NO-LOCK NO-ERROR.

    {oe/rel-stat.i lv-stat x}.  

    IF INDEX("CPZ",lv-stat) GT 0 THEN DO:
      MESSAGE ENTRY(INDEX("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice")
              " release entries can not be released." VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.

    IF v-first THEN DO ON ENDKEY UNDO, RETRY:
         ASSIGN  v-first = NO
                 choice  = YES
                 v-invoice = AVAIL tt-report AND oe-rel.s-code EQ "I".
        MESSAGE "Create " +
                TRIM(STRING(AVAIL tt-report AND oe-rel.s-code EQ "I","Invoice/BOL")) +
                " for Release Date-" + trim(STRING(oe-rel.rel-date)) +
                " and ShipID-" +  trim(oe-rel.ship-id) +
                " and FOB-" + TRIM(tt-report.flute) + " ?"
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
    END.
    IF choice THEN DO:
        out-recid = RECID(oe-rel).
       /* run oe/relbol.p (recid(xoe-ordl)).  */
        FIND FIRST oe-rell WHERE oe-rell.company = oe-rel.company
              AND oe-rell.r-no     EQ oe-rel.link-no
              AND oe-rell.ord-no   EQ oe-rel.ord-no
              AND oe-rell.i-no     EQ oe-rel.i-no
              AND oe-rell.line     EQ oe-rel.line
              AND oe-rell.rel-no   EQ oe-rel.rel-no
              AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
              AND oe-rell.po-no    EQ oe-rel.po-no
            NO-LOCK NO-ERROR.
        /* wfk - to catch case where link-no is not assigned */
        FIND FIRST oe-rell WHERE oe-rell.company = oe-rel.company
              AND oe-rell.ord-no   EQ oe-rel.ord-no
              AND oe-rell.i-no     EQ oe-rel.i-no
              AND oe-rell.line     EQ oe-rel.line
              AND oe-rell.rel-no   EQ oe-rel.rel-no
              AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
              AND oe-rell.po-no    EQ oe-rel.po-no
            NO-LOCK NO-ERROR.

        IF NOT AVAIL oe-rell THEN DO:

          v-cust-no = oe-rel.cust-no.

          IF addxfer-log THEN
          DO:

               IF oe-rel.s-code EQ 'T' AND lv-cust-x NE "" AND
                   CAN-FIND(FIRST shipto WHERE
                   shipto.company EQ cocode AND
                   shipto.cust-no EQ lv-cust-x AND
                   shipto.ship-no EQ oe-rel.ship-no AND
                   shipto.ship-id EQ oe-rel.ship-id) THEN
                   v-cust-no = lv-cust-x.
          END.
      
          /* wfk {oe/findrelh.i oe-rel v-cust-no} */
          RUN oe/actrelmerg.p (INPUT ROWID(oe-rel), INPUT "FINDRELH", INPUT-OUTPUT iocPrompt, OUTPUT vrRelh).
          FIND oe-relh WHERE ROWID(oe-relh) EQ vrRElh NO-LOCK NO-ERROR.
          
          IF AVAIL oe-relh THEN
          FIND LAST oe-rell
              WHERE oe-rell.company EQ oe-relh.company
                AND oe-rell.r-no    EQ oe-relh.r-no
              USE-INDEX r-no NO-LOCK NO-ERROR.
        END.
        
        IF AVAIL oe-rell THEN out-recid = RECID(oe-rell).
        ELSE DO: 
            
            ASSIGN
               v-auto = YES
               out-recid = RECID(oe-rel).                
            RUN oe/relbol.p (RECID(xoe-ordl)).
            v-auto = NO.               
        END.

        RUN oe/do-bol.p(INPUT NOT v-invoice).
        FIND oe-rell WHERE RECID(oe-rell) EQ out-recid NO-LOCK NO-ERROR.
        IF AVAIL oe-rell AND oe-rell.link-no NE 0 THEN
        FIND oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.                    
    END.

    IF oeBolPrompt-log AND AVAILABLE xoe-ordl THEN DO:
      IF NOT AVAIL cust THEN
          FIND FIRST cust NO-LOCK 
          WHERE cust.company EQ xoe-ordl.company
          AND cust.cust-no EQ xoe-ordl.cust-no NO-ERROR.
      IF AVAIL cust THEN
          FOR EACH notes NO-LOCK WHERE notes.rec_key = cust.rec_key AND
                                LOOKUP(notes.note_code,oeBolPrompt-char) <> 0 :
             FIND FIRST oe-boll NO-LOCK
                 WHERE oe-boll.company  EQ xoe-ordl.company
                 AND oe-boll.ord-no   EQ xoe-ordl.ord-no
                 AND oe-boll.i-no     EQ xoe-ordl.i-no 
                 NO-ERROR.
             IF AVAILABLE oe-boll THEN 
                 FIND FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no NO-ERROR.
             IF AVAILABLE oe-bolh THEN DO:
                 CREATE bf-notes .
                 BUFFER-COPY notes EXCEPT rec_key TO bf-notes .
                 ASSIGN bf-notes.rec_key = oe-bolh.rec_key .
             END.
          END.
    END. /*oeBolPrompt-log  */


RUN release-shared-buffers.

RUN notify-source.

RUN local-open-query.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bol-all-item B-table-Win 
PROCEDURE create-bol-all-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR choice AS LOG NO-UNDO.
DEF BUFFER bf-rel FOR oe-rel.
DEF VAR v-all-items AS LOG NO-UNDO.
DEF VAR v-first AS LOG NO-UNDO.
DEF VAR lv-save-recid AS RECID NO-UNDO.
DEF VAR v-all-i AS LOG NO-UNDO.
DEF VAR v-rel-type AS CHAR NO-UNDO.

DEF VAR v-chkflg AS LOG INIT NO NO-UNDO.

FIND xoe-ord WHERE xoe-ord.company = g_company AND
                   xoe-ord.ord-no = oe-rel.ord-no NO-LOCK.
FIND FIRST oe-ctrl WHERE oe-ctrl.company = xoe-ord.company NO-LOCK .

{sys/inc/addrelse.i}
{sys/inc/oereordr.i}

choice = NO.



/* Check if the only release is an invoice only */
v-all-i = YES.
FOR EACH bf-rel
      WHERE bf-rel.company EQ xoe-ord.company
        AND bf-rel.ord-no  EQ xoe-ord.ord-no
        AND bf-rel.link-no EQ 0
      NO-LOCK:
    v-rel-type = bf-rel.s-code.
    IF v-rel-type NE "I" THEN
        v-all-i = NO.    
END.

/* gdm - 02020902 */
IF NOT v-all-i THEN DO:
    RUN check-release NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.

    RUN credit-check NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.
END.

lv-save-recid = RECID(oe-rel).

/* ======== in sys/inc/addrelse.i 
find sys-ctrl where sys-ctrl.company eq cocode
                and sys-ctrl.name    eq "ADDRELSE"  no-lock no-error.
if not avail sys-ctrl then do:
   create sys-ctrl.
   assign sys-ctrl.company = cocode
          sys-ctrl.name = "ADDRELSE"
          sys-ctrl.log-fld = yes.
   v-do-bol = sys-ctrl.log-fld.             
end.    
else v-do-bol = avail sys-ctrl and sys-ctrl.log-fld and oe-ctrl.ship-from.
========= */       

FOR EACH bf-rel
    WHERE bf-rel.company EQ xoe-ord.company
      AND bf-rel.ord-no  EQ xoe-ord.ord-no
      AND bf-rel.link-no EQ 0
    NO-LOCK:

  RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
       
  IF INDEX("AB",lv-stat) EQ 0 THEN DO:
    choice = YES.
    LEAVE.
  END.
END.

IF NOT choice THEN RELEASE bf-rel.

SESSION:SET-WAIT-STATE("general").
/*    
if avail bf-rel and v-do-bol then
do on endkey undo, return:
  choice = v-do-def.
  message "Create Bill of Lading?" update choice format "yes/no".
end.

else choice = no.
*/

FIND xoe-ordl OF xoe-ord NO-LOCK NO-ERROR.
IF AMBIG xoe-ordl THEN
   MESSAGE "All Items on Order?" VIEW-AS ALERT-BOX QUESTION
            BUTTON YES-NO UPDATE choice .
IF choice = ? THEN RETURN .

v-all-items = choice.

FOR EACH bf-rel
    WHERE bf-rel.company EQ xoe-ord.company
      AND bf-rel.ord-no  EQ xoe-ord.ord-no
      AND bf-rel.link-no EQ 0
    NO-LOCK,
    FIRST xoe-ordl OF bf-rel WHERE (RECID(xoe-ordl) EQ recid(oe-ordl) OR v-all-items) NO-LOCK
                      BREAK BY bf-rel.rel-date BY bf-rel.ship-id:

    IF FIRST-OF(bf-rel.ship-id) THEN v-first = YES.

    RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).

    IF v-first THEN DO ON ENDKEY UNDO, RETRY:
         ASSIGN  v-first = NO
                 choice  = YES.
        MESSAGE "Create BOL for Release Date-" + trim(STRING(bf-rel.rel-date)) +
                " and ShipID-" +  trim(bf-rel.ship-id) + " ?"
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
    END.
    IF choice THEN DO:
        out-recid = RECID(bf-rel).
       /* run oe/relbol.p (recid(xoe-ordl)).  */
        IF LAST-OF(bf-rel.ship-id) THEN DO:

            v-cust-no = bf-rel.cust-no.

            IF addxfer-log THEN
            DO:

                 IF bf-rel.s-code EQ 'T' AND lv-cust-x NE "" AND
                    CAN-FIND(FIRST shipto WHERE
                    shipto.company EQ cocode AND
                    shipto.cust-no EQ lv-cust-x AND
                    shipto.ship-no EQ bf-rel.ship-no AND
                    shipto.ship-id EQ bf-rel.ship-id) THEN
                    v-cust-no = lv-cust-x.
            END.
            /*  07011402   {oe/findrelh.i bf-rel v-cust-no} */
            RUN oe/actrelmerg.p (INPUT ROWID(bf-rel), INPUT "FINDRELH", INPUT-OUTPUT iocPrompt, OUTPUT vrRelh).
            FIND oe-relh WHERE ROWID(oe-relh) EQ vrRElh NO-LOCK NO-ERROR.

            IF AVAIL oe-relh THEN
            FIND LAST oe-rell
                WHERE oe-rell.company EQ oe-relh.company
                  AND oe-rell.r-no    EQ oe-relh.r-no
                USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAIL oe-relh THEN out-recid = RECID(oe-rell).
            ELSE DO:
                ASSIGN
                v-auto = YES
                out-recid = RECID(bf-rel).                
                RUN oe/relbol.p (RECID(xoe-ordl)).
                v-auto = NO.               
            END.                       
            RUN oe/do-bol.p(INPUT YES).
            FIND oe-rell WHERE RECID(oe-rell) EQ out-recid NO-LOCK NO-ERROR.
            IF AVAIL oe-rell AND oe-rell.link-no NE 0 THEN
            FIND oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.             
        END.   
    END.
END.

/* neet to complete  =======
if oereordr-log then do:  
   find first itemfg {sys/look/itemfgrlW.i}
                 and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
   if avail itemfg then run oe/d-fgqty.w  /*oe/fg-qtys.p*/  (recid(itemfg)).
end.  
=============*/

RUN release-shared-buffers.

RUN notify-source.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddRecord B-table-Win 
PROCEDURE pAddRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bf-oe-rel FOR oe-rel.
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
  DEFINE VARIABLE lv-rowid2 AS ROWID NO-UNDO.
  DEFINE VARIABLE iL AS INTEGER INITIAL 0 EXTENT 2 NO-UNDO.
  DEFINE VARIABLE lv-repos-recid AS RECID NO-UNDO.
  DEFINE VARIABLE v-nxt-r-no AS INTEGER NO-UNDO.
  DEFINE VARIABLE lMatchingSRecordFound AS LOGICAL NO-UNDO.
  DEFINE BUFFER bf-add-oe-rel FOR oe-rel.

  DEFINE BUFFER b-oe-ordl FOR oe-ordl.
  DEFINE BUFFER b-oe-rel FOR oe-rel.

  DEFINE VARIABLE cShipTo   AS CHAR NO-UNDO.
  DEFINE VARIABLE iRelQty   AS INT  NO-UNDO.
  DEFINE VARIABLE dtRelDate AS DATE NO-UNDO.
  DEFINE VARIABLE dtDelDate AS DATE NO-UNDO.
  DEFINE VARIABLE cPO       AS CHAR NO-UNDO.
  DEFINE VARIABLE lRelease  AS LOG  NO-UNDO.
  DEFINE VARIABLE lCancel AS LOG NO-UNDO.

  lv-rowid2 = IF AVAIL oe-rel THEN ROWID(oe-rel) ELSE ? .
  /* Code placed here will execute PRIOR to standard behavior. */
  IF CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) THEN DO:

      RUN prompt-set-release (OUTPUT lCancel, OUTPUT cShipTo, OUTPUT iRelQty, OUTPUT dtRelDate,
                              OUTPUT cPO, OUTPUT lRelease, OUTPUT dtDelDate).
      IF lCancel THEN
        RETURN "ADM-ERROR". 
      RUN add-set-releases (INPUT cShipTo, INPUT iRelQty, INPUT dtRelDate, INPUT lRelease, INPUT dtDelDate, INPUT cPO).
      MESSAGE "Releases Created"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN "ADM-ERROR". 
  END.

  
  IF AVAILABLE oe-ordl THEN
  DO:
     FOR EACH bf-oe-rel NO-LOCK 
         WHERE bf-oe-rel.company EQ oe-ordl.company
           AND bf-oe-rel.ord-no = oe-ordl.ord-no
           AND bf-oe-rel.i-no = oe-ordl.i-no
           AND bf-oe-rel.line = oe-ordl.line
           AND bf-oe-rel.line NE 0
           BY bf-oe-rel.line:
         iL[1] = iL[1] + 1.
     END.

     RUN oe/d-ordrel.w (?, ROWID(oe-ordl), "add",OUTPUT lv-rowid).
     
     FOR EACH bf-oe-rel NO-LOCK 
         WHERE bf-oe-rel.company EQ oe-ordl.company
           AND bf-oe-rel.ord-no = oe-ordl.ord-no
           AND bf-oe-rel.i-no = oe-ordl.i-no
           AND bf-oe-rel.line = oe-ordl.line
           AND bf-oe-rel.line NE 0
           BY bf-oe-rel.line:
         ASSIGN
             iL[2]    = iL[2] + 1.
     END.

     IF iL[2] GT 0 AND (iL[1] NE iL[2] OR iL[2] EQ 1) THEN DO:
         IF lv-rowid NE ? THEN do:
             RUN reopen-query . 
             RUN repo-query (lv-rowid).
         END.
     END.
     ELSE DO:
       RUN repo-query (lv-rowid2).
     END.

  END.

  IF RelType-int = 1 AND AVAIL tt-report AND oe-rel.s-code EQ "I" THEN DO:

     /* Check if a matching 'S' record already exists */
     lMatchingSRecordFound = NO.
     FOR EACH bf-add-oe-rel WHERE bf-add-oe-rel.company EQ oe-rel.company
         AND bf-add-oe-rel.ord-no EQ oe-rel.ord-no
         AND bf-add-oe-rel.LINE   EQ oe-rel.LINE
         AND bf-add-oe-rel.i-no   EQ oe-rel.i-no
         AND bf-add-oe-rel.s-code EQ "S"
         NO-LOCK:
        lMatchingSRecordFound = YES.
         
     END.

     IF  lMatchingSRecordFound EQ NO  THEN DO:
     
       ASSIGN  v-inv-ship        = YES
               v-qty-inv-only    = oe-rel.qty
               v-totqty-inv-only = oe-rel.tot-qty.
  
       /* Add a new oe-rel */
       RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
       
       CREATE bf-add-oe-rel.
       BUFFER-COPY oe-rel EXCEPT r-no rec_key TO bf-add-oe-rel.
       ASSIGN bf-add-oe-rel.s-code = "S"
              bf-add-oe-rel.r-no = v-nxt-r-no.

       /* Reset everything */
       RUN build-report-file.
  
       lv-repos-recid = RECID(bf-add-oe-rel).

       RUN oe/d-ordrel.w (ROWID(bf-add-oe-rel),ROWID(oe-ordl), "update", OUTPUT lv-rowid) .

       RUN reopen-query .
       RUN repo-query (lv-rowid).

     END. /* If 'S' record not found */
  END. /* If updating an invoice only "I" record */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRecord B-table-Win 
PROCEDURE pUpdateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    DEFINE VARIABLE lv-repos-recid AS RECID NO-UNDO.
    DEFINE VARIABLE v-nxt-r-no AS INTEGER NO-UNDO.
    DEFINE VARIABLE lMatchingSRecordFound AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-add-oe-rel FOR oe-rel.
    
    IF AVAILABLE oe-ordl AND AVAILABLE oe-rel THEN
    DO:
       RUN oe/d-ordrel.w (ROWID(oe-rel),ROWID(oe-ordl), "update", OUTPUT lv-rowid) . 
       
          IF lv-rowid NE ? THEN do: 
              RUN reopen-query .
              RUN repo-query (lv-rowid).
          END.
     END.

     IF RelType-int = 1 AND AVAIL tt-report AND oe-rel.s-code EQ "I" THEN DO:

     /* Check if a matching 'S' record already exists */
     lMatchingSRecordFound = NO.
     FOR EACH bf-add-oe-rel WHERE bf-add-oe-rel.company EQ oe-rel.company
         AND bf-add-oe-rel.ord-no EQ oe-rel.ord-no
         AND bf-add-oe-rel.LINE   EQ oe-rel.LINE
         AND bf-add-oe-rel.i-no   EQ oe-rel.i-no
         AND bf-add-oe-rel.s-code EQ "S"
         NO-LOCK:
        lMatchingSRecordFound = YES.
         
     END.

     IF  lMatchingSRecordFound EQ NO  THEN DO:
     
       ASSIGN  v-inv-ship        = YES
               v-qty-inv-only    = oe-rel.qty
               v-totqty-inv-only = oe-rel.tot-qty.
  
       /* Add a new oe-rel */
       RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
       
       CREATE bf-add-oe-rel.
       BUFFER-COPY oe-rel EXCEPT r-no rec_key TO bf-add-oe-rel.
       ASSIGN bf-add-oe-rel.s-code = "S"
              bf-add-oe-rel.r-no = v-nxt-r-no.
  

  
       /* Reset everything */
       RUN build-report-file.
  
       lv-repos-recid = RECID(bf-add-oe-rel).

       RUN oe/d-ordrel.w (ROWID(bf-add-oe-rel),ROWID(oe-ordl), "update", OUTPUT lv-rowid) .

       RUN reopen-query .
       RUN repo-query (lv-rowid).

     END. /* If 'S' record not found */
  END. /* If updating an invoice only "I" record */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewRecord B-table-Win 
PROCEDURE pViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    
    IF AVAILABLE oe-ordl AND AVAILABLE oe-rel THEN
    DO:
       RUN oe/d-ordrel.w (ROWID(oe-rel),ROWID(oe-ordl), "view", OUTPUT lv-rowid) . 
       RUN reopen-query .
       RUN repo-query (lv-rowid).
          
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-job B-table-Win 
PROCEDURE create-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-bld-job LIKE job.job-no NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR li2 AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEFINE VARIABLE v-prod-cat AS CHARACTER  NO-UNDO.

  DEF BUFFER b-job FOR job.
  DEF BUFFER oe-rel-job FOR reftable.

  RELEASE job.
  RELEASE itemfg.

  SESSION:SET-WAIT-STATE("general").

  IF AVAIL oe-rel THEN DO TRANSACTION:
     
     FIND FIRST job NO-LOCK
         WHERE job.company EQ oe-rel.company
           AND job.job     EQ oe-rel.job
         NO-ERROR.
    
     IF AVAIL job THEN DO:

        MESSAGE "Job already exists for Scheduled Release, rebuild standards?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.

        IF NOT ll THEN
        DO:
           SESSION:SET-WAIT-STATE("").
           RETURN.
        END.

        ELSE ll = NO.
     END.
    
     ELSE DO:
       RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
       IF INDEX("LSABI",lv-stat) GT 0 THEN
       FIND FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ oe-ordl.company
             AND itemfg.i-no    EQ oe-ordl.i-no
             AND itemfg.est-no  NE ""
             AND CAN-FIND(FIRST eb
                          WHERE eb.company  EQ itemfg.company
                            AND eb.est-no   EQ itemfg.est-no
                            AND eb.stock-no EQ itemfg.i-no)
           NO-ERROR.
     END.
    
     IF AVAIL itemfg THEN DO:
        FIND FIRST sys-ctrl
            WHERE sys-ctrl.company EQ itemfg.company
              AND sys-ctrl.name    EQ "JOBCREAT"
            NO-LOCK NO-ERROR.
        
        FIND FIRST eb WHERE eb.company  EQ itemfg.company
                        AND eb.est-no   EQ itemfg.est-no
                        AND eb.stock-no EQ itemfg.i-no NO-LOCK NO-ERROR. 
        IF AVAILABLE eb THEN 
           v-prod-cat =  eb.procat.
            
        RUN jc/job-no.p (INPUT-OUTPUT v-bld-job, 
                         INPUT-OUTPUT li, 
                         INPUT v-prod-cat,
                         INPUT itemfg.est-no).

        IF v-bld-job EQ "" THEN DO:
           ASSIGN
              ll = YES
              v-bld-job = " " + itemfg.est-no
              li        = 0.  
          
           IF AVAIL sys-ctrl THEN
              v-bld-job = SUBSTR(sys-ctrl.char-fld,1,1) + TRIM(v-bld-job).
          
           v-bld-job = FILL(" ",6 - LENGTH(TRIM(v-bld-job))) + TRIM(v-bld-job).

           FIND LAST b-job NO-LOCK
               WHERE b-job.company EQ itemfg.company
                 AND b-job.job-no EQ v-bld-job
               NO-ERROR.
          
           IF AVAIL b-job THEN DO:
              ll = NO.
             
              MESSAGE "Job(s) already exist for Estimate: " + TRIM(itemfg.est-no) +
                      ", create new one?"
                  VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                  UPDATE ll.
             
              IF ll THEN li = b-job.job-no2 + 1.
           END.
        END.
       
        ELSE DO:
           IF NOT AVAIL sys-ctrl THEN DO:
              MESSAGE "Must have System Control Parameter 'JOBCREATE'..."
                  VIEW-AS ALERT-BOX ERROR.
              ll = NO.
           END.
          
           ELSE ll = YES.
           
           IF ll THEN DO:
              FIND LAST b-job NO-LOCK
                  WHERE b-job.company EQ cocode
                    AND b-job.job-no  BEGINS SUBSTR(sys-ctrl.char-fld,2,1)
                  NO-ERROR.
                
              ASSIGN
               li        = (IF AVAIL b-job THEN INT(SUBSTR(b-job.job-no,2,5)) ELSE 0) + 1
               v-bld-job = SUBSTR(sys-ctrl.char-fld,2,1) + STRING(li,"99999")
               li        = 0.
           END.
        END.
     END.
    
     IF ll THEN DO:
        li2 = 1.
        FIND LAST job NO-LOCK
            WHERE job.company EQ itemfg.company USE-INDEX job
            NO-ERROR.
        FIND LAST job-hdr NO-LOCK
            WHERE job-hdr.company EQ itemfg.company USE-INDEX job
            NO-ERROR.
        IF job-hdr.job GT job.job THEN li2 = job-hdr.job + 1.
        IF job.job GE job-hdr.job THEN li2 = job.job + 1.
       
        CREATE job.
        ASSIGN
         job.job        = li2
         job.company    = itemfg.company
         job.loc        = locode
         job.est-no     = itemfg.est-no
         job.start-date = TODAY
         job.stat       = "P"
         job.job-no     = v-bld-job
         job.job-no2    = li.      
        
        
        ASSIGN oe-rel.job = job.job.
     END.
    
     IF AVAIL job THEN DO:
        nufile = YES.
        RUN jc/jc-calc.p (RECID(job), NO).
        fil_id = RECID(job).
        RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.
        nufile = NO.
        RELEASE job.
     END.
  END.

  SESSION:SET-WAIT-STATE("").
  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-oe-rell B-table-Win 
PROCEDURE create-oe-rell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-whse LIKE oe-rell.loc NO-UNDO.
DEF VAR v-loc-bin LIKE oe-rell.loc-bin NO-UNDO.
DEF VAR v-fgfile AS LOG NO-UNDO.
DEF VAR v-none AS LOG INIT YES NO-UNDO.
DEF VAR lv-all-or-one AS cha NO-UNDO.
DEF VAR lv-rowids AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ll-bin-tag AS LOG NO-UNDO.
DEF VAR lv-job-no AS CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-selected-value AS cha NO-UNDO. /*all,one,notag*/
DEF VAR v-s-code AS CHAR NO-UNDO.
DEF BUFFER b-reftable FOR reftable.
DEF BUFFER bf-oe-rel FOR oe-rel.
DEFINE BUFFER bf-notes FOR notes .
DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE BUFFER bf-oe-relh FOR oe-relh .

IF NOT AVAIL oe-rel THEN
    RETURN.

FIND FIRST oe-relh WHERE oe-relh.company   = oe-rel.company
                     AND oe-relh.cust-no   = oe-rel.cust-no
    /*
                     AND oe-relh.ship-no   = oe-rel.ship-no
                     AND oe-relh.ship-id   = oe-rel.ship-id */
                     AND oe-relh.rel-date  = oe-rel.rel-date
                   NO-LOCK NO-ERROR.
IF NOT AVAIL oe-relh THEN DO:
    RUN oe/cre-relh.p (INPUT RECID(oe-rel)).
    FIND FIRST bf-oe-ordl NO-LOCK
          WHERE bf-oe-ordl.company EQ oe-rel.company
            AND bf-oe-ordl.ord-no  EQ oe-rel.ord-no
            AND bf-oe-ordl.line    EQ oe-rel.line
          NO-ERROR.
      FIND bf-oe-relh WHERE RECID(bf-oe-relh) EQ relh-recid NO-LOCK.
      IF AVAIL bf-oe-ordl THEN 
        FOR EACH notes NO-LOCK WHERE notes.rec_key = bf-oe-ordl.rec_key 
                 AND notes.note_type <> "S" and notes.note_type <> "o"  :
             IF AVAILABLE bf-oe-relh THEN DO:
                 CREATE bf-notes .
                 BUFFER-COPY notes EXCEPT rec_key TO bf-notes .
                 ASSIGN bf-notes.rec_key = bf-oe-relh.rec_key .
             END.
        END.

    FIND FIRST oe-relh WHERE oe-relh.company   = oe-rel.company
                     AND oe-relh.cust-no   = oe-rel.cust-no
    /*
                     AND oe-relh.ship-no   = oe-rel.ship-no
                     AND oe-relh.ship-id   = oe-rel.ship-id */
                     AND oe-relh.rel-date  = oe-rel.rel-date
                   NO-LOCK NO-ERROR.

END.
  


/* -------------------------------------------------- oe/cre-rell.p 01/98 JLF */
/* order entry - Create actual releases from planned release line             */
/* -------------------------------------------------------------------------- */


/* {sys/inc/var.i shared} */

/* {oe/d-selbin.i NEW} */




{sys/inc/addrelse.i}

DO TRANSACTION:
  {sys/inc/relmerge.i}

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "BOLWHSE"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLWHSE"
     sys-ctrl.descrip = "Default Warehouse for Adding Release/BOL"
     sys-ctrl.log-fld = NO.
    MESSAGE "System control record NOT found. " sys-ctrl.descrip
    UPDATE sys-ctrl.char-fld.
  END.
  IF AVAIL sys-ctrl THEN v-whse = sys-ctrl.char-fld.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "BOLPRINT"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLPRINT"
     sys-ctrl.descrip = "Print Bill of Lading Headers on Plain Paper"
     sys-ctrl.log-fld = NO.
    MESSAGE "System control record NOT found. " sys-ctrl.descrip
            UPDATE sys-ctrl.char-fld.
  END.
  IF AVAIL sys-ctrl THEN v-loc-bin = sys-ctrl.char-fld.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "AUTOPOST"
      NO-LOCK NO-ERROR.
  v-fgfile = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "FGFILE".
END.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

DEF BUFFER bf-rell FOR oe-rell .
DEF VAR li-nxt-rel-no AS INT NO-UNDO.

FOR EACH bf-rell
    WHERE bf-rell.company EQ cocode
      AND bf-rell.ord-no  EQ oe-rel.ord-no NO-LOCK 
      BY bf-rell.rel-no DESC:
    
      li-nxt-rel-no =  bf-rell.rel-no.
      LEAVE.  
END.
li-nxt-rel-no = li-nxt-rel-no + 1.
/*========== */

DO TRANSACTION:
  FIND CURRENT oe-relh EXCLUSIVE.
  FIND CURRENT oe-rel EXCLUSIVE.
  ASSIGN
   oe-relh.printed = NO
   oe-rel.rel-no   = li-nxt-rel-no.
   oe-rel.b-ord-no = oe-relh.b-ord-no.
  FIND CURRENT oe-relh NO-LOCK.
  FIND CURRENT oe-rel NO-LOCK.
END.

FIND FIRST oe-ordl
    WHERE oe-ordl.company EQ cocode
      AND oe-ordl.ord-no  EQ oe-rel.ord-no
      AND oe-ordl.i-no    EQ oe-rel.i-no
      AND oe-ordl.line    EQ oe-rel.line
    NO-LOCK.

FIND FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ oe-rel.i-no
    NO-LOCK NO-ERROR.
    
ll-bin-tag = AVAIL oe-ordl             AND
             addrelse-cha EQ "Bin/Tag" AND
             CAN-FIND(FIRST fg-bin
                      WHERE fg-bin.company EQ cocode
                        AND fg-bin.i-no    EQ oe-ordl.i-no
                        AND fg-bin.qty     GT 0).
    
IF ll-bin-tag THEN DO:
  ASSIGN
   ll        = NO
   lv-job-no = TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99").

  IF lv-job-no EQ "-00" THEN lv-job-no = "".


 v-s-code  = IF oe-rel.s-code <> "" THEN oe-rel.s-code ELSE
                   IF oe-ordl.is-a-component THEN "S" ELSE
                   IF AVAIL oe-ctrl AND oe-ctrl.ship-from THEN "B" ELSE "I".
 lv-selected-value = "NoTag".
  
  ASSIGN
   lv-all-or-one = /*STRING(ll,"ALL/ONE")*/ lv-selected-value  /*all,one,notag*/
   ll-bin-tag    = lv-all-or-one NE "ONE" OR
                   CAN-FIND(FIRST fg-bin
                            WHERE fg-bin.company EQ cocode
                              AND fg-bin.i-no    EQ oe-ordl.i-no
                              AND fg-bin.job-no  EQ oe-ordl.job-no
                              AND fg-bin.job-no2 EQ oe-ordl.job-no2
                              AND fg-bin.qty     GT 0).
END.
  
IF v-none THEN DO TRANSACTION:

  
  CREATE oe-rell.
  ASSIGN
   out-recid       = RECID(oe-rell)
   oe-rell.company = oe-rel.company
   oe-rell.r-no    = oe-relh.r-no
   oe-rell.rel-no  = li-nxt-rel-no
   oe-rell.loc     = locode
   oe-rell.ord-no  = oe-rel.ord-no
   oe-rell.qty     = oe-rel.qty
   oe-rell.i-no    = oe-rel.i-no
   oe-rell.job-no  = oe-ordl.job-no
   oe-rell.job-no2 = oe-ordl.job-no2
   oe-rell.po-no   = oe-rel.po-no
   oe-rell.line    = oe-rel.line
   oe-rell.printed = NO
   oe-rell.posted  = NO
   oe-rell.deleted = NO
   oe-rell.loc     = oe-rel.spare-char-1
   oe-rell.lot-no  = oe-rel.lot-no
   oe-rell.frt-pay = oe-rel.frt-pay 
   oe-rell.fob-code = oe-rel.fob-code
   oe-rell.sell-price = oe-rel.sell-price
   oe-rell.zeroPrice = oe-rel.zeroPrice
   /** Set link to the planned releases **/
   oe-rell.link-no = oe-rel.r-no
   oe-rell.s-code  = IF oe-rel.s-code <> "" THEN oe-rel.s-code ELSE
                     IF oe-ordl.is-a-component THEN "S" ELSE
                     IF AVAIL oe-ctrl AND oe-ctrl.ship-from THEN "B" ELSE "I".
   FIND bf-oe-rel WHERE RECID(bf-oe-rel) = RECID(oe-rel) EXCLUSIVE-LOCK.
   bf-oe-rel.link-no = oe-rell.r-no.
   RELEASE bf-oe-rel.
  
     
       
  IF v-whse EQ "SHIPTO" THEN DO:
    FIND FIRST shipto
      WHERE shipto.company EQ cocode
        AND shipto.cust-no EQ oe-rel.cust-no
        AND shipto.ship-no EQ oe-rel.ship-no
      USE-INDEX ship-no NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN 
      ASSIGN
       oe-rell.loc     = shipto.loc
       oe-rell.loc-bin = shipto.loc-bin.
  END.
  
  ELSE DO:
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.job-no  EQ oe-ordl.job-no
          AND fg-bin.job-no2 EQ oe-ordl.job-no2
          AND fg-bin.i-no    EQ oe-rell.i-no
          AND fg-bin.qty     GE oe-rell.qty
        USE-INDEX job NO-LOCK NO-ERROR.
      
    IF NOT AVAIL fg-bin THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.job-no  EQ oe-ordl.job-no
          AND fg-bin.job-no2 EQ oe-ordl.job-no2
          AND fg-bin.i-no    EQ oe-rell.i-no
          AND fg-bin.qty     GT 0
        USE-INDEX job NO-LOCK NO-ERROR.
        
    IF NOT AVAIL fg-bin THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.job-no  EQ oe-ordl.job-no
          AND fg-bin.job-no2 EQ oe-ordl.job-no2
          AND fg-bin.i-no    EQ oe-rell.i-no
        USE-INDEX job NO-LOCK NO-ERROR.
        
    IF NOT AVAIL fg-bin AND oe-ordl.job-no EQ "" THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ oe-rell.i-no
          AND fg-bin.ord-no  EQ oe-rel.ord-no
          AND fg-bin.qty     GT 0
        USE-INDEX co-ino NO-LOCK NO-ERROR.
        
    IF NOT AVAIL fg-bin AND oe-ordl.job-no EQ "" THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ oe-rell.i-no
          AND fg-bin.ord-no  EQ oe-rel.ord-no
        USE-INDEX co-ino NO-LOCK NO-ERROR.
        
    IF NOT AVAIL fg-bin AND oe-ordl.job-no EQ "" THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ oe-rell.i-no
          AND fg-bin.qty     GT 0
        USE-INDEX co-ino NO-LOCK NO-ERROR.
        
    IF NOT AVAIL fg-bin AND oe-ordl.job-no EQ "" THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ oe-rell.i-no
        USE-INDEX co-ino NO-LOCK NO-ERROR.
        
    IF AVAIL fg-bin THEN DO:
      ASSIGN
       oe-rell.loc      = fg-bin.loc
       oe-rell.loc-bin  = fg-bin.loc-bin
       oe-rell.tag      = fg-bin.tag
       oe-rell.job-no   = fg-bin.job-no
       oe-rell.job-no2  = fg-bin.job-no2
       oe-rell.qty-case = fg-bin.case-count.
       
    END.
                           
    ELSE 
    IF v-fgfile THEN DO:
      FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ oe-rell.i-no
        NO-LOCK NO-ERROR.
      ASSIGN
       oe-rell.loc     = itemfg.def-loc
       oe-rell.loc-bin = itemfg.def-loc-bin.
    END.
  END.
  
  IF oe-rell.loc EQ "" OR oe-rell.loc-bin EQ "" THEN DO:
    FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-rell.i-no
      NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN
      ASSIGN
       oe-rell.loc     = itemfg.def-loc
       oe-rell.loc-bin = itemfg.def-loc-bin.
    IF oe-rell.loc EQ "" OR oe-rell.loc-bin EQ "" THEN DO:
      FIND FIRST cust WHERE cust.company EQ cocode
                        AND cust.active  EQ "X" 
                      NO-LOCK NO-ERROR.
      IF AVAIL cust THEN DO:
        FIND FIRST shipto WHERE shipto.company EQ cocode
                            AND shipto.cust-no EQ cust.cust-no
                          NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN
          ASSIGN   
           oe-rell.loc     = shipto.loc
           oe-rell.loc-bin = shipto.loc-bin.
      END.            
    END.
  END.

  IF oe-rell.loc-bin EQ "" THEN oe-rell.loc-bin = v-loc-bin.

  IF oe-rell.qty-case EQ 0 THEN
    oe-rell.qty-case = IF AVAIL itemfg AND itemfg.case-count GT 0
                       THEN itemfg.case-count
                       ELSE
                       IF oe-ordl.cas-cnt GT 0 THEN oe-ordl.cas-cnt
                       ELSE 1.

  ASSIGN
   oe-rell.cases    = TRUNC((oe-rell.qty - oe-rell.partial) /
                            oe-rell.qty-case,0)
   oe-rell.partial  = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case). 

  IF oe-rell.qty LT 0 OR lv-selected-value = "NoTag" THEN oe-rell.tag = "".
  
  RUN oe/rel-stat-upd.p (ROWID(oe-rell)).
END. /* transaction */

IF avail(oe-rell) AND oe-rell.s-code = "I" THEN DO TRANSACTION:
  ASSIGN 
    oe-rell.loc-bin = ""
    oe-rell.job-no  = ""
    oe-rell.job-no2 = 0
    .
END.

DO TRANSACTION:  
  FIND CURRENT oe-rel EXCLUSIVE.
  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
  FIND CURRENT oe-rel NO-LOCK.
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record B-table-Win 
PROCEDURE create-report-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-phantom AS LOG NO-UNDO.
      

  FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL oe-rel THEN
  FIND FIRST oe-ord
      WHERE oe-ord.company EQ oe-rel.company 
        AND oe-ord.ord-no  EQ oe-rel.ord-no
      NO-LOCK NO-ERROR.

  IF AVAIL oe-ord THEN DO:
    FIND FIRST tt-report
        WHERE tt-report.rec-id EQ RECID(oe-rel)
        NO-ERROR.

    IF NOT AVAIL tt-report THEN CREATE tt-report.

    {oe/rel-stat.i lv-stat}

    RELEASE inv-line.
    IF lv-stat EQ "Z" AND AVAIL oe-boll THEN
    FIND FIRST inv-line
        WHERE inv-line.company EQ oe-boll.company
          AND inv-line.b-no    EQ oe-boll.b-no
          AND inv-line.ord-no  EQ oe-boll.ord-no
          AND inv-line.i-no    EQ oe-boll.i-no
          AND inv-line.po-no   NE ""
        NO-LOCK NO-ERROR.

    RUN create-report-record-1 (ip-phantom,
                                IF AVAIL oe-relh THEN oe-relh.rel-date
                                                 ELSE oe-rel.rel-date).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record-1 B-table-Win 
PROCEDURE create-report-record-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-phantom AS LOG NO-UNDO.
  DEF INPUT PARAM ip-date AS DATE NO-UNDO.
  
  DEF VAR v-reltype AS cha NO-UNDO.
  DEF BUFFER bf-oe-rel FOR oe-rel.
  /* To make record unique */
  iTTReportLine = iTTReportLine + 1.
    ASSIGN
     /* tt-report.term-id = v-term */
     tt-report.company = oe-rel.company
     tt-report.ord-no = oe-rel.ord-no
     tt-report.line   = iTTReportLine
     tt-report.rec-id  = RECID(oe-rel)
     ld-date           = ip-date
     tt-report.del-zone  = STRING(YEAR(ld-date),"9999") +
                         STRING(MONTH(ld-date),"99")  +
                         STRING(DAY(ld-date),"99")
     tt-report.stat  = STRING(ld-date,"99999999")
     tt-report.tax = ip-phantom
     tt-report.po-no   = /*IF AVAIL inv-line THEN inv-line.po-no
                         ELSE
                         IF AVAIL oe-boll THEN oe-boll.po-no
                         ELSE
                         IF AVAIL oe-rell THEN oe-rell.po-no
                         ELSE*/ oe-rel.po-no
     tt-report.qty     = oe-rel.qty
     tt-report.opened = (AVAIL oe-relh AND oe-relh.printed) OR
                         INDEX("PCZ",lv-stat) GT 0
     tt-report.q-rel = (IF AVAIL oe-relh THEN oe-relh.release# ELSE 0).
     tt-report.job-start-date = IF AVAIL oe-bolh THEN /* string(*/ oe-bolh.bol-date /* ,"99999999") */ ELSE ?
     . 
     IF oe-rel.spare-char-4 GT "" THEN 
      ASSIGN
      tt-report.prom-date = DATE(ENTRY(1, oe-rel.spare-char-4))
      tt-report.prom-code = ENTRY(2, oe-rel.spare-char-4)
      tt-report.pr-uom    = ENTRY(3, oe-rel.spare-char-4)
      .
     /* task 04011103*/
     FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                           AND sys-ctrl.name EQ "RelType" NO-LOCK NO-ERROR.
     IF AVAIL sys-ctrl THEN
        FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
                  AND sys-ctrl-ship.ship-id = oe-rel.ship-id NO-LOCK NO-ERROR.
        IF NOT AVAIL sys-ctrl-shipto THEN
        FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
                  AND sys-ctrl-ship.ship-id = "" NO-LOCK NO-ERROR.
     IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN v-reltype = sys-ctrl-shipto.char-fld.
     ELSE IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-reltype = sys-ctrl.char-fld.
     ELSE v-reltype = "B" .
     IF v-relType <> "" THEN DO:
          IF oe-rel.s-code EQ '' THEN DO:
           FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel)
              EXCLUSIVE-LOCK.
           bf-oe-rel.s-code = IF ll-transfer THEN "T"
                                    ELSE IF oe-ordl.is-a-component THEN "S"
                                    ELSE SUBSTRING(v-relType,1,1).
           FIND CURRENT bf-oe-rel NO-LOCK.
           RELEASE bf-oe-rel.
         END.
     END.


     tt-report.s-basis[1] = IF v-reltype <> "" THEN oe-rel.s-code
                       ELSE IF ll-transfer            THEN "T"
                       ELSE
                       IF oe-ordl.is-a-component AND
                       (oe-rel.s-code = "" OR
                           oe-rel.s-code NE "T")   THEN "S"
                       ELSE
                       IF oe-rel.s-code <> ""      THEN oe-rel.s-code
                       ELSE
                       IF AVAIL oe-rell          THEN oe-rell.s-code
                                                 ELSE "B".
    IF adm-new-record AND ll-transfer /* (oe-ord.TYPE = "T") */ THEN
        ASSIGN tt-report.s-basis[1] = "T".
    IF adm-new-record AND RelType-int = 1 AND v-inv-ship = YES THEN DO: /* task 07241401 */
         FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel)
            EXCLUSIVE-LOCK.
         ASSIGN tt-report.s-basis[1] = "S"
                bf-oe-rel.qty = v-qty-inv-only 
                bf-oe-rel.tot-qty = v-totqty-inv-only 
                v-inv-ship = NO .
         RELEASE bf-oe-rel.
    END.
    ASSIGN
          tt-report.lot-no  = oe-rel.lot-no
          tt-report.frt-pay = oe-rel.frt-pay
          tt-report.flute   = oe-rel.fob-code.

     ASSIGN tt-report.price = oe-rel.sell-price
            tt-report.whsed = oe-rel.zeroPrice > 0 .
      

    IF oeinq THEN 
      tt-report.del-zone = STRING(9999999999 - INT(tt-report.del-zone),"9999999999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE credit-check B-table-Win 
PROCEDURE credit-check :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-chkflg AS LOG INIT NO NO-UNDO.

/* gdm - 02020902 */
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN DO:

    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ oe-ordl.company
          AND cust.cust-no EQ oe-ordl.cust-no NO-ERROR.
    IF AVAIL cust THEN RUN oe/CRcheck.p (INPUT ROWID(cust),
                                         INPUT YES,
                                         OUTPUT v-chkflg).
    IF v-chkflg THEN DO:

        MESSAGE 
            "Can't create BOL, there are unpaid invoices."
            "Please create actual release first."
          VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.    
    END.
  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE custom-panel-state B-table-Win 
PROCEDURE custom-panel-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-panel-state AS CHAR NO-UNDO.


  RUN enable-ticket.

  IF NOT AVAIL oe-ordl OR oe-ordl.stat EQ "C" OR oe-ordl.opened EQ NO THEN
    io-panel-state = "disable-all".
  ELSE
  IF AVAIL oe-rel THEN
    IF io-panel-state EQ "add-only" OR io-panel-state EQ "disable-all" THEN
      io-panel-state = "initial".
    ELSE DO:
      IF AVAIL tt-report AND oe-rel.s-code EQ "I" THEN
        io-panel-state = "NoBOL".

      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company      EQ oe-ordl.company
            AND itemfg.i-no         EQ oe-ordl.i-no
            AND TRIM(itemfg.est-no) NE ""
          NO-ERROR.

      IF TRIM(oe-ordl.job-no) NE ""               OR
         TRIM(oe-ordl.est-no) NE ""               OR
         NOT AVAIL itemfg                         OR
         NOT CAN-FIND(FIRST eb
                      WHERE eb.company  EQ itemfg.company
                        AND eb.est-no   EQ itemfg.est-no
                        AND eb.stock-no EQ itemfg.i-no
                        AND eb.est-type NE 3
                        AND eb.est-type NE 4
                        AND eb.est-type NE 7
                        AND eb.est-type NE 8)     OR
         oe-ordl.is-a-component                   THEN
        io-panel-state = io-panel-state + "," + "NoJob".

      IF SUBSTR(io-panel-state,1,1) EQ "," THEN
        io-panel-state = SUBSTR(io-panel-state,2,1000).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-phantoms B-table-Win 
PROCEDURE delete-phantoms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-rel FOR oe-rel.
  DEF BUFFER b-tt-report FOR tt-report.


  FOR EACH b-tt-report WHERE b-tt-report.tax:
    FIND FIRST b-oe-rel WHERE RECID(b-oe-rel) EQ b-tt-report.rec-id NO-ERROR.
    IF AVAIL b-oe-rel THEN DELETE b-oe-rel.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-ticket B-table-Win 
PROCEDURE enable-ticket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE relStatus AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.


  out-recid = ?.

  IF AVAIL oe-rel THEN DO:
    FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-rel.company 
          AND oe-ord.ord-no  EQ oe-rel.ord-no
        NO-ERROR.
    {oe/rel-stat.i relStatus}
    {methods/run_link.i "container-source" "relTicketEnabled" "(CAN-DO('A,B',relStatus))"}
    IF NOT CAN-DO('A,B',relStatus) THEN RETURN.
    /*FIND FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-rel.link-no NO-ERROR.*/
    IF AVAIL oe-relh THEN DO:
      IF AVAIL oe-rell THEN out-recid = RECID(oe-rell).
      RUN custom/setUserPrint.p (g_company,'oe-relh_.',
                                 'begin_cust-no,end_cust-no,begin_relnum,end_relnum,begin_ord-no,end_ord-no,tb_printed,tb_posted,begin_date,end_date,begin_whse,end_whse',
                                 oe-relh.cust-no + ',' + oe-relh.cust-no + ',' +
                                 STRING(oe-relh.release#) + ',' + STRING(oe-relh.release#) + ',' +
                                 STRING(oe-rel.ord-no) + ',' + STRING(oe-rel.ord-no) + ',' +
                                 STRING(oe-relh.printed) + ',' + STRING(oe-relh.posted) + ',' +
                                 STRING(oe-relh.rel-date) + ',' + STRING(oe-relh.rel-date) + ',' +
                                 ',zzzzz').
    END.
  END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-max B-table-Win 
PROCEDURE get-max :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* This will display "" records -  */
    RUN dispatch ('view').                           
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-oe-rel FOR oe-rel.

  DEF VAR cShipTo   AS CHAR NO-UNDO.
  DEF VAR iRelQty   AS INT  NO-UNDO.
  DEF VAR dtRelDate AS DATE NO-UNDO.
  DEF VAR dtDelDate AS DATE NO-UNDO.
  DEF VAR cPO       AS CHAR NO-UNDO.
  DEF VAR lRelease  AS LOG  NO-UNDO.
  DEF VAR lCancel AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) THEN DO:

      RUN prompt-set-release (OUTPUT lCancel, OUTPUT cShipTo, OUTPUT iRelQty, OUTPUT dtRelDate,
                              OUTPUT cPO, OUTPUT lRelease, OUTPUT dtDelDate).
      IF lCancel THEN
        RETURN "ADM-ERROR". 
      RUN add-set-releases (INPUT cShipTo, INPUT iRelQty, INPUT dtRelDate, INPUT lRelease, INPUT dtDelDate, INPUT cPO).
      MESSAGE "Releases Created"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "ADM-ERROR". 
  END.
  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       oe/oe-rel.del, oe/oerelunp.p 
------------------------------------------------------------------------------*/
  DEF BUFFER bf-rel FOR oe-rel.
  DEF BUFFER b-tt-report FOR tt-report.
  DEF BUFFER bf-oe-rell FOR oe-rell.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.

  DEF VAR v-oe-rell-rec AS ROWID NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT AVAIL oe-rel THEN DO:
      MESSAGE "Manually added actual releases may not be deleted..."
              VIEW-AS ALERT-BOX ERROR .
      RETURN.
  END.

  FIND bf-rel WHERE RECID(bf-rel) = RECID(oe-rel). 

  FIND FIRST oe-ord
      WHERE oe-ord.company EQ oe-rel.company 
        AND oe-ord.ord-no  EQ oe-rel.ord-no
      NO-LOCK.

  {oe/rel-stat.i lv-stat}
  /*IF USERID("NOSWEAT") NE "ASI" THEN*/ DO:
      IF INDEX("CPZ",lv-stat) > 0 THEN DO:
          MESSAGE "Posted releases may not be deleted, must delete BOL first!"
                  VIEW-AS ALERT-BOX ERROR .
          RETURN.
      END.
      ELSE IF INDEX("AB",lv-stat) > 0 THEN DO:
          MESSAGE "Actual or Backordered release should not be deleted. " SKIP
                  "Delete anyway?"
                   VIEW-AS ALERT-BOX WARNING  BUTTON YES-NO UPDATE ll-ans1 AS LOG.
          IF ll-ans1 AND AVAIL oe-relh THEN DO:
             bf-rel.link-no = 0 .
             FOR EACH oe-rell WHERE oe-rell.company = bf-rel.company
                                AND oe-rell.r-no = oe-relh.r-no
                                AND oe-rell.i-no = bf-rel.i-no
                              USE-INDEX r-no:
                 RUN oe/relldel1.p  (RECID(oe-rell)).
                 DELETE oe-rell.
             END.
             FIND FIRST oe-rell WHERE oe-rell.company = bf-rel.company
                                  AND oe-rell.r-no = oe-relh.r-no
                                USE-INDEX r-no NO-LOCK NO-ERROR.
             IF NOT AVAIL oe-rell THEN DO:
                FIND CURRENT oe-relh NO-ERROR.
                IF AVAIL oe-relh THEN DO: 
                   DISABLE TRIGGERS FOR LOAD OF oe-relh.
                   DELETE oe-relh.
                END.
             END.
          END.
          ELSE RETURN.
      END.
      ELSE
      IF NOT adm-new-record THEN DO:
        {custom/askdel.i}
      END.
  END.

  /*
  find oe-ord where oe-ord.company = oe-rel.company 
                and oe-ord.ord-no = oe-rel.ord-no no-lock .
  if  oe-ord.posted then return.
     
  find first oe-relh where oe-relh.company = oe-rel.company 
                       and oe-relh.ord-no = oe-rel.ord-no
                       and oe-relh.ship-id = oe-rel.ship-id 
                       and oe-relh.posted = no use-index relh
                       no-lock no-error.

  if avail oe-relh then do:  
     find first oe-rell
         WHERE oe-rell.company EQ oe-relh.company
           AND oe-rell.r-no    EQ oe-relh.r-no
           and oe-rell.i-no = oe-rel.i-no
           and oe-rell.link-no = oe-rel.r-no
         USE-INDEX r-no no-lock no-error.
     if avail oe-rell then do:
        message "Acutal release entry exists for this planned release. Can not delete. " 
                view-as alert-box error.
        return.
     end.                      
  end.                      
  else if not ll-canceled then do:
       if oe-rel.link-no <> 0  then run unpost-item.       
       else do:
           MESSAGE "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.
           IF NOT response THEN  RETURN "ADM-ERROR":U.
              find first oe-ordl where oe-ordl.company eq oe-rel.company
                          and oe-ordl.ord-no  eq oe-rel.ord-no
                          and oe-ordl.i-no    eq oe-rel.i-no
                          and oe-ordl.line    eq oe-rel.line     no-error.
              if avail oe-ordl then oe-ordl.t-rel-qty = oe-ordl.t-rel-qty - oe-rel.qty. 
  END.

  end.              
  ll-canceled = no.
  */

  FIND FIRST notes WHERE notes.rec_key EQ oe-rel.rec_key NO-ERROR.
  IF AVAIL notes THEN DELETE notes.
  v-oe-rell-rec = ?.
  FIND FIRST bf-oe-rell WHERE bf-oe-rell.link-no EQ oe-rel.r-no
                          AND bf-oe-rell.company EQ oe-rel.company
                          AND bf-oe-rell.ord-no  EQ oe-rel.ord-no
                          AND bf-oe-rell.i-no    EQ oe-rel.i-no
                        NO-LOCK NO-ERROR.

  IF AVAIL(bf-oe-rell) THEN
    v-oe-rell-rec = ROWID(bf-oe-rell).
/*   FIND FIRST bf-oe-ordl                         */
/*      WHERE bf-oe-ordl.company EQ oe-rel.company */
/*        AND bf-oe-ordl.ord-no  EQ oe-rel.ord-no  */
/*        AND bf-oe-ordl.LINE    EQ oe-rel.LINE    */
/*      NO-LOCK NO-ERROR.                          */
  IF oe-rel.spare-dec-1 GT 0 THEN DO:
    FIND FIRST itemfg-loc 
      WHERE itemfg-loc.company EQ oe-rel.company
        AND itemfg-loc.i-no    EQ oe-rel.i-no
        AND itemfg-loc.loc     EQ oe-rel.spare-char-1
      EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL itemfg-loc THEN
        ASSIGN itemfg-loc.q-alloc = itemfg-loc.q-alloc - oe-rel.spare-dec-1
               itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

    lJustDeletedLine = TRUE.
  /* Code placed here will execute AFTER standard behavior.    */
  /*FOR EACH b-tt-report WHERE NOT CAN-FIND(oe-rel WHERE RECID(oe-rel) EQ b-tt-report.rec-id):
    DELETE b-tt-report.
  END.*/
  IF v-oe-rell-rec NE ? AND USERID("NOSWEAT") EQ "ASI" THEN DO:
      /* Only ASI can delete the actual release, so make sure to
         delete the oe-rell so the oe-rel is automatically recreated */
      FIND bf-oe-rell WHERE ROWID(bf-oe-rell) EQ v-oe-rell-rec
                      EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bf-oe-rell THEN
          DELETE bf-oe-rell.
  END.
  lv-rel-recid = ?.

  RUN release-shared-buffers.
  /* Redistribute quantity to itemfg-loc */
  IF AVAIL oe-ordl THEN
    RUN fg/fgitmloc.p (INPUT oe-ordl.i-no, INPUT ROWID(oe-ordl)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy B-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(lr-rel-lib) THEN
    DELETE OBJECT lr-rel-lib.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
      
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-stat AS CHAR NO-UNDO.
DEF BUFFER bfOeRel FOR oe-rel.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT AVAIL {&FIRST-EXTERNAL-TABLE}                 OR
     NOT AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}}  OR
     NOT AVAIL {&SECOND-TABLE-IN-QUERY-{&browse-name}} THEN
    RETURN "ADM-ERROR".

  /*DO WITH FRAME {&FRAME-NAME}:
    IF NOT oereleas-log THEN
      oe-rel.qty:LABEL IN BROWSE {&browse-name} = "Shipped Qty".
  END.*/
  v-stat = get-rel-stat().
  IF v-stat NE oe-rel.stat AND v-stat GT "" THEN
  DO:
    /*FIND oe-rel WHERE
         ROWID(oe-rel) EQ ROWID(b-oe-rel) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      */
    IF AVAIL oe-rel THEN
    DO:
       FIND bfOeRel WHERE ROWID(bfOeRel) EQ  ROWID(oe-rel) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       IF AVAIL bfOeRel THEN
       bfOeRel.stat = v-stat.
       FIND CURRENT bfOeRel NO-LOCK NO-ERROR.
    END.

  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */

  IF AVAIL oe-rel AND NOT adm-new-record THEN DO:
    FIND oe-ord WHERE oe-ord.company = g_company AND
                    oe-ord.ord-no = oe-ordl.ord-no NO-LOCK.
        
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

    IF INDEX("CPZ", lv-stat) > 0 AND USERID("NOSWEAT") NE "ASI" THEN DO:
       MESSAGE ENTRY(INDEX("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice")
               "release entries can not be modified"
           VIEW-AS ALERT-BOX ERROR.
       adm-brs-in-update = NO.
       RETURN ERROR.
    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"ordrel-source", OUTPUT char-hdl).
  RUN reset-button IN WIDGET-HANDLE(char-hdl) (NO).*/
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"ordrel-source", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO: 
      IF adm-new-record OR adm-cur-state = "Update-begin" THEN
      RUN reset-button IN WIDGET-HANDLE(char-hdl) (NO).
      ELSE
      RUN reset-button IN WIDGET-HANDLE(char-hdl) (YES).
  END. /* if valid-handle */

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"ordbol-source", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
  RUN reset-button IN WIDGET-HANDLE(char-hdl) (NO).

  /* if a new record, no date change reason needed, if blank, diaglog will appear */
  IF NOT l-update-reason-perms OR adm-new-record 
      OR oe-rel.spare-char-2:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
    oe-rel.spare-char-2:READ-ONLY IN BROWSE {&browse-name} = TRUE .

  RUN release-shared-buffers.

  IF AVAIL tt-report THEN
  DO WITH FRAME {&FRAME-NAME}:
      IF INDEX("AB",lv-stat) GT 0 OR ll-transfer THEN
        APPLY "entry" TO tt-report.opened IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO oe-rel.s-code IN BROWSE {&browse-name}.
    END.
      
  v-browse-in-update = YES.
  
  IF (adm-cur-state BEGINS "ADD" OR adm-cur-state BEGINS "link-changed") THEN DO:      
      tt-report.stat:READ-ONLY IN BROWSE {&browse-name} = FALSE.
  END.
  ELSE
    tt-report.stat:READ-ONLY IN BROWSE {&browse-name} = 
    NOT l-update-reason-perms OR (oeDateAuto-log AND OeDateAuto-Char = "Colonial").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit B-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*{methods/winReSizeLocInit.i}*/
  IF NOT l-update-reason-perms OR (oeDateAuto-log AND OeDateAuto-Char = "Colonial") THEN DO:

      DISABLE tt-report.stat.
      END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  relh-recid = ?.
  
  RUN build-report-file.

  FIND FIRST tt-report NO-LOCK NO-ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  /*RUN delete-phantoms.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  /*DEF VAR phandle AS HANDLE NO-UNDO.*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

   {methods/winReSizeLocInit.i} /* task 01211416 */
  /* Code placed here will execute AFTER standard behavior.    */
  /*{methods/template/local/setvalue.i} */
  DEFINE VARIABLE relStatus AS CHARACTER NO-UNDO.

  IF AVAILABLE oe-rel THEN DO:
    RUN oe/rel-stat.p (ROWID(oe-rel),OUTPUT relStatus).
    {methods/run_link.i "container-source" "relTicketEnabled" "(CAN-DO('A,B',relStatus))"}
  END.

  &IF "{&SETVALUE}" NE "no" &THEN
     &IF "{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}" NE "" &THEN
         &Scoped-define TABLENAME {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
     &ELSE
         &Scoped-define TABLENAME {&FIRST-EXTERNAL-TABLE}
     &ENDIF

     &IF INDEX("{&NORECKEY}","{&TABLENAME}") = 0 &THEN
         IF AVAILABLE {&TABLENAME} THEN
         DO:
           FIND CURRENT {&TABLENAME} NO-LOCK.
           {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
                    "({&TABLENAME}.rec_key, string(oe-rel.ord-no))" }
           {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
                    "(CAN-FIND(FIRST notes WHERE notes.rec_key = {&TABLENAME}.rec_key))"}
           {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
                    "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = {&TABLENAME}.rec_key))"}
         END.
     &ENDIF
  &ENDIF
  APPLY "ENTRY" TO FRAME {&FRAME-NAME}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-due-date B-table-Win 
PROCEDURE new-due-date :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF BUFFER bf-shipto FOR shipto.
DEFINE VARIABLE dTempDate AS DATE NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        
        dTempDate = DATE(tt-report.prom-date:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
        /* Date being entered not yet complete */
        IF ERROR-STATUS:ERROR THEN 
            RETURN. 
    
        FIND FIRST oe-ord
            WHERE oe-ord.company EQ oe-ordl.company 
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-LOCK.

        RUN oe/custxship.p (oe-ordl.company,
            oe-ord.cust-no,
            oe-rel.ship-id:SCREEN-VALUE IN BROWSE {&browse-name},
            BUFFER bf-shipto).
        /* if oeDateAuto is 'colonial' then release date is due date - dock appt days */
        IF AVAIL bf-shipto THEN                           
            ASSIGN
                tt-report.stat:SCREEN-VALUE IN BROWSE {&browse-name} 
                
                = STRING(
                get-date(DATE(tt-report.prom-date:SCREEN-VALUE IN BROWSE {&browse-name}), bf-shipto.spare-int-2, "-")                
                ,"99/99/9999")
             .
      
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE notify-source B-table-Win 
PROCEDURE notify-source :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  IF AVAIL oe-ordl THEN DO:
    lv-rowid = IF AVAIL oe-rel THEN ROWID(oe-rel) ELSE ?.
    
    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN DO:
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"bolrel-source",OUTPUT char-hdl).

      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN reposit-item IN WIDGET-HANDLE(char-hdl) (RECID(oe-ord), RECID(oe-ordl)).
    END.

    RUN reopen-query.

    IF lv-rowid NE ? THEN RUN repo-query (lv-rowid).
  END.

  RUN enable-ticket.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prompt-set-release B-table-Win 
PROCEDURE prompt-set-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER oplCancel   AS LOG NO-UNDO.
DEF OUTPUT PARAMETER opcShipTo   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opiRelQty   AS INT NO-UNDO.
DEF OUTPUT PARAMETER opdtRelDate AS DATE NO-UNDO.
DEF OUTPUT PARAMETER opcPO       AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oplRelease  AS LOG NO-UNDO.
DEF OUTPUT PARAMETER opdtDelDate AS DATE NO-UNDO.

DEFINE VARIABLE lcUserPrompt AS CHARACTER INIT "".

DEFINE VARIABLE lcShipTo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-parms     AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-values    AS CHARACTER NO-UNDO.
DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
DEFINE VARIABLE choice       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValid       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE liRelQty     AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldRelDate    AS DATE      NO-UNDO.
DEFINE VARIABLE ldDelDate    AS DATE      NO-UNDO.
DEFINE VARIABLE llReplace    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcPO         AS CHARACTER NO-UNDO.
DEFINE VARIABLE opiNewOrder  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lvErrMsg     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lvcDefaultShipto AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvcDefaultPo AS CHARACTER NO-UNDO.

DEF BUFFER bf-oe-rel FOR oe-rel.

/* Only looking at order # since release would not exist for the set header item */
FIND FIRST bf-oe-rel WHERE bf-oe-rel.company EQ oe-ordl.company
        AND bf-oe-rel.ord-no EQ oe-ordl.ord-no
    NO-LOCK NO-ERROR.
IF AVAIL bf-oe-rel THEN
    ASSIGN lvcDefaultShipto = bf-oe-rel.ship-id
           lvcDefaultPO     = bf-oe-rel.po-no.
ELSE
    IF AVAIL oe-ordl THEN
        ASSIGN lvcDefaultShipto = oe-ordl.ship-id
               lvcDefaultPO     = oe-ordl.po-no.

ip-parms = 
   /* Box Title */
   "type=literal,name=label11,row=2,col=18,enable=false,width=58,scrval=" + lcUserPrompt + ",FORMAT=X(58)" 
      
    /* Set Attributes */
    + "|type=attrib,name=cust-no,row=1,col=1,enable=false,width=2,inpval=" + oe-ordl.cust-no
    + "|type=attrib,name=company,row=1,col=1,enable=false,width=2,inpval=" + cocode
    + "|type=attrib,name=loc,row=1,col=1,enable=false,width=2,inpval=" + locode
    

    /* Ship To Code */
    + "|type=literal,name=label6,row=2.2,col=31,enable=false,width=58,scrval=" + "ShipTo Code:" + ",FORMAT=X(58)"
    + "|type=fill-in,name=custShipTo,row=2,col=45,enable=true,width=15,initial=" + lvcDefaultShipto

    /* Release Qty */
    + "|type=literal,name=label7,row=3.2,col=28,enable=false,width=58,scrval=" + "Set Release Qty:" + ",FORMAT=X(58)"
    + "|type=fill-in,name=fi_relqty,row=3,col=45,enable=true,width=15,data-type=integer"    

    /* Release Date */
    + "|type=literal,name=label8,row=4.2,col=27,enable=false,width=58,scrval=" + "Set Release Date:" + ",FORMAT=X(58)"
    + "|type=fill-in,name=fi_reldate,row=4,col=45,enable=true,width=15,data-type=date,initial=" + STRING(TODAY)

    /* PO # */
    + "|type=literal,name=label12,row=5.2,col=31,enable=false,width=30,scrval=" + "Customer PO:" + ",FORMAT=X(30)"
    + "|type=fill-in,name=po-no,row=5,col=45,enable=true,width=22,format=x(15),initial=" + lvcDefaultPO

    /* Replace existing releases toggle box */
    + "|type=literal,name=label9,row=6.5,col=23,enable=false,width=38,scrval=" + "Replace Existing Releases for date: " + ",FORMAT=X(58)"
    + "|type=toggle,name=tb_replace,row=6.5,col=20,enable=true,width=2,data-type=logical"

    /* Delete Release Date */
   /* + "|type=literal,name=label10,row=6,col=52,enable=false,width=9,scrval=" + "Del Date:" + ",FORMAT=X(9)" */
    + "|type=fill-in,name=fi_deldate,row=6.3,col=62,enable=true,width=15,data-type=date,deffield=fi_reldate,depfield=tb_replace,initial=" + STRING(TODAY)
    

    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true,width=12,height=3 " 
    /* Box Title */
    + "|type=win,name=fi3,enable=true,label=         Set Quantity to Create Component Releases?,FORMAT=X(30),height=11".

prompt-loop:
DO WHILE TRUE:

    RUN custom/d-prompt.w (INPUT "", ip-parms, "", OUTPUT op-values).
    
    /* Process values using names given above */
    DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
        IF ENTRY(i, op-values) EQ "default" THEN
          choice = ENTRY(i + 1, op-values) NO-ERROR.

        /* Ship To */
        IF ENTRY(i, op-values) EQ "custShipTo" THEN
          lcShipTo = ENTRY(i + 1, op-values) NO-ERROR.            

        /* Release Qty */
        IF ENTRY(i, op-values) EQ "fi_relqty" THEN
          liRelQty = INTEGER(ENTRY(i + 1, op-values)) NO-ERROR.            

        /* Release Date */
        IF ENTRY(i, op-values) EQ "fi_reldate" THEN
          ldRelDate = DATE(ENTRY(i + 1, op-values)) NO-ERROR. 

        /* Replace Existing */
        IF ENTRY(i, op-values) EQ "tb_replace" THEN
          llReplace = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR. 

        /* Delete Date */
        IF ENTRY(i, op-values) EQ "fi_deldate" THEN
          ldDElDate = DATE(ENTRY(i + 1, op-values)) NO-ERROR. 

        /* PO # */
        IF ENTRY(i, op-values) EQ "po-no" THEN
          lcPO = ENTRY(i + 1, op-values) NO-ERROR. 
    END.

    lvErrMsg = "".
    IF choice NE "CANCEL" THEN DO:
    
        FIND FIRST shipto WHERE shipto.company = cocode
              AND shipto.cust-no EQ oe-ordl.cust-no
              AND shipto.ship-id EQ lcShipTo
            NO-LOCK NO-ERROR.
        IF NOT AVAIL shipto THEN
            lvErrMsg = "Please enter a valid ship to id".
        IF liRelQty LE 0 THEN
            lvErrMsg = "Please enter a release quantity that is more than zero.".
        IF ldRelDate EQ ? THEN
            lvErrMsg = "Please enter a release date.".

    END.

    IF lvErrMsg GT "" THEN 
      MESSAGE lvErrMsg VIEW-AS ALERT-BOX.
    ELSE
        LEAVE.
 END.

ASSIGN
 oplCancel = IF choice EQ "CANCEL" THEN TRUE ELSE FALSE
 opcShipTo = lcShipTo
 opcPO     = lcPo
 opiRelQty = liRelQty
 opdtRelDate = ldRelDate
 opdtDelDate = ldDelDate
 oplRelease  = llReplace.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-item B-table-Win 
PROCEDURE release-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF adm-brs-in-update THEN RETURN.
DEF BUFFER bf-oe-rel FOR oe-rel.
DEF VAR lrCurrentRel AS ROWID NO-UNDO.
SESSION:SET-WAIT-STATE ('general').

 /*IF oe-rel.s-code = "I"  THEN DO: 
     MESSAGE "Invoice only record can not Release..." VIEW-AS ALERT-BOX ERROR.
     RETURN.
 END.*/ /* Ticket - 22911 */


lrCurrentRel = ROWID(oe-rel).
FIND FIRST oe-ctrl WHERE oe-ctrl.company = g_company NO-LOCK NO-ERROR.
{sys/inc/addrelse.i}
/*
if v-do-bol then do:
  message "Create Bill of lading?" view-as alert-box question button yes-no
                 update choice as log.
  if choice then do:
     run create-bol.
     return.
  end.
end.
*/
  
  FIND xoe-ord WHERE xoe-ord.company = g_company AND
                     xoe-ord.ord-no = oe-ordl.ord-no NO-LOCK.
  v-scr-s-code = oe-rel.s-code:SCREEN-VALUE IN BROWSE {&browse-name}.
  IF v-scr-s-code NE "I" THEN DO:
    RUN check-release NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.  
  END.
  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

  IF INDEX("ABCPZ", lv-stat) > 0 THEN DO:
     MESSAGE ENTRY(INDEX("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice")
             "release entries can not be modified"
             VIEW-AS ALERT-BOX ERROR.
     RETURN .
  END.

  FOR EACH w-ordl:
    DELETE w-ordl.
  END.

  ASSIGN
     v-auto = NO
     fil_id = RECID(oe-ordl).  

  EMPTY TEMP-TABLE tt-email.
  
  RUN oe/CheckAckPrint.p(INPUT ROWID(oe-ord)).
  RUN oe/actrel.p (RECID(oe-rel), INPUT-OUTPUT iocPrompt).

  RUN send-email-proc.

  RUN release-shared-buffers.

  /* wfk - this is overkill, but making sure there is no way this stays locked */
  FOR EACH bf-oe-rel WHERE bf-oe-rel.company EQ g_company
      AND bf-oe-rel.ord-no EQ oe-ordl.ord-no NO-LOCK.
      FIND oe-rel WHERE ROWID(oe-rel) EQ ROWID(bf-oe-rel) 
          NO-LOCK.
  END.
  FIND oe-rel WHERE ROWID(oe-rel) EQ lrCurrentRel NO-LOCK NO-ERROR.
  RUN notify-source.

SESSION:SET-WAIT-STATE ('').  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-item-all B-table-Win 
PROCEDURE release-item-all :
/*------------------------------------------------------------------------------
  Purpose:  
  Parameters:  <none>
    Notes:  
------------------------------------------------------------------------------*/
SESSION:SET-WAIT-STATE ('general').
FIND FIRST oe-ctrl WHERE oe-ctrl.company = g_company NO-LOCK NO-ERROR.
{sys/inc/addrelse.i}
DEF VAR lrCurrentRel AS ROWID NO-UNDO.
DEF BUFFER bf-oe-rel FOR oe-rel.
/*
if v-do-bol then do:
  message "Create Bill of lading?" view-as alert-box question button yes-no
                 update choice as log.
  if choice then do:
     run create-bol.
     return.
  end.
end.
*/
  
  FIND xoe-ord WHERE xoe-ord.company = g_company AND
                     xoe-ord.ord-no = oe-ordl.ord-no NO-LOCK.

  v-scr-s-code = oe-rel.s-code:SCREEN-VALUE IN BROWSE {&browse-name}.
  IF v-scr-s-code NE "I" THEN DO:
    RUN check-release NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.    
  END.

  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

  IF INDEX("ABCPZ", lv-stat) > 0 THEN DO:
     MESSAGE ENTRY(INDEX("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice")
             "release entries can not be modified"
             VIEW-AS ALERT-BOX ERROR.
     RETURN .
  END.

  DEF VAR ls-rel-what AS cha NO-UNDO.
  RUN oe/d-relwht.w (OUTPUT ls-rel-what) .
  
  FOR EACH w-ordl:
    DELETE w-ordl.
  END.
  v-auto = NO.

  IF ls-rel-what = "all" THEN DO:
     fil_id = RECID(oe-ordl).
     EMPTY TEMP-TABLE tt-email.
     RUN oe/autorel.p .
     RUN send-email-proc.
  END.
  ELSE IF ls-rel-what = "item" THEN DO:
       fil_id = RECID(oe-ordl).  
       RUN oe/actrel.p (RECID(oe-rel), INPUT-OUTPUT iocPrompt).
  END.

  RUN release-shared-buffers.
  /* wfk - this is overkill, but making sure there is no way this stays locked */
  FOR EACH bf-oe-rel WHERE bf-oe-rel.company EQ g_company
      AND bf-oe-rel.ord-no EQ oe-ordl.ord-no NO-LOCK.
      FIND oe-rel WHERE ROWID(oe-rel) EQ ROWID(bf-oe-rel) 
          NO-LOCK.
  END.
  FIND oe-rel WHERE ROWID(oe-rel) EQ lrCurrentRel NO-LOCK NO-ERROR.

  RUN notify-source.
  
SESSION:SET-WAIT-STATE ('').  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-shared-buffers B-table-Win 
PROCEDURE release-shared-buffers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RELEASE xoe-ord.
  RELEASE xoe-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE relticket-printed B-table-Win 
PROCEDURE relticket-printed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  FIND oe-rell WHERE RECID(oe-rell) EQ out-recid NO-LOCK NO-ERROR.
  IF AVAIL oe-rell AND oe-rell.link-no NE 0 THEN
  FIND oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.

  RUN notify-source.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch ('open-query').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.


  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

  RUN dispatch ("row-changed").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_notes B-table-Win 
PROCEDURE select_notes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF AVAIL oe-rel THEN
  RUN windows/datenote.w (INPUT oe-rel.rec_key, INPUT PROGRAM-NAME(1), "RDC,DRC", "R,L").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-email-proc B-table-Win 
PROCEDURE send-email-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR ls-to-list AS cha NO-UNDO.
   DEF VAR lv-mailto AS cha NO-UNDO.
   DEF VAR lv-mailsubject AS cha NO-UNDO.
   DEF VAR lv-mailbody AS cha NO-UNDO.
   DEF VAR retcode AS INT NO-UNDO.
   DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
   
   v-prgmname = "actrel.".

   FOR EACH tt-email,
       FIRST cust WHERE
       cust.company = cocode AND
       cust.cust-no = tt-email.cust-no AND
       cust.active = "E"
       NO-LOCK
       BREAK BY tt-email.ord-no:

       IF FIRST-OF(tt-email.ord-no) THEN
          lv-mailbody = "Order Number " +  STRING(tt-email.ord-no)
                      + " has been released." + CHR(10).

       lv-mailbody = lv-mailbody 
                   + "Item: " + STRING(tt-email.i-no,"X(15)")
                   + " Qty: " + STRING(tt-email.rel-qty,"->>,>>>,>>9")
                   + " Date: " + STRING(tt-email.rel-date,"99/99/99")
                   + " PO#: "  + STRING(tt-email.po-no,"X(15)") + CHR(10).
       
       IF LAST-OF(tt-email.ord-no) THEN DO:
           
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}
           IF ls-to-list NE '' THEN DO:
             ASSIGN lv-mailto = "To:" + ls-to-list
                    lv-mailsubject = "Release Generated".
             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,"",1,OUTPUT retcode).
           END.
       END. /* last-of(tt-email.cust-no) */
   END.
   
END PROCEDURE.

PROCEDURE mail EXTERNAL "xpMail.dll" :
   DEF INPUT PARAM mailTo AS CHAR.
   DEF INPUT PARAM mailsubject AS CHAR.
   DEF INPUT PARAM mailText AS CHAR.
   DEF INPUT PARAM mailFiles AS CHAR.
   DEF INPUT PARAM mailDialog AS LONG.
   DEF OUTPUT PARAM retCode AS LONG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "oe-rel" "company"}
  {src/adm/template/sndkycas.i "Carrier" "oe-rel" "Carrier"}
  {src/adm/template/sndkycas.i "r-no" "oe-rel" "r-no"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "oe-ordl"}
  {src/adm/template/snd-list.i "oe-rel"}
  {src/adm/template/snd-list.i "tt-report"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-buttons B-table-Win 
PROCEDURE set-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
    RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("initial").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-lot-from-boll B-table-Win 
PROCEDURE set-lot-from-boll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Special case where oe-boll exists with a lot-no
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-rel-id AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-rell-id AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-boll-id AS ROWID NO-UNDO.
DEF BUFFER bf-oe-rel FOR oe-rel.
DEF BUFFER bf-oe-rell FOR oe-rell.
DEF BUFFER bf-oe-boll FOR oe-boll.
DEF BUFFER bf-ref FOR reftable.
FIND bf-oe-rel WHERE ROWID(bf-oe-rel) = ipr-rel-id EXCLUSIVE-LOCK.
FIND bf-oe-rell WHERE ROWID(bf-oe-rell) = ipr-rell-id EXCLUSIVE-LOCK.
IF ipr-boll-id NE ? THEN
    FIND bf-oe-boll WHERE ROWID(bf-oe-boll) = ipr-boll-id EXCLUSIVE-LOCK.
IF NOT AVAIL bf-oe-boll THEN
  FIND FIRST bf-oe-boll WHERE bf-oe-boll.company  EQ oe-rell.company
                     AND bf-oe-boll.r-no     EQ oe-rell.r-no
                     AND bf-oe-boll.ord-no   EQ oe-rell.ord-no
                     AND bf-oe-boll.i-no     EQ oe-rell.i-no
                     AND bf-oe-boll.line     EQ oe-rell.line
                   NO-LOCK NO-ERROR.


/* FIND FIRST bf-ref WHERE                                     */
/*       bf-ref.reftable EQ "oe-rel.lot-no" AND                */
/*       bf-ref.company  EQ STRING(bf-oe-rel.r-no,"9999999999")*/
/*       NO-ERROR.                                             */
/*  IF NOT AVAIL bf-ref THEN                                   */
/*  DO:                                                        */
/*     CREATE bf-ref.                                          */
/*     ASSIGN                                                  */
/*       bf-ref.reftable = "oe-rel.lot-no"                     */
/*       bf-ref.company  = STRING(oe-rel.r-no,"9999999999").   */
/*  END.                                                       */
/*                                                             */
/*  IF AVAIL bf-oe-boll THEN                                   */
/*     bf-ref.CODE  = bf-oe-boll.lot-no.                       */
/*  ELSE                                                       */
/*     bf-ref.CODE  = bf-oe-rell.lot-no.                       */
/*                                                             */
/*  IF AVAIL bf-ref AND AVAIL(bf-oe-boll) THEN DO:             */
/*    IF bf-ref.CODE EQ "" AND bf-oe-boll.lot-no NE "" THEN    */
/*       bf-ref.CODE = bf-oe-boll.lot-no.                      */
/*    IF bf-oe-rel.lot-no = "" AND bf-oe-boll.lot-no NE "" THEN*/
/*      bf-oe-rel.lot-no = bf-oe-boll.lot-no.                  */
/*  END.                                                       */
  IF AVAIL bf-oe-boll THEN
     bf-oe-rel.lot-no  = bf-oe-boll.lot-no.
  ELSE
     bf-oe-rel.lot-no  = bf-oe-rell.lot-no.

  IF bf-oe-rel.lot-no <> "" AND AVAIL(bf-oe-boll) THEN DO:
    IF bf-oe-rel.lot-no EQ "" AND bf-oe-boll.lot-no NE "" THEN
       bf-oe-rel.lot-no = bf-oe-boll.lot-no.
    IF bf-oe-rel.lot-no = "" AND bf-oe-boll.lot-no NE "" THEN
      bf-oe-rel.lot-no = bf-oe-boll.lot-no.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.
  adm-cur-state = p-state.
  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE unpost-item B-table-Win 
PROCEDURE unpost-item :
/*------------------------------------------------------------------------------
  Purpose:    from oe/oerelunp.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-oe-ordl FOR oe-ordl.
    
DEF VAR choice AS LOG NO-UNDO.


IF AVAIL oe-rel AND oe-rel.link-no EQ 0 AND USERID("NOSWEAT") NE "ASI" THEN DO:
  MESSAGE "Cannot unpost planned releases, posted only..."
          VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

FIND FIRST oe-ord
    WHERE oe-ord.company EQ oe-rel.company 
      AND oe-ord.ord-no  EQ oe-rel.ord-no
    NO-LOCK NO-ERROR.

{oe/rel-stat.i lv-stat}

RELEASE oe-boll.

IF AVAIL oe-rell AND lv-stat EQ "P" THEN
FIND FIRST oe-boll
    WHERE oe-boll.company  EQ cocode
      AND oe-boll.ord-no   EQ oe-rell.ord-no
      AND oe-boll.line     EQ oe-rell.line
      AND oe-boll.i-no     EQ oe-rell.i-no
      AND oe-boll.r-no     EQ oe-rell.r-no
      AND oe-boll.rel-no   EQ oe-rell.rel-no
      AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
    NO-LOCK NO-ERROR.

IF INDEX("CZ",lv-stat) NE 0 THEN 
  MESSAGE "Cannot unpost, this release has been Invoiced..."
     VIEW-AS ALERT-BOX ERROR.
ELSE
IF AVAIL oe-boll THEN
  MESSAGE "Sorry, first you must delete BOL: " +
          TRIM(STRING(oe-boll.bol-no,">>>>>>>>>>")) + "..."
     VIEW-AS ALERT-BOX ERROR.

ELSE DO:
  IF NOT choice THEN 
    MESSAGE "Warning, this will erase the actual release flag."
            "This will cause the item to appear as unreleased!"
            SKIP "Do you want to continue? "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice .
  
  IF choice THEN DO:


    /* Added to remove release qty from order line total release */
    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE-LOCK NO-ERROR.

    FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ ROWID(oe-rel) EXCLUSIVE-LOCK NO-ERROR.
    
    b-oe-ordl.t-rel-qty = b-oe-ordl.t-rel-qty - oe-rel.qty.
    
    /* oe-rell has link to oe-rel, so use that to get oe-relh */
    FIND FIRST oe-rell
        WHERE oe-rell.company EQ oe-rel.company
          AND oe-rell.ord-no  EQ oe-rel.ord-no      
          AND oe-rell.i-no    EQ oe-rel.i-no
          AND oe-rell.line    EQ oe-rel.line
          AND oe-rell.link-no EQ oe-rel.r-no
        NO-LOCK NO-ERROR.
    IF AVAIL oe-rell THEN
      FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-ERROR.
    
    IF AVAIL oe-relh THEN DO:


      FOR EACH oe-rell
          WHERE oe-rell.company EQ oe-rel.company
            AND oe-rell.ord-no  EQ oe-rel.ord-no            
            AND oe-rell.i-no    EQ oe-rel.i-no
            AND oe-rell.line    EQ oe-rel.line
            AND oe-rell.link-no EQ oe-rel.r-no:
        DELETE oe-rell.
      END. /* Each oe-rell */

      FIND FIRST oe-rell
          WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
          USE-INDEX r-no NO-LOCK NO-ERROR.
      IF NOT AVAIL oe-rell THEN DO:
        oe-relh.posted = NO.
        DELETE oe-relh.
      END. /* no more oe-rell's for this oe-relh */

      RELEASE oe-relh.


    END. /* If avail oe-relh */

    b-oe-rel.link-no = 0.
    RELEASE b-oe-rel.
    FIND CURRENT b-oe-ordl NO-LOCK.

    RUN reopen-query.
    RUN repo-query (ROWID(oe-rel)).

  END.  /* choice */
END.  /* else */

ll-unposted = choice.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-dates B-table-Win 
PROCEDURE update-dates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-date AS DATE NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

  DEF BUFFER b-oe-ordl FOR oe-ordl.


  lv-date = DATE(INT(SUBSTR(tt-report.stat,1,2)),
                 INT(SUBSTR(tt-report.stat,3,2)),
                 INT(SUBSTR(tt-report.stat,5,4))).

  {oe/rel-stat.i lv-stat}

  IF INDEX("AB",lv-stat) GT 0 THEN DO:
    IF AVAIL oe-relh THEN DO TRANSACTION:
      FIND CURRENT oe-relh.
      oe-relh.rel-date = lv-date.
      FIND CURRENT oe-relh NO-LOCK.
    END.

    IF AVAIL oe-relh THEN RUN oe/d-dudate.w (ROWID(oe-relh)).
  END.

  ELSE
  IF INDEX("SLI",lv-stat) GT 0 THEN DO:
    IF NOT (oeDateAuto-log AND OeDateAuto-Char EQ "Colonial") THEN DO:
      ll = NO.
      IF AVAIL oe-ordl AND oe-ordl.req-date GT lv-date THEN
        MESSAGE "Change order line item due date to " + TRIM(STRING(lv-date)) "?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.
      IF ll THEN DO:
        DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.
        FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl).
        b-oe-ordl.req-date = lv-date.
        FIND CURRENT b-oe-ordl NO-LOCK NO-ERROR.
      END.
      ll = NO.
      FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
      IF AVAIL oe-ord AND oe-ord.due-date GT lv-date THEN
        MESSAGE "Change order header due date to " + TRIM(STRING(lv-date)) "?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.
      IF ll THEN DO:
        FIND CURRENT oe-ord NO-ERROR.
        oe-ord.due-date = lv-date.
        FIND CURRENT oe-ord NO-LOCK NO-ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyShipNote d-oeitem
PROCEDURE CopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Copies Ship Note from rec_key to rec_key
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.   

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-rel-qty B-table-Win 
FUNCTION get-rel-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER bf-oe-rel FOR oe-rel.
  IF AVAIL oe-rel THEN
      FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel)
        NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-oe-rel THEN DO:
    FIND bf-oe-rel WHERE RECID(bf-oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
    IF AVAIL bf-oe-rel THEN
      FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(bf-oe-rel) NO-ERROR.
  END.

  RETURN IF /*(NOT oereleas-log AND INDEX("AB",get-rel-stat()) GT 0) OR*/
            (INDEX("SIL",get-rel-stat()) GT 0     
             OR oe-rel.stat:SCREEN-VALUE IN BROWSE {&browse-name} EQ "") THEN 0
         ELSE
         IF AVAIL tt-report                 AND
            INDEX("AB",get-rel-stat()) GT 0 THEN tt-report.qty
         ELSE
         IF AVAIL bf-oe-rel THEN bf-oe-rel.qty
         ELSE INT(oe-rel.qty:SCREEN-VALUE IN BROWSE {&browse-name}).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-rel-stat B-table-Win 
FUNCTION get-rel-stat RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NOT AVAIL oe-rel THEN
  FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.

  RUN oe/rel-stat.p (IF AVAIL oe-rel THEN ROWID(oe-rel) ELSE ?, OUTPUT lv-stat).

  RETURN lv-stat.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-tot-qty B-table-Win 
FUNCTION get-tot-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NOT AVAIL oe-rel THEN DO:
    FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
    IF AVAIL oe-rel THEN
      FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(oe-rel) NO-ERROR.
  END.

  RETURN IF NOT oereleas-log                AND
            AVAIL tt-report                 AND
            INDEX("AB",get-rel-stat()) GT 0 THEN tt-report.qty
         ELSE
         IF AVAIL oe-rel THEN oe-rel.tot-qty
         ELSE INT(oe-rel.tot-qty:SCREEN-VALUE IN BROWSE {&browse-name}).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

