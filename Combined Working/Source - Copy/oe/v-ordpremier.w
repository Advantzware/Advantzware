&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: oe\v-ord.w

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
{oe/ordholdstat.i} 

/* for oecomm.i */
DEF NEW SHARED VAR v-upd-comm AS LOG INITIAL YES NO-UNDO.
DEF NEW SHARED VAR v-misc AS LOG INIT NO NO-UNDO.
DEF NEW SHARED VAR v-fr-tax LIKE oe-ctrl.f-tax NO-UNDO.
{sys/inc/var.i "new shared" }
DEF BUFFER bfx-ord FOR oe-ord.
DEF VAR li-lead-days AS INT NO-UNDO.
DEF VAR ll-f-bill AS LOG NO-UNDO.
DEF VAR li-sold-no AS INT NO-UNDO.
DEF VAR ls-ship-i AS cha EXTENT 4 NO-UNDO.
DEF VAR v-slow-ord AS LOG NO-UNDO.
DEF VAR v-beeler AS LOG NO-UNDO.
DEF VAR v-ilwalker AS LOG NO-UNDO.
DEF NEW SHARED VAR v-create-job AS LOG NO-UNDO.
DEF VAR v-custype LIKE cust.type NO-UNDO.
DEF VAR v-ord-limit LIKE cust.ord-lim NO-UNDO.
DEF VAR v-crd-limit LIKE cust.cr-lim NO-UNDO.
DEF VAR v-valdcode AS cha INIT "ON,BY,MH" NO-UNDO.
DEF VAR v-valtype AS cha INIT "O,R,C" NO-UNDO.
DEF VAR v-duelist AS cha INIT "AM,ASAP,BY,CPU,CR,HFR,HOLD,HOT,INK,MH,MUST,NB4,NCUST,NITEM,NCNI,OE,ON,PPR,RWRK,RUSH,TOOL,WO,$$$" NO-UNDO. /* Task 04081403 */
DEF VAR v-oecount LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-full-cost AS LOG NO-UNDO.
DEF VAR oeprompt LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-quo-price LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-inactive AS LOG NO-UNDO.
DEF VAR save_id AS RECID NO-UNDO.
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF VAR v-job-no LIKE oe-ord.job-no NO-UNDO.
DEF VAR v-job-no2 LIKE oe-ord.job-no2 NO-UNDO.
DEF VAR v-exp-limit AS INT INIT 10 NO-UNDO.
DEF VAR v-n-ord LIKE oe-ctrl.n-ord NO-UNDO.
DEF VAR v-estord-id AS RECID EXTENT 10 NO-UNDO.
DEF VAR v-multord AS LOG NO-UNDO.
DEF VAR ll-ord-no-override AS LOG NO-UNDO.

{ce/print4.i "new shared"}
{ce/print42.i "new shared"}
DEF NEW SHARED WORKFILE work-ordl LIKE oe-ordl.
DEF NEW SHARED VAR nufile AS LOG NO-UNDO.
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.
DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.  /* for oe-ordl.qty and oe-ordm calc */
DEF NEW SHARED VAR v-d-rel AS INT NO-UNDO.
DEF VAR lv-new-row-id AS ROWID NO-UNDO.  /* first creation error */
DEF NEW SHARED VAR v-qty-mod AS LOG NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
DEF VAR ll-order-from-est AS LOG NO-UNDO.  /* is order created from estimate */
DEF VAR ll-cust-displayed AS LOG NO-UNDO.
DEF VAR ll-est-no-mod AS LOG NO-UNDO.
DEF VAR ld-lastship-dec AS DEC NO-UNDO.
DEF VAR ld-lastship-cha AS CHAR NO-UNDO.
DEF VAR ll-valid-po-no AS LOG NO-UNDO.
DEF VAR ll-is-new-rec AS LOG NO-UNDO.
DEF VAR ll-ord-from-est-run AS LOG NO-UNDO.
DEF VAR ll-from-tandem AS LOG NO-UNDO.
DEF VAR lv-old-cust-no LIKE oe-ord.cust-no NO-UNDO.
DEF VAR ll-new-po AS LOG NO-UNDO.
DEF VAR ll-new-due AS LOG NO-UNDO.
DEF VAR lv-type-codes AS CHAR NO-UNDO.
DEF VAR lv-type-dscrs AS CHAR NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ship-from AS CHAR NO-UNDO.
DEF VAR llAutoAddItems AS LOGICAL NO-UNDO.
DEF VAR llCreateFromEst AS LOGICAL NO-UNDO.
DEF VAR oeDateAuto-log AS LOG NO-UNDO.
DEF VAR oeDateAuto-char AS CHAR NO-UNDO.

DEFINE VARIABLE prodDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE dueDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE scheduleHndl AS HANDLE NO-UNDO.
DEFINE VARIABLE copyRecord AS LOGICAL NO-UNDO.
DEFINE VARIABLE copyRowID AS ROWID NO-UNDO.
/* permissions for changing dates and date reason codes */
DEF VAR l-update-reason-perms AS LOG NO-UNDO.
DEF VAR v-access-close AS LOG.
DEF VAR v-access-list AS CHAR.
DEF VAR v-margin AS DEC NO-UNDO.
DEFINE VARIABLE OEPO#Xfer-log AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lv-change-inv-po AS LOGICAL     NO-UNDO.
DEF VAR OEJobHold-log AS LOG NO-UNDO.
DEF VAR lcReturn   AS CHAR NO-UNDO.
DEF VAR llRecFound AS LOG  NO-UNDO.
RUN sys/ref/nk1look.p (cocode, "OEJobHold", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).
IF llRecFound THEN
   OEJobHold-log = LOGICAL(lcReturn) NO-ERROR.  
DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xeb FOR eb.
DEF NEW SHARED BUFFER xef FOR ef.


&Scoped-define sman-fields oe-ord.sman oe-ord.s-pct oe-ord.s-comm

DEF NEW SHARED TEMP-TABLE w-ord NO-UNDO FIELD w-ord-no LIKE oe-ord.ord-no.

DEF TEMP-TABLE old-oe-ord NO-UNDO LIKE oe-ord.

DEF NEW SHARED TEMP-TABLE tt-oe-ordl NO-UNDO LIKE oe-ordl
    FIELD to-be-deleted AS LOG INIT YES
    FIELD row-id AS ROWID
    INDEX row-id row-id. 

/* Keep track of eb.stock values to erase per user input */
DEF TEMP-TABLE tt-del-eb NO-UNDO
    FIELD del-rowid AS ROWID.
 DEF VAR lv-text AS CHAR NO-UNDO.
 DEF VAR v-spec-note AS cha FORM "x(70)" EXTENT 15 NO-UNDO.
     {custom/formtext.i NEW}

ASSIGN
 cocode = g_company
 locode = g_loc.

{oe/tt-item-qty-price.i}

{oe/oe-sysct1.i NEW}
{sys/ref/CustList.i NEW}        
  DO TRANSACTION:
    {sys/inc/oedate.i}
    {sys/inc/oecomb.i}
    {sys/inc/job#.i}
    {sys/inc/graphic.i}
    {sys/inc/oeestcom.i}
    {sys/inc/OEPrepTaxCode.i}
    {sys/inc/shiptorep.i}
    {sys/inc/custlistform.i ""OU1"" }
  END.

RUN methods/prgsecur.p
    (INPUT "OEDateChg",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT l-update-reason-perms, /* Allowed? Yes/NO */
     OUTPUT v-access-close, /* used in template/windows.i  */
     OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */

DEF VAR cRtnChar AS CHAR NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode, "OEPO#Xfer", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cRtnChar, OUTPUT lRecFound).
OEPO#Xfer-log = LOGICAL(cRtnChar) NO-ERROR.


RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeDateAuto-log = LOGICAL(cRtnChar) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeDateAuto-char = cRtnChar NO-ERROR. 
/* transaction */
{sys/inc/f16to32.i}

/* transaction */
 {sys/inc/ceprepprice.i} 
{sys/inc/funcToWorkDay.i}
RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).

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
&Scoped-define EXTERNAL-TABLES oe-ord
&Scoped-define FIRST-EXTERNAL-TABLE oe-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ord.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ord.ord-no oe-ord.est-no oe-ord.job-no ~
oe-ord.job-no2 oe-ord.spare-char-2 oe-ord.sold-id oe-ord.ord-date ~
oe-ord.due-code oe-ord.due-date oe-ord.last-date oe-ord.prod-date ~
oe-ord.po-no oe-ord.contact oe-ord.over-pct oe-ord.under-pct oe-ord.terms ~
oe-ord.tax-gr oe-ord.frt-pay oe-ord.carrier oe-ord.fob-code oe-ord.sman[1] ~
oe-ord.s-pct[1] oe-ord.s-comm[1] oe-ord.sman[2] oe-ord.s-pct[2] ~
oe-ord.s-comm[2] oe-ord.sman[3] oe-ord.s-pct[3] oe-ord.s-comm[3] ~
oe-ord.cc-type oe-ord.cc-expiration oe-ord.cc-num oe-ord.cc-auth ~
oe-ord.spare-char-1 
&Scoped-define ENABLED-TABLES oe-ord
&Scoped-define FIRST-ENABLED-TABLE oe-ord
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 btnCalendar-5 RECT-30 RECT-33 RECT-35 RECT-36 RECT-37 
&Scoped-Define DISPLAYED-FIELDS oe-ord.ord-no oe-ord.est-no oe-ord.job-no ~
oe-ord.job-no2 oe-ord.user-id oe-ord.stat oe-ord.spare-char-2 ~
oe-ord.cust-no oe-ord.sold-id oe-ord.ord-date oe-ord.cust-name ~
oe-ord.sold-name oe-ord.due-code oe-ord.due-date oe-ord.addr[1] ~
oe-ord.sold-addr[1] oe-ord.last-date oe-ord.addr[2] oe-ord.sold-addr[2] ~
oe-ord.prod-date oe-ord.city oe-ord.state oe-ord.zip oe-ord.sold-city ~
oe-ord.sold-state oe-ord.sold-zip oe-ord.po-no oe-ord.contact ~
oe-ord.over-pct oe-ord.under-pct oe-ord.terms oe-ord.terms-d oe-ord.tax-gr ~
oe-ord.managed oe-ord.frt-pay oe-ord.carrier oe-ord.fob-code oe-ord.sman[1] ~
oe-ord.sname[1] oe-ord.s-pct[1] oe-ord.s-comm[1] oe-ord.sman[2] ~
oe-ord.sname[2] oe-ord.s-pct[2] oe-ord.s-comm[2] oe-ord.sman[3] ~
oe-ord.sname[3] oe-ord.s-pct[3] oe-ord.s-comm[3] oe-ord.cc-type ~
oe-ord.cc-expiration oe-ord.cc-num oe-ord.cc-auth oe-ord.spare-char-1 ~
oe-ord.approved-date 
&Scoped-define DISPLAYED-TABLES oe-ord
&Scoped-define FIRST-DISPLAYED-TABLE oe-ord
&Scoped-Define DISPLAYED-OBJECTS fi_type fi_prev_order fi_sname-lbl ~
fi_s-pct-lbl fi_s-comm-lbl fi_sman-lbl 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,calendarPopup,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS oe-ord.cust-no 
&Scoped-define ADM-ASSIGN-FIELDS fi_type oe-ord.stat oe-ord.cust-name ~
oe-ord.sold-name oe-ord.addr[1] oe-ord.sold-addr[1] oe-ord.addr[2] ~
oe-ord.sold-addr[2] oe-ord.city oe-ord.state oe-ord.zip oe-ord.sold-city ~
oe-ord.sold-state oe-ord.sold-zip oe-ord.terms-d fi_prev_order ~
oe-ord.managed oe-ord.sname[1] oe-ord.sname[2] oe-ord.sname[3] 
&Scoped-define calendarPopup btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 btnCalendar-5 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-colonial-rel-date V-table-Win 
FUNCTION get-colonial-rel-date RETURNS DATE
  ( iprRel AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-3 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-4 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-5 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE fi_prev_order AS CHARACTER FORMAT "X(6)":U 
     LABEL "Previous Order #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE fi_s-comm-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Comm.%" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_s-pct-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "% of Sales" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_sman-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_sname-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Name" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_type AS CHARACTER FORMAT "X" 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 3.2 BY 1 TOOLTIP "(O)riginal, (R)epeat, Repeat with (C)hange, inhouse (T)ransfer".

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.4 BY 5.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75.4 BY 5
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75.4 BY 4.76.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.4 BY 4.76.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ord.ord-no AT ROW 1.24 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi_type AT ROW 1.24 COL 29 COLON-ALIGNED HELP
          "Enter Order Type (O)riginal, (R)epeat, repeat with (C)hange"
     oe-ord.est-no AT ROW 1.24 COL 47.2 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     oe-ord.job-no AT ROW 1.24 COL 72.8 COLON-ALIGNED
          LABEL "Job Num" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 16.6 BY 1
     oe-ord.job-no2 AT ROW 1.24 COL 89.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     oe-ord.user-id AT ROW 1.24 COL 108.4 COLON-ALIGNED
          LABEL "Last User"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ord.stat AT ROW 1.24 COL 132.2 COLON-ALIGNED
          LABEL "Status"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     oe-ord.spare-char-2 AT ROW 1.24 COL 144.4 COLON-ALIGNED WIDGET-ID 8
          LABEL "Type" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1 TOOLTIP "Order Hold Type"
     oe-ord.cust-no AT ROW 2.67 COL 10 COLON-ALIGNED
          LABEL "Bill To" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.sold-id AT ROW 2.67 COL 59.2 COLON-ALIGNED
          LABEL "Sold To"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-ord.ord-date AT ROW 2.67 COL 125.4 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.cust-name AT ROW 3.86 COL 10 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     oe-ord.sold-name AT ROW 3.86 COL 59.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     oe-ord.due-code AT ROW 3.86 COL 114.4 COLON-ALIGNED
          LABEL "Due Date" FORMAT "XXXXX"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     oe-ord.due-date AT ROW 3.86 COL 125.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.addr[1] AT ROW 5.05 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     oe-ord.sold-addr[1] AT ROW 5.05 COL 59.2 COLON-ALIGNED
          LABEL ""
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     oe-ord.last-date AT ROW 5.05 COL 125.4 COLON-ALIGNED
          LABEL "Last Ship"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.addr[2] AT ROW 6.24 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     oe-ord.sold-addr[2] AT ROW 6.24 COL 59.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     oe-ord.prod-date AT ROW 6.24 COL 125.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.city AT ROW 7.43 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     oe-ord.state AT ROW 7.43 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     oe-ord.zip AT ROW 7.43 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.sold-city AT ROW 7.43 COL 59.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     oe-ord.sold-state AT ROW 7.43 COL 82.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ord.sold-zip AT ROW 7.43 COL 88.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.po-no AT ROW 7.43 COL 112 COLON-ALIGNED
          LABEL "PO#" FORMAT "x(22)"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-ord.contact AT ROW 8.91 COL 10.6 COLON-ALIGNED
          LABEL "Contact"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     oe-ord.over-pct AT ROW 10.1 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     oe-ord.under-pct AT ROW 11.29 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     oe-ord.terms AT ROW 12.48 COL 15 COLON-ALIGNED
          LABEL "Pay Terms"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     oe-ord.terms-d AT ROW 12.48 COL 27 COLON-ALIGNED NO-LABEL FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     fi_prev_order AT ROW 8.91 COL 60 COLON-ALIGNED WIDGET-ID 2
     oe-ord.tax-gr AT ROW 10.1 COL 60 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ord.managed AT ROW 11.29 COL 50
          VIEW-AS TOGGLE-BOX
          SIZE 26 BY 1
     oe-ord.frt-pay AT ROW 10.05 COL 89 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Prepaid", "P":U,
"Collect", "C":U,
"Bill", "B":U,
"3rd Party", "T":U
          SIZE 50 BY .95
     oe-ord.carrier AT ROW 12.19 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     oe-ord.fob-code AT ROW 12.19 COL 120 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "DEST", "DEST":U,
"ORIG", "ORIG":U
          SIZE 23 BY .95
     oe-ord.sman[1] AT ROW 14.76 COL 2.8 COLON-ALIGNED NO-LABEL FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.sname[1] AT ROW 14.76 COL 12.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     oe-ord.s-pct[1] AT ROW 14.76 COL 49.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.s-comm[1] AT ROW 14.76 COL 63.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.sman[2] AT ROW 15.95 COL 2.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.sname[2] AT ROW 15.95 COL 12.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     oe-ord.s-pct[2] AT ROW 15.95 COL 49.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.s-comm[2] AT ROW 15.95 COL 63.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.sman[3] AT ROW 17.14 COL 2.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.sname[3] AT ROW 17.14 COL 12.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     oe-ord.s-pct[3] AT ROW 17.14 COL 49.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.s-comm[3] AT ROW 17.14 COL 63.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ord.cc-type AT ROW 14.33 COL 96 COLON-ALIGNED
          LABEL "Payment Type"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-ord.cc-expiration AT ROW 14.33 COL 120 COLON-ALIGNED
          LABEL "Expire"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-ord.cc-num AT ROW 15.52 COL 91 COLON-ALIGNED
          LABEL "Account #"
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     oe-ord.cc-auth AT ROW 16.71 COL 91 COLON-ALIGNED
          LABEL "Ref #"
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     fi_sname-lbl AT ROW 13.95 COL 15.8 COLON-ALIGNED NO-LABEL
     fi_s-pct-lbl AT ROW 13.95 COL 48.2 COLON-ALIGNED NO-LABEL
     fi_s-comm-lbl AT ROW 13.95 COL 63.2 COLON-ALIGNED NO-LABEL
     fi_sman-lbl AT ROW 13.95 COL 2.8 NO-LABEL
     btnCalendar-1 AT ROW 2.67 COL 144.4
     btnCalendar-2 AT ROW 3.86 COL 144.4
     btnCalendar-3 AT ROW 5.05 COL 144.4
     btnCalendar-4 AT ROW 6.24 COL 144.4
     btnCalendar-5 AT ROW 14.33 COL 138
     oe-ord.spare-char-1 AT ROW 15.52 COL 133.6 COLON-ALIGNED WIDGET-ID 4
          LABEL "VCode" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     oe-ord.approved-date AT ROW 2.67 COL 99 COLON-ALIGNED HELP
          "Enter the date this order was approved" WIDGET-ID 10
          LABEL "Hold/Appr Date"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     "Freight Charge" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.1 COL 81
          FGCOLOR 9 
     "FOB:" VIEW-AS TEXT
          SIZE 6 BY .86 AT ROW 12.19 COL 113
     RECT-30 AT ROW 8.71 COL 1.6
     RECT-33 AT ROW 8.71 COL 78
     RECT-35 AT ROW 13.71 COL 78
     RECT-36 AT ROW 13.71 COL 1.6
     RECT-37 AT ROW 2.52 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ord
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
         HEIGHT             = 17.62
         WIDTH              = 153.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

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

/* SETTINGS FOR FILL-IN oe-ord.addr[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.addr[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.approved-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-4 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-5 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN oe-ord.cc-auth IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-expiration IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-num IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cc-type IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.city IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.contact IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.cust-name IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ord.cust-no IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN oe-ord.due-code IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ord.due-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.est-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN fi_prev_order IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_s-comm-lbl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_s-pct-lbl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sman-lbl IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi_sname-lbl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_type IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.job-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ord.job-no2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.last-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX oe-ord.managed IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.ord-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.po-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ord.s-comm[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.s-pct[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sman[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ord.sname[1] IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ord.sname[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.sname[3] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.sold-addr[1] IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ord.sold-addr[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.sold-city IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.sold-id IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.sold-name IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.sold-state IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.sold-zip IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ord.spare-char-2 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ord.stat IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ord.state IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.tax-gr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.terms IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.terms-d IN FRAME F-Main
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN oe-ord.user-id IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ord.zip IN FRAME F-Main
   NO-ENABLE 2                                                          */
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
ON CTRL-O OF FRAME F-Main
OR ctrl-o OF btnCalendar-1 ANYWHERE
DO:
  DEF VAR char-hdl AS CHAR.

  ll-ord-no-override = TRUE.
  /* Add with ctrl-o allows user to specify an order number */
  /* prior to getting the next sequence value for order number */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN manual-apply-add IN WIDGET-HANDLE(char-hdl).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR look-recid AS RECID NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.


  lw-focus = FOCUS.

    CASE lw-focus:NAME :
         WHEN "est-no" THEN DO:
              RUN windows/l-est.w (g_company,g_loc,oe-ord.est-no:screen-value, OUTPUT char-val).
              IF char-val <> "" THEN DO:
                 FIND FIRST eb WHERE STRING(RECID(eb)) = char-val NO-LOCK NO-ERROR.
                 IF AVAIL eb THEN DO:
                   oe-ord.est-no:screen-value = eb.est-no.
                   APPLY "value-changed" TO oe-ord.est-no.
                   RUN valid-cust-user("est") NO-ERROR.
                   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
                   RUN get-from-est.
                 END.
              END.  
         END.
         WHEN "cust-no" THEN DO:
              RUN windows/l-custact.w (g_company, oe-ord.cust-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
              FIND cust WHERE RECID(cust) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL cust THEN DO:
                oe-ord.cust-no:SCREEN-VALUE = cust.cust-no.
                RUN new-cust-no.  
              END.
         END.  

        WHEN "spare-char-2" THEN DO:
            RUN windows/l-holdtype.w (OUTPUT char-val).
            /* If value selected, set code to first entry of string,
               set tooltip to second entry of string (description). */
            IF char-val <> "" THEN
                ASSIGN lw-focus:SCREEN-VALUE = TRIM(char-val)
                       lw-focus:TOOLTIP = getOrdStatDescr(TRIM(char-val)).
        END.

         WHEN "sold-id" THEN DO:
              RUN windows/l-soldto.w (g_company, oe-ord.cust-no:screen-value,oe-ord.sold-id:screen-value, OUTPUT char-val).
              IF char-val <> "" THEN DO:
                 ASSIGN /*oe-ord.sold-no:screen-value      = soldto.sold-no*/             
                        li-sold-no = int(ENTRY(1,char-val))
                        oe-ord.sold-id:screen-value      = ENTRY(2,char-val) 
                        oe-ord.sold-name:screen-value    = ENTRY(3,char-val)
                        oe-ord.sold-addr[1]:screen-value = ENTRY(4,char-val)
                        oe-ord.sold-addr[2]:screen-value = ENTRY(5,char-val)
                        oe-ord.sold-city:screen-value    = ENTRY(6,char-val)
                        oe-ord.sold-state:screen-value   = ENTRY(7,char-val)
                        oe-ord.sold-zip:screen-value     = ENTRY(8,char-val)
                        .

              END.
         END.  
         WHEN "sman" THEN DO:
              li = FRAME-INDEX.
              RUN windows/l-sman.w (g_company, OUTPUT char-val).
              IF char-val NE "" THEN DO:
                IF li EQ 1 AND oe-ord.sman[1]:screen-value NE entry(1,char-val) THEN 
                  oe-ord.sman[1]:screen-value = ENTRY(1,char-val).
                ELSE
                IF li EQ 2 AND oe-ord.sman[2]:screen-value NE entry(1,char-val) THEN 
                  oe-ord.sman[2]:screen-value = ENTRY(1,char-val).
                ELSE
                IF li EQ 3 AND oe-ord.sman[3]:screen-value NE entry(1,char-val) THEN 
                  oe-ord.sman[3]:screen-value = ENTRY(1,char-val).
                ELSE li = 0.
                IF li NE 0 THEN RUN new-sman (li).
              END.
         END.  
         WHEN "tax-gr" THEN DO:
              RUN windows/l-stax.w (g_company,oe-ord.tax-gr:screen-value, OUTPUT char-val).
              IF char-val <> "" THEN oe-ord.tax-gr:screen-value = ENTRY(1,char-val).
         END.
         WHEN "carrier" THEN DO:
              RUN windows/l-carrie.w (g_company,g_loc,oe-ord.carrier:screen-value, OUTPUT char-val).
              IF char-val <> "" THEN oe-ord.carrier:screen-value = ENTRY(1,char-val).
         END.
         WHEN "terms" THEN DO:
              RUN windows/l-terms.w (g_company,oe-ord.terms:screen-value, OUTPUT char-val).
              IF char-val <> "" AND entry(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
                oe-ord.terms:screen-value = ENTRY(1,char-val).
                APPLY "value-changed" TO oe-ord.terms.
              END.
         END.
         WHEN "due-code" THEN DO:
              RUN windows/l-dcode.w (v-duelist, OUTPUT char-val).
              IF char-val <> "" THEN oe-ord.due-code:SCREEN-VALUE = ENTRY(1,char-val).

         END.
         WHEN "fi_type" THEN DO:
              RUN windows/l-ordtyp.w (fi_type:SCREEN-VALUE, OUTPUT char-val).
              IF char-val <> "" THEN fi_type:SCREEN-VALUE = ENTRY(1,char-val).
         END.
    END CASE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 V-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i oe-ord.ord-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 V-table-Win
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i oe-ord.due-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 V-table-Win
ON CHOOSE OF btnCalendar-3 IN FRAME F-Main
DO:
  {methods/btnCalendar.i oe-ord.last-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-4 V-table-Win
ON CHOOSE OF btnCalendar-4 IN FRAME F-Main
DO:
  {methods/btnCalendar.i oe-ord.prod-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-5 V-table-Win
ON CHOOSE OF btnCalendar-5 IN FRAME F-Main
DO:
  {methods/btnCalendar.i oe-ord.cc-expiration}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.carrier V-table-Win
ON LEAVE OF oe-ord.carrier IN FRAME F-Main /* Carrier */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    IF oe-ord.carrier:screen-value <> "" AND
       NOT CAN-FIND(FIRST carrier WHERE carrier.company = g_company AND
                                  carrier.loc = g_loc AND
                                  carrier.carrier = oe-ord.carrier:screen-value)
    THEN DO:                              
         MESSAGE "Invalid Carrier. Try help. " VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.                                
    END.  
    {&methods/lValidateError.i NO}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.cc-expiration
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.cc-expiration V-table-Win
ON HELP OF oe-ord.cc-expiration IN FRAME F-Main /* Expire */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.cust-no V-table-Win
ON ENTRY OF oe-ord.cust-no IN FRAME F-Main /* Bill To */
DO:
  lv-old-cust-no = {&self-name}:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.cust-no V-table-Win
ON LEAVE OF oe-ord.cust-no IN FRAME F-Main /* Bill To */
DO:
  IF LASTKEY NE -1 /*AND LASTKEY NE KEYCODE(KBLABEL("SHIFT-TAB"))*/ THEN DO:
    IF TRIM(oe-ord.cust-no:SCREEN-VALUE) NE "" THEN DO:
       RUN valid-cust-user("") NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-cust-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      IF ll-from-tandem THEN RUN est-from-tandem.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.cust-no V-table-Win
ON VALUE-CHANGED OF oe-ord.cust-no IN FRAME F-Main /* Bill To */
DO:
  /*RUN new-cust-no.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.due-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.due-code V-table-Win
ON LEAVE OF oe-ord.due-code IN FRAME F-Main /* Due Date */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    IF oe-ord.due-code:screen-value <> "" AND
       lookup(oe-ord.due-code:screen-value,v-duelist) = 0 THEN 
    DO:
       MESSAGE "Invalid Due Code. Try help. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.due-date V-table-Win
ON HELP OF oe-ord.due-date IN FRAME F-Main /* Due Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.due-date V-table-Win
ON LEAVE OF oe-ord.due-date IN FRAME F-Main /* Due Date */
DO:

  IF LASTKEY = -1 THEN RETURN.
   {&methods/lValidateError.i YES}
  dueDateChanged = SELF:MODIFIED. /* used in proc local-assign-record */
  IF SELF:modified AND date(SELF:screen-value) < date(oe-ord.ord-date:screen-value) THEN DO:
     MESSAGE "Due Date can not be earlier than Order Date(" oe-ord.ord-date:SCREEN-VALUE ")." VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.

  RUN valid-due-date NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    RETURN NO-APPLY.
  IF DATE(oe-ord.due-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) > DATE(oe-ord.last-date:SCREEN-VALUE) 
  THEN oe-ord.last-date:SCREEN-VALUE = oe-ord.due-date:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.est-no V-table-Win
ON ENTRY OF oe-ord.est-no IN FRAME F-Main /* Estimate # */
DO:
  IF ll-from-tandem THEN DO:
    {&self-name}:SCREEN-VALUE = "".
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.

  ELSE ll-est-no-mod = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.est-no V-table-Win
ON LEAVE OF oe-ord.est-no IN FRAME F-Main /* Estimate # */
DO:
  IF LASTKEY NE -1 THEN DO:
      RUN valid-cust-user("est") NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-est-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF ll-est-no-mod AND TRIM({&self-name}:SCREEN-VALUE) NE "" THEN DO:
      RUN get-from-est.
      IF RETURN-VALUE NE "" THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.est-no V-table-Win
ON VALUE-CHANGED OF oe-ord.est-no IN FRAME F-Main /* Estimate # */
DO:
  ASSIGN
   ll-est-no-mod            = YES
   oe-ord.cust-no:SENSITIVE = TRIM({&self-name}:SCREEN-VALUE) EQ "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_type V-table-Win
ON ENTRY OF fi_type IN FRAME F-Main /* Type */
DO:
   IF SELF:SCREEN-VALUE EQ "T" AND
      CAN-FIND(FIRST cust WHERE cust.company EQ cocode
                            AND cust.cust-no EQ oe-ord.cust-no:SCREEN-VALUE
                            AND cust.active  EQ "X") THEN DO:
     APPLY "leave" TO SELF.
     RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_type V-table-Win
ON LEAVE OF fi_type IN FRAME F-Main /* Type */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_type V-table-Win
ON VALUE-CHANGED OF fi_type IN FRAME F-Main /* Type */
DO:
  RUN new-type.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.fob-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.fob-code V-table-Win
ON RETURN OF oe-ord.fob-code IN FRAME F-Main /* FOB Code */
DO:
     APPLY "tab" TO SELF.
   RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.frt-pay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.frt-pay V-table-Win
ON RETURN OF oe-ord.frt-pay IN FRAME F-Main /* Freight Pay Code */
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.job-no V-table-Win
ON ENTRY OF oe-ord.job-no IN FRAME F-Main /* Job Num */
DO:
    IF oe-ord.est-no:screen-value = "" THEN DO:
       APPLY "tab" TO SELF.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.job-no V-table-Win
ON LEAVE OF oe-ord.job-no IN FRAME F-Main /* Job Num */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.job-no2 V-table-Win
ON ENTRY OF oe-ord.job-no2 IN FRAME F-Main /* Run # */
DO:
    IF oe-ord.est-no:screen-value = "" THEN DO:
       APPLY "tab" TO SELF.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.job-no2 V-table-Win
ON LEAVE OF oe-ord.job-no2 IN FRAME F-Main /* Run # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.last-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.last-date V-table-Win
ON HELP OF oe-ord.last-date IN FRAME F-Main /* Last Ship */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.last-date V-table-Win
ON LEAVE OF oe-ord.last-date IN FRAME F-Main /* Last Ship */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    IF SELF:modified AND date(SELF:screen-value) < TODAY THEN DO:
       MESSAGE "Last Ship Date can not be earlier than TODAY." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.ord-date V-table-Win
ON HELP OF oe-ord.ord-date IN FRAME F-Main /* Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.ord-date V-table-Win
ON LEAVE OF oe-ord.ord-date IN FRAME F-Main /* Date */
DO:
    IF LASTKEY = -1 THEN RETURN.
  /*  
    if self:modified and date(self:screen-value) < today then do:
       message "Order Date can not be earlier than TODAY." view-as alert-box error.
       return no-apply.
    end.
  */  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.ord-no V-table-Win
ON LEAVE OF oe-ord.ord-no IN FRAME F-Main /* Order# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ord-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.po-no V-table-Win
ON LEAVE OF oe-ord.po-no IN FRAME F-Main /* PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-po-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.prod-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.prod-date V-table-Win
ON HELP OF oe-ord.prod-date IN FRAME F-Main /* Prod. Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.prod-date V-table-Win
ON LEAVE OF oe-ord.prod-date IN FRAME F-Main /* Prod. Date */
DO:
  IF LASTKEY = -1 THEN RETURN.
  {&methods/lValidateError.i YES}
  prodDateChanged = SELF:MODIFIED AND SELF:SCREEN-VALUE NE "". /* used in proc local-assign-record */
  IF SELF:modified AND date(SELF:screen-value) < TODAY THEN DO:
     MESSAGE "Prod. Date can not be earlier than TODAY." VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.prod-date V-table-Win
ON VALUE-CHANGED OF oe-ord.prod-date IN FRAME F-Main /* Prod. Date */
DO:
   IF LASTKEY = -1 THEN RETURN.
   prodDateChanged = SELF:MODIFIED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.sman[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.sman[1] V-table-Win
ON LEAVE OF oe-ord.sman[1] IN FRAME F-Main /* Salesman Code[1] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sman (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.sman[1] V-table-Win
ON VALUE-CHANGED OF oe-ord.sman[1] IN FRAME F-Main /* Salesman Code[1] */
DO:
  RUN new-sman (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.sman[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.sman[2] V-table-Win
ON LEAVE OF oe-ord.sman[2] IN FRAME F-Main /* Salesman Code[2] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sman (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.sman[2] V-table-Win
ON VALUE-CHANGED OF oe-ord.sman[2] IN FRAME F-Main /* Salesman Code[2] */
DO:
  RUN new-sman (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.sman[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.sman[3] V-table-Win
ON LEAVE OF oe-ord.sman[3] IN FRAME F-Main /* Salesman Code[3] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sman (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.sman[3] V-table-Win
ON VALUE-CHANGED OF oe-ord.sman[3] IN FRAME F-Main /* Salesman Code[3] */
DO:
  RUN new-sman (3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.sold-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.sold-id V-table-Win
ON LEAVE OF oe-ord.sold-id IN FRAME F-Main /* Sold To */
DO:
  IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}
    FIND FIRST soldto WHERE soldto.company = g_company AND
                            soldto.cust-no = oe-ord.cust-no:screen-value
                        AND trim(soldto.sold-id) = trim(oe-ord.sold-id:screen-value)
                        NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN 
       ASSIGN /*oe-ord.sold-no:screen-value      = soldto.sold-no*/             
             li-sold-no = soldto.sold-no
             oe-ord.sold-id:screen-value      = soldto.sold-id
             oe-ord.sold-name:screen-value    = soldto.sold-name
             oe-ord.sold-addr[1]:screen-value = soldto.sold-addr[1]
             oe-ord.sold-addr[2]:screen-value = soldto.sold-addr[2]
             oe-ord.sold-city:screen-value    = soldto.sold-city
             oe-ord.sold-state:screen-value   = soldto.sold-state
             oe-ord.sold-zip:screen-value     = soldto.sold-zip.

    ELSE IF oe-ord.sold-id:screen-value <> "" THEN DO:
         MESSAGE "Invalid Sold To. Try help. " VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.
   {&methods/lValidateError.i NO}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.spare-char-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.spare-char-2 V-table-Win
ON LEAVE OF oe-ord.spare-char-2 IN FRAME F-Main /* Type */
DO:
    /* If the hold type value was changed, determine if line items need to be changed. */
IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}

    IF oe-ord.spare-char-2:SCREEN-VALUE <> "" THEN DO:  /* task 08011408 */
        IF LOOKUP(oe-ord.spare-char-2:SCREEN-VALUE,gcOrdStatList) = 0  THEN DO:
          MESSAGE "Invalid Type. Try help. " VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
        END.

    END.

  IF oe-ord.spare-char-2:SCREEN-VALUE <> oe-ord.spare-char-2 THEN DO:

      DEF VAR viCount AS INT NO-UNDO INIT 0.

      /* Get number of items on the order. */
      RUN os-Get-Num-Items (INPUT oe-ord.company, INPUT oe-ord.ord-no, OUTPUT viCount).

      /* If 1 item, then set flag to update the item status. */
      IF viCount = 1 THEN
          ASSIGN glStatTypeItemUpdate = YES.

      /* If more than 1 item, prompt to update item status. 
         Procedure sets glStatTypeItemUpdate flag in ordholdstat.i. 
         Update will occur in local-update-record (on save) */
      IF viCount > 1 THEN
          RUN os-Prompt-Item-Update.


  END.
{&methods/lValidateError.i NO}
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.tax-gr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.tax-gr V-table-Win
ON LEAVE OF oe-ord.tax-gr IN FRAME F-Main /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-gr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.terms V-table-Win
ON LEAVE OF oe-ord.terms IN FRAME F-Main /* Pay Terms */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    IF oe-ord.terms:screen-value <> "" AND
       NOT CAN-FIND(FIRST terms WHERE terms.t-code = oe-ord.terms:screen-value)
    THEN DO:
       MESSAGE "Invalid Terms Code. Try help. " VIEW-AS ALERT-BOX ERROR.
       oe-ord.terms-d:screen-value = "".
       RETURN NO-APPLY.
    END.

    FIND FIRST terms WHERE terms.t-code = oe-ord.terms:screen-value NO-LOCK NO-ERROR.
    IF AVAIL terms THEN oe-ord.terms-d:screen-value = terms.dscr.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.terms V-table-Win
ON VALUE-CHANGED OF oe-ord.terms IN FRAME F-Main /* Pay Terms */
DO:
  DEF VAR li AS INT NO-UNDO.


  FIND terms WHERE terms.t-code EQ oe-ord.terms:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAIL terms THEN
    ASSIGN
     oe-ord.terms:SCREEN-VALUE   = terms.t-code
     oe-ord.terms-d:SCREEN-VALUE = terms.dscr.

  oe-ord.terms:SCREEN-VALUE = CAPS(oe-ord.terms:SCREEN-VALUE).
  DO li = 1 TO LENGTH(oe-ord.terms:SCREEN-VALUE):
    APPLY "cursor-right" TO oe-ord.terms.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  {sys/inc/f3help.i}
  SESSION:DATA-ENTRY-RETURN = YES.

  RUN oe/oe-sysct.p.

  {oe/oe-sysct.i}

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "FASTOE"
       NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN sys-ctrl.company  = cocode
            sys-ctrl.name     = "FASTOE"
            sys-ctrl.descrip  = "Quick Order Creation"
            sys-ctrl.char-fld = "ASI"
            sys-ctrl.log-fld  = NO.
      MESSAGE "System control record NOT found. " sys-ctrl.descrip
          UPDATE sys-ctrl.char-fld.
  END.
  ASSIGN v-slow-ord = NOT (sys-ctrl.char-fld EQ "Argrov" OR
                           sys-ctrl.char-fld EQ "Beeler")
         v-beeler   = sys-ctrl.char-fld EQ "Beeler"
         v-ilwalker = sys-ctrl.char-fld EQ "ILWalker".

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "JOBCREAT"
       NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN sys-ctrl.company = cocode
            sys-ctrl.name    = "JOBCREAT"
            sys-ctrl.descrip = "Create Job Standards during OE?"
            sys-ctrl.log-fld = NO.
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  END.

  v-create-job = sys-ctrl.log-fld.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "OEPROMPT"
       NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN sys-ctrl.company = cocode
            sys-ctrl.name    = "OEPROMPT"
            sys-ctrl.log-fld = YES
            sys-ctrl.descrip = "Prompt for duplicate PO?".
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  END.

  oeprompt = sys-ctrl.log-fld.


  IF v-oecomm-cha NE "Manual" THEN RUN hide-comm (YES).

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-order V-table-Win 
PROCEDURE add-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch ('add-record').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-tandem V-table-Win 
PROCEDURE add-tandem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ll-from-tandem = YES.

  RUN dispatch ("add-record").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addItems V-table-Win 
PROCEDURE addItems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR char-hdl AS CHAR NO-UNDO.

IF llAutoAddItems THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
    RUN select-page IN WIDGET-HANDLE(char-hdl) (3).

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"oeitem-target",OUTPUT char-hdl).      
    RUN add-auto IN WIDGET-HANDLE(char-hdl). 

/*     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl). */
/*     RUN notify IN WIDGET-HANDLE(char-hdl) ('row-available').                               */
/*     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl). */
/*     RUN notify IN WIDGET-HANDLE(char-hdl) ('row-available').                               */
END.
llAutoAddItems = NO.
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
  {src/adm/template/row-list.i "oe-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ord"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bill_notes V-table-Win 
PROCEDURE bill_notes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF BUFFER bf-cust-note FOR cust .
IF  adm-new-record THEN DO:
DO WITH FRAME {&FRAME-NAME}:
    FOR EACH tt-formtext:
        DELETE tt-formtext .
    END.

     FIND FIRST cust WHERE cust.company EQ cocode
                 AND cust.cust-no EQ oe-ord.cust-no:SCREEN-VALUE
                 NO-LOCK NO-ERROR.

     IF AVAIL cust THEN
     FIND FIRST notes WHERE  notes.rec  = cust.rec_key 
                     AND notes.note_type = "G"
                     AND note_group  = "BN" NO-LOCK NO-ERROR.

     IF NOT AVAIL notes THEN DO:
         FIND FIRST bf-cust-note WHERE bf-cust-note.company EQ cocode
             AND bf-cust-note.active  EQ "X" NO-LOCK NO-ERROR.

        IF AVAIL bf-cust-note THEN
        FIND FIRST notes WHERE notes.rec  = bf-cust-note.rec_key 
                     AND notes.note_type = "G"
                     AND note_group  = "BN" NO-LOCK NO-ERROR.
     END. /* not avail notes */

     IF AVAIL notes THEN DO:
         lv-text  = notes.note_text .

          DO i = 1 TO 15:
              CREATE tt-formtext.
                  ASSIGN tt-line-no = i
                         tt-length  = 70. 
            END.
            RUN custom/formtext.p (lv-text).
            i = 0.
            /*v-inst2 = "".*/
            ASSIGN v-spec-note = "" .
            FOR EACH tt-formtext:
                  i = i + 1.
                  IF  i <= 15 THEN v-spec-note[i] = tt-formtext.tt-text.  
            END.

             ASSIGN
         oe-ord.bill-i[1] = v-spec-note[1]
         oe-ord.bill-i[2] = v-spec-note[2]
         oe-ord.bill-i[3] = v-spec-note[3]
         oe-ord.bill-i[4] = v-spec-note[4].
     END.
  END. /* bill notes */

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE change-page-logic V-table-Win 
PROCEDURE change-page-logic :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Give focus to something in frame so that ctrl-o will be caught */
  /* This procedure called from change-page in w-order */
  APPLY 'entry' TO btnCalendar-1 IN FRAME f-main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-record V-table-Win 
PROCEDURE check-for-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT-OUTPUT PARAM op-avail AS LOG NO-UNDO.

op-avail =  AVAIL {&FIRST-ENABLED-TABLE}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-tandem-button V-table-Win 
PROCEDURE check-tandem-button :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-enabled AS LOG NO-UNDO.


  RUN custom/frame-en.p (FRAME {&FRAME-NAME}:HANDLE, "{&ENABLED-FIELDS}", OUTPUT op-enabled).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-use-1 V-table-Win 
PROCEDURE check-use-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF oe-ord.est-no:SCREEN-VALUE NE "" THEN DO:
      FIND FIRST est
           WHERE est.company EQ cocode
             AND est.est-no  EQ oe-ord.est-no:SCREEN-VALUE 
           NO-LOCK NO-ERROR.

       {est/checkuse.i "no cancel"} 
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-use-2 V-table-Win 
PROCEDURE check-use-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    &Scoped-define SECOND-EXTERNAL-TABLE itemfg
    &Scoped-define THIRD-EXTERNAL-TABLE job
    &Scoped-define FOURTH-EXTERNAL-TABLE job-hdr
    &Scoped-define FIFTH-EXTERNAL-TABLE 

    FOR EACH oe-ordl
        WHERE oe-ordl.company EQ oe-ord.company
          AND oe-ordl.ord-no  EQ oe-ord.ord-no
          NO-LOCK:

      FIND FIRST itemfg
          WHERE itemfg.company EQ oe-ordl.company
            AND itemfg.i-no    EQ oe-ordl.i-no 
            NO-LOCK NO-ERROR.

      IF TRIM(oe-ordl.job-no) NE "" THEN
      FIND FIRST job
          WHERE job.company EQ oe-ordl.company
            AND job.job-no  EQ oe-ordl.job-no
            AND job.job-no2 EQ oe-ordl.job-no2
          NO-LOCK NO-ERROR.

      IF AVAIL job THEN
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.i-no    EQ oe-ordl.i-no
          NO-LOCK NO-ERROR.

      DO WHILE TRUE:
        {custom/checkuse.i "no cancel"}

        RELEASE itemfg.

        IF AVAIL job-hdr THEN
        FIND NEXT job-hdr
            WHERE job-hdr.company EQ job.company
              AND job-hdr.job     EQ job.job
              AND job-hdr.job-no  EQ job.job-no
              AND job-hdr.job-no2 EQ job.job-no2
              AND job-hdr.i-no    EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.
        IF NOT AVAIL job-hdr THEN LEAVE.
      END.
    END.

    &Scoped-define SECOND-EXTERNAL-TABLE
    &Scoped-define THIRD-EXTERNAL-TABLE
    &Scoped-define FOURTH-EXTERNAL-TABLE
    &Scoped-define FIFTH-EXTERNAL-TABLE
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkOrdNo V-table-Win 
PROCEDURE checkOrdNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT-OUTPUT PARAM io-ord-no AS INT NO-UNDO.

  DEFINE BUFFER b-oe-ord FOR oe-ord.

  DO WHILE CAN-FIND(FIRST b-oe-ord
                    WHERE b-oe-ord.company EQ g_company
                      AND b-oe-ord.ord-no  EQ io-ord-no
                      AND ROWID(b-oe-ord)  NE ipRowID):
    io-ord-no = io-ord-no + 1.
    RUN updateOrdNo (INPUT-OUTPUT io-ord-no).
    IF io-ord-no EQ ? THEN DO:
        MESSAGE "Waiting to get next order number..." .
        PAUSE 1.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-reopen V-table-Win 
PROCEDURE close-reopen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

  RUN browse-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid).

  RUN oe/d-clsoe.w (ROWID(oe-ord)).

  RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (lv-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyOrder V-table-Win 
PROCEDURE copyOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipFromCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipFromOrdNo AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ipToOrdNo AS INTEGER NO-UNDO.    

    RUN oe/copyord2.p (INPUT ipFromCompany,
                      INPUT ipToCompany,
                      INPUT ipFromOrdNo,
                      INPUT-OUTPUT ipToOrdNo,
                      INPUT-OUTPUT fil_id,
                      INPUT-OUTPUT v-qty-mod,
                      INPUT-OUTPUT nufile,
                      INPUT NO /* don't increment order # */).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-empty-order V-table-Win 
PROCEDURE create-empty-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Created to ensure that the order number does not conflict
               with another being currently added (since we don't have 
               a sequence for order number. If this succeeds, can continue
               with the order copy.
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcCompany   AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiNextOrdNo AS INT  NO-UNDO.

DEF BUFFER bf-ord FOR oe-ord.

CREATE bf-ord.
ASSIGN bf-ord.company = ipcCompany
       bf-ord.ord-no  = ipiNextOrdNo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-job V-table-Win 
PROCEDURE create-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-recid AS RECID NO-UNDO.

  DEF BUFFER v-ord-job-hdr FOR job-hdr.

  DEF VAR v-job-job LIKE job.job NO-UNDO.
  DEF VAR v-job-no LIKE job.job-no NO-UNDO.
  DEF VAR v-job-no2 LIKE job.job-no2 NO-UNDO.
  DEF VAR li-j-no AS INT NO-UNDO.

  /* === from oe/oe-ord1.p  ============= */

  FIND LAST job WHERE job.company EQ cocode NO-LOCK NO-ERROR.
  v-job-job = IF AVAIL job THEN job.job + 1 ELSE 1.
  ASSIGN
   v-job-no  = oe-ord.job-no
   v-job-no2 = oe-ord.job-no2.

  FOR EACH job
      WHERE job.company EQ cocode
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2:
    DELETE job.
  END.

  CREATE job.
  ASSIGN job.job        = v-job-job
         job.company    = cocode
         job.loc        = locode
         job.est-no     = oe-ord.est-no
         job.job-no     = v-job-no
         job.job-no2    = v-job-no2
         job.stat       = "P"
         op-recid = RECID(job).

  FOR EACH oe-ordl WHERE oe-ordl.company EQ oe-ord.company
                     AND oe-ordl.ord-no  EQ oe-ord.ord-no exclusive:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ oe-ord.job-no
            AND job-hdr.job-no2 EQ oe-ord.job-no2
            AND job-hdr.ord-no  EQ oe-ord.ord-no
            AND job-hdr.i-no    EQ oe-ordl.i-no
          NO-ERROR.

      IF NOT AVAIL job-hdr THEN DO:
         FIND FIRST itemfg WHERE itemfg.company EQ oe-ordl.company
                             AND itemfg.i-no    EQ oe-ordl.i-no
                             NO-LOCK NO-ERROR.   

         CREATE job-hdr.
         ASSIGN job-hdr.company      = cocode
                job-hdr.loc          = locode
                job-hdr.est-no       = oe-ord.est-no
                job-hdr.i-no         = oe-ordl.i-no
                job-hdr.qty          = oe-ordl.qty 
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no
                job-hdr.blank-no     = oe-ordl.blank-no.

         IF AVAIL itemfg THEN
              ASSIGN job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         ASSIGN job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
      END.

      ELSE
      DO WHILE TRUE:
        FIND v-ord-job-hdr WHERE ROWID(v-ord-job-hdr) EQ ROWID(job-hdr)
            EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL v-ord-job-hdr THEN DO:
          FIND CURRENT v-ord-job-hdr NO-LOCK NO-ERROR.
          FIND CURRENT job-hdr NO-ERROR.
          LEAVE.
        END.
      END.

      ASSIGN job-hdr.est-no  = oe-ord.est-no
             job-hdr.job     = job.job
             job-hdr.job-no  = job.job-no
             job-hdr.job-no2 = job.job-no2
             oe-ordl.est-no  = job-hdr.est-no
             oe-ordl.job-no  = job-hdr.job-no
             oe-ordl.job-no2 = job-hdr.job-no2
            oe-ordl.j-no = job-hdr.j-no.

        FIND CURRENT job-hdr NO-LOCK.
    END.
    IF oe-ord.stat EQ "H" THEN 
      RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT "Hold").
  FIND CURRENT job NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-misc V-table-Win 
PROCEDURE create-misc :
/*------------------------------------------------------------------------------
  Purpose:    from  oe/ordlmisc.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
  DEF VAR li-line AS INT NO-UNDO.
  DEF VAR v-tax-rate AS DEC FORM ">,>>9.99<<<" NO-UNDO.
  DEF VAR v-frt-tax-rate LIKE v-tax-rate NO-UNDO.
  DEF BUFFER bf-eb FOR eb .

  FIND bf-eb WHERE RECID(bf-eb) = ip-recid NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-eb THEN RETURN.


  FIND FIRST cust NO-LOCK
      WHERE cust.company = g_company 
        AND cust.cust-no = oe-ord.cust-no
      NO-ERROR.

  FOR EACH est-prep WHERE est-prep.company = g_company
                      AND est-prep.est-no = bf-eb.est-no
                      AND est-prep.simon = "S"     NO-LOCK .
      FIND FIRST oe-ordm WHERE oe-ordm.company = g_company
                           AND oe-ordm.ord-no = oe-ord.ord-no
                           AND oe-ordm.charge = est-prep.code
                           NO-LOCK NO-ERROR.
      IF NOT AVAIL oe-ordm THEN DO:
         FIND LAST oe-ordm OF oe-ord NO-LOCK NO-ERROR.
         li-line = IF AVAIL oe-ordm THEN oe-ordm.line + 1 ELSE 1.
         FIND cust WHERE cust.company = g_company 
                     AND cust.cust-no = oe-ord.cust-no EXCLUSIVE-LOCK NO-ERROR.
         FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
         FIND FIRST prep WHERE prep.company = g_company 
                           AND prep.code = est-prep.code NO-LOCK NO-ERROR.
         CREATE oe-ordm.
         ASSIGN oe-ordm.company = g_company
                oe-ordm.ord-no = oe-ord.ord-no
                oe-ordm.line = li-line
                oe-ordm.charge = est-prep.code
                oe-ordm.dscr = IF est-prep.dscr <> "" THEN est-prep.dscr ELSE prep.dscr
                oe-ordm.actnum = IF AVAIL prep AND prep.actnum <> "" THEN prep.actnum ELSE ar-ctrl.sales
                oe-ordm.amt =  IF ceprepprice-chr EQ "Profit" THEN
                                  (est-prep.cost * est-prep.qty) / (1 - (est-prep.mkup / 100)) * 
                                  (est-prep.amtz / 100)
                               ELSE
                                  (est-prep.cost * est-prep.qty) * (1 + (est-prep.mkup / 100)) * 
                                  (est-prep.amtz / 100)
                oe-ordm.est-no = est-prep.est-no
                oe-ordm.tax = cust.sort = "Y" AND oe-ord.tax-gr <> ""
                oe-ordm.cost = (est-prep.cost * est-prep.qty * (est-prep.amtz / 100))
                oe-ordm.bill  = "Y".

         IF PrepTax-log THEN 
            ASSIGN oe-ordm.tax = TRUE
                   oe-ordm.spare-char-1 = IF cust.spare-char-1 <> "" THEN cust.spare-char-1 ELSE oe-ord.tax-gr.
                   .  
         RUN ar/cctaxrt.p (INPUT g_company, oe-ord.tax-gr,
                            OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

         IF AVAIL cust THEN DO:
           FIND CURRENT cust EXCLUSIVE-LOCK.
           cust.ord-bal = cust.ord-bal + oe-ordm.amt +
                          (IF oe-ordm.tax THEN (oe-ordm.amt * v-tax-rate / 100) ELSE 0).            
           FIND CURRENT cust NO-LOCK.
         END.
      END.

      FIND CURRENT oe-ordm NO-LOCK.
  END.

  IF AVAIL cust THEN
      FIND CURRENT cust NO-LOCK.

  FOR EACH ef OF bf-eb /*where ef.company = g_company and
                    ef.est-no = oe-ord.est-no */
                    NO-LOCK:
      DO i = 1 TO 5:
         IF ef.mis-simon[i] = "S" THEN DO:
            FIND LAST oe-ordm OF oe-ord NO-LOCK NO-ERROR.
            li-line = IF AVAIL oe-ordm THEN oe-ordm.line + 1 ELSE 1.
            FIND cust WHERE cust.company = g_company 
                        AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
            FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
            FIND FIRST prep WHERE prep.company = g_company 
                           AND prep.code = ef.mis-cost[i] NO-LOCK NO-ERROR.

            CREATE oe-ordm.
            ASSIGN oe-ordm.company = g_company
                   oe-ordm.ord-no = oe-ord.ord-no
                   oe-ordm.line = li-line
                   oe-ordm.charge = ef.mis-cost[i]
                   oe-ordm.bill  = "Y"
                   oe-ordm.tax = cust.sort = "Y" AND oe-ord.tax-gr <> ""
                   oe-ordm.amt = IF ceprepprice-chr EQ "Profit" THEN
                                    (ef.mis-labf[i] + ef.mis-matf[i] +
                                    ((ef.mis-labm[i] + ef.mis-matm[i]) * (lv-qty / 1000))) /
                                    (1 - (ef.mis-mkup[i] / 100))
                                 ELSE
                                    (ef.mis-labf[i] + ef.mis-matf[i] +
                                    ((ef.mis-labm[i] + ef.mis-matm[i]) * (lv-qty / 1000))) *
                                    (1 + (ef.mis-mkup[i] / 100))
                   oe-ordm.est-no = oe-ord.est-no
                   oe-ordm.dscr = IF AVAIL prep THEN prep.dscr ELSE ""
                   oe-ordm.actnum = IF AVAIL prep AND prep.actnum <> "" THEN prep.actnum ELSE ar-ctrl.sales
                   oe-ordm.cost = (ef.mis-labf[i] + ef.mis-matf[i] +
                                  ((ef.mis-labm[i] + ef.mis-matm[i]) * (lv-qty / 1000))).

            RUN ar/cctaxrt.p (INPUT g_company, oe-ord.tax-gr,
                              OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

            IF AVAIL cust THEN DO:
              FIND CURRENT cust EXCLUSIVE-LOCK.
              cust.ord-bal = cust.ord-bal + oe-ordm.amt +
                             (IF oe-ordm.tax THEN (oe-ordm.amt * v-tax-rate / 100) ELSE 0).
              FIND CURRENT cust NO-LOCK.
            END.

            FIND CURRENT oe-ordm NO-LOCK.            
         END.  /* simon = "S" */
      END.  /* do */              
  END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-release V-table-Win 
PROCEDURE create-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-qty-sum AS INT NO-UNDO.
  DEF VAR v-nxt-r-no AS INT INIT 1 NO-UNDO.
  DEF VAR v-lst-rel AS DATE NO-UNDO.
  DEF VAR v-pct-chg AS DEC NO-UNDO.
  DEF VAR v-ship-id LIKE oe-rel.ship-id NO-UNDO.
  DEF VAR v-num-shipto AS INT NO-UNDO.

  DEF BUFFER xoe-rel FOR oe-rel.


  FIND xoe-ord WHERE RECID(xoe-ord) = recid(oe-ord) NO-LOCK.  
  FIND FIRST oe-ordl WHERE oe-ordl.company = oe-ord.company AND
                           oe-ordl.ord-no = oe-ord.ord-no
                           NO-LOCK NO-ERROR.
  ASSIGN v-qty-sum  = 0.
  {oe/oe-rel.a &fil="oe-ordl"}
  ASSIGN v-ship-id = "".
  FIND FIRST xoe-rel WHERE xoe-rel.company EQ cocode
                       AND xoe-rel.ord-no  EQ oe-ordl.ord-no
                       AND recid(xoe-rel)  NE recid(oe-rel)
                       AND xoe-rel.link-no EQ 0
                       NO-LOCK NO-ERROR.                             
  IF NOT AVAIL xoe-rel THEN DO:
     FOR EACH shipto WHERE shipto.company EQ cocode
                       AND shipto.cust-no EQ oe-ordl.cust-no:
               ASSIGN v-num-shipto = v-num-shipto + 1.
     END.

     IF v-num-shipto GT 1 THEN
     DO:
         IF oe-ordl.est-no NE "" THEN
         DO:
            FIND FIRST eb WHERE eb.company = oe-ordl.company AND
                                eb.est-no EQ oe-ordl.est-no
                            AND eb.form-no  NE 0
                               NO-LOCK NO-ERROR.
            IF AVAIL eb THEN ASSIGN v-ship-id = eb.ship-id.
         END.
         ELSE DO:
            FIND FIRST shipto WHERE shipto.company EQ cocode
                                AND shipto.cust-no EQ oe-ordl.cust-no
                                AND shipto.ship-id = oe-ordl.cust-no
                                NO-LOCK NO-ERROR.
            IF AVAIL shipto THEN ASSIGN v-ship-id = shipto.ship-id.
            ELSE DO:
                 FIND FIRST shipto WHERE shipto.company EQ cocode
                                     AND shipto.cust-no EQ oe-ordl.cust-no
                                     NO-LOCK NO-ERROR.
                 IF AVAIL shipto THEN ASSIGN v-ship-id = shipto.ship-id.   
            END.
         END.
         RUN oe/d-shipid.w (INPUT oe-ord.cust-no, INPUT-OUTPUT v-ship-id , INPUT-OUTPUT v-ship-from).
         ASSIGN oe-rel.ship-id = TRIM(v-ship-id).
         FIND FIRST shipto WHERE shipto.company = cocode AND
                                  shipto.cust-no = oe-ord.cust-no  AND
                                  shipto.ship-id = v-ship-id
                                  USE-INDEX ship-id NO-LOCK NO-ERROR.
         IF AVAILABLE shipto THEN DO:
            ASSIGN v-ship-id           = shipto.ship-id
                   oe-rel.ship-no      = shipto.ship-no
                                oe-rel.ship-id      = shipto.ship-id
                                oe-rel.ship-addr[1] = shipto.ship-addr[1]
                                oe-rel.ship-addr[2] = shipto.ship-addr[2]
                                oe-rel.ship-city    = shipto.ship-city
                                oe-rel.ship-state   = shipto.ship-state
                                oe-rel.ship-zip     = shipto.ship-zip
                                oe-rel.ship-i[1] = shipto.notes[1]
                                oe-rel.ship-i[2] = shipto.notes[2]
                                oe-rel.ship-i[3] = shipto.notes[3]
                               oe-rel.ship-i[4] = shipto.notes[4].

         IF v-ship-from GT "" THEN
             oe-rel.spare-char-1 = v-ship-from.

             /* if add mode then use default carrier */
          /*   if sel = 3 /* and NOT oe-rel.carrier ENTERED */ then do: */
            FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                                  AND sys-ctrl.name    EQ "OECARIER"
                             NO-LOCK NO-ERROR.
            IF NOT AVAIL sys-ctrl THEN DO:
                               CREATE sys-ctrl.
                               ASSIGN
                                 sys-ctrl.company  = cocode
                                 sys-ctrl.name     = "OECARIER"
                                 sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
                                 sys-ctrl.char-fld = "ShipTo".

                               DO WHILE TRUE:
                                 MESSAGE "Default Shipping Carrier from Header or Shipto?" 
                                   UPDATE sys-ctrl.char-fld.
                                 IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "ShipTo" THEN LEAVE. 
                               END.
            END.
            oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto.carrier
                               ELSE oe-ord.carrier.
         END.
         /* Run Freight calculation  */
         RUN oe/oe-frtcl.p.
      END.  /* multi ship to */
      ELSE DO:
           FIND FIRST shipto WHERE shipto.company EQ cocode AND
                                        shipto.cust-no EQ oe-ord.cust-no AND
                                        shipto.ship-id EQ v-ship-id
                                  NO-LOCK NO-ERROR.
            IF NOT AVAIL shipto THEN
                 FIND FIRST shipto WHERE shipto.company EQ cocode AND
                                          shipto.cust-no EQ oe-ord.cust-no
                                    NO-LOCK NO-ERROR.
            IF AVAILABLE shipto THEN DO:
               ASSIGN oe-rel.ship-no      = shipto.ship-no
                      oe-rel.ship-id      = shipto.ship-id
                         oe-rel.ship-addr[1] = shipto.ship-addr[1]
                         oe-rel.ship-addr[2] = shipto.ship-addr[2]
                         oe-rel.ship-city    = shipto.ship-city
                         oe-rel.ship-state   = shipto.ship-state
                         oe-rel.ship-zip     = shipto.ship-zip
                         oe-rel.ship-i[1] = shipto.notes[1]
                         oe-rel.ship-i[2] = shipto.notes[2]
                         oe-rel.ship-i[3] = shipto.notes[3]
                         oe-rel.ship-i[4] = shipto.notes[4].
               /* if add mode then use default carrier */
               IF adm-new-record /* and NOT oe-rel.carrier ENTERED */ THEN DO:
                  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                                        AND sys-ctrl.name    EQ "OECARIER"
                       NO-LOCK NO-ERROR.
                  IF NOT AVAIL sys-ctrl THEN DO:
                     CREATE sys-ctrl.
                     ASSIGN sys-ctrl.company  = cocode
                            sys-ctrl.name     = "OECARIER"
                            sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                            sys-ctrl.char-fld = "ShipTo".

                     DO WHILE TRUE:
                        MESSAGE "Default Shipping Carrier from Header or Shipto?" 
                        UPDATE sys-ctrl.char-fld.
                        IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "Sh~ipTo" THEN LEAVE. 
                     END.
                  END.
                  oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto~.carrier
                                     else oe-ord.carrier.
               END.
            END. /* avail shipto */
      END. /* not multi */
  END. /* if no oe-rel */
  ELSE DO:
       FIND FIRST shipto WHERE shipto.company = cocode AND
                               shipto.cust-no = oe-ord.cust-no  AND
                               shipto.ship-id = xoe-rel.ship-id
                               USE-INDEX ship-id NO-LOCK NO-ERROR.
       IF AVAILABLE shipto THEN DO:
           ASSIGN oe-rel.ship-no      = shipto.ship-no
                      oe-rel.ship-id      = shipto.ship-id
                       oe-rel.ship-addr[1] = shipto.ship-addr[1]
                       oe-rel.ship-addr[2] = shipto.ship-addr[2]
                       oe-rel.ship-city    = shipto.ship-city
                       oe-rel.ship-state   = shipto.ship-state
                       oe-rel.ship-zip     = shipto.ship-zip
                       oe-rel.ship-i[1] = shipto.notes[1]
                       oe-rel.ship-i[2] = shipto.notes[2]
                       oe-rel.ship-i[3] = shipto.notes[3]
                       oe-rel.ship-i[4] = shipto.notes[4].
               /* if add mode then use default carrier */
           IF adm-new-record THEN DO:                
                 FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                                       AND sys-ctrl.name    EQ "OECARIER"
                 NO-LOCK NO-ERROR.
                 IF NOT AVAIL sys-ctrl THEN DO:
                    CREATE sys-ctrl.
                    ASSIGN sys-ctrl.company  = cocode
                           sys-ctrl.name     = "OECARIER"
                           sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                           sys-ctrl.char-fld = "ShipTo".

                    DO WHILE TRUE:
                        MESSAGE "Default Shipping Carrier from Header or Shipto?" 
                        UPDATE sys-ctrl.char-fld.
                        IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "Sh~ipTo" THEN LEAVE. 
                    END.
                 END.
                 oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto~.carrier
                                    else oe-ord.carrier.
           END.           
       END.           
  END.

  fil_id = RECID(oe-ordl).
  IF AVAIL oe-ordl AND (oe-ordl.est-no NE "" AND oe-ordl.job-no EQ "") THEN DO:
       MESSAGE " Since job number is blank, a job will not be created "
                  VIEW-AS ALERT-BOX.
  END.  
  ELSE RUN oe/ordlup.p.         /* Update Inventory and Job Costing */

 IF /*oe-ord.est-no eq "" and*/ oe-ordl.est-no NE "" THEN DO:
      fil_id = RECID(oe-ordl).
      RUN oe/estupl.p.
      v-qty-mod = NO.
      fil_id = RECID(oe-ordl).
      IF AVAIL oe-ordl AND (oe-ordl.est-no NE "" AND oe-ordl.job-no EQ "") THEN DO:
         MESSAGE " Since job number is blank, a purchase order will not be create~d "
                 VIEW-AS ALERT-BOX .
      END.  
      ELSE DO:
         RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.
      END.   
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createFromEst V-table-Win 
PROCEDURE createFromEst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       This runs from the save button on the panel thus allowing
               local-update-record to complete first. If local-update-record
               does not complete, all of the locked records are held
------------------------------------------------------------------------------*/
DEF VAR char-hdl AS CHAR NO-UNDO.

IF llCreateFromEst THEN DO:
  RUN order-from-est (YES). 
/*     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl). */
/*     RUN notify IN WIDGET-HANDLE(char-hdl) ('row-available').                               */

  /* This is here because adding 2 orders one after another from an estimate */
  /* was causing errors in b-ordinq                                          */
  /* The other option would be to reopen query or intialize                  */
      IF AVAIL(oe-ord) THEN DO:
          FIND FIRST oe-ordl WHERE oe-ordl.company EQ oe-ord.company
              AND oe-ordl.ord-no EQ oe-ord.ord-no
              NO-LOCK NO-ERROR.
          IF AVAIL oe-ordl THEN DO:          
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
            RUN one-row-query IN WIDGET-HANDLE(char-hdl) (ROWID(oe-ordl)).
          END.
      END.

END.
llCreateFromEst = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-itemfg V-table-Win 
PROCEDURE crt-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* -------------------------------------------------- fg/ce-addfg.p 08/98 JLF */
/* Add FG thru estimating                                                     */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER v-item LIKE itemfg.i-no.

DEF VAR v-alloc LIKE itemfg.alloc INIT YES.
DEF VAR tmpstore AS cha NO-UNDO.
DEF VAR i AS INT NO-UNDO.

 {ce/msfcalc.i}
 {oe/fgfreight.i} 

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SETPRINT"
    NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN v-alloc = sys-ctrl.log-fld.

FIND FIRST cust  WHERE cust.company EQ cocode
                   AND cust.cust-no EQ xeb.cust-no
    NO-LOCK NO-ERROR.

CREATE itemfg.
ASSIGN
 itemfg.company    = cocode
 itemfg.loc        = locode
 itemfg.i-no       = v-item
 itemfg.i-code     = "C"
 itemfg.i-name     = oe-ordl.i-name
 itemfg.part-dscr1 = oe-ordl.part-dscr1
 itemfg.part-dscr2 = oe-ordl.part-dscr2
 itemfg.sell-price = oe-ordl.price
 itemfg.sell-uom   = oe-ordl.pr-uom
 itemfg.part-no    = oe-ordl.part-no
 itemfg.cust-no    = oe-ord.cust-no
 itemfg.cust-name  = oe-ord.cust-name
 itemfg.pur-uom    = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.prod-uom   = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.alloc      = v-alloc
 itemfg.stocked    = YES.
 /* Create an itemfg-loc for the default warehouse */
 RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").

IF v-graphic-char NE "" THEN 
DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
END.

 IF AVAIL xeb THEN DO:
    ASSIGN itemfg.die-no     = xeb.die-no
           itemfg.plate-no   = xeb.plate-no
           itemfg.style      = xeb.style
           itemfg.procat     = xeb.procat
           itemfg.cad-no     = xeb.cad-no
           itemfg.upc-no     = xeb.upc-no
           itemfg.spc-no     = xeb.spc-no
           itemfg.isaset     = xeb.form-no EQ 0
           itemfg.pur-man    = xeb.pur-man
           itemfg.alloc      = xeb.set-is-assembled.

    IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.
    {oe/fgfreighta.i xeb}  
    {fg/set-inks1.i itemfg xeb}
    {sys/inc/fgcascnt.i itemfg xeb}
    {sys/inc/updfgdim.i "xeb"} 

 END.
 ELSE DO:
    IF itemfg.def-loc = "" AND itemfg.def-loc-bin = "" THEN DO:
       FIND FIRST cust WHERE cust.company = cocode AND
                          cust.active  = "X"    NO-LOCK NO-ERROR.
       IF AVAIL cust THEN DO:
          FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN DO:
             ASSIGN itemfg.def-loc        = shipto.loc        
                    itemfg.def-loc-bin    = shipto.loc-bin.
          END.
       END.
    END.
 END.  


FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
itemfg.i-code = IF oe-ordl.est-no NE "" THEN "C"
                ELSE IF AVAIL oe-ctrl THEN
                        IF oe-ctrl.i-code THEN "S"
                        ELSE "C"
                ELSE "S".
/* ==== not yet 
if itemfg.i-code eq "S" then do:
  fil_id = recid(itemfg).
  run oe/fg-item.p.
  fil_id = recid(oe-ordl).
  {oe/ordlfg.i}
  display oe-ordl.i-name oe-ordl.i-no oe-ordl.price
          oe-ordl.pr-uom oe-ordl.cas-cnt oe-ordl.part-dscr2 oe-ordl.cost
          oe-ordl.part-no oe-ordl.part-dscr1 with frame oe-ordlf.
end.
*/

FIND CURRENT itemfg NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE del-detail-recs V-table-Win 
PROCEDURE del-detail-recs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER xfg-set FOR fg-set.
DEF BUFFER xitemfg FOR itemfg.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER del-job-hdr FOR job-hdr.

DEFINE VARIABLE v-totord          LIKE oe-ord.t-revenue NO-UNDO.
DEFINE VARIABLE v-tot-tax         LIKE oe-ord.tax       NO-UNDO.
DEFINE VARIABLE v-tot-freight     LIKE oe-ord.t-freight NO-UNDO.
DEFINE VARIABLE v-qty-lft         LIKE oe-ordl.qty      NO-UNDO.
DEFINE VARIABLE v-new-ord         AS LOGICAL          INITIAL NO NO-UNDO.
DEFINE VARIABLE v-ext-price       LIKE oe-ordl.t-price  NO-UNDO.
DEFINE VARIABLE v-tax-rate        AS DECIMAL          FORMAT "->>>.99" NO-UNDO.
DEFINE VARIABLE v-frt-tax-rate    LIKE v-tax-rate       NO-UNDO.
DEFINE VARIABLE v-period          AS INTEGER          NO-UNDO.
DEFINE VARIABLE tmp-ordm-amt      LIKE oe-ordm.amt      NO-UNDO.
DEFINE VARIABLE tmp-tax           LIKE oe-ord.tax       NO-UNDO.
DEFINE VARIABLE v-continue        AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-blank-fg-on-est AS INTEGER          NO-UNDO.
DEFINE VARIABLE char-hdl          AS cha              NO-UNDO.
DEFINE VARIABLE loop-limit        AS INTEGER          NO-UNDO.


DEF VAR orig-ord LIKE oe-ord.ord-no.
DEF VAR v-deleted AS LOG NO-UNDO.
DEF VAR ll-ans AS LOG NO-UNDO.
DEF BUFFER bf-oe-ord FOR oe-ord.

  orig-ord = oe-ord.ord-no.
  v-deleted = oe-ord.deleted.
  /* Needed to set q-alloc without a recalculation in trigger */
  FIND bf-oe-ord WHERE ROWID(bf-oe-ord) = ROWID(oe-ord) EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL bf-oe-ord THEN DO:
      bf-oe-ord.deleted = YES.
      RELEASE bf-oe-ord.
  END.

  FIND FIRST period
    WHERE period.company EQ oe-ord.company
      AND period.pst     LE oe-ord.ord-date
      AND period.pend    GE oe-ord.ord-date
    NO-LOCK NO-ERROR.
  v-period = IF AVAIL period AND period.pstat THEN period.pnum ELSE 1.

  IF (oe-ord.est-no NE "" OR INT(oe-ord.est-no) NE 0) THEN DO:
    FIND FIRST est
    WHERE est.company EQ oe-ord.company
    AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(oe-ord.est-no))) +
    TRIM(oe-ord.est-no)
    NO-LOCK NO-ERROR.
    IF AVAIL est THEN DO:

      FIND CURRENT est EXCLUSIVE.
      FIND LAST xoe-ord 
        WHERE xoe-ord.company = oe-ord.company AND
          xoe-ord.est-no = oe-ord.est-no AND
          RECID(xoe-ord) NE RECID(oe-ord)
        NO-LOCK NO-ERROR.
      IF AVAIL xoe-ord THEN
        ASSIGN
        est.ord-date = xoe-ord.ord-date
        est.ord-no   = xoe-ord.ord-no.
      ELSE DO:
        IF est.ord-date EQ oe-ord.ord-date THEN est.ord-date = ?.
        IF est.ord-no EQ oe-ord.ord-no THEN est.ord-no = 0.
      END.

      FIND CURRENT est NO-LOCK.
    END.
  END.

  FOR EACH oe-ordl OF oe-ord:
    RUN oe/oe-ordd.p(RECID(oe-ordl),OUTPUT v-continue).
    IF NOT v-continue THEN RETURN ERROR /* NO-APPLY */.

    FOR EACH oe-rel
      WHERE oe-rel.company = oe-ordl.company
      AND oe-rel.ord-no = oe-ordl.ord-no
      AND oe-rel.i-no = oe-ordl.i-no
      AND oe-rel.LINE = oe-ordl.LINE:
      DELETE oe-rel.
    END. /* each oe-rel */

    FIND FIRST itemfg
      WHERE itemfg.company EQ oe-ordl.company
        AND itemfg.i-no    EQ oe-ordl.i-no
      NO-ERROR.
    IF AVAIL itemfg THEN DO:

      IF AVAIL(itemfg) AND AVAIL(oe-ord) THEN DO:
        RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT oe-ord.loc).
        FIND FIRST itemfg-loc
          WHERE itemfg-loc.company EQ itemfg.company
            AND itemfg-loc.i-no    EQ itemfg.i-no
            AND itemfg-loc.loc     EQ oe-ord.loc
          EXCLUSIVE-LOCK NO-ERROR.
      END.

      IF oe-ord.TYPE NE "T" THEN DO:
        itemfg.q-alloc = itemfg.q-alloc - oe-ordl.qty.
        IF AVAIL(itemfg-loc) THEN
        itemfg-loc.q-alloc = itemfg-loc.q-alloc - oe-ordl.qty.
      END.

      IF itemfg.q-alloc LT 0 THEN DO:
        itemfg.q-alloc = 0.
        IF AVAIL(itemfg-loc) THEN
        itemfg-loc.q-alloc = 0.
      END.

      ASSIGN
        itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
        itemfg.q-ptd     = itemfg.q-ptd - oe-ordl.qty
        itemfg.q-ord-ytd = itemfg.q-ord-ytd - oe-ordl.qty.
      IF AVAIL(itemfg-loc) THEN
        ASSIGN
          itemfg-loc.q-avail   = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
          itemfg-loc.q-ptd     = itemfg-loc.q-ptd - oe-ordl.qty
          itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd - oe-ordl.qty.

      IF oe-ord.TYPE NE "T"                                                   AND
      NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) THEN
      RUN fg/comp-upd.p (RECID(itemfg), oe-ordl.qty * -1, "q-alloc", 0).
    END. /* avail itemfg */

    RUN ar/cctaxrt.p (INPUT oe-ord.company, INPUT oe-ord.tax-gr,
    OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

    ASSIGN
    v-qty-lft   = oe-ordl.qty - oe-ordl.inv-qty
    v-ext-price = 0.

    IF v-qty-lft GT 0 THEN DO:
      IF oe-ordl.pr-uom BEGINS "L" AND oe-ordl.pr-uom NE "LB" THEN
        v-ext-price = oe-ordl.price - ROUND( (oe-ordl.price * oe-ordl.disc) / 100, 2).
      ELSE
      IF oe-ordl.pr-uom = "CS" THEN DO:
        FIND FIRST itemfg {sys/look/itemfgrlW.i}
        AND itemfg.i-no = oe-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL itemfg AND itemfg.case-count NE 0 THEN
        v-ext-price = ((v-qty-lft / itemfg.case-count) * oe-ordl.price) -
        ROUND((((v-qty-lft / itemfg.case-count) *
        oe-ordl.price) * oe-ordl.disc) / 100, 2).
        ELSE
        v-ext-price = (v-qty-lft * oe-ordl.price) -
        ROUND(((v-qty-lft * oe-ordl.price) * oe-ordl.disc) / 100, 2).
      END.
      ELSE
      IF oe-ordl.pr-uom = "C" THEN
        v-ext-price = ((v-qty-lft / 100) *
        oe-ordl.price) - ROUND((((v-qty-lft / 100) *
        oe-ordl.price) * oe-ordl.disc) / 100, 2).
      ELSE
      IF oe-ordl.pr-uom = "M" THEN
        v-ext-price = ((v-qty-lft / 1000) *
        oe-ordl.price) - ROUND(( ((v-qty-lft / 1000) *
        oe-ordl.price) * oe-ordl.disc) / 100, 2).
      ELSE /** default per thousand **/
        v-ext-price = ((v-qty-lft) *
        oe-ordl.price) - ROUND(( ((v-qty-lft) *
        oe-ordl.price) * oe-ordl.disc) / 100, 2).

      /** calculate freight charges **/
      ASSIGN v-tot-freight = v-tot-freight +
      (ROUND(oe-ordl.t-freight / oe-ordl.qty, 2) *
      v-qty-lft).
      /** calculate tax charges **/
      IF oe-ordl.tax AND v-tax-rate GT 0 THEN ASSIGN
      v-tot-tax = v-tot-tax + ROUND((v-ext-price * v-tax-rate) / 100,2).
    END. /* inv-qty ne 0 */

    IF oe-ordl.tax THEN
      ASSIGN v-totord = (v-totord + v-ext-price +
      ROUND((v-ext-price * v-tax-rate) / 100,2)).
    ELSE
      ASSIGN v-totord = v-totord + v-ext-price.

    FOR EACH oe-ordm
      WHERE oe-ordm.company EQ oe-ordl.company
      AND oe-ordm.ord-no  EQ oe-ordl.ord-no
      AND oe-ordm.est-no  EQ oe-ordl.est-no:
      IF oe-ordm.bill EQ "Y" THEN DO:
        ASSIGN  v-totord = v-totord + oe-ordm.amt.
        IF oe-ordm.tax AND v-tax-rate GT 0 THEN
        ASSIGN v-tot-tax = (v-tot-tax +
        ROUND((oe-ordm.amt * v-tax-rate) / 100,2))
        v-totord = (v-totord +
        ROUND((oe-ordm.amt * v-tax-rate) / 100,2)).
      END.

      IF oe-ord.stat = "N" OR oe-ord.stat = "A" OR oe-ord.stat = "H" THEN
      DELETE oe-ordm.
    END. /* each oe-ordm */

    IF oe-ordl.job-no NE "" THEN DO:
      FIND FIRST job
        WHERE job.company EQ oe-ordl.company
          AND job.job-no  EQ oe-ordl.job-no
          AND job.job-no2 EQ oe-ordl.job-no2
        USE-INDEX job-no NO-LOCK NO-ERROR.

      FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company  EQ oe-ordl.company
        AND job-hdr.est-no   EQ oe-ordl.est-no
        AND job-hdr.job-no   EQ oe-ordl.job-no
        AND job-hdr.job-no2  EQ oe-ordl.job-no2
        AND ((job-hdr.ord-no EQ oe-ordl.ord-no AND
        job-hdr.i-no   EQ oe-ordl.i-no) OR
        job-hdr.ord-no  EQ 0)
        USE-INDEX enum:

        IF job-hdr.ord-no NE 0 THEN DO:          
          IF AVAIL job THEN DO:
            {util/dljobkey.i}
          END.
        END.

        DO loop-limit = 1 TO 1000:
          FIND del-job-hdr WHERE ROWID(del-job-hdr) EQ ROWID(job-hdr)
          EXCLUSIVE NO-WAIT NO-ERROR.
          IF AVAIL del-job-hdr THEN DO:
            DELETE del-job-hdr.
            LEAVE.
          END.
        END.
      END. /* Each Job-hdr */

      FIND FIRST job
        WHERE job.company EQ oe-ordl.company
          AND job.job-no  EQ oe-ordl.job-no
          AND job.job-no2 EQ oe-ordl.job-no2
        USE-INDEX job-no EXCLUSIVE NO-ERROR.

      IF AVAIL job                                          AND
      NOT CAN-FIND(FIRST job-hdr
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2) THEN DO:

        RUN jc/jc-dall.p (RECID(job)).

        FOR EACH job-mat
          WHERE job-mat.company  EQ job.company
          AND job-mat.job      EQ job.job
          AND job-mat.job-no   EQ job.job-no
          AND job-mat.job-no2  EQ job.job-no2
          USE-INDEX seq-idx:
          DELETE job-mat.
        END.

        FOR EACH mat-act
          WHERE mat-act.company  EQ job.company
          AND mat-act.job      EQ job.job
          AND mat-act.job-no   EQ job.job-no
          AND mat-act.job-no2  EQ job.job-no2
          USE-INDEX job:
          DELETE mat-act.
        END.

        FOR EACH job-mch
          WHERE job-mch.company  EQ job.company
          AND job-mch.job      EQ job.job
          AND job-mch.job-no   EQ job.job-no
          AND job-mch.job-no2  EQ job.job-no2
          USE-INDEX seq-idx:
          DELETE job-mch.
        END.

        FOR EACH mch-act
          WHERE mch-act.company  EQ job.company
          AND mch-act.job      EQ job.job
          AND mch-act.job-no   EQ job.job-no
          AND mch-act.job-no2  EQ job.job-no2
          USE-INDEX job:
          DELETE mch-act.
        END.

        FOR EACH job-prep
          WHERE job-prep.company  EQ job.company
          AND job-prep.job      EQ job.job
          AND job-prep.job-no   EQ job.job-no
          AND job-prep.job-no2  EQ job.job-no2:
          DELETE job-prep.
        END.

        FOR EACH job-farm
            WHERE job-farm.company EQ job.company
              AND job-farm.job-no  EQ job.job-no
              AND job-farm.job-no2 EQ job.job-no2
            EXCLUSIVE:
          DELETE job-farm.
        END.

        FOR EACH job-farm-rctd
            WHERE job-farm-rctd.company EQ job.company
              AND job-farm-rctd.job-no  EQ job.job-no
              AND job-farm-rctd.job-no2 EQ job.job-no2
            EXCLUSIVE:
          DELETE job-farm-rctd.
        END.

        FOR EACH misc-act
          WHERE misc-act.company  EQ job.company
          AND misc-act.job      EQ job.job
          AND misc-act.job-no   EQ job.job-no
          AND misc-act.job-no2  EQ job.job-no2
          USE-INDEX misc-idx:
          DELETE misc-act.
        END.

        IF job.exported THEN DO:
          job.stat = "X".
          RUN jc/kiwiexp2.p (RECID(job)).
        END.

        DELETE job.
      END. /* Avail Job */
    END. /* Job-no ne "" */

    FOR EACH tt-del-eb:
      FIND eb WHERE ROWID(eb) = tt-del-eb.del-rowid EXCLUSIVE-LOCK.
      eb.stock-no = "".
    END.

    IF oe-ordl.t-inv-qty = 0 THEN ASSIGN v-new-ord = YES.
    /*if index("NAH",oe-ord.stat) gt 0 THEN delete oe-ordl.*/
  END. /* each oe-ordl */

  /*task 07221004*/
  IF INDEX("NAH",oe-ord.stat) GT 0 THEN
  FOR EACH oe-ordl OF oe-ord:
    DELETE oe-ordl.
  END.

  FIND CURRENT itemfg NO-LOCK NO-ERROR.

  RUN ar/cctaxrt.p (INPUT oe-ord.company, INPUT oe-ord.tax-gr,
  OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

  FOR EACH oe-ordm
    WHERE oe-ordm.company EQ oe-ord.company
    AND oe-ordm.ord-no  EQ oe-ord.ord-no:
    IF oe-ordm.bill EQ "Y" THEN DO:
      v-totord = v-totord + oe-ordm.amt.
      IF oe-ordm.tax AND v-tax-rate GT 0 THEN
      ASSIGN
      v-tot-tax = v-tot-tax +
      ROUND((oe-ordm.amt * v-tax-rate) / 100,2)
      v-totord  = v-totord +
      ROUND((oe-ordm.amt * v-tax-rate) / 100,2).
    END.

    IF INDEX("NAH",oe-ord.stat) GT 0 THEN DELETE oe-ordm.
  END. /* each oe-ordm */

  FIND CURRENT oe-ordm NO-LOCK NO-ERROR.

  IF oe-ord.f-bill THEN DO:
    IF v-fr-tax THEN
    v-totord = (v-totord + oe-ord.t-freight +
    ROUND((oe-ord.t-freight * v-frt-tax-rate) / 100,2)).
    ELSE
    v-totord = v-totord + oe-ord.t-freight.
  END.

  IF v-fr-tax THEN
  v-tot-tax = v-tot-tax + ROUND((v-tot-freight * v-frt-tax-rate) / 100,2).

  FIND FIRST cust
    WHERE cust.company EQ oe-ord.company
    AND cust.cust-no EQ oe-ord.cust-no
  EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL cust THEN DO:
    cust.ord-bal  = cust.ord-bal - v-totord.
    IF cust.ord-bal LT 0 THEN ASSIGN cust.ord-bal = 0.
    IF v-new-ord THEN
    ASSIGN
    cust.n-sales[13]      = cust.n-sales[13]  - 1
    cust.n-sales[v-period] = cust.n-sales[v-period] - 1.
    IF cust.n-sales[13]  LT 0 THEN ASSIGN cust.n-sales[13] = 0.
    IF cust.n-sales[v-period] LT 0 THEN ASSIGN cust.n-sales[v-period] = 0.
    FIND CURRENT cust NO-LOCK.
  END.


  FOR EACH work-ordl:
    DELETE work-ordl.
  END.

  FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company EQ oe-ord.company
    AND job-hdr.ord-no  EQ oe-ord.ord-no
    AND job-hdr.est-no  EQ oe-ord.est-no:

    DO loop-limit = 1 TO 1000:
      FIND del-job-hdr WHERE ROWID(del-job-hdr) EQ ROWID(job-hdr)
      EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL del-job-hdr THEN DO:
        DELETE del-job-hdr.
        LEAVE.
      END.
    END.
  END.
  RELEASE job.
  RELEASE job-hdr.

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

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE fi_type fi_prev_order.
  END.

  IF v-oecomm-cha NE "Manual" THEN RUN hide-comm (YES).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-cust-detail V-table-Win 
PROCEDURE display-cust-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.

  DEF VAR v-lead-days LIKE oe-ord.lead-days NO-UNDO.
  DEF VAR v-last-date LIKE oe-ord.last-date NO-UNDO.
  DEF VAR v-due-date  LIKE oe-ord.due-date  NO-UNDO.
  DEF VAR v-ord-date  LIKE oe-ord.ord-date  NO-UNDO.


  DO :
    {sys/inc/lastship.i} 
  END.


  FIND cust WHERE RECID(cust) = ip-recid NO-LOCK NO-ERROR.
  IF AVAIL cust THEN DO WITH FRAME {&frame-name} :
     ASSIGN oe-ord.cust-no:screen-value   = cust.cust-no
            oe-ord.cust-name:screen-value = cust.name
            oe-ord.addr[1]:screen-value   = cust.addr[1]
            oe-ord.addr[2]:screen-value   = cust.addr[2]
            oe-ord.city:screen-value      = cust.city
            oe-ord.state:screen-value     = cust.state
            oe-ord.zip:screen-value       = cust.zip
            oe-ord.contact:screen-value   = cust.contact
           /* oe-ord.lead-days:screen-value = cust.ship-days */
            li-lead-days = cust.ship-days
            oe-ord.last-date:screen-value = STRING(DATE(oe-ord.ord-date:Screen-value) + 
                                                   cust.ship-days)
            oe-ord.due-date:screen-value  = oe-ord.last-date:screen-value
            oe-ord.terms:screen-value     = cust.terms
            oe-ord.over-pct:screen-value  = STRING(cust.over-pct)
            oe-ord.under-pct:screen-value = STRING(cust.under-pct)
            oe-ord.fob-code:screen-value  = cust.fob-code
            oe-ord.frt-pay:screen-value   = IF oe-ord.frt-pay EQ "" THEN
                                            cust.frt-pay ELSE oe-ord.frt-pay
            oe-ord.tax-gr:screen-value    = cust.tax-gr
            /*oe-ord.f-bill:screen-value    = oe-ord.frt-pay:screen-value eq "B" */
            ll-f-bill = oe-ord.frt-pay = "B"
            oe-ord.sman[1]:screen-value   = cust.sman
            oe-ord.s-pct[1]:screen-value = "100.00"
            v-custype         = cust.type
            v-ord-limit       = cust.ord-lim
            v-crd-limit       = cust.cr-lim - (cust.acc-bal + cust.ord-bal).

       RUN get-po-def .

      IF v-shiptorep-log THEN DO:  /* task 05301401 */
          FIND FIRST shipto WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ cust.cust-no
                NO-LOCK NO-ERROR.
          IF AVAIL shipto AND shipto.spare-char-1 <> "" THEN
          oe-ord.sman[1]:screen-value   = shipto.spare-char-1 .

      END.


      IF  lastship-cha = "Stock/Custom" THEN DO:
          /* If order has no estimate. */
          IF oe-ord.est-no:SCREEN-VALUE = "" THEN
              ASSIGN oe-ord.due-date:SCREEN-VALUE = STRING(DATE(oe-ord.ord-date:Screen-value) + lastship-int).
          ELSE
              ASSIGN oe-ord.due-date:SCREEN-VALUE = STRING(DATE(oe-ord.ord-date:Screen-value) + INT(lastship-dec)).
      END.

    ASSIGN
     lv-old-cust-no    = oe-ord.cust-no:SCREEN-VALUE
     ll-cust-displayed = YES.

    ASSIGN
     v-lead-days = li-lead-days
     v-last-date = DATE(oe-ord.last-date:SCREEN-VALUE)
     v-due-date  = DATE(oe-ord.due-date:SCREEN-VALUE)
     v-ord-date  = DATE(oe-ord.ord-date:SCREEN-VALUE).

    {oe/lastship.i "v-" 1}

    ASSIGN
     li-lead-days                  = v-lead-days
     oe-ord.last-date:SCREEN-VALUE = STRING(v-last-date)
     oe-ord.due-date:SCREEN-VALUE  = STRING(v-due-date).

     FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "SALESREP" NO-ERROR.
     IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN DO:
       FIND FIRST sys-ctrl-shipto NO-LOCK
          WHERE sys-ctrl-shipto.company EQ cocode
          AND sys-ctrl-shipto.name    EQ "SALESREP" 
          AND sys-ctrl-shipto.cust-vend-no = oe-ord.cust-no:SCREEN-VALUE
        NO-ERROR.
       IF AVAIL sys-ctrl-shipto AND oe-ord.sman[1]:SCREEN-VALUE NE sys-ctrl-shipto.char-fld THEN DO:
         ASSIGN oe-ord.sman[2]:SCREEN-VALUE = sys-ctrl-shipto.char-fld
                oe-ord.s-comm[2]:SCREEN-VALUE = STRING(sys-ctrl-shipto.dec-fld)
                oe-ord.s-pct[2]:screen-value = "100.00".
         FIND sman WHERE sman.company = oe-ord.company
            AND sman.sman = oe-ord.sman[2]:SCREEN-VALUE
            NO-LOCK NO-ERROR.
         IF AVAIL sman THEN ASSIGN oe-ord.sname[2]:screen-value = sman.sname.
       END.

     END.

    FIND sman WHERE sman.company = oe-ord.company
                AND sman.sman = oe-ord.sman[1]:SCREEN-VALUE  /*cust.sman*/ /* task 05301401 */
                NO-LOCK NO-ERROR.
    IF AVAIL sman THEN ASSIGN oe-ord.sname[1]:screen-value = sman.sname
                              oe-ord.s-comm[1]:screen-value = STRING(sman.scomm)
                              v-margin = 0.

    IF oe-ord.carrier EQ "" THEN oe-ord.carrier:screen-value = cust.carrier.

    FIND FIRST terms WHERE terms.company EQ cocode
                        AND terms.t-code  EQ cust.terms
               NO-LOCK NO-ERROR.
    IF AVAIL terms THEN  oe-ord.terms-d:screen-value = terms.dscr.
    ELSE oe-ord.terms-d:screen-value = "".

    FIND FIRST soldto WHERE soldto.company EQ cocode
                        AND soldto.cust-no EQ cust.cust-no
                        AND soldto.sold-id BEGINS trim(cust.cust-no)
        USE-INDEX sold-id NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN
      ASSIGN /*oe-ord.sold-no:screen-value      = soldto.sold-no*/             
             li-sold-no = soldto.sold-no
             oe-ord.sold-id:screen-value      = soldto.sold-id
             oe-ord.sold-name:screen-value    = soldto.sold-name
             oe-ord.sold-addr[1]:screen-value = soldto.sold-addr[1]
             oe-ord.sold-addr[2]:screen-value = soldto.sold-addr[2]
             oe-ord.sold-city:screen-value    = soldto.sold-city
             oe-ord.sold-state:screen-value   = soldto.sold-state
             oe-ord.sold-zip:screen-value     = soldto.sold-zip.

    FIND FIRST shipto WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ cust.cust-no
        NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN
       ASSIGN /*oe-ord.ship-i[1]:screen-value = shipto.notes[1]
              oe-ord.ship-i[2]:screen-value = shipto.notes[2]
              oe-ord.ship-i[3]:screen-value = shipto.notes[3]
              oe-ord.ship-i[4]:screen-value = shipto.notes[4]*/
              ls-ship-i[1] = shipto.notes[1]
              ls-ship-i[2] = shipto.notes[2]
              ls-ship-i[3] = shipto.notes[3]
              ls-ship-i[4] = shipto.notes[4].

    IF cust.active EQ "X" THEN fi_type:screen-value = "T".

    IF INDEX("HA",oe-ord.stat:screen-value) = 0 THEN DO:
       /*if (cust.cr-lim - (cust.acc-bal + cust.ord-bal) < 0) or cust.cr-hold
       then do:
            message "Customer has exceeded their credit limit." skip
                    "The order will placed on HOLD." 
                    view-as alert-box warning.
            oe-ord.stat:screen-value = "H".
       end.*/

      RUN oe/creditck.p (ROWID(cust), NO).
      FIND CURRENT cust NO-LOCK NO-ERROR.
      IF AVAIL cust AND cust.cr-hold THEN oe-ord.stat:SCREEN-VALUE = "H".      
    END.   
  END.

  IF AVAIL cust THEN
      FIND CURRENT cust NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE est-from-tandem V-table-Win 
PROCEDURE est-from-tandem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-new-tandem AS LOG NO-UNDO.

  DEF BUFFER est FOR est.
  DEF BUFFER eb FOR eb.

  /*
  RUN ce/new-est.p (4, OUTPUT lv-rowid). ce-ctrl locking moved to button save in est/d-select.w

  FIND eb WHERE ROWID(eb) EQ lv-rowid NO-LOCK NO-ERROR.

  IF AVAIL eb THEN */
  DO WITH FRAME {&FRAME-NAME}:
    RUN est/d-selest.w (?, NO, oe-ord.cust-no:SCREEN-VALUE,
                        OUTPUT ll-new-tandem, OUTPUT lv-rowid).

    FIND eb WHERE ROWID(eb) EQ lv-rowid NO-LOCK NO-ERROR.

    IF ll-new-tandem THEN DO:
      oe-ord.est-no:SCREEN-VALUE = eb.est-no.
      APPLY "value-changed" TO oe-ord.est-no.

      FIND FIRST xest OF eb NO-LOCK NO-ERROR.

      FOR EACH eb OF xest EXCLUSIVE:
        eb.cust-no = oe-ord.cust-no:SCREEN-VALUE.

        FOR EACH shipto
            WHERE shipto.company EQ eb.company
              AND shipto.cust-no EQ eb.cust-no
            NO-LOCK
            BREAK BY shipto.ship-no DESC:
          IF shipto.ship-id EQ eb.ship-id THEN LEAVE.
          IF LAST(shipto.ship-no) THEN eb.ship-id = shipto.ship-id.
        END.
      END.

      RELEASE xeb.

      RUN est/oeselest.p.

      SESSION:SET-WAIT-STATE ("general").
      RUN get-from-est.
      SESSION:SET-WAIT-STATE ("").
    END.

    ELSE DO:
      FIND FIRST est OF eb EXCLUSIVE NO-ERROR.
      IF AVAIL est THEN DELETE est.
    END.
  END.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-current-order V-table-Win 
PROCEDURE Get-current-order :
/*------------------------------------------------------------------------------
  Purpose:     Return the order number to the calling program.
  Parameters:  Company, Order Number
  Notes:       Called from order hold status button.
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER piCompany LIKE oe-ord.company NO-UNDO.
  DEFINE OUTPUT PARAMETER piOrdNo   LIKE oe-ord.ord-no NO-UNDO.

  IF AVAILABLE oe-ord THEN
      ASSIGN piCompany = oe-ord.company
             piOrdNo  = oe-ord.ord-no.

  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-from-est V-table-Win 
PROCEDURE get-from-est :
/*------------------------------------------------------------------------------
  Purpose:     get all info from estimate    
              /* same purpose as oe/orduest.p & oe/estup.p */
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&methods/lValidateError.i YES}
DEF VAR v-est-no LIKE est.est-no NO-UNDO.
DEF VAR v-est-type LIKE est.est-type NO-UNDO.
DEF VAR v-factor AS DEC NO-UNDO.
DEF VAR v-run-list AS CHAR INIT
                "oe/calc-one.p,oe/calc-box.p,ce/tan/print4.p,ce/com/print4.p".
DEF VAR i AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR x AS INT NO-UNDO.
DEF VAR nufile AS LOG NO-UNDO.
DEF VAR v-blk-qty AS INT NO-UNDO.
DEF VAR v-tax-rate AS DEC FORM "->>>.99" NO-UNDO.
DEF VAR v-frt-tax-rate AS DEC FORM "->>>,99" NO-UNDO.
DEF VAR v-quo-price LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR li-line-no AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEF VAR hld-id AS RECID NO-UNDO.
DEF VAR hld-stat LIKE job.stat NO-UNDO.
DEF VAR hld-nufile AS LOG NO-UNDO.
DEF VAR lv-pr-uom AS cha NO-UNDO.
DEF VAR lv-date AS DATE NO-UNDO.
DEF VAR ll-do-job AS LOG NO-UNDO.
DEFINE VARIABLE v-prod-cat AS CHARACTER  NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR lv-qty AS INT NO-UNDO.
DEF VAR ld-marg% AS DEC NO-UNDO.
DEF VAR v-com AS DEC NO-UNDO.
DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cEstNo AS CHARACTER   NO-UNDO.

DO:
v-est-no = oe-ord.est-no:screen-value IN FRAME {&frame-name}.
RUN util/rjust.p (INPUT-OUTPUT v-est-no,8).
oe-ord.est-no:screen-value IN FRAME {&frame-name} = v-est-no.

FIND FIRST xest
    WHERE xest.company EQ cocode
      AND xest.est-no  EQ v-est-no 
    NO-LOCK NO-ERROR.

ASSIGN
 v-est-type = xest.est-type - IF xest.est-type GT 4 THEN 4 ELSE 0
 ll-do-job  = job#-int EQ 0 OR
              CAN-FIND(FIRST eb WHERE eb.company EQ xest.company
                                  AND eb.est-no  EQ xest.est-no
                                  AND eb.pur-man EQ NO
                                  AND eb.form-no NE 0).

IF AVAIL xest THEN DO:
  /** CHECK for INACTIVE CUSTOMERS **/
  FOR EACH eb WHERE eb.company = cocode AND
                    eb.est-no   EQ xest.est-no
                AND eb.form-no NE 0
                AND TRIM(eb.cust-no) NE ""
           NO-LOCK BREAK BY eb.est-no BY eb.cust-no:

    IF FIRST-OF(eb.cust-no) THEN DO:
      /** finding the customer is done this way because the index is not
      setup efficently to find the customer regardles of active stat **/
      FIND FIRST cust {sys/ref/custW.i}
                      AND cust.cust-no EQ eb.cust-no
           USE-INDEX cust NO-LOCK NO-ERROR.
      IF NOT AVAIL cust OR cust.active EQ "I" THEN DO:
         MESSAGE              "   Inactive Customer:" cust.name SKIP
            "   Orders May not Be Processed for Inactive Customers.  "
            VIEW-AS ALERT-BOX WARNING.
         ASSIGN v-inactive = YES.
         RETURN.
      END. /* do */
    END. /* first-of(eb.cust-no) */
    IF v-est-fg1 = "HOLD" AND eb.stock-no = "" THEN DO:
       MESSAGE "Sorry, FG item does not exist. Order has not been approved."
           VIEW-AS ALERT-BOX ERROR.
       APPLY "ENTRY" TO OE-ORD.EST-NO.
       RETURN "NO FGITEM".
    END.
    IF eb.stock-no <> "" THEN DO:
        RUN fg/GetItemfgActInact.p (INPUT g_company,
                                    INPUT eb.stock-no,
                                    OUTPUT lActive).

        IF NOT lActive THEN DO:
            MESSAGE eb.stock-no "has InActive Status. Order cannot be placed for the Inactive Item."
                  VIEW-AS ALERT-BOX ERROR. 
            RETURN "Inactive FGItem".
       END.

    END.
  END. /* each eb */

  ASSIGN
   j         = 1
   fil_id    = save_id        /* reset fil_id, scrambled in calc...*/
   ll-order-from-est = YES.              

  FIND FIRST xeb WHERE xeb.company = xest.company 
                   AND xeb.est-no EQ xest.est-no
                   AND xeb.form-no  EQ 0
       NO-LOCK NO-ERROR.     
  FOR EACH eb WHERE eb.company = xest.company 
                AND eb.est-no  EQ xest.est-no
                AND eb.form-no  NE 0
                AND eb.blank-no NE 0
                AND TRIM(eb.cust-no) NE ""
      NO-LOCK,
      FIRST cust NO-LOCK
      {sys/ref/custW.i}
        AND cust.cust-no EQ eb.cust-no
      USE-INDEX cust
      BREAK BY eb.est-no BY eb.cust-no BY eb.form-no BY eb.blank-no:

    IF FIRST-OF(eb.cust-no) THEN DO:
        /* if order is not created from above - only one cust-no */
          v-prod-cat = eb.procat.
      FIND xoe-ord WHERE RECID(xoe-ord) = recid(oe-ord) NO-LOCK.

      IF ll-do-job THEN DO:
          cEstNo = FILL(" ",6 - length(TRIM(oe-ord.est-no:screen-value))) + trim(oe-ord.est-no:screen-value).
        v-job-no = FILL(" ",6 - length(TRIM(oe-ord.ord-no:screen-value))) + oe-ord.ord-no:screen-value.
        RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-prod-cat,
                         INPUT cEstNo).

        IF v-job-no EQ "" THEN
          v-job-no = cEstNo.

      END.
      ELSE
        ASSIGN
         v-job-no  = ""
         v-job-no2 = 0.

        RUN display-cust-detail (RECID(cust)).

      ASSIGN oe-ord.sold-id:screen-value   = eb.cust-no /** DEFAULT to first SOLD to **/
             oe-ord.sman[1]:screen-value   = eb.sman
             oe-ord.cust-no:screen-value   = eb.cust-no
             oe-ord.carrier:screen-value   = eb.carrier
             oe-ord.frt-pay:screen-value   = eb.chg-method
             oe-ord.s-pct[1]:screen-value  = "100"
             oe-ord.due-code:screen-value  = "ON"
/*              v-job-no2 = 0 */
             oe-ord.job-no:screen-value    = v-job-no
             oe-ord.job-no2:screen-value   = STRING(v-job-no2)
             oe-ord.cust-no:screen-value   = cust.cust-no
             oe-ord.cust-name:screen-value = cust.name
             oe-ord.addr[1]:screen-value   = cust.addr[1]
             oe-ord.addr[2]:screen-value   = cust.addr[2]
             oe-ord.city:screen-value      = cust.city
             oe-ord.state:screen-value     = cust.state
             oe-ord.zip:screen-value       = cust.zip
             oe-ord.contact:screen-value   = cust.contact
             oe-ord.last-date:SCREEN-VALUE = STRING(oe-ord.ord-date + cust.ship-days)
             oe-ord.due-date:SCREEN-VALUE  = oe-ord.last-date:SCREEN-VALUE
             oe-ord.terms:screen-value     = cust.terms
             oe-ord.over-pct:screen-value  = STRING(cust.over-pct)
             oe-ord.under-pct:screen-value = STRING(cust.under-pct)
             oe-ord.fob-code:screen-value  = cust.fob-code
             oe-ord.tax-gr:screen-value    = cust.tax-gr
             v-custype         = cust.type.

      IF lastship-cha = "Stock/Custom" THEN DO:
          /* If order has no estimate. */
          IF oe-ord.est-no:SCREEN-VALUE = "" THEN
              ASSIGN oe-ord.due-date:SCREEN-VALUE = STRING(DATE(oe-ord.ord-date:Screen-value) + lastship-int).
          ELSE
              ASSIGN oe-ord.due-date:SCREEN-VALUE = STRING(DATE(oe-ord.ord-date:Screen-value) + INT(lastship-dec)).
      END.

      FIND FIRST sman WHERE
           sman.company EQ eb.company AND
           sman.sman EQ eb.sman
           NO-LOCK NO-ERROR.
      IF AVAIL sman THEN
            ASSIGN oe-ord.sname[1]:screen-value = sman.sname .

      IF oeestcom-log = NO OR
         NOT(AVAIL sman AND sman.commbasis EQ "M") THEN
         ASSIGN
            oe-ord.s-comm[1]:screen-value = STRING(eb.comm)
            v-margin = 0.
      ELSE
      DO:
         lv-qty = 0.
         FIND FIRST est-qty WHERE
              est-qty.company = xest.company AND
              est-qty.est-no = xest.est-no
              NO-LOCK NO-ERROR.

         /*best guess before actually picking qty
           order header will be updated later with current commission*/
         IF AVAIL est-qty THEN
            lv-qty = est-qty.qty[1].

         v-qty = IF NOT(v-est-type EQ 3 OR v-est-type EQ 4) THEN
                    lv-qty
                 ELSE
                    eb.bl-qty.

         IF NOT(eb.est-type EQ 4 OR eb.est-type EQ 8) THEN
            FOR EACH probe WHERE
                probe.company = eb.company AND
                probe.est-no = eb.est-no AND
                probe.probe-date NE ? AND
                probe.est-qty EQ v-qty
                NO-LOCK
                BY probe.probe-date DESC
                BY probe.probe-time DESC:

                LEAVE.
            END.
         ELSE
            FOR EACH probe WHERE
                probe.company = eb.company AND
                probe.est-no = eb.est-no AND
                probe.probe-date NE ?
                NO-LOCK
                BY probe.probe-date DESC
                BY probe.probe-time DESC:

                LEAVE.
            END.

         IF AVAIL probe THEN
            ASSIGN
               oe-ord.s-comm[1]:SCREEN-VALUE = STRING(probe.comm)
               v-margin = probe.market-price.
         ELSE
            ASSIGN
               oe-ord.s-comm[1]:screen-value = STRING(eb.comm)
               v-margin = 0.
      END.

      IF xest.ord-no NE 0 THEN fi_prev_order:screen-value = STRING(xest.ord-no).
      IF oe-ord.TYPE = "T" AND oe-ord.pord- GT 0 THEN
          fi_prev_order:SCREEN-VALUE = STRING(oe-ord.pord-).
      IF FIRST(eb.cust-no) THEN 
         fil_id = RECID(xoe-ord).



      FIND FIRST terms WHERE terms.company EQ cocode
                        AND terms.t-code  EQ cust.terms
               NO-LOCK NO-ERROR.
      IF AVAIL terms THEN  oe-ord.terms-d:screen-value = terms.dscr.
      ELSE oe-ord.terms-d:screen-value = "".

      IF cust.active EQ "X" THEN fi_type:screen-value = "T".

      v-factor = IF xest.est-type GE 1 AND xest.est-type LE 4 THEN  lastship-dec
                 ELSE 1. 
      IF lastship-cha EQ "Fibre"  THEN
        ASSIGN
         oe-ord.last-date:SCREEN-VALUE = STRING(TODAY + (cust.ship-days * v-factor))
         oe-ord.due-date:SCREEN-VALUE  = STRING(TODAY + (lastship-int * v-factor)).

      IF oe-ord.carrier:screen-value EQ "" THEN oe-ord.carrier:screen-value = cust.carrier.
    END. /* first-of(eb.cust-no) */

    LEAVE. /** 2pc box & Set headers **/
  END. /* each eb */
END. /* avail xest */
END. /*  */

RUN get-po-def .

ASSIGN
 ll-est-no-mod     = NO
 lv-old-cust-no    = oe-ord.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
 ll-cust-displayed = YES.

RUN release-shared-buffers.
{&methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-po-def V-table-Win 
PROCEDURE get-po-def :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "OEBlanketPO#"
       NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN sys-ctrl.company  = cocode
            sys-ctrl.name     = "OEBlanketPO#"
            sys-ctrl.descrip  = "User Define Default Customer Po#"
            sys-ctrl.char-fld = "" 
            sys-ctrl.log-fld  = YES.
  END.
  IF AVAIL sys-ctrl  THEN
      FOR EACH sys-ctrl-shipto OF sys-ctrl WHERE 
      sys-ctrl-shipto.cust-vend-no = oe-ord.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      AND  sys-ctrl-shipto.log-fld NO-LOCK :
        ASSIGN oe-ord.po-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sys-ctrl-shipto.char-fld .
        LEAVE.
     END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-comm V-table-Win 
PROCEDURE hide-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-hidden AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
        fi_s-pct-lbl:HIDDEN     = ip-hidden
        fi_s-comm-lbl:HIDDEN    = ip-hidden
        oe-ord.s-pct[1]:HIDDEN IN FRAME {&FRAME-NAME}  = ip-hidden
        oe-ord.s-pct[2]:HIDDEN IN FRAME {&FRAME-NAME} = ip-hidden
        oe-ord.s-pct[3]:HIDDEN IN FRAME {&FRAME-NAME} = ip-hidden
        oe-ord.s-comm[1]:HIDDEN IN FRAME {&FRAME-NAME} = ip-hidden
        oe-ord.s-comm[2]:HIDDEN IN FRAME {&FRAME-NAME} = ip-hidden
        oe-ord.s-comm[3]:HIDDEN IN FRAME {&FRAME-NAME} = ip-hidden.

     IF NOT ip-hidden THEN
     DO:
        DISPLAY rect-36
                fi_sman-lbl
                fi_sname-lbl
                fi_s-pct-lbl
                fi_s-comm-lbl.

        IF AVAIL oe-ord THEN
           DISPLAY
              oe-ord.sman[1]
              oe-ord.sman[2]
              oe-ord.sman[3]   
              oe-ord.sname[1]
              oe-ord.sname[2]  
              oe-ord.sname[3]
              oe-ord.s-pct[1]
              oe-ord.s-pct[2]
              oe-ord.s-pct[3]
              oe-ord.s-comm[1]
              oe-ord.s-comm[2]
              oe-ord.s-comm[3].
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hold-approve V-table-Win 
PROCEDURE hold-approve :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF    VAR      char-hdl         AS cha     NO-UNDO.
    DEF    VAR      lv-uom           LIKE itemfg.prod-uom NO-UNDO.
    DEF    VAR      lv-job-recid     AS RECID   NO-UNDO.
    DEF    VAR      choice           AS LOG     NO-UNDO.
    DEF    VAR      hld-id           AS RECID   NO-UNDO.
    DEF    VAR      hld-stat         LIKE job.stat NO-UNDO.
    DEF    VAR      hld-nufile       AS LOG     NO-UNDO.
    DEF    VAR      v-run-schedule   AS LOG     NO-UNDO.
    DEF    VAR      lBypassZeroPrice AS LOG     NO-UNDO.
    DEFINE VARIABLE lBypassDuplicate AS LOGICAL NO-UNDO.

    DEF BUFFER b-oe-ord  FOR oe-ord.
    DEF BUFFER b-oe-ordl FOR oe-ordl.
    DEF BUFFER b-cust    FOR cust.
{&methods/lValidateError.i YES}

    RELEASE cust.


    IF AVAIL oe-ord AND can-do("H,W",oe-ord.stat) AND 
        CAN-FIND(FIRST b-oe-ordl 
        WHERE b-oe-ordl.company EQ oe-ord.company
        AND b-oe-ordl.ord-no  EQ oe-ord.ord-no
        AND b-oe-ordl.price = 0) THEN 
    DO:
        lBypassZeroPrice = NO.
        MESSAGE "Please Note: An item on this order has $0 sales price.  Continue?"
            VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE lBypassZeroPrice.
        IF NOT lBypassZeroPrice THEN
            RETURN ERROR.
    END.


    IF AVAIL oe-ord 
        AND can-do("W",oe-ord.stat) 
        AND oe-ord.po-no NE '' THEN 
    DO: 


        FIND FIRST b-oe-ord 
            WHERE b-oe-ord.company EQ oe-ord.company
            AND b-oe-ord.cust-no  EQ oe-ord.cust-no
            AND b-oe-ord.po-no EQ oe-ord.po-no
            AND ROWID(b-oe-ord) NE ROWID(oe-ord) 
            NO-LOCK NO-ERROR.
        IF AVAIL b-oe-ord 
            AND b-oe-ord.spare-char-3 EQ oe-ord.spare-char-3 
            AND oe-ord.spare-char-3 NE '' THEN 
        DO:
            MESSAGE "Duplicate order for the same PO Number(" oe-ord.po-no ") and Ariba Payload Id"
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
        ELSE IF AVAIL b-oe-ord THEN 
            DO:
                lBypassDuplicate = NO.
                FOR EACH oe-ordl OF oe-ord NO-LOCK:
                    IF NOT CAN-FIND(FIRST b-oe-ordl 
                        WHERE b-oe-ordl.company EQ oe-ordl.company
                        AND b-oe-ordl.cust-no EQ oe-ordl.cust-no
                        AND b-oe-ordl.LINE EQ oe-ordl.LINE
                        AND b-oe-ordl.i-no EQ oe-ordl.i-no
                        AND b-oe-ordl.po-no EQ oe-ordl.po-no
                        AND b-oe-ordl.qty EQ oe-ordl.qty
                        AND ROWID(b-oe-ordl) NE ROWID(oe-ordl))
                        THEN 
                    DO:
                        lBypassDuplicate = YES.
                        LEAVE.
                    END.
                END.
                IF lBypassDuplicate EQ NO THEN
                    MESSAGE "Duplicate order for the same PO Number(" oe-ord.po-no ") found.  Continue?"
                        VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE lBypassDuplicate.
                IF NOT lBypassDuplicate THEN RETURN ERROR.
            END.
    END. /* ord.stat = W */


    /* wfk - changed from exclusive */
    IF AVAIL oe-ord THEN
        FIND b-oe-ord WHERE ROWID(b-oe-ord) EQ ROWID(oe-ord)
            NO-LOCK NO-ERROR NO-WAIT.

    IF AVAIL b-oe-ord /*AND CAN-DO("A,H,W",oe-ord.stat)*/ THEN
        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no
            NO-LOCK NO-ERROR.

    IF AVAIL cust THEN 
    DO:

        IF oe-ord.stat EQ "W" THEN 
        DO:
            /* Set detault values, run credit check, create PO, create job */
            SESSION:SET-WAIT-STATE("General").

            DO TRANSACTION:  /* Set default values */

                FIND CURRENT oe-ord EXCLUSIVE-LOCK.
                ASSIGN
                    oe-ord.user-id     = USERID("nosweat")
                    oe-ord.approved-id = USERID("nosweat")
                    oe-ord.t-freight   = 0.

                IF oe-ord.type EQ "" THEN oe-ord.type = "O".

                IF oe-ord.sman[1] EQ "" THEN
                    ASSIGN
                        oe-ord.sman    = ""
                        oe-ord.sman[1] = cust.sman.

                IF oe-ord.sman[1] NE "" AND oe-ord.s-pct[1] EQ 0 THEN
                    oe-ord.s-pct[1] = 100.00.

                DO i = 1 TO EXTENT(oe-ord.sman):
                    IF oe-ord.s-comm[i] GE 100 THEN
                        ASSIGN
                            oe-ord.s-comm = 0
                            v-margin      = 0.

                    FIND FIRST sman
                        WHERE sman.company EQ oe-ord.company
                        AND sman.sman    EQ oe-ord.sman[i]
                        NO-LOCK NO-ERROR.
                    IF AVAIL sman THEN 
                    DO:
                        oe-ord.sname[1] = sman.sname.

                        IF oe-ord.s-comm[i] LE 0 THEN
                        DO:
                            oe-ord.s-comm[i] = sman.scomm.
                            IF i = 1 THEN
                                v-margin = 0.
                        END.
                    END.
                END. /* do i = 1 to sman */

                FOR EACH b-oe-ordl NO-LOCK
                    WHERE b-oe-ordl.company EQ oe-ord.company
                    AND b-oe-ordl.ord-no  EQ oe-ord.ord-no
                    AND b-oe-ordl.job-no  EQ ""
                    AND b-oe-ordl.i-no    NE "",
                    FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ b-oe-ordl.company
                    AND itemfg.i-no    EQ b-oe-ordl.i-no:

                    FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(b-oe-ordl)
                        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                    IF NOT AVAIL oe-ordl THEN NEXT.

                    ASSIGN
                        oe-ordl.req-code  = oe-ord.due-code
                        oe-ordl.req-date  = oe-ord.due-date
                        oe-ordl.prom-code = oe-ord.due-code
                        oe-ordl.prom-date = oe-ord.due-date.

                    DO i = 1 TO MIN(EXTENT(oe-ordl.s-man),EXTENT(oe-ord.sman)):
                        ASSIGN
                            oe-ordl.s-man[i]  = oe-ord.sman[i]
                            oe-ordl.s-pct[i]  = oe-ord.s-pct[i]
                            oe-ordl.s-comm[i] = oe-ord.s-comm[i].

                        IF i = 1 THEN
                            ASSIGN
                                oe-ordl.q-qty = oe-ord.t-fuel
                                v-margin      = oe-ord.t-fuel.
                    END.

                    FIND FIRST po-ordl NO-LOCK
                        WHERE po-ordl.company   EQ oe-ordl.company
                        AND po-ordl.i-no      EQ oe-ordl.i-no
                        AND po-ordl.po-no     EQ oe-ordl.po-no-po
                        AND po-ordl.item-type EQ NO
                        USE-INDEX item-ordno NO-ERROR.
                    IF AVAIL po-ordl THEN
                        ASSIGN
                            lv-uom       = po-ordl.cons-uom
                            oe-ordl.cost = po-ordl.cons-cost.
                    ELSE
                        ASSIGN
                            lv-uom       = itemfg.prod-uom
                            oe-ordl.cost = itemfg.total-std-cost.

                    IF lv-uom NE "M" THEN
                        RUN sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                            oe-ordl.cost, OUTPUT oe-ordl.cost).

                    RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT oe-ordl.t-freight).
                    oe-ord.t-freight = oe-ord.t-freight + oe-ordl.t-freight.
                    FIND CURRENT oe-ordl NO-LOCK.
                    RELEASE oe-ordl.
                END. /* Each oe-ordl */
                FIND CURRENT oe-ord NO-LOCK.
            END. /* Transaction, set default values */


            RUN oe/ordfrate.p (ROWID(oe-ord)).

            FIND xoe-ord WHERE ROWID(xoe-ord) EQ ROWID(oe-ord) NO-LOCK NO-ERROR.
            IF AVAIL xoe-ord THEN RUN oe/oe-comm.p.

            RUN oe/calcordt.p (ROWID(oe-ord)).

            RUN oe/creditck.p (ROWID(cust), YES). 

            FIND FIRST b-cust WHERE
                b-cust.company EQ cust.company AND
                b-cust.cust-no EQ cust.cust-no
                NO-LOCK.

            DO TRANSACTION: /* Set oe-ord.stat */
                FIND CURRENT oe-ord EXCLUSIVE-LOCK.
                IF b-cust.cr-hold THEN 
                DO:
                    ASSIGN
                        oe-ord.stat:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "H"
                        oe-ord.stat                                     = "H"
                        oe-ord.approved-date                            = TODAY.
                    IF OEJobHold-log THEN 
                        RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT "Hold").
                END.
                ELSE 
                DO:
                    ASSIGN
                        oe-ord.stat:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "A"
                        oe-ord.stat                                     = "A"
                        oe-ord.approved-date                            = TODAY.
                    IF OEJobHold-log THEN 
                        RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT "Approve").             
                END.
                FIND CURRENT oe-ord NO-LOCK.
            END. /* transaction, set oe-ord.stat */


            IF v-create-job AND oe-ord.job-no NE "" THEN 
            DO TRANSACTION: /*create job*/

                FIND FIRST job NO-LOCK
                    WHERE job.company EQ oe-ord.company
                    AND job.job-no  EQ oe-ord.job-no
                    AND job.job-no2 EQ oe-ord.job-no2
                    NO-ERROR.

                IF AVAIL job AND TRIM(job.est-no) NE TRIM(oe-ord.est-no) THEN
                    IF CAN-FIND(FIRST job-hdr
                        WHERE job-hdr.company EQ job.company
                        AND job-hdr.job     EQ job.job
                        AND job-hdr.job-no  EQ job.job-no
                        AND job-hdr.job-no2 EQ job.job-no2
                        AND job-hdr.ord-no  NE oe-ord.ord-no) OR
                        CAN-FIND(FIRST b-oe-ord
                        WHERE b-oe-ord.company EQ job.company
                        AND b-oe-ord.job-no  EQ job.job-no
                        AND b-oe-ord.job-no2 EQ job.job-no2
                        AND b-oe-ord.est-no  EQ job.est-no)   OR
                        CAN-FIND(FIRST b-oe-ordl
                        WHERE b-oe-ordl.company EQ job.company
                        AND b-oe-ordl.job-no  EQ job.job-no
                        AND b-oe-ordl.job-no2 EQ job.job-no2
                        AND b-oe-ordl.est-no  EQ job.est-no)  THEN RELEASE job.
                    ELSE
                    DO:
                        FIND CURRENT job NO-ERROR.
                        IF AVAIL job THEN DELETE job.
                    END.

                IF NOT AVAIL job THEN 
                DO:
                    RUN create-job (OUTPUT lv-job-recid).
                    FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
                END.                 

                v-qty-mod = YES.

                IF AVAIL job AND INDEX("HWPRL",job.stat) NE 0 THEN 
                DO:
                    /*IF NOT v-qty-mod THEN
                       RUN oe/job-qty.p (ROWID(oe-ord), OUTPUT v-qty-mod).*/

                    IF v-qty-mod OR job.stat EQ "P" THEN 
                    DO:
                        RUN jc/chkrebld.p (RECID(job), OUTPUT choice).     
                        IF NOT choice THEN 
                        DO:
                            ASSIGN 
                                hld-id     = fil_id
                                hld-nufile = nufile 
                                hld-stat   = job.stat
                                nufile     = YES.

                            RUN jc/jc-calc.p(RECID(job), NO).
                            ASSIGN 
                                fil_id = hld-id
                                nufile = hld-nufile.

                            IF hld-stat NE "P" THEN 
                            DO:
                                FIND CURRENT job EXCLUSIVE.
                                job.stat = hld-stat.
                                FIND CURRENT job NO-LOCK.
                            END.
                        END.
                    END.
                END.

                FIND FIRST sys-ctrl WHERE
                    sys-ctrl.company EQ cocode AND
                    sys-ctrl.name    EQ "SCHEDULE"
                    NO-LOCK NO-ERROR.

                v-run-schedule = IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO
                ELSE IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES
                ELSE NO.

                FOR EACH oe-ordl NO-LOCK
                    WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.ord-no  EQ oe-ord.ord-no
                    AND oe-ordl.is-a-component EQ NO

                    BREAK BY oe-ordl.job-no
                    BY oe-ordl.job-no2:

                    IF LAST-OF(oe-ordl.job-no2) THEN 
                    DO:
                        ASSIGN
                            hld-id     = fil_id
                            hld-nufile = nufile
                            fil_id     = RECID(oe-ordl).

                        RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.
                        /* check oe-ordl.due-date and calc promised date and job's start-date */

                        IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date.

                        ASSIGN
                            fil_id = hld-id
                            nufile = hld-nufile.
                    END.
                END.
            END.  /* transaction if v-create-job */

            SESSION:SET-WAIT-STATE("").
        END. /* if a web order */

        ELSE 
        DO TRANSACTION: /*not web order*/
            FIND CURRENT oe-ord EXCLUSIVE-LOCK.
            ASSIGN 
                oe-ord.stat          = IF oe-ord.stat NE "H" THEN "H" ELSE "A"
                oe-ord.approved-date = TODAY.       
            IF OEJobHold-log THEN 
                RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT (IF oe-ord.stat EQ "H" THEN  "Hold" ELSE "Approve")).
            FIND CURRENT oe-ord NO-LOCK.
        END. /* transaction not a web order */


        DO TRANSACTION: /* Update or-ord.posted */
            FIND CURRENT oe-ord EXCLUSIVE-LOCK.
            IF oe-ord.stat EQ "A" THEN oe-ord.posted = NO.

            FIND CURRENT oe-ord NO-LOCK NO-ERROR.
            FIND CURRENT b-oe-ord NO-LOCK NO-ERROR.
            FIND CURRENT oe-ord NO-LOCK.
        END. /* transaction */


        DO TRANSACTION:
            /* Update cust.cr-hold */
            FIND CURRENT cust EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL cust THEN 
            DO:

                cust.cr-hold = oe-ord.stat EQ "H" OR
                    CAN-FIND(FIRST b-oe-ord
                    WHERE b-oe-ord.company EQ oe-ord.company
                    AND b-oe-ord.cust-no EQ oe-ord.cust-no
                    AND b-oe-ord.stat    EQ "H").

                FIND CURRENT cust NO-LOCK NO-ERROR.
            END. /* avail cust */
        END. /* transaction - update cust */


        RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
            RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (ROWID(oe-ord)).
    END. /* if avail cust */

{&methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF oe-ord.est-no:SENSITIVE IN FRAME {&FRAME-NAME} THEN APPLY 'entry' TO oe-ord.est-no.
  ll-ord-from-est-run = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF    VAR      lv-date           LIKE oe-ord.due-date NO-UNDO.
    DEF    VAR      lv-stat           AS CHAR    NO-UNDO.
    DEF    VAR      lv-ord-no         LIKE oe-ord.ord-no NO-UNDO.
    DEF    VAR      calcDueDate       AS DATE    NO-UNDO.
    DEF    VAR      calcStartDate     AS DATE    NO-UNDO.
    DEF    VAR      li-tries          AS INT     NO-UNDO.
    DEF    VAR      llUpdateStartDate AS LOG     NO-UNDO.
    DEFINE VARIABLE lInvoiceFound     AS LOGICAL NO-UNDO.

    DEF    VAR      dCalcDueDate      AS DATE    NO-UNDO.
    DEF    VAR      dCalcPromDate     AS DATE    NO-UNDO.
    DEF BUFFER b-oe-rel    FOR oe-rel.
    DEF BUFFER due-job-hdr FOR job-hdr.
    DEF BUFFER bf-oe-ordl  FOR oe-ordl.
    DEF BUFFER bf-job      FOR job.
    DEF BUFFER bf-oe-ord   FOR oe-ord.
    DEF BUFFER bf-oe-rel   FOR oe-rel.

    SESSION:SET-WAIT-STATE ("general").
    DEF VAR char-hdl AS CHAR NO-UNDO.

    /* Buttons were made sensitive = no during add, so reverse that here */
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
    RUN make-buttons-sensitive IN WIDGET-HANDLE(char-hdl).

    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
        ll-new-po = NO.
        lv-change-inv-po = NO.

        IF NOT adm-new-record AND oe-ord.po-no NE oe-ord.po-no:SCREEN-VALUE THEN 
        DO:

            MESSAGE "Update all order lines/releases with this "  +
                TRIM(oe-ord.po-no:LABEL) + "?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-new-po.

            IF OEPO#Xfer-log THEN 
            DO:
                lInvoiceFound = FALSE.
                FOR EACH oe-boll 
                    WHERE oe-boll.company EQ oe-ord.company
                    AND oe-boll.ord-no EQ oe-ord.ord-no
                    NO-LOCK,
                    EACH inv-head WHERE inv-head.company EQ oe-boll.company
                    AND inv-head.bol-no EQ oe-boll.bol-no
                    NO-LOCK,             
                    FIRST inv-line 
                    WHERE inv-line.r-no   EQ inv-head.r-no 
                    AND inv-line.ord-no  EQ oe-boll.ord-no 
                    NO-LOCK .
                    lInvoiceFound = TRUE.
                    LEAVE.
                END. /* each oe-boll */
                IF lInvoiceFound THEN
                    MESSAGE "Unposted Invoice Exists, transfer new Customer PO# to Invoice?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lv-change-inv-po.
            END. /* if oepo#xfer-log */

        END. /* If PO # changed */

    END. /* Do with frame */

    ASSIGN
        lv-date   = oe-ord.due-date
        lv-ord-no = oe-ord.ord-no.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    FIND FIRST terms WHERE terms.t-code = oe-ord.terms NO-LOCK NO-ERROR.
    IF AVAIL terms THEN oe-ord.terms-d = terms.dscr.


    IF dueDateChanged AND OEDateAuto-char = "colonial" THEN 
    DO:
        FOR EACH oe-ordl 
            WHERE oe-ordl.company EQ oe-ord.company
            AND oe-ordl.ord-no  EQ oe-ord.ord-no
            NO-LOCK:

            FOR EACH oe-rel 
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                NO-LOCK
                BY oe-rel.rel-date:

                IF LOOKUP(oe-rel.stat, 'A,C,P,Z' ) EQ 0 THEN 
                DO:

                    RUN new-due-date (INPUT ROWID(oe-rel)).
                    FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ rowid(oe-rel) EXCLUSIVE-LOCK.

                    bf-oe-rel.spare-char-4 = STRING(oe-ord.due-date) + ",,".


                    bf-oe-rel.rel-date = get-colonial-rel-date(ROWID(bf-oe-rel)).        
                    FIND CURRENT bf-oe-rel NO-LOCK.
                    RELEASE bf-oe-rel.
                END.

                /* Only consider first one */
                LEAVE.
            END. /* each oe-rel */
        END. /* Each oe-ordl */
    END.  /* if dueDateChanged...*/

    IF oe-ord.job-no NE '' THEN 
    DO:
        FIND FIRST job EXCLUSIVE-LOCK WHERE job.company EQ oe-ord.company
            AND job.job-no EQ oe-ord.job-no
            AND job.job-no2 EQ oe-ord.job-no2 NO-ERROR.
        IF AVAILABLE job THEN 
        DO:
            IF dueDateChanged THEN 
            DO:

                job.due-date = oe-ord.due-date.
                FOR EACH due-job-hdr EXCLUSIVE-LOCK
                    WHERE due-job-hdr.company EQ job.company
                    AND due-job-hdr.job     EQ job.job
                    AND due-job-hdr.job-no  EQ job.job-no
                    AND due-job-hdr.job-no2 EQ job.job-no2:
                    due-job-hdr.due-date = oe-ord.due-date.
                END. /* each due-job-hdr */
            END. /* if duedatechanged */
            FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ oe-ord.company
                AND sys-ctrl.name EQ 'SCHEDULE' NO-ERROR.
            IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
            DO:
                IF prodDateChanged THEN
                    job.start-date = IF sys-ctrl.char-fld NE 'NoDate' THEN oe-ord.prod-date
                    ELSE ?.

                IF dueDateChanged AND sys-ctrl.char-fld EQ 'PlanDate' THEN 
                DO:
                    IF NOT VALID-HANDLE(scheduleHndl) THEN
                        RUN custom/schedule.p PERSISTENT SET scheduleHndl.
                    RUN scheduleJob IN scheduleHndl (ROWID(job),OUTPUT calcStartDate,OUTPUT calcDueDate).
                    IF calcDueDate NE oe-ord.due-date THEN
                        MESSAGE 'Machine Capacity calulated Scheduled Completion date of'
                            calcDueDate SKIP 'Update Due Date and Promise Date on Order?'
                            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                            UPDATE updateDueDate AS LOGICAL.
                    IF updateDueDate THEN 
                    DO:
                        FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord) EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAIL bf-oe-ord THEN
                            ASSIGN
                                job.due-date    = calcDueDate
                                oe-ord.due-date = calcDueDate.
                        FIND CURRENT bf-oe-ord NO-LOCK NO-ERROR.
                    END.
                END. /* if duedatechanged */

                job.start-date = calcStartDate.
                FIND CURRENT job NO-LOCK.
            END. /* avail sys-ctrl */

        END. /* avail job */
    END. /* if job-no ne '' */

    /*BV-Update order line job start dates*/
    FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ oe-ord.company
        AND sys-ctrl.name EQ 'SCHEDULE' NO-ERROR.
    IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND prodDateChanged THEN 
    DO:
        MESSAGE 'Update Job Start Date on all items with new Prod. Date of '
            oe-ord.prod-date
            '?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE llUpdateStartDate.
        IF llUpdateStartDate THEN 
        DO:
            FOR EACH bf-oe-ordl OF oe-ord EXCLUSIVE-LOCK:
                bf-oe-ordl.spare-int-2 = INT(oe-ord.prod-date).                    
            END. /*each bf-oe-ordl*/
        END. /*llUpdateStartDate*/
    END. /*ProdDateChanged*/

    FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf-oe-ord THEN 
    DO:
        ASSIGN
            prodDateChanged = NO
            dueDateChanged  = NO
            oe-ord.type     = fi_type
            oe-ord.po-no2   = fi_prev_order
            oe-ord.t-fuel   = v-margin.

        oe-ord.pord-no = INT(fi_prev_order) NO-ERROR.
    END.

    IF  lv-change-inv-po THEN 
    DO:        
        RUN oe/poNoChange.p (INPUT oe-ord.company,
            INPUT oe-ord.ord-no,
            INPUT oe-ord.po-no,
            INPUT "").
    END.

    FOR EACH oe-ordl OF oe-ord:
        IF ll-new-po THEN 
        DO:
            oe-ordl.po-no = oe-ord.po-no.
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company  EQ oe-ordl.company
                AND oe-rel.ord-no   EQ oe-ordl.ord-no
                AND oe-rel.i-no     EQ oe-ordl.i-no
                AND oe-rel.line     EQ oe-ordl.line:

                RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

                li-tries = 0.
                IF INDEX("SLIA",lv-stat) GT 0 THEN 
                DO WHILE TRUE:
                    li-tries = li-tries + 1.
                    IF li-tries GE 1000 THEN LEAVE.

                    FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ ROWID(oe-rel)
                    EXCLUSIVE NO-WAIT NO-ERROR.
                    IF AVAIL b-oe-rel THEN 
                    DO:
                        b-oe-rel.po-no = oe-ordl.po-no.
                        LEAVE.
                    END.
                END.
            END.
        END.

        IF oe-ordl.est-no NE "" THEN 
        DO:
            FIND eb
                WHERE eb.company  EQ oe-ordl.company
                AND eb.est-no   EQ oe-ordl.est-no
                AND ((eb.est-type NE 2 AND eb.est-type NE 6) OR
                ((eb.est-type EQ 2 OR eb.est-type EQ 6) AND
                eb.form-no EQ 0))
            EXCLUSIVE NO-ERROR.
            IF AVAIL eb THEN eb.stock-no = oe-ordl.i-no.
        END.
    END.

    RUN bill_notes .

    IF oe-ord.due-date NE lv-date THEN
        FOR EACH oe-ordl OF oe-ord BREAK BY oe-ordl.line:
            IF NOT ll-new-due THEN
                ll-new-due = FIRST(oe-ordl.line) AND LAST(oe-ordl.line).

            IF NOT ll-new-due THEN
                MESSAGE "Update all line items with this Due Date?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll-new-due.

            IF ll-new-due THEN 
            DO:
                oe-ordl.req-date = oe-ord.due-date.

                IF oe-ordl.req-date GT oe-ordl.prom-date THEN
                    oe-ordl.prom-date = oe-ordl.req-date.

                IF oeDateAuto-log AND OeDateAuto-Char = "Colonial" THEN 
                DO:
                    RUN oe/dueDateCalc.p (INPUT oe-ord.cust-no,
                        INPUT oe-ordl.req-date,
                        INPUT oe-ordl.prom-date,
                        INPUT "DueDate",
                        INPUT ROWID(oe-ordl),
                        OUTPUT dCalcDueDate,
                        OUTPUT dCalcPromDate).
                    oe-ordl.prom-date = dCalcPromDate.     

                END. /* if oeDateAuto for colonial */

            END. /* If ll-new-due */
            ELSE LEAVE.
        END. /* each oe-ordl */
    FIND CURRENT oe-ordl NO-LOCK NO-ERROR.

    RELEASE eb.
    RELEASE oe-rel.
    RELEASE bfx-ord.
    RELEASE oe-ordl.

    FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord) EXCLUSIVE-LOCK NO-ERROR.  
    IF AVAIL bf-oe-ord THEN 
    DO:
        IF oe-ord.frt-pay = "B" THEN oe-ord.f-bill = YES.
        ELSE oe-ord.f-bill = NO.
    END.
    FIND CURRENT bf-oe-ord NO-LOCK NO-ERROR.

    /* 10171315 - Was not being set after an add for some reason */
    ASSIGN
        adm-new-record           = NO
        oe-ord.cust-no:SENSITIVE = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR char-hdl AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

    /* Buttons were made not sensitive during add, so reverse that here */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
  RUN make-buttons-sensitive IN WIDGET-HANDLE(char-hdl).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  copyRecord = NO.
  RUN release-shared-buffers.
  RUN disable-fields.
  /* To allow ctrl-o to be picked up */
  APPLY 'entry' TO btnCalendar-1 IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       include oe/copyOrder.i contains procedure copyOrder
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DEFINE VARIABLE nextOrdNo AS INTEGER NO-UNDO.
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
  DEF BUFFER bf-ord FOR oe-ord.
  /* gdm - 06040919 */
  MESSAGE 
       'Copy Order ? ' 
      VIEW-AS ALERT-BOX QUESTION 
      BUTTONS OK-CANCEL
       UPDATE v-copyflg AS LOGICAL.

  IF v-copyflg THEN
  DO:
     SESSION:SET-WAIT-STATE('General':U).


     RUN nextOrdNo (OUTPUT nextOrdNo).

     /* copyOrder now creates order using next available order # */
     RUN copyOrder (g_company,g_company,oe-ord.ord-no,INPUT-OUTPUT nextOrdNo).
     FIND FIRST oe-ord NO-LOCK
         WHERE oe-ord.company EQ g_company
           AND oe-ord.ord-no  EQ nextOrdNo
         NO-ERROR.

     RUN reset-browser (ROWID(oe-ord)).

     SESSION:SET-WAIT-STATE('':U).
     RUN dispatch ('cancel-record':U).
     MESSAGE 'Order Copy Complete!, Update Order?' VIEW-AS ALERT-BOX
       QUESTION BUTTONS YES-NO UPDATE updateOrder AS LOGICAL.
     IF updateOrder THEN
     {methods/run_link.i "TableIO-SOURCE" "new-state" "('update-begin':U)"}.
     RETURN. /* we do copy manually above, so cancel adm copy call */
  END.

  ELSE RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER bf-oe-ord FOR oe-ord.
  DEF VAR li-next-ordno AS INT NO-UNDO.
  DEF VAR liUserOrdNo   AS INT NO-UNDO.
  DEF VAR liOrdnoSV AS INT NO-UNDO.
  DEF BUFFER bf-company FOR company.

  /* Code placed here will execute PRIOR to standard behavior. */
  DEF VAR char-hdl AS CHAR NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
  RUN make-buttons-insensitive IN WIDGET-HANDLE(char-hdl).
  IF ll-ord-no-override THEN
      RUN promptForOrder (OUTPUT liUserOrdNo).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  lv-new-row-id = ROWID(oe-ord).

  /* Get current sequence value */
  RUN sys/ref/asicurseq.p (INPUT g_company, 
                        INPUT "order_seq", 
                        OUTPUT li-next-ordno) NO-ERROR. 
  liOrdnoSV = liUserOrdNo.

  IF      ll-ord-no-override 
      AND liOrdNoSV NE 0
      /* AND liOrdnoSV LT li-next-ordno + 1  */
      AND NOT CAN-FIND(FIRST bf-oe-ord 
                       WHERE bf-oe-ord.company EQ g_company
                         AND bf-oe-ord.ord-no EQ liOrdnoSV) THEN DO:
      /* If user has entered an order number beyond sequence value, */
      /* update sequence value                                      */
      IF liOrdNoSV GT li-next-ordno THEN DO:
         FIND bf-company WHERE bf-company.company EQ g_company NO-LOCK NO-ERROR.
         DYNAMIC-CURRENT-VALUE("order_seq" + bf-company.spare-char-1, "ASI") = liOrdNoSV.
      END.
      li-next-ordno = liOrdnoSV.

  END.
  ELSE DO:
      /* Ord No via sequence value */ 
      RUN sys/ref/asiseq.p (INPUT g_company, INPUT "order_seq", OUTPUT li-next-ordno) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
        MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

      DO WHILE CAN-FIND(FIRST bf-oe-ord
                        WHERE bf-oe-ord.company EQ g_company
                          AND bf-oe-ord.ord-no  EQ li-next-ordno
                          AND ROWID(bf-oe-ord)  NE ROWID(oe-ord)):

          RUN sys/ref/asiseq.p (INPUT g_company, 
                              INPUT "order_seq", 
                              OUTPUT li-next-ordno) NO-ERROR.    

      END.
  END. /* Ord-no via sequence value */
  ll-ord-no-override = FALSE.
  FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord) EXCLUSIVE-LOCK.

  ASSIGN bf-oe-ord.company  = g_company
         bf-oe-ord.loc      = g_loc
         bf-oe-ord.ord-date = TODAY
         bf-oe-ord.ord-no   = li-next-ordno
         bf-oe-ord.user-id  = USERID("nosweat")
         bf-oe-ord.entered-id = USERID("nosweat")
         bf-oe-ord.type     = "O"
         bf-oe-ord.due-code = "ON" NO-ERROR.

  FIND CURRENT bf-oe-ord NO-LOCK.

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
        fi_type:SCREEN-VALUE = oe-ord.TYPE
        fi_prev_order:SCREEN-VALUE = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2
                                     ELSE STRING(oe-ord.pord-no).
  END.

  FOR EACH b-oe-ordl
      WHERE b-oe-ordl.company EQ oe-ord.company
        AND b-oe-ordl.ord-no  EQ oe-ord.ord-no:
    DELETE b-oe-ordl.
  END.

  CREATE b-oe-ordl.
  ASSIGN
   b-oe-ordl.company = bf-oe-ord.company
   b-oe-ordl.ord-no  = bf-oe-ord.ord-no
   b-oe-ordl.line    = 0.

   FIND FIRST oe-ctrl  
     WHERE oe-ctrl.company EQ g_company 
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   /* If update is not possible, next session that succeeds will update */
   /* with the next available number                                    */
   IF AVAIL oe-ctrl THEN DO:
     oe-ctrl.n-ord = oe-ord.ord-no + 1.
     RELEASE oe-ctrl.
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER xfg-set FOR fg-set.
DEF BUFFER xitemfg FOR itemfg.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER del-job-hdr FOR job-hdr.

DEFINE VARIABLE v-totord          LIKE oe-ord.t-revenue NO-UNDO.
DEFINE VARIABLE v-tot-tax         LIKE oe-ord.tax       NO-UNDO.
DEFINE VARIABLE v-tot-freight     LIKE oe-ord.t-freight NO-UNDO.
DEFINE VARIABLE v-qty-lft         LIKE oe-ordl.qty      NO-UNDO.
DEFINE VARIABLE v-new-ord         AS LOGICAL          INITIAL NO NO-UNDO.
DEFINE VARIABLE v-ext-price       LIKE oe-ordl.t-price  NO-UNDO.
DEFINE VARIABLE v-tax-rate        AS DECIMAL          FORMAT "->>>.99" NO-UNDO.
DEFINE VARIABLE v-frt-tax-rate    LIKE v-tax-rate       NO-UNDO.
DEFINE VARIABLE v-period          AS INTEGER          NO-UNDO.
DEFINE VARIABLE tmp-ordm-amt      LIKE oe-ordm.amt      NO-UNDO.
DEFINE VARIABLE tmp-tax           LIKE oe-ord.tax       NO-UNDO.
DEFINE VARIABLE v-continue        AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-blank-fg-on-est AS INTEGER          NO-UNDO.
DEFINE VARIABLE char-hdl          AS cha              NO-UNDO.
DEFINE VARIABLE loop-limit        AS INTEGER          NO-UNDO.

DEF VAR orig-ord LIKE oe-ord.ord-no.
DEF VAR v-deleted AS LOG NO-UNDO.
DEF VAR ll-ans AS LOG NO-UNDO.
DEF BUFFER bf-oe-ord FOR oe-ord.

/* Code placed here will execute PRIOR to standard behavior. */
    RUN pre-del-validate.

    IF RETURN-VALUE EQ "ERROR" OR RETURN-VALUE EQ "ADM-ERROR" THEN
        RETURN NO-APPLY.

    FOR EACH oe-ordl OF oe-ord NO-LOCK:

        FIND FIRST itemfg
          WHERE itemfg.company EQ oe-ordl.company
            AND itemfg.i-no    EQ oe-ordl.i-no
          NO-ERROR.

        IF AVAIL itemfg THEN DO:
          ll-ans = NO.

          IF itemfg.avg-cost       EQ 0 AND itemfg.last-cost EQ 0 AND
          itemfg.total-std-cost EQ 0 AND itemfg.beg-bal   EQ 0 AND
          itemfg.q-onh          EQ 0 AND itemfg.q-ono     EQ 0 AND
          itemfg.q-alloc        EQ 0 AND itemfg.q-back    EQ 0 AND
          itemfg.q-avail        EQ 0 THEN
          FOR FIRST est
            WHERE est.company EQ oe-ord.company
            AND est.est-no  EQ oe-ord.est-no
            NO-LOCK,

            EACH eb
            WHERE eb.company    EQ est.company
              AND eb.est-no     EQ est.est-no
              AND eb.stock-no   NE ""
              AND (eb.stock-no  EQ oe-ordl.i-no OR
            est.est-type EQ 2 OR est.est-type EQ 6):

            IF NOT ll-ans THEN DO:


              ll-ans = YES.
              MESSAGE "Remove FG Item#" +
                (IF est.est-type EQ 2 OR est.est-type EQ 6 THEN "s" ELSE "")
                "from the Estimate?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE ll-ans.


            END. /* If has no inventory or cost */

            IF ll-ans THEN DO:
                CREATE tt-del-eb.
                ASSIGN tt-del-eb.del-rowid = ROWID(eb).
            END.

            RELEASE eb.
          END. /* for first est, each eb */
        END. /* if avail itemfg */
    END. /* each ordl of ord */
    {&methods/lValidateError.i YES}
    /* {oe/v-ord-d.i} */
    RUN del-detail-recs.
    IF RETURN-VALUE EQ "ERROR" OR RETURN-VALUE EQ "ADM-ERROR" THEN
        RETURN NO-APPLY.


    IF INDEX("NAHW",oe-ord.stat) GT 0 THEN DO:
        /* Dispatch standard ADM method.                             */
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ).
    END.

    ELSE DO:
        FIND bf-oe-ord WHERE ROWID(bf-oe-ord) = ROWID(oe-ord) EXCLUSIVE-LOCK NO-ERROR.
        bf-oe-ord.stat = "D".
        FIND CURRENT bf-oe-ord NO-LOCK.

        RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    END.
    /* Code placed here will execute AFTER standard behavior.    */

    /* Set back to original value in case something else needs it */
    FIND bf-oe-ord WHERE ROWID(bf-oe-ord) = ROWID(oe-ord) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf-oe-ord THEN DO:
      bf-oe-ord.deleted = v-deleted.
      FIND CURRENT bf-oe-ord NO-LOCK.
    END.

    /* RELEASE oe-ord. */
    RELEASE itemfg.
    RELEASE itemfg-loc.
    RELEASE oe-ordl.
    RELEASE job.
    RUN release-shared-buffers.

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    RUN reopen-query IN WIDGET-HANDLE(char-hdl).
    {&methods/lValidateError.i NO}
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
  IF AVAIL oe-ord AND NOT adm-new-record THEN DO:
     ASSIGN
        fi_type = oe-ord.TYPE
        fi_prev_order = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2
                        ELSE STRING(oe-ord.pord-no).
     IF oe-ord.TYPE EQ "T" AND oe-ord.pord-no GT 0 THEN
         fi_prev_order = STRING(oe-ord.pord-no).

  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN oe-ord.spare-char-2:TOOLTIP =  getOrdStatDescr(oe-ord.spare-char-2:SCREEN-VALUE).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN ll-from-tandem = NO.

  /*if not avail oe-ord and lv-new-row-id = ? then return.

  if not avail oe-ord and lv-new-row-id <> ? then 
      find oe-ord where rowid(oe-ord) = lv-new-row-id no-lock no-error.*/

  RUN hide-comm (NO).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT v-slow-ord AND NOT adm-new-record THEN DISABLE oe-ord.sold-id.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.
    IF (AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Corrware" AND
        sys-ctrl.log-fld EQ NO)                            OR
       NOT adm-new-record                                  THEN
      DISABLE oe-ord.est-no oe-ord.job-no oe-ord.job-no2.

    IF NOT oedate-log THEN DISABLE oe-ord.ord-date.

    DISABLE oe-ord.ord-no.

    lv-old-cust-no = oe-ord.cust-no:SCREEN-VALUE.

    ENABLE fi_type fi_prev_order.
    IF NOT job#-log THEN DISABLE oe-ord.job-no oe-ord.job-no2.
    IF adm-new-record THEN
        ENABLE oe-ord.due-date.
    ELSE
        IF l-update-reason-perms THEN 
            ENABLE oe-ord.due-date.
        ELSE
            DISABLE oe-ord.due-date. 


/*     RUN process-status(""). */
    /* If status is hold, enable status type, else disable status type. */
    ASSIGN oe-ord.spare-char-2:SENSITIVE = (IF oe-ord.stat:SCREEN-VALUE = "H" THEN TRUE ELSE FALSE).
  END.

  ASSIGN
   ll-est-no-mod  = NO
   ll-valid-po-no = NO.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-est-no AS cha NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER bf-oe-ord FOR oe-ord.

  /* Code placed here will execute PRIOR to standard behavior. */
  ll-is-new-rec = adm-new-record.

  IF NOT ll-ord-from-est-run THEN
      ASSIGN ll-is-new-rec = NO.

  /* ==== validation ========*/  
  DO WITH FRAME {&frame-name}:
     RUN valid-cust-user("est") NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-cust-user("") NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN check-use-1 NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN check-use-2 NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     IF ll-est-no-mod AND oe-ord.est-no:SCREEN-VALUE NE "" THEN DO:
       RUN get-from-est.
       IF RETURN-VALUE NE "" THEN RETURN NO-APPLY.
     END.

     RUN valid-ord-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-type NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-est-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-job-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF adm-new-record THEN
     RUN valid-job-no2 NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-cust-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-po-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-tax-gr NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-due-date.
     IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.
     {&methods/lValidateError.i YES}
     IF oe-ord.carrier:screen-value <> "" AND
        NOT CAN-FIND(FIRST carrier WHERE carrier.company = g_company AND
                                  carrier.loc = g_loc AND
                                  carrier.carrier = oe-ord.carrier:screen-value)
     THEN DO:                              
         MESSAGE "Invalid Carrier. Try help. " VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.                                
     END.       
     IF oe-ord.terms:screen-value <> "" AND
        NOT CAN-FIND(FIRST terms WHERE terms.t-code = oe-ord.terms:screen-value)
     THEN DO:
        MESSAGE "Invalid Terms Code. Try help. " VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.
     IF oe-ord.due-code:screen-value <> "" AND
        lookup(oe-ord.due-code:screen-value,v-duelist) = 0 THEN 
     DO:
        MESSAGE "Invalid Due Code. Try help. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-ord.due-code.
        RETURN NO-APPLY.
     END.

     IF oe-ord.spare-char-2:SCREEN-VALUE <> "" AND oe-ord.stat:SCREEN-VALUE = "H"  THEN DO:  /* task 08011408 */
        IF LOOKUP(oe-ord.spare-char-2:SCREEN-VALUE,gcOrdStatList) = 0  THEN DO:
          MESSAGE "Invalid Type. Try help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO oe-ord.spare-char-2 .
         RETURN NO-APPLY.
        END.
    END.
     /*
     if oe-ord.ord-date:modified and date(oe-ord.ord-date:screen-value) < today then do:
        message "Order Date can not be earlier than TODAY." view-as alert-box error.
        apply "entry" to oe-ord.ord-date.
        return no-apply.
     end.
     */
     IF oe-ord.last-date:modified AND date(oe-ord.last-date:screen-value) < TODAY THEN DO:
        MESSAGE "Last Ship Date can not be earlier than TODAY." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-ord.last-date.
        RETURN NO-APPLY.
     END.
     IF oe-ord.prod-date:modified AND date(oe-ord.prod-date:screen-value) < TODAY THEN DO:
        MESSAGE "Prod. Date can not be earlier than TODAY." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-ord.prod-date.
        RETURN NO-APPLY.
     END.
     IF oe-ord.due-date:modified AND date(oe-ord.due-date:screen-value) < date(oe-ord.ord-date:screen-value) THEN DO:
        MESSAGE "Due Date can not be earlier than Order Date(" oe-ord.due-date:SCREEN-VALUE ")." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-ord.due-date.
        RETURN NO-APPLY.
     END.
     {&methods/lValidateError.i NO}
  END.  /* frame {&frame-name} */

  RUN valid-sman (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  /* ==== end of validation ===*/

  ASSIGN
   lv-rowid   = ROWID(oe-ord)
   ll-new-po  = NO
   ll-new-due = NO.

  DO WITH FRAME {&FRAME-NAME}:
    IF DATE(oe-ord.due-date:SCREEN-VALUE) GT
       DATE(oe-ord.last-date:SCREEN-VALUE)THEN  
      oe-ord.last-date:SCREEN-VALUE = oe-ord.due-date:SCREEN-VALUE.
  END.

  FOR EACH old-oe-ord:
    DELETE old-oe-ord.
  END.
  CREATE old-oe-ord.
  BUFFER-COPY oe-ord TO old-oe-ord.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* If hold status needs to be changed on line items,
     process the line item update in oe/ordholdstat.i. */
  IF glStatTypeItemUpdate = YES THEN
      RUN os-Update-Line-Items (INPUT oe-ord.company, 
                                INPUT oe-ord.ord-no,
                                INPUT oe-ord.spare-char-2).

  /* Code placed here will execute AFTER standard behavior.    */
  /* ===  don't go item page yet. -> move page to 2 */

  IF ll-is-new-rec THEN DO:
    IF oe-ord.est-no EQ "" AND AVAIL(oe-ord) THEN DO:
      FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord) EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bf-oe-ord THEN
        bf-oe-ord.f-bill = oe-ord.frt-pay EQ "B".
      FIND CURRENT bf-oe-ord NO-LOCK.
    END.

    ELSE DO:
        /* Need to allow update-record to complete before this extensive */
        /* order from est logic to avoid holding records                 */
        /* RUN order-from-est (YES). */
        llCreateFromEst = TRUE.

    END.

    RUN reset-browser (lv-rowid).

    IF AVAIL oe-ord AND oe-ord.est-no EQ "" THEN DO:
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
      RUN notify IN WIDGET-HANDLE(char-hdl) ('row-available').   
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
      llAutoAddItems = TRUE. /* To be checked in the addItems procedure */
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
      RUN notify IN WIDGET-HANDLE(char-hdl) ('row-available'). 
    END.
    ll-ord-from-est-run = NO.
  END.

  ELSE DO:

    BUFFER-COMPARE oe-ord TO old-oe-ord SAVE RESULT IN ll.
    IF ll-new-po OR ll-new-due OR NOT ll THEN DO:
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
      RUN repo-query1 IN WIDGET-HANDLE(char-hdl) (ROWID(oe-ord)). 
    END.
  END.
  ASSIGN
   ll-from-tandem    = NO.

  RELEASE bfx-ord.  

  RUN release-shared-buffers.

  RUN disable-fields.
  SESSION:SET-WAIT-STATE('').

  IF NOT ll-is-new-rec THEN
  RUN oe/sman-upd.p (ROWID(oe-ord)).

  IF copyRecord THEN DO:
    MESSAGE 'update' VIEW-AS ALERT-BOX.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cust-no V-table-Win 
PROCEDURE new-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ oe-ord.cust-no:SCREEN-VALUE
          AND INDEX("AXSE",cust.active) GT 0
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN RUN display-cust-detail (RECID(cust)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-due-date V-table-Win 
PROCEDURE new-due-date :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipOeRelRow AS ROWID NO-UNDO.

    DEF BUFFER bf-shipto FOR shipto.
    DEF BUFFER bf-oe-rel FOR oe-rel.
    DEF VAR dCalcDueDate AS DATE NO-UNDO.
    DEF VAR dCalcPromDate AS DATE NO-UNDO.
    DEFINE VARIABLE dTempDate AS DATE NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:

        FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ipOeRelRow EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL bf-oe-rel AND AVAIL oe-ord THEN 
        DO:
          bf-oe-rel.rel-date = get-colonial-rel-date(ROWID(bf-oe-rel)).
        END.
        FIND CURRENT bf-oe-rel NO-LOCK.
        RELEASE bf-oe-rel.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-sman V-table-Win 
PROCEDURE new-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR lv-sman LIKE sman.sman NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    lv-sman = IF ip-int EQ 3 THEN oe-ord.sman[3]:SCREEN-VALUE
              ELSE
              IF ip-int EQ 2 THEN oe-ord.sman[2]:SCREEN-VALUE
                             ELSE oe-ord.sman[1]:SCREEN-VALUE.

    IF lv-sman NE "" THEN DO:
      FIND FIRST sman
          WHERE sman.company EQ cocode
            AND sman.sman    EQ lv-sman
          NO-LOCK NO-ERROR.
      IF AVAIL sman THEN DO:
        IF ip-int EQ 3 THEN DO:
          oe-ord.sname[3]:SCREEN-VALUE = sman.sname.
          IF DEC(oe-ord.s-pct[3]:SCREEN-VALUE) EQ 0 THEN oe-ord.s-pct[3]:SCREEN-VALUE = "100".
          IF DEC(oe-ord.s-comm[3]:SCREEN-VALUE) EQ 0 THEN oe-ord.s-comm[3]:SCREEN-VALUE = STRING(sman.scomm).
        END.
        ELSE
        IF ip-int EQ 2 THEN DO:
          oe-ord.sname[2]:SCREEN-VALUE = sman.sname.
          IF DEC(oe-ord.s-pct[2]:SCREEN-VALUE) EQ 0 THEN oe-ord.s-pct[2]:SCREEN-VALUE = "100".
          IF DEC(oe-ord.s-comm[2]:SCREEN-VALUE) EQ 0 THEN oe-ord.s-comm[2]:SCREEN-VALUE = STRING(sman.scomm).
        END.
        ELSE DO:
          oe-ord.sname[1]:SCREEN-VALUE = sman.sname.
          IF DEC(oe-ord.s-pct[1]:SCREEN-VALUE) EQ 0 THEN oe-ord.s-pct[1]:SCREEN-VALUE = "100".
          IF DEC(oe-ord.s-comm[1]:SCREEN-VALUE) EQ 0 THEN
             ASSIGN
                oe-ord.s-comm[1]:SCREEN-VALUE = STRING(sman.scomm)
                v-margin = 0.
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-type V-table-Win 
PROCEDURE new-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fi_type:SCREEN-VALUE = CAPS(fi_type:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextOrdNo V-table-Win 
PROCEDURE nextOrdNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  Should no longer be needed
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-ord-no AS INTEGER NO-UNDO.
  DEF VAR iNextOrd AS INT NO-UNDO.

/*   FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ g_company */
/*        NO-ERROR.                                                */
/*   IF AVAIL oe-ctrl THEN DO:                                     */
/*     op-ord-no = oe-ctrl.n-ord.                                  */
/*                                                                 */
/*   END.                                                          */
/*   ELSE op-ord-no = ?.                                           */
  RUN sys/ref/asiseq.p (INPUT g_company, INPUT "order_seq", OUTPUT iNextOrd) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

op-ord-no = iNextOrd.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE notify-ctrl-o-override V-table-Win 
PROCEDURE notify-ctrl-o-override :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* This notifies local-create that user has pressed ctrl-o */
ll-ord-no-override = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE order-from-est V-table-Win 
PROCEDURE order-from-est :
/*------------------------------------------------------------------------------
  Purpose:     /* same purpose as oe/orduest.p & oe/estup.p */
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-new-ord AS LOG NO-UNDO.
DEF VAR op-apply-entry-to-est AS LOG NO-UNDO.
DEF VAR v-ord-rec AS ROWID NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR cReturnVal AS CHAR NO-UNDO.
DEF BUFFER bf-oe-ord FOR oe-ord.
IF AVAIL oe-ord THEN
    v-ord-rec = ROWID(oe-ord).
 /* {oe/ordfrest.i} */
nufile = YES.
RUN oe/ordfrest.p 
(INPUT THIS-PROCEDURE,
 INPUT        v-ord-rec,
 INPUT-OUTPUT ip-new-ord,
 INPUT-OUTPUT v-qty-mod,
 INPUT-OUTPUT v-inactive,
 INPUT-OUTPUT v-est-fg1,
 INPUT-OUTPUT lv-qty,
 INPUT-OUTPUT v-d-rel,
 INPUT-OUTPUT v-quo-price-log,
 INPUT-OUTPUT save_id,
 INPUT-OUTPUT v-job-no,
 INPUT-OUTPUT v-job-no2,
 INPUT-OUTPUT fil_id,
 INPUT-OUTPUT v-exp-limit,
 INPUT-OUTPUT v-n-ord,
 INPUT-OUTPUT nufile,
 INPUT-OUTPUT v-estord-id  /* EXTENT 10 */,
 INPUT-OUTPUT v-multord,
 INPUT-OUTPUT v-margin,
 INPUT-OUTPUT v-custype,
 INPUT-OUTPUT ld-lastship-dec,
 INPUT-OUTPUT lastship-cha,
 INPUT-OUTPUT lastship-int,
 INPUT-OUTPUT v-foamdate-log,
 INPUT-OUTPUT v-foamdate-int,
 INPUT-OUTPUT v-est-fg,
 INPUT-OUTPUT v-full-cost,
 INPUT-OUTPUT oeestcom-log,
 INPUT-OUTPUT v-oecomb-int,
 INPUT-OUTPUT ll-from-tandem,
 INPUT-OUTPUT adm-new-record,
 INPUT-OUTPUT ll-is-new-rec,
 INPUT-OUTPUT v-create-job,
 OUTPUT  op-apply-entry-to-est).

 cReturnVal = RETURN-VALUE. 
 IF cReturnVal GT "" THEN DO:
     CASE cReturnVal:
         WHEN "Inactive Cust" THEN
          MESSAGE              "   Inactive Customer" SKIP
             "   Orders May not Be Processed for Inactive Customers.  "
             VIEW-AS ALERT-BOX WARNING.
         WHEN "NO FGITEM" THEN DO:
              MESSAGE "Sorry, FG item does not exist. Order has not been approved."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ord.est-no IN FRAME {&FRAME-NAME}.
         END.
         WHEN "No Next Order" THEN
                   MESSAGE " Unable to Obtain next available Order Number. " 
                     VIEW-AS ALERT-BOX ERROR.
         WHEN  "No Control" THEN
                    MESSAGE "Company " cocode " has no Order-Control record." 
                            VIEW-AS ALERT-BOX ERROR.
         WHEN "CANCEL" THEN
                   RETURN NO-APPLY.
     END CASE.
 END.

  FIND CURRENT itemfg NO-LOCK NO-ERROR.

  /* Convert recid to rowid */
  FIND bf-oe-ord WHERE RECID(bf-oe-ord) EQ fil_id NO-LOCK.
  IF AVAIL bf-oe-ord THEN
      lv-rowid = ROWID(bf-oe-ord).


  RUN release-shared-buffers.

  adm-new-record = NO.
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

  RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (lv-rowid).
  /* RUN dispatch ('open-query'). */

  RUN dispatch ('row-changed').

  /* wfk - viewer was not updating, must have been in update mode */
  RUN local-initialize.
  RUN dispatch ('row-changed').    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-del-validate V-table-Win 
PROCEDURE pre-del-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&methods/lValidateError.i YES}
IF CAN-FIND(FIRST ar-invl
    WHERE ar-invl.company EQ oe-ord.company
    AND ar-invl.ord-no  EQ oe-ord.ord-no) THEN DO:

  MESSAGE "Order has been Invoice, no deletion allowed..."
          VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.

END.



IF (oe-ord.posted AND oe-ord.stat NE "H" ) OR
INDEX("CZ",oe-ord.stat) NE 0            THEN DO:
  IF oe-ord.stat = "H" THEN 
     MESSAGE "Order already posted. No deletion allowed." VIEW-AS ALERT-BOX ERROR.
  ELSE 
     MESSAGE "Order has been closed. No changes allowed"
              VIEW-AS ALERT-BOX ERROR.

  RETURN ERROR.
END.



IF oe-ord.job-no NE "" THEN DO:
  FIND FIRST job
      WHERE job.company EQ oe-ord.company
          AND job.job-no  EQ oe-ord.job-no
          AND job.job-no2 EQ oe-ord.job-no2
          AND (job.stat EQ "C" OR job.stat EQ "W" OR job.stat EQ "Z")
      USE-INDEX job-no NO-LOCK NO-ERROR.


  IF AVAIL job THEN DO:
    MESSAGE "Order cannot be Deleted, Job has been Processed or Closed."
            "You Must Close the Order."
            VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.

END.



FIND FIRST oe-ordl OF oe-ord
    WHERE oe-ordl.rel-stat
        OR oe-ordl.t-inv-qty NE 0
        OR oe-ordl.t-ship-qty NE 0
    NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN DO:
  MESSAGE "Previous Quantities have been released for this Order." SKIP
          VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.



FIND FIRST oe-ordl OF oe-ord WHERE oe-ordl.po-no-po <> 0 NO-LOCK NO-ERROR.
IF AVAIL oe-ordl  THEN DO:

    MESSAGE "Cannot delete, purchase order for board exists." VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.

END.



FOR EACH oe-ordl OF oe-ord NO-LOCK:
    IF oe-ordl.job-no GT ""  THEN DO:
        RELEASE fg-rcpth.
        FIND FIRST fg-rcpts USE-INDEX cust-no
            WHERE fg-rcpts.company EQ oe-ord.company
                AND fg-rcpts.cust-no EQ oe-ord.cust-no
                AND fg-rcpts.job-no  EQ FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                TRIM(oe-ordl.job-no)
                AND fg-rcpts.job-no2 EQ oe-ordl.job-no2
                AND fg-rcpts.i-no    EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.

        IF NOT AVAIL fg-rcpts THEN
        FIND FIRST fg-rcpth USE-INDEX job
            WHERE fg-rcpth.company EQ oe-ord.company
                AND fg-rcpth.job-no  EQ FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                TRIM(oe-ordl.job-no)
                AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                AND fg-rcpth.i-no    EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.


        IF (AVAIL fg-rcpts OR AVAIL fg-rcpth) AND
          oe-ordl.job-no NE ""  THEN DO:


            MESSAGE "Finished Goods Transactions Exists For This Order. "
                    "Deleting Is Not Permitted!  You Must Close The Order."
                     VIEW-AS ALERT-BOX ERROR.
              RETURN ERROR.        
        END. /* if aval fg-rcpts... */
    END.

END. /* Each oe-ordl */

IF NOT adm-new-record THEN DO:
  MESSAGE "Delete Order" STRING(oe-ord.ord-no) "for" STRING(oe-ord.cust-no) "?" VIEW-AS ALERT-BOX QUESTION
  BUTTON YES-NO UPDATE ll-ans AS LOG.

  IF NOT ll-ans THEN RETURN ERROR.

  RUN check-use-1 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN "ADM-ERROR".

  RUN check-use-2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN "ADM-ERROR".
END.

{&methods/lValidateError.i NO}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-status V-table-Win 
PROCEDURE process-status :
/*------------------------------------------------------------------------------
  Purpose:     Set order status and enable status type.
  Parameters:  status
  Notes:       Status button will send 'set' parameter.
               local-enable-fields will send blank parameter.
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcStatus AS CHAR NO-UNDO.

/*    /* Ignore this functionality if not in update mode. */                                               */
/*    RUN GET-ATTRIBUTE ("FIELDS-ENABLED":U).                                                              */
/*    IF RETURN-VALUE = "NO" THEN RETURN.  /* not in update mode */                                        */
/*                                                                                                         */
/*   DO WITH FRAME {&FRAME-NAME}:                                                                          */
/*                                                                                                         */
/*       /* If status is NOT hold, then process hold. */                                                   */
/*       IF pcStatus = "set" AND oe-ord.stat:SCREEN-VALUE <> "H" THEN                                      */
/*           ASSIGN oe-ord.stat:SCREEN-VALUE = "H".                                                        */
/*                                                                                                         */
/*       /* If status is hold, enable status type, else disable status type. */                            */
/*       ASSIGN oe-ord.spare-char-2:SENSITIVE = (IF oe-ord.stat:SCREEN-VALUE = "H" THEN TRUE ELSE FALSE).  */
/*   END.                                                                                                  */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE promptForOrder V-table-Win 
PROCEDURE promptForOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER opiNewOrder AS INT NO-UNDO.
DEF VAR lcParams AS CHAR.

DEF VAR liOrderNo AS INT NO-UNDO.
DEF VAR choice AS CHAR NO-UNDO.
DEF VAR op-values AS CHAR NO-UNDO.
DEF VAR lValid AS LOG NO-UNDO.
DEF VAR i AS INT.
DEF VAR ip-parms AS CHAR.
DEF VAR lcUserPrompt AS CHAR INIT "Enter the order number to use:".

DEF VAR lcNewOrder AS CHAR.

ip-parms = 
   "type=literal,name=fi4,row=4,col=18,enable=false,width=58,scrval=" + lcUserPrompt + ",FORMAT=X(58)"
    + "|type=fill-in,name=tg2,row=5,col=18,enable=true,width=10"
    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
    + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=11".
prompt-loop:
DO WHILE TRUE:
    RUN custom/d-prompt.w (INPUT "", ip-parms, "", OUTPUT op-values).

    DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
        IF ENTRY(i, op-values) EQ "default" THEN
          choice = ENTRY(i + 1, op-values) NO-ERROR.
        IF ENTRY(i, op-values) EQ "tg2" THEN
          lcNewOrder = ENTRY(i + 1, op-values) NO-ERROR.            
    END.

    IF choice NE "CANCEL" THEN DO:
        liOrderNo = INTEGER(lcNewOrder) NO-ERROR.
        RUN valid-entered-ord-no (INPUT liOrderNo, OUTPUT lValid).
        IF NOT lValid THEN
            NEXT prompt-loop.
        opiNewOrder = liOrderNo.
    END.
    LEAVE.
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

  RELEASE xoe-ord.
  RELEASE xest.
  RELEASE xef.
  RELEASE xeb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-browser V-table-Win 
PROCEDURE reset-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF VAR char-hdl AS cha NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN record-added IN WIDGET-HANDLE(char-hdl).
  RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (ip-rowid).

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
  {src/adm/template/snd-list.i "oe-ord"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ord-no V-table-Win 
PROCEDURE update-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    READKEY PAUSE 0.
    IF adm-new-record                AND
       oe-ord.ord-no:SENSITIVE EQ NO THEN DO:
      oe-ord.ord-no:SENSITIVE = YES.
      APPLY "entry" TO oe-ord.ord-no.
      ll-ord-no-override = TRUE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-start-date V-table-Win 
PROCEDURE update-start-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-update-job-stdate AS LOG  NO-UNDO.
 DEF VAR lv-prom-date AS DATE NO-UNDO.
 DEFINE VARIABLE v-run-schedule AS LOGICAL NO-UNDO.
 IF NOT avail(oe-ordl) THEN RETURN.
 IF oe-ordl.job-no = "" THEN RETURN.

   DEF BUFFER bx-ordl FOR oe-ordl.
   DEF VAR lv-first-due-date AS DATE NO-UNDO.
   lv-first-due-date = oe-ordl.req-date.

  FOR EACH bx-ordl WHERE bx-ordl.company = oe-ordl.company
                      AND bx-ordl.job-no = oe-ordl.job-no
                      AND bx-ordl.job-no2 = oe-ordl.job-no2 
                      AND RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
       lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
                           ELSE lv-first-due-date.
  END.

  DEF BUFFER bf-hdr FOR job-hdr.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bf-job FOR job.
  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-m-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-job-time  AS INT NO-UNDO.
  DEF VAR lv-maccum-time AS INT NO-UNDO.
  DEF VAR lv-job-hr AS INT NO-UNDO.
  DEF VAR lv-job-day AS INT NO-UNDO.
  DEF VAR lv-wrk-st-time AS INT NO-UNDO.
  DEF VAR lv-chk-date AS DATE NO-UNDO.
  DEF VAR li-num-of-wkend AS INT NO-UNDO.
  DEF VAR lv-start-date-fr AS DATE NO-UNDO.

  /*===  calculate start date from due-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0.

  FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no 
                    AND bf-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK,
      EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                        AND bf-mch.job-no = bf-hdr.job-no
                        AND bf-mch.job-no2 = bf-hdr.job-no2 NO-LOCK:
          ASSIGN
             lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                             TRUNCATE(bf-mch.mr-hr,0) * 3600 +
                           ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
             lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                             TRUNCATE(bf-mch.run-hr,0) * 3600 +
                           ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
             lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
  END.

  ASSIGN
     lv-job-hr = IF lv-job-time MOD 3600 > 0 THEN TRUNCATE(lv-job-time / 3600,0) + 1
                 ELSE TRUNCATE(lv-job-time / 3600,0)
     lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN truncate(lv-job-hr / 8,0) + 1
                  ELSE TRUNCATE(lv-job-hr / 8,0)
     lv-start-date = lv-first-due-date - lv-job-day. /*- 1. */

  /*  get from mach-calendar 
  lv-chk-date = lv-start-date.
  li-num-of-wkend = 0.
  DO i = 1 TO lv-first-due-date - lv-start-date:
     IF WEEKDAY(lv-chk-date) = 1 OR WEEKDAY(lv-chk-date) = 7 THEN li-num-of-wkend = li-num-of-wkend + 1.
     lv-chk-date = lv-chk-date + 1.
  END.
  lv-start-date = lv-start-date - li-num-of-wkend.
  */
  FIND bx-ordl WHERE RECID(bx-ordl) = RECID(oe-ordl).
  lv-prom-date = TODAY + lv-job-day.
  IF lv-start-date < TODAY  /* ip-type = "Update-2" is from v-ord.w*/
  THEN DO:
     lv-update-job-stdate = NO.
     /*MESSAGE "JOB CANNOT BE COMPLETED BEFORE REQUESTED DUE DATE DUE TO TOTAL MACHINE HOURS."
         SKIP
         "PROMISED DATE WILL BE   " lv-prom-date SKIP
         "UPDATE JOB's START DATE & DUE DATE?" UPDATE lv-update-job-stdate
            VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
    */
     MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
             "Due Date is before Calculates Promised Date. Update Due Date?" UPDATE lv-update-job-stdate
             VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
     /*IF lv-update-job-stdate THEN .
     ELSE DO:
         bx-ordl.prom-date = lv-prom-date.           
         return.
     END. */
     lv-start-date = TODAY.
  END.

  v-run-schedule = NOT CAN-FIND(FIRST sys-ctrl
                                WHERE sys-ctrl.company EQ oe-ord.company
                                  AND sys-ctrl.name EQ 'SCHEDULE'
                                  AND sys-ctrl.char-fld EQ 'NoDate'
                                  AND sys-ctrl.log-fld EQ YES).
  IF v-run-schedule THEN DO: /* run if above does not exist */

  /* === reset start-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0
         li-num-of-wkend = 0.

  FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no
                    AND bf-hdr.job-no2 = oe-ordl.job-no2,
      EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                    AND bf-mch.job-no = bf-hdr.job-no
                    AND bf-mch.job-no2 = bf-hdr.job-no2
                    AND NOT bf-mch.anchored
               BREAK BY bf-mch.frm BY bf-mch.blank-no BY bf-mch.pass BY bf-mch.m-code:

          FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                            AND mach-calendar.m-code = bf-mch.m-code
                            AND mach-calendar.m-date = lv-start-date
                            NO-LOCK NO-ERROR.
          lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                      ELSE 28800. /* 8 HRs*/
          IF lv-m-time LT 0 THEN lv-m-time = 28800.
          lv-maccum-time = lv-maccum-time + lv-m-time.
          IF FIRST(bf-mch.frm) THEN DO:
             FIND FIRST bf-job OF bf-hdr.
             ASSIGN
                bf-job.start-date = lv-start-date
                lv-wrk-st-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
          END.
          IF FIRST-OF(bf-mch.frm) THEN
                bf-hdr.start-date = job.start-date.

          ASSIGN
          lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                      TRUNCATE(bf-mch.mr-hr,0) * 3600 +
                    ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
          lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                      TRUNCATE(bf-mch.run-hr,0) * 3600 +
                    ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
          bf-mch.seq-no = 0                 
          bf-mch.start-time-su = lv-wrk-st-time
          bf-mch.start-time = lv-wrk-st-time + lv-mr-time
          bf-mch.start-date-su = lv-start-date
          lv-start-date-fr = lv-start-date
          lv-job-time = lv-job-time + lv-mr-time
          lv-start-date = lv-start-date + 
                          IF lv-mr-time > lv-m-time AND
                             lv-mr-time MOD lv-m-time > 0 THEN TRUNCATE(lv-mr-time / lv-m-time,0) 
                          ELSE IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0) - 1
                          ELSE 0
          lv-start-date-fr = lv-start-date.
          IF lv-m-time <> lv-maccum-time THEN DO:
             lv-start-date = lv-start-date + 
                          IF lv-job-time > lv-maccum-time AND
                             lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                          ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                          ELSE 0.
          END.
          ASSIGN
          lv-start-date-fr = lv-start-date
          bf-mch.end-date-su = lv-start-date
          bf-mch.start-date = lv-start-date
          lv-job-time = lv-job-time + lv-run-time
          lv-start-date = lv-start-date + 
                          IF lv-run-time > lv-m-time AND
                             lv-run-time MOD lv-m-time > 0 THEN TRUNCATE(lv-run-time / lv-m-time,0) 
                          ELSE IF lv-run-time > lv-m-time THEN TRUNCATE(lv-run-time / lv-m-time,0) - 1
                          ELSE 0
          lv-start-date-fr = lv-start-date.

          IF lv-m-time <> lv-maccum-time THEN
             lv-start-date = lv-start-date + 
                          IF lv-job-time > lv-maccum-time AND
                             lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                          ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                          ELSE 0.

          ASSIGN bf-mch.end-time = bf-mch.start-time + lv-run-time
                 bf-mch.end-time-su = bf-mch.start-time-su + lv-mr-time
                 bf-mch.end-date = lv-start-date           
                 lv-wrk-st-time = lv-wrk-st-time + lv-mr-time + lv-run-time.
  END.
  END. /* if v-run-schedule*/

  ASSIGN
     bx-ordl.prom-date = lv-prom-date
     bx-ordl.req-date = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.req-date.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateOrdNo V-table-Win 
PROCEDURE updateOrdNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Should not be needed since now using a sequence
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-ord-no LIKE oe-ord.ord-no NO-UNDO.
  DEF VAR iCurOrd AS INT NO-UNDO.
/*   DEF VAR li AS INT NO-UNDO.                                                               */
/*                                                                                            */
/*                                                                                            */
/*   RELEASE oe-ctrl.                                                                         */
/*                                                                                            */
/*   DO WHILE NOT AVAIL oe-ctrl AND i LT 2000:                                                */
/*     li = li + 1.                                                                           */
/*                                                                                            */
/*     FIND FIRST oe-ctrl EXCLUSIVE-LOCK WHERE oe-ctrl.company EQ g_company NO-WAIT NO-ERROR. */
/*                                                                                            */
/*     IF AVAIL oe-ctrl THEN DO:                                                              */
/*       oe-ctrl.n-ord = io-ord-no + 1.                                                       */
/*       FIND CURRENT oe-ctrl NO-LOCK NO-ERROR.                                               */
/*     END.                                                                                   */
/*                                                                                            */
/*     ELSE                                                                                   */
/*     IF li GE 2000 THEN io-ord-no = ?.                                                      */
/*   END.                                                                                     */
/*                                                                                            */
/*   RELEASE oe-ctrl.                                                                         */
  RUN sys/ref/asicurseq.p (INPUT g_company, INPUT "order_seq", OUTPUT iCurOrd) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

io-ord-no = iCurOrd.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no V-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG INIT NO NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF lv-old-cust-no NE oe-ord.cust-no:SCREEN-VALUE THEN RUN new-cust-no.

    IF oe-ord.cust-no:SCREEN-VALUE EQ "" THEN
      MESSAGE TRIM(oe-ord.cust-no:LABEL) + " must not be blank..."
          VIEW-AS ALERT-BOX ERROR.

    ELSE
    DO li = 1 TO 2:
      ll = CAN-FIND(FIRST cust WHERE cust.company EQ cocode
                                 AND cust.cust-no EQ oe-ord.cust-no:SCREEN-VALUE
                                 AND INDEX("AXSE",cust.active) GT 0).
      IF ll THEN DO:
        IF li EQ 2 THEN RUN new-cust-no.
        LEAVE.
      END.

      ELSE
      IF li EQ 1 THEN DO:
        MESSAGE "This customer does not exist, would you like to add it?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
        IF ll THEN RUN est/custfly.w (oe-ord.cust-no:SCREEN-VALUE).
        ELSE LEAVE.
      END.
    END.

    IF NOT ll THEN DO:
      APPLY "entry" TO oe-ord.cust-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-user V-table-Win 
PROCEDURE valid-cust-user :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ip-est AS CHAR NO-UNDO .
    DEF VAR v-cust-chk AS CHAR NO-UNDO .
    DEF VAR v-est-no AS CHAR NO-UNDO .
  {methods/lValidateError.i YES}
custcount = "".
DEF VAR lActive AS LOG NO-UNDO.
RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'OU1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""OU1""}
    DO WITH FRAME {&FRAME-NAME}:

        IF ip-est = "est" THEN DO:
            v-est-no = FILL(" ",8 - LENGTH(TRIM(oe-ord.est-no:SCREEN-VALUE))) +
                                            TRIM(oe-ord.est-no:SCREEN-VALUE) .
            FIND FIRST eb
                WHERE eb.company EQ g_company
                AND eb.est-no  EQ v-est-no
                NO-LOCK NO-ERROR.
            IF AVAIL eb THEN
                v-cust-chk = eb.cust-no .
            ELSE v-cust-chk = "".
        END.
        ELSE DO:
            v-cust-chk = oe-ord.cust-no:SCREEN-VALUE .
        END.
    END.

  IF ou-log THEN
    DO WITH FRAME {&FRAME-NAME}:
      IF LOOKUP(v-cust-chk,custcount) = 0 THEN DO:
          MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX ERROR.
          IF ip-est = "est" THEN 
              APPLY "entry" TO oe-ord.est-no .
          ELSE
              APPLY "entry" TO oe-ord.cust-no .
          RETURN ERROR.
      END.
    END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-due-date V-table-Win 
PROCEDURE valid-due-date :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValid    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE ldDate    AS DATE    NO-UNDO.
  {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:

        ASSIGN 
            lValid    = YES
            lContinue = YES
            ldDate    = DATE(oe-ord.due-date:screen-value).

        RUN oe/dateFuture.p (INPUT cocode, INPUT ldDate, INPUT YES /* prompt */, OUTPUT lValid, OUTPUT lContinue).
        IF NOT lValid AND  NOT lContinue THEN 
        DO:      
            APPLY "entry" TO oe-ord.ord-no.
            RETURN ERROR.
        END.

    END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-entered-ord-no V-table-Win 
PROCEDURE valid-entered-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipiOrdNo AS INT NO-UNDO.  
  DEF OUTPUT PARAMETER oplGoodOrder AS LOG NO-UNDO.
  DEF BUFFER b-oe-ord FOR oe-ord.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF ipiOrdNo EQ 0 OR
       CAN-FIND(FIRST b-oe-ord
                WHERE b-oe-ord.company EQ cocode
                  AND b-oe-ord.ord-no  EQ ipiOrdNo
                  AND ROWID(b-oe-ord)  NE ROWID(oe-ord))

    THEN DO:

      MESSAGE 
              "Order Number is zero or invalid, please re-enter..."          
          VIEW-AS ALERT-BOX ERROR.
      oplGoodOrder = FALSE.

    END.
    ELSE
        oplGoodOrder = TRUE.

  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no V-table-Win 
PROCEDURE valid-est-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
 /* IF adm-new-record THEN*/
 DO WITH FRAME {&FRAME-NAME}:
    IF oe-ord.est-no:SCREEN-VALUE NE "" THEN DO:
      FIND FIRST est
          WHERE est.company EQ g_company
            AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(oe-ord.est-no:SCREEN-VALUE))) +
                               TRIM(oe-ord.est-no:SCREEN-VALUE)
          NO-LOCK NO-ERROR.
      IF NOT AVAIL est THEN DO:
        MESSAGE "Invalid Estimate#, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-ord.est-no.
        RETURN ERROR.
      END.

      IF v-quo-price-log AND v-quo-price-dec EQ 1 THEN DO:
        FOR EACH quotehd
            WHERE quotehd.company EQ est.company
              AND quotehd.loc     EQ est.loc
              AND quotehd.est-no  EQ est.est-no
             NO-LOCK,

             EACH quoteitm OF quotehd NO-LOCK,

             EACH quoteqty OF quoteitm NO-LOCK:
          LEAVE.
        END.

        IF NOT AVAIL quoteqty THEN DO:
          MESSAGE "No quotes exists for this estimate..."
              VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO oe-ord.est-no.
          RETURN ERROR.
        END.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no V-table-Win 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    oe-ord.job-no:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(oe-ord.job-no:SCREEN-VALUE))) +
                                 TRIM(oe-ord.job-no:SCREEN-VALUE).
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 V-table-Win 
PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF oe-ord.est-no:SCREEN-VALUE NE "" AND
       oe-ord.job-no:SCREEN-VALUE NE "" AND
       oe-ord.job-no:SENSITIVE          AND
       CAN-FIND(FIRST job-hdr
                WHERE job-hdr.company EQ cocode
                  AND job-hdr.job-no  EQ oe-ord.job-no:SCREEN-VALUE
                  AND job-hdr.job-no2 EQ INT(oe-ord.job-no2:SCREEN-VALUE)) THEN DO:
      MESSAGE "Sorry, job# already exists..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-ord.job-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-no V-table-Win 
PROCEDURE valid-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ord FOR oe-ord.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF INT(oe-ord.ord-no:SCREEN-VALUE) EQ 0 OR
       CAN-FIND(FIRST b-oe-ord
                WHERE b-oe-ord.company EQ cocode
                  AND b-oe-ord.ord-no  EQ INT(oe-ord.ord-no:SCREEN-VALUE)
                  AND ROWID(b-oe-ord)  NE ROWID(oe-ord))
    THEN DO:
      MESSAGE TRIM(oe-ord.ord-no:LABEL) +
              " is zero or invalid, please re-enter..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-ord.ord-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no V-table-Win 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ oe-ord.company
          AND cust.cust-no EQ oe-ord.cust-no:SCREEN-VALUE
          AND cust.po-mandatory
        NO-ERROR.

    IF AVAIL cust AND TRIM(oe-ord.po-no:SCREEN-VALUE) EQ "" THEN DO:
      MESSAGE "PO# is mandatory for this Customer..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-ord.po-no.
      RETURN ERROR.
    END.

    IF NOT ll-valid-po-no AND oeprompt AND oe-ord.po-no:SCREEN-VALUE NE "" THEN
    FIND FIRST b-oe-ordl
        WHERE b-oe-ordl.company EQ oe-ord.company
          AND b-oe-ordl.po-no   EQ oe-ord.po-no:SCREEN-VALUE
          AND b-oe-ordl.cust-no EQ oe-ord.cust-no:SCREEN-VALUE
          AND b-oe-ordl.ord-no  NE INT(oe-ord.ord-no:SCREEN-VALUE)
        NO-LOCK NO-ERROR.

    IF AVAIL b-oe-ordl THEN DO:
      MESSAGE "Customer PO already exists for Order/Item - " + 
              TRIM(STRING(b-oe-ordl.ord-no,">>>>>>>>")) + "/" +
              TRIM(b-oe-ordl.i-no) " ." SKIP
              "Do you want to continue?"
          VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
      IF NOT ll-ans THEN DO:
        APPLY "entry" TO oe-ord.po-no.
        RETURN ERROR.
      END.
      ELSE ll-valid-po-no = YES.
    END.
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
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-sman LIKE sman.sman NO-UNDO.


  {methods/lValidateError.i YES}
  li = ip-int.

  IF li EQ 0 THEN
    ASSIGN
     ip-int = 1
     li     = 3.

  DO ip-int = ip-int TO li WITH FRAME {&FRAME-NAME}:
    lv-sman = IF ip-int EQ 3 THEN oe-ord.sman[3]:SCREEN-VALUE
              ELSE
              IF ip-int EQ 2 THEN oe-ord.sman[2]:SCREEN-VALUE
                             ELSE oe-ord.sman[1]:SCREEN-VALUE.

    IF lv-sman NE "" THEN DO:
        FIND FIRST sman NO-LOCK WHERE sman.company EQ cocode
                                  AND sman.sman    EQ lv-sman NO-ERROR.
        IF NOT AVAILABLE sman THEN DO:
          MESSAGE "Invalid Sales Rep, try help..." VIEW-AS ALERT-BOX ERROR.
          IF ip-int EQ 3 THEN APPLY "entry" TO oe-ord.sman[3].
          ELSE
          IF ip-int EQ 2 THEN APPLY "entry" TO oe-ord.sman[2].
                         ELSE APPLY "entry" TO oe-ord.sman[1].
          RETURN ERROR.
        END.
        ELSE DO:
          IF ip-int EQ 3 THEN oe-ord.sname[3]:SCREEN-VALUE = sman.sname.
          ELSE
          IF ip-int EQ 2 THEN oe-ord.sname[2]:SCREEN-VALUE = sman.sname.
                         ELSE oe-ord.sname[1]:SCREEN-VALUE = sman.sname.
        END.
    END.
    ELSE DO:
      IF ip-int EQ 3 THEN
        ASSIGN
         oe-ord.s-pct[3]:SCREEN-VALUE  = "0"
         oe-ord.s-comm[3]:SCREEN-VALUE = "0".
      ELSE
      IF ip-int EQ 2 THEN
        ASSIGN
         oe-ord.s-pct[2]:SCREEN-VALUE  = "0"
         oe-ord.s-comm[2]:SCREEN-VALUE = "0".
      ELSE
        ASSIGN
         oe-ord.s-pct[1]:SCREEN-VALUE  = "0"
         oe-ord.s-comm[1]:SCREEN-VALUE = "0"
         v-margin = 0.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax-gr V-table-Win 
PROCEDURE valid-tax-gr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF oe-ord.tax-gr:SCREEN-VALUE NE "" AND
       NOT CAN-FIND(FIRST stax
                    WHERE stax.company   EQ cocode
                      AND stax.tax-group EQ oe-ord.tax-gr:SCREEN-VALUE)
    THEN DO:
      MESSAGE TRIM(oe-ord.tax-gr:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-ord.tax-gr.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-type V-table-Win 
PROCEDURE valid-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF LOOKUP(fi_type:SCREEN-VALUE,lv-type-codes) LE 0 OR
       (fi_type:SCREEN-VALUE EQ "T" AND
        NOT CAN-FIND(FIRST cust WHERE cust.company EQ cocode
                                  AND cust.cust-no EQ oe-ord.cust-no:SCREEN-VALUE
                                  AND cust.active  EQ "X")) THEN DO:
      MESSAGE "Invalid Type, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_type.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-colonial-rel-date V-table-Win 
FUNCTION get-colonial-rel-date RETURNS DATE
  ( iprRel AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR opRelDate AS DATE NO-UNDO.
  DEF VAR rShipTo AS ROWID NO-UNDO.
  DEF BUFFER bf-shipto FOR shipto.    
  DEF BUFFER bf-oe-ord FOR oe-ord.
  DEF BUFFER bf-oe-rel FOR oe-rel.
  opRelDate = ?.
  FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ iprRel NO-LOCK NO-ERROR.
  RUN sys/ref/shipToOfRel.p (INPUT ROWID(oe-rel), OUTPUT rShipTo).
  FIND bf-shipto WHERE ROWID(bf-shipto) EQ rShipTo NO-LOCK NO-ERROR.
  FIND FIRST bf-oe-ord WHERE bf-oe-ord.company EQ bf-oe-rel.company
     AND bf-oe-ord.ord-no EQ bf-oe-rel.ord-no
     NO-LOCK NO-ERROR.

  /* order header due-date - dock appt days, adjusted for weekends */
  IF AVAIL bf-shipto AND AVAIL(bf-oe-ord) THEN
     opRelDate = get-date(bf-oe-ord.due-date, bf-shipto.spare-int-2, "-").
  RETURN opRelDate.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

