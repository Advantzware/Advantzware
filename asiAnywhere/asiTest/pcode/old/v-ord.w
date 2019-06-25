&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
/* for oecomm.i */
def new shared var v-upd-comm as log initial yes no-undo.
DEF NEW SHARED VAR v-misc AS LOG INIT NO NO-UNDO.
DEF NEW SHARED VAR v-fr-tax LIKE oe-ctrl.f-tax NO-UNDO.
{sys/inc/var.i "new shared" }
def buffer bfx-ord for oe-ord.
def var li-lead-days as int no-undo.
def var ll-f-bill as log no-undo.
def var li-sold-no as int no-undo.
def var ls-ship-i as cha extent 4 no-undo.
def var v-slow-ord as log no-undo.
def var v-beeler as log no-undo.
def var v-ilwalker as log no-undo.
def new shared var v-create-job as log no-undo.
def var v-custype like cust.type no-undo.
def var v-ord-limit like cust.ord-lim no-undo.
def var v-crd-limit like cust.cr-lim no-undo.
def var v-valdcode as cha init "ON,BY,MH" no-undo.
def var v-valtype as cha init "O,R,C" no-undo.
def var v-duelist as cha init "ASAP,NB4,MUST,HOT,RUSH,WO,HOLD,CR,BY,ON,MH,$$$,AM,INK,OE,RWRK,TOOL,HFR" no-undo.
def var v-oecount like sys-ctrl.log-fld no-undo.
def var v-full-cost as log no-undo.
def var oeprompt like sys-ctrl.log-fld no-undo.
def var v-quo-price like sys-ctrl.log-fld no-undo.
def var v-inactive as log no-undo.
def var save_id as recid no-undo.
def new shared var fil_id as recid no-undo.
def var v-job-no like oe-ord.job-no no-undo.
def var v-job-no2 like oe-ord.job-no2 no-undo.
def var v-exp-limit as int init 10 no-undo.
def var v-n-ord like oe-ctrl.n-ord no-undo.
def var v-estord-id as recid extent 10 no-undo.
def var v-multord as log no-undo.
{ce/print4.i "new shared"}
{ce/print42.i "new shared"}
def NEW SHARED WORKFILE work-ordl LIKE oe-ordl.
def new shared var nufile as log no-undo.
def new shared buffer xoe-ord for oe-ord.
def new shared var lv-qty as int no-undo.  /* for oe-ordl.qty and oe-ordm calc */
def var lv-new-row-id as rowid no-undo.  /* first creation error */
def new shared var v-qty-mod as log no-undo.
def new shared var qty as INT no-undo.
def var ll-order-from-est as log no-undo.  /* is order created from estimate */
def var ll-cust-displayed as log no-undo.
def var ll-est-no-mod as log no-undo.
DEF VAR ld-lastship-dec AS DEC NO-UNDO.
DEF VAR ld-lastship-cha AS CHAR NO-UNDO.
DEF VAR ll-valid-po-no AS LOG NO-UNDO.
def var ll-is-new-rec as log no-undo.
DEF VAR ll-from-tandem AS LOG NO-UNDO.
DEF VAR lv-old-cust-no LIKE oe-ord.cust-no NO-UNDO.
DEF VAR ll-new-po AS LOG NO-UNDO.
DEF VAR ll-new-due AS LOG NO-UNDO.
DEF VAR lv-type-codes AS CHAR NO-UNDO.
DEF VAR lv-type-dscrs AS CHAR NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

DEFINE VARIABLE prodDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE dueDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE scheduleHndl AS HANDLE NO-UNDO.
DEFINE VARIABLE copyRecord AS LOGICAL NO-UNDO.
DEFINE VARIABLE copyRowID AS ROWID NO-UNDO.

def NEW SHARED buffer xest for est.
def NEW SHARED buffer xeb for eb.
def NEW SHARED buffer xef for ef.

DEF BUFFER oe-ord-whs-order FOR reftable.
DEF BUFFER oe-ordl-whs-item FOR reftable.

&Scoped-define sman-fields oe-ord.sman oe-ord.s-pct oe-ord.s-comm

DEF WORKFILE w-ord FIELD w-ord-no LIKE oe-ord.ord-no.

DEF WORKFILE old-oe-ord LIKE oe-ord.

DEF TEMP-TABLE tt-oe-ordl LIKE oe-ordl
    FIELD to-be-deleted AS LOG INIT YES
    FIELD row-id AS ROWID
    INDEX row-id row-id. 

ASSIGN
 cocode = g_company
 locode = g_loc.
      
{oe/oe-sysct1.i NEW}

DO TRANSACTION:
  {sys/inc/oedate.i}
  {sys/inc/oecomb.i}
  {sys/inc/job#.i}
  {sys/inc/graphic.i}
END.
{sys/inc/f16to32.i}

RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-ord
&Scoped-define FIRST-EXTERNAL-TABLE oe-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ord.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ord.ord-no oe-ord.est-no oe-ord.job-no ~
oe-ord.job-no2 oe-ord.sold-id oe-ord.ord-date oe-ord.due-code ~
oe-ord.due-date oe-ord.last-date oe-ord.prod-date oe-ord.po-no ~
oe-ord.contact oe-ord.over-pct oe-ord.under-pct oe-ord.terms oe-ord.pord-no ~
oe-ord.tax-gr oe-ord.frt-pay oe-ord.carrier oe-ord.fob-code oe-ord.sman[1] ~
oe-ord.s-pct[1] oe-ord.s-comm[1] oe-ord.sman[2] oe-ord.s-pct[2] ~
oe-ord.s-comm[2] oe-ord.sman[3] oe-ord.s-pct[3] oe-ord.s-comm[3] ~
oe-ord.cc-type oe-ord.cc-expiration oe-ord.cc-num oe-ord.cc-auth 
&Scoped-define ENABLED-TABLES oe-ord
&Scoped-define FIRST-ENABLED-TABLE oe-ord
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 btnCalendar-5 RECT-30 RECT-33 RECT-35 RECT-36 RECT-37 
&Scoped-Define DISPLAYED-FIELDS oe-ord.ord-no oe-ord.est-no oe-ord.job-no ~
oe-ord.job-no2 oe-ord.user-id oe-ord.stat oe-ord.cust-no oe-ord.sold-id ~
oe-ord.ord-date oe-ord.cust-name oe-ord.sold-name oe-ord.due-code ~
oe-ord.due-date oe-ord.addr[1] oe-ord.sold-addr[1] oe-ord.last-date ~
oe-ord.addr[2] oe-ord.sold-addr[2] oe-ord.prod-date oe-ord.city ~
oe-ord.state oe-ord.zip oe-ord.sold-city oe-ord.sold-state oe-ord.sold-zip ~
oe-ord.po-no oe-ord.contact oe-ord.over-pct oe-ord.under-pct oe-ord.terms ~
oe-ord.terms-d oe-ord.pord-no oe-ord.tax-gr oe-ord.frt-pay oe-ord.carrier ~
oe-ord.fob-code oe-ord.sman[1] oe-ord.sname[1] oe-ord.s-pct[1] ~
oe-ord.s-comm[1] oe-ord.sman[2] oe-ord.sname[2] oe-ord.s-pct[2] ~
oe-ord.s-comm[2] oe-ord.sman[3] oe-ord.sname[3] oe-ord.s-pct[3] ~
oe-ord.s-comm[3] oe-ord.cc-type oe-ord.cc-expiration oe-ord.cc-num ~
oe-ord.cc-auth 
&Scoped-define DISPLAYED-TABLES oe-ord
&Scoped-define FIRST-DISPLAYED-TABLE oe-ord
&Scoped-Define DISPLAYED-OBJECTS fi_type tb_whs-order fi_sname-lbl ~
fi_s-pct-lbl fi_s-comm-lbl fi_sman-lbl 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,calendarPopup,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS oe-ord.cust-no 
&Scoped-define ADM-ASSIGN-FIELDS fi_type oe-ord.stat oe-ord.cust-name ~
oe-ord.sold-name oe-ord.addr[1] oe-ord.sold-addr[1] oe-ord.addr[2] ~
oe-ord.sold-addr[2] oe-ord.city oe-ord.state oe-ord.zip oe-ord.sold-city ~
oe-ord.sold-state oe-ord.sold-zip oe-ord.terms-d tb_whs-order ~
oe-ord.sname[1] oe-ord.sname[2] oe-ord.sname[3] 
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-3 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-4 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-5 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

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
     SIZE 77 BY 5.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 5
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 4.52.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 4.52.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 143 BY 6.19.

DEFINE VARIABLE tb_whs-order AS LOGICAL INITIAL no 
     LABEL "Managed Inventory" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ord.ord-no AT ROW 1.24 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi_type AT ROW 1.24 COL 30 COLON-ALIGNED HELP
          "Enter Order Type (O)riginal, (R)epeat, repeat with (C)hange"
     oe-ord.est-no AT ROW 1.24 COL 48 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     oe-ord.job-no AT ROW 1.24 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.6 BY 1
     oe-ord.job-no2 AT ROW 1.24 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     oe-ord.user-id AT ROW 1.24 COL 113 COLON-ALIGNED
          LABEL "Last User"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ord.stat AT ROW 1.24 COL 137 COLON-ALIGNED
          LABEL "Status"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     oe-ord.cust-no AT ROW 2.67 COL 10 COLON-ALIGNED
          LABEL "Bill To" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.sold-id AT ROW 2.67 COL 57 COLON-ALIGNED
          LABEL "Sold To"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-ord.ord-date AT ROW 2.67 COL 118 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.cust-name AT ROW 3.86 COL 10 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     oe-ord.sold-name AT ROW 3.86 COL 57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     oe-ord.due-code AT ROW 3.86 COL 107 COLON-ALIGNED
          LABEL "Due Date" FORMAT "XXXX"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     oe-ord.due-date AT ROW 3.86 COL 118 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.addr[1] AT ROW 5.05 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     oe-ord.sold-addr[1] AT ROW 5.05 COL 57 COLON-ALIGNED
          LABEL ""
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     oe-ord.last-date AT ROW 5.05 COL 118 COLON-ALIGNED
          LABEL "Last Ship"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-ord.addr[2] AT ROW 6.24 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     oe-ord.sold-addr[2] AT ROW 6.24 COL 57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     oe-ord.prod-date AT ROW 6.24 COL 118 COLON-ALIGNED
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
          SIZE 10 BY 1
     oe-ord.sold-city AT ROW 7.43 COL 57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     oe-ord.sold-state AT ROW 7.43 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     oe-ord.sold-zip AT ROW 7.43 COL 86 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     oe-ord.po-no AT ROW 7.43 COL 118 COLON-ALIGNED
          LABEL "Cust PO#"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ord.contact AT ROW 9.1 COL 10 COLON-ALIGNED
          LABEL "Contact"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     oe-ord.over-pct AT ROW 10.29 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     oe-ord.under-pct AT ROW 11.48 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     oe-ord.terms AT ROW 12.67 COL 15 COLON-ALIGNED
          LABEL "Pay Terms"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     oe-ord.terms-d AT ROW 12.67 COL 27 COLON-ALIGNED NO-LABEL FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     oe-ord.pord-no AT ROW 9.1 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ord.tax-gr AT ROW 10.29 COL 60 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     tb_whs-order AT ROW 11.48 COL 50
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
     oe-ord.sman[1] AT ROW 15.05 COL 4 COLON-ALIGNED NO-LABEL FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.sname[1] AT ROW 15.05 COL 15 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     oe-ord.s-pct[1] AT ROW 15.05 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.s-comm[1] AT ROW 15.05 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.sman[2] AT ROW 16.24 COL 4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.sname[2] AT ROW 16.24 COL 15 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     oe-ord.s-pct[2] AT ROW 16.24 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.s-comm[2] AT ROW 16.24 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.sman[3] AT ROW 17.43 COL 4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-ord.sname[3] AT ROW 17.43 COL 15 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     oe-ord.s-pct[3] AT ROW 17.43 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.s-comm[3] AT ROW 17.43 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-ord.cc-type AT ROW 14.33 COL 97 COLON-ALIGNED
          LABEL "Payment Type"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     oe-ord.cc-expiration AT ROW 14.33 COL 120 COLON-ALIGNED
          LABEL "Expire"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ord.cc-num AT ROW 15.52 COL 97 COLON-ALIGNED
          LABEL "Account #"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     oe-ord.cc-auth AT ROW 16.71 COL 97 COLON-ALIGNED
          LABEL "Ref #"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     fi_sname-lbl AT ROW 14.33 COL 17 COLON-ALIGNED NO-LABEL
     fi_s-pct-lbl AT ROW 14.33 COL 46 COLON-ALIGNED NO-LABEL
     fi_s-comm-lbl AT ROW 14.33 COL 61 COLON-ALIGNED NO-LABEL
     fi_sman-lbl AT ROW 14.33 COL 2 COLON-ALIGNED NO-LABEL
     btnCalendar-1 AT ROW 2.67 COL 137
     btnCalendar-2 AT ROW 3.86 COL 137
     btnCalendar-3 AT ROW 5.05 COL 137
     btnCalendar-4 AT ROW 6.24 COL 137
     btnCalendar-5 AT ROW 14.33 COL 138
     "Freight Charge" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.1 COL 81
          FGCOLOR 9 
     "FOB:" VIEW-AS TEXT
          SIZE 6 BY .86 AT ROW 12.19 COL 113
     RECT-30 AT ROW 8.86 COL 1
     RECT-33 AT ROW 8.86 COL 79
     RECT-35 AT ROW 14.1 COL 79
     RECT-36 AT ROW 14.1 COL 1
     RECT-37 AT ROW 2.43 COL 1
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
   Other Settings: PERSISTENT-ONLY
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
         WIDTH              = 143.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN oe-ord.addr[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.addr[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
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
/* SETTINGS FOR FILL-IN fi_s-comm-lbl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_s-pct-lbl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sman-lbl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sname-lbl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_type IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.job-no2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.last-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.ord-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ord.po-no IN FRAME F-Main
   EXP-LABEL                                                            */
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
/* SETTINGS FOR FILL-IN oe-ord.stat IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ord.state IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ord.tax-gr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_whs-order IN FRAME F-Main
   NO-ENABLE 2                                                          */
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
ANYWHERE
DO:
  RUN update-ord-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
  def var char-val as cha no-undo.
  def var look-recid as recid no-undo.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.


  lw-focus = FOCUS.

    case lw-focus:name :
         when "est-no" then do:
              run windows/l-est.w (g_company,g_loc,oe-ord.est-no:screen-value, output char-val).
              if char-val <> "" then do:
                 find first eb where string(recid(eb)) = char-val no-lock no-error.
                 if avail eb then DO:
                   oe-ord.est-no:screen-value = eb.est-no.
                   APPLY "value-changed" TO oe-ord.est-no.
                   RUN get-from-est.
                 END.
              end.  
         end.
         WHEN "cust-no" THEN DO:
              RUN windows/l-custact.w (g_company, oe-ord.cust-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
              FIND cust WHERE RECID(cust) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL cust THEN DO:
                oe-ord.cust-no:SCREEN-VALUE = cust.cust-no.
                RUN new-cust-no.  
              END.
         END.  
         when "sold-id" then do:
              run windows/l-soldto.w (g_company, oe-ord.cust-no:screen-value,oe-ord.sold-id:screen-value, output char-val).
              if char-val <> "" then do:
                 assign /*oe-ord.sold-no:screen-value      = soldto.sold-no*/             
                        li-sold-no = int(entry(1,char-val))
                        oe-ord.sold-id:screen-value      = entry(2,char-val) 
                        oe-ord.sold-name:screen-value    = entry(3,char-val)
                        oe-ord.sold-addr[1]:screen-value = entry(4,char-val)
                        oe-ord.sold-addr[2]:screen-value = entry(5,char-val)
                        oe-ord.sold-city:screen-value    = entry(6,char-val)
                        oe-ord.sold-state:screen-value   = entry(7,char-val)
                        oe-ord.sold-zip:screen-value     = entry(8,char-val)
                        .

              end.
         end.  
         when "sman" then do:
              li = frame-index.
              run windows/l-sman.w (g_company, output char-val).
              if char-val ne "" then do:
                if li eq 1 and oe-ord.sman[1]:screen-value ne entry(1,char-val) then 
                  oe-ord.sman[1]:screen-value = entry(1,char-val).
                else
                if li eq 2 and oe-ord.sman[2]:screen-value ne entry(1,char-val) then 
                  oe-ord.sman[2]:screen-value = entry(1,char-val).
                else
                if li eq 3 and oe-ord.sman[3]:screen-value ne entry(1,char-val) then 
                  oe-ord.sman[3]:screen-value = entry(1,char-val).
                else li = 0.
                if li ne 0 then run new-sman (li).
              end.
         end.  
         when "tax-gr" then do:
              run windows/l-stax.w (g_company,oe-ord.tax-gr:screen-value, output char-val).
              if char-val <> "" then oe-ord.tax-gr:screen-value = entry(1,char-val).
         end.
         when "carrier" then do:
              run windows/l-carrie.w (g_company,g_loc,oe-ord.carrier:screen-value, output char-val).
              if char-val <> "" then oe-ord.carrier:screen-value = entry(1,char-val).
         end.
         when "terms" then do:
              run windows/l-terms.w (g_company,oe-ord.terms:screen-value, output char-val).
              if char-val <> "" AND entry(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
                oe-ord.terms:screen-value = entry(1,char-val).
                APPLY "value-changed" TO oe-ord.terms.
              END.
         end.
         when "due-code" then do:
              run windows/l-dcode.w (v-duelist, output char-val).
              if char-val <> "" then oe-ord.due-code:SCREEN-VALUE = entry(1,char-val).
                                            
         end.
         WHEN "fi_type" THEN DO:
              run windows/l-ordtyp.w (fi_type:SCREEN-VALUE, output char-val).
              if char-val <> "" then fi_type:SCREEN-VALUE = entry(1,char-val).
         END.
    end case.
    return no-apply.
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
    if lastkey = -1 then return.
    
    if oe-ord.carrier:screen-value <> "" and
       not can-find(first carrier where carrier.company = g_company and
                                  carrier.loc = g_loc and
                                  carrier.carrier = oe-ord.carrier:screen-value)
    then do:                              
         message "Invalid Carrier. Try help. " view-as alert-box error.
         return no-apply.                                
    end.       
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
    if lastkey = -1 then return.
    if oe-ord.due-code:screen-value <> "" and
       lookup(oe-ord.due-code:screen-value,v-duelist) = 0 then 
    do:
       message "Invalid Due Code. Try help. " view-as alert-box error.
       return no-apply.
    end.
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
  if lastkey = -1 then return.
  dueDateChanged = SELF:MODIFIED. /* used in proc local-assign-record */
  if self:modified and date(self:screen-value) < date(oe-ord.ord-date:screen-value) then do:
     message "Due Date can not be earlier than Order Date(" oe-ord.ord-date:SCREEN-VALUE ")." view-as alert-box error.
     return no-apply.
  end.
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
ON return OF oe-ord.fob-code IN FRAME F-Main /* FOB Code */
DO:
     apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.frt-pay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.frt-pay V-table-Win
ON return OF oe-ord.frt-pay IN FRAME F-Main /* Freight Pay Code */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ord.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.job-no V-table-Win
ON ENTRY OF oe-ord.job-no IN FRAME F-Main /* Job Number */
DO:
    if oe-ord.est-no:screen-value = "" then do:
       apply "tab" to self.
       return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ord.job-no V-table-Win
ON LEAVE OF oe-ord.job-no IN FRAME F-Main /* Job Number */
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
    if oe-ord.est-no:screen-value = "" then do:
       apply "tab" to self.
       return no-apply.
    end.
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
    if lastkey = -1 then return.
    
    if self:modified and date(self:screen-value) < today then do:
       message "Last Ship Date can not be earlier than TODAY." view-as alert-box error.
       return no-apply.
    end.

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
    if lastkey = -1 then return.
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
ON LEAVE OF oe-ord.po-no IN FRAME F-Main /* Cust PO# */
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
  if lastkey = -1 then return.
  prodDateChanged = SELF:MODIFIED. /* used in proc local-assign-record */
  if self:modified and date(self:screen-value) < today then do:
     message "Prod. Date can not be earlier than TODAY." view-as alert-box error.
     return no-apply.
  end.
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
    find first soldto where soldto.company = g_company and
                            soldto.cust-no = oe-ord.cust-no:screen-value
                        and trim(soldto.sold-id) = trim(oe-ord.sold-id:screen-value)
                        no-lock no-error.
    if avail soldto then 
       assign /*oe-ord.sold-no:screen-value      = soldto.sold-no*/             
             li-sold-no = soldto.sold-no
             oe-ord.sold-id:screen-value      = soldto.sold-id
             oe-ord.sold-name:screen-value    = soldto.sold-name
             oe-ord.sold-addr[1]:screen-value = soldto.sold-addr[1]
             oe-ord.sold-addr[2]:screen-value = soldto.sold-addr[2]
             oe-ord.sold-city:screen-value    = soldto.sold-city
             oe-ord.sold-state:screen-value   = soldto.sold-state
             oe-ord.sold-zip:screen-value     = soldto.sold-zip.

    else if oe-ord.sold-id:screen-value <> "" then do:
         message "Invalid Sold To. Try help. " view-as alert-box error.
         return no-apply.
    end.
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
    if lastkey = -1 then return.
    
    if oe-ord.terms:screen-value <> "" and
       not can-find(first terms where terms.t-code = oe-ord.terms:screen-value)
    then do:
       message "Invalid Terms Code. Try help. " view-as alert-box error.
       oe-ord.terms-d:screen-value = "".
       return no-apply.
    end.
    
    find first terms where terms.t-code = oe-ord.terms:screen-value no-lock no-error.
    if avail terms then oe-ord.terms-d:screen-value = terms.dscr.
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
  session:data-entry-return = yes.

  RUN oe/oe-sysct.p.

  {oe/oe-sysct.i}

  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "FASTOE"
       no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company  = cocode
            sys-ctrl.name     = "FASTOE"
            sys-ctrl.descrip  = "Quick Order Creation"
            sys-ctrl.char-fld = "ASI"
            sys-ctrl.log-fld  = no.
      message "System control record NOT found. " sys-ctrl.descrip
          update sys-ctrl.char-fld.
  end.
  assign v-slow-ord = not (sys-ctrl.char-fld eq "Argrov" or
                           sys-ctrl.char-fld eq "Beeler")
         v-beeler   = sys-ctrl.char-fld eq "Beeler"
         v-ilwalker = sys-ctrl.char-fld eq "ILWalker".

  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "JOBCREAT"
       no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "JOBCREAT"
            sys-ctrl.descrip = "Create Job Standards during OE?"
            sys-ctrl.log-fld = no.
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
  v-create-job = sys-ctrl.log-fld.

  find first sys-ctrl where sys-ctrl.company eq cocode
            and sys-ctrl.name    eq "OEPROMPT"
       no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "OEPROMPT"
            sys-ctrl.log-fld = yes
            sys-ctrl.descrip = "Prompt for duplicate PO?".
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-job V-table-Win 
PROCEDURE create-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output param op-recid as recid no-undo.

  DEF BUFFER v-ord-job-hdr FOR job-hdr.

  def var v-job-job like job.job no-undo.
  def var v-job-no like job.job-no no-undo.
  def var v-job-no2 like job.job-no2 no-undo.
  def var li-j-no as int no-undo.
    
  /* === from oe/oe-ord1.p  ============= */
         
  find last job where job.company eq cocode no-lock no-error.
  v-job-job = if avail job then job.job + 1 else 1.
  ASSIGN
   v-job-no  = oe-ord.job-no
   v-job-no2 = oe-ord.job-no2.

  FOR EACH job
      WHERE job.company EQ cocode
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2:
    DELETE job.
  END.

  create job.
  assign job.job        = v-job-job
         job.company    = cocode
         job.loc        = locode
         job.est-no     = oe-ord.est-no
         job.job-no     = v-job-no
         job.job-no2    = v-job-no2
         job.stat       = "P"
         op-recid = recid(job).

  for each oe-ordl where oe-ordl.company eq oe-ord.company
                     and oe-ordl.ord-no  eq oe-ord.ord-no exclusive:
      find first job-hdr no-lock
          where job-hdr.company eq cocode
            and job-hdr.job-no  eq oe-ord.job-no
            and job-hdr.job-no2 eq oe-ord.job-no2
            and job-hdr.ord-no  eq oe-ord.ord-no
            and job-hdr.i-no    eq oe-ordl.i-no
          no-error.

      if not avail job-hdr then do:
         find first itemfg where itemfg.company eq oe-ordl.company
                             and itemfg.i-no    eq oe-ordl.i-no
                             no-lock no-error.   
         
         create job-hdr.
         assign job-hdr.company      = cocode
                job-hdr.loc          = locode
                job-hdr.est-no       = oe-ord.est-no
                job-hdr.i-no         = oe-ordl.i-no
                job-hdr.qty          = oe-ordl.qty 
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no
                job-hdr.blank-no     = oe-ordl.blank-no.

         if avail itemfg then
              assign job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         assign job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
      end.

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

      assign job-hdr.est-no  = oe-ord.est-no
             job-hdr.job     = job.job
             job-hdr.job-no  = job.job-no
             job-hdr.job-no2 = job.job-no2
             oe-ordl.est-no  = job-hdr.est-no
             oe-ordl.job-no  = job-hdr.job-no
             oe-ordl.job-no2 = job-hdr.job-no2
             oe-ordl.j-no = job-hdr.j-no.

      FIND CURRENT job-hdr NO-LOCK.
  end.

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
  def input param ip-recid as recid no-undo.
  def var li-line as int no-undo.
  def var v-tax-rate as dec form ">,>>9.99<<<" no-undo.
  def var v-frt-tax-rate like v-tax-rate no-undo.
  def buffer bf-eb for eb .
  
  find bf-eb where recid(bf-eb) = ip-recid no-lock no-error.
  if not avail bf-eb then return.


  find first cust no-lock
      where cust.company = g_company 
        and cust.cust-no = oe-ord.cust-no
      no-error.
  
  for each est-prep where est-prep.company = g_company
                      and est-prep.est-no = bf-eb.est-no
                      and est-prep.simon = "S"     no-lock .
      find first oe-ordm where oe-ordm.company = g_company
                           and oe-ordm.ord-no = oe-ord.ord-no
                           and oe-ordm.charge = est-prep.code
                           no-lock no-error.
      if not avail oe-ordm then do:
         find last oe-ordm of oe-ord no-lock no-error.
         li-line = if avail oe-ordm then oe-ordm.line + 1 else 1.
         find cust where cust.company = g_company 
                     and cust.cust-no = oe-ord.cust-no no-error.
         find first ar-ctrl where ar-ctrl.company = g_company no-lock no-error.
         find first prep where prep.company = g_company 
                           and prep.code = est-prep.code no-lock no-error.
         create oe-ordm.
         assign oe-ordm.company = g_company
                oe-ordm.ord-no = oe-ord.ord-no
                oe-ordm.line = li-line
                oe-ordm.charge = est-prep.code
                oe-ordm.dscr = if est-prep.dscr <> "" then est-prep.dscr else prep.dscr
                oe-ordm.actnum = if avail prep and prep.actnum <> "" then prep.actnum else ar-ctrl.sales
                oe-ordm.amt =  /* (est-prep.cost * est-prep.qty) * ((est-prep.mkup / 100) + 1) * 
                               (est-prep.amtz / 100) */
                               (est-prep.cost * est-prep.qty) / (1 - (est-prep.mkup / 100)) * 
                               (est-prep.amtz / 100) 
                oe-ordm.est-no = est-prep.est-no
                oe-ordm.tax = cust.sort = "Y" and oe-ord.tax-gr <> ""
                oe-ordm.cost = (est-prep.cost * est-prep.qty * (est-prep.amtz / 100))
                oe-ordm.bill  = "Y"
              /*  oe-ordm.ord-i-no
                oe-ordm.ord-line */
                .

         run ar/cctaxrt.p (input g_company, oe-ord.tax-gr,
                            output v-tax-rate, output v-frt-tax-rate).

         if avail cust then do:
           FIND CURRENT cust.
           cust.ord-bal = cust.ord-bal + oe-ordm.amt +
                          (if oe-ordm.tax then (oe-ordm.amt * v-tax-rate / 100) else 0).            
           FIND CURRENT cust NO-LOCK.
         END.
      end.

      FIND CURRENT oe-ordm NO-LOCK.
  end.
  for each ef of bf-eb /*where ef.company = g_company and
                    ef.est-no = oe-ord.est-no */
                    no-lock:
      do i = 1 to 5:
         if ef.mis-simon[i] = "S" then do:
            find last oe-ordm of oe-ord no-lock no-error.
            li-line = if avail oe-ordm then oe-ordm.line + 1 else 1.
            find cust where cust.company = g_company 
                        and cust.cust-no = oe-ord.cust-no no-error.
            find first ar-ctrl where ar-ctrl.company = g_company no-lock no-error.
            find first prep where prep.company = g_company 
                           and prep.code = ef.mis-cost[i] no-lock no-error.

            create oe-ordm.
            assign oe-ordm.company = g_company
                   oe-ordm.ord-no = oe-ord.ord-no
                   oe-ordm.line = li-line
                   oe-ordm.charge = ef.mis-cost[i]
                   oe-ordm.bill  = "Y"
                   oe-ordm.tax = cust.sort = "Y" and oe-ord.tax-gr <> ""
                   oe-ordm.amt = (ef.mis-labf[i] + ef.mis-matf[i] +
                                  ((ef.mis-labm[i] + ef.mis-matm[i]) * (lv-qty / 1000))) /
                                  (1 - (ef.mis-mkup[i] / 100))
                   oe-ordm.est-no = oe-ord.est-no
                   oe-ordm.dscr = if avail prep then prep.dscr else ""
                   oe-ordm.actnum = if avail prep and prep.actnum <> "" then prep.actnum else ar-ctrl.sales
                   oe-ordm.cost = (ef.mis-labf[i] + ef.mis-matf[i] +
                                  ((ef.mis-labm[i] + ef.mis-matm[i]) * (lv-qty / 1000))) 
                   /*  oe-ordm.ord-i-no
                   oe-ordm.ord-line */
                .

            run ar/cctaxrt.p (input g_company, oe-ord.tax-gr,
                              output v-tax-rate, output v-frt-tax-rate).

            if avail cust then do:
              FIND CURRENT cust.
              cust.ord-bal = cust.ord-bal + oe-ordm.amt +
                             (if oe-ordm.tax then (oe-ordm.amt * v-tax-rate / 100) else 0).
              FIND CURRENT cust NO-LOCK.
            END.

            FIND CURRENT oe-ordm NO-LOCK.            
         end.  /* simon = "S" */
      end.  /* do */              
  end.  
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
  def var v-qty-sum as int no-undo.
  def var v-nxt-r-no as int init 1 no-undo.
  def var v-lst-rel as date no-undo.
  def var v-pct-chg as dec no-undo.
  def var v-ship-id like oe-rel.ship-id no-undo.
  def var v-num-shipto as int no-undo.
  
  def buffer xoe-rel for oe-rel.

  
  find xoe-ord where recid(xoe-ord) = recid(oe-ord) no-lock.  
  find first oe-ordl where oe-ordl.company = oe-ord.company and
                           oe-ordl.ord-no = oe-ord.ord-no
                           no-lock no-error.
  assign v-qty-sum  = 0.
  {oe/oe-rel.a &fil="oe-ordl"}
  assign v-ship-id = "".
  find first xoe-rel where xoe-rel.company eq cocode
                       and xoe-rel.ord-no  eq oe-ordl.ord-no
                       and recid(xoe-rel)  ne recid(oe-rel)
                       and xoe-rel.link-no eq 0
                       no-lock no-error.                             
  if not avail xoe-rel then do:
     for each shipto where shipto.company eq cocode
                       and shipto.cust-no eq oe-ordl.cust-no:
               assign v-num-shipto = v-num-shipto + 1.
     end.
     
     if v-num-shipto gt 1 then
     do:
         if oe-ordl.est-no ne "" then
         do:
            find first eb where eb.company = oe-ordl.company and
                                eb.est-no eq oe-ordl.est-no
                            and eb.form-no  ne 0
                               no-lock no-error.
            if avail eb then assign v-ship-id = eb.ship-id.
         end.
         else do:
            find first shipto where shipto.company eq cocode
                                and shipto.cust-no eq oe-ordl.cust-no
                                and shipto.ship-id = oe-ordl.cust-no
                                no-lock no-error.
            if avail shipto then assign v-ship-id = shipto.ship-id.
            else do:
                 find first shipto where shipto.company eq cocode
                                     and shipto.cust-no eq oe-ordl.cust-no
                                     no-lock no-error.
                 if avail shipto then assign v-ship-id = shipto.ship-id.   
            end.
         end.
         run oe/d-shipid.w (input oe-ord.cust-no, input-output v-ship-id)  .
         assign oe-rel.ship-id = trim(v-ship-id).
         find first shipto where shipto.company = cocode and
                                  shipto.cust-no = oe-ord.cust-no  and
                                  shipto.ship-id = v-ship-id
                                  use-index ship-id no-lock no-error.
         if available shipto then do:
            assign v-ship-id           = shipto.ship-id
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
             /* if add mode then use default carrier */
          /*   if sel = 3 /* and NOT oe-rel.carrier ENTERED */ then do: */
            find first sys-ctrl where sys-ctrl.company eq cocode
                                  and sys-ctrl.name    eq "OECARIER"
                             no-lock no-error.
            if not avail sys-ctrl then do:
                               create sys-ctrl.
                               assign
                                 sys-ctrl.company  = cocode
                                 sys-ctrl.name     = "OECARIER"
                                 sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
                                 sys-ctrl.char-fld = "ShipTo".
       
                               do while true:
                                 message "Default Shipping Carrier from Header or Shipto?" 
                                   update sys-ctrl.char-fld.
                                 if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "ShipTo" then leave. 
                               end.
            end.
            oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto.carrier
                               else oe-ord.carrier.
         end.
         /* Run Freight calculation  */
         run oe/oe-frtcl.p.
      end.  /* multi ship to */
      else do:
           find first shipto where shipto.company eq cocode and
                                        shipto.cust-no eq oe-ord.cust-no and
                                        shipto.ship-id eq v-ship-id
                                  no-lock no-error.
            if not avail shipto then
                 find first shipto where shipto.company eq cocode and
                                          shipto.cust-no eq oe-ord.cust-no
                                    no-lock no-error.
            if available shipto then do:
               assign oe-rel.ship-no      = shipto.ship-no
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
               if adm-new-record /* and NOT oe-rel.carrier ENTERED */ then do:
                  find first sys-ctrl where sys-ctrl.company eq cocode
                                        and sys-ctrl.name    eq "OECARIER"
                       no-lock no-error.
                  if not avail sys-ctrl then do:
                     create sys-ctrl.
                     assign sys-ctrl.company  = cocode
                            sys-ctrl.name     = "OECARIER"
                            sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                            sys-ctrl.char-fld = "ShipTo".
       
                     do while true:
                        message "Default Shipping Carrier from Header or Shipto?" 
                        update sys-ctrl.char-fld.
                        if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "Sh~ipTo" then leave. 
                     end.
                  end.
                  oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto~.carrier
                                     else oe-ord.carrier.
               end.
            end. /* avail shipto */
      end. /* not multi */
  end. /* if no oe-rel */
  else do:
       find first shipto where shipto.company = cocode and
                               shipto.cust-no = oe-ord.cust-no  and
                               shipto.ship-id = xoe-rel.ship-id
                               use-index ship-id no-lock no-error.
       if available shipto then do:
           assign oe-rel.ship-no      = shipto.ship-no
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
           if adm-new-record then do:                
                 find first sys-ctrl where sys-ctrl.company eq cocode
                                       and sys-ctrl.name    eq "OECARIER"
                 no-lock no-error.
                 if not avail sys-ctrl then do:
                    create sys-ctrl.
                    assign sys-ctrl.company  = cocode
                           sys-ctrl.name     = "OECARIER"
                           sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                           sys-ctrl.char-fld = "ShipTo".
       
                    do while true:
                        message "Default Shipping Carrier from Header or Shipto?" 
                        update sys-ctrl.char-fld.
                        if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "Sh~ipTo" then leave. 
                    end.
                 end.
                 oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto~.carrier
                                    else oe-ord.carrier.
           end.           
       end.           
  end.
         
  fil_id = recid(oe-ordl).
  if avail oe-ordl and (oe-ordl.est-no ne "" and oe-ordl.job-no eq "") then do:
       message " Since job number is blank, a job will not be created "
                  view-as alert-box.
  end.  
  else run oe/ordlup.p.         /* Update Inventory and Job Costing */
           
 if /*oe-ord.est-no eq "" and*/ oe-ordl.est-no ne "" then do:
      fil_id = recid(oe-ordl).
      run oe/estupl.p.
      v-qty-mod = no.
      fil_id = recid(oe-ordl).
      if avail oe-ordl and (oe-ordl.est-no ne "" and oe-ordl.job-no eq "") then do:
         message " Since job number is blank, a purchase order will not be create~d "
                 view-as alert-box .
      end.  
      else do:
         run po/do-po.p.
      end.   
 end.

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

def input parameter v-item like itemfg.i-no.

def var v-alloc like itemfg.alloc init yes.
def var tmpstore as cha no-undo.
def var i as int no-undo.

{ce/msfcalc.i}

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "SETPRINT"
    no-lock no-error.
if avail sys-ctrl then v-alloc = sys-ctrl.log-fld.
       
find first cust  where cust.company eq cocode
                   and cust.cust-no eq xeb.cust-no
    no-lock no-error.

create itemfg.
assign
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
 itemfg.stocked    = yes.

IF v-graphic-char NE "" THEN 
DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
END.

 if avail xeb then do:
    assign itemfg.die-no     = xeb.die-no
           itemfg.plate-no   = xeb.plate-no
           itemfg.style      = xeb.style
           itemfg.procat     = xeb.procat
           itemfg.cad-no     = xeb.cad-no
           itemfg.upc-no     = xeb.upc-no
           itemfg.spc-no     = xeb.spc-no
           itemfg.isaset     = xeb.form-no eq 0
           itemfg.pur-man    = xeb.pur-man
           itemfg.alloc      = xeb.set-is-assembled.

    IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.

    {fg/set-inks1.i itemfg xeb}
 
    {sys/inc/fgcascnt.i itemfg xeb}

    {sys/inc/updfgdim.i "xeb"}
 end.
 else do:
    if itemfg.def-loc = "" and itemfg.def-loc-bin = "" then do:
       find first cust where cust.company = cocode and
                          cust.active  = "X"    no-lock no-error.
       if avail cust then do:
          find first shipto of cust no-lock no-error.
          if avail shipto then do:
             assign itemfg.def-loc        = shipto.loc        
                    itemfg.def-loc-bin    = shipto.loc-bin.
          end.
       end.
    end.
 end.  


find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
itemfg.i-code = if oe-ordl.est-no ne "" then "C"
                else if avail oe-ctrl then
                        if oe-ctrl.i-code then "S"
                        else "C"
                else "S".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-fields V-table-Win 
PROCEDURE disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE fi_type tb_whs-order.
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
  def input parameter ip-recid as recid no-undo.
  
  DEF VAR v-lead-days LIKE oe-ord.lead-days NO-UNDO.
  DEF VAR v-last-date LIKE oe-ord.last-date NO-UNDO.
  DEF VAR v-due-date  LIKE oe-ord.due-date  NO-UNDO.
  DEF VAR v-ord-date  LIKE oe-ord.ord-date  NO-UNDO.


  DO TRANSACTION:
    {sys/inc/lastship.i}
  END.

  find cust where recid(cust) = ip-recid no-lock no-error.
  if avail cust then do with frame {&frame-name} :
     assign oe-ord.cust-no:screen-value   = cust.cust-no
            oe-ord.cust-name:screen-value = cust.name
            oe-ord.addr[1]:screen-value   = cust.addr[1]
            oe-ord.addr[2]:screen-value   = cust.addr[2]
            oe-ord.city:screen-value      = cust.city
            oe-ord.state:screen-value     = cust.state
            oe-ord.zip:screen-value       = cust.zip
            oe-ord.contact:screen-value   = cust.contact
           /* oe-ord.lead-days:screen-value = cust.ship-days */
            li-lead-days = cust.ship-days
            oe-ord.last-date:screen-value = string(date(oe-ord.ord-date:Screen-value) + 
                                                   cust.ship-days)
            oe-ord.due-date:screen-value  = oe-ord.last-date:screen-value
            oe-ord.terms:screen-value     = cust.terms
            oe-ord.over-pct:screen-value  = string(cust.over-pct)
            oe-ord.under-pct:screen-value = string(cust.under-pct)
            oe-ord.fob-code:screen-value  = cust.fob-code
            oe-ord.frt-pay:screen-value   = if oe-ord.frt-pay eq "" then
                                            cust.frt-pay else oe-ord.frt-pay
            oe-ord.tax-gr:screen-value    = cust.tax-gr
            /*oe-ord.f-bill:screen-value    = oe-ord.frt-pay:screen-value eq "B" */
            ll-f-bill = oe-ord.frt-pay = "B"
            oe-ord.sman[1]:screen-value   = cust.sman
            oe-ord.s-pct[1]:screen-value = "100.00"
            v-custype         = cust.type
            v-ord-limit       = cust.ord-lim
            v-crd-limit       = cust.cr-lim - (cust.acc-bal + cust.ord-bal).

    assign
     lv-old-cust-no    = oe-ord.cust-no:SCREEN-VALUE
     ll-cust-displayed = yes.

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

    find sman where sman.company = oe-ord.company
                and sman.sman = cust.sman
                no-lock no-error.
    if avail sman then assign oe-ord.sname[1]:screen-value = sman.sname
                              oe-ord.s-comm[1]:screen-value = string(sman.scomm)
                              .
    
    if oe-ord.carrier eq "" then oe-ord.carrier:screen-value = cust.carrier.

    find first terms where terms.company eq cocode
                        and terms.t-code  eq cust.terms
               no-lock no-error.
    if avail terms then  oe-ord.terms-d:screen-value = terms.dscr.
    else oe-ord.terms-d:screen-value = "".
         
    find first soldto where soldto.company eq cocode
                        and soldto.cust-no eq cust.cust-no
                        and soldto.sold-id begins trim(cust.cust-no)
        use-index sold-id no-lock no-error.
    if avail soldto then
      assign /*oe-ord.sold-no:screen-value      = soldto.sold-no*/             
             li-sold-no = soldto.sold-no
             oe-ord.sold-id:screen-value      = soldto.sold-id
             oe-ord.sold-name:screen-value    = soldto.sold-name
             oe-ord.sold-addr[1]:screen-value = soldto.sold-addr[1]
             oe-ord.sold-addr[2]:screen-value = soldto.sold-addr[2]
             oe-ord.sold-city:screen-value    = soldto.sold-city
             oe-ord.sold-state:screen-value   = soldto.sold-state
             oe-ord.sold-zip:screen-value     = soldto.sold-zip.

    find first shipto where shipto.company eq cocode
                        and shipto.cust-no eq cust.cust-no
        no-lock no-error.
    if avail shipto then
       assign /*oe-ord.ship-i[1]:screen-value = shipto.notes[1]
              oe-ord.ship-i[2]:screen-value = shipto.notes[2]
              oe-ord.ship-i[3]:screen-value = shipto.notes[3]
              oe-ord.ship-i[4]:screen-value = shipto.notes[4]*/
              ls-ship-i[1] = shipto.notes[1]
              ls-ship-i[2] = shipto.notes[2]
              ls-ship-i[3] = shipto.notes[3]
              ls-ship-i[4] = shipto.notes[4].
       
    if cust.active eq "X" then fi_type:screen-value = "T".
    
    if index("HA",oe-ord.stat:screen-value) = 0 then do:
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
    end.   
  end.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-from-est V-table-Win 
PROCEDURE get-from-est :
/*------------------------------------------------------------------------------
  Purpose:     get all info from estimate    
              /* same purpose as oe/orduest.p & oe/estup.p */
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var v-est-no like est.est-no no-undo.
def var v-est-type like est.est-type no-undo.
def var v-factor as dec no-undo.
def var v-run-list as char init
                "oe/calc-one.p,oe/calc-box.p,ce/tan/print4.p,ce/com/print4.p".
def var i as int no-undo.
def var j as int no-undo.
def var x as int no-undo.
def var nufile as log no-undo.
def var v-blk-qty as int no-undo.
def var v-tax-rate as dec form "->>>.99" no-undo.
def var v-frt-tax-rate as dec form "->>>,99" no-undo.
def var v-quo-price like sys-ctrl.log-fld no-undo.
def var li-line-no as int no-undo.
def var choice as log no-undo.
def var hld-id as recid no-undo.
def var hld-stat like job.stat no-undo.
def var hld-nufile as log no-undo.
def var lv-pr-uom as cha no-undo.
DEF VAR lv-date AS DATE NO-UNDO.
DEF VAR ll-do-job AS LOG NO-UNDO.


DO TRANSACTION:
v-est-no = oe-ord.est-no:screen-value in frame {&frame-name}.
run util/rjust.p (input-output v-est-no,8).
oe-ord.est-no:screen-value in frame {&frame-name} = v-est-no.
 
find first xest
    where xest.company eq cocode
      and xest.est-no  eq v-est-no 
    no-lock no-error.

ASSIGN
 v-est-type = xest.est-type - IF xest.est-type GT 4 THEN 4 ELSE 0
 ll-do-job  = job#-int EQ 0 OR
              CAN-FIND(FIRST eb WHERE eb.company EQ xest.company
                                  AND eb.est-no  EQ xest.est-no
                                  AND eb.pur-man EQ NO
                                  AND eb.form-no NE 0).

if avail xest then do:
  /** CHECK for INACTIVE CUSTOMERS **/
  for each eb where eb.company = cocode and
                    eb.est-no   eq xest.est-no
                and eb.form-no ne 0
                AND TRIM(eb.cust-no) NE ""
           no-lock break by eb.est-no by eb.cust-no:
      
    if first-of(eb.cust-no) then do:
      /** finding the customer is done this way because the index is not
      setup efficently to find the customer regardles of active stat **/
      find first cust {sys/ref/cust.w}
                      and cust.cust-no eq eb.cust-no
           use-index cust no-lock no-error.
      if not avail cust or cust.active eq "I" then do:
         message              "   Inactive Customer:" cust.name skip
            "   Orders May not Be Processed for Inactive Customers.  "
            view-as alert-box warning.
         assign v-inactive = yes.
         return.
      end. /* do */
    end. /* first-of(eb.cust-no) */
    IF v-est-fg1 = "HOLD" AND eb.stock-no = "" THEN DO:
       MESSAGE "Sorry, FG item does not exist. Order has not been approved."
           VIEW-AS ALERT-BOX ERROR.
       APPLY "ENTRY" TO OE-ORD.EST-NO.
       RETURN "NO FGITEM".
    END.
    IF eb.stock-no <> "" THEN DO:
       FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
                             AND reftable.company  EQ g_company
                             AND reftable.loc      EQ ""
                             AND reftable.code     EQ eb.stock-no
                             NO-LOCK NO-ERROR.
       IF AVAIL reftable AND reftable.code2 = "I" THEN DO:
          MESSAGE eb.stock-no "has InActive Status. Order cannot be placed for the Inactive Item."
                  VIEW-AS ALERT-BOX ERROR. 
          RETURN "Inactive FGItem".
       END.
          
    END.
  end. /* each eb */

  assign
   j         = 1
   fil_id    = save_id        /* reset fil_id, scrambled in calc...*/
   ll-order-from-est = yes.              
   
  find first xeb where xeb.company = xest.company 
                   and xeb.est-no eq xest.est-no
                   and xeb.form-no  eq 0
       no-lock no-error.     
  for each eb where eb.company = xest.company 
                and eb.est-no  eq xest.est-no
                and eb.form-no  ne 0
                and eb.blank-no ne 0
                AND TRIM(eb.cust-no) NE ""
      no-lock,
      FIRST cust NO-LOCK
      {sys/ref/cust.w}
        AND cust.cust-no eq eb.cust-no
      USE-INDEX cust
      break by eb.est-no by eb.cust-no by eb.form-no by eb.blank-no:
      
    if first-of(eb.cust-no) then do:
        /* if order is not created from above - only one cust-no */
      find xoe-ord where recid(xoe-ord) = recid(oe-ord) NO-LOCK.

      IF ll-do-job THEN DO:
        v-job-no = fill(" ",6 - length(trim(oe-ord.ord-no:screen-value))) + oe-ord.ord-no:screen-value.
        RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2).
         
        IF v-job-no EQ "" THEN
          v-job-no = fill(" ",6 - length(trim(oe-ord.est-no:screen-value))) + trim(oe-ord.est-no:screen-value).
      END.

      ELSE
        ASSIGN
         v-job-no  = ""
         v-job-no2 = 0.

      RUN display-cust-detail (RECID(cust)).

      assign oe-ord.sold-id:screen-value   = eb.cust-no /** DEFAULT to first SOLD to **/
             oe-ord.sman[1]:screen-value   = eb.sman
             oe-ord.cust-no:screen-value   = eb.cust-no
             oe-ord.carrier:screen-value   = eb.carrier
             oe-ord.frt-pay:screen-value   = eb.chg-method
             oe-ord.s-comm[1]:screen-value = string(eb.comm)
             oe-ord.s-pct[1]:screen-value  = "100"
             oe-ord.due-code:screen-value  = "ON"
             v-job-no2 = 0
             oe-ord.job-no:screen-value    = v-job-no
             oe-ord.job-no2:screen-value   = string(v-job-no2).

      if xest.ord-no ne 0 then oe-ord.pord-no:screen-value = string(xest.ord-no).
      
      if first(eb.cust-no) then do:
         fil_id = recid(xoe-ord).
      end.    
      assign oe-ord.cust-no:screen-value   = cust.cust-no
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
             oe-ord.over-pct:screen-value  = string(cust.over-pct)
             oe-ord.under-pct:screen-value = string(cust.under-pct)
             oe-ord.fob-code:screen-value  = cust.fob-code
             oe-ord.tax-gr:screen-value    = cust.tax-gr
             v-custype         = cust.type.

      find first terms where terms.company eq cocode
                        and terms.t-code  eq cust.terms
               no-lock no-error.
      if avail terms then  oe-ord.terms-d:screen-value = terms.dscr.
      else oe-ord.terms-d:screen-value = "".

      if cust.active eq "X" then fi_type:screen-value = "T".

      v-factor = if xest.est-type ge 1 and xest.est-type le 4 then lastship-dec
                 else 1.
      if lastship-cha eq "Fibre" then
        ASSIGN
         oe-ord.last-date:SCREEN-VALUE = STRING(TODAY + (cust.ship-days * v-factor))
         oe-ord.due-date:SCREEN-VALUE  = STRING(TODAY + (lastship-int * v-factor)).

      if oe-ord.carrier:screen-value eq "" then oe-ord.carrier:screen-value = cust.carrier.
    end. /* first-of(eb.cust-no) */

    leave. /** 2pc box & Set headers **/
  end. /* each eb */
end. /* avail xest */
END. /* transaction */

assign
 ll-est-no-mod     = NO
 lv-old-cust-no    = oe-ord.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
 ll-cust-displayed = YES.

RUN release-shared-buffers.

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
     /*
     rect-36:HIDDEN          = ip-hidden
     fi_sman-lbl:HIDDEN      = ip-hidden
     fi_sname-lbl:HIDDEN     = ip-hidden
     */
     fi_s-pct-lbl:HIDDEN     = ip-hidden
     fi_s-comm-lbl:HIDDEN    = ip-hidden
     /*
     oe-ord.sman[1]:HIDDEN   = ip-hidden
     oe-ord.sman[2]:HIDDEN   = ip-hidden
     oe-ord.sman[3]:HIDDEN   = ip-hidden
     oe-ord.sname[1]:HIDDEN  = ip-hidden
     oe-ord.sname[2]:HIDDEN  = ip-hidden
     oe-ord.sname[3]:HIDDEN  = ip-hidden
     */
     oe-ord.s-pct[1]:HIDDEN  = ip-hidden
     oe-ord.s-pct[2]:HIDDEN  = ip-hidden
     oe-ord.s-pct[3]:HIDDEN  = ip-hidden
     oe-ord.s-comm[1]:HIDDEN = ip-hidden
     oe-ord.s-comm[2]:HIDDEN = ip-hidden
     oe-ord.s-comm[3]:HIDDEN = ip-hidden.
  

    IF NOT ip-hidden THEN
      DISPLAY rect-36
              fi_sman-lbl
              fi_sname-lbl
              fi_s-pct-lbl
              fi_s-comm-lbl
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
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-uom LIKE itemfg.prod-uom NO-UNDO.

  DEF BUFFER b-oe-ord FOR oe-ord.
  DEF BUFFER b-oe-ordl FOR oe-ordl.


  RELEASE cust.

  IF AVAIL oe-ord THEN
  FIND b-oe-ord WHERE ROWID(b-oe-ord) EQ ROWID(oe-ord)
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

  IF AVAIL b-oe-ord /*AND CAN-DO("A,H,W",oe-ord.stat)*/ THEN
  FIND FIRST cust
      WHERE cust.company eq cocode
        AND cust.cust-no eq oe-ord.cust-no
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

  IF AVAIL cust THEN DO:
    IF oe-ord.stat EQ "W" THEN DO:
      ASSIGN
       oe-ord.user-id   = USERID("nosweat")
       oe-ord.t-freight = 0.

      IF oe-ord.type EQ "" THEN oe-ord.type = "O".

      IF oe-ord.sman[1] EQ "" THEN
        ASSIGN
         oe-ord.sman    = ""
         oe-ord.sman[1] = cust.sman.

      IF oe-ord.sman[1] NE "" AND oe-ord.s-pct[1] EQ 0 THEN
        oe-ord.s-pct[1] = 100.00.

      DO i = 1 TO EXTENT(oe-ord.sman):
        IF oe-ord.s-comm[i] GE 100 THEN oe-ord.s-comm = 0.

        FIND FIRST sman
            WHERE sman.company EQ oe-ord.company
              AND sman.sman    EQ oe-ord.sman[i]
            NO-LOCK NO-ERROR.
        IF AVAIL sman THEN DO:
          oe-ord.sname[1] = sman.sname.

          IF oe-ord.s-comm[i] LE 0 THEN oe-ord.s-comm[i] = sman.scomm.
        END.
      END.

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
      END.

      RUN oe/ordfrate.p (ROWID(oe-ord)).

      FIND xoe-ord WHERE ROWID(xoe-ord) EQ ROWID(oe-ord) NO-LOCK NO-ERROR.
      IF AVAIL xoe-ord THEN RUN oe/oe-comm.p.

      RUN oe/calcordt.p (ROWID(oe-ord)).
    END.

    oe-ord.stat = IF oe-ord.stat NE "H" THEN "H" ELSE "A".

    IF oe-ord.stat EQ "A" THEN oe-ord.posted = NO.

    FIND CURRENT oe-ord NO-LOCK NO-ERROR.
    FIND CURRENT b-oe-ord NO-LOCK NO-ERROR.
       
    /* update credit hold field in cust file */
    cust.cr-hold = oe-ord.stat EQ "H" OR
                   CAN-FIND(FIRST b-oe-ord
                            WHERE b-oe-ord.company EQ oe-ord.company
                              AND b-oe-ord.cust-no EQ oe-ord.cust-no
                              AND b-oe-ord.stat    EQ "H").
    FIND CURRENT cust NO-LOCK NO-ERROR.

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (ROWID(oe-ord)).
  END.

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
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-date LIKE oe-ord.due-date NO-UNDO.
  DEF VAR lv-stat AS CHAR NO-UNDO.
  DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.
  DEF VAR calcDueDate AS DATE NO-UNDO.
  DEF VAR calcStartDate AS DATE NO-UNDO.
  DEF VAR li-tries AS INT NO-UNDO.

  DEF BUFFER b-oe-rel FOR oe-rel.
  DEF BUFFER due-job-hdr FOR job-hdr.


  SESSION:SET-WAIT-STATE ("general").

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ll-new-po = NO.

    IF NOT adm-new-record AND oe-ord.po-no NE oe-ord.po-no:SCREEN-VALUE THEN
      MESSAGE "Update all order lines/releases with this "  +
              TRIM(oe-ord.po-no:LABEL) + "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-new-po.
  END.

  ASSIGN
   lv-date   = oe-ord.due-date
   lv-ord-no = oe-ord.ord-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  find first terms where terms.t-code = oe-ord.terms no-lock no-error.
  if avail terms then oe-ord.terms-d = terms.dscr.

  IF oe-ord.ord-no NE lv-ord-no THEN DO:
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode EXCLUSIVE NO-WAIT NO-ERROR.
    IF AVAIL oe-ctrl                  AND
       lv-ord-no + 1 EQ oe-ctrl.n-ord THEN oe-ctrl.n-ord = oe-ctrl.n-ord - 1.
    FIND CURRENT oe-ctrl NO-LOCK NO-ERROR.

    FOR EACH oe-ordl
        WHERE oe-ordl.company EQ oe-ord.company
          AND oe-ordl.ord-no  EQ lv-ord-no:
      oe-ordl.ord-no = oe-ord.ord-no.
    END.
  END.

  IF oe-ord.job-no NE '' THEN DO:
    FIND FIRST job EXCLUSIVE-LOCK WHERE job.company EQ oe-ord.company
                                    AND job.job-no EQ oe-ord.job-no
                                    AND job.job-no2 EQ oe-ord.job-no2 NO-ERROR.
    IF AVAILABLE job THEN DO:
      IF dueDateChanged THEN DO:
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
      IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
        IF prodDateChanged THEN
        job.start-date = IF sys-ctrl.char-fld NE 'NoDate' THEN oe-ord.prod-date
                         ELSE ?.
        IF dueDateChanged AND sys-ctrl.char-fld EQ 'PlanDate' THEN DO:
          IF NOT VALID-HANDLE(scheduleHndl) THEN
          RUN custom/schedule.p PERSISTENT SET scheduleHndl.
          RUN scheduleJob IN scheduleHndl (ROWID(job),OUTPUT calcStartDate,OUTPUT calcDueDate).
          IF calcDueDate NE oe-ord.due-date THEN
          MESSAGE 'Machine Capacity calulated Scheduled Completion date of'
            calcDueDate SKIP 'Update Due Date and Promise Date on Order?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE updateDueDate AS LOGICAL.
          IF updateDueDate THEN
          ASSIGN
            job.due-date = calcDueDate
            oe-ord.due-date = calcDueDate.
        END. /* if duedatechanged */
        job.start-date = calcStartDate.
        FIND CURRENT job NO-LOCK.
      END. /* avail sys-ctrl */
    END. /* avail job */
  END. /* if job-no ne '' */
  ASSIGN
    prodDateChanged = NO
    dueDateChanged = NO
    oe-ord.type = fi_type.

  RUN whs-order (1).

  FOR EACH oe-ordl OF oe-ord:
    IF ll-new-po THEN DO:
      oe-ordl.po-no = oe-ord.po-no.
      FOR EACH oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-ordl.company
            AND oe-rel.ord-no   EQ oe-ordl.ord-no
            AND oe-rel.i-no     EQ oe-ordl.i-no
            AND oe-rel.line     EQ oe-ordl.line:

        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

        li-tries = 0.
        IF INDEX("SLIA",lv-stat) GT 0 THEN DO WHILE TRUE:
          li-tries = li-tries + 1.
          IF li-tries GE 1000 THEN LEAVE.

          FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ ROWID(oe-rel)
              EXCLUSIVE NO-WAIT NO-ERROR.
          IF AVAIL b-oe-rel THEN DO:
            b-oe-rel.po-no = oe-ordl.po-no.
            LEAVE.
          END.
        END.
      END.
    END.

    IF oe-ordl.est-no NE "" THEN DO:
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

  IF oe-ord.due-date NE lv-date THEN
  FOR EACH oe-ordl OF oe-ord BREAK BY oe-ordl.line:
    IF NOT ll-new-due THEN
      ll-new-due = FIRST(oe-ordl.line) AND LAST(oe-ordl.line).

    IF NOT ll-new-due THEN
      MESSAGE "Update all line items with this Due Date?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-new-due.

    IF ll-new-due THEN DO:
      oe-ordl.req-date = oe-ord.due-date.

      IF oe-ordl.req-date GT oe-ordl.prom-date THEN
        oe-ordl.prom-date = oe-ordl.req-date.
    END.
    ELSE LEAVE.
  END. /* each oe-ordl */
  FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
  
  RELEASE eb.
  RELEASE oe-rel.
  RELEASE bfx-ord.
  RELEASE oe-ordl.

  IF oe-ord.frt-pay = "B" THEN oe-ord.f-bill = YES.
  ELSE oe-ord.f-bill = NO.

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
  copyRecord = NO.
  RUN release-shared-buffers.
  RUN disable-fields.

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

  SESSION:SET-WAIT-STATE('General':U).
  RUN nextOrdNo (OUTPUT nextOrdNo).
  IF nextOrdNo EQ ? THEN RETURN ERROR.
  IF CAN-FIND(FIRST oe-ord
              WHERE oe-ord.company EQ g_company
                AND oe-ord.ord-no EQ nextOrdNo) THEN
  nextOrdNo = nextOrdNo + 1.
  RUN checkOrdNo (ROWID(oe-ord),INPUT-OUTPUT nextOrdNo).
  RUN copyOrder (g_company,g_company,oe-ord.ord-no,nextOrdNo).
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  
END PROCEDURE.

{oe/copyOrder.i} /* containers procedure copyOrder */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  
  DEF VAR li-next-ordno AS INT NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN nextOrdNo (OUTPUT li-next-ordno).
  IF li-next-ordno EQ ? THEN RETURN ERROR.
                                 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-new-row-id = ROWID(oe-ord).

  RUN checkOrdNo (ROWID(oe-ord),INPUT-OUTPUT li-next-ordno).

  assign oe-ord.company = g_company
         oe-ord.loc = g_loc
         oe-ord.ord-date = today
         oe-ord.ord-no = li-next-ordno
         oe-ord.user-id = userid("nosweat")
         oe-ord.type = "O"
         oe-ord.due-code = "ON".

  DO WITH FRAME {&FRAME-NAME}:
    fi_type:SCREEN-VALUE = oe-ord.type.
  END.

  FOR EACH b-oe-ordl
      WHERE b-oe-ordl.company EQ oe-ord.company
        AND b-oe-ordl.ord-no  EQ oe-ord.ord-no:
    DELETE b-oe-ordl.
  END.

  CREATE b-oe-ordl.
  ASSIGN
   b-oe-ordl.company = oe-ord.company
   b-oe-ordl.ord-no  = oe-ord.ord-no
   b-oe-ordl.line    = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  {oe/v-ord-d.i}

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
    fi_type = oe-ord.type.

    RUN whs-order (0).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

    ENABLE fi_type tb_whs-order.
    IF NOT job#-log THEN DISABLE oe-ord.job-no oe-ord.job-no2 .
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
  IF NOT AVAIL oe-ord THEN RUN dispatch ('view').  /* force to display frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var v-est-no as cha no-undo.
  def var choice as log no-undo.
  def var char-hdl as cha no-undo.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

      
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  /* Code placed here will execute PRIOR to standard behavior. */
  ll-is-new-rec = adm-new-record.
  /* ==== validation ========*/  
  do with frame {&frame-name}:
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

     RUN valid-job-no2 NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-cust-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-po-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-tax-gr NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     if oe-ord.carrier:screen-value <> "" and
        not can-find(first carrier where carrier.company = g_company and
                                  carrier.loc = g_loc and
                                  carrier.carrier = oe-ord.carrier:screen-value)
     then do:                              
         message "Invalid Carrier. Try help. " view-as alert-box error.
         return no-apply.                                
     end.       
     if oe-ord.terms:screen-value <> "" and
        not can-find(first terms where terms.t-code = oe-ord.terms:screen-value)
     then do:
        message "Invalid Terms Code. Try help. " view-as alert-box error.
        return no-apply.
     end.
     if oe-ord.due-code:screen-value <> "" and
        lookup(oe-ord.due-code:screen-value,v-duelist) = 0 then 
     do:
        message "Invalid Due Code. Try help. " view-as alert-box error.
        apply "entry" to oe-ord.due-code.
        return no-apply.
     end.
     /*
     if oe-ord.ord-date:modified and date(oe-ord.ord-date:screen-value) < today then do:
        message "Order Date can not be earlier than TODAY." view-as alert-box error.
        apply "entry" to oe-ord.ord-date.
        return no-apply.
     end.
     */
     if oe-ord.last-date:modified and date(oe-ord.last-date:screen-value) < today then do:
        message "Last Ship Date can not be earlier than TODAY." view-as alert-box error.
        apply "entry" to oe-ord.last-date.
        return no-apply.
     end.
     if oe-ord.prod-date:modified and date(oe-ord.prod-date:screen-value) < today then do:
        message "Prod. Date can not be earlier than TODAY." view-as alert-box error.
        apply "entry" to oe-ord.prod-date.
        return no-apply.
     end.
     if oe-ord.due-date:modified and date(oe-ord.due-date:screen-value) < date(oe-ord.ord-date:screen-value) then do:
        message "Due Date can not be earlier than Order Date(" oe-ord.due-date:SCREEN-VALUE ")." view-as alert-box error.
        apply "entry" to oe-ord.due-date.
        return no-apply.
     end.

     IF oe-ord.s-pct[1]:HIDDEN EQ NO               AND
        (oe-ord.sman[1]:SCREEN-VALUE NE "" OR
         oe-ord.sman[2]:SCREEN-VALUE NE "" OR
         oe-ord.sman[3]:SCREEN-VALUE NE "")        AND
        (DEC(oe-ord.s-pct[1]:SCREEN-VALUE) +
         DEC(oe-ord.s-pct[2]:SCREEN-VALUE) +
         DEC(oe-ord.s-pct[3]:SCREEN-VALUE) NE 100) THEN DO: 

       MESSAGE "Order's Salesman Commission % of Sales does not equal 100%, continue?"
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE ll.
       IF NOT ll THEN DO:
         APPLY "entry" TO oe-ord.s-pct[1].
         RETURN NO-APPLY.
       END.
     END.
  end.  /* frame {&frame-name} */

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

  /* Code placed here will execute AFTER standard behavior.    */
  /* ===  don't go item page yet. -> move page to 2 */
  IF ll-is-new-rec THEN DO:
    IF oe-ord.est-no EQ "" THEN DO TRANSACTION:
      FIND CURRENT oe-ord.
      oe-ord.f-bill = oe-ord.frt-pay EQ "B".
      FIND CURRENT oe-ord NO-LOCK.
    END.
        
    ELSE RUN order-from-est (YES).

    RUN reset-browser (lv-rowid).

    IF AVAIL oe-ord AND oe-ord.est-no EQ "" THEN DO:
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
      RUN notify in WIDGET-HANDLE(char-hdl) ('row-available').       
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
      RUN select-page IN WIDGET-HANDLE(char-hdl) (3).
      RUN get-link-handle in adm-broker-hdl(THIS-PROCEDURE,"oeitem-target",OUTPUT char-hdl).
      RUN add-auto IN WIDGET-HANDLE(char-hdl).
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
      RUN notify IN WIDGET-HANDLE(char-hdl) ('row-available'). 
    END.
  END.

  ELSE DO:
    BUFFER-COMPARE oe-ord TO old-oe-ord SAVE RESULT IN ll.
    IF ll-new-po OR ll-new-due OR NOT ll THEN DO:
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
      RUN repo-query1 IN WIDGET-HANDLE(char-hdl) (ROWID(oe-ord)). 
    END.
  END.
  ASSIGN
   ll-order-from-est = NO
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
          IF DEC(oe-ord.s-comm[1]:SCREEN-VALUE) EQ 0 THEN oe-ord.s-comm[1]:SCREEN-VALUE = STRING(sman.scomm).
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
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-ord-no AS INTEGER NO-UNDO.


  FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ g_company NO-ERROR.
  IF AVAIL oe-ctrl THEN DO:
    op-ord-no = oe-ctrl.n-ord.
    RUN updateOrdNo (INPUT-OUTPUT op-ord-no).
  END.
  ELSE op-ord-no = ?.

  IF op-ord-no EQ ? THEN
    MESSAGE "Unable to obtain next avail order number..."
        VIEW-AS ALERT-BOX ERROR.

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

  {oe/ordfrest.i}

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
  DEF VAR lv-m-time AS INT no-undo.
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
                    AND bf-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK:
      FOR EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                        AND bf-mch.job-no = bf-hdr.job-no
                        AND bf-mch.job-no2 = bf-hdr.job-no2 NO-LOCK:
          lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                          truncate(bf-mch.mr-hr,0) * 3600 +
                        ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
          lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                          truncate(bf-mch.run-hr,0) * 3600 +
                        ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60.

          lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
      END.
  END.
  
  

  lv-job-hr = IF lv-job-time MOD 3600 > 0 THEN truncate(lv-job-time / 3600,0) + 1
               ELSE truncate(lv-job-time / 3600,0).
  lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN truncate(lv-job-hr / 8,0) + 1
               ELSE TRUNCATE(lv-job-hr / 8,0).

  lv-start-date = lv-first-due-date - lv-job-day . /*- 1. */
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
               BREAK BY bf-mch.frm BY bf-mch.blank-no by bf-mch.pass BY bf-mch.m-code:

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
             bf-job.start-date = lv-start-date.
             lv-wrk-st-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
          END.
          IF FIRST-OF(bf-mch.frm) THEN
                bf-hdr.start-date = job.start-date.
      
          lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.mr-hr,0) * 3600 +
                    ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
          lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.run-hr,0) * 3600 +
                    ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60.

          ASSIGN bf-mch.seq-no = 0                 
                 bf-mch.start-time-su = lv-wrk-st-time
                 bf-mch.start-time = lv-wrk-st-time + lv-mr-time
                 bf-mch.start-date-su = lv-start-date 
                 .

          lv-start-date-fr = lv-start-date.
          lv-job-time = lv-job-time + lv-mr-time .
          lv-start-date = lv-start-date + 
                          IF lv-mr-time > lv-m-time AND
                             lv-mr-time MOD lv-m-time > 0 THEN TRUNCATE(lv-mr-time / lv-m-time,0) 
                          ELSE IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0) - 1
                          ELSE 0.
          /*
          RUN get-num-of-hol (lv-start-date-fr,lv-start-date,OUTPUT li-num-of-wkend).
          lv-start-date = lv-start-date + li-num-of-wkend.
          RUN get-next-workday (INPUT-OUTPUT lv-start-date,"NEXT"). 
          */
          lv-start-date-fr = lv-start-date.
          IF lv-m-time <> lv-maccum-time THEN DO:
             lv-start-date = lv-start-date + 
                          IF lv-job-time > lv-maccum-time AND
                             lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                          ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                          ELSE 0.
              /*
             RUN get-num-of-hol (lv-start-date-fr,lv-start-date,OUTPUT li-num-of-wkend).
             lv-start-date = lv-start-date + li-num-of-wkend.
             RUN get-next-workday (INPUT-OUTPUT lv-start-date,"NEXT").
             */
          END.
          lv-start-date-fr = lv-start-date.
          ASSIGN bf-mch.end-date-su = lv-start-date
                 bf-mch.start-date = lv-start-date .
          lv-job-time = lv-job-time + lv-run-time .
          lv-start-date = lv-start-date + 
                          IF lv-run-time > lv-m-time AND
                             lv-run-time MOD lv-m-time > 0 THEN TRUNCATE(lv-run-time / lv-m-time,0) 
                          ELSE IF lv-run-time > lv-m-time THEN TRUNCATE(lv-run-time / lv-m-time,0) - 1
                          ELSE 0.
         /*
          RUN get-num-of-hol (lv-start-date-fr,lv-start-date,OUTPUT li-num-of-wkend).
          lv-start-date = lv-start-date + li-num-of-wkend.
          RUN get-next-workday (INPUT-OUTPUT lv-start-date,"NEXT").
          */
          lv-start-date-fr = lv-start-date.
          IF lv-m-time <> lv-maccum-time THEN DO:
             lv-start-date = lv-start-date + 
                          IF lv-job-time > lv-maccum-time AND
                             lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                          ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                          ELSE 0.
             /*
             RUN get-num-of-hol (lv-start-date-fr,lv-start-date,OUTPUT li-num-of-wkend).
             lv-start-date = lv-start-date + li-num-of-wkend.
             RUN get-next-workday (INPUT-OUTPUT lv-start-date,"NEXT").
             */
          END.
          ASSIGN bf-mch.end-time = bf-mch.start-time + lv-run-time
                 bf-mch.end-time-su = bf-mch.start-time-su + lv-mr-time
                 bf-mch.end-date = lv-start-date
                 .           
          lv-wrk-st-time = lv-wrk-st-time + lv-mr-time + lv-run-time.
  END.
  END. /* if v-run-schedule*/
  
  bx-ordl.prom-date = lv-prom-date.
  bx-ordl.req-date = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.req-date.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateOrdNo V-table-Win 
PROCEDURE updateOrdNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-ord-no LIKE oe-ord.ord-no NO-UNDO.

  DEF VAR li AS INT NO-UNDO.


  RELEASE oe-ctrl.

  DO WHILE NOT AVAIL oe-ctrl AND i LT 1000:
    li = li + 1.

    FIND FIRST oe-ctrl EXCLUSIVE-LOCK WHERE oe-ctrl.company EQ g_company NO-WAIT NO-ERROR.

    IF AVAIL oe-ctrl THEN DO:
      oe-ctrl.n-ord = io-ord-no + 1.
      FIND CURRENT oe-ctrl NO-LOCK NO-ERROR.
    END.

    ELSE
    IF li GE 1000 THEN io-ord-no = ?.
  END.

  RELEASE oe-ctrl.

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
            WHERE quotehd.company eq est.company
              AND quotehd.loc     eq est.loc
              AND quotehd.est-no  eq est.est-no
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

  DO WITH FRAME {&FRAME-NAME}:
    oe-ord.job-no:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(oe-ord.job-no:SCREEN-VALUE))) +
                                 TRIM(oe-ord.job-no:SCREEN-VALUE).
  END.

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
  DEF BUFFER cust-po-mand FOR reftable.

  
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ oe-ord.company
          AND cust.cust-no EQ oe-ord.cust-no:SCREEN-VALUE
          AND CAN-FIND(FIRST cust-po-mand
                       WHERE cust-po-mand.reftable EQ "cust.po-mand"
                         AND cust-po-mand.company  EQ cust.company
                         AND cust-po-mand.loc      EQ ""
                         AND cust-po-mand.code     EQ cust.cust-no
                         AND cust-po-mand.val[1]   EQ 1)
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
          MESSAGE "Invalid Salesman, try help..." VIEW-AS ALERT-BOX ERROR.
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
         oe-ord.s-comm[1]:SCREEN-VALUE = "0".
    END.
  END.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whs-order V-table-Win 
PROCEDURE whs-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.


  RELEASE oe-ord-whs-order.

  IF AVAIL oe-ord THEN DO:
    FIND FIRST oe-ord-whs-order NO-LOCK
        WHERE oe-ord-whs-order.reftable EQ "oe-ord.whs-order"
          AND oe-ord-whs-order.company  EQ oe-ord.company
          AND oe-ord-whs-order.loc      EQ STRING(oe-ord.ord-no,"9999999999")
        NO-ERROR.

    IF NOT AVAIL oe-ord-whs-order THEN DO TRANSACTION:
      CREATE oe-ord-whs-order.
      ASSIGN
       oe-ord-whs-order.reftable = "oe-ord.whs-order"
       oe-ord-whs-order.company  = oe-ord.company
       oe-ord-whs-order.loc      = STRING(oe-ord.ord-no,"9999999999").
      FIND CURRENT oe-ord-whs-order NO-LOCK.
    END.
  END.

  IF ip-int EQ 0 THEN DO WITH FRAME {&FRAME-NAME}:
    tb_whs-order:SCREEN-VALUE = STRING(AVAIL oe-ord-whs-order AND
                                       oe-ord-whs-order.val[1] EQ 1,"yes/no").
  END.

  ELSE
  IF AVAIL oe-ord-whs-order THEN DO TRANSACTION:
    FIND CURRENT oe-ord-whs-order.
    oe-ord-whs-order.val[1] = INT(tb_whs-order).
    FIND CURRENT oe-ord-whs-order NO-LOCK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

