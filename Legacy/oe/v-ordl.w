&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: oe\v-ordl.w

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
&scoped-define order-disable ordl
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}
{sys/inc/var.i new shared }

DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
def var v-use-rel like sys-ctrl.log-fld no-undo.
def var v-upd-comm as log init yes no-undo.
def var v-dup-item as log no-undo.
def var v-job-meth as cha no-undo.
def var v-oecount as log no-undo.
def var v-full-cost as log no-undo.
def var v-quo-price as log no-undo.
def var v-est-fg as log no-undo.
def var v-est-fg1 as cha no-undo.
def var v-rtn-code as int no-undo.
def new shared buffer xest for est.
def new shared buffer xeb for eb.
def new shared buffer xef for ef.
def var v-valdcode as cha init "ON,BY,MH" no-undo.
def var v-bld-job as cha no-undo.
def var v-est-no as cha no-undo.  /* for adjust est-no */
/* for oe/oe-price.p ========*/
define new shared buffer xoe-ord for oe-ord.    /* BUFFER WITH ORDER HEADER */
define new shared var save_id as recid no-undo.  /* RECORD ID FOR ORDER LINE */
define new shared var v-i-item like oe-ordl.i-no no-undo. /* INPUT ITEM */
define new shared var v-i-qty like oe-ordl.qty no-undo. /* INPUT QUANTITY */
define new shared var price-ent as log NO-UNDO.
DEFINE NEW SHARED VAR matrixExists AS LOG NO-UNDO.
define new shared var fil_id as recid no-undo. 
def new shared var nufile as log no-undo.
def new shared var v-qty-mod as log no-undo.
def new shared var v-create-job as log no-undo.
def var lv-ordl-recid as recid no-undo.
def var lv-change-prom-date as log no-undo.  /* flag for updating oe-ordl.prom-date*/
def var lv-uom-list as cha init "M,EA,L,CS,C" no-undo.
DEF VAR ld-lastship-dec AS DEC NO-UNDO.
DEF VAR ld-lastship-cha AS CHAR NO-UNDO.
DEF VAR v-print-head LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-print-fmt LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.
DEF VAR v-rel AS INT NO-UNDO.
DEF VAR v-margin AS DEC NO-UNDO.
DEF VAR v-ship-from AS CHAR NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ordl.est-no oe-ordl.qty oe-ordl.i-no ~
oe-ordl.part-no oe-ordl.i-name oe-ordl.part-dscr1 oe-ordl.part-dscr2 ~
oe-ordl.price oe-ordl.pr-uom oe-ordl.cas-cnt oe-ordl.disc oe-ordl.po-no ~
oe-ordl.po-no-po oe-ordl.vend-no oe-ordl.req-code oe-ordl.req-date ~
oe-ordl.prom-code oe-ordl.prom-date oe-ordl.tax oe-ordl.s-man[1] ~
oe-ordl.s-pct[1] oe-ordl.s-man[2] oe-ordl.s-pct[2] oe-ordl.s-man[3] ~
oe-ordl.s-pct[3] 
&Scoped-define ENABLED-TABLES oe-ordl
&Scoped-define FIRST-ENABLED-TABLE oe-ordl
&Scoped-Define ENABLED-OBJECTS RECT-31 RECT-36 RECT-37 
&Scoped-Define DISPLAYED-FIELDS oe-ordl.est-no oe-ordl.qty oe-ordl.i-no ~
oe-ordl.part-no oe-ordl.i-name oe-ordl.part-dscr1 oe-ordl.part-dscr2 ~
oe-ordl.price oe-ordl.pr-uom oe-ordl.cost oe-ordl.cas-cnt oe-ordl.disc ~
oe-ordl.t-price oe-ordl.po-no oe-ordl.po-no-po oe-ordl.vend-no ~
oe-ordl.req-code oe-ordl.req-date oe-ordl.prom-code oe-ordl.prom-date ~
oe-ordl.job-no oe-ordl.job-no2 oe-ordl.tax oe-ordl.s-man[1] ~
oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.s-man[2] oe-ordl.s-pct[2] ~
oe-ordl.s-comm[2] oe-ordl.s-man[3] oe-ordl.s-pct[3] oe-ordl.s-comm[3] 
&Scoped-define DISPLAYED-TABLES oe-ordl
&Scoped-define FIRST-DISPLAYED-TABLE oe-ordl
&Scoped-Define DISPLAYED-OBJECTS li-on-hand li-on-order li-alloc ~
li-backorder li-avail li-reorder 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS oe-ordl.cost oe-ordl.t-price 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTaxable V-table-Win
FUNCTION fGetTaxable RETURNS LOGICAL PRIVATE
  (ipcCompany AS CHARACTER,
   ipcCust AS CHARACTER,
   ipcShipto AS CHARACTER,
   ipcFGItemID AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
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
DEFINE VARIABLE li-alloc AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Allocated" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE li-avail AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Available" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE li-backorder AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Backorder" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE li-on-hand AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "On Hand" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE li-on-order AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "On Order" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE li-reorder AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Reorder" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 4.29.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 4.29.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 129 BY 14.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-ordl.est-no AT ROW 1.24 COL 17 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-ordl.qty AT ROW 2.19 COL 17 COLON-ALIGNED
          LABEL "Quantity" FORMAT "->>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     oe-ordl.i-no AT ROW 3.14 COL 17 COLON-ALIGNED
          LABEL "FG Item#" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     oe-ordl.part-no AT ROW 4.1 COL 17 COLON-ALIGNED
          LABEL "Cust Part #" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     oe-ordl.i-name AT ROW 5.05 COL 17 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     oe-ordl.part-dscr1 AT ROW 6 COL 17 COLON-ALIGNED
          LABEL "Description" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     oe-ordl.part-dscr2 AT ROW 6.95 COL 17 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     oe-ordl.price AT ROW 3.14 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     oe-ordl.pr-uom AT ROW 3.14 COL 104 COLON-ALIGNED
          LABEL "UOM" FORMAT "XXX"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     oe-ordl.cost AT ROW 6 COL 76 COLON-ALIGNED
          LABEL "Cost/M"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     oe-ordl.cas-cnt AT ROW 4.1 COL 104 COLON-ALIGNED
          LABEL "Count"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     oe-ordl.disc AT ROW 4.1 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     oe-ordl.t-price AT ROW 5.05 COL 76 COLON-ALIGNED
          LABEL "Total Price"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     oe-ordl.po-no AT ROW 8.14 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-ordl.po-no-po AT ROW 9.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-ordl.vend-no AT ROW 9.1 COL 51 COLON-ALIGNED
          LABEL "Board Vendor"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-ordl.req-code AT ROW 7.91 COL 76 COLON-ALIGNED
          LABEL "Priority"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     oe-ordl.req-date AT ROW 7.91 COL 107 COLON-ALIGNED
          LABEL "Due Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     oe-ordl.prom-code AT ROW 9.1 COL 76 COLON-ALIGNED
          LABEL "Priority"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     oe-ordl.prom-date AT ROW 9.1 COL 107 COLON-ALIGNED
          LABEL "Scheduled Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     oe-ordl.job-no AT ROW 1.24 COL 111 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ordl.job-no2 AT ROW 1.24 COL 122 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     oe-ordl.tax AT ROW 3.14 COL 120
          LABEL "Tax"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
     oe-ordl.s-man[1] AT ROW 11.48 COL 8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     oe-ordl.s-pct[1] AT ROW 11.48 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     oe-ordl.s-comm[1] AT ROW 11.48 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-ordl.s-man[2] AT ROW 12.48 COL 8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     oe-ordl.s-pct[2] AT ROW 12.48 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     oe-ordl.s-comm[2] AT ROW 12.48 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     oe-ordl.s-man[3] AT ROW 13.48 COL 8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     oe-ordl.s-pct[3] AT ROW 13.48 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     oe-ordl.s-comm[3] AT ROW 13.48 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     li-on-hand AT ROW 11.48 COL 74 COLON-ALIGNED
     li-on-order AT ROW 12.43 COL 74 COLON-ALIGNED
     li-alloc AT ROW 13.38 COL 74 COLON-ALIGNED
     li-backorder AT ROW 11.48 COL 106 COLON-ALIGNED
     li-avail AT ROW 12.43 COL 106 COLON-ALIGNED
     li-reorder AT ROW 13.38 COL 106 COLON-ALIGNED
     "Comm. %" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 10.76 COL 36
          FGCOLOR 9 
     "% of Sales" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 10.76 COL 21
          FGCOLOR 9 
     "FG Item Qtys" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 10.76 COL 61
          FGCOLOR 9 
     " -" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 9.81 COL 99
     "SalesRep" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 10.76 COL 9
          FGCOLOR 9 
     RECT-31 AT ROW 10.52 COL 8
     RECT-36 AT ROW 10.52 COL 59
     RECT-37 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-ordl
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
         HEIGHT             = 17.1
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */
DO TRANSACTION:
{src/adm/method/viewer.i}
END.

/*{methods/template/viewer.i}*/

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

/* SETTINGS FOR FILL-IN oe-ordl.cas-cnt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.cost IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ordl.est-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-ordl.i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.i-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.job-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.job-no2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-alloc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-avail IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-backorder IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-on-hand IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-on-order IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN li-reorder IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.part-dscr1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.part-dscr2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.part-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.pr-uom IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       oe-ordl.pr-uom:PRIVATE-DATA IN FRAME F-Main     = 
                "111".

/* SETTINGS FOR FILL-IN oe-ordl.prom-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.prom-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.qty IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.req-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.req-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-comm[1] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-comm[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.s-comm[3] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.t-price IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ordl.tax IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.vend-no IN FRAME F-Main
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
    def var char-val as cha no-undo.
    def var look-recid as recid no-undo. 

    find oe-ord where oe-ord.company = gcompany and oe-ord.ord-no = oe-ordl.ord-no
                   no-lock no-error.
    case focus:name :
         when "est-no" then do:
              run windows/l-est.w (g_company,g_loc,focus:screen-value, output char-val).
              if char-val <> "" then do:
                 run display-est-detail (char-val).
              end.                
         end.   
         when "i-no" then do:
              run windows/l-itemfg.w (gcompany, oe-ord.cust-no, focus:screen-value, output char-val, output look-recid).
              if char-val <> "" then do:
                 assign focus:screen-value = entry(1,char-val)
                        oe-ordl.i-name:screen-value = entry(2,char-val).
                 run display-fgitem .
                 apply "entry" to oe-ordl.price.
              end.                           
         end.
         when "part-no" then do:
              run windows/l-itemfp.w (gcompany, oe-ord.cust-no, focus:screen-value, output char-val, output look-recid).
              if char-val <> "" then do:
                 assign focus:screen-value = entry(1,char-val)
                        oe-ordl.part-dscr1:screen-value = entry(2,char-val)
                        oe-ordl.part-dscr2:screen-value = entry(3,char-val).
                 run display-fgpart (look-recid).
                 apply "entry" to oe-ordl.price.
              end.             
         end.
         when "sman" then do:
              run windows/l-sman.w (gcompany, output char-val).
              if char-val <> "" then do:
                 case focus:index:
                      when 1 then assign oe-ordl.s-man[1]:screen-value = entry(1,char-val)
                                         /*oe-ordl.sname[1]:screen-value = entry(2,char-val) */
                                         oe-ordl.s-comm[1]:screen-value = entry(3,char-val)
                                         v-margin = 0.
                      when 2 then assign oe-ordl.s-man[2]:screen-value = entry(1,char-val)
                                         /*oe-ordl.sname[2]:screen-value = entry(2,char-val)*/
                                         oe-ordl.s-comm[2]:screen-value = entry(3,char-val)
                                         .
                      when 3 then assign oe-ordl.s-man[3]:screen-value = entry(1,char-val)
                                        /* oe-ordl.sname[3]:screen-value = entry(2,char-val) */
                                         oe-ordl.s-comm[3]:screen-value = entry(3,char-val)
                                         .
                 end.
              end.
         end.  
         when "price" then do:       /* oe/history2.p */              
              run windows/l-report.w (gcompany,oe-ord.cust-no,oe-ordl.i-no:screen-value,oe-ordl.pr-uom:screen-value,output char-val).
              if char-val <> "" then do:
                focus:screen-value = entry(1,char-val).
                if oe-ordl.pr-uom:screen-value eq "" then oe-ordl.pr-uom:screen-value = "M".
              end.         
         end.
         when "pr-uom" then do:
              run windows/l-stduom.w (gcompany,lv-uom-list,oe-ordl.pr-uom:screen-value, output char-val).
              if char-val <> "" then focus:screen-value = entry(1,char-val).         
         end.
         when "req-code" or when "prom-code" then do:
              run windows/l-dcode.w (v-valdcode, output char-val).
              if char-val <> "" then assign focus:screen-value = entry(1,char-val).                                            
         end.
         when "pr-uom" then do:
              run windows/l-stduom.w (gcompany,lv-uom-list,oe-ordl.pr-uom:screen-value, output char-val).
              if char-val <> "" then focus:screen-value = entry(1,char-val).         
         end.
         when "vend-no" then do:
              run windows/l-vendno.w (gcompany,focus:screen-value, output char-val).
              if char-val <> "" then focus:screen-value = entry(1,char-val).         
         end.
         when "po-no-po" then do:
              run windows/l-ponopo.w (gcompany,yes,focus:screen-value, output char-val).
              if char-val <> "" then assign focus:screen-value = entry(1,char-val)
                                            oe-ordl.vend-no:screen-value = entry(2,char-val)  .         
         end.

    end case.
    return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.cas-cnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.cas-cnt V-table-Win
ON LEAVE OF oe-ordl.cas-cnt IN FRAME F-Main /* Count */
DO:
    if int(oe-ordl.cas-cnt:screen-value) > int(oe-ordl.qty:screen-value) then do:
       message "Unit count may not be greater than quantity." skip
               "Setting unit count to equal quantity. "
               view-as alert-box information.
       oe-ordl.cas-cnt:screen-value = oe-ordl.qty:screen-value.        
    end.
    {oe/ordltot.i oe-ordl qty oe-ordl}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.disc V-table-Win
ON LEAVE OF oe-ordl.disc IN FRAME F-Main /* Discount */
DO:
    if self:modified then do:
       oe-ordl.t-price:screen-value = string( (input oe-ordl.qty *
                         input oe-ordl.price) - round( ((input oe-ordl.qty *
                         input oe-ordl.price) * input oe-ordl.disc) / 100, 2) ).

    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.est-no V-table-Win
ON LEAVE OF oe-ordl.est-no IN FRAME F-Main /* Estimate # */
DO:
    if lastkey = -1 then return.
    {&methods/lValidateError.i YES}

    if oe-ordl.est-no:screen-value <> "" then do:
       v-est-no = oe-ordl.est-no:screen-value.
       run util/rjust.p (input-output v-est-no,8).
       oe-ordl.est-no:screen-value = v-est-no.
       find first est where est.est-no = oe-ordl.est-no:screen-value
                  no-lock no-error.
       if not avail est then do:
          message "Invalid Estimate#. Try help." view-as alert-box error.
          return no-apply.
       end.
       else do:
          find first eb where eb.company = cocode and
                              eb.est-no = oe-ordl.est-no:screen-value
                              no-lock no-error.
          if avail eb then run display-est-detail (string(recid(eb))).
       end.
    end.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.i-no V-table-Win
ON LEAVE OF oe-ordl.i-no IN FRAME F-Main /* FG Item# */
DO:
    if lastkey = -1 then return.

    def var ls-i-no as cha no-undo.
    def var ls-part-no as cha no-undo.
    def var ls-est-no as cha no-undo.
    def var ls-uom as cha no-undo.

 if self:modified and self:screen-value <> "0" then do:   
    run display-fgitem no-error.

    if return-value <> "" then return no-apply.

    if error-status:error then do: /* not avail itemfg */
     /*  message "This item does not exist, would you like to add it?" view-as alert-box question
               button yes-no update ll-ans as log.  
       if ll-ans then do:
     */
       assign ls-i-no = oe-ordl.i-no:screen-value
              ls-part-no = oe-ordl.part-no:screen-value
              ls-est-no = oe-ordl.est-no:screen-value
              ls-uom = oe-ordl.pr-uom:screen-value.

       run oe/d-citmfg.w (ls-est-no, input-output ls-i-no,
                          input-output ls-part-no,input-output ls-uom) no-error.
       if ls-i-no = "" then return no-apply.  /* cancel */

       if ls-i-no <> "" then do:   
          assign oe-ordl.i-no:screen-value = ls-i-no
                 oe-ordl.part-no:screen-value = ls-part-no.
          find xest where xest.company = gcompany 
                      and int(xest.est-no) = int(oe-ordl.est-no:screen-value)
                      no-lock no-error.
          if avail xest then do: 
             find xeb where xeb.company = gcompany and xeb.est-no = xest.est-no
                        and xeb.form-no = 0 no-lock no-error.
             if not avail xeb then find first xeb where xeb.company = gcompany and xeb.est-no = xest.est-no
                                                    and xeb.form-no = oe-ordl.form-no
                                                    and xeb.blank-no = oe-ordl.blank-no
                                                    no-lock no-error.
             if avail xeb then do:
                 find xef where xef.company = gcompany and xef.est-no = xeb.est-no
                            and xef.form-no = xeb.form-no
                            no-lock no-error.                 
                 run crt-itemfg (self:screen-value,"M").
             end.    
          end.   
          else do: /* no xest or oe-ordl.est-no = "" */
               run crt-itemfg (self:screen-value,ls-uom).
          end.
       end. 
       apply "entry" to oe-ordl.price.     
       return no-apply.
    end.   
 end. /* modified */   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.job-no V-table-Win
ON LEAVE OF oe-ordl.job-no IN FRAME F-Main /* Job Number */
DO:
   def var i as int no-undo.

   assign v-bld-job = "".
   do i = 1 to 6:
      if substring(input oe-ordl.job-no,i,1) ne " " then
             assign v-bld-job  = v-bld-job +     substring(input oe-ordl.job-no,i,1).
   end. /* 1 - 6 */
   assign oe-ordl.job-no:screen-value =
                   string(fill(" ",6 - length(v-bld-job))) +
                   (trim(v-bld-job)).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.job-no2 V-table-Win
ON LEAVE OF oe-ordl.job-no2 IN FRAME F-Main /* Run # */
DO:
    {&methods/lValidateError.i YES}
    run util/rjust.p (input-output v-bld-job, input 6).
    find first job-hdr where job-hdr.company = cocode and
                             job-hdr.job-no = v-bld-job and
                             job-hdr.job-no2 = input oe-ordl.job-no2
                     use-index job-no no-lock no-error.
    if avail job-hdr then do:
          message "   JOB NUMBER " + string(job-hdr.job-no) + "-" +
                   string(job-hdr.job-no2) + " has already been used."
                  view-as alert-box error  .
          return no-apply.
    end.        
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.part-no V-table-Win
ON LEAVE OF oe-ordl.part-no IN FRAME F-Main /* Cust Part # */
DO:
      if lastkey = -1 then return.
  {&methods/lValidateError.i YES}
  if self:modified then do:    
      find first itemfg where itemfg.company = gcompany 
                          and itemfg.part-no = oe-ordl.part-no:screen-value
                          and itemfg.cust-no = oe-ord.cust-no
                          no-lock no-error.
      if not avail itemfg then do:
         find first itemfg where itemfg.company = gcompany 
                          and itemfg.part-no = oe-ordl.part-no:screen-value
                          no-lock no-error.
         if not avail itemfg then do:
            message "Invalid Cust Part#. Try help. " view-as alert-box.
            return no-apply.
         end.
         else do:
            if itemfg.cust-no <> oe-ord.cust-no and itemfg.cust-no <> "" then do:
               find first cust where cust.company = gcompany and
                                     cust.cust-no = itemfg.cust-no
                                     no-lock no-error.
               if avail cust and cust.active <> "X" then do:                      
                  message "This item exists for a different customer!. Do you want to continue?"
                          view-as alert-box question button yes-no update ll-ans as log.
                  if not ll-ans then  return no-apply.       
               end.  
            end.   
         end.   
      end.                      
      run display-fgpart (recid(itemfg) )  no-error.
      if error-status:error then return no-apply.

      apply "entry" to oe-ordl.price.
      return no-apply.
  end.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.po-no-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.po-no-po V-table-Win
ON LEAVE OF oe-ordl.po-no-po IN FRAME F-Main /* Board PO # */
DO:
    if lastkey = -1 then return.
    {&methods/lValidateError.i YES}
    def var ld-cost as dec no-undo.

    if self:screen-value ne "0" then do:
       find first po-ord where po-ord.company eq cocode
                           and po-ord.po-no   eq input oe-ordl.po-no-po
                no-lock no-error.
       if not avail po-ord then do:
          message "You have entered an invalid Purchase Order."
                   view-as alert-box error .
          return no-apply.
       end.

       if oe-ordl.job-no ne "" then do:
          find first job-hdr where job-hdr.company eq cocode
                    and job-hdr.job-no  eq oe-ordl.job-no
                    and job-hdr.job-no2 eq oe-ordl.job-no2
                  no-lock no-error.
          if avail job-hdr then
              for each job-mat where job-mat.company eq cocode
                                 and job-mat.job-no  eq job-hdr.job-no
                                 and job-mat.job-no2 eq job-hdr.job-no2
                                 and job-mat.job eq job-hdr.job  no-lock,
                  first item where item.company  eq cocode
                               and item.i-no     eq job-mat.rm-i-no
                               and item.mat-type eq "B" no-lock:                 

                  find first po-ordl where po-ordl.company   eq cocode
                            and po-ordl.i-no      eq job-mat.rm-i-no
                            and po-ordl.po-no     eq po-ord.po-no
                            and po-ordl.item-type eq yes
                            use-index item-ordno no-lock no-error.
                if not avail po-ordl then do:
                   message "You have entered an invalid Purchase Order for this Item."
                             view-as alert-box error.
                   return no-apply.
                end.
              end. /* for each job-mat */
            end. /* job-no ne "" */
            else do:
              find first po-ordl  where po-ordl.company   eq cocode
                    and po-ordl.i-no      eq oe-ordl.i-no
                    and po-ordl.po-no     eq po-ord.po-no
                    and po-ordl.item-type eq no
                  use-index item-ordno no-lock no-error.
              if not avail po-ordl then do:
                  message "You have entered an invalid Purchase Order for this Item."
                          view-as alert-box error.
                  return no-apply.
              end.              
              if po-ordl.cons-uom eq "M" then
                   oe-ordl.cost:screen-value = string(po-ordl.cons-cost). 
              else do:                 
                 run sys/ref/convcuom.p (po-ordl.cons-uom, "M", 0, 0, 0, 0,
                                           po-ordl.cons-cost, output ld-cost).
                 oe-ordl.cost:screen-value = string(ld-cost).                          
              end.                             
            end.

          end.  
          {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.pr-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.pr-uom V-table-Win
ON LEAVE OF oe-ordl.pr-uom IN FRAME F-Main /* UOM */
DO:
    def var lv-out-cost as dec no-undo.

    if lastkey = -1 then return.
    {&methods/lValidateError.i YES}

    if oe-ordl.pr-uom:screen-value <> "" then do:
       find first uom where uom.uom eq oe-ordl.pr-uom:screen-value
                        and lookup(uom.uom,lv-uom-list) ne 0
                 no-lock no-error.
            if not avail uom then do:
               message " Invalid Unit Of Measure, Must Enter - C, CS, L, M, or EA "
                         view-as alert-box error.
               return no-apply.
            end.
    end.
    else oe-ordl.pr-uom:screen-value = "M".

    if oe-ordl.pr-uom:screen-value ne "M" then
        run sys/ref/convcuom.p(oe-ordl.pr-uom:screen-value, "M", 0, 0, 0, 0,
                               dec(oe-ordl.cost:screen-value), output lv-out-cost).
     assign oe-ordl.cost:screen-value = string(lv-out-cost).

    {oe/ordltot.i oe-ordl qty oe-ordl}
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.price V-table-Win
ON LEAVE OF oe-ordl.price IN FRAME F-Main /* Price */
DO:
/*    oe-ordl.t-price:screen-value = string(int(oe-ordl.qty:screen-value) * 
                                          dec(oe-ordl.price:screen-value) / 1000). 
*/
   {oe/ordltot.i oe-ordl qty oe-ordl}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.prom-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.prom-date V-table-Win
ON LEAVE OF oe-ordl.prom-date IN FRAME F-Main /* Scheduled Date */
DO:
    if self:modified then do:
       message "Change all promise dates on order? " 
               view-as alert-box question button yes-no update ll-ans as log.
       if ll-ans then lv-change-prom-date = yes.
       else lv-change-prom-date = no.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.qty V-table-Win
ON LEAVE OF oe-ordl.qty IN FRAME F-Main /* Quantity */
DO:
   if lastkey = -1 then return.
   def var lv-price as dec no-undo.
   def var lv-pr-uom as cha no-undo.
   def var v-tmp-part as cha no-undo.
   def var v-set as cha no-undo.
   def var v-qty as int no-undo.
   def var v-checkset as log no-undo.

   if dec(oe-ordl.qty:screen-value) = 0 then do:
      message "Quantity can not be 0. " view-as alert-box error.
      return no-apply.
   end.

   if int(oe-ordl.qty:screen-value) < int(oe-ordl.cas-cnt:screen-value) or
      int(oe-ordl.cas-cnt:screen-value) = 0
   then oe-ordl.cas-cnt:screen-value = oe-ordl.qty:screen-value. 

   find xoe-ord where xoe-ord.company = gcompany and
                       xoe-ord.ord-no = oe-ordl.ord-no
                       no-lock no-error.
   assign save_id = recid(oe-ordl)
          v-i-item = oe-ordl.i-no:screen-value
          v-i-qty = int(oe-ordl.qty:screen-value)
          .
   run oe/oe-price.p. 

   if oe-ordl.est-no:screen-value ne "" and not avail xest then
        find first xest where xest.company eq cocode and
                   xest.est-no eq input oe-ordl.est-no no-lock no-error.
   if avail xest and
      (xest.est-type eq 2 or xest.est-type eq 6) then do:
             assign fil_id = recid(oe-ordl).
             /*if nufile then old-qty = 0.*/
             run oe/fgadd2.p.   /** 2pc box fg create/update routine **/
   end.


   if avail xest and v-quo-price then do:
      assign lv-price = dec(oe-ordl.price:screen-value)
             lv-pr-uom = oe-ordl.pr-uom:SCREEN-VALUE
             v-i-qty   = DEC(oe-ordl.qty:SCREEN-VALUE).

      run oe/getqpric.p (recid(xest), oe-ordl.part-no,
                         v-tmp-part,
                         input-output lv-price,
                         input-output lv-pr-uom,
                         OUTPUT lv-q-no,
                         INPUT-OUTPUT v-i-qty).
      assign oe-ordl.price:screen-value = string(lv-price)
             oe-ordl.pr-uom:screen-value = lv-pr-uom
             oe-ordl.qty:SCREEN-VALUE = STRING(v-i-qty).
   end. 

    /* Begin Calculate Weight for Order */
    /* take old qty - new qty find weight and add to order */
    find first itemfg where itemfg.company eq cocode
                        and itemfg.i-no eq oe-ordl.i-no:screen-value no-lock no-error.
    if avail itemfg then do:
       /*assign oe-ordl.t-weight:screen-value = string(( oe-ordl.qty / 100 ) * itemfg.weight-100 )
              /*xoe-ord.t-weight = xoe-ord.t-weight +
                   (( oe-ordl.qty - old-qty ) / 100 ) * itemfg.weight-100  */
                   .
       */           
       if itemfg.isaset and itemfg.alloc NE YES and v-checkset then do:
          assign v-set = oe-ordl.i-no:screen-value
                 v-qty = int(oe-ordl.qty:screen-value).
          run fg/checkset.p (recid(itemfg), ?, input-output v-qty).
          if v-qty lt int(oe-ordl.qty:screen-value) then do:
                pause.
                hide frame checkset no-pause.
          end.
       end.
    end.
    run oe/oe-frtcl.p.  /* Calculate Freight  */

   {oe/ordltot.i oe-ordl qty oe-ordl}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.s-pct[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-pct[1] V-table-Win
ON LEAVE OF oe-ordl.s-pct[1] IN FRAME F-Main /* Pct of Sale[1] */
DO:
    {&methods/lValidateError.i YES}
    if int(self:screen-value) < 0 or int(self:screen-value) > 100 then do:
       message "Percent of Sales Out of Range (0 - 100). Please ReEnter. "
              view-as alert-box error.
       return no-apply.
    end.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.tax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.tax V-table-Win
ON VALUE-CHANGED OF oe-ordl.tax IN FRAME F-Main /* Tax */
DO:
    {&methods/lValidateError.i YES}
    if not avail oe-ord then find oe-ord where oe-ord.company = gcompany and
                                  oe-ord.ord-no = oe-ordl.ord-no no-lock no-error. 
    if self:screen-value = "yes" and oe-ord.tax-gr = "" then do:
       message "Invalid tax code on order header. " view-as alert-box error.
       return no-apply.
    end.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.vend-no V-table-Win
ON LEAVE OF oe-ordl.vend-no IN FRAME F-Main /* Board Vendor */
DO:
   {&methods/lValidateError.i YES}
  if self:screen-value <> "" then do:
     find first vend where vend.company eq cocode
                       and vend.vend-no eq input oe-ordl.vend-no
                no-lock no-error.
     if not avail vend then do:
         message "You have entered an invalid Vendor No.  Try help."
                   view-as alert-box error.
         return no-apply.
     end.
  end. 
   {&methods/lValidateError.i NO}  
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
DO TRANSACTION:
{ce/print4a.i "new shared" }
END.

/* ==== FOR REPRICE ===*/
define new shared var v-procat like oe-prmtx.procat no-undo. /* ITEM CATEGORY */
define new shared var v-price-lev as int no-undo.

session:data-entry-return = yes.
assign gcompany = g_company
       gloc = g_loc
       cocode = gcompany
       locode = gloc.

{sys/inc/f16to32.i}
  DO TRANSACTION:
     {sys/inc/graphic.i}
  END.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVPRINT"
      no-lock no-error.
  if avail sys-ctrl then
    ASSIGN
     v-print-head = sys-ctrl.log-fld
     v-print-fmt  = sys-ctrl.char-fld.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  v-rel = oe-ordl.rel.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-alloc-qty V-table-Win 
PROCEDURE calc-alloc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param v-alloc as int no-undo.
def var v-type as cha no-undo.

find first itemfg where itemfg.company = gcompany and
                        itemfg.i-no = oe-ordl.i-no
                        no-lock no-error.                          
v-alloc = 0.

for each oe-ordl where oe-ordl.company eq cocode
                   and oe-ordl.i-no    eq itemfg.i-no
            use-index item no-lock,   
    first oe-ord  where oe-ord.company eq cocode
          and oe-ord.ord-no  eq oe-ordl.ord-no
          and index("CZT",oe-ord.stat) eq 0
          and oe-ord.type ne "T"
          use-index ord-no no-lock:

    for each oe-rel where oe-rel.company eq cocode
          and oe-rel.ord-no  eq oe-ordl.ord-no
          and oe-rel.i-no    eq oe-ordl.i-no
          and oe-rel.line    eq oe-ordl.line
          use-index ord-item no-lock:

       {oe/rel-stat.i v-type} 
       if v-type eq "P" then v-alloc = v-alloc + oe-rel.qty.
       else 
       if v-type eq "Z" and not oe-ctrl.u-inv then
       for each inv-line FIELDS(ship-qty)
           where inv-line.company eq cocode
             and inv-line.ord-no  eq oe-boll.ord-no
             and inv-line.b-no    eq oe-boll.b-no
             and inv-line.i-no    eq oe-boll.i-no
             and inv-line.line    eq oe-boll.line
           no-lock:        
         v-alloc = v-alloc + inv-line.ship-qty.
       end.
    end.

    for each oe-relh  where oe-relh.company eq cocode
             and oe-relh.ord-no  eq oe-ordl.ord-no
             and oe-relh.posted  eq no
             and oe-relh.deleted eq no
             use-index order no-lock,    
        each oe-rell FIELDS(qty) where oe-rell.company eq cocode
             and oe-rell.r-no    eq oe-relh.r-no
             AND oe-rell.ord-no  EQ oe-ordl.ord-no
             and oe-rell.i-no    eq oe-ordl.i-no
             and oe-rell.line    eq oe-ordl.line
             no-lock:
         v-alloc = v-alloc + oe-rell.qty.  
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-release V-table-Win 
PROCEDURE create-release :
/*------------------------------------------------------------------------------
  Purpose:     from oe/oe-ordl.x for nufile
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

  find first sys-ctrl where sys-ctrl.company = cocode and
                            sys-ctrl.name = "jobcreat" 
                            no-lock no-error.
  v-create-job = if available sys-ctrl then sys-ctrl.log-fld else no.
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
                                no-lock no-error.
            if avail shipto then assign v-ship-id = shipto.ship-id.
         end.
         run oe/d-shipid.w (input oe-ordl.cust-no, input-output v-ship-id, INPUT-OUTPUT v-ship-from)  .
         assign oe-rel.ship-id = trim(v-ship-id).
         IF v-ship-from GT "" THEN
             oe-rel.spare-char-1 = v-ship-from.
         find first shipto where shipto.company = cocode and
                                  shipto.cust-no = xoe-ord.cust-no  and
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
                               else xoe-ord.carrier.
            RELEASE sys-ctrl.
         end.
         /* Run Freight calculation  */
         run oe/oe-frtcl.p.
      end.  /* multi ship to */
      else do:
           find first shipto where shipto.company eq cocode and
                                        shipto.cust-no eq xoe-ord.cust-no and
                                        shipto.ship-id eq v-ship-id
                                  no-lock no-error.
            if not avail shipto then
                 find first shipto where shipto.company eq cocode and
                                          shipto.cust-no eq xoe-ord.cust-no
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
                                     else xoe-ord.carrier.
               end.
            end. /* avail shipto */
      end. /* not multi */
  end. /* if no oe-rel */
  else do:
       find first shipto where shipto.company = cocode and
                               shipto.cust-no = xoe-ord.cust-no  and
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
                                    else xoe-ord.carrier.
                 RELEASE sys-ctrl.
           end.           
       end.           
  end.

  fil_id = recid(oe-ordl).
  if avail oe-ordl and (oe-ordl.est-no ne "" and oe-ordl.job-no eq "") then do:
       message " Since job number is blank, a job will not be created "
                  view-as alert-box.
  end.  
  else run oe/ordlup.p.         /* Update Inventory and Job Costing */

 if xoe-ord.est-no eq "" and oe-ordl.est-no ne "" then do:
      fil_id = recid(oe-ordl).
      run oe/estupl.p.
      v-qty-mod = no.
      fil_id = recid(oe-ordl).
      if avail oe-ordl and (oe-ordl.est-no ne "" and oe-ordl.job-no eq "")~ then do:
         message " Since job number is blank, a purchase order will not be create~d "
                 view-as alert-box .
      end.  
/*      else run po/do-po.p. */
      status default "".
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
/*
{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.
*/
def var cocode as cha no-undo.
def var locode as cha no-undo.
def var tmpstore as cha no-undo.
def var i as int no-undo.

def var v-alloc like itemfg.alloc init yes.
DEF BUFFER b-shipto FOR shipto.

assign cocode = gcompany
       locode = gloc
       .

{ce/msfcalc.i}

{oe/fgfreight.i} 
find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "SETPRINT"
    no-lock no-error.
if avail sys-ctrl then v-alloc = sys-ctrl.log-fld.

find first cust  where cust.company eq gcompany
                   and cust.cust-no eq xeb.cust-no
    no-lock no-error.

create itemfg.
assign
 itemfg.company    = gcompany
 itemfg.loc        = gloc
 itemfg.i-no       = v-item
 itemfg.i-code     = "C"
 itemfg.i-name     = oe-ordl.i-name:screen-value in frame {&frame-name}
 itemfg.part-dscr1 = oe-ordl.part-dscr1:screen-value
 itemfg.part-dscr2 = oe-ordl.part-dscr2:Screen-value
 itemfg.sell-price = dec(oe-ordl.price:screen-value)
 itemfg.sell-uom   = oe-ordl.pr-uom:screen-value
 itemfg.part-no    = oe-ordl.part-no:screen-value
 itemfg.cust-no    = oe-ord.cust-no
 itemfg.cust-name  = oe-ord.cust-name
 itemfg.pur-uom    = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.prod-uom   = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.alloc      = v-alloc
 itemfg.stocked    = YES
 itemfg.setupDate  = TODAY.
  /* Create an itemfg-loc for the default warehouse */
  RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").

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
           itemfg.alloc      = NOT xeb.set-is-assembled.

    {oe/fgfreighta.i xeb}

    FIND FIRST b-shipto WHERE
      b-shipto.company EQ cocode AND
      b-shipto.cust-no EQ xeb.cust-no AND
      b-shipto.ship-no EQ xeb.ship-no
      NO-LOCK NO-ERROR.

    IF AVAILABLE b-shipto THEN DO:
      itemfg.ship-meth = b-shipto.ship-meth.
      RELEASE b-shipto.
    END.

    {fg/set-inks1.i itemfg xeb}.

    {sys/inc/fgcascnt.i itemfg xeb}

     /* {sys/inc/updfgdim.i "xeb"} replaced with below (02211202) */
     RUN oe/updfgdim.p (INPUT ROWID(xeb), INPUT ROWID(itemfg)).
     FIND CURRENT xeb EXCLUSIVE-LOCK.
     FIND CURRENT itemfg EXCLUSIVE-LOCK.

 end.
 else  do:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-est-detail V-table-Win 
PROCEDURE display-est-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-char as cha no-undo.
  def var ls-qty as CHAR no-undo.
  def var lv-price as dec no-undo.
  def var lv-pr-uom as cha no-undo.
  DEF VAR lv-qty AS DEC NO-UNDO.

  find first eb where string(recid(eb)) = ip-char no-lock no-error.
  if avail eb then do WITH FRAME {&Frame-name} :
     find first est-qty where est-qty.company = eb.company
                     and est-qty.est-no = eb.est-no
          no-lock no-error.
     ls-qty = if avail est-qty then string(est-qty.eqty) else "".
     assign oe-ordl.est-no:screen-value = eb.est-no
            oe-ordl.i-no:screen-value = eb.stock-no
            oe-ordl.part-no:screen-value = eb.part-no
            oe-ordl.i-name:screen-value = eb.part-dscr1 
            oe-ordl.part-dscr1:screen-value = eb.part-dscr1
            oe-ordl.part-dscr2:screen-value = eb.part-dscr2
            oe-ordl.qty:screen-value = ls-qty
            .
  end.
  /* get price from quote */  
  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "QUOPRICE"
       no-lock no-error.
  ASSIGN
     v-quo-price = if avail sys-ctrl then sys-ctrl.log-fld else NO
     lv-price = dec(oe-ordl.price:screen-value)
     lv-qty   = DEC(oe-ordl.qty:SCREEN-VALUE).

  if avail xest and v-quo-price then 
     run oe/getqpric.p (recid(xest), oe-ordl.part-no:screen-value, "",
                        input-output lv-price,
                        input-output lv-pr-uom,
                        OUTPUT lv-q-no,
                        INPUT-OUTPUT lv-qty).
  if lv-pr-uom <> "" then oe-ordl.pr-uom:screen-value = string(lv-pr-uom).
  ASSIGN
     oe-ordl.price:screen-value = string(lv-pr-uom)
     oe-ordl.qty:SCREEN-VALUE   = STRING(lv-qty).
  if lv-pr-uom = "M" then oe-ordl.t-price:screen-value = oe-ordl.price:screen-value.  /* for "M" */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-fgitem V-table-Win 
PROCEDURE display-fgitem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var li-cnt as int no-undo.
  def var lv-out-cost as dec no-undo.
  def var x as int no-undo.
  DEF VAR li-unit AS INT NO-UNDO.

  find first itemfg where itemfg.company = gcompany and
                          itemfg.i-no = oe-ordl.i-no:screen-value in frame {&frame-name}
                          no-lock no-error.                          
  if not avail itemfg then return error.

  run validate-fgitem no-error.
  if error-status:error then return "fail validation".

  if not avail oe-ord then find oe-ord where oe-ord.company = gcompany and
                                oe-ord.ord-no = oe-ordl.ord-no no-lock. 

  /* ====== oe/oedlfg.i ===========*/
  if itemfg.class eq "*" then oe-ordl.disc:screen-value = "0".
  if oe-ord.type eq "O" and oe-ordl.est-no ne "" then
     assign oe-ordl.i-name:screen-value     = itemfg.i-name
            oe-ordl.i-no:screen-value       = itemfg.i-no
            oe-ordl.part-dscr2:screen-value = itemfg.part-dscr2.
  else assign oe-ordl.i-name:screen-value = itemfg.i-name
              oe-ordl.i-no:screen-value   = itemfg.i-no
              oe-ordl.price:screen-value  = string(itemfg.sell-price)
              oe-ordl.pr-uom:screen-value = itemfg.sell-uom
              oe-ordl.cas-cnt:screen-value = string(itemfg.case-count)
              oe-ordl.part-dscr2:screen-value = itemfg.part-dscr2.

  if oe-ordl.est-no:screen-value eq "" then
     assign oe-ordl.part-no:screen-value    = itemfg.part-no
            oe-ordl.part-dscr1:screen-value = itemfg.part-dscr1
            oe-ordl.cas-cnt:screen-value    = string(itemfg.case-count).

  else do:
      run oe/oe-cnt.p(recid(oe-ordl), output li-cnt, output li-unit).
      assign
       oe-ordl.cas-cnt:screen-value    = string(li-cnt).
  end.

  if oe-ordl.job-no:screen-value = "" then do:
     find first po-ordl where po-ordl.company   eq cocode
                          and po-ordl.i-no      eq oe-ordl.i-no:screen-value
                          and po-ordl.po-no     eq int(oe-ordl.po-no-po:screen-value)
                          and po-ordl.item-type eq no
                          use-index item-ordno no-lock no-error.
     if avail po-ordl then
        assign oe-ordl.pr-uom:screen-value = po-ordl.cons-uom
               oe-ordl.cost:screen-value   = string(po-ordl.cons-cost).
     else assign oe-ordl.pr-uom:screen-value = itemfg.prod-uom
                 oe-ordl.cost:screen-value   = string(itemfg.total-std-cost).

     if oe-ordl.pr-uom ne "M" then
        run sys/ref/convcuom.p(oe-ordl.pr-uom:screen-value, "M", 0, 0, 0, 0,
                               dec(oe-ordl.cost:screen-value), output lv-out-cost).
     assign oe-ordl.cost:screen-value = string(lv-out-cost)
            oe-ordl.pr-uom:screen-value = itemfg.sell-uom.                        
  end.
  /* ======= end of oe/ordlfg.i ========*/

  if v-use-rel then run calc-alloc-qty (output li-alloc).
  else li-alloc = itemfg.q-alloc.

  if avail itemfg then assign li-on-hand = itemfg.q-onh
                              li-on-order = itemfg.q-ono
                              /*li-alloc = itemfg.q-alloc */
                              li-backorder = itemfg.q-back
                              li-avail = itemfg.q-onh + itemfg.q-ono - li-alloc
                              li-reorder = itemfg.ord-level.
  else assign li-on-hand = 0
              li-on-order = 0
              li-alloc = 0
              li-backorder = 0
              li-avail = 0
              li-reorder = 0.
 display li-on-hand li-on-order li-alloc 
              li-backorder 
              li-avail 
              li-reorder with frame {&frame-name}.

 li-reorder:bgcolor = ?.            

 if (itemfg.q-onh + itemfg.q-ono - li-alloc) <= itemfg.ord-level and
    itemfg.ord-level > 0 then do:
    assign li-reorder:bgcolor = 12.   
 end.

 if itemfg.isaset and itemfg.t-sqft eq 0 AND
     CAN-FIND(FIRST fg-set WHERE fg-set.company EQ cocode
                             AND fg-set.set-no  EQ itemfg.i-no
                             AND fg-set.part-no NE fg-set.set-no) then
   run fg/updsetdm.p (recid(itemfg)).

 if oe-ordl.est-no:screen-value eq "" then do:
    if v-upd-comm then do:               
       {oe/oescomm.i oe-ordl.s-man[1]:screen-value 1}
       {oe/oescomm.i oe-ordl.s-man[1]:screen-value 2}
       {oe/oescomm.i oe-ordl.s-man[1]:screen-value 3}              
    end.
 end.
 assign save_id = recid(oe-ordl)
        v-i-item = oe-ordl.i-no:screen-value
        v-i-qty = int(oe-ordl.qty:screen-value)
        .
 run oe/oe-price.p. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-fgpart V-table-Win 
PROCEDURE display-fgpart :
/*------------------------------------------------------------------------------
  Purpose:     oe/ordlfg.i
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-recid as recid no-undo.
  def var lv-out-cost as dec no-undo.
  def var li-cnt LIKE oe-ordl.cas-cnt no-undo.
  def var li-unit LIKE oe-ordl.cases-unit no-undo.
  def var x as int no-undo.

run validate-fgitem no-error.
if error-status:error then return error.

do with frame {&frame-name}:  
  find itemfg where recid(itemfg) = ip-recid no-lock.
  if itemfg.class eq "*" then oe-ordl.disc:screen-value = "0".
  if oe-ord.type eq "O" and oe-ordl.est-no ne "" then
     assign oe-ordl.i-name:screen-value     = itemfg.i-name
            oe-ordl.i-no:screen-value       = itemfg.i-no
            oe-ordl.part-dscr2:screen-value = itemfg.part-dscr2.
  else assign oe-ordl.i-name:screen-value = itemfg.i-name
              oe-ordl.i-no:screen-value   = itemfg.i-no
              oe-ordl.price:screen-value  = string(itemfg.sell-price)
              oe-ordl.pr-uom:screen-value = itemfg.sell-uom
              oe-ordl.cas-cnt:screen-value = string(itemfg.case-count)
              oe-ordl.part-dscr2:screen-value = itemfg.part-dscr2.

  if oe-ordl.est-no:screen-value eq "" then
     assign oe-ordl.part-no:screen-value    = itemfg.part-no
            oe-ordl.part-dscr1:screen-value = itemfg.part-dscr1
            oe-ordl.cas-cnt:screen-value    = string(itemfg.case-count).

  else do:
      run oe/oe-cnt.p(recid(oe-ordl), output li-cnt, output li-unit).
      assign
       oe-ordl.cas-cnt:screen-value    = string(li-cnt).
  end.

  if oe-ordl.job-no:screen-value = "" then do:
     find first po-ordl where po-ordl.company   eq cocode
                          and po-ordl.i-no      eq oe-ordl.i-no:screen-value
                          and po-ordl.po-no     eq int(oe-ordl.po-no-po:screen-value)
                          and po-ordl.item-type eq no
                          use-index item-ordno no-lock no-error.
     if avail po-ordl then
        assign oe-ordl.pr-uom:screen-value = po-ordl.cons-uom
               oe-ordl.cost:screen-value   = string(po-ordl.cons-cost).
     else assign oe-ordl.pr-uom:screen-value = itemfg.prod-uom
                 oe-ordl.cost:screen-value   = string(itemfg.total-std-cost).

     if oe-ordl.pr-uom ne "M" then
        run sys/ref/convcuom.p(oe-ordl.pr-uom:screen-value, "M", 0, 0, 0, 0,
                               dec(oe-ordl.cost:screen-value), output lv-out-cost).
     assign oe-ordl.cost:screen-value = string(lv-out-cost)
            oe-ordl.pr-uom:screen-value = itemfg.sell-uom.                        
  end.

end.  /* frame {&frame-name} */

  /* ====== from display-fgitem like oe/fg-qtys.p  =========*/  
  if v-use-rel then run calc-alloc-qty (output li-alloc).
  else li-alloc = itemfg.q-alloc.
  if avail itemfg then assign li-on-hand = itemfg.q-onh
                              li-on-order = itemfg.q-ono
                              /*li-alloc = itemfg.q-alloc */
                              li-backorder = itemfg.q-back
                              li-avail = itemfg.q-onh + itemfg.q-ono - li-alloc
                              li-reorder = itemfg.ord-level.
  else assign li-on-hand = 0
              li-on-order = 0
              li-alloc = 0
              li-backorder = 0
              li-avail = 0
              li-reorder = 0.
  display li-on-hand li-on-order li-alloc 
              li-backorder 
              li-avail 
              li-reorder with frame {&frame-name}.

  li-reorder:bgcolor = ?.            
  if (itemfg.q-onh + itemfg.q-ono - li-alloc) <= itemfg.ord-level and
     itemfg.ord-level > 0 then do:
     assign li-reorder:bgcolor = 12.   
  end.
  /* =========== end display qtys ============*/

 if itemfg.isaset and itemfg.t-sqft eq 0 AND
     CAN-FIND(FIRST fg-set WHERE fg-set.company EQ cocode
                             AND fg-set.set-no  EQ itemfg.i-no
                             AND fg-set.part-no NE fg-set.set-no) then
   run fg/updsetdm.p (recid(itemfg)).

 if oe-ordl.est-no:screen-value eq "" then do:
    if v-upd-comm then do:               
       {oe/oescomm.i oe-ordl.s-man[1]:screen-value 1}
       {oe/oescomm.i oe-ordl.s-man[1]:screen-value 2}
       {oe/oescomm.i oe-ordl.s-man[1]:screen-value 3}              
    end.
 end.
 assign save_id = recid(oe-ordl)
        v-i-item = oe-ordl.i-no:screen-value
        v-i-qty = int(oe-ordl.qty:screen-value)
        .
 run oe/oe-price.p. 

 if oe-ordl.qty:screen-value = "0" or oe-ordl.qty:screen-value = "" then
    apply "entry" to oe-ordl.qty.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-fgqtys V-table-Win 
PROCEDURE display-fgqtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find first itemfg where itemfg.company = gcompany and
                          itemfg.i-no = oe-ordl.i-no:screen-value in frame {&frame-name}
                          no-lock no-error.                          
  if not avail itemfg then return error.

  if v-use-rel then run calc-alloc-qty (output li-alloc).
  else li-alloc = itemfg.q-alloc.

  if avail itemfg then assign li-on-hand = itemfg.q-onh
                              li-on-order = itemfg.q-ono
                              /*li-alloc = itemfg.q-alloc */
                              li-backorder = itemfg.q-back
                              li-avail = itemfg.q-onh + itemfg.q-ono - li-alloc
                              li-reorder = itemfg.ord-level.
  else assign li-on-hand = 0
              li-on-order = 0
              li-alloc = 0
              li-backorder = 0
              li-avail = 0
              li-reorder = 0.
 display li-on-hand li-on-order li-alloc 
              li-backorder 
              li-avail 
              li-reorder with frame {&frame-name}.

 li-reorder:bgcolor = ?.            
 if (itemfg.q-onh + itemfg.q-ono - li-alloc) <= itemfg.ord-level and
    itemfg.ord-level > 0 then do:
    assign li-reorder:bgcolor = 12.   
 end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer xoe-ordl for oe-ordl.

  /* Code placed here will execute PRIOR to standard behavior. */
  if not avail oe-ord then 
     find oe-ord where oe-ord.company = gcompany and oe-ord.ord-no = oe-ordl.ord-no
                   no-lock no-error.

  nufile = adm-new-record.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  find xoe-ord where recid(xoe-ord) = recid(oe-ord).
  find first itemfg where itemfg.company eq cocode
                      and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
  if avail itemfg then do:
       assign oe-ordl.t-weight = ( oe-ordl.qty / 100 ) * itemfg.weight-100 
              xoe-ord.t-weight = xoe-ord.t-weight +
                   (( oe-ordl.qty /* - old-qty */) / 100 ) * itemfg.weight-100  
                   .
  end.               

  if lv-change-prom-date then do:  
     for each xoe-ordl where xoe-ordl.company eq gcompany
                         and xoe-ordl.ord-no eq oe-ord.ord-no
                        and recid(xoe-ordl) ne recid(oe-ordl):
         assign xoe-ordl.prom-date = oe-ordl.prom-date.
     end.
  end.

  if /*adm-new-record and not */ adm-adding-record then run create-release.  

  oe-ordl.q-qty = v-margin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-ordl for oe-ordl.
  def var z as int no-undo.
  def var i as int no-undo.
  def var char-hdl as cha no-undo.
  def var lv-ord-rowid as rowid no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  if not avail oe-ordl then do:  /* first ordl record creation */
       run get-link-handle in adm-broker-hdl (this-procedure, "record-source", output char-hdl).
       run get-ord-no in widget-handle(char-hdl) (output lv-ord-rowid).
       find oe-ord where rowid(oe-ord) = lv-ord-rowid no-lock no-error.
  end.  
  else find first oe-ord where oe-ord.ord-no = oe-ordl.ord-no no-lock.

  if avail oe-ord then do:
   z = 1.
   find last bf-ordl of oe-ord no-lock no-error.
   if avail bf-ordl then z = bf-ordl.line + 1.
  end.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  lv-ordl-recid = recid(oe-ordl).  /* progress bug for first record creation */
  if avail oe-ord then do:
   find first cust {sys/ref/custW.i} and cust.cust-no = oe-ord.cust-no
                   use-index cust no-lock.
   assign
    oe-ordl.company   = cocode
    oe-ordl.ord-no    = oe-ord.ord-no
    oe-ordl.cust-no   = oe-ord.cust-no
    oe-ordl.line      = z
    oe-ordl.po-no     = oe-ord.po-no
    oe-ordl.req-code  = oe-ord.due-code
    oe-ordl.req-date  = oe-ord.due-date
    oe-ordl.prom-code = oe-ord.due-code
    oe-ordl.prom-date = oe-ord.due-date
    oe-ordl.disc      = cust.disc
    oe-ordl.tax       = fGetTaxable(oe-ord.company, oe-ord.cust-no, oe-ord.ship-id, "").
/* already created 
   if oe-ord.est-no ne "" then do:
      for each work-ordl where work-ordl.cust-no eq oe-ord.cust-no
                            by work-ordl.form-no
        by work-ordl.blank-no:

       assign
        oe-ordl.i-name     = work-ordl.i-name
        oe-ordl.part-no    = work-ordl.part-no
        oe-ordl.part-dscr1 = work-ordl.part-dscr1
        oe-ordl.part-dscr2 = work-ordl.part-dscr2
        oe-ordl.i-no       = work-ordl.i-no
        oe-ordl.qty        = work-ordl.qty
        oe-ordl.i-no       = work-ordl.i-no
        oe-ordl.est-no     = work-ordl.est-no
        oe-ordl.est-type   = work-ordl.est-type
        oe-ordl.t-weight   = work-ordl.t-weight
        oe-ordl.t-freight  = work-ordl.t-freight
        oe-ordl.cost       = work-ordl.cost
        oe-ordl.price      = work-ordl.price
        oe-ordl.pr-uom     = work-ordl.pr-uom
        oe-ordl.cas-cnt    = work-ordl.cas-cnt
        oe-ordl.blank-no   = work-ordl.blank-no
        oe-ordl.form-no    = work-ordl.form-no.

       delete work-ordl.

       z = 99.

       leave.
     end.

     if z eq 1 then
        assign oe-ordl.est-no   = oe-ord.est-no
               oe-ordl.est-type = oe-ord.est-type.
   end. 
======*/    

   if oe-ord.est-no ne "" then
      assign
       oe-ordl.job-no  = oe-ord.job-no
       oe-ordl.job-no2 = oe-ord.job-no2
       oe-ordl.est-no   = oe-ord.est-no
       oe-ordl.est-type = oe-ord.est-type.


   do i = 1 to 3:
     assign
      oe-ordl.s-man[i]  = oe-ord.sman[i]
      oe-ordl.s-pct[i]  = oe-ord.s-pct[i]
      oe-ordl.s-comm[i] = oe-ord.s-comm[i].
   end.

   ASSIGN
      oe-ordl.q-qty = oe-ord.t-fuel
      v-margin = oe-ord.t-fuel.

   display {&DISPLAYED-FIELDS} with frame {&frame-name}.   

  end. /* avail oe-ord */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  message "Delete Currently Selected Record?" view-as alert-box question
          button yes-no update ll-ans as log.
  if not ll-ans then return.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  if not avail oe-ordl then do: 
   /*  def var char-hdl as cha no-undo.
     run get-link-handle in adm-broker-hdl(this-procedure, "tableio-source", output char-hdl).
     run notify in widget-handle(char-hdl) ('add-record').
   */  
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

     return.  
  end.

  find first sys-ctrl where sys-ctrl.company eq gcompany
                      and sys-ctrl.name    eq "OEREORDR"
       no-lock no-error.
  if not avail sys-ctrl then DO:
     create sys-ctrl.
     assign sys-ctrl.company = gcompany
            sys-ctrl.name    = "OEREORDR"
            sys-ctrl.descrip = "Use Actual Releases to calculate Qty Allocated in OE?"
            sys-ctrl.log-fld = no.
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
  v-use-rel = sys-ctrl.log-fld.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run display-fgqtys.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE order-disable V-table-Win 
PROCEDURE order-disable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if not avail oe-ordl then return.
find oe-ord of oe-ordl no-lock no-error.
do with frame {&frame-name}:
   if oe-ordl.est-no <> "" then do:
      disable oe-ordl.est-no with frame {&frame-name}.
      find first eb where eb.est-no = oe-ordl.est-no and 
                       eb.part-no = oe-ordl.part-no no-lock no-error.
      if avail eb and eb.stock-no <> "" then disable oe-ordl.i-no with frame {&frame-name}.
   end.   
   if avail oe-ord and oe-ord.type <> "O" then  disable oe-ordl.part-no with frame {&frame-name}.
   if oe-ordl.est-type = 4 then disable oe-ordl.qty .   
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-his V-table-Win 
PROCEDURE select-his :
/*------------------------------------------------------------------------------
  Purpose:     from oe/history1.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find xoe-ord where xoe-ord.company = oe-ordl.company and
                     xoe-ord.ord-no = oe-ordl.ord-no no-lock no-error.

  find first cust {sys/ref/custW.i} and
                  cust.cust-no eq xoe-ord.cust-no
                  use-index cust no-lock no-error.

  def var char-hdl as cha no-undo.
  run get-link-handle in adm-broker-hdl (this-procedure,"container-source",output char-hdl).
  run init-history in widget-handle(char-hdl) (this-procedure).

  run dispatch ('open-query').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-price V-table-Win 
PROCEDURE select-price :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find xoe-ord where xoe-ord.company = oe-ordl.company and
                     xoe-ord.ord-no = oe-ordl.ord-no no-lock no-error.

  assign v-i-qty = 0
         v-price-lev = 0.
  /* Get appropriate level */
  run oe/oe-level.p (BUFFER xoe-ord, OUTPUT v-price-lev).

  repeat:
       message "What Level should the Items be Repriced At?" update v-price-lev .
       if v-price-lev le 0 or v-price-lev ge 11 then do:
         message "Level must be Between 1 and 10.  Please ReEnter." view-as alert-box error.
         next.
       end.
       leave.
   end.

   run oe/oe-repr1.p (BUFFER xoe-ord, v-price-lev).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-stat V-table-Win 
PROCEDURE select-stat :
/*------------------------------------------------------------------------------
  Purpose:     from oe/credit.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find xoe-ord where xoe-ord.company = oe-ordl.company and
                     xoe-ord.ord-no = oe-ordl.ord-no no-lock no-error.

  find first cust {sys/ref/custW.i} and
                  cust.cust-no eq xoe-ord.cust-no
                  use-index cust no-lock no-error.

  /*run oe/d-credit.w./* (cocode,cust.cust-no).*/ */
  def var char-hdl as cha no-undo.
  run get-link-handle in adm-broker-hdl (this-procedure,"container-source",output char-hdl).
  run init-credit-inq in widget-handle(char-hdl) (this-procedure).

  run dispatch ('open-query').


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
  {src/adm/template/snd-list.i "oe-ordl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-fgitem V-table-Win 
PROCEDURE validate-fgitem :
/*------------------------------------------------------------------------------
  Purpose:     from oe/oe-ordlu.i
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var v-est-no as cha no-undo.
def buffer xeb for eb.
def buffer xoe-ordl for oe-ordl.
  {methods/lValidateError.i YES}
{oe/oe-sysct.i}
if not avail oe-ord then do:
   if not avail oe-ordl then find oe-ordl where recid(oe-ordl) = lv-ordl-recid no-lock.
   find oe-ord where oe-ord.company = gcompany and
                     oe-ord.ord-no = oe-ordl.ord-no no-lock no-error.
end.
/*
message "validate avail oe-ord? " avail oe-ord 
        "avial ordl? " avail oe-ordl  lv-ordl-recid view-as alert-box.
*/
do with frame {&frame-name} :
   if oe-ordl.est-no:screen-value ne "" then do:
       v-est-no = input oe-ordl.est-no.
       run util/rjust.p (input-output v-est-no,input 8).
       find first xeb where xeb.company   eq cocode
                        and xeb.est-no    eq v-est-no
                        and xeb.cust-no   eq oe-ord.cust-no
                        and xeb.form-no   ne 0
                        and (xeb.est-type eq 1 or xeb.est-type eq 5)
                        use-index est-no no-lock no-error.
       if avail xeb                          and
          xeb.stock-no ne ""                 and
          xeb.stock-no ne input oe-ordl.i-no then do:
              message "Item # must match Estimate's Item #" view-as alert-box error.         
              display xeb.stock-no @ oe-ordl.i-no.
              apply "entry" to oe-ordl.i-no.
              return error.
       end.
   end.

   find first xoe-ordl where xoe-ordl.company eq oe-ordl.company and
              xoe-ordl.ord-no eq oe-ordl.ord-no and
              xoe-ordl.i-no eq input oe-ordl.i-no /* and
              xoe-ordl.est-no eq input oe-ordl.est-no*/ no-lock no-error.
   if avail xoe-ordl and (recid(xoe-ordl) ne recid(oe-ordl)) then do:
      assign v-dup-item = no.
      message " This Item Has Already Been Entered Previously On This Order! "
              view-as alert-box error.
      assign v-dup-item = yes.
      return error.
   end.

   find first itemfg where itemfg.company = cocode and itemfg.i-no eq input oe-ordl.i-no
               no-lock no-error.
   if avail itemfg then do:
      if not v-est-fg                  and
         avail xeb                     and
         itemfg.part-no ne xeb.part-no then do:
         message " FG customer part number does not match"
                       "estimate's, continue?" 
                  view-as alert-box warning button yes-no update choice as log.
         if not choice then return error.
      end.
   end.
   if not avail itemfg and
     (oe-ordl.i-no:screen-value ne "0") then return error.     
   if not avail itemfg and
      (oe-ordl.i-no:screen-value = "0") then do:
      message "Invalid FG Item#. Try help. " view-as alert-box error.
      return error.
   end. /* not avail */

end. /* frame {&frame-name} */
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTaxable V-table-Win
FUNCTION fGetTaxable RETURNS LOGICAL PRIVATE
  ( ipcCompany AS CHARACTER, ipcCust AS CHARACTER , ipcShipto AS CHARACTER, ipcFGItemID AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Gets the Taxable flag based on inputs
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.

RUN system\TaxProcs.p (ipcCompany, ipcCust, ipcShipto, ipcFGItemID, OUTPUT lTaxable).  
RETURN lTaxable.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


