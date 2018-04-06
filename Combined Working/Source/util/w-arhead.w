&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: util/w-arhead.w
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

DEF VAR v-invno AS INT                       NO-UNDO.
DEF VAR v-pos  AS INT                        NO-UNDO.
DEF VAR v-reccnt AS INT                      NO-UNDO.
DEF VAR ls AS CHAR                           NO-UNDO.
DEF VAR v-line-count AS INT                  NO-UNDO.
DEF VAR v-start-pos AS INT INIT 1            NO-UNDO.
DEF VAR li AS INT                            NO-UNDO.
DEF VAR fg-uom-list AS cha                   NO-UNDO.
DEF VAR ll-calc-disc-FIRST AS LOG            NO-UNDO.
DEF VAR v-format LIKE sys-ctrl.char-fld      NO-UNDO.
DEF VAR v-cost AS DEC EXTENT 4               NO-UNDO.
DEF VAR v-basis LIKE sman.commbasis INIT ""  NO-UNDO.
DEF VAR v-ref-ar AS INT                      NO-UNDO.
DEF VAR v-ref-inv AS INT                     NO-UNDO.
DEF VAR v-ref-arl AS INT                     NO-UNDO.
DEF VAR v-tax AS DEC                         NO-UNDO.
DEF VAR xx-amt LIKE inv-head.t-inv-rev       NO-UNDO.
DEF VAR xx-cost1 AS DEC                      NO-UNDO.
DEF VAR xx-cost2 AS DEC                      NO-UNDO.
DEF VAR v-tot-inv-rev AS DEC                 NO-UNDO.
DEF VAR v-last-inv LIKE ar-inv.inv-no        NO-UNDO.
DEF VAR v-tr-date LIKE gltrans.tr-date       NO-UNDO.
DEF VAR v-ord-lines AS INT                   NO-UNDO.
DEF VAR v-ar-lines  AS INT                   NO-UNDO.
DEF VAR v-ar-sales LIKE ar-invl.actnum       NO-UNDO.

DEF BUFFER b-inv-line FOR inv-line.
DEF BUFFER b-oe-ordl  FOR oe-ordl.
DEF BUFFER b-ar-invl  FOR ar-invl.



{methods/defines/hndldefs.i}
/* {methods/prgsecur.i} */

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-inv

/* Definitions for FRAME fMain                                          */
&Scoped-define SELF-NAME fMain
&Scoped-define QUERY-STRING-fMain FOR EACH ar-inv SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY {&SELF-NAME} FOR EACH ar-inv SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain ar-inv
&Scoped-define FIRST-TABLE-IN-QUERY-fMain ar-inv


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ar-inv.addr[1] ar-inv.addr[2] ~
ar-inv.bank-code ar-inv.cust-name ar-inv.city ar-inv.company ar-inv.contact ~
ar-inv.curr-code[1] ar-inv.curr-code[2] ar-inv.check-no ar-inv.disc-% ~
ar-inv.disc-days ar-inv.disc-taken ar-inv.due ar-inv.due-code ~
ar-inv.due-date ar-inv.e-num ar-inv.bill-to ar-inv.est-no ar-inv.est-type ~
ar-inv.ex-rate ar-inv.exported ar-inv.f-bill ar-inv.fob-code ~
ar-inv.fr-out-c ar-inv.fr-out-m ar-inv.freight ar-inv.freq-code ~
ar-inv.frt-pay ar-inv.fuel ar-inv.fuel-bill ar-inv.gross ar-inv.inv-date ~
ar-inv.inv-no ar-inv.job-no ar-inv.job-no2 ar-inv.last-date ~
ar-inv.lead-days ar-inv.loc ar-inv.net ar-inv.ord-date ar-inv.ord-no ~
ar-inv.over-pct ar-inv.paid ar-inv.pay-date ar-inv.period ar-inv.po-no ~
ar-inv.pord-no ar-inv.posted ar-inv.printed ar-inv.prod-date ar-inv.recur ~
ar-inv.ship-id ar-inv.sman-no ar-inv.sold-addr[1] ar-inv.sold-addr[2] ~
ar-inv.sold-city ar-inv.sold-id ar-inv.stat ar-inv.state ar-inv.t-comm ~
ar-inv.t-cost ar-inv.t-disc ar-inv.t-sales ar-inv.t-weight ar-inv.tax-amt ~
ar-inv.tax-code ar-inv.terms ar-inv.terms-d ar-inv.tot-ord ar-inv.type ~
ar-inv.under-pct ar-inv.upd-date ar-inv.upd-time ar-inv.user-id ar-inv.zip ~
ar-inv.sold-name ar-inv.cust-no ar-inv.carrier ar-inv.sold-no ~
ar-inv.sold-state ar-inv.sold-zip 
&Scoped-define ENABLED-TABLES ar-inv
&Scoped-define FIRST-ENABLED-TABLE ar-inv
&Scoped-Define ENABLED-OBJECTS begin_inv btn-delete btn-exit btn-cancel ~
btn-update RECT-18 
&Scoped-Define DISPLAYED-FIELDS ar-inv.addr[1] ar-inv.addr[2] ~
ar-inv.bank-code ar-inv.cust-name ar-inv.city ar-inv.company ar-inv.contact ~
ar-inv.curr-code[1] ar-inv.curr-code[2] ar-inv.check-no ar-inv.disc-% ~
ar-inv.disc-days ar-inv.disc-taken ar-inv.due ar-inv.due-code ~
ar-inv.due-date ar-inv.e-num ar-inv.bill-to ar-inv.est-no ar-inv.est-type ~
ar-inv.ex-rate ar-inv.exported ar-inv.f-bill ar-inv.fob-code ~
ar-inv.fr-out-c ar-inv.fr-out-m ar-inv.freight ar-inv.freq-code ~
ar-inv.frt-pay ar-inv.fuel ar-inv.fuel-bill ar-inv.gross ar-inv.inv-date ~
ar-inv.inv-no ar-inv.job-no ar-inv.job-no2 ar-inv.last-date ~
ar-inv.lead-days ar-inv.loc ar-inv.net ar-inv.ord-date ar-inv.ord-no ~
ar-inv.over-pct ar-inv.paid ar-inv.pay-date ar-inv.period ar-inv.po-no ~
ar-inv.pord-no ar-inv.posted ar-inv.printed ar-inv.prod-date ar-inv.recur ~
ar-inv.ship-id ar-inv.sman-no ar-inv.sold-addr[1] ar-inv.sold-addr[2] ~
ar-inv.sold-city ar-inv.sold-id ar-inv.stat ar-inv.state ar-inv.t-comm ~
ar-inv.t-cost ar-inv.t-disc ar-inv.t-sales ar-inv.t-weight ar-inv.tax-amt ~
ar-inv.tax-code ar-inv.terms ar-inv.terms-d ar-inv.tot-ord ar-inv.type ~
ar-inv.under-pct ar-inv.upd-date ar-inv.upd-time ar-inv.user-id ar-inv.x-no ~
ar-inv.zip ar-inv.sold-name ar-inv.cust-no ar-inv.carrier ar-inv.sold-no ~
ar-inv.sold-state ar-inv.sold-zip 
&Scoped-define DISPLAYED-TABLES ar-inv
&Scoped-define FIRST-DISPLAYED-TABLE ar-inv
&Scoped-Define DISPLAYED-OBJECTS begin_inv 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-delete 
     LABEL "D&elete" 
     SIZE 16 BY 1.19.

DEFINE BUTTON btn-exit 
     LABEL "&Exit" 
     SIZE 19 BY 1.19.

DEFINE BUTTON btn-update 
     LABEL "Update" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>" INITIAL 0 
     LABEL "AR Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 1.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      ar-inv SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     begin_inv AT ROW 2.43 COL 20 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     ar-inv.addr[1] AT ROW 3.62 COL 20 COLON-ALIGNED
          LABEL "Address 1"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ar-inv.addr[2] AT ROW 4.57 COL 20 COLON-ALIGNED
          LABEL "Address 2"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ar-inv.bank-code AT ROW 5.76 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ar-inv.cust-name AT ROW 7.91 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ar-inv.city AT ROW 8.86 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-inv.company AT ROW 10.05 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-inv.contact AT ROW 11.05 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     ar-inv.curr-code[1] AT ROW 12.05 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-inv.curr-code[2] AT ROW 13.05 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-inv.check-no AT ROW 2.43 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ar-inv.disc-% AT ROW 14.1 COL 20 COLON-ALIGNED FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ar-inv.disc-days AT ROW 15.1 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-inv.disc-taken AT ROW 16 COL 20 COLON-ALIGNED
          LABEL "Disc Amt Taken"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     ar-inv.due AT ROW 9.1 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-inv.due-code AT ROW 16.95 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     ar-inv.due-date AT ROW 17.91 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.e-num AT ROW 18.86 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-inv.bill-to AT ROW 6.95 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ar-inv.est-no AT ROW 19.86 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     ar-inv.est-type AT ROW 20.86 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.4 BY 1
     ar-inv.ex-rate AT ROW 23.14 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.exported AT ROW 8.38 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-inv.f-bill AT ROW 6 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ar-inv.fob-code AT ROW 9.57 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     ar-inv.fr-out-c AT ROW 16.24 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-inv.fr-out-m AT ROW 12.43 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-inv.freight AT ROW 6 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.freq-code AT ROW 10.76 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ar-inv.frt-pay AT ROW 11.95 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 27.67.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     ar-inv.fuel AT ROW 25.05 COL 38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.fuel-bill AT ROW 25.05 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ar-inv.gross AT ROW 3.86 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     ar-inv.inv-date AT ROW 4.81 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.inv-no AT ROW 3.62 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-inv.job-no AT ROW 13.14 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ar-inv.job-no2 AT ROW 18.62 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-inv.last-date AT ROW 16.71 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.lead-days AT ROW 17.67 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     ar-inv.loc AT ROW 22.91 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     ar-inv.net AT ROW 6.95 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-inv.ord-date AT ROW 20.52 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.ord-no AT ROW 19.33 COL 108 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-inv.over-pct AT ROW 13.38 COL 69 COLON-ALIGNED FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ar-inv.paid AT ROW 8.14 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     ar-inv.pay-date AT ROW 3.38 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.period AT ROW 10.05 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-inv.po-no AT ROW 14.33 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-inv.pord-no AT ROW 15.52 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-inv.posted AT ROW 16.71 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ar-inv.printed AT ROW 17.91 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ar-inv.prod-date AT ROW 14.33 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.recur AT ROW 21.95 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-inv.ship-id AT ROW 5.29 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ar-inv.sman-no AT ROW 4.33 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     ar-inv.sold-addr[1] AT ROW 12.91 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     ar-inv.sold-addr[2] AT ROW 13.86 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     ar-inv.sold-city AT ROW 6.24 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-inv.sold-id AT ROW 11.95 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 27.67.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     ar-inv.stat AT ROW 2.43 COL 115 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ar-inv.state AT ROW 7.19 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     ar-inv.t-comm AT ROW 21.52 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-inv.t-cost AT ROW 22.52 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-inv.t-disc AT ROW 23.52 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-inv.t-sales AT ROW 24.52 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-inv.t-weight AT ROW 20.52 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-inv.tax-amt AT ROW 4.81 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.tax-code AT ROW 18.38 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-inv.terms AT ROW 19.57 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     ar-inv.terms-d AT ROW 24.81 COL 122 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ar-inv.tot-ord AT ROW 17.19 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-inv.type AT ROW 21.71 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ar-inv.under-pct AT ROW 19.57 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ar-inv.upd-date AT ROW 20.52 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.upd-time AT ROW 21.48 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-inv.user-id AT ROW 22.67 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ar-inv.x-no AT ROW 23.62 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ar-inv.zip AT ROW 8.14 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     ar-inv.sold-name AT ROW 2.43 COL 67 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ar-inv.cust-no AT ROW 2.43 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ar-inv.carrier AT ROW 11.24 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     ar-inv.sold-no AT ROW 11 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     ar-inv.sold-state AT ROW 14.81 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     ar-inv.sold-zip AT ROW 15.81 COL 138 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     btn-delete AT ROW 26.71 COL 77
     btn-exit AT ROW 26.71 COL 93
     btn-cancel AT ROW 26.71 COL 59
     btn-update AT ROW 26.71 COL 41
     "AR Invoice Repair Utility" VIEW-AS TEXT
          SIZE 29 BY 1.19 AT ROW 1.24 COL 62
          FONT 6
     RECT-18 AT ROW 26.48 COL 39
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 27.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Restore AR Invoices"
         HEIGHT             = 27.67
         WIDTH              = 156.2
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN ar-inv.addr[1] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.addr[2] IN FRAME fMain
   EXP-LABEL                                                            */
ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME fMain     = 
                "parm".

/* SETTINGS FOR FILL-IN ar-inv.disc-% IN FRAME fMain
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ar-inv.disc-taken IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.over-pct IN FRAME fMain
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ar-inv.x-no IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ar-inv SHARE-LOCK.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Restore AR Invoices */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Restore AR Invoices */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fMain
&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv wWin
ON GO OF begin_inv IN FRAME fMain /* AR Invoice# */
DO:
 
   RUN edit-checks NO-ERROR.

   IF ERROR-STATUS:ERROR THEN 
   DO:
       RETURN NO-APPLY.
   END.

   RUN build-records NO-ERROR.
   RUN display-ar-inv   NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv wWin
ON LEAVE OF begin_inv IN FRAME fMain /* AR Invoice# */
DO:
   IF LASTKEY = -1 THEN
      RETURN.

     assign {&self-name}
            begin_inv.

   RUN edit-checks NO-ERROR.

   IF ERROR-STATUS:ERROR THEN 
   DO:
       RETURN NO-APPLY.
   END.

   RUN build-records NO-ERROR.
   RUN display-ar-inv   NO-ERROR.
     

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel wWin
ON CHOOSE OF btn-cancel IN FRAME fMain /* Cancel */
DO:
    CLEAR FRAME fmain.
    apply "entry" TO begin_inv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete wWin
ON CHOOSE OF btn-delete IN FRAME fMain /* Delete */
DO:

    RUN delete-inv NO-ERROR.
    RUN delete-ar NO-ERROR.
    CLEAR FRAME fmain.
    APPLY "entry" TO begin_inv IN FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-exit wWin
ON CHOOSE OF btn-exit IN FRAME fMain /* Exit */
DO:
  APPLY "Close" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update wWin
ON CHOOSE OF btn-update IN FRAME fMain /* Update */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  
  RUN update-ar-inv.
  RUN util/b-arline.w (BUFFER ar-inv) NO-ERROR.
  APPLY "entry" TO begin_inv IN FRAME fmain.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

 
    
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  /* check security */
 
/*   IF access-close THEN DO:            */
/*      APPLY "close" TO THIS-PROCEDURE. */
/*      RETURN .                         */
/*   END.                                */
  RUN enable_UI.

  {methods/nowait.i}

  APPLY "entry" TO begin_inv IN FRAME fmain.

  HIDE FRAME ar-line-frame.

  /*
  RUN disable-ar-inv-fields.
  */

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-ar-inv wWin 
PROCEDURE build-ar-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WHILE TRUE :
        FIND ar-inv WHERE ar-inv.x-no = v-ref-ar NO-LOCK NO-ERROR.
        IF AVAILABLE ar-inv THEN 
            v-ref-ar = v-ref-ar + 1.
        ELSE LEAVE.
    END.

    find first period where 
         period.company eq cocode
     and period.pst     LE v-tr-date
     and period.pend    GE v-tr-date
     NO-LOCK NO-ERROR.

    CREATE ar-inv .
    ASSIGN ar-inv.x-no           = v-ref-ar
           ar-inv.company        = inv-head.company
           ar-inv.ord-no         = oe-bolh.b-ord-no
           ar-inv.ord-date       = oe-bolh.bol-date
           ar-inv.inv-no         = v-invno
           ar-inv.sold-name      = inv-head.sold-name
           ar-inv.bill-to        = inv-head.bill-to
           ar-inv.sold-city      = inv-head.sold-city
           ar-inv.sold-zip       = inv-head.sold-zip
           ar-inv.contact        = inv-head.contact
           ar-inv.terms          = inv-head.terms
           ar-inv.frt-pay        = inv-head.frt-pay
           ar-inv.fob-code       = inv-head.fob-code
           ar-inv.carrier        = inv-head.carrier
           ar-inv.cust-no        = inv-head.cust-no
           ar-inv.inv-date       = v-tr-date
           ar-inv.sold-id        = inv-head.sold-no
           ar-inv.ship-id        = inv-head.sold-no /* RLL */
           ar-inv.addr[1]        = inv-head.addr[1]
           ar-inv.addr[2]        = inv-head.addr[2]
           ar-inv.state          = inv-head.state
           ar-inv.zip            = inv-head.zip
           ar-inv.city           = inv-head.city
           ar-inv.sold-state     = inv-head.sold-state
           ar-inv.cust-name      = inv-head.cust-name
           ar-inv.terms-d        = inv-head.terms-d
           ar-inv.sold-addr[1]   = inv-head.sold-addr[1]
           ar-inv.sold-addr[2]   = inv-head.sold-addr[2]
           ar-inv.bill-i[1]      = inv-head.bill-i[1]
           ar-inv.bill-i[2]      = inv-head.bill-i[2]
           ar-inv.bill-i[3]      = inv-head.bill-i[3]
           ar-inv.bill-i[4]      = inv-head.bill-i[4]
           ar-inv.f-bill         = IF inv-head.t-inv-freight GT 0 THEN YES
                                   ELSE NO
           ar-inv.ship-i[1]      = inv-head.ship-i[1]
           ar-inv.ship-i[2]      = inv-head.ship-i[2]
           ar-inv.ship-i[3]      = inv-head.ship-i[3]
           ar-inv.ship-i[4]      = inv-head.ship-i[4]
           ar-inv.STAT           = inv-head.STAT
           ar-inv.TAX-code       = inv-head.TAX-GR
           ar-inv.t-comm         =  ar-inv.t-comm + inv-head.t-comm
           ar-inv.t-weight       = inv-head.t-inv-weight         
           ar-inv.t-cost         = inv-head.t-inv-cost     
           ar-inv.due            = v-tot-inv-rev        
           ar-inv.gross          = v-tot-inv-rev 
           ar-inv.tax-amt        = 0
           ar-inv.t-cost         = inv-head.t-inv-cost
           ar-inv.posted         = yes
           ar-inv.printed        = yes
           ar-inv.period         = period.pnum
           ar-inv.disc-taken     = 0
           ar-inv.paid           = 0 
           ar-inv.t-sales        = inv-head.t-inv-rev - inv-head.t-inv-tax
           ar-inv.net            = inv-head.t-inv-rev - inv-head.t-inv-tax
           ar-inv.freight        = inv-head.t-inv-freight.
                   
           if inv-head.f-bill then
              ASSIGN ar-inv.t-sales = ar-inv.t-sales - inv-head.t-inv-freight .

           find first terms where terms.company = inv-head.company and
                      terms.t-code  = inv-head.terms
                      no-lock no-error.

           if available terms then
              assign ar-inv.due-date  = ar-inv.inv-date + terms.net-days
                     ar-inv.disc-%    = terms.disc-rate
                     ar-inv.disc-days = terms.disc-days.

           /* multiple currency mods */
           FIND FIRST cust WHERE cust.company = inv-head.company
                  AND cust.cust-no = inv-head.cust-no NO-LOCK NO-ERROR.
           IF AVAIL cust THEN ASSIGN ar-inv.curr-code[1] = cust.curr-code.
           IF cust.curr-code = "" THEN DO:
           FIND company WHERE company.company = inv-head.company NO-LOCK NO-ERROR.
           IF AVAIL company THEN ar-inv.curr-code[1] = company.curr-code.
           END.            
           FIND currency WHERE currency.company = inv-head.company
                                     AND currency.c-code = ar-inv.curr-code[1] NO-LOCK NO-ERROR.
           IF AVAIL currency THEN ar-inv.ex-rate = currency.ex-rate .  
           

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-ar-lines wWin 
PROCEDURE build-ar-lines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DISABLE TRIGGERS FOR LOAD OF ar-invl.

 FOR EACH inv-line WHERE
      inv-line.company = oe-bolh.company
  AND inv-line.inv-no  = v-invno NO-LOCK:


    FIND FIRST oe-bolh  WHERE oe-bolh.b-no EQ inv-line.b-no NO-LOCK NO-ERROR.
    FIND FIRST oe-boll OF oe-bolh NO-LOCK NO-ERROR.
    FIND FIRST inv-misc WHERE inv-misc.r-no =  inv-head.r-no NO-LOCK NO-ERROR. 
    FIND FIRST itemfg WHERE
         itemfg.company = inv-line.company
     AND itemfg.i-no    = inv-line.i-no
         NO-LOCK NO-ERROR.    

     if avail itemfg then
       find first fgcat
            where fgcat.company eq oe-bolh.company
              and fgcat.procat  eq itemfg.procat
              no-lock no-error.
          
    CREATE ar-invl.
    ASSIGN ar-invl.x-no       = v-ref-ar
           ar-invl.actnum     = if avail fgcat AND fgcat.glacc NE "" THEN fgcat.glacc
                                ELSE v-ar-sales
           ar-invl.inv-no     = v-invno
           ar-invl.inv-date   = v-tr-date
           ar-invl.bol-no     = IF AVAIL oe-bolh THEN oe-bolh.bol-no ELSE inv-head.bol-no
           ar-invl.b-no       = inv-line.b-no
           ar-invl.company    = inv-line.company
           ar-invl.ord-no     = oe-boll.ord-no
           ar-invl.ord-line   = INT(inv-line.part-dscr2)
           ar-invl.cust-no    = oe-boll.cust-no
           ar-invl.line       = inv-line.LINE
           ar-invl.est-no     = inv-line.est-no
           ar-invl.est-type   = inv-line.est-type
           ar-invl.form-no    = inv-line.form-no
           ar-invl.blank-no   = inv-line.blank-no
           ar-invl.job-no     = inv-line.job-no
           ar-invl.job-no2    = inv-line.job-no2
           ar-invl.part-no    = inv-line.part-no
           ar-invl.i-no       = inv-line.i-no
           ar-invl.i-name     = inv-line.i-name
           ar-invl.i-dscr     = inv-line.i-dscr
           ar-invl.po-no      = inv-line.po-no
           ar-invl.req-code   = inv-line.req-code
           ar-invl.req-date   = inv-line.req-date
           ar-invl.prom-code  = inv-line.prom-code
           ar-invl.prom-date  = inv-line.prom-date
           ar-invl.part-dscr1 = inv-line.part-dscr1
           ar-invl.part-dscr2 = inv-line.part-dscr2
           ar-invl.po-no-po   = inv-line.po-no-po
           ar-invl.cas-cnt    = inv-line.cas-cnt
           ar-invl.pr-uom     = inv-line.pr-uom
           ar-invl.unit-pr    = inv-line.price
           ar-invl.tax        = inv-line.tax
           ar-invl.disc       = inv-line.disc
           ar-invl.amt        = inv-line.t-price   /* total price of invoiced item */
           ar-invl.t-weight   = inv-line.t-weight  /* total weight of invoiced item */
           ar-invl.t-freight  = inv-line.t-freight /* total freight of invoiced item */
           ar-invl.ship-qty   = inv-line.ship-qty
           ar-invl.inv-qty    = inv-line.inv-qty
           ar-invl.qty        = inv-line.qty
           ar-invl.sman[1]    = inv-line.sman[1]
           ar-invl.sman[2]    = inv-line.sman[2]
           ar-invl.sman[3]    = inv-line.sman[3]
           ar-invl.s-pct[1]   = inv-line.s-pct[1]
           ar-invl.s-pct[2]   = inv-line.s-pct[2]
           ar-invl.s-pct[3]   = inv-line.s-pct[3]
           ar-invl.s-comm[1]  = inv-line.s-comm[1]
           ar-invl.s-comm[2]  = inv-line.s-comm[2]
           ar-invl.s-comm[3]  = inv-line.s-comm[3]
           ar-invl.sname[1]   = inv-line.sname[1]
           ar-invl.sname[2]   = inv-line.sname[2]
           ar-invl.sname[3]   = inv-line.sname[3]
           ar-invl.s-commbasis[1] = inv-line.s-commbasis[1]
           ar-invl.s-commbasis[2] = inv-line.s-commbasis[2]
           ar-invl.s-commbasis[3] = inv-line.s-commbasis[3]
           ar-invl.misc       = no
           ar-invl.posted     = yes
           ar-invl.pr-qty-uom = inv-line.pr-uom
           ar-invl.cost       = inv-line.cost
           ar-invl.dscr[1]    = "M"
           ar-invl.t-cost     = inv-line.inv-qty * inv-line.cost /* not sure */

           ar-invl.std-tot-cost = inv-line.cost
           ar-invl.std-lab-cost = IF AVAIL itemfg THEN itemfg.std-lab-cost
                                  ELSE 0
           ar-invl.std-fix-cost = IF AVAIL itemfg THEN itemfg.std-fix-cost
                                  ELSE 0
           ar-invl.std-var-cost = IF AVAIL itemfg THEN itemfg.std-var-cost
                                  ELSE 0
           ar-invl.std-mat-cost = IF AVAIL itemfg THEN itemfg.std-mat-cost
                                  ELSE 0.

           if ar-invl.ord-no eq 0 then ar-invl.s-pct[1] = 100. 


 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-inv-header wWin 
PROCEDURE build-inv-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 

 /*IF ERROR-STATUS:ERROR THEN
    RETURN NO-APPLY. */

 FIND FIRST shipto NO-LOCK
       WHERE shipto.company EQ oe-bolh.company
         AND shipto.ship-id EQ oe-bolh.ship-id
         AND shipto.cust-no EQ oe-bolh.cust-no
         AND shipto.ship-no NE 1
         USE-INDEX ship-id NO-ERROR.

  IF NOT AVAIL shipto THEN
  FIND FIRST shipto NO-LOCK
      WHERE shipto.company EQ oe-bolh.company
        AND shipto.cust-no EQ oe-bolh.cust-no
      USE-INDEX ship-no NO-ERROR.

   
  CREATE inv-head.
  ASSIGN inv-head.sold-no    = shipto.ship-id
       inv-head.sold-name    = shipto.ship-name
       inv-head.sold-addr[1] = shipto.ship-addr[1]
       inv-head.sold-addr[2] = shipto.ship-addr[2]
       inv-head.sold-state   = shipto.ship-state
       inv-head.sold-city    = shipto.ship-city
       inv-head.sold-zip     = shipto.ship-zip
       inv-head.r-no         = v-ref-inv
       inv-head.company      = oe-bolh.company
       inv-head.bol-no       = oe-bolh.bol-no
       inv-head.bill-to      = oe-bolh.cust-no
       inv-head.cust-no      = oe-bolh.cust-no
       inv-head.frt-pay      = oe-bolh.frt-pay
       inv-head.carrier      = oe-bolh.carrier
       inv-head.ship-i[1]    = oe-bolh.ship-i[1]
       inv-head.ship-i[2]    = oe-bolh.ship-i[2]
       inv-head.ship-i[3]    = oe-bolh.ship-i[3]
       inv-head.ship-i[4]    = oe-bolh.ship-i[4]
       inv-head.fob-code     = oe-ord.fob-code
       inv-head.contact      = oe-ord.contact
       inv-head.terms        = oe-ord.terms
       inv-head.terms-d      = oe-ord.terms-d
       inv-head.f-bill       = NO
       inv-head.tax-gr       = IF AVAIL shipto AND shipto.tax-code NE ""
                               THEN shipto.tax-code ELSE oe-ord.tax-gr
       inv-head.tot-ord      = 0
       inv-head.inv-no       = v-invno
       inv-head.stat         = ""
       inv-head.deleted      = NO
       inv-head.posted       = NO
       inv-head.inv-date     = v-tr-date
       inv-head.inv-no       = v-invno
       inv-head.cust-name    = cust.name
       inv-head.addr[1]      = cust.addr[1]
       inv-head.addr[2]      = cust.addr[2]
       inv-head.city         = cust.city
       inv-head.state        = cust.state
       inv-head.zip          = cust.zip
       inv-head.curr-code[1] = cust.curr-code.


      FIND FIRST usergrps WHERE
           usergrps.usergrps = "IN"
           NO-LOCK NO-ERROR.

      IF AVAIL usergrps AND TRIM(usergrps.users) NE "" THEN
      DO:
         ASSIGN
          v-line-count = 0
          v-start-pos  = 1.

         DO li = 1 TO LENGTH(usergrps.users):
            ls = SUBSTR(usergrps.users,li,1).

            IF v-line-count < 5 AND ls EQ CHR(10) OR ls EQ CHR(13) THEN
               ASSIGN
                  v-line-count = v-line-count + 1
                  inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos)
                  v-start-pos = li + 1.

            IF v-line-count < 5 AND li = LENGTH(usergrps.users) AND
               NOT(ls EQ CHR(10) OR ls EQ CHR(13)) THEN
               ASSIGN
                  v-line-count = v-line-count + 1
                  inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos + 1).
         END.

         RELEASE usergrps.
      END.

      DO li = 1 TO 4:
         IF inv-head.bill-i[li] = "" THEN
            inv-head.bill-i[li] = oe-ord.bill-i[li].
      END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-inv-lines wWin 
PROCEDURE build-inv-lines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH oe-boll WHERE oe-boll.b-no = oe-bolh.b-no NO-LOCK :


   ASSIGN v-ord-lines = v-ord-lines + 1.

   FIND FIRST itemfg WHERE
             itemfg.company = oe-boll.company
         AND itemfg.i-no    = oe-boll.i-no
         NO-LOCK NO-ERROR.

  FIND FIRST oe-ordl
      WHERE oe-ordl.company EQ oe-boll.company
        AND oe-ordl.ord-no  EQ oe-boll.ord-no
        AND oe-ordl.line    EQ oe-boll.line
        AND oe-ordl.i-no    EQ oe-boll.i-no
       USE-INDEX ord-no  NO-LOCK NO-ERROR .

  IF AVAIL oe-ordl THEN
    find first inv-line
        where inv-line.r-no   eq inv-head.r-no
          and inv-line.ord-no eq oe-boll.ord-no
          and inv-line.b-no   eq oe-bolh.b-no
          and inv-line.i-no   eq oe-boll.i-no
          and inv-line.line   eq oe-boll.line
          and inv-line.po-no  eq oe-boll.po-no
          use-index r-no no-error.



  IF NOT AVAIL inv-line THEN 
  DO:
      
      CREATE inv-line.
      ASSIGN inv-line.r-no       = v-ref-inv
          inv-line.company    = oe-bolh.company
       inv-line.ord-no     = oe-boll.ord-no
       inv-line.inv-no     = v-invno
       inv-line.b-no       = oe-bolh.b-no
       inv-line.line       = oe-boll.line
       inv-line.i-no       = oe-boll.i-no
       inv-line.stat       = oe-boll.s-code
       inv-line.est-no     = oe-ordl.est-no
       inv-line.est-type   = oe-ord.est-type
       inv-line.ord-date   = oe-ord.ord-date
       inv-line.part-no    = oe-ordl.part-no
       inv-line.i-name     = oe-ordl.i-name
       inv-line.i-dscr     = oe-ordl.i-dscr
       inv-line.pr-uom     = oe-ordl.pr-uom
       inv-line.price      = oe-ordl.price
       inv-line.cas-cnt    = IF oe-ordl.pr-uom EQ "CS" THEN oe-ordl.cas-cnt
                                                       ELSE oe-boll.qty-case
       inv-line.req-code   = oe-ordl.req-code
       inv-line.req-date   = oe-ordl.req-date
       inv-line.prom-code  = oe-ordl.prom-code
       inv-line.prom-date  = oe-ordl.prom-date
       inv-line.part-dscr1 = oe-ordl.part-dscr1

       inv-line.part-dscr2 = string(oe-ordl.LINE)
       inv-line.po-no-po   = oe-ordl.po-no-po
       inv-line.e-num      = oe-ordl.e-num
       inv-line.form-no    = oe-ordl.form-no
       inv-line.blank-no   = oe-ordl.blank-no
       inv-line.j-no       = oe-ordl.j-no
       inv-line.job-no     = oe-ordl.job-no
       inv-line.job-no2    = oe-ordl.job-no2
       inv-line.tax        = oe-ordl.tax
       inv-line.disc       = oe-ordl.disc
       inv-line.qty        = oe-ordl.qty
       inv-line.p-c        = oe-boll.p-c
       inv-line.po-no      = oe-boll.po-no.
    

  
       IF oe-boll.sell-price NE 0 THEN
          inv-line.price = oe-boll.sell-price.
          
    
  
  END.

  ASSIGN inv-line.t-weight      = inv-line.t-weight + oe-boll.weight
         inv-head.t-inv-weight  = inv-head.t-inv-weight + oe-boll.weight
         inv-line.t-freight     = inv-line.t-freight + oe-boll.freight
         inv-head.t-inv-freight = inv-head.t-inv-freight + oe-boll.freight.
 
  IF oe-boll.s-code ne "S" and not oe-ordl.is-a-component then
     inv-line.inv-qty = inv-line.inv-qty + oe-boll.qty.
  
  /** Increase ship Qty when ship or invoice & ship **/
  if oe-boll.s-code ne "I"                                            or
     can-find(first b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) then
    inv-line.ship-qty = inv-line.ship-qty + oe-boll.qty.

  /*
  PAUSE 999.

  RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT oe-ordl.ship-qty, OUTPUT oe-ordl.inv-qty).
  */


  IF inv-line.pr-uom BEGINS "L" AND inv-line.pr-uom NE "LB" THEN
     inv-line.t-price = inv-line.price *
                           IF inv-line.inv-qty LT 0 THEN -1 ELSE 1.
  ELSE IF inv-line.pr-uom EQ "CS" THEN
     inv-line.t-price = inv-line.inv-qty /
                           (IF inv-line.cas-cnt NE 0 THEN
                             inv-line.cas-cnt
                            ELSE
                            IF itemfg.case-count NE 0 THEN
                              itemfg.case-count ELSE 1) *
                           inv-line.price.
  ELSE IF LOOKUP(inv-line.pr-uom,fg-uom-list) GT 0 THEN
       inv-line.t-price = inv-line.inv-qty * inv-line.price.
  ELSE
    FOR EACH uom
       WHERE uom.uom  EQ inv-line.pr-uom
         AND uom.mult NE 0 NO-LOCK:
      inv-line.t-price = inv-line.inv-qty / uom.mult * inv-line.price.
      LEAVE.
    END.

  inv-line.t-price = ROUND(inv-line.t-price,2).

  IF inv-line.disc NE 0 THEN
     inv-line.t-price = 
        IF ll-calc-disc-first THEN 
          (inv-line.t-price - ROUND(inv-line.t-price * inv-line.disc / 100,2))
        ELSE
          ROUND(inv-line.t-price * (1 - (inv-line.disc / 100)),2).


  /** Calculations **/
  ASSIGN v-tot-inv-rev     = v-tot-inv-rev + inv-line.t-price.

  FIND FIRST stax WHERE stax.company = inv-head.company AND
                      stax.tax-group = inv-head.tax-gr NO-LOCK NO-ERROR.
  if avail stax THEN
      v-tax = ROUND(inv-line.t-price * stax.tax-rate[1] / 100,2).
    
  

  /*

  RUN oe/invlcost.p (ROWID(inv-line),
                     OUTPUT v-cost[1], OUTPUT v-cost[2],
                     OUTPUT v-cost[3], OUTPUT v-cost[4],
                     OUTPUT xx-cost1, OUTPUT xx-cost2).
  
          .
  
  inv-head.t-inv-cost = inv-head.t-inv-cost + inv-line.cost.
  */

  ASSIGN
     xx-amt = inv-head.t-inv-rev
     inv-head.t-inv-rev = xx-amt
     inv-head.t-inv-tax =  inv-head.t-inv-tax + v-tax.

  /**
  MESSAGE "Invoice = " inv-line.inv-no              SKIP  
         "Inv tot = " inv-head.t-inv-rev      SKIP
        "Inv Line = " inv-line.t-price       SKIP
        "Paid     = " t-invoice.tt-amt         SKIP
        "Freight = "  inv-head.t-inv-freight SKIP
        "group   = " inv-head.tax-gr         SKIP
        "price   = " inv-line.t-price        SKIP
        "tax     = " inv-line.tax            SKIP
        "tax     = " v-tax                   SKIP
        "cost    = " xx-cost1
    VIEW-AS ALERT-BOX.
  **/

  do i = 1 to 3:          /** Calculate Commission Amount **/
    ASSIGN inv-line.sname[i]   = oe-ord.sname[i]
           inv-line.s-comm[i]  = oe-ordl.s-comm[i]
           inv-line.s-pct[i]   = oe-ordl.s-pct[i]
           inv-line.sman[i]    = oe-ordl.s-man[i].
  end.

  DO i = 1 TO EXTENT(inv-line.sman):    /** Calculate Commission Amount **/
     RUN custom/combasis.p (oe-boll.company, inv-line.sman[i], cust.type, itemfg.procat, 0,
                           cust.cust-no,
                           OUTPUT v-basis).

     IF v-basis EQ "G" THEN
      inv-line.comm-amt[i] = ROUND(((inv-line.t-price - inv-line.t-cost)
                                       * inv-line.s-comm[i]) / 100,2).

     ELSE
      inv-line.comm-amt[i] = ROUND((((inv-line.t-price
                                       * inv-line.s-pct[i]) / 100)
                                       * inv-line.s-comm[i]) / 100,2).        
      
  END.

END.

 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-misc-charges wWin 
PROCEDURE build-misc-charges :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH oe-boll WHERE oe-boll.b-no = oe-bolh.b-no NO-LOCK :

  for each oe-ordm
     where oe-ordm.company eq oe-boll.company
       AND oe-ordm.ord-no  EQ oe-boll.ord-no
       and oe-ordm.bill    eq "I"
       NO-LOCK:
  
      ASSIGN v-ord-lines = v-ord-lines + 1.

      create inv-misc.
      BUFFER-COPY oe-ordm EXCEPT rec_key TO inv-misc
      ASSIGN inv-misc.r-no           = v-ref-inv
             inv-misc.posted         = no
             inv-misc.deleted        = no
             inv-misc.inv-i-no       = oe-ordm.ord-i-no
             inv-misc.inv-line       = oe-ordm.ord-line
             inv-misc.s-commbasis[1] = oe-ordm.commbasis[1].

   
      find LAST b-ar-invl  WHERE
           b-ar-invl.x-no = v-ref-arl
           no-lock NO-ERROR.

      
DISABLE TRIGGERS FOR LOAD OF ar-inv.           
      create ar-invl.
      ASSIGN ar-invl.x-no           = v-ref-ar
             ar-invl.company        = inv-misc.company
             ar-invl.INV-NO         = inv-head.inv-no
             ar-invl.inv-date       = v-tr-date
             ar-invl.ord-no         = inv-misc.ord-no
             ar-invl.cust-no        = inv-head.cust-no
             ar-invl.line           = IF AVAIL b-ar-invl THEN b-ar-invl.LINE + 1
                                      ELSE 1
             ar-invl.est-no         = inv-misc.est-no
             ar-invl.tax            = inv-misc.tax
             ar-invl.actnum         = inv-misc.actnum
             ar-invl.prep-amt       = inv-misc.amt
             ar-invl.qty            = 1
             ar-invl.unit-pr        = inv-misc.amt
             ar-invl.amt            = inv-misc.amt
             ar-invl.t-cost         = inv-misc.cost
             ar-invl.cost           = ar-invl.t-cost / 1000
             ar-invl.dscr[1]        = "M"
             ar-invl.prep-charge    = inv-misc.charge
             ar-invl.prep-cost      = inv-misc.cost
             ar-invl.prep-dscr      = inv-misc.dscr
             ar-invl.i-name         = inv-misc.charge
             ar-invl.i-dscr         = inv-misc.dscr
             ar-invl.po-no          = inv-misc.po-no
             ar-invl.po-no-po       = inv-misc.po-no-po
             ar-invl.sman[1]        = inv-misc.s-man[1]
             ar-invl.sman[2]        = inv-misc.s-man[2]
             ar-invl.sman[3]        = inv-misc.s-man[3]
             ar-invl.s-pct[1]       = inv-misc.s-pct[1]
             ar-invl.s-pct[2]       = inv-misc.s-pct[2]
             ar-invl.s-pct[3]       = inv-misc.s-pct[3]
             ar-invl.s-comm[1]      = inv-misc.s-comm[1]
             ar-invl.s-comm[2]      = inv-misc.s-comm[2]
             ar-invl.s-comm[3]      = inv-misc.s-comm[3]
             ar-invl.s-commbasis[1] = inv-misc.s-commbasis[1]
             ar-invl.s-commbasis[2] = inv-misc.s-commbasis[2]
             ar-invl.s-commbasis[3] = inv-misc.s-commbasis[3]
             ar-invl.inv-i-no       = inv-misc.inv-i-no
             ar-invl.inv-line       = inv-misc.inv-line
             ar-invl.misc           = YES
             ar-invl.billable       = IF inv-misc.bill EQ "I" THEN TRUE
                                      ELSE FALSE
             ar-invl.posted         = YES.

             IF NOT ar-invl.billable THEN ar-invl.amt = 0.
              
  END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-records wWin 
PROCEDURE build-records :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME fmain:

     v-invno = int(begin_inv:SCREEN-VALUE).

     FIND FIRST ar-inv WHERE
          ar-inv.company EQ oe-bolh.company
      AND ar-inv.inv-no  EQ int(begin_inv:SCREEN-VALUE)
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

     IF NOT AVAIL ar-inv THEN
     DO:
        RUN get-ref-numbers  NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
           APPLY "entry" TO begin_inv IN FRAME fmain.
           RETURN.
        END.

        RUN build-inv-header   NO-ERROR.
        RUN build-ar-inv       NO-ERROR.
        RUN build-inv-lines    NO-ERROR.
        RUN build-ar-lines     NO-ERROR.
        RUN build-misc-charges NO-ERROR.
        RUN delete-inv         NO-ERROR.
        RUN reset-ar-ctrl      NO-ERROR.
     END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-invoice-totals wWin 
PROCEDURE calc-invoice-totals :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-ar wWin 
PROCEDURE delete-ar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DISABLE TRIGGERS FOR LOAD OF ar-inv.  
DISABLE TRIGGERS FOR LOAD OF ar-invl.

FIND FIRST ar-inv WHERE
     ar-inv.company = cocode 
 AND ar-inv.inv-no = v-invno
     EXCLUSIVE-LOCK NO-ERROR.

  FOR EACH ar-invl WHERE
      ar-invl.inv-no = ar-inv.inv-no EXCLUSIVE-LOCK:
      i = i + 1.
      DELETE ar-invl.
  END.

  IF AVAIL ar-inv THEN DELETE ar-inv.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-inv wWin 
PROCEDURE delete-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF inv-head.
DISABLE TRIGGERS FOR LOAD OF inv-line.

FIND FIRST inv-head WHERE
     inv-head.company = cocode
 AND inv-head.inv-no = v-invno
     EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL inv-head THEN
DO:
   FOR EACH inv-line WHERE
       inv-line.inv-no = v-invno EXCLUSIVE-LOCK:
       DELETE inv-line.
   END.


   DELETE inv-head.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-ar-inv-fields wWin 
PROCEDURE disable-ar-inv-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DISABLE  ar-inv.addr[1] ar-inv.addr[2] 
         ar-inv.bank-code ar-inv.bill-to ar-inv.check-no ar-inv.city 
         ar-inv.company ar-inv.contact ar-inv.curr-code[1] ar-inv.curr-code[2] 
         ar-inv.cust-name ar-inv.disc-% ar-inv.disc-days ar-inv.disc-taken 
         ar-inv.due ar-inv.due-code ar-inv.due-date ar-inv.e-num ar-inv.est-no 
         ar-inv.est-type ar-inv.ex-rate ar-inv.exported ar-inv.f-bill 
         ar-inv.fob-code ar-inv.fr-out-c ar-inv.fr-out-m ar-inv.freight 
         ar-inv.freq-code ar-inv.frt-pay ar-inv.fuel ar-inv.fuel-bill 
         ar-inv.gross ar-inv.inv-date ar-inv.inv-no ar-inv.job-no 
         ar-inv.job-no2 ar-inv.last-date ar-inv.lead-days ar-inv.loc ar-inv.net 
         ar-inv.ord-date ar-inv.ord-no ar-inv.over-pct ar-inv.paid 
         ar-inv.pay-date ar-inv.period ar-inv.po-no ar-inv.pord-no 
         ar-inv.posted ar-inv.printed ar-inv.prod-date ar-inv.recur 
         ar-inv.rec_key ar-inv.ship-id ar-inv.sman-no ar-inv.sold-addr[1] 
         ar-inv.sold-addr[2] ar-inv.sold-city ar-inv.sold-id ar-inv.stat 
         ar-inv.state ar-inv.t-comm ar-inv.t-cost ar-inv.t-disc ar-inv.t-sales 
         ar-inv.t-weight ar-inv.tax-amt ar-inv.tax-code ar-inv.terms 
         ar-inv.terms-d ar-inv.tot-ord ar-inv.type ar-inv.under-pct 
         ar-inv.upd-date ar-inv.upd-time ar-inv.user-id ar-inv.x-no ar-inv.zip 
      WITH FRAME fMain.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-ar-inv wWin 
PROCEDURE display-ar-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISPLAY ar-inv.x-no           
        ar-inv.sman-no        
        ar-inv.company      
        ar-inv.ord-no         
        ar-inv.inv-no         
        ar-inv.sold-name      
        ar-inv.est-no         
        ar-inv.ord-date      
        ar-inv.bill-to       
        ar-inv.sold-city      
        ar-inv.sold-zip      
        ar-inv.contact        
        ar-inv.terms          
        ar-inv.lead-days     
        ar-inv.frt-pay        
        ar-inv.fob-code       
        ar-inv.under-pct      
        ar-inv.carrier       
        ar-inv.fr-out-c       
        ar-inv.fr-out-m       
        ar-inv.over-pct       
        ar-inv.last-date      
        ar-inv.due-date      
        ar-inv.type          
        ar-inv.t-weight       
        ar-inv.freight        
        ar-inv.t-comm         
        ar-inv.tax-amt        
        ar-inv.t-sales        
        ar-inv.t-cost         
        ar-inv.cust-no        
        ar-inv.po-no          
        ar-inv.tot-ord        
        ar-inv.t-disc       
        ar-inv.net           
        ar-inv.loc           
        ar-inv.inv-date       
        ar-inv.sold-no        
        ar-inv.addr[1]        
        ar-inv.addr[2]      
        ar-inv.state         
        ar-inv.zip            
        ar-inv.city           
        ar-inv.sold-state     
        ar-inv.cust-name      
        ar-inv.terms-d        
        ar-inv.posted         
        ar-inv.sold-addr[1]   
        ar-inv.sold-addr[2]   
        ar-inv.pord-no       
        ar-inv.job-no2           
        ar-inv.due-code       
        ar-inv.e-num          
        ar-inv.est-type       
        ar-inv.f-bill        
        ar-inv.prod-date          
        ar-inv.stat          
        ar-inv.tax-code       
        ar-inv.period       
        ar-inv.disc-taken     
        ar-inv.paid           
        ar-inv.job-no        
        ar-inv.ship-id        
        ar-inv.gross          
        ar-inv.due            
        ar-inv.check-no       
        ar-inv.disc-%         
        ar-inv.disc-days      
        ar-inv.pay-date       
        ar-inv.printed        
        ar-inv.sold-id        
        ar-inv.user-id       
        ar-inv.exported                  
        ar-inv.bank-code    
        ar-inv.rec_key       
        ar-inv.upd-date       
        ar-inv.curr-code[1]   
        ar-inv.ex-rate       
        ar-inv.freq-code      
        ar-inv.recur              
        ar-inv.fuel-bill     
        ar-inv.fuel
        WITH FRAME fmain.          

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE edit-checks wWin 
PROCEDURE edit-checks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DO WITH FRAME fmain:
   
   FIND FIRST oe-boll WHERE
        oe-boll.company EQ cocode AND
        oe-boll.inv-no = int(begin_inv:SCREEN-VALUE)
        NO-LOCK NO-ERROR.


   IF NOT AVAIL oe-boll THEN
   DO:
      MESSAGE "There are no Bill of Lading lines for this invoice#"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN ERROR.
   END.

   FIND oe-bolh OF oe-boll NO-LOCK NO-ERROR.


   IF NOT AVAIL oe-bolh THEN
   DO:
      MESSAGE "There is no Bill of Lading Header for this invoice#"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN ERROR.
   END.

   FIND oe-ord WHERE oe-ord.company EQ oe-boll.company
     AND oe-ord.ord-no  EQ oe-boll.ord-no
     USE-INDEX ord-no  NO-LOCK NO-ERROR .

   IF NOT AVAIL oe-ord THEN
   DO:
      MESSAGE "There is no Order Header for Order"  oe-boll.ord-no
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN ERROR.
   END.

   FIND FIRST cust WHERE
        cust.company EQ oe-bolh.company
        AND cust.cust-no EQ oe-bolh.cust-no
        NO-LOCK NO-ERROR.

   IF NOT AVAIL cust THEN
   DO:
      MESSAGE "There is no Customer " oe-bolh.cust-no " for this invoice" 
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN ERROR.
   END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-ar-inv wWin 
PROCEDURE enable-ar-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-ar-inv-fields wWin 
PROCEDURE enable-ar-inv-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ENABLE btn-update btn-cancel ar-inv.addr[1] ar-inv.addr[2] 
         ar-inv.bank-code ar-inv.bill-to ar-inv.check-no ar-inv.city 
         ar-inv.company ar-inv.contact ar-inv.curr-code[1] ar-inv.curr-code[2] 
         ar-inv.cust-name ar-inv.disc-% ar-inv.disc-days ar-inv.disc-taken 
         ar-inv.due ar-inv.due-code ar-inv.due-date ar-inv.e-num ar-inv.est-no 
         ar-inv.est-type ar-inv.ex-rate ar-inv.exported ar-inv.f-bill 
         ar-inv.fob-code ar-inv.fr-out-c ar-inv.fr-out-m ar-inv.freight 
         ar-inv.freq-code ar-inv.frt-pay ar-inv.fuel ar-inv.fuel-bill 
         ar-inv.gross ar-inv.inv-date ar-inv.inv-no ar-inv.job-no 
         ar-inv.job-no2 ar-inv.last-date ar-inv.lead-days ar-inv.loc ar-inv.net 
         ar-inv.ord-date ar-inv.ord-no ar-inv.over-pct ar-inv.paid 
         ar-inv.pay-date ar-inv.period ar-inv.po-no ar-inv.pord-no 
         ar-inv.posted ar-inv.printed ar-inv.prod-date ar-inv.recur 
         ar-inv.rec_key ar-inv.ship-id ar-inv.sman-no ar-inv.sold-addr[1] 
         ar-inv.sold-addr[2] ar-inv.sold-city ar-inv.sold-id ar-inv.stat 
         ar-inv.state ar-inv.t-comm ar-inv.t-cost ar-inv.t-disc ar-inv.t-sales 
         ar-inv.t-weight ar-inv.tax-amt ar-inv.tax-code ar-inv.terms 
         ar-inv.terms-d ar-inv.tot-ord ar-inv.type ar-inv.under-pct 
         ar-inv.upd-date ar-inv.upd-time ar-inv.user-id ar-inv.x-no ar-inv.zip 
      WITH FRAME fMain IN WINDOW wWin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY begin_inv 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE ar-inv THEN 
    DISPLAY ar-inv.addr[1] ar-inv.addr[2] ar-inv.bank-code ar-inv.cust-name 
          ar-inv.city ar-inv.company ar-inv.contact ar-inv.curr-code[1] 
          ar-inv.curr-code[2] ar-inv.check-no ar-inv.disc-% ar-inv.disc-days 
          ar-inv.disc-taken ar-inv.due ar-inv.due-code ar-inv.due-date 
          ar-inv.e-num ar-inv.bill-to ar-inv.est-no ar-inv.est-type 
          ar-inv.ex-rate ar-inv.exported ar-inv.f-bill ar-inv.fob-code 
          ar-inv.fr-out-c ar-inv.fr-out-m ar-inv.freight ar-inv.freq-code 
          ar-inv.frt-pay ar-inv.fuel ar-inv.fuel-bill ar-inv.gross 
          ar-inv.inv-date ar-inv.inv-no ar-inv.job-no ar-inv.job-no2 
          ar-inv.last-date ar-inv.lead-days ar-inv.loc ar-inv.net 
          ar-inv.ord-date ar-inv.ord-no ar-inv.over-pct ar-inv.paid 
          ar-inv.pay-date ar-inv.period ar-inv.po-no ar-inv.pord-no 
          ar-inv.posted ar-inv.printed ar-inv.prod-date ar-inv.recur 
          ar-inv.ship-id ar-inv.sman-no ar-inv.sold-addr[1] ar-inv.sold-addr[2] 
          ar-inv.sold-city ar-inv.sold-id ar-inv.stat ar-inv.state ar-inv.t-comm 
          ar-inv.t-cost ar-inv.t-disc ar-inv.t-sales ar-inv.t-weight 
          ar-inv.tax-amt ar-inv.tax-code ar-inv.terms ar-inv.terms-d 
          ar-inv.tot-ord ar-inv.type ar-inv.under-pct ar-inv.upd-date 
          ar-inv.upd-time ar-inv.user-id ar-inv.x-no ar-inv.zip ar-inv.sold-name 
          ar-inv.cust-no ar-inv.carrier ar-inv.sold-no ar-inv.sold-state 
          ar-inv.sold-zip 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE begin_inv ar-inv.addr[1] ar-inv.addr[2] ar-inv.bank-code 
         ar-inv.cust-name ar-inv.city ar-inv.company ar-inv.contact 
         ar-inv.curr-code[1] ar-inv.curr-code[2] ar-inv.check-no ar-inv.disc-% 
         ar-inv.disc-days ar-inv.disc-taken ar-inv.due ar-inv.due-code 
         ar-inv.due-date ar-inv.e-num ar-inv.bill-to ar-inv.est-no 
         ar-inv.est-type ar-inv.ex-rate ar-inv.exported ar-inv.f-bill 
         ar-inv.fob-code ar-inv.fr-out-c ar-inv.fr-out-m ar-inv.freight 
         ar-inv.freq-code ar-inv.frt-pay ar-inv.fuel ar-inv.fuel-bill 
         ar-inv.gross ar-inv.inv-date ar-inv.inv-no ar-inv.job-no 
         ar-inv.job-no2 ar-inv.last-date ar-inv.lead-days ar-inv.loc ar-inv.net 
         ar-inv.ord-date ar-inv.ord-no ar-inv.over-pct ar-inv.paid 
         ar-inv.pay-date ar-inv.period ar-inv.po-no ar-inv.pord-no 
         ar-inv.posted ar-inv.printed ar-inv.prod-date ar-inv.recur 
         ar-inv.ship-id ar-inv.sman-no ar-inv.sold-addr[1] ar-inv.sold-addr[2] 
         ar-inv.sold-city ar-inv.sold-id ar-inv.stat ar-inv.state ar-inv.t-comm 
         ar-inv.t-cost ar-inv.t-disc ar-inv.t-sales ar-inv.t-weight 
         ar-inv.tax-amt ar-inv.tax-code ar-inv.terms ar-inv.terms-d 
         ar-inv.tot-ord ar-inv.type ar-inv.under-pct ar-inv.upd-date 
         ar-inv.upd-time ar-inv.user-id ar-inv.zip ar-inv.sold-name 
         ar-inv.cust-no ar-inv.carrier ar-inv.sold-no ar-inv.sold-state 
         ar-inv.sold-zip btn-delete btn-exit btn-cancel btn-update RECT-18 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-gltrans-record wWin 
PROCEDURE get-gltrans-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------------------------*/
DEF VAR v-error AS LOG.

FOR EACH gltrans WHERE
         gltrans.jrnl = "OEINV" NO-LOCK:

   v-pos = R-INDEX(gltrans.tr-dscr,"Inv#").

   IF v-pos = 0 THEN NEXT.

    v-pos = v-pos + 4.
    v-invno = INT(SUBSTRING(gltrans.tr-dscr,v-pos,10)).


    IF v-invno NE begin_inv THEN  NEXT .
    ELSE
    DO:
       v-error = FALSE.
       LEAVE.
    END.
END.

IF v-error THEN
DO:
   MESSAGE "Invoice# does not exist on GL Transaction File"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN ERROR.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-ref-numbers wWin 
PROCEDURE get-ref-numbers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* save the current last-inv-no */
FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode
      EXCLUSIVE NO-ERROR NO-WAIT.

IF NOT AVAIL ar-ctrl THEN
DO:
   MESSAGE "The AR Control is currently locked. Cannot Update!"
       VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.

IF AVAIL ar-ctrl THEN
   ASSIGN  v-last-inv = ar-ctrl.last-inv
           v-ar-sales = ar-ctrl.sales.

/*FIND LAST ar-inv WHERE 
     ar-inv.inv-no GT 3237 and
     ar-inv.inv-no LT 4207 NO-LOCK USE-INDEX x-no.*/

find last ar-inv use-index x-no no-lock no-error.

v-ref-ar = ar-inv.x-no + 1.


v-ref-inv = next-value(inv_r_no_seq).

FIND LAST ar-invl NO-LOCK USE-INDEX x-no.
v-ref-arl = ar-invl.x-no + 1.

GLTRANS-LOOP:
FOR EACH gltrans WHERE
         gltrans.jrnl = "OEINV" 
         NO-LOCK:

   v-pos = R-INDEX(gltrans.tr-dscr,"Inv#").

   IF v-pos = 0 THEN NEXT.

    v-pos = v-pos + 4.
   
   IF INT(SUBSTRING(gltrans.tr-dscr,v-pos,10)) = begin_inv THEN
   DO:
       ASSIGN v-tr-date = gltrans.tr-date.
       LEAVE GLTRANS-LOOP.
   END.

END.

IF v-tr-date = ? THEN
DO:
   IF begin_inv EQ 4515 THEN
      v-tr-date = 6/17/2008.
   ELSE IF begin_inv EQ 4516 THEN
      v-tr-date = 7/7/08.
   ELSE IF begin_inv EQ 4519 THEN
      v-tr-date = 7/9/2008.
   ELSE IF begin_inv EQ 4520 THEN
      v-tr-date = 7/8/2008.
   ELSE IF begin_inv EQ 24 THEN
      v-tr-date = 4/9/2008.
   ELSE IF begin_inv EQ 25 THEN
      v-tr-date = 4/1/2008.
   ELSE IF begin_inv EQ 26 THEN
      v-tr-date = 4/1/2008.
   ELSE
      v-tr-date = 7/9/2008.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-ar-ctrl wWin 
PROCEDURE reset-ar-ctrl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode
      EXCLUSIVE NO-ERROR NO-WAIT.

IF NOT AVAIL ar-ctrl THEN
DO:
   MESSAGE "The AR Control is currently locked. Cannot Update!"
       VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.

IF AVAIL ar-ctrl THEN
   ASSIGN  ar-ctrl.last-inv = v-last-inv.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process wWin 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


                       

session:set-wait-state("General").
message " Process Is Completed." view-as alert-box.
session:set-wait-state("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ar-inv wWin 
PROCEDURE update-ar-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME fmain:

ASSIGN {&displayed-fields}.

/* ASSIGN  ar-inv.sold-name   = ar-inv.sold-name:SCREEN-VALUE                       */
/*         ar-inv.ord-no      = int(ar-inv.ord-no:SCREEN-VALUE)                     */
/*         ar-inv.inv-no      = int(ar-inv.inv-no:SCREEN-VALUE)                     */
/*         ar-inv.est-no      = ar-inv.est-no:SCREEN-VALUE                          */
/*         ar-inv.ord-date    = date(ar-inv.ord-date:SCREEN-VALUE)                  */
/*         ar-inv.bill-to     = ar-inv.bill-to:SCREEN-VALUE                         */
/*         ar-inv.sold-city   = ar-inv.sold-city:SCREEN-VALUE                       */
/*         ar-inv.sold-zip    = ar-inv.sold-zip:SCREEN-VALUE                        */
/*         ar-inv.contact     = ar-inv.contact:SCREEN-VALUE                         */
/*         ar-inv.terms       = ar-inv.terms:SCREEN-VALUE                           */
/*         ar-inv.lead-days   = int(ar-inv.lead-days:SCREEN-VALUE)                  */
/*         ar-inv.frt-pay     = ar-inv.frt-pay:SCREEN-VALUE                         */
/*         ar-inv.fob-code    = ar-inv.fob-code:SCREEN-VALUE                        */
/*         ar-inv.under-pct   = dec(ar-inv.under-pct:SCREEN-VALUE)                  */
/*                                                                                  */
/*         ar-inv.carrier     = ar-inv.carrier:SCREEN-VALUE                         */
/*         ar-inv.fr-out-c    = dec(ar-inv.fr-out-c:SCREEN-VALUE)                   */
/*         ar-inv.fr-out-m    = dec(ar-inv.fr-out-m:SCREEN-VALUE)                   */
/*         ar-inv.over-pct    = dec(ar-inv.over-pct:SCREEN-VALUE)                   */
/*         ar-inv.last-date   = date(ar-inv.last-date:SCREEN-VALUE)                 */
/*         ar-inv.due-date    = date(ar-inv.due-date:SCREEN-VALUE)                  */
/*                                                                                  */
/*         ar-inv.type        = ar-inv.type:SCREEN-VALUE                            */
/*         ar-inv.t-weight    = int(ar-inv.t-weight:SCREEN-VALUE)                   */
/*         ar-inv.freight     = dec(ar-inv.freight:SCREEN-VALUE)                    */
/*         ar-inv.t-comm      = dec(ar-inv.t-comm:SCREEN-VALUE)                     */
/*         ar-inv.tax-amt     = dec(ar-inv.tax-amt:SCREEN-VALUE)                    */
/*         ar-inv.t-sales     = dec(ar-inv.t-sales:SCREEN-VALUE)                    */
/*         ar-inv.t-cost      = dec(ar-inv.t-cost:SCREEN-VALUE)                     */
/*         ar-inv.cust-no     = ar-inv.cust-no:SCREEN-VALUE                         */
/*                                                                                  */
/*         ar-inv.po-no       = ar-inv.po-no:SCREEN-VALUE                           */
/*         ar-inv.tot-ord     = dec(ar-inv.tot-ord:SCREEN-VALUE)                    */
/*         ar-inv.t-disc      = dec(ar-inv.t-disc:SCREEN-VALUE)                     */
/*         ar-inv.net         = dec(ar-inv.net:SCREEN-VALUE)                        */
/*                                                                                  */
/*         ar-inv.loc         = ar-inv.loc:SCREEN-VALUE                             */
/*         ar-inv.inv-date    = date(ar-inv.inv-date:SCREEN-VALUE)                  */
/*         ar-inv.sold-no     = int(ar-inv.sold-no:SCREEN-VALUE)                    */
/*         ar-inv.addr[1]     = ar-inv.addr[1]:SCREEN-VALUE                         */
/*         ar-inv.addr[2]     = ar-inv.addr[2]:SCREEN-VALUE                         */
/*         ar-inv.state       = ar-inv.state:SCREEN-VALUE                           */
/*         ar-inv.zip         = ar-inv.zip:SCREEN-VALUE                             */
/*                                                                                  */
/*         ar-inv.city        = ar-inv.city:SCREEN-VALUE                            */
/*         ar-inv.sold-state  = ar-inv.sold-state:SCREEN-VALUE                      */
/*         ar-inv.terms-d     = ar-inv.terms-d:SCREEN-VALUE                         */
/*         ar-inv.posted      = IF ar-inv.posted:SCREEN-VALUE = "YES" then true     */
/*                              ELSE FALSE.                                         */
/*                                                                                  */
/*         ASSIGN                                                                   */
/*         ar-inv.sold-addr[1] = ar-inv.sold-addr[1]:SCREEN-VALUE                   */
/*         ar-inv.sold-addr[2] = ar-inv.sold-addr[2]:SCREEN-VALUE                   */
/*         ar-inv.pord-no      = int(ar-inv.pord-no:SCREEN-VALUE)                   */
/*         ar-inv.job-no2      = int(ar-inv.job-no2:SCREEN-VALUE)                   */
/*         ar-inv.due-code     = ar-inv.due-code:SCREEN-VALUE                       */
/*         ar-inv.e-num        = int(ar-inv.e-num:SCREEN-VALUE)                     */
/*         ar-inv.est-type     = int(ar-inv.est-type:SCREEN-VALUE)                  */
/*         ar-inv.f-bill       = IF ar-inv.f-bill:SCREEN-VALUE = "yes" THEN TRUE    */
/*                               ELSE FALSE.                                        */
/*                                                                                  */
/*         ASSIGN                                                                   */
/*         ar-inv.prod-date    = date(ar-inv.prod-date:SCREEN-VALUE)                */
/*         ar-inv.stat         = ar-inv.stat:SCREEN-VALUE                           */
/*         ar-inv.tax-code     = ar-inv.tax-code:SCREEN-VALUE                       */
/*         ar-inv.period       = int(ar-inv.period:SCREEN-VALUE)                    */
/*         ar-inv.disc-taken   = dec(ar-inv.disc-taken:SCREEN-VALUE)                */
/*                                                                                  */
/*         ar-inv.paid         = dec(ar-inv.paid:SCREEN-VALUE)                      */
/*         ar-inv.job-no       = ar-inv.job-no:SCREEN-VALUE                         */
/*                                                                                  */
/*         ar-inv.ship-id      = ar-inv.ship-id:SCREEN-VALUE                        */
/*         ar-inv.gross        = dec(ar-inv.gross:SCREEN-VALUE)                     */
/*         ar-inv.due          = dec(ar-inv.due:SCREEN-VALUE)                       */
/*                                                                                  */
/*         ar-inv.check-no     = int(ar-inv.check-no:SCREEN-VALUE)                  */
/*         ar-inv.disc-%       = dec(ar-inv.disc-%:SCREEN-VALUE)                    */
/*                                                                                  */
/*         ar-inv.disc-days    = int(ar-inv.disc-days:SCREEN-VALUE)                 */
/*         ar-inv.pay-date     = date(ar-inv.pay-date:SCREEN-VALUE)                 */
/*         ar-inv.printed      = IF ar-inv.printed:SCREEN-VALUE = "YES" THEN TRUE   */
/*                               ELSE FALSE.                                        */
/*                                                                                  */
/*         ASSIGN                                                                   */
/*         ar-inv.sold-id      = ar-inv.sold-id:SCREEN-VALUE                        */
/*         ar-inv.user-id      = ar-inv.user-id:SCREEN-VALUE.                       */
/*         ar-inv.exported     = IF ar-inv.exported:SCREEN-VALUE = "YES" THEN TRUE  */
/*                               ELSE FALSE.                                        */
/*                                                                                  */
/*         ASSIGN                                                                   */
/*         ar-inv.bank-code    = ar-inv.bank-code:SCREEN-VALUE                      */
/*         ar-inv.rec_key      = ar-inv.rec_key:SCREEN-VALUE                        */
/*                                                                                  */
/*         ar-inv.upd-date     = date(ar-inv.upd-date:SCREEN-VALUE)                 */
/*         ar-inv.curr-code[1] = ar-inv.curr-code[1]:SCREEN-VALUE                   */
/*                                                                                  */
/*         ar-inv.ex-rate      = dec(ar-inv.ex-rate:SCREEN-VALUE)                   */
/*         ar-inv.freq-code    = ar-inv.freq-code:SCREEN-VALUE                      */
/*         ar-inv.recur        = IF ar-inv.recur:SCREEN-VALUE EQ "YES" THEN TRUE    */
/*                               ELSE FALSE                                         */
/*         ar-inv.fuel-bill    = IF ar-inv.fuel-bill:SCREEN-VALUE = "YES" THEN TRUE */
/*                               ELSE FALSE                                         */
/*         ar-inv.fuel         = dec(ar-inv.fuel-bill:SCREEN-VALUE).                */
/*                                                                                  */
/*                                                                                  */
/*                                                                                  */
/*                                                                                  */
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

