&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/vend.w

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
{custom/gcompany.i}
{custom/format.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

DEF BUFFER b-vend FOR vend.
DEF VAR vend-char-f AS CHAR NO-UNDO.
DEF VAR vend-log-f AS LOG NO-UNDO.
DEFINE VARIABLE l-valid AS LOGICAL NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.
{sys/ref/sys-ctrl.i}


&SCOPED-DEFINE vend-maint enable-vend-fields

&SCOPED-DEFINE where-poexport                  ~
    WHERE reftable.reftable EQ "vend.poexport" ~
      AND reftable.company  EQ vend.company    ~
      AND reftable.loc      EQ ""              ~
      AND reftable.code     EQ vend.vend-no

DO TRANSACTION:
  {sys/inc/aptax.i}
  {sys/inc/poexport.i}

  FOR EACH reftable WHERE reftable.reftable EQ "vend.poexport":
    FIND FIRST b-vend
        WHERE b-vend.company   EQ reftable.company
          AND b-vend.vend-no   EQ reftable.code
          AND b-vend.po-export EQ ""
        NO-ERROR.
    IF AVAIL b-vend THEN DO:
      b-vend.po-export = reftable.dscr.
      FIND CURRENT b-vend NO-LOCK.
    END.
    DELETE reftable.
  END.
  find first sys-ctrl
           where sys-ctrl.company eq g_company
             and sys-ctrl.name    eq "VendXfer"
           no-lock no-error.
         if not avail sys-ctrl then
         do:
           create sys-ctrl.
           assign
            sys-ctrl.company = g_company
            sys-ctrl.name    = "VendXfer"
            sys-ctrl.descrip = "Transfer Vendor Information to Sister Plants?".

         end.
         assign
            vend-char-f =  sys-ctrl.char-fld 
            vend-log-f = sys-ctrl.log-fld .
END.

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
&Scoped-define EXTERNAL-TABLES vend
&Scoped-define FIRST-EXTERNAL-TABLE vend


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR vend.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS vend.active vend.name vend.add1 vend.add2 ~
vend.city vend.state vend.zip vend.country vend.Postal vend.tax-id ~
vend.remit vend.r-add1 vend.r-add2 vend.r-city vend.r-state vend.r-zip ~
vend.r-country vend.r-postal vend.check-memo vend.type vend.contact ~
vend.buyer vend.area-code vend.phone vend.fax-area vend.fax vend.fax-prefix ~
vend.fax-country vend.over-pct vend.under-pct vend.actnum vend.curr-code ~
vend.tax-gr vend.code-1099 vend.an-edi-vend cb_paytype vend.terms vend.disc-% ~
vend.rebate-% vend.frt-pay vend.disc-days vend.carrier vend.fob-code ~
vend.loc 
&Scoped-define ENABLED-TABLES vend
&Scoped-define FIRST-ENABLED-TABLE vend
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-29 RECT-30 RECT-31 
&Scoped-Define DISPLAYED-FIELDS vend.vend-no vend.active vend.name ~
vend.add1 vend.add2 vend.city vend.state vend.zip vend.country vend.Postal ~
vend.tax-id vend.remit vend.r-add1 vend.r-add2 vend.r-city vend.r-state ~
vend.r-zip vend.r-country vend.r-postal vend.check-memo vend.type ~
vend.contact vend.buyer vend.area-code vend.phone vend.fax-area vend.fax ~
vend.fax-prefix vend.fax-country vend.over-pct vend.under-pct vend.actnum ~
vend.actdscr vend.curr-code vend.tax-gr vend.code-1099 vend.an-edi-vend cb_paytype ~
vend.terms vend.disc-% vend.po-export vend.rebate-% vend.frt-pay ~
vend.disc-days vend.carrier vend.fob-code vend.loc 
&Scoped-define DISPLAYED-TABLES vend
&Scoped-define FIRST-DISPLAYED-TABLE vend
&Scoped-Define DISPLAYED-OBJECTS ventype_Dscr buyer_buyer-n ~
terms_dscr carrier_dscr curr_dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS vend.vend-no 
&Scoped-define ADM-ASSIGN-FIELDS vend.actdscr vend.po-export 
&Scoped-define DISPLAY-FIELD F-Main vend.type vend.buyer vend.actnum ~
vend.terms vend.carrier 

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
DEFINE VARIABLE cb_paytype AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pay Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE buyer_buyer-n AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE carrier_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE curr_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE terms_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE ventype_Dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 16.91.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 8.57.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 3.81.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cb_paytype AT ROW 10.05 COL 124.6 COLON-ALIGNED WIDGET-ID 12
     vend.vend-no AT ROW 1.24 COL 10 COLON-ALIGNED
          LABEL "Vendor"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     vend.active AT ROW 1.24 COL 43 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Active", "A":U,
"Inactive", "I":U
          SIZE 29 BY .71
     vend.name AT ROW 2.19 COL 10 COLON-ALIGNED FORMAT "x(45)"
          VIEW-AS FILL-IN 
          SIZE 57 BY 1
          BGCOLOR 15 FONT 4
     vend.add1 AT ROW 3.62 COL 10 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.add2 AT ROW 4.57 COL 12 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.city AT ROW 5.52 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 15 FONT 4
     vend.state AT ROW 5.52 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     vend.zip AT ROW 5.52 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     vend.country AT ROW 6.48 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     vend.Postal AT ROW 6.48 COL 41 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     vend.tax-id AT ROW 7.67 COL 11 COLON-ALIGNED
          LABEL "Tax ID#"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     vend.remit AT ROW 10.76 COL 12 COLON-ALIGNED FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 51 BY 1
          BGCOLOR 15 FONT 4
     vend.r-add1 AT ROW 11.71 COL 12 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.r-add2 AT ROW 12.67 COL 12 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.r-city AT ROW 13.62 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 15 FONT 4
     vend.r-state AT ROW 13.62 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     vend.r-zip AT ROW 13.62 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 4
     vend.r-country AT ROW 14.57 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     vend.r-postal AT ROW 14.57 COL 45 COLON-ALIGNED
          LABEL "Postal Code"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     vend.check-memo AT ROW 16 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
          BGCOLOR 15 
     vend.type AT ROW 1.24 COL 83 COLON-ALIGNED
          LABEL "Type"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     ventype_Dscr AT ROW 1.24 COL 96 COLON-ALIGNED NO-LABEL
     vend.contact AT ROW 2.24 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE NO-VALIDATE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     vend.buyer AT ROW 3.19 COL 83 COLON-ALIGNED
          LABEL "Buyer"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     buyer_buyer-n AT ROW 3.24 COL 96 COLON-ALIGNED NO-LABEL
     vend.area-code AT ROW 4.29 COL 83 COLON-ALIGNED
          LABEL "Phone" FORMAT "(xxx)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     vend.phone AT ROW 4.29 COL 91 COLON-ALIGNED NO-LABEL FORMAT "xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
          BGCOLOR 15 FONT 4
     vend.fax-area AT ROW 4.33 COL 119 COLON-ALIGNED
          LABEL "Fax" FORMAT "(xxx)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     vend.fax AT ROW 4.33 COL 127 COLON-ALIGNED NO-LABEL FORMAT "xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FONT 4
     vend.fax-prefix AT ROW 5.29 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     vend.fax-country AT ROW 5.29 COL 114 COLON-ALIGNED
          LABEL "Fax Country"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     vend.over-pct AT ROW 6.48 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     vend.under-pct AT ROW 6.48 COL 133 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     vend.actnum AT ROW 7.67 COL 77 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     vend.actdscr AT ROW 7.67 COL 105 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
          BGCOLOR 7 FGCOLOR 0 
     vend.curr-code AT ROW 9.1 COL 84.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 
     vend.tax-gr AT ROW 10.05 COL 72.2 COLON-ALIGNED
          LABEL "Tax" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 
     vend.code-1099 AT ROW 10.05 COL 97.4 COLON-ALIGNED FORMAT "X"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
     vend.an-edi-vend AT ROW 10.05 COL 104.8
          LABEL "EDI"
          VIEW-AS TOGGLE-BOX
          SIZE 8.2 BY 1.05
     vend.terms AT ROW 11.24 COL 74.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     terms_dscr AT ROW 11.24 COL 87 COLON-ALIGNED NO-LABEL
     vend.disc-% AT ROW 12.29 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     vend.po-export AT ROW 12.29 COL 111 COLON-ALIGNED
          LABEL "POEXPORT" FORMAT "X(50)"
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     vend.rebate-% AT ROW 13.71 COL 112.4 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Max PO Cost" FORMAT ">>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     vend.frt-pay AT ROW 15.29 COL 83 COLON-ALIGNED
          LABEL "Freight Pay" FORMAT "X"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     vend.disc-days AT ROW 15.05 COL 121 COLON-ALIGNED
          LABEL "Lead Time Days"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE NO-VALIDATE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     vend.carrier AT ROW 16.24 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     carrier_dscr AT ROW 16.24 COL 98 COLON-ALIGNED NO-LABEL
     vend.fob-code AT ROW 14.57 COL 132 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Dest", "Dest":U,
"Orig", "Orig":U,
"None", ""
          SIZE 12 BY 2.62
     curr_dscr AT ROW 9.1 COL 94 COLON-ALIGNED NO-LABEL
     vend.loc AT ROW 14.24 COL 83 COLON-ALIGNED WIDGET-ID 4
          LABEL "Whs"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     "Status:" VIEW-AS TEXT
          SIZE 9 BY .76 AT ROW 1.24 COL 34
     "Default G/L#:" VIEW-AS TEXT
          SIZE 17 BY .71 AT ROW 7.67 COL 57
          FGCOLOR 9 FONT 6
     "Remit to Address" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 9.33 COL 5
          FGCOLOR 9 FONT 6
     "Shipping Information" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 13.62 COL 72
          FGCOLOR 9 FONT 6
     "FOB:" VIEW-AS TEXT
          SIZE 7 BY .76 AT ROW 13.86 COL 134
     RECT-1 AT ROW 1 COL 1
     RECT-29 AT ROW 8.86 COL 2
     RECT-30 AT ROW 13.62 COL 67
     RECT-31 AT ROW 8.86 COL 67
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE NO-VALIDATE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.vend
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
         HEIGHT             = 21.38
         WIDTH              = 160.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit 4 Custom                          */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN vend.actdscr IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN vend.actnum IN FRAME F-Main
   ALIGN-L 4 EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN vend.add1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.add2 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR TOGGLE-BOX vend.an-edi-vend IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.area-code IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN vend.buyer IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN buyer_buyer-n IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend.carrier IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN carrier_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend.code-1099 IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN curr_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend.disc-days IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.fax IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vend.fax-area IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN vend.fax-country IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.frt-pay IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN vend.loc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.name IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vend.phone IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vend.po-export IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN vend.r-add1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.r-postal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.rebate-% IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN vend.remit IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vend.tax-gr IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN vend.tax-id IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.terms IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN terms_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend.type IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN vend.vend-no IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ventype_Dscr IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME vend.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.actnum V-table-Win
ON LEAVE OF vend.actnum IN FRAME F-Main /* Account Number */
DO:
  if lastkey ne -1 then do:    
    run valid-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF SELF:MODIFIED THEN DO:
       FIND FIRST account WHERE account.company = gcompany
                            AND account.actnum = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAIL account THEN vend.actdscr:SCREEN-VALUE = account.dscr.
    END.
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.add1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.add1 V-table-Win
ON LEAVE OF vend.add1 IN FRAME F-Main /* Address */
DO:
  if adm-new-record and vend.r-add1:screen-value eq "" then
    vend.r-add1:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.add2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.add2 V-table-Win
ON LEAVE OF vend.add2 IN FRAME F-Main /* address line 2 */
DO:
  if adm-new-record and vend.r-add2:screen-value eq "" then
    vend.r-add2:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.buyer V-table-Win
ON LEAVE OF vend.buyer IN FRAME F-Main /* Buyer */
DO:
  if lastkey ne -1 then do:
    run valid-buyer NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.carrier V-table-Win
ON LEAVE OF vend.carrier IN FRAME F-Main /* Carrier */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.city
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.city V-table-Win
ON HELP OF vend.city IN FRAME F-Main /* City */
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.

  CASE FOCUS:NAME:
    WHEN "city" THEN DO:                   
       RUN windows/l-city.w (FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
       IF char-val NE "" THEN vend.city:SCREEN-VALUE = ENTRY(1,char-val).

       RUN vend-city.

       IF vend.zip:SCREEN-VALUE NE vend.zip THEN
        RUN zip-carrier.

        RETURN NO-APPLY.

    END.  /* vend.city*/
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.city V-table-Win
ON LEAVE OF vend.city IN FRAME F-Main /* City */
DO:
  if adm-new-record and vend.r-city:screen-value eq "" then
    vend.r-city:screen-value = {&self-name}:screen-value.

  IF LASTKEY NE -1 THEN
  DO:
     RUN vend-city.     
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.code-1099
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.code-1099 V-table-Win
ON LEAVE OF vend.code-1099 IN FRAME F-Main /* 1099 Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-code-1099 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.country
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.country V-table-Win
ON LEAVE OF vend.country IN FRAME F-Main /* Country */
DO:
  if adm-new-record and vend.r-country:screen-value eq "" then
    vend.r-country:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.curr-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.curr-code V-table-Win
ON LEAVE OF vend.curr-code IN FRAME F-Main /* Currency Code */
DO:
  if lastkey ne -1 then do:
    run valid-curr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.frt-pay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.frt-pay V-table-Win
ON LEAVE OF vend.frt-pay IN FRAME F-Main /* Freight Pay */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN display-frt-pay NO-ERROR .
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  END.
END.                    /* Task 10211302   */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.loc V-table-Win
ON HELP OF vend.loc IN FRAME F-Main /* Whs */
DO:
     DEF VAR char-val AS CHAR.
     run windows/l-loc.w (cocode,vend.loc:screen-value, output char-val).
     if char-val <> "" then 
        assign vend.loc:SCREEN-VALUE = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.loc V-table-Win
ON LEAVE OF vend.loc IN FRAME F-Main /* Whs */
DO:
    if lastkey ne -1 then do:
    run valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.name V-table-Win
ON LEAVE OF vend.name IN FRAME F-Main /* Name */
DO:
  if adm-new-record and vend.remit:screen-value eq "" then
    vend.remit:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.po-export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.po-export V-table-Win
ON HELP OF vend.po-export IN FRAME F-Main /* POEXPORT */
DO:
  DEF VAR char-val AS cha NO-UNDO.

  CASE FOCUS:NAME:
    WHEN "po-export" THEN DO:
      RUN windows/l-syschr.w (cocode, "POEXPORT", FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" THEN vend.po-export:SCREEN-VALUE = ENTRY(1,char-val).
    END.  /* vend.po-export*/
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.po-export V-table-Win
ON LEAVE OF vend.po-export IN FRAME F-Main /* POEXPORT */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-po-sec.
    IF NOT l-valid THEN RETURN NO-APPLY.
    RUN valid-po-export NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.Postal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.Postal V-table-Win
ON LEAVE OF vend.Postal IN FRAME F-Main /* Postal Code */
DO:
  if adm-new-record and vend.r-postal:screen-value eq "" then
    vend.r-postal:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.r-state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.r-state V-table-Win
ON LEAVE OF vend.r-state IN FRAME F-Main /* State */
DO:
  if lastkey ne -1 then do:
    run valid-r-state NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.state V-table-Win
ON LEAVE OF vend.state IN FRAME F-Main /* State */
DO:
  if lastkey ne -1 then do:
    run valid-state NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    if adm-new-record and vend.r-state:screen-value eq "" then
      vend.r-state:screen-value = {&self-name}:screen-value.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.tax-gr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.tax-gr V-table-Win
ON LEAVE OF vend.tax-gr IN FRAME F-Main /* Tax */
DO:
  if lastkey ne -1 then do:
    run valid-stax NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.terms V-table-Win
ON LEAVE OF vend.terms IN FRAME F-Main /* Terms */
DO:
  if lastkey ne -1 then do:
    run valid-terms NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.type V-table-Win
ON LEAVE OF vend.type IN FRAME F-Main /* Type */
DO:
  if lastkey ne -1 then do:
    run valid-vendtype NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.vend-no V-table-Win
ON LEAVE OF vend.vend-no IN FRAME F-Main /* Vendor */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-vend-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.zip V-table-Win
ON HELP OF vend.zip IN FRAME F-Main /* Zip */
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR city-val AS cha NO-UNDO.
  DEF VAR state-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.

  CASE FOCUS:NAME:
    WHEN "zip" THEN DO:                   

       RUN windows/l-zipcod.w (FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT city-val,OUTPUT state-val,OUTPUT rec-val).
       IF char-val NE "" THEN vend.zip:SCREEN-VALUE = ENTRY(1,char-val).
       IF city-val NE "" THEN vend.city:SCREEN-VALUE = ENTRY(1,city-val).
       IF state-val NE "" THEN vend.state:SCREEN-VALUE = ENTRY(1,state-val).

       RUN vend-zip.

    END.  /* vend.zip*/
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.zip V-table-Win
ON LEAVE OF vend.zip IN FRAME F-Main /* Zip */
DO:
  if adm-new-record and trim(vend.r-zip:screen-value) eq "-" then
    vend.r-zip:screen-value = {&self-name}:screen-value.

  IF LASTKEY NE -1 THEN
  DO:
     RUN vend-zip.

     IF vend.zip:SCREEN-VALUE NE vend.zip THEN
        RUN zip-carrier.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "vend"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "vend"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignCC V-table-Win 
PROCEDURE assignCC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   vend.payment-type = cb_paytype:SCREEN-VALUE IN FRAME {&frame-name}.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-payment-type-list V-table-Win 
PROCEDURE build-payment-type-list :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ilogic AS LOG NO-UNDO.
   cb_paytype:LIST-ITEMS IN FRAME {&frame-name} = "".

   FOR EACH payment-type NO-LOCK WHERE payment-type.company = cocode.
       ilogic = cb_paytype:ADD-LAST (payment-type.type) IN FRAME {&frame-name}.
   END.

   cb_payType:SCREEN-VALUE = cb_payType:ENTRY (1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-vend-fields V-table-Win 
PROCEDURE disable-vend-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE vend.po-export.
/*    DISABLE tb_cc tb_billpay.*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-frt-pay V-table-Win 
PROCEDURE display-frt-pay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Task 10211302   */

IF INDEX("PBC",vend.frt-pay:SCREEN-VALUE IN FRAME {&FRAME-NAME}) EQ 0 
    AND vend.frt-pay:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
    MESSAGE "Please type B for Billable, C for Collect, P for Prepaid or Leave Blank " 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN ERROR .
END.             /* Task 10211302   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayCC V-table-Win 
PROCEDURE displayCC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF AVAILABLE vend THEN cb_paytype:SCREEN-VALUE IN FRAME {&frame-name} = vend.payment-type. 

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-vend-fields V-table-Win 
PROCEDURE enable-vend-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF poexport-log AND poexport-cha EQ "Vendor" THEN ENABLE vend.po-export.
/*    ENABLE tb_cc tb_billpay.*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:


    IF vend.frt-pay:SCREEN-VALUE BEGINS '"' THEN
   vend.frt-pay:SCREEN-VALUE = "".
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
/* gdm - 07080903 */
DEF VAR v-old-poexport LIKE vend.po-export NO-UNDO.

ASSIGN v-old-poexport = vend.po-export.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

/* gdm - 07080903 */
  IF vend.po-export:SCREEN-VALUE IN FRAME {&FRAME-NAME}= "" AND 
     v-old-poexport NE ""
     THEN ASSIGN vend.po-export:SCREEN-VALUE = v-old-poexport
                 vend.po-export = v-old-poexport.
/* gdm - 07080903 end */

  /* Code placed here will execute AFTER standard behavior.    */
  RUN assignCC.
  IF adm-new-record THEN DO:
     IF vend-log-f THEN 
         RUN vend-new-log(vend-char-f).
 END.
 ELSE DO:
     IF vend-log-f THEN 
         RUN vend-update-log(vend-char-f).
 END.

  RUN reftable-values (NO).


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
  RUN disable-vend-fields.

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
  {methods/viewers/create/vend.i}

  DO WITH FRAME {&FRAME-NAME}:
    vend.po-export:SCREEN-VALUE = "".
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
  /*def var char-hdl as cha no-undo.
  DEF VAR ll-blank AS LOG NO-UNDO.*/
  DEFINE VAR thisOne AS CHAR NO-UNDO.
  DEFINE BUFFER buff-vend FOR vend .

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

    /* Code placed here will execute BEFORE standard behavior.    */ 
   IF vend-log-f THEN do:
      FIND CURRENT vend NO-LOCK NO-ERROR.
      DO I = 1 TO NUM-ENTRIES(vend-char-f):
          ASSIGN thisOne = ENTRY(i,vend-char-f).
          FIND FIRST buff-vend WHERE buff-vend.vend-no = vend.vend-no 
                                  AND buff-vend.company = thisOne EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL buff-vend THEN
              DELETE buff-vend .
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
  IF AVAILABLE vend THEN RUN build-payment-type-list.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


  RUN displayCC.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lNewRec AS LOG NO-UNDO.
DEFINE VARIABLE container-hdl AS CHARACTER     NO-UNDO.

  IF adm-new-record THEN
    lNewRec = TRUE.
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-vend-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  RUN valid-po-sec.
  IF NOT l-valid THEN RETURN NO-APPLY.

  run valid-vendtype.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  IF vend.actnum:MODIFIED IN FRAME {&frame-name} THEN DO:
       FIND FIRST account WHERE account.company = gcompany
                            AND account.actnum = vend.actnum:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAIL account THEN vend.actdscr:SCREEN-VALUE = account.dscr.
  END.

  run valid-buyer NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  run valid-state NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  run valid-actnum NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  run valid-stax NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-code-1099 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  run valid-terms NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-po-export NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  run valid-r-state NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  run valid-carrier NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  

  run valid-loc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  

  run display-frt-pay  NO-ERROR.        /* Task 10211302   */
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .  

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-vend-fields.

  IF lNewRec THEN DO WITH FRAME {&FRAME-NAME}:

      run get-link-handle in adm-broker-hdl(this-procedure,"container-source", output container-hdl).
      run passNewVend IN widget-handle(container-hdl) (vend.vend-no:SCREEN-VALUE).
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


  IF AVAIL vend THEN DO:
    /*FIND FIRST reftable {&where-poexport} NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.reftable = "vend.poexport"
       reftable.company  = vend.company
       reftable.loc      = ""
       reftable.code     = vend.vend-no.
    END.

    IF ip-display THEN
      fi_export = reftable.dscr.
    ELSE
      reftable.dscr = fi_export.*/
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
  {src/adm/template/snd-list.i "vend"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum V-table-Win 
PROCEDURE valid-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
&Scoped-define SELF-NAME vend.actnum

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AP GL#"
    no-lock no-error.

if not avail sys-ctrl                                     or
   not sys-ctrl.log-fld                                   or
   {&self-name}:screen-value in frame {&frame-name} ne "" then do:
  {custom/validate/account.i &where = sys/look/faccnumW.i}
  IF NOT v-avail THEN RETURN ERROR.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-buyer V-table-Win 
PROCEDURE valid-buyer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
&Scoped-define SELF-NAME vend.buyer

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


if {&self-name}:screen-value in frame {&frame-name} ne "" then do:
  {custom/validate/buyer.i &where = sys\look/buyerW.i}
  IF NOT v-avail THEN RETURN ERROR.
end.

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
&Scoped-define SELF-NAME vend.carrier

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


if {&self-name}:screen-value in frame {&frame-name} ne "" then do:
  {custom/validate/carrier.i &where = sys\look/carrierW.i}
  IF NOT v-avail THEN RETURN ERROR.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-code-1099 V-table-Win 
PROCEDURE valid-code-1099 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    vend.code-1099:SCREEN-VALUE = CAPS(vend.code-1099:SCREEN-VALUE).

    IF LOOKUP(TRIM(vend.code-1099:SCREEN-VALUE),",Y,N") LE 0 THEN DO:
      MESSAGE TRIM(vend.code-1099:LABEL) + " " + "may be space, 'Y', or 'N'..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend.code-1099.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-curr V-table-Win 
PROCEDURE valid-curr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
&Scoped-define SELF-NAME vend.curr-code

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


if {&self-name}:screen-value in frame {&frame-name} ne "" then do:
   {custom/validate/currency.i &where = sys/look/curr.i }
  IF NOT v-avail THEN RETURN ERROR.
end.



  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc V-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
&Scoped-define SELF-NAME vend.loc

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


if {&self-name}:screen-value in frame {&frame-name} ne "" then do:
  {custom/validate/loc.i &where = sys/look/locW.i}
  IF NOT v-avail THEN RETURN ERROR.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-export V-table-Win 
PROCEDURE valid-po-export :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ls-name-value AS cha NO-UNDO.
  DEF VAR ls-field-value AS cha NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ls-field-value = TRIM(vend.po-export:SCREEN-VALUE).

    IF ls-field-value GT "" THEN DO:
      IF CAN-DO(name-fld-list,"POEXPORT") THEN DO:
        ls-name-value = str-init[LOOKUP("POEXPORT",name-fld-list)].

        IF ls-field-value EQ "Vendor" THEN
          lv-msg = TRIM(vend.po-export:LABEL) + "cannot be Vendor...".
        ELSE
        IF CAN-DO(ls-name-value,ls-field-value) THEN
          vend.po-export:SCREEN-VALUE = ENTRY(LOOKUP(ls-field-value,ls-name-value),ls-name-value).
        ELSE
          lv-msg = TRIM(ls-field-value) + " is not on lookup...".
      END.
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend.po-export.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-sec V-table-Win 
PROCEDURE valid-po-sec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  l-valid = YES.

  DO WITH FRAME {&frame-name}:    
    IF NOT ll-secure                                              AND
       STRING(vend.po-export) NE vend.po-export:SCREEN-VALUE   THEN DO:

      RUN sys/ref/d-passmi.w (OUTPUT ll-secure).

      IF NOT ll-secure THEN
        ASSIGN
         l-valid                   = NO
         vend.po-export:SCREEN-VALUE = STRING(vend.po-export).
    END.

    IF NOT l-valid THEN APPLY "entry" TO vend.po-export.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-r-state V-table-Win 
PROCEDURE valid-r-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
&Scoped-define SELF-NAME vend.r-state

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


if {&self-name}:screen-value in frame {&frame-name} ne "" then do:
  {custom/validate/state.i &where = sys/look/trueW.i}
  IF NOT v-avail THEN RETURN ERROR.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-state V-table-Win 
PROCEDURE valid-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
&Scoped-define SELF-NAME vend.state

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


if {&self-name}:screen-value in frame {&frame-name} ne "" then do:
  {custom/validate/state.i &where = sys/look/trueW.i}
  IF NOT v-avail THEN RETURN ERROR.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-stax V-table-Win 
PROCEDURE valid-stax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
&Scoped-define SELF-NAME vend.tax-gr

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


IF aptax-log                                              OR
   {&self-name}:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
  {custom/validate/stax.i &where = sys/look/staxW.i}
  IF NOT v-avail THEN RETURN ERROR.
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
&Scoped-define SELF-NAME vend.terms

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


if {&self-name}:screen-value in frame {&frame-name} ne "" then do:
  {custom/validate/terms.i &where = sys/look/termsW.i}
  IF NOT v-avail THEN RETURN ERROR.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend-no V-table-Win 
PROCEDURE valid-vend-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF vend.vend-no:SCREEN-VALUE EQ "" OR
       CAN-FIND(FIRST b-vend WHERE b-vend.company EQ cocode
                               AND b-vend.vend-no EQ vend.vend-no:SCREEN-VALUE
                               AND ROWID(b-vend)  NE ROWID(vend)) THEN DO:
      MESSAGE TRIM(vend.vend-no:LABEL) + " " +
              TRIM(IF vend.vend-no:SCREEN-VALUE EQ "" THEN "may not be spaces"
                                                      ELSE "already exists") +
              "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend.vend-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vendtype V-table-Win 
PROCEDURE valid-vendtype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
&Scoped-define SELF-NAME vend.type

DEF VAR v-avail AS LOG INIT YES NO-UNDO.


if {&self-name}:screen-value in frame {&frame-name} ne "" then do:
  {custom/validate/ventype.i &where = sys/look/trueW.i}
  IF NOT v-avail THEN RETURN ERROR.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vend-city V-table-Win 
PROCEDURE vend-city :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF vend.city:SCREEN-VALUE NE "" THEN
    FIND FIRST nosweat.zipcode
        WHERE nosweat.zipcode.city EQ vend.city:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL nosweat.zipcode THEN do:
      ASSIGN
        vend.state:SCREEN-VALUE = nosweat.zipcode.state
        vend.zip:SCREEN-VALUE = nosweat.zipcode.zipcode  .      
    END.   
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vend-new-log V-table-Win 
PROCEDURE vend-new-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER vend-char AS CHAR NO-UNDO.
DEF VAR thisOne AS CHAR NO-UNDO.
 DEFINE BUFFER buff-vend FOR vend .
 FIND CURRENT vend NO-LOCK.
 DO I = 1 TO NUM-ENTRIES(vend-char):
     ASSIGN thisOne = ENTRY(i,vend-char).
     CREATE buff-vend .
     BUFFER-COPY vend EXCEPT company  TO buff-vend.
     ASSIGN buff-vend.company = thisone.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vend-update-log V-table-Win 
PROCEDURE vend-update-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER vend-char AS CHAR NO-UNDO.
 FIND CURRENT vend NO-LOCK.
 DEF VAR thisOne AS CHAR NO-UNDO.
 DEFINE BUFFER bf-vend FOR vend .

   DO I = 1 TO NUM-ENTRIES(vend-char):
     ASSIGN thisOne = ENTRY(i,vend-char).
     FIND FIRST bf-vend WHERE bf-vend.vend-no = vend.vend-no 
                          AND bf-vend.company = thisOne EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL bf-vend THEN do:
     BUFFER-COPY vend EXCEPT company frt-pay Purch last-year ytd-msf lyytd-msf hibal hibal-date num-inv lpay 
         lpay-date avg-pay acc-bal ord-bal TO bf-vend.
     END.
     ELSE DO:
     CREATE bf-vend .
     BUFFER-COPY vend EXCEPT company frt-pay Purch last-year ytd-msf lyytd-msf hibal hibal-date num-inv lpay 
         lpay-date avg-pay acc-bal ord-bal TO bf-vend.
     ASSIGN bf-vend.company = thisone.
     END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vend-zip V-table-Win 
PROCEDURE vend-zip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF vend.zip:SCREEN-VALUE NE "" THEN
    FIND FIRST nosweat.zipcode
        WHERE nosweat.zipcode.zipcode EQ vend.zip:SCREEN-VALUE        
        NO-LOCK NO-ERROR.
    IF AVAIL nosweat.zipcode THEN do:
      ASSIGN
        vend.state:SCREEN-VALUE = nosweat.zipcode.state
        vend.city:SCREEN-VALUE = nosweat.zipcode.city.    
    END.   
  END.
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
   FIND FIRST nosweat.zipcode
        WHERE nosweat.zipcode.zipcode EQ vend.zip:SCREEN-VALUE
        NO-LOCK NO-ERROR.

   ASSIGN
      vend.carrier:SCREEN-VALUE = IF AVAIL zipcode AND 
                                     TRIM(nosweat.zipcode.carrier) NE "" 
                                    THEN nosweat.zipcode.carrier 
                                    ELSE vend.carrier:SCREEN-VALUE.     
      /* gdm - 10010913 end*/
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

