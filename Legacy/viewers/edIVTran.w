&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewerid/<table>.w

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
&Scoped-define EXTERNAL-TABLES EDIVTran
&Scoped-define FIRST-EXTERNAL-TABLE EDIVTran


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDIVTran.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS EDIVTran.Partner EDIVTran.BOL-No ~
EDIVTran.Invoice-no EDIVTran.Cust EDIVTran.Cust-po EDIVTran.Invoice-date ~
EDIVTran.Ship-Date EDIVTran.Contact-name EDIVTran.Company ~
EDIVTran.Contact-phone EDIVTran.Contact-phone-qual EDIVTran.FOB-Text ~
EDIVTran.Terms EDIVTran.Terms-type EDIVTran.Tot-volume EDIVTran.Volume-uom ~
EDIVTran.Tot-disc EDIVTran.Terms-net-days EDIVTran.Terms-basis ~
EDIVTran.Tot-net EDIVTran.Tot-Gross EDIVTran.Terms-desc[1] EDIVTran.Tot-frt ~
EDIVTran.Tot-cartons EDIVTran.Carton-uom-code EDIVTran.Terms-desc[2] ~
EDIVTran.Tot-wght EDIVTran.Tot-qty EDIVTran.Wght-uom ~
EDIVTran.Terms-disc-days EDIVTran.Terms-disc-date EDIVTran.Lines ~
EDIVTran.Last-line EDIVTran.Terms-disc-pct EDIVTran.Terms-disc-amt ~
EDIVTran.Terms-net-date EDIVTran.Terms-day-of-month EDIVTran.Cust-po-date ~
EDIVTran.Ship-date-code EDIVTran.Del-date EDIVTran.Del-date-qual ~
EDIVTran.Misc-date1 EDIVTran.Misc-date1-code EDIVTran.Pro-Number ~
EDIVTran.Sf-code EDIVTran.St-code EDIVTran.Carrier EDIVTran.Ship-stat ~
EDIVTran.By-code EDIVTran.Carrier-code EDIVTran.Trailer-Number ~
EDIVTran.Routing[1] EDIVTran.Release-no 
&Scoped-define ENABLED-TABLES EDIVTran
&Scoped-define FIRST-ENABLED-TABLE EDIVTran
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 RECT-6 RECT-7 
&Scoped-Define DISPLAYED-FIELDS EDIVTran.Partner EDIVTran.BOL-No ~
EDIVTran.Invoice-no EDIVTran.Cust EDIVTran.Cust-po EDIVTran.Invoice-date ~
EDIVTran.Ship-Date EDIVTran.Contact-name EDIVTran.Company ~
EDIVTran.Contact-phone EDIVTran.Contact-phone-qual EDIVTran.FOB-Text ~
EDIVTran.Terms EDIVTran.Terms-type EDIVTran.Tot-volume EDIVTran.Volume-uom ~
EDIVTran.Tot-disc EDIVTran.Terms-net-days EDIVTran.Terms-basis ~
EDIVTran.Tot-net EDIVTran.Tot-Gross EDIVTran.Terms-desc[1] EDIVTran.Tot-frt ~
EDIVTran.Tot-cartons EDIVTran.Carton-uom-code EDIVTran.Terms-desc[2] ~
EDIVTran.Tot-wght EDIVTran.Tot-qty EDIVTran.Wght-uom ~
EDIVTran.Terms-disc-days EDIVTran.Terms-disc-date EDIVTran.Lines ~
EDIVTran.Last-line EDIVTran.Terms-disc-pct EDIVTran.Terms-disc-amt ~
EDIVTran.Terms-net-date EDIVTran.Terms-day-of-month EDIVTran.Cust-po-date ~
EDIVTran.Ship-date-code EDIVTran.Del-date EDIVTran.Del-date-qual ~
EDIVTran.Misc-date1 EDIVTran.Misc-date1-code EDIVTran.Pro-Number ~
EDIVTran.Sf-code EDIVTran.St-code EDIVTran.Carrier EDIVTran.Ship-stat ~
EDIVTran.By-code EDIVTran.Carrier-code EDIVTran.Trailer-Number ~
EDIVTran.Routing[1] EDIVTran.Release-no 
&Scoped-define DISPLAYED-TABLES EDIVTran
&Scoped-define FIRST-DISPLAYED-TABLE EDIVTran


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,List-4,List-5,F1   */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|asi.EDIVTran.company
Partner||y|asi.EDIVTran.Partner
Carrier||y|asi.EDIVTran.Carrier
rec_key||y|asi.EDIVTran.rec_key
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,Partner,Carrier,rec_key"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 150 BY 24.29.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 7.81.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 7.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     EDIVTran.Partner AT ROW 1.19 COL 56 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     EDIVTran.BOL-No AT ROW 1.19 COL 85.2 COLON-ALIGNED WIDGET-ID 2
          LABEL "BOL No"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     EDIVTran.Invoice-no AT ROW 1.24 COL 16 COLON-ALIGNED WIDGET-ID 56
          LABEL "Invoice No"
          VIEW-AS FILL-IN 
          SIZE 28.4 BY 1
     EDIVTran.Cust AT ROW 2.91 COL 19.6 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     EDIVTran.Cust-po AT ROW 2.91 COL 85 COLON-ALIGNED WIDGET-ID 40
          LABEL "Cust PO"
          VIEW-AS FILL-IN 
          SIZE 28.4 BY 1
     EDIVTran.Invoice-date AT ROW 3.81 COL 85 COLON-ALIGNED WIDGET-ID 54
          LABEL "Invoice Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     EDIVTran.Ship-Date AT ROW 3.86 COL 19.6 COLON-ALIGNED WIDGET-ID 98
          LABEL "Ship Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     EDIVTran.Contact-name AT ROW 4.81 COL 19.6 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     EDIVTran.Company AT ROW 4.81 COL 85 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDIVTran.Contact-phone AT ROW 5.81 COL 19.4 COLON-ALIGNED WIDGET-ID 20
          LABEL "Contact Phone"
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
     EDIVTran.Contact-phone-qual AT ROW 5.81 COL 54.4 COLON-ALIGNED WIDGET-ID 22
          LABEL "Qual"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVTran.FOB-Text AT ROW 7.38 COL 19.6 COLON-ALIGNED WIDGET-ID 52
          LABEL "FOB Text"
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     EDIVTran.Terms AT ROW 9 COL 101 COLON-ALIGNED WIDGET-ID 110
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     EDIVTran.Terms-type AT ROW 9 COL 130 COLON-ALIGNED WIDGET-ID 132
          LABEL "TermsType"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVTran.Tot-volume AT ROW 9.19 COL 19 COLON-ALIGNED WIDGET-ID 146
          LABEL "Tot Volume"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDIVTran.Volume-uom AT ROW 9.19 COL 37.8 COLON-ALIGNED WIDGET-ID 156
          LABEL "Uom"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVTran.Tot-disc AT ROW 9.19 COL 60 COLON-ALIGNED WIDGET-ID 136
          LABEL "Tot Disc"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDIVTran.Terms-net-days AT ROW 9.95 COL 101 COLON-ALIGNED WIDGET-ID 130
          LABEL "Net Days"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDIVTran.Terms-basis AT ROW 9.95 COL 130 COLON-ALIGNED WIDGET-ID 112
          LABEL "Basis"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVTran.Tot-net AT ROW 10.14 COL 60 COLON-ALIGNED WIDGET-ID 142
          LABEL "Tot Net"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDIVTran.Tot-Gross AT ROW 10.19 COL 19 COLON-ALIGNED WIDGET-ID 140
          LABEL "Tot Gross"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDIVTran.Terms-desc[1] AT ROW 10.95 COL 101.2 COLON-ALIGNED WIDGET-ID 116
          LABEL "Desc"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     EDIVTran.Tot-frt AT ROW 11.14 COL 19 COLON-ALIGNED WIDGET-ID 138
          LABEL "Tot Frt"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     EDIVTran.Tot-cartons AT ROW 11.14 COL 60 COLON-ALIGNED WIDGET-ID 134
          LABEL "Tot Cartons"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDIVTran.Carton-uom-code AT ROW 11.14 COL 79.4 COLON-ALIGNED WIDGET-ID 12
          LABEL "Uom"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVTran.Terms-desc[2] AT ROW 11.91 COL 101.2 COLON-ALIGNED NO-LABEL WIDGET-ID 118
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     EDIVTran.Tot-wght AT ROW 12.1 COL 19 COLON-ALIGNED WIDGET-ID 148
          LABEL "Tot Wght"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDIVTran.Tot-qty AT ROW 12.1 COL 60 COLON-ALIGNED WIDGET-ID 144
          LABEL "Tot Qty"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     EDIVTran.Wght-uom AT ROW 12.19 COL 38.4 COLON-ALIGNED WIDGET-ID 158
          LABEL "Uom"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVTran.Terms-disc-days AT ROW 12.91 COL 101.2 COLON-ALIGNED WIDGET-ID 124
          LABEL "Disc Days"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDIVTran.Terms-disc-date AT ROW 12.91 COL 128 COLON-ALIGNED WIDGET-ID 122
          LABEL "Discount Date"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     EDIVTran.Lines AT ROW 13.05 COL 19 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDIVTran.Last-line AT ROW 13.05 COL 60 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     EDIVTran.Terms-disc-pct AT ROW 13.91 COL 133.8 COLON-ALIGNED WIDGET-ID 126
          LABEL "Disc Pct"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDIVTran.Terms-disc-amt AT ROW 13.95 COL 101 COLON-ALIGNED WIDGET-ID 120
          LABEL "Disc Amt"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     EDIVTran.Terms-net-date AT ROW 14.95 COL 101 COLON-ALIGNED WIDGET-ID 128
          LABEL "Net Date"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     EDIVTran.Terms-day-of-month AT ROW 14.95 COL 139.2 COLON-ALIGNED WIDGET-ID 114
          LABEL "Day Of Month"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVTran.Cust-po-date AT ROW 17 COL 21 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     EDIVTran.Ship-date-code AT ROW 17 COL 59.6 COLON-ALIGNED WIDGET-ID 100
          LABEL "Ship Date Code"
          VIEW-AS FILL-IN 
          SIZE 7.4 BY 1
     EDIVTran.Del-date AT ROW 17.95 COL 21 COLON-ALIGNED WIDGET-ID 44
          LABEL "Del Date"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     EDIVTran.Del-date-qual AT ROW 17.95 COL 59.6 COLON-ALIGNED WIDGET-ID 46
          LABEL "Qual"
          VIEW-AS FILL-IN 
          SIZE 7.4 BY 1
     EDIVTran.Misc-date1 AT ROW 18.91 COL 21 COLON-ALIGNED WIDGET-ID 62
          LABEL "Misc Date"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     EDIVTran.Misc-date1-code AT ROW 18.91 COL 59.6 COLON-ALIGNED WIDGET-ID 64
          LABEL "Qual"
          VIEW-AS FILL-IN 
          SIZE 7.4 BY .95
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     EDIVTran.Pro-Number AT ROW 21 COL 21.2 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     EDIVTran.Sf-code AT ROW 21 COL 81.6 COLON-ALIGNED WIDGET-ID 96
          LABEL "Ship From"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDIVTran.St-code AT ROW 21 COL 114 COLON-ALIGNED WIDGET-ID 108
          LABEL "Ship To"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDIVTran.Carrier AT ROW 21.95 COL 21.2 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     EDIVTran.Ship-stat AT ROW 21.95 COL 81.6 COLON-ALIGNED WIDGET-ID 102
          LABEL "Ship Stat"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     EDIVTran.By-code AT ROW 22 COL 114 COLON-ALIGNED WIDGET-ID 6
          LABEL "By Code"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     EDIVTran.Carrier-code AT ROW 22.91 COL 21.2 COLON-ALIGNED WIDGET-ID 10
          LABEL "Carrier Code"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     EDIVTran.Trailer-Number AT ROW 22.91 COL 81.6 COLON-ALIGNED WIDGET-ID 150
          LABEL "Trailer Number"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     EDIVTran.Routing[1] AT ROW 23.86 COL 21 COLON-ALIGNED WIDGET-ID 86
          LABEL "Routing"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     EDIVTran.Release-no AT ROW 23.86 COL 81.6 COLON-ALIGNED WIDGET-ID 84
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     "Shipping Info" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 20.29 COL 6.2 WIDGET-ID 170
          FGCOLOR 9 
     "Terms" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.38 COL 89 WIDGET-ID 162
          FGCOLOR 9 
     "Totals" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.38 COL 5 WIDGET-ID 166
          FGCOLOR 9 
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 8.67 COL 87 WIDGET-ID 160
     RECT-6 AT ROW 8.67 COL 2 WIDGET-ID 164
     RECT-7 AT ROW 20.52 COL 4 WIDGET-ID 168
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.EDIVTran
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
         HEIGHT             = 24.62
         WIDTH              = 151.2.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN EDIVTran.BOL-No IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.By-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Carrier-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Carton-uom-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Contact-phone IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Contact-phone-qual IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Cust-po IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Del-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Del-date-qual IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.FOB-Text IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Invoice-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Invoice-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Misc-date1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Misc-date1-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Routing[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Sf-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Ship-Date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Ship-date-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Ship-stat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.St-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-basis IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-day-of-month IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-desc[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-disc-amt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-disc-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-disc-days IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-disc-pct IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-net-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-net-days IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Terms-type IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Tot-cartons IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Tot-disc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Tot-frt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Tot-Gross IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Tot-net IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Tot-qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Tot-volume IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Tot-wght IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Trailer-Number IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Volume-uom IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN EDIVTran.Wght-uom IN FRAME F-Main
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "EDIVTran"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDIVTran"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "EDIVTran" "company"}
  {src/adm/template/sndkycas.i "Partner" "EDIVTran" "Partner"}
  {src/adm/template/sndkycas.i "Carrier" "EDIVTran" "Carrier"}
  {src/adm/template/sndkycas.i "rec_key" "EDIVTran" "rec_key"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "EDIVTran"}

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

