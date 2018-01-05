&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters: <none>

  Output Parameters: <none>

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

&SCOPED-DEFINE setBrowseFocus
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES EDIVTran

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table EDIVTran.Invoice-no ~
EDIVTran.Partner EDIVTran.Vendor EDIVTran.Cust EDIVTran.Tot-net ~
EDIVTran.Tot-Gross EDIVTran.Tot-cartons EDIVTran.Wght-uom ~
EDIVTran.Volume-uom EDIVTran.Vn-code EDIVTran.Trailer-Number ~
EDIVTran.Tot-wght EDIVTran.Tot-volume EDIVTran.Tot-qty EDIVTran.Tot-frt ~
EDIVTran.Tot-disc EDIVTran.Terms-type EDIVTran.Terms-net-days ~
EDIVTran.Terms-net-date EDIVTran.Terms-disc-pct EDIVTran.Terms-disc-days ~
EDIVTran.Terms-disc-date EDIVTran.Terms-disc-amt EDIVTran.Terms-desc[2] ~
EDIVTran.Terms-desc[1] EDIVTran.Terms-day-of-month EDIVTran.Terms-basis ~
EDIVTran.Terms EDIVTran.St-code EDIVTran.Special-svc-code EDIVTran.Sn-code ~
EDIVTran.Ship-stat EDIVTran.Ship-date-code EDIVTran.Ship-Date ~
EDIVTran.Sf-code EDIVTran.Seq EDIVTran.Routing[4] EDIVTran.Routing[3] ~
EDIVTran.Routing[2] EDIVTran.Routing[1] EDIVTran.Release-no ~
EDIVTran.Ref3-code EDIVTran.Ref3 EDIVTran.Ref2-code EDIVTran.Ref2 ~
EDIVTran.rec_key EDIVTran.Re-code EDIVTran.Promo-code EDIVTran.Pro-Number ~
EDIVTran.Misc-date1-code EDIVTran.Misc-date1 EDIVTran.Lines ~
EDIVTran.Last-line EDIVTran.Invoice-date EDIVTran.FOB-Text ~
EDIVTran.FOB-Qual EDIVTran.FOB-Code EDIVTran.Del-date-qual ~
EDIVTran.Del-date EDIVTran.Cust-po-date EDIVTran.Cust-po EDIVTran.Cust-div ~
EDIVTran.Cust-dept EDIVTran.Curr-seller EDIVTran.Curr-rate-seller ~
EDIVTran.Curr-rate-buyer EDIVTran.Curr-buyer EDIVTran.Contract ~
EDIVTran.Contact-phone-qual EDIVTran.Contact-phone EDIVTran.Contact-name ~
EDIVTran.Contact-funct-code EDIVTran.Company EDIVTran.Carton-uom-code ~
EDIVTran.Carrier-code EDIVTran.Carrier EDIVTran.By-code EDIVTran.Bt-code ~
EDIVTran.BOL-No 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH EDIVTran WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH EDIVTran WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table EDIVTran
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table EDIVTran


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      EDIVTran
    FIELDS(EDIVTran.Invoice-no
      EDIVTran.Partner
      EDIVTran.Vendor
      EDIVTran.Cust
      EDIVTran.Tot-net
      EDIVTran.Tot-Gross
      EDIVTran.Tot-cartons
      EDIVTran.Wght-uom
      EDIVTran.Volume-uom
      EDIVTran.Vn-code
      EDIVTran.Trailer-Number
      EDIVTran.Tot-wght
      EDIVTran.Tot-volume
      EDIVTran.Tot-qty
      EDIVTran.Tot-frt
      EDIVTran.Tot-disc
      EDIVTran.Terms-type
      EDIVTran.Terms-net-days
      EDIVTran.Terms-net-date
      EDIVTran.Terms-disc-pct
      EDIVTran.Terms-disc-days
      EDIVTran.Terms-disc-date
      EDIVTran.Terms-disc-amt
      EDIVTran.Terms-desc[2]
      EDIVTran.Terms-desc[1]
      EDIVTran.Terms-day-of-month
      EDIVTran.Terms-basis
      EDIVTran.Terms
      EDIVTran.St-code
      EDIVTran.Special-svc-code
      EDIVTran.Sn-code
      EDIVTran.Ship-stat
      EDIVTran.Ship-date-code
      EDIVTran.Ship-Date
      EDIVTran.Sf-code
      EDIVTran.Seq
      EDIVTran.Routing[4]
      EDIVTran.Routing[3]
      EDIVTran.Routing[2]
      EDIVTran.Routing[1]
      EDIVTran.Release-no
      EDIVTran.Ref3-code
      EDIVTran.Ref3
      EDIVTran.Ref2-code
      EDIVTran.Ref2
      EDIVTran.rec_key
      EDIVTran.Re-code
      EDIVTran.Promo-code
      EDIVTran.Pro-Number
      EDIVTran.Misc-date1-code
      EDIVTran.Misc-date1
      EDIVTran.Lines
      EDIVTran.Last-line
      EDIVTran.Invoice-date
      EDIVTran.FOB-Text
      EDIVTran.FOB-Qual
      EDIVTran.FOB-Code
      EDIVTran.Del-date-qual
      EDIVTran.Del-date
      EDIVTran.Cust-po-date
      EDIVTran.Cust-po
      EDIVTran.Cust-div
      EDIVTran.Cust-dept
      EDIVTran.Curr-seller
      EDIVTran.Curr-rate-seller
      EDIVTran.Curr-rate-buyer
      EDIVTran.Curr-buyer
      EDIVTran.Contract
      EDIVTran.Contact-phone-qual
      EDIVTran.Contact-phone
      EDIVTran.Contact-name
      EDIVTran.Contact-funct-code
      EDIVTran.Company
      EDIVTran.Carton-uom-code
      EDIVTran.Carrier-code
      EDIVTran.Carrier
      EDIVTran.By-code
      EDIVTran.Bt-code
      EDIVTran.BOL-No) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      EDIVTran.Invoice-no COLUMN-LABEL "Invoice No" FORMAT "x(22)":U
      EDIVTran.Partner FORMAT "x(15)":U
      EDIVTran.Vendor FORMAT "x(10)":U
      EDIVTran.Cust FORMAT "x(10)":U
      EDIVTran.Tot-net COLUMN-LABEL "Total Net Amt" FORMAT "->,>>>,>>>.99":U
      EDIVTran.Tot-Gross COLUMN-LABEL "Total Gross Amount" FORMAT "->,>>>,>>>.99":U
      EDIVTran.Tot-cartons COLUMN-LABEL "Total cartons" FORMAT "->>>>>>9":U
      EDIVTran.Wght-uom FORMAT "x(2)":U
      EDIVTran.Volume-uom FORMAT "x(2)":U
      EDIVTran.Vn-code FORMAT "x(12)":U
      EDIVTran.Trailer-Number FORMAT "x(15)":U
      EDIVTran.Tot-wght FORMAT ">>>>>>>9":U
      EDIVTran.Tot-volume FORMAT "->>>>>>9":U
      EDIVTran.Tot-qty FORMAT "->>>>>>>>>.9999":U
      EDIVTran.Tot-frt FORMAT "->,>>>,>>>.99":U
      EDIVTran.Tot-disc FORMAT "->,>>>,>>>.99":U
      EDIVTran.Terms-type FORMAT "x(2)":U
      EDIVTran.Terms-net-days FORMAT ">>9":U
      EDIVTran.Terms-net-date FORMAT "99/99/9999":U
      EDIVTran.Terms-disc-pct FORMAT ">>>.999":U
      EDIVTran.Terms-disc-days FORMAT ">>9":U
      EDIVTran.Terms-disc-date FORMAT "99/99/9999":U
      EDIVTran.Terms-disc-amt FORMAT "->>>>>>>.99":U
      EDIVTran.Terms-desc[2] FORMAT "x(35)":U
      EDIVTran.Terms-desc[1] FORMAT "x(35)":U
      EDIVTran.Terms-day-of-month FORMAT "99":U
      EDIVTran.Terms-basis FORMAT "x(2)":U
      EDIVTran.Terms FORMAT "x(4)":U
      EDIVTran.St-code FORMAT "x(12)":U
      EDIVTran.Special-svc-code FORMAT "x(4)":U
      EDIVTran.Sn-code FORMAT "x(12)":U
      EDIVTran.Ship-stat FORMAT "x(2)":U
      EDIVTran.Ship-date-code FORMAT "x(3)":U
      EDIVTran.Ship-Date FORMAT "99/99/9999":U
      EDIVTran.Sf-code FORMAT "x(12)":U
      EDIVTran.Seq FORMAT ">>>>>>9":U
      EDIVTran.Routing[4] FORMAT "x(35)":U
      EDIVTran.Routing[3] FORMAT "x(35)":U
      EDIVTran.Routing[2] FORMAT "x(35)":U
      EDIVTran.Routing[1] FORMAT "x(35)":U
      EDIVTran.Release-no FORMAT "x(30)":U
      EDIVTran.Ref3-code FORMAT "x(2)":U
      EDIVTran.Ref3 FORMAT "x(30)":U
      EDIVTran.Ref2-code FORMAT "x(2)":U
      EDIVTran.Ref2 FORMAT "x(30)":U
      EDIVTran.rec_key FORMAT "X(20)":U
      EDIVTran.Re-code FORMAT "x(12)":U
      EDIVTran.Promo-code FORMAT "x(15)":U
      EDIVTran.Pro-Number FORMAT "x(30)":U
      EDIVTran.Misc-date1-code FORMAT "x(3)":U
      EDIVTran.Misc-date1 FORMAT "99/99/9999":U
      EDIVTran.Lines FORMAT ">>9":U
      EDIVTran.Last-line FORMAT ">>9":U
      EDIVTran.Invoice-date FORMAT "99/99/9999":U
      EDIVTran.FOB-Text FORMAT "x(60)":U
      EDIVTran.FOB-Qual FORMAT "x(2)":U
      EDIVTran.FOB-Code FORMAT "x(2)":U
      EDIVTran.Del-date-qual FORMAT "x(2)":U
      EDIVTran.Del-date FORMAT "99/99/9999":U
      EDIVTran.Cust-po-date FORMAT "99/99/9999":U
      EDIVTran.Cust-po FORMAT "x(22)":U
      EDIVTran.Cust-div FORMAT "x(15)":U
      EDIVTran.Cust-dept FORMAT "x(8)":U
      EDIVTran.Curr-seller FORMAT "x(3)":U
      EDIVTran.Curr-rate-seller FORMAT "->>>>>>9.99<":U
      EDIVTran.Curr-rate-buyer FORMAT "->>>>>>9.99<":U
      EDIVTran.Curr-buyer FORMAT "x(3)":U
      EDIVTran.Contract FORMAT "x(30)":U
      EDIVTran.Contact-phone-qual FORMAT "x(2)":U
      EDIVTran.Contact-phone FORMAT "x(21)":U
      EDIVTran.Contact-name FORMAT "x(35)":U
      EDIVTran.Contact-funct-code FORMAT "x(2)":U
      EDIVTran.Company FORMAT "x(8)":U
      EDIVTran.Carton-uom-code FORMAT "x(2)":U
      EDIVTran.Carrier-code FORMAT "x(6)":U
      EDIVTran.Carrier FORMAT "x(30)":U
      EDIVTran.By-code FORMAT "x(12)":U
      EDIVTran.Bt-code FORMAT "x(12)":U
      EDIVTran.BOL-No FORMAT "x(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 18.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 19.33 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 19.33 COL 2
     RECT-4 AT ROW 19.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.81
         WIDTH              = 146.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.EDIVTran"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   > asi.EDIVTran.Invoice-no
"Invoice-no" "Invoice No" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = asi.EDIVTran.Partner
     _FldNameList[3]   = asi.EDIVTran.Vendor
     _FldNameList[4]   = asi.EDIVTran.Cust
     _FldNameList[5]   > asi.EDIVTran.Tot-net
"Tot-net" "Total Net Amt" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.EDIVTran.Tot-Gross
"Tot-Gross" "Total Gross Amount" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.EDIVTran.Tot-cartons
"Tot-cartons" "Total cartons" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = asi.EDIVTran.Wght-uom
     _FldNameList[9]   = asi.EDIVTran.Volume-uom
     _FldNameList[10]   = asi.EDIVTran.Vn-code
     _FldNameList[11]   = asi.EDIVTran.Trailer-Number
     _FldNameList[12]   = asi.EDIVTran.Tot-wght
     _FldNameList[13]   = asi.EDIVTran.Tot-volume
     _FldNameList[14]   = asi.EDIVTran.Tot-qty
     _FldNameList[15]   = asi.EDIVTran.Tot-frt
     _FldNameList[16]   = asi.EDIVTran.Tot-disc
     _FldNameList[17]   = asi.EDIVTran.Terms-type
     _FldNameList[18]   = asi.EDIVTran.Terms-net-days
     _FldNameList[19]   = asi.EDIVTran.Terms-net-date
     _FldNameList[20]   = asi.EDIVTran.Terms-disc-pct
     _FldNameList[21]   = asi.EDIVTran.Terms-disc-days
     _FldNameList[22]   = asi.EDIVTran.Terms-disc-date
     _FldNameList[23]   = asi.EDIVTran.Terms-disc-amt
     _FldNameList[24]   = asi.EDIVTran.Terms-desc[2]
     _FldNameList[25]   = asi.EDIVTran.Terms-desc[1]
     _FldNameList[26]   = asi.EDIVTran.Terms-day-of-month
     _FldNameList[27]   = asi.EDIVTran.Terms-basis
     _FldNameList[28]   = asi.EDIVTran.Terms
     _FldNameList[29]   = asi.EDIVTran.St-code
     _FldNameList[30]   = asi.EDIVTran.Special-svc-code
     _FldNameList[31]   = asi.EDIVTran.Sn-code
     _FldNameList[32]   = asi.EDIVTran.Ship-stat
     _FldNameList[33]   = asi.EDIVTran.Ship-date-code
     _FldNameList[34]   = asi.EDIVTran.Ship-Date
     _FldNameList[35]   = asi.EDIVTran.Sf-code
     _FldNameList[36]   = asi.EDIVTran.Seq
     _FldNameList[37]   = asi.EDIVTran.Routing[4]
     _FldNameList[38]   = asi.EDIVTran.Routing[3]
     _FldNameList[39]   = asi.EDIVTran.Routing[2]
     _FldNameList[40]   = asi.EDIVTran.Routing[1]
     _FldNameList[41]   = asi.EDIVTran.Release-no
     _FldNameList[42]   = asi.EDIVTran.Ref3-code
     _FldNameList[43]   = asi.EDIVTran.Ref3
     _FldNameList[44]   = asi.EDIVTran.Ref2-code
     _FldNameList[45]   = asi.EDIVTran.Ref2
     _FldNameList[46]   = asi.EDIVTran.rec_key
     _FldNameList[47]   = asi.EDIVTran.Re-code
     _FldNameList[48]   = asi.EDIVTran.Promo-code
     _FldNameList[49]   = asi.EDIVTran.Pro-Number
     _FldNameList[50]   = asi.EDIVTran.Misc-date1-code
     _FldNameList[51]   = asi.EDIVTran.Misc-date1
     _FldNameList[52]   = asi.EDIVTran.Lines
     _FldNameList[53]   = asi.EDIVTran.Last-line
     _FldNameList[54]   = asi.EDIVTran.Invoice-date
     _FldNameList[55]   = asi.EDIVTran.FOB-Text
     _FldNameList[56]   = asi.EDIVTran.FOB-Qual
     _FldNameList[57]   = asi.EDIVTran.FOB-Code
     _FldNameList[58]   = asi.EDIVTran.Del-date-qual
     _FldNameList[59]   = asi.EDIVTran.Del-date
     _FldNameList[60]   = asi.EDIVTran.Cust-po-date
     _FldNameList[61]   = asi.EDIVTran.Cust-po
     _FldNameList[62]   = asi.EDIVTran.Cust-div
     _FldNameList[63]   = asi.EDIVTran.Cust-dept
     _FldNameList[64]   = asi.EDIVTran.Curr-seller
     _FldNameList[65]   = asi.EDIVTran.Curr-rate-seller
     _FldNameList[66]   = asi.EDIVTran.Curr-rate-buyer
     _FldNameList[67]   = asi.EDIVTran.Curr-buyer
     _FldNameList[68]   = asi.EDIVTran.Contract
     _FldNameList[69]   = asi.EDIVTran.Contact-phone-qual
     _FldNameList[70]   = asi.EDIVTran.Contact-phone
     _FldNameList[71]   = asi.EDIVTran.Contact-name
     _FldNameList[72]   = asi.EDIVTran.Contact-funct-code
     _FldNameList[73]   = asi.EDIVTran.Company
     _FldNameList[74]   = asi.EDIVTran.Carton-uom-code
     _FldNameList[75]   = asi.EDIVTran.Carrier-code
     _FldNameList[76]   = asi.EDIVTran.Carrier
     _FldNameList[77]   = asi.EDIVTran.By-code
     _FldNameList[78]   = asi.EDIVTran.Bt-code
     _FldNameList[79]   = asi.EDIVTran.BOL-No
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  {src/adm/template/snd-list.i "EDIVTran"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBrowseFocus B-table-Win 
PROCEDURE setBrowseFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

