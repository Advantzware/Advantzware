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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES EDIVTran
&Scoped-define FIRST-EXTERNAL-TABLE EDIVTran


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDIVTran.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES EDIVLine

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table EDIVLine.Line EDIVLine.Item-no ~
EDIVLine.Cust-item-no EDIVLine.UPC EDIVLine.Qty-shipped EDIVLine.Uom-code ~
EDIVLine.Selling-price EDIVLine.Unit-price EDIVLine.Taxable ~
EDIVLine.Special-svc-code EDIVLine.Size-desc EDIVLine.ship-stat ~
EDIVLine.SF-Code EDIVLine.Seq EDIVLine.Qty-var EDIVLine.Qty-ord-orig ~
EDIVLine.Product-type EDIVLine.Price-basis EDIVLine.Partner ~
EDIVLine.Pack-size EDIVLine.Item-wght-each EDIVLine.Item-net ~
EDIVLine.Item-gross EDIVLine.Item-each-cube EDIVLine.Item-disc-amount ~
EDIVLine.Item-ctn-wght EDIVLine.Item-ctn-cube EDIVLine.Invoice-no ~
EDIVLine.Dimension[3] EDIVLine.Dimension[2] EDIVLine.Dimension[1] ~
EDIVLine.Description[2] EDIVLine.Description[1] EDIVLine.Cust-po-line ~
EDIVLine.Config-code 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH EDIVLine OF EDIVTran WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH EDIVLine OF EDIVTran WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table EDIVLine
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table EDIVLine


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
     SIZE 154 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      EDIVLine
    FIELDS(EDIVLine.Line
      EDIVLine.Item-no
      EDIVLine.Cust-item-no
      EDIVLine.UPC
      EDIVLine.Qty-shipped
      EDIVLine.Uom-code
      EDIVLine.Selling-price
      EDIVLine.Unit-price
      EDIVLine.Taxable
      EDIVLine.Special-svc-code
      EDIVLine.Size-desc
      EDIVLine.ship-stat
      EDIVLine.SF-Code
      EDIVLine.Seq
      EDIVLine.Qty-var
      EDIVLine.Qty-ord-orig
      EDIVLine.Product-type
      EDIVLine.Price-basis
      EDIVLine.Partner
      EDIVLine.Pack-size
      EDIVLine.Item-wght-each
      EDIVLine.Item-net
      EDIVLine.Item-gross
      EDIVLine.Item-each-cube
      EDIVLine.Item-disc-amount
      EDIVLine.Item-ctn-wght
      EDIVLine.Item-ctn-cube
      EDIVLine.Invoice-no
      EDIVLine.Dimension[3]
      EDIVLine.Dimension[2]
      EDIVLine.Dimension[1]
      EDIVLine.Description[2]
      EDIVLine.Description[1]
      EDIVLine.Cust-po-line
      EDIVLine.Config-code) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      EDIVLine.Line FORMAT ">>9":U
      EDIVLine.Item-no COLUMN-LABEL "Item Number" FORMAT "x(16)":U
      EDIVLine.Cust-item-no COLUMN-LABEL "Cust Item No" FORMAT "x(30)":U
      EDIVLine.UPC FORMAT "x(13)":U
      EDIVLine.Qty-shipped COLUMN-LABEL "Qty Shipped" FORMAT "->>>,>>>,>>>.99":U
      EDIVLine.Uom-code COLUMN-LABEL "Uom Code" FORMAT "x(2)":U
      EDIVLine.Selling-price COLUMN-LABEL "Selling Price" FORMAT "->>>,>>>,>>>.99":U
      EDIVLine.Unit-price COLUMN-LABEL "Unit Price" FORMAT "->>,>>>,>>9.99":U
      EDIVLine.Taxable FORMAT "yes/no":U
      EDIVLine.Special-svc-code FORMAT "x(4)":U
      EDIVLine.Size-desc COLUMN-LABEL "Size Desc" FORMAT "x(20)":U
      EDIVLine.ship-stat COLUMN-LABEL "ship stat" FORMAT "x(2)":U
      EDIVLine.SF-Code COLUMN-LABEL "SF Code" FORMAT "x(12)":U
      EDIVLine.Seq FORMAT ">>>>>>9":U
      EDIVLine.Qty-var COLUMN-LABEL "Qty Variance" FORMAT "->,>>>,>>>.99":U
      EDIVLine.Qty-ord-orig FORMAT "->,>>>,>>>.99":U
      EDIVLine.Product-type FORMAT "x(3)":U
      EDIVLine.Price-basis FORMAT "x(2)":U
      EDIVLine.Partner FORMAT "x(05)":U
      EDIVLine.Pack-size FORMAT ">>>,>>9":U
      EDIVLine.Item-wght-each FORMAT "->,>>>,>>>.999":U
      EDIVLine.Item-net FORMAT "->,>>>,>>>.99":U
      EDIVLine.Item-gross FORMAT "->,>>>,>>>.99":U
      EDIVLine.Item-each-cube FORMAT ">,>>>,>>>.99":U
      EDIVLine.Item-disc-amount FORMAT "->,>>>,>>>.99":U
      EDIVLine.Item-ctn-wght FORMAT "->,>>>,>>>.999":U
      EDIVLine.Item-ctn-cube FORMAT ">,>>>,>>>.99":U
      EDIVLine.Invoice-no FORMAT "x(22)":U
      EDIVLine.Dimension[3] FORMAT "x(10)":U
      EDIVLine.Dimension[2] FORMAT "x(10)":U
      EDIVLine.Dimension[1] FORMAT "x(10)":U
      EDIVLine.Description[2] FORMAT "x(30)":U
      EDIVLine.Description[1] FORMAT "x(30)":U
      EDIVLine.Cust-po-line FORMAT "x(10)":U
      EDIVLine.Config-code FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 154 BY 25.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 26.48 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 26.48 COL 79 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 26.48 COL 141 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 26.48 COL 2
     RECT-4 AT ROW 26.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: asi.EDIVTran
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
         HEIGHT             = 26.67
         WIDTH              = 154.
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
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.EDIVLine OF asi.EDIVTran"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _FldNameList[1]   = asi.EDIVLine.Line
     _FldNameList[2]   > asi.EDIVLine.Item-no
"EDIVLine.Item-no" "Item Number" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.EDIVLine.Cust-item-no
"EDIVLine.Cust-item-no" "Cust Item No" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = asi.EDIVLine.UPC
     _FldNameList[5]   > asi.EDIVLine.Qty-shipped
"EDIVLine.Qty-shipped" "Qty Shipped" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.EDIVLine.Uom-code
"EDIVLine.Uom-code" "Uom Code" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.EDIVLine.Selling-price
"EDIVLine.Selling-price" "Selling Price" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.EDIVLine.Unit-price
"EDIVLine.Unit-price" "Unit Price" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = asi.EDIVLine.Taxable
     _FldNameList[10]   = asi.EDIVLine.Special-svc-code
     _FldNameList[11]   > asi.EDIVLine.Size-desc
"EDIVLine.Size-desc" "Size Desc" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.EDIVLine.ship-stat
"EDIVLine.ship-stat" "ship stat" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.EDIVLine.SF-Code
"EDIVLine.SF-Code" "SF Code" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = asi.EDIVLine.Seq
     _FldNameList[15]   > asi.EDIVLine.Qty-var
"EDIVLine.Qty-var" "Qty Variance" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   = asi.EDIVLine.Qty-ord-orig
     _FldNameList[17]   = asi.EDIVLine.Product-type
     _FldNameList[18]   = asi.EDIVLine.Price-basis
     _FldNameList[19]   = asi.EDIVLine.Partner
     _FldNameList[20]   = asi.EDIVLine.Pack-size
     _FldNameList[21]   = asi.EDIVLine.Item-wght-each
     _FldNameList[22]   = asi.EDIVLine.Item-net
     _FldNameList[23]   = asi.EDIVLine.Item-gross
     _FldNameList[24]   = asi.EDIVLine.Item-each-cube
     _FldNameList[25]   = asi.EDIVLine.Item-disc-amount
     _FldNameList[26]   = asi.EDIVLine.Item-ctn-wght
     _FldNameList[27]   = asi.EDIVLine.Item-ctn-cube
     _FldNameList[28]   = asi.EDIVLine.Invoice-no
     _FldNameList[29]   = asi.EDIVLine.Dimension[3]
     _FldNameList[30]   = asi.EDIVLine.Dimension[2]
     _FldNameList[31]   = asi.EDIVLine.Dimension[1]
     _FldNameList[32]   = asi.EDIVLine.Description[2]
     _FldNameList[33]   = asi.EDIVLine.Description[1]
     _FldNameList[34]   = asi.EDIVLine.Cust-po-line
     _FldNameList[35]   = asi.EDIVLine.Config-code
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
  {src/adm/template/snd-list.i "EDIVLine"}

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

