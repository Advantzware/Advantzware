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
&Scoped-define EXTERNAL-TABLES EDCat
&Scoped-define FIRST-EXTERNAL-TABLE EDCat


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDCat.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES EDCatline

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table EDCatline.Wght-uom ~
EDCatline.Volume-uom EDCatline.Vendor-Item EDCatline.UPC EDCatline.Uom-code ~
EDCatline.Size-ID EDCatline.Seq EDCatline.rec_key EDCatline.Qty-Min ~
EDCatline.Qty-Max EDCatline.Qty EDCatline.Price EDCatline.PID-Code ~
EDCatline.Partner EDCatline.Pack-wght EDCatline.Pack-Volume ~
EDCatline.Pack-size EDCatline.Lines EDCatline.Line EDCatline.Last-line ~
EDCatline.Inner-units EDCatline.Dimensions[3] EDCatline.Dimensions[2] ~
EDCatline.Dimensions[1] EDCatline.Dim-uom EDCatline.Description[2] ~
EDCatline.Description[1] EDCatline.Config-code EDCatline.Commodity-qual ~
EDCatline.Commodity-Grouping EDCatline.Commodity-code EDCatline.Color-ID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH EDCatline OF EDCat WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH EDCatline OF EDCat WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table EDCatline
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table EDCatline


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
      EDCatline
    FIELDS(EDCatline.Wght-uom
      EDCatline.Volume-uom
      EDCatline.Vendor-Item
      EDCatline.UPC
      EDCatline.Uom-code
      EDCatline.Size-ID
      EDCatline.Seq
      EDCatline.rec_key
      EDCatline.Qty-Min
      EDCatline.Qty-Max
      EDCatline.Qty
      EDCatline.Price
      EDCatline.PID-Code
      EDCatline.Partner
      EDCatline.Pack-wght
      EDCatline.Pack-Volume
      EDCatline.Pack-size
      EDCatline.Lines
      EDCatline.Line
      EDCatline.Last-line
      EDCatline.Inner-units
      EDCatline.Dimensions[3]
      EDCatline.Dimensions[2]
      EDCatline.Dimensions[1]
      EDCatline.Dim-uom
      EDCatline.Description[2]
      EDCatline.Description[1]
      EDCatline.Config-code
      EDCatline.Commodity-qual
      EDCatline.Commodity-Grouping
      EDCatline.Commodity-code
      EDCatline.Color-ID) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      EDCatline.Wght-uom FORMAT "x(2)":U
      EDCatline.Volume-uom FORMAT "x(2)":U
      EDCatline.Vendor-Item FORMAT "x(30)":U
      EDCatline.UPC FORMAT "x(13)":U
      EDCatline.Uom-code FORMAT "x(2)":U
      EDCatline.Size-ID FORMAT "x(05)":U
      EDCatline.Seq FORMAT ">>>>>>9":U
      EDCatline.rec_key FORMAT "X(20)":U
      EDCatline.Qty-Min FORMAT "->,>>>,>>9":U
      EDCatline.Qty-Max FORMAT "->,>>>,>>9":U
      EDCatline.Qty FORMAT "->,>>>,>>>.99":U
      EDCatline.Price FORMAT "->>,>>9.99":U
      EDCatline.PID-Code FORMAT "x(2)":U
      EDCatline.Partner FORMAT "x(05)":U
      EDCatline.Pack-wght FORMAT "->>,>>9.99":U
      EDCatline.Pack-Volume FORMAT "->>,>>9.99":U
      EDCatline.Pack-size COLUMN-LABEL "Pack Size" FORMAT ">>>,>>9":U
      EDCatline.Lines FORMAT ">>9":U
      EDCatline.Line FORMAT ">>>>>9":U
      EDCatline.Last-line COLUMN-LABEL "Last Line" FORMAT ">>9":U
      EDCatline.Inner-units COLUMN-LABEL "Inner Units" FORMAT "->,>>>,>>9":U
      EDCatline.Dimensions[3] FORMAT "->>,>>9.99":U
      EDCatline.Dimensions[2] FORMAT "->>,>>9.99":U
      EDCatline.Dimensions[1] FORMAT "->>,>>9.99":U
      EDCatline.Dim-uom COLUMN-LABEL "Dim Uom" FORMAT "x(2)":U
      EDCatline.Description[2] FORMAT "x(40)":U
      EDCatline.Description[1] FORMAT "x(40)":U
      EDCatline.Config-code COLUMN-LABEL "Config Code" FORMAT "x(1)":U
      EDCatline.Commodity-qual COLUMN-LABEL "Commodity Qual" FORMAT "x(2)":U
      EDCatline.Commodity-Grouping COLUMN-LABEL "Commodity Grouping" FORMAT "x(30)":U
      EDCatline.Commodity-code COLUMN-LABEL "Commodity Code" FORMAT "x(16)":U
      EDCatline.Color-ID FORMAT "999":U
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
   External Tables: asi.EDCat
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
         HEIGHT             = 19.52
         WIDTH              = 145.
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
     _TblList          = "asi.EDCatline OF asi.EDCat"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _FldNameList[1]   = asi.EDCatline.Wght-uom
     _FldNameList[2]   = asi.EDCatline.Volume-uom
     _FldNameList[3]   = asi.EDCatline.Vendor-Item
     _FldNameList[4]   = asi.EDCatline.UPC
     _FldNameList[5]   = asi.EDCatline.Uom-code
     _FldNameList[6]   = asi.EDCatline.Size-ID
     _FldNameList[7]   = asi.EDCatline.Seq
     _FldNameList[8]   = asi.EDCatline.rec_key
     _FldNameList[9]   = asi.EDCatline.Qty-Min
     _FldNameList[10]   = asi.EDCatline.Qty-Max
     _FldNameList[11]   = asi.EDCatline.Qty
     _FldNameList[12]   = asi.EDCatline.Price
     _FldNameList[13]   = asi.EDCatline.PID-Code
     _FldNameList[14]   = asi.EDCatline.Partner
     _FldNameList[15]   = asi.EDCatline.Pack-wght
     _FldNameList[16]   = asi.EDCatline.Pack-Volume
     _FldNameList[17]   > asi.EDCatline.Pack-size
"EDCatline.Pack-size" "Pack Size" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   = asi.EDCatline.Lines
     _FldNameList[19]   = asi.EDCatline.Line
     _FldNameList[20]   > asi.EDCatline.Last-line
"EDCatline.Last-line" "Last Line" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > asi.EDCatline.Inner-units
"EDCatline.Inner-units" "Inner Units" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   = asi.EDCatline.Dimensions[3]
     _FldNameList[23]   = asi.EDCatline.Dimensions[2]
     _FldNameList[24]   = asi.EDCatline.Dimensions[1]
     _FldNameList[25]   > asi.EDCatline.Dim-uom
"EDCatline.Dim-uom" "Dim Uom" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   = asi.EDCatline.Description[2]
     _FldNameList[27]   = asi.EDCatline.Description[1]
     _FldNameList[28]   > asi.EDCatline.Config-code
"EDCatline.Config-code" "Config Code" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > asi.EDCatline.Commodity-qual
"EDCatline.Commodity-qual" "Commodity Qual" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > asi.EDCatline.Commodity-Grouping
"EDCatline.Commodity-Grouping" "Commodity Grouping" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > asi.EDCatline.Commodity-code
"EDCatline.Commodity-code" "Commodity Code" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   = asi.EDCatline.Color-ID
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
  {src/adm/template/row-list.i "EDCat"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDCat"}

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
  {src/adm/template/snd-list.i "EDCat"}
  {src/adm/template/snd-list.i "EDCatline"}

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

