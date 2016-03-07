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
def var ll-enable-fields as log no-undo.  /* bug with set-focus */
DEF VAR ll-new-file AS LOG NO-UNDO.
DEF VAR lv-part-no LIKE quote-vendor-item.part-no NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}
cocode = g_company.

DEF TEMP-TABLE tt-quote-vendor-item FIELD row-id AS ROWID INDEX row-id row-id.

ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

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
&Scoped-define EXTERNAL-TABLES quote-vendor
&Scoped-define FIRST-EXTERNAL-TABLE quote-vendor


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR quote-vendor.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES quote-vendor-item

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table quote-vendor-item.part-no quote-vendor-item.i-no ~
quote-vendor-item.part-dscr1 quote-vendor-item.part-dscr2 quote-vendor-item.style ~
display-qty() @ quote-vendor-item.qty quote-vendor-item.qty quote-vendor-item.price ~
display-price() @ quote-vendor-item.price quote-vendor-item.uom quote-vendor-item.size ~
display-uom() @ quote-vendor-item.uom quote-vendor-item.i-dscr quote-vendor-item.i-coldscr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table quote-vendor-item.part-no ~
quote-vendor-item.i-no quote-vendor-item.part-dscr1 quote-vendor-item.part-dscr2 quote-vendor-item.style ~
quote-vendor-item.qty quote-vendor-item.price quote-vendor-item.uom quote-vendor-item.size quote-vendor-item.i-dscr ~
quote-vendor-item.i-coldscr 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table quote-vendor-item
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table quote-vendor-item
&Scoped-define QUERY-STRING-Browser-Table FOR EACH quote-vendor-item OF quote-vendor WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH quote-vendor-item OF quote-vendor WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table quote-vendor-item
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table quote-vendor-item


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-price B-table-Win 
FUNCTION display-price RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-qty B-table-Win 
FUNCTION display-qty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-uom B-table-Win 
FUNCTION display-uom RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 130 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      quote-vendor-item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      quote-vendor-item.part-no COLUMN-LABEL "Cust Part#" FORMAT "x(20)":U
      quote-vendor-item.i-no FORMAT "x(15)":U WIDTH 22
      quote-vendor-item.part-dscr1 FORMAT "x(30)":U
      quote-vendor-item.part-dscr2 COLUMN-LABEL "Item Description 2" FORMAT "x(30)":U
      quote-vendor-item.style COLUMN-LABEL "Style" FORMAT "x(4)":U
      display-qty() @ quote-vendor-item.qty
      quote-vendor-item.qty FORMAT ">>>,>>>,>>9":U
      quote-vendor-item.price FORMAT ">>>,>>9.9999":U WIDTH 15.2
      display-price() @ quote-vendor-item.price
      quote-vendor-item.uom FORMAT "x(3)":U
      quote-vendor-item.size FORMAT "x(30)":U
      display-uom() @ quote-vendor-item.uom
      quote-vendor-item.i-dscr COLUMN-LABEL "Board" FORMAT "x(30)":U
      quote-vendor-item.i-coldscr COLUMN-LABEL "Color" FORMAT "x(20)":U
  ENABLE
      quote-vendor-item.part-no
      quote-vendor-item.i-no
      quote-vendor-item.part-dscr1
      quote-vendor-item.part-dscr2
      quote-vendor-item.style
      quote-vendor-item.qty
      quote-vendor-item.price
      quote-vendor-item.uom
      quote-vendor-item.size
      quote-vendor-item.i-dscr
      quote-vendor-item.i-coldscr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 130 BY 6.43
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 7.67 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 7.67 COL 76 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 7.67 COL 117 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 7.67 COL 2
     RECT-4 AT ROW 7.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.quote-vendor
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
         HEIGHT             = 7.86
         WIDTH              = 130.
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
     _TblList          = "ASI.quote-vendor-item OF ASI.quote-vendor"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > ASI.quote-vendor-item.part-no
"quote-vendor-item.part-no" "Cust Part#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.quote-vendor-item.i-no
"quote-vendor-item.i-no" ? ? "character" ? ? ? ? ? ? yes ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.quote-vendor-item.part-dscr1
"quote-vendor-item.part-dscr1" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.quote-vendor-item.part-dscr2
"quote-vendor-item.part-dscr2" "Item Description 2" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.quote-vendor-item.style
"quote-vendor-item.style" "Style" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"display-qty() @ quote-vendor-item.qty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.quote-vendor-item.qty
"quote-vendor-item.qty" ? ">>>,>>>,>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.quote-vendor-item.price
"quote-vendor-item.price" ? ">>>,>>9.9999" "decimal" ? ? ? ? ? ? yes ? no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"display-price() @ quote-vendor-item.price" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.quote-vendor-item.uom
"quote-vendor-item.uom" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.quote-vendor-item.size
"quote-vendor-item.size" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"display-uom() @ quote-vendor-item.uom" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.quote-vendor-item.i-dscr
"quote-vendor-item.i-dscr" "Board" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.quote-vendor-item.i-coldscr
"quote-vendor-item.i-coldscr" "Color" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
   def var phandle as widget-handle no-undo.
   def var char-hdl as cha no-undo.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state in phandle ('update-begin':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* Tool kit set-focus problem */
  IF NOT ll-enable-fields THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR row-val AS ROWID NO-UNDO.

  CASE FOCUS:NAME :
    WHEN "part-no" THEN DO:
      IF ll-new-file THEN DO:
        RUN windows/l-cpart.w (g_company,quote-vendor.cust-no,FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT row-val).
        FIND itemfg WHERE ROWID(itemfg) EQ row-val NO-LOCK NO-ERROR.
        IF AVAIL itemfg AND ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE THEN DO:
          FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
          RUN new-part-no.
        END.
      END.

      ELSE DO:
        RUN windows/l-fgpart.w (g_company,quote-vendor.cust-no,FOCUS:SCREEN-VALUE,OUTPUT row-val).
        FIND itemfg WHERE ROWID(itemfg) EQ row-val NO-LOCK NO-ERROR.
        IF AVAIL itemfg AND itemfg.part-no NE FOCUS:SCREEN-VALUE THEN DO:
          FOCUS:SCREEN-VALUE = itemfg.part-no.
          RUN new-part-no.
        END.
      END.
    END.
  END CASE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  /* {src/adm/template/brsleave.i} */
     {brsleave.i}
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  
  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN 
  BROWSE {&browse-name}:FETCH-SELECTED-ROW(1).
      
   {src/adm/template/brschnge.i}
   /*{methods/template/local/setvalue.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quote-vendor-item.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF quote-vendor-item.part-no IN BROWSE Browser-Table /* Cust Part# */
DO:
  IF NOT adm-new-record THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF quote-vendor-item.part-no IN BROWSE Browser-Table /* Cust Part# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-part-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF quote-vendor-item.part-no IN BROWSE Browser-Table /* Cust Part# */
DO:
  RUN new-part-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quote-vendor-item.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF quote-vendor-item.i-no IN BROWSE Browser-Table /* Item No */
DO:
  APPLY 'TAB':U TO {&self-name} IN BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quote-vendor-item.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.style Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF quote-vendor-item.style IN BROWSE Browser-Table /* Style */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quote-vendor-item.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF quote-vendor-item.qty IN BROWSE Browser-Table /* Qty */
DO:
  IF NOT adm-new-record THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF quote-vendor-item.qty IN BROWSE Browser-Table /* Qty */
DO:
  /*{est/quoprice.i "quote-vendor-item" ":SCREEN-VALUE IN BROWSE {&browse-name}"}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quote-vendor-item.price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.price Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF quote-vendor-item.price IN BROWSE Browser-Table /* Price */
DO:
   IF NOT adm-new-record THEN do:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quote-vendor-item.uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.uom Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF quote-vendor-item.uom IN BROWSE Browser-Table /* UOM */
DO:
   IF NOT adm-new-record THEN do:
       APPLY "leave" TO SELF.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF quote-vendor-item.uom IN BROWSE Browser-Table /* UOM */
DO:
  IF NOT adm-new-record THEN do:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quote-vendor-item.size
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quote-vendor-item.size Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF quote-vendor-item.size IN BROWSE Browser-Table /* Dimensions */
DO:
   /*IF NOT adm-new-record THEN do:
       APPLY "leave" TO SELF.
       RETURN NO-APPLY.
   END.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
ASSIGN
 cocode = g_company
 locode = g_loc.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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
  {src/adm/template/row-list.i "quote-vendor"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "quote-vendor"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF adm-new-record THEN DO:
    FIND FIRST quote-vendor-qty OF quote-vendor-item NO-LOCK NO-ERROR.
    IF NOT AVAIL quote-vendor-qty THEN DO:
      CREATE quote-vendor-qty.
      ASSIGN
       quote-vendor-qty.company    = quote-vendor-item.company
       quote-vendor-qty.loc        = quote-vendor-item.loc
       quote-vendor-qty.q-no       = quote-vendor-item.q-no
       quote-vendor-qty.line       = quote-vendor-item.line
       quote-vendor-qty.qty        = quote-vendor-item.qty
       quote-vendor-qty.price      = quote-vendor-item.price
       quote-vendor-qty.uom        = quote-vendor-item.uom
       quote-vendor-qty.quote-date = TODAY
       quote-vendor-qty.quote-user = USERID("nosweat").

      RELEASE itemfg.

      IF ll-new-file THEN DO:
        ASSIGN
         lv-part-no = quote-vendor-item.part-no:SCREEN-VALUE IN BROWSE {&browse-name}
         lv-rowid   = ?.
        RUN custom/getcpart.p (cocode, quote-vendor.cust-no,
                               INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
        FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.
      END.

      IF NOT AVAIL itemfg THEN
      FIND FIRST itemfg
          WHERE itemfg.company  EQ quote-vendor-item.company
            AND itemfg.part-no  EQ quote-vendor-item.part-no
            AND itemfg.part-no  NE ""
            AND (itemfg.cust-no EQ quote-vendor.cust-no OR
                 itemfg.i-code  EQ "S")
          NO-LOCK NO-ERROR.

      IF AVAIL itemfg THEN DO:
        RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                               itemfg.std-mat-cost,
                               OUTPUT quote-vendor-qty.mat-cost).

        RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                               itemfg.std-lab-cost,
                               OUTPUT quote-vendor-qty.lab-cost).

        RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                               itemfg.std-fix-cost,
                               OUTPUT quote-vendor-qty.fo-cost).

        RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                               itemfg.std-var-cost,
                               OUTPUT quote-vendor-qty.vo-cost).
      END.
    END.
  END.

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-next-line as int no-undo.

  DEF BUFFER bquote-vendor-item FOR quote-vendor-item.
  
  find last bquote-vendor-item use-index q-line where bquote-vendor-item.company = quote-vendor.company
                                         and bquote-vendor-item.loc = quote-vendor.loc
                                         and bquote-vendor-item.q-no = quote-vendor.q-no
                 no-lock no-error.
  li-next-line = if avail bquote-vendor-item then bquote-vendor-item.line + 1 else 1.
                 
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign quote-vendor-item.company = quote-vendor.company
         quote-vendor-item.loc =  quote-vendor.loc
         quote-vendor-item.q-no = quote-vendor.q-no
         quote-vendor-item.line = li-next-line
         quote-vendor-item.upd-date = TODAY
         quote-vendor-item.upd-user = USERID("nosweat").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER b-{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} FOR {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.


  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/askdel.i}

  FOR EACH tt-{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}:
    DELETE tt-{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
      {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.
      CREATE tt-{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.
      tt-{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.row-id = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
    END.
    {&browse-name}:DESELECT-ROWS ().
  END.

  FOR EACH tt-{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}:
    REPOSITION {&browse-name} TO ROWID tt-{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.row-id NO-ERROR.
    IF AVAIL {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
      {&browse-name}:SELECT-FOCUSED-ROW ().
      /* Dispatch standard ADM method.                             */
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
    END.
  END.

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN
    {&browse-name}:SELECT-FOCUSED-ROW ().
  ELSE DO:
    {methods/run_link.i "RECORD-SOURCE" "resetQuery"}
  END.

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
  ll-enable-fields = YES.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO quote-vendor-item.part-no IN BROWSE {&browse-name}.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "value-changed" TO browse-order IN FRAME {&FRAME-NAME}.
  RUN dispatch ("get-first").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-uom NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-enable-fields = no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-part-no B-table-Win 
PROCEDURE new-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-i-qty LIKE quote-vendor-item.qty NO-UNDO.
  DEF var class-qty AS INT EXTENT 13 NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RELEASE itemfg.

    IF ll-new-file THEN DO:
      ASSIGN
       lv-part-no = quote-vendor-item.part-no:SCREEN-VALUE IN BROWSE {&browse-name}
       lv-rowid   = ?.
      RUN custom/getcpart.p (cocode, quote-vendor.cust-no,
                             INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
      FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.
    END.

    IF NOT AVAIL itemfg THEN
    FIND FIRST itemfg
        WHERE itemfg.company  EQ cocode
          AND itemfg.part-no  EQ quote-vendor-item.part-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND itemfg.part-no  NE ""
          AND (itemfg.cust-no EQ quote-vendor.cust-no OR
               itemfg.i-code  EQ "S")
        NO-LOCK NO-ERROR.
    
    IF AVAIL itemfg THEN DO:
      ASSIGN
       quote-vendor-item.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name
       quote-vendor-item.style:SCREEN-VALUE IN BROWSE {&browse-name}      = itemfg.style
       quote-vendor-item.price:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(itemfg.sell-price)
       quote-vendor-item.uom:SCREEN-VALUE IN BROWSE {&browse-name}        = itemfg.sell-uom
       quote-vendor-item.size:SCREEN-VALUE IN BROWSE {&browse-name}       = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])
       /*RCO400*/
       quote-vendor-item.i-no:SCREEN-VALUE = itemfg.i-no   .

    END. /* avail itemfg */
  END. /* with frame {&frame-name} */

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
  {src/adm/template/snd-list.i "quote-vendor"}
  {src/adm/template/snd-list.i "quote-vendor-item"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no B-table-Win 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF ll-new-file THEN DO:
      ASSIGN
       lv-part-no = quote-vendor-item.part-no:SCREEN-VALUE IN BROWSE {&browse-name}
       lv-rowid   = ?.
      RUN custom/getcpart.p (cocode, quote-vendor.cust-no,
                             INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
    END.

    IF NOT CAN-FIND(itemfg WHERE ROWID(itemfg) EQ lv-rowid) AND
       NOT CAN-FIND(FIRST itemfg
                    WHERE itemfg.company  EQ cocode
                      AND itemfg.part-no  EQ quote-vendor-item.part-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND itemfg.part-no  NE ""
                      AND (itemfg.cust-no EQ quote-vendor.cust-no OR
                           itemfg.i-code  EQ "S"))
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO quote-vendor-item.part-no IN BROWSE {&browse-name}. 
      RETURN ERROR.
    END.
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom B-table-Win 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-uom LIKE uom.uom NO-UNDO.
  DEF VAR uom-list AS CHAR INIT "M,C,EA,CS" NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN sys/ref/uom-fg.p (NO ,OUTPUT uom-list).

    ASSIGN
     quote-vendor-item.uom:SCREEN-VALUE IN BROWSE {&browse-name} =
         CAPS(quote-vendor-item.uom:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-uom = quote-vendor-item.uom:SCREEN-VALUE IN BROWSE {&browse-name}.

    IF LOOKUP(lv-uom,uom-list) LE 0 THEN DO:
      MESSAGE "UOM must be " + TRIM(uom-list) VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO quote-vendor-item.uom IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-price B-table-Win 
FUNCTION display-price RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL quote-vendor-item THEN
     FIND FIRST quote-vendor-qty WHERE ASI.quote-vendor-qty.company = ASI.quote-vendor-item.company
          AND ASI.quote-vendor-qty.loc = ASI.quote-vendor-item.loc
          AND ASI.quote-vendor-qty.q-no = ASI.quote-vendor-item.q-no
          AND ASI.quote-vendor-qty.line = ASI.quote-vendor-item.line  USE-INDEX qt-qty NO-LOCK NO-ERROR.
  

  IF AVAIL quote-vendor-qty THEN RETURN quote-vendor-qty.price.
  ELSE IF AVAIL quote-vendor-item THEN RETURN quote-vendor-item.price.
  ELSE RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-qty B-table-Win 
FUNCTION display-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL quote-vendor-item THEN
     FIND FIRST quote-vendor-qty WHERE ASI.quote-vendor-qty.company = ASI.quote-vendor-item.company
          AND ASI.quote-vendor-qty.loc = ASI.quote-vendor-item.loc
          AND ASI.quote-vendor-qty.q-no = ASI.quote-vendor-item.q-no
          AND ASI.quote-vendor-qty.line = ASI.quote-vendor-item.line  USE-INDEX qt-qty NO-LOCK NO-ERROR.
  IF AVAIL quote-vendor-qty THEN RETURN int(quote-vendor-qty.qty).
  ELSE IF AVAIL quote-vendor-item THEN RETURN int(quote-vendor-item.qty).
  ELSE RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-uom B-table-Win 
FUNCTION display-uom RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL quote-vendor-item THEN
     FIND FIRST quote-vendor-qty WHERE ASI.quote-vendor-qty.company = ASI.quote-vendor-item.company
          AND ASI.quote-vendor-qty.loc = ASI.quote-vendor-item.loc
          AND ASI.quote-vendor-qty.q-no = ASI.quote-vendor-item.q-no
          AND ASI.quote-vendor-qty.line = ASI.quote-vendor-item.line  USE-INDEX qt-qty NO-LOCK NO-ERROR.
  

  IF AVAIL quote-vendor-qty THEN RETURN quote-vendor-qty.uom.
  ELSE IF AVAIL quote-vendor-item THEN RETURN quote-vendor-item.uom.
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

