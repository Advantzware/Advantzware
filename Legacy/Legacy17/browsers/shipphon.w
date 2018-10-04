&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/emailcst.w

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

&SCOPED-DEFINE dataGridInclude dataGrid\browsers\shipphon.i
&SCOPED-DEFINE BRWSDEFS   emailcst
&SCOPED-DEFINE emailTable shipto
&SCOPED-DEFINE browse2    methods/browsers/phoneBrowse2.i
&SCOPED-DEFINE IAM        sphone
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}

ASSIGN
  cocode = g_company
  locode = g_loc.

DEFINE VARIABLE emailCode AS LOGICAL NO-UNDO.

&IF defined (AppBuilder_is_Running) = 0
  &THEN {methods\defines\phone.i}
  &ELSE {methods\defines\phone.i &NEW="NEW"}
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emailcod
&Scoped-define FIRST-EXTERNAL-TABLE emailcod


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emailcod.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES phone shipto

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table shipto.cust-no shipto.ship-id ~
shipto.ship-name phone.attention phone.titlcode phone.rec_key ~
shipto.rec_key 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH phone WHERE NOT phone.table_rec_key = '' NO-LOCK, ~
      EACH shipto WHERE shipto.rec_key  = phone.table_rec_key ~
      AND shipto.rec_key  = phone.table_rec_key NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH phone WHERE NOT phone.table_rec_key = '' NO-LOCK, ~
      EACH shipto WHERE shipto.rec_key  = phone.table_rec_key ~
      AND shipto.rec_key  = phone.table_rec_key NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table phone shipto
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table phone
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table shipto


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 btnAdd btnSelectAll ~
btnClearAll browse-order auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD emailCode B-table-Win 
FUNCTION emailCode RETURNS LOGICAL
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetCurrShipTo B-table-Win 
FUNCTION GetCurrShipTo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     LABEL "&Add" 
     SIZE 16 BY 1.

DEFINE BUTTON btnClearAll 
     LABEL "Clea&r All" 
     SIZE 16 BY 1.

DEFINE BUTTON btnSelectAll 
     LABEL "&Select All" 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 59 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 121 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      phone
    FIELDS(phone.attention
      phone.titlcode
      phone.rec_key), 
      shipto
    FIELDS(shipto.cust-no
      shipto.ship-id
      shipto.ship-name
      shipto.rec_key) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      shipto.cust-no FORMAT "x(8)":U WIDTH 14 LABEL-FGCOLOR 15 LABEL-BGCOLOR 4
      shipto.ship-id FORMAT "x(8)":U WIDTH 14 LABEL-FGCOLOR 15 LABEL-BGCOLOR 4
      shipto.ship-name FORMAT "x(30)":U WIDTH 38 LABEL-FGCOLOR 15 LABEL-BGCOLOR 4
      phone.attention FORMAT "X(35)":U WIDTH 38 LABEL-FGCOLOR 15 LABEL-BGCOLOR 4
      phone.titlcode COLUMN-LABEL "Title" FORMAT "X(8)":U WIDTH 10
            LABEL-FGCOLOR 15 LABEL-BGCOLOR 4
      phone.rec_key FORMAT "X(20)":U
      shipto.rec_key FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 121 BY 7
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     btnAdd AT ROW 1.95 COL 123
     btnSelectAll AT ROW 3.14 COL 123
     btnClearAll AT ROW 4.33 COL 123
     browse-order AT ROW 8.38 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 8.38 COL 74 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 8.38 COL 108 HELP
          "CLEAR AUTO FIND Value"
     " Available" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 1 COL 123
          BGCOLOR 4 FGCOLOR 15 
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 8.38 COL 2
     RECT-4 AT ROW 8.14 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: asi.emailcod
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
         HEIGHT             = 8.57
         WIDTH              = 138.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table TEXT-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:MAX-DATA-GUESS IN FRAME F-Main         = 50
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "NOSWEAT.phone WHERE asi.emailcod <external> ... ...,asi.shipto WHERE NOSWEAT.phone ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED, USED"
     _JoinCode[1]      = "NOT NOSWEAT.phone.table_rec_key = ''"
     _JoinCode[2]      = "asi.shipto.rec_key  = NOSWEAT.phone.table_rec_key"
     _Where[2]         = "asi.shipto.rec_key  = NOSWEAT.phone.table_rec_key"
     _FldNameList[1]   > asi.shipto.cust-no
"shipto.cust-no" ? ? "character" ? ? ? 4 15 ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.shipto.ship-id
"shipto.ship-id" ? ? "character" ? ? ? 4 15 ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.shipto.ship-name
"shipto.ship-name" ? ? "character" ? ? ? 4 15 ? no ? no no "38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > NOSWEAT.phone.attention
"phone.attention" ? ? "character" ? ? ? 4 15 ? no ? no no "38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > NOSWEAT.phone.titlcode
"phone.titlcode" "Title" ? "character" ? ? ? 4 15 ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = NOSWEAT.phone.rec_key
     _FldNameList[7]   = asi.shipto.rec_key
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
  APPLY 'CHOOSE':U TO btnAdd.
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
/*   {methods/run_link.i "CONTAINER-SOURCE" "setPhoneValues" "('ShipTo',ROWID(shipto))"} */

  assign s-rec_key = shipto.rec_key when avail shipto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd B-table-Win
ON CHOOSE OF btnAdd IN FRAME F-Main /* Add */
DO:
  RUN addEmailCode.
  APPLY 'ENTRY' TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearAll B-table-Win
ON CHOOSE OF btnClearAll IN FRAME F-Main /* Clear All */
DO:
  IF AVAILABLE phone THEN
  {&BROWSE-NAME}:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectAll B-table-Win
ON CHOOSE OF btnSelectAll IN FRAME F-Main /* Select All */
DO:
  IF AVAILABLE phone THEN
  {&BROWSE-NAME}:SELECT-ALL().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
/*
ON FIND OF cust
DO:
  IF emailCode() THEN RETURN ERROR.
END.
*/
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addEmailCode B-table-Win 
PROCEDURE addEmailCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:

    {&BROWSE-NAME}:FETCH-SELECTED-ROW (i) NO-ERROR.

    IF NOT AVAILABLE phone OR CAN-FIND (FIRST emaildtl 
                                        WHERE emaildtl.emailcod      EQ emailcod.emailcod
                                          AND emaildtl.table_rec_key EQ phone.rec_key) 
      THEN NEXT.

    CREATE emaildtl.

    ASSIGN
      emaildtl.emailcod      = emailcod.emailcod
      emaildtl.table_rec_key = phone.rec_key.

    IF NOT CAN-FIND(FIRST reftable WHERE
       reftable.rec_key = phone.rec_key AND
       reftable.CODE    = emailcod.emailcod) THEN
       DO:
          CREATE reftable.
          ASSIGN reftable.rec_key = phone.rec_key
                 reftable.CODE    = emailcod.emailcod.
          RELEASE reftable.
       END.
  END.

  {methods/run_link.i "Refresh-Target" "refreshQuery"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "emailcod"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emailcod"}

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
  APPLY 'VALUE-CHANGED':U TO browse-order IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshQuery B-table-Win 
PROCEDURE refreshQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    APPLY 'VALUE-CHANGED':U TO browse-order.
  END.

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
  {src/adm/template/snd-list.i "emailcod"}
  {src/adm/template/snd-list.i "phone"}
  {src/adm/template/snd-list.i "shipto"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION emailCode B-table-Win 
FUNCTION emailCode RETURNS LOGICAL
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN CAN-FIND(emaildtl WHERE emaildtl.emailcod EQ emailcod.emailcod
                             AND emaildtl.table_rec_key EQ phone.rec_key).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetCurrShipTo B-table-Win 
FUNCTION GetCurrShipTo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  apply 'entry':u to Browser-Table in frame {&frame-name}.

  if avail shipto 
    THEN RETURN shipto.rec_key.
    ELSE RETURN ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

