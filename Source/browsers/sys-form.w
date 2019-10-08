&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/emailcod.w

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

&SCOPED-DEFINE yellowColumnsName sys-ctrl-shipto            /*task# 12271301*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

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
&Scoped-define EXTERNAL-TABLES sys-ctrl
&Scoped-define FIRST-EXTERNAL-TABLE sys-ctrl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR sys-ctrl.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES sys-ctrl-shipto

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table sys-ctrl-shipto.cust-vend ~
sys-ctrl-shipto.cust-vend-no sys-ctrl-shipto.ship-id ~
sys-ctrl-shipto.char-fld sys-ctrl-shipto.date-fld sys-ctrl-shipto.dec-fld ~
sys-ctrl-shipto.int-fld sys-ctrl-shipto.log-fld 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH sys-ctrl-shipto OF sys-ctrl WHERE ~{&KEY-PHRASE} ~
      AND ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH sys-ctrl-shipto OF sys-ctrl WHERE ~{&KEY-PHRASE} ~
      AND ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table sys-ctrl-shipto
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table sys-ctrl-shipto


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS fi_sortby browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetCurrentRecord B-table-Win 
FUNCTION GetCurrentRecord RETURNS CHARACTER
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
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 37 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      sys-ctrl-shipto
    FIELDS(sys-ctrl-shipto.cust-vend
      sys-ctrl-shipto.cust-vend-no
      sys-ctrl-shipto.ship-id
      sys-ctrl-shipto.char-fld
      sys-ctrl-shipto.date-fld
      sys-ctrl-shipto.dec-fld
      sys-ctrl-shipto.int-fld
      sys-ctrl-shipto.log-fld) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      sys-ctrl-shipto.cust-vend COLUMN-LABEL "Type" FORMAT "Cust/Vend":U LABEL-BGCOLOR 14
      sys-ctrl-shipto.cust-vend-no COLUMN-LABEL "Cust/Vend" FORMAT "x(8)":U 
            WIDTH 12.2 LABEL-BGCOLOR 14
      sys-ctrl-shipto.ship-id COLUMN-LABEL "ShipTo" FORMAT "x(8)":U
            WIDTH 12.2 LABEL-BGCOLOR 14
      sys-ctrl-shipto.char-fld COLUMN-LABEL "Character" FORMAT "x(8)":U
            WIDTH 12.2 LABEL-BGCOLOR 14
      sys-ctrl-shipto.date-fld COLUMN-LABEL "Date" FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      sys-ctrl-shipto.dec-fld COLUMN-LABEL "Decimal" FORMAT "->>,>>9.99":U LABEL-BGCOLOR 14
      sys-ctrl-shipto.int-fld COLUMN-LABEL "Integer" FORMAT "->,>>>,>>9":U LABEL-BGCOLOR 14
      sys-ctrl-shipto.log-fld COLUMN-LABEL "Log" FORMAT "yes/no":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 99 BY 16.43
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_sortby AT ROW 16.86 COL 55 COLON-ALIGNED NO-LABEL
     browse-order AT ROW 17.67 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 17.67 COL 55 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.67 COL 86 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 17.67 COL 2
     RECT-4 AT ROW 17.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: asi.sys-ctrl
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
         HEIGHT             = 17.86
         WIDTH              = 99.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:PRIVATE-DATA     = 
                "4".

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "4".
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.     /*task# 12271301*/

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.sys-ctrl-shipto OF asi.sys-ctrl"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "~{&KEY-PHRASE}"
     _FldNameList[1]   > ASI.sys-ctrl-shipto.cust-vend
"sys-ctrl-shipto.cust-vend" "Type" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.sys-ctrl-shipto.cust-vend-no
"sys-ctrl-shipto.cust-vend-no" "Cust/Vend" ? "character" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.sys-ctrl-shipto.ship-id
"sys-ctrl-shipto.ship-id" "ShipTo" ? "character" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.sys-ctrl-shipto.char-fld
"sys-ctrl-shipto.char-fld" "Character" ? "character" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.sys-ctrl-shipto.date-fld
"sys-ctrl-shipto.date-fld" "Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.sys-ctrl-shipto.dec-fld
"sys-ctrl-shipto.dec-fld" "Decimal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.sys-ctrl-shipto.int-fld
"sys-ctrl-shipto.int-fld" "Integer" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.sys-ctrl-shipto.log-fld
"sys-ctrl-shipto.log-fld" "Log" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
    
    /* gdm - 09190806 */
    RUN StartSearch.

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
{custom/getcmpny.i}
{custom/yellowColumns.i}            /*task# 12271301*/

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
  {src/adm/template/row-list.i "sys-ctrl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "sys-ctrl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Values B-table-Win 
PROCEDURE Get-Values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opName AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opModule AS CHARACTER NO-UNDO.

  ASSIGN
    opName = IF AVAILABLE sys-ctrl THEN sys-ctrl.name ELSE ''
    opModule = IF AVAILABLE sys-ctrl THEN sys-ctrl.module ELSE ''.

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
  APPLY "value-changed" TO BROWSE {&browse-name}.
  APPLY "entry" TO BROWSE {&browse-name}.
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
  {src/adm/template/snd-list.i "sys-ctrl"}
  {src/adm/template/snd-list.i "sys-ctrl-shipto"}

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
    APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetCurrentRecord B-table-Win 
FUNCTION GetCurrentRecord RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  APPLY 'ENTRY':U TO Browser-Table IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

