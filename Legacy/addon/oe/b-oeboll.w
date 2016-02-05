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
{sys/inc/VAR.i "new shared"}
{custom/globdefs.i }
ASSIGN cocode = g_company
       locode = g_loc.

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
&Scoped-define EXTERNAL-TABLES oe-bolh
&Scoped-define FIRST-EXTERNAL-TABLE oe-bolh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-bolh.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-boll

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-boll.ord-no oe-boll.po-no ~
oe-boll.i-no oe-boll.tag oe-boll.loc oe-boll.loc-bin oe-boll.job-no ~
oe-boll.job-no2 oe-boll.cases oe-boll.qty-case oe-boll.partial ~
oe-boll.weight oe-boll.qty oe-boll.p-c 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table oe-boll.ord-no ~
oe-boll.po-no oe-boll.i-no oe-boll.tag oe-boll.loc oe-boll.loc-bin ~
oe-boll.job-no oe-boll.job-no2 oe-boll.cases oe-boll.qty-case ~
oe-boll.partial oe-boll.weight oe-boll.qty oe-boll.p-c 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table oe-boll
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table oe-boll
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-boll WHERE oe-boll.company eq oe-bolh.company and ~
oe-bolh.b-no eq oe-bolh.b-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-boll WHERE oe-boll.company eq oe-bolh.company and ~
oe-bolh.b-no eq oe-bolh.b-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-boll
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-boll


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table browse-order auto_find ~
Btn_Clear_Find RECT-4 
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
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order", 1
     SIZE 13 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-boll
    FIELDS(oe-boll.ord-no
      oe-boll.po-no
      oe-boll.i-no
      oe-boll.tag
      oe-boll.loc
      oe-boll.loc-bin
      oe-boll.job-no
      oe-boll.job-no2
      oe-boll.cases
      oe-boll.qty-case
      oe-boll.partial
      oe-boll.weight
      oe-boll.qty
      oe-boll.p-c) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-boll.ord-no COLUMN-LABEL "Order" FORMAT ">>>>>9":U COLUMN-FGCOLOR 9
      oe-boll.po-no FORMAT "x(15)":U
      oe-boll.i-no COLUMN-LABEL "FG Item #" FORMAT "x(15)":U
      oe-boll.tag COLUMN-LABEL "Tag" FORMAT "x(8)":U
      oe-boll.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
      oe-boll.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      oe-boll.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U
      oe-boll.job-no2 COLUMN-LABEL "" FORMAT "99":U
      oe-boll.cases COLUMN-LABEL "Units" FORMAT "->>>,>>Z":U
      oe-boll.qty-case COLUMN-LABEL "Qty/Unit" FORMAT "->>>,>>Z":U
      oe-boll.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
      oe-boll.weight COLUMN-LABEL "Weight" FORMAT ">>>>9":U
      oe-boll.qty FORMAT "->>,>>>,>>9":U
      oe-boll.p-c COLUMN-LABEL "P/C" FORMAT "C/P":U
  ENABLE
      oe-boll.ord-no
      oe-boll.po-no
      oe-boll.i-no
      oe-boll.tag
      oe-boll.loc
      oe-boll.loc-bin
      oe-boll.job-no
      oe-boll.job-no2
      oe-boll.cases
      oe-boll.qty-case
      oe-boll.partial
      oe-boll.weight
      oe-boll.qty
      oe-boll.p-c
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 6.43
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 7.67 COL 10 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 7.67 COL 75 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 7.67 COL 131 HELP
          "CLEAR AUTO FIND Value"
     RECT-4 AT ROW 7.43 COL 1
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 7.67 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.oe-bolh
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.oe-boll WHERE ASI.oe-bolh ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,,"
     _JoinCode[1]      = "oe-boll.company eq oe-bolh.company and
oe-bolh.b-no eq oe-bolh.b-no"
     _FldNameList[1]   > ASI.oe-boll.ord-no
"oe-boll.ord-no" "Order" ? "integer" ? 9 ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > ASI.oe-boll.po-no
"oe-boll.po-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > ASI.oe-boll.i-no
"oe-boll.i-no" "FG Item #" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > ASI.oe-boll.tag
"oe-boll.tag" "Tag" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.oe-boll.loc
"oe-boll.loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > ASI.oe-boll.loc-bin
"oe-boll.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > ASI.oe-boll.job-no
"oe-boll.job-no" "Job #" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > ASI.oe-boll.job-no2
"oe-boll.job-no2" "" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > ASI.oe-boll.cases
"oe-boll.cases" "Units" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > ASI.oe-boll.qty-case
"oe-boll.qty-case" "Qty/Unit" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > ASI.oe-boll.partial
"oe-boll.partial" "Partial" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > ASI.oe-boll.weight
"oe-boll.weight" "Weight" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > ASI.oe-boll.qty
"oe-boll.qty" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > ASI.oe-boll.p-c
"oe-boll.p-c" "P/C" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
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
  {src/adm/template/row-list.i "oe-bolh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-bolh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-boll B-table-Win 
PROCEDURE delete-boll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-do-bol AS LOG NO-UNDO.
if oe-bolh.posted then oe-boll.deleted = yes.
else delete oe-boll.


DISABLE TRIGGERS FOR LOAD OF oe-relh.

{oe/bollrell.i}




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
  assign
   oe-boll.b-no   = oe-bolh.b-no
   oe-boll.bol-no = oe-bolh.bol-no
   oe-boll.r-no   = oe-bolh.r-no.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-boll FOR oe-boll.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/askdel.i}
  

  DEF VAR v-do-bol AS LOG NO-UNDO.
  if oe-bolh.posted then oe-boll.deleted = yes.
  else /* Dispatch standard ADM method.                             */
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

      DISABLE TRIGGERS FOR LOAD OF oe-relh.

      {oe/bollrell.i}
 FIND CURRENT oe-bolh EXCLUSIVE-LOCK.
 FIND FIRST bf-boll where bf-boll.company eq oe-bolh.company and bf-boll.b-no eq oe-bolh.b-no NO-LOCK NO-ERROR.
 IF NOT AVAIL bf-boll THEN DELETE oe-bolh.

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
  {src/adm/template/snd-list.i "oe-bolh"}
  {src/adm/template/snd-list.i "oe-boll"}

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

