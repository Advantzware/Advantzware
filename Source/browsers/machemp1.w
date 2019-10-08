&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
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
&SCOPED-DEFINE yellowColumnsName machemp
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE start-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE end-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE total-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE employee-name AS CHARACTER NO-UNDO.

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/var.i new shared}
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
&Scoped-define INTERNAL-TABLES machemp machtran employee

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table machemp.employee ~
Employee-Name(machtran.company,machemp.employee) @ employee-name ~
machtran.machine machemp.start_date ~
STRING(machemp.start_time,'HH:MM am') @ start-time machemp.end_date ~
machemp.shift Time_String(machemp.end_time,yes) @ end-time machemp.ratetype ~
machemp.posted Time_String(machemp.total_time,no) @ total-time 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH machemp WHERE ~{&KEY-PHRASE} ~
      AND machemp.posted = no USE-INDEX postemp NO-LOCK, ~
      EACH machtran WHERE TRUE /* Join to machemp incomplete */ ~
      AND machtran.rec_key eq machemp.table_rec_key ~
 AND machtran.company = gcompany NO-LOCK, ~
      FIRST employee WHERE TRUE /* Join to machemp incomplete */ ~
      AND employee.company eq gcompany AND ~
employee.employee eq machemp.employee OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH machemp WHERE ~{&KEY-PHRASE} ~
      AND machemp.posted = no USE-INDEX postemp NO-LOCK, ~
      EACH machtran WHERE TRUE /* Join to machemp incomplete */ ~
      AND machtran.rec_key eq machemp.table_rec_key ~
 AND machtran.company = gcompany NO-LOCK, ~
      FIRST employee WHERE TRUE /* Join to machemp incomplete */ ~
      AND employee.company eq gcompany AND ~
employee.employee eq machemp.employee OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table machemp machtran employee
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table machemp
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table machtran
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table employee


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Employee-Name B-table-Win 
FUNCTION Employee-Name RETURNS CHARACTER
  (ip-company AS CHARACTER,ip-employee AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Get_Rate B-table-Win 
FUNCTION Get_Rate RETURNS DECIMAL
  ( ip-rate AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Time_String B-table-Win 
FUNCTION Time_String RETURNS CHARACTER
  (ip-time AS INTEGER,ip-clock-time AS LOGICAL) FORWARD.

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
     SIZE 57 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 142 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      machemp
    FIELDS(machemp.employee
      machemp.employee
      machemp.start_date
      machemp.start_time
      machemp.end_date
      machemp.shift
      machemp.end_time
      machemp.ratetype
      machemp.posted
      machemp.total_time), 
      machtran, 
      employee SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      machemp.employee FORMAT "x(5)":U LABEL-BGCOLOR 14
      Employee-Name(machtran.company,machemp.employee) @ employee-name COLUMN-LABEL "Name" FORMAT "X(14)":U
      machtran.machine FORMAT "x(6)":U
      machemp.start_date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      STRING(machemp.start_time,'HH:MM am') @ start-time COLUMN-LABEL "Started" FORMAT "X(8)":U
      machemp.end_date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      machemp.shift FORMAT "XX":U LABEL-BGCOLOR 14
      Time_String(machemp.end_time,yes) @ end-time COLUMN-LABEL "Ended" FORMAT "X(8)":U
      machemp.ratetype FORMAT "X(12)":U LABEL-BGCOLOR 14
      machemp.posted FORMAT "yes/no":U LABEL-BGCOLOR 14
      Time_String(machemp.total_time,no) @ total-time COLUMN-LABEL "Total" FORMAT "X(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 142 BY 18.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 19.33 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 129 HELP
          "CLEAR AUTO FIND Value"
     fi_sortby AT ROW 19.38 COL 43.4 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 19.33 COL 2
     RECT-4 AT ROW 19.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


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
         HEIGHT             = 19.52
         WIDTH              = 142.
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

/* SETTINGS FOR RADIO-SET browse-order IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "machemp,machtran WHERE machemp ... ...,employee WHERE machemp ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,, FIRST OUTER"
     _Where[1]         = "machemp.posted = no USE-INDEX postemp"
     _Where[2]         = "machtran.rec_key eq machemp.table_rec_key
 AND machtran.company = gcompany"
     _Where[3]         = "employee.company eq gcompany AND
employee.employee eq machemp.employee"
     _FldNameList[1]   > machemp.employee
"machemp.employee" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Employee-Name(machtran.company,machemp.employee) @ employee-name" "Name" "X(14)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = machtran.machine
     _FldNameList[4]   > machemp.start_date
"machemp.start_date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"STRING(machemp.start_time,'HH:MM am') @ start-time" "Started" "X(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > machemp.end_date
"machemp.end_date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > machemp.shift
"machemp.shift" ? "XX" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"Time_String(machemp.end_time,yes) @ end-time" "Ended" "X(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > machemp.ratetype
"machemp.ratetype" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > machemp.posted
"machemp.posted" ? ? "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"Time_String(machemp.total_time,no) @ total-time" "Total" "X(5)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   RUN startSearch.
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
{custom/yellowColumns.i}
{custom/getcmpny.i}
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
  def output param op-rec-key like machtran.rec_key.
  
  op-rec-key = machtran.rec_key.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR FromEmpId AS CHAR INIT "" NO-UNDO.
DEF VAR ToEmpId AS CHAR INIT "zzzzz" NO-UNDO.

IF AVAIL machemp AND machemp.employee NE "" THEN
    ASSIGN
    FromEmpId = machemp.employee
    ToEmpId   = machemp.employee .

RUN addon/browsers/rd-mh1exp.w (FromEmpId,ToEmpId).

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
  {src/adm/template/snd-list.i "machemp"}
  {src/adm/template/snd-list.i "machtran"}
  {src/adm/template/snd-list.i "employee"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Employee-Name B-table-Win 
FUNCTION Employee-Name RETURNS CHARACTER
  (ip-company AS CHARACTER,ip-employee AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  Get Employee Name
    Notes:  
------------------------------------------------------------------------------*/
  FIND employee WHERE employee.company = ip-company
                  AND employee.employee = ip-employee NO-LOCK NO-ERROR.
  IF AVAILABLE employee THEN
  RETURN employee.last_name + ',' +  employee.first_name.
  ELSE
  RETURN ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Get_Rate B-table-Win 
FUNCTION Get_Rate RETURNS DECIMAL
  ( ip-rate AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  {custom/viewrate.i}
    RETURN machemp.rate.
  else
    RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Time_String B-table-Win 
FUNCTION Time_String RETURNS CHARACTER
  (ip-time AS INTEGER,ip-clock-time AS LOGICAL):
/*------------------------------------------------------------------------------
  Purpose:  return time in string format
    Notes:  
------------------------------------------------------------------------------*/
  IF ip-time = 0 AND machemp.end_date EQ ? THEN
  RETURN ''.
  ELSE
  IF ip-clock-time THEN
  RETURN STRING(ip-time,'HH:MM am').
  ELSE
  RETURN STRING(ip-time,'HH:MM').

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

