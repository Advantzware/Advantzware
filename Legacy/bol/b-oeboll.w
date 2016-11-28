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
DEFINE VARIABLE li-cost AS DECIMAL NO-UNDO.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{custom/gcompany.i}

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
oe-boll.i-no oe-boll.tag oe-boll.loc oe-boll.loc-bin oe-boll.cases ~
oe-boll.qty-case oe-boll.partial oe-boll.job-no oe-boll.job-no2 oe-boll.p-c ~
get-cost() @ li-cost 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table oe-boll.po-no ~
oe-boll.i-no oe-boll.tag oe-boll.loc oe-boll.loc-bin oe-boll.cases ~
oe-boll.qty-case oe-boll.partial oe-boll.job-no oe-boll.job-no2 oe-boll.p-c 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table oe-boll
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table oe-boll
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-boll WHERE oe-boll.company eq oe-bolh.company and ~
oe-boll.b-no eq oe-bolh.b-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-boll WHERE oe-boll.company eq oe-bolh.company and ~
oe-boll.b-no eq oe-bolh.b-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-boll
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-boll


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-23 RECT-4 browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-cost B-table-Win 
FUNCTION get-cost RETURNS DECIMAL
  (  ) FORWARD.

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
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 54 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143 BY 8.57.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 137 BY 1.43.

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
      oe-boll.cases
      oe-boll.qty-case
      oe-boll.partial
      oe-boll.job-no
      oe-boll.job-no2
      oe-boll.p-c) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-boll.ord-no FORMAT ">>>>>9":U
      oe-boll.po-no COLUMN-LABEL "PO No" FORMAT "x(15)":U
      oe-boll.i-no FORMAT "x(15)":U
      oe-boll.tag COLUMN-LABEL "Tag" FORMAT "x(8)":U
      oe-boll.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
      oe-boll.loc-bin COLUMN-LABEL "Bin Loc." FORMAT "x(8)":U
      oe-boll.cases COLUMN-LABEL "Units" FORMAT "->>>,>>Z":U
      oe-boll.qty-case COLUMN-LABEL "Qty/Unit" FORMAT "->>>,>>Z":U
      oe-boll.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
      oe-boll.job-no COLUMN-LABEL "Job No" FORMAT "x(6)":U
      oe-boll.job-no2 COLUMN-LABEL "" FORMAT "99":U
      oe-boll.p-c COLUMN-LABEL "P/C" FORMAT "C/P":U
      get-cost() @ li-cost COLUMN-LABEL "Cost/M" FORMAT ">>>,>>9.99":U
  ENABLE
      oe-boll.po-no
      oe-boll.i-no
      oe-boll.tag
      oe-boll.loc
      oe-boll.loc-bin
      oe-boll.cases
      oe-boll.qty-case
      oe-boll.partial
      oe-boll.job-no
      oe-boll.job-no2
      oe-boll.p-c
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 142 BY 7.14
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 8.62 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 8.62 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 8.62 COL 121 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 8.62 COL 2
     RECT-23 AT ROW 1 COL 1
     RECT-4 AT ROW 8.38 COL 1
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
         HEIGHT             = 8.81
         WIDTH              = 143.4.
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

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.oe-boll WHERE ASI.oe-bolh ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED, OUTER"
     _JoinCode[1]      = "oe-boll.company eq oe-bolh.company and
oe-boll.b-no eq oe-bolh.b-no"
     _FldNameList[1]   = ASI.oe-boll.ord-no
     _FldNameList[2]   > ASI.oe-boll.po-no
"oe-boll.po-no" "PO No" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-boll.i-no
"oe-boll.i-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-boll.tag
"oe-boll.tag" "Tag" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-boll.loc
"oe-boll.loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-boll.loc-bin
"oe-boll.loc-bin" "Bin Loc." ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-boll.cases
"oe-boll.cases" "Units" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-boll.qty-case
"oe-boll.qty-case" "Qty/Unit" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-boll.partial
"oe-boll.partial" "Partial" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.oe-boll.job-no
"oe-boll.job-no" "Job No" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-boll.job-no2
"oe-boll.job-no2" "" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-boll.p-c
"oe-boll.p-c" "P/C" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"get-cost() @ li-cost" "Cost/M" ">>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
{custom/getcmpny.i}
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-cost B-table-Win 
FUNCTION get-cost RETURNS DECIMAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF li-cost:VISIBLE IN BROWSE Browser-Table EQ NO THEN
     RETURN 0.
    
  DEF VAR v-t-cost AS DEC DECIMALS 4 NO-UNDO.
  DEF VAR v-uom AS CHAR NO-UNDO.
  def var v-cost AS DEC DECIMALS 4 extent 4 NO-UNDO.
  def var v-cost-m AS DEC DECIMALS 4 extent 4 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

  find first job-hdr WHERE
       job-hdr.company eq oe-boll.company AND
       job-hdr.job-no  eq oe-boll.job-no AND
       job-hdr.job-no2 eq oe-boll.job-no2 AND
       job-hdr.i-no    eq oe-boll.i-no
       no-lock no-error.

  find first itemfg WHERE
       itemfg.company EQ oe-boll.company AND
       itemfg.i-no eq oe-boll.i-no
       NO-LOCK no-error.

    find first fg-bin
        where fg-bin.company eq oe-boll.company
          and fg-bin.i-no    eq oe-boll.i-no
          and fg-bin.tag     eq oe-boll.tag
          and fg-bin.loc     eq oe-boll.loc
          and fg-bin.loc-bin eq oe-boll.loc-bin
          and fg-bin.job-no  eq oe-boll.job-no
          and fg-bin.job-no2 eq oe-boll.job-no2
        no-lock no-error.
  
    if avail fg-bin and fg-bin.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = fg-bin.std-lab-cost
       v-cost-m[2] = fg-bin.std-fix-cost
       v-cost-m[3] = fg-bin.std-var-cost
       v-cost-m[4] = fg-bin.std-mat-cost
       v-uom       = fg-bin.pur-uom.
       
    else
    if avail job-hdr and job-hdr.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = job-hdr.std-lab-cost
       v-cost-m[2] = job-hdr.std-fix-cost
       v-cost-m[3] = job-hdr.std-var-cost
       v-cost-m[4] = job-hdr.std-mat-cost
       v-uom       = "M".
       
    else   
      assign
       v-cost-m[1] = itemfg.std-lab-cost
       v-cost-m[2] = itemfg.std-fix-cost
       v-cost-m[3] = itemfg.std-var-cost
       v-cost-m[4] = itemfg.std-mat-cost.

    if v-uom eq "" then
       v-uom = itemfg.prod-uom.

    do i = 1 to 4:

       if v-uom ne "M" then
          run sys/ref/convcuom3.p(cocode,v-uom, "M", 0, 0, 0, 0,
                                 v-cost-m[i], output v-cost-m[i]).
       
       v-cost[i] = v-cost[i] + (v-cost-m[i] * oe-boll.qty / 1000).
    end.
  
  do i = 1 to 4:
     v-cost[i] = v-cost[i] / (oe-boll.qty / 1000).
    
     if v-cost[i] eq ? then v-cost[i] = 0.
  end.

  v-t-cost = v-cost[1] + v-cost[2] + v-cost[3] + v-cost[4].

  if v-t-cost eq ? then
     v-t-cost = 0.

  RETURN v-t-cost.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

