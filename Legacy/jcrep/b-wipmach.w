&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  b-wiptag.w

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

&SCOPED-DEFINE yellowColumnsName b-wipmach
&SCOPED-DEFINE browse2 methods\browsers\wiptag-mch2.i
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

ASSIGN cocode = g_company
       locode = g_loc.

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
&Scoped-define EXTERNAL-TABLES wiptag
&Scoped-define FIRST-EXTERNAL-TABLE wiptag


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR wiptag.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES wiptag-mch item mach dept

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table wiptag.tag-no ~
wiptag-mch.m-code dept.dscr wiptag.wip-warehouse wiptag.wip-rm-bin ~
wiptag.job-no wiptag.job-no2 wiptag-mch.produced-qty dept.seq 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH wiptag-mch OF wiptag WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH item WHERE TRUE /* Join to wiptag incomplete */ ~
      AND item.company = cocode ~
 AND item.i-no = wiptag.rm-i-no NO-LOCK, ~
      EACH mach WHERE TRUE /* Join to wiptag-mch incomplete */ ~
      AND mach.company = cocode and ~
asi.mach.m-code = wiptag-mch.m-code NO-LOCK, ~
      EACH dept WHERE TRUE /* Join to mach incomplete */ ~
      AND dept.code = mach.dept[1] NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH wiptag-mch OF wiptag WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH item WHERE TRUE /* Join to wiptag incomplete */ ~
      AND item.company = cocode ~
 AND item.i-no = wiptag.rm-i-no NO-LOCK, ~
      EACH mach WHERE TRUE /* Join to wiptag-mch incomplete */ ~
      AND mach.company = cocode and ~
asi.mach.m-code = wiptag-mch.m-code NO-LOCK, ~
      EACH dept WHERE TRUE /* Join to mach incomplete */ ~
      AND dept.code = mach.dept[1] NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table wiptag-mch item mach dept
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table wiptag-mch
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table item
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table mach
&Scoped-define FOURTH-TABLE-IN-QUERY-Browser-Table dept


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Browser-Table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 Browser-Table browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

/* Custom List Definitions                                              */
/* filterFields,List-2,List-3,List-4,List-5,List-6                      */

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
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 63 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      wiptag-mch
    FIELDS(wiptag-mch.m-code
      wiptag-mch.produced-qty), 
      item, 
      mach, 
      dept SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      wiptag.tag-no COLUMN-LABEL "WIP Tag" FORMAT "X(20)":U WIDTH 26
      wiptag-mch.m-code COLUMN-LABEL "Machine Code" FORMAT "x(6)":U
            WIDTH 18
      dept.dscr COLUMN-LABEL "Machine Department" FORMAT "x(25)":U
            WIDTH 30 LABEL-BGCOLOR 14
      wiptag.wip-warehouse COLUMN-LABEL "WIP Whs" FORMAT "x(5)":U
            WIDTH 12
      wiptag.wip-rm-bin COLUMN-LABEL "WIP Bin" FORMAT "x(8)":U
            WIDTH 12
      wiptag.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U WIDTH 10
      wiptag.job-no2 COLUMN-LABEL "#" FORMAT ">9":U WIDTH 3.4
      wiptag-mch.produced-qty COLUMN-LABEL "Tag Qty Produced" FORMAT "->>>,>>>,>>9.9<<<<<":U
            WIDTH 26
      dept.seq FORMAT ">9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 15.71
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1.24 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 17.43 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 17.43 COL 68 COLON-ALIGNED
     auto_find AT ROW 17.43 COL 111 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.43 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 17.43 COL 2
     RECT-4 AT ROW 17.19 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: asi.wiptag
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
         HEIGHT             = 17.67
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table RECT-4 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       browse-order:HIDDEN IN FRAME F-Main           = TRUE
       browse-order:PRIVATE-DATA IN FRAME F-Main     = 
                "1".

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "4"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

ASSIGN 
       dept.seq:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.wiptag-mch OF ASI.wiptag,asi.item WHERE asi.wiptag ...,asi.mach WHERE ASI.wiptag-mch ...,asi.dept WHERE asi.mach ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,,,,"
     _Where[2]         = "asi.item.company = cocode
 AND asi.item.i-no = wiptag.rm-i-no"
     _Where[3]         = "asi.mach.company = cocode and
asi.mach.m-code = asi.wiptag-mch.m-code"
     _Where[4]         = "asi.dept.code = asi.mach.dept[1]"
     _FldNameList[1]   > ASI.wiptag.tag-no
"wiptag.tag-no" "WIP Tag" ? "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.wiptag-mch.m-code
"wiptag-mch.m-code" "Machine Code" ? "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.dept.dscr
"dept.dscr" "Machine Department" ? "character" ? ? ? 14 ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.wiptag.wip-warehouse
"wiptag.wip-warehouse" "WIP Whs" ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.wiptag.wip-rm-bin
"wiptag.wip-rm-bin" "WIP Bin" ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.wiptag.job-no
"wiptag.job-no" "Job" ? "character" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.wiptag.job-no2
"wiptag.job-no2" "#" ? "integer" ? ? ? ? ? ? no ? no no "3.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.wiptag-mch.produced-qty
"wiptag-mch.produced-qty" "Tag Qty Produced" ? "decimal" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.dept.seq
"dept.seq" ? ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main B-table-Win
ON HELP OF FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR v-rec-val AS cha NO-UNDO.

/*   CASE FOCUS:NAME:                                                                                  */
/*       WHEN "tb_machine" THEN DO:                                                                    */
/*          RUN windows/l-mach.w (g_company, g_loc, FOCUS:SCREEN-VALUE, OUTPUT char-val).              */
/*          FOCUS:SCREEN-VALUE = ENTRY(1,char-val).                                                    */
/*          RETURN NO-APPLY.                                                                           */
/*       END.                                                                                          */
/*       WHEN "tb_tag-no" THEN DO:                                                                     */
/*         RUN windows/l-wptag1.w (g_company, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT v-rec-val).  */
/*         IF char-val <> "" THEN SELF:SCREEN-VALUE = char-val.                                        */
/*                                                                                                     */
/*         return no-apply.                                                                            */
/*       END.                                                                                          */
/*       WHEN "tb_job-no" THEN DO:                                                                     */
/*                                                                                                     */
/*       END.                                                                                          */
/*   END CASE.                                                                                         */
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME auto_find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL auto_find B-table-Win
ON ENTRY OF auto_find IN FRAME F-Main /* Auto Find */
DO:
  MESSAGE "ENTRY INTO BROWSE" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,'tagNo-target',OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
      RUN reopen-query IN WIDGET-HANDLE(char-hdl).
      MESSAGE "Reopened query in browse" VIEW-AS ALERT-BOX.
  END.
     

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "wiptag"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "wiptag"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyValueChanged B-table-Win 
PROCEDURE applyValueChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY 'value-changed' TO BROWSE {&browse-name}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-current-rowid B-table-Win 
PROCEDURE get-current-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-rowid2 AS ROWID NO-UNDO.

  op-rowid = IF AVAILABLE wiptag THEN ROWID(wiptag) ELSE ?.
  op-rowid2 = IF AVAILABLE wiptag-mch THEN ROWID(wiptag-mch) ELSE ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMachine B-table-Win 
PROCEDURE getMachine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-machine AS cha .

  op-machine = IF AVAIL wiptag-mch THEN wiptag-mch.m-code ELSE "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTag# B-table-Win 
PROCEDURE getTag# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-tag# AS cha .

  op-tag# = IF AVAIL wiptag THEN wiptag.tag-no ELSE "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 
 
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,'tagNo-target',OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN reopen-query IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-brwse B-table-Win 
PROCEDURE refresh-brwse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,'tagNo-target',OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
      RUN reopen-query IN WIDGET-HANDLE(char-hdl).
/*       MESSAGE "Reopened query in browse" VIEW-AS ALERT-BOX.  */
  END.
/*   ELSE MESSAGE "VALID-HANDLE(WIDGET-HANDLE(char-hdl))" SKIP     */
/*               "char-hdl " char-hdl                              */
/*       VALID-HANDLE(WIDGET-HANDLE(char-hdl)) VIEW-AS ALERT-BOX.  */
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS rowid NO-UNDO.
  DEF INPUT PARAM ip-rowid2 AS rowid NO-UNDO.

  REPOSITION {&browse-name} TO ROWID ip-rowid, ip-rowid2 NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ('row-changed').

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
  {src/adm/template/snd-list.i "wiptag"}
  {src/adm/template/snd-list.i "wiptag-mch"}
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "mach"}
  {src/adm/template/snd-list.i "dept"}

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

