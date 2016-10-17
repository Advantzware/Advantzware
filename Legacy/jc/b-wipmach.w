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

&SCOPED-DEFINE yellowColumnsName b-wiptag

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

   DEF VAR li-mach AS CHAR NO-UNDO.
   DEF VAR li-mach-out AS CHAR NO-UNDO.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES wiptag wiptag-mch

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table wiptag.tag-no ~
wiptag-mch.m-code get-mach-info (output li-mach-out) @ li-mach wiptag.rm-bin wiptag.rm-whs wiptag.wip-warehouse ~
wiptag.wip-rm-bin wiptag.job-no wiptag.job-no2 wiptag.rm-i-no ~
wiptag.fg-i-no wiptag.rm-tag-no wiptag-mch.produced-qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH wiptag WHERE ~{&KEY-PHRASE} ~
      AND wiptag.company = g_company AND wiptag.sts = "Printed" ~
 AND (ASI.wiptag.tag-no begins tb_tag-no or tb_tag-no = '') ~
and (asi.wiptag.job-no begins tb_job-no or tb_job-no = "") NO-LOCK, ~
      EACH wiptag-mch OF wiptag ~
      WHERE (ASI.wiptag-mch.m-code begins tb_machine or tb_machine = '') NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH wiptag WHERE ~{&KEY-PHRASE} ~
      AND wiptag.company = g_company AND wiptag.sts = "Printed" ~
 AND (ASI.wiptag.tag-no begins tb_tag-no or tb_tag-no = '') ~
and (asi.wiptag.job-no begins tb_job-no or tb_job-no = "") NO-LOCK, ~
      EACH wiptag-mch OF wiptag ~
      WHERE (ASI.wiptag-mch.m-code begins tb_machine or tb_machine = '') NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table wiptag wiptag-mch
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table wiptag
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table wiptag-mch


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_tag-no tb_machine tb_job-no ~
tb_job-no2 Browser-Table    
&Scoped-Define DISPLAYED-OBJECTS tb_tag-no tb_machine tb_job-no tb_job-no2 ~
 fi_sortby  

/* Custom List Definitions                                              */
/* filterFields,List-2,List-3,List-4,List-5,List-6                      */
&Scoped-define filterFields tb_tag-no tb_machine tb_job-no tb_job-no2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-mach-info B-table-Win 
FUNCTION get-mach-info RETURNS CHAR
  (OUTPUT op-mach-pal AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */


DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE tb_job-no AS CHARACTER FORMAT "x(6)" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_job-no2 AS INTEGER FORMAT ">9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_machine AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_tag-no AS CHARACTER FORMAT "X(20)" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1
     BGCOLOR 15  NO-UNDO.



DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      wiptag
    FIELDS(wiptag.tag-no
      wiptag.rm-bin
      wiptag.rm-whs
      wiptag.wip-warehouse
      wiptag.wip-rm-bin
      wiptag.job-no
      wiptag.job-no2
      wiptag.rm-i-no
      wiptag.fg-i-no
      wiptag.rm-tag-no), 
      wiptag-mch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      wiptag.tag-no FORMAT "X(20)":U WIDTH 26 LABEL-BGCOLOR 14
      wiptag-mch.m-code FORMAT "x(6)":U WIDTH 10.4 LABEL-BGCOLOR 14
      get-mach-info (output li-mach-out) @ li-mach COLUMN-LABEL "Machine Name" FORMAT "X(15)":U
            WIDTH 22 LABEL-BGCOLOR 14
      wiptag.rm-bin FORMAT "x(8)":U WIDTH 11 LABEL-BGCOLOR 14
      wiptag.rm-whs COLUMN-LABEL "RM Whs" FORMAT "x(5)":U WIDTH 11 LABEL-BGCOLOR 14
      wiptag.wip-warehouse COLUMN-LABEL "WIP Whs" FORMAT "x(5)":U WIDTH 11 LABEL-BGCOLOR 14
      wiptag.wip-rm-bin COLUMN-LABEL "WIP Bin" FORMAT "x(8)":U WIDTH 12 LABEL-BGCOLOR 14
      wiptag.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U WIDTH 10
            LABEL-BGCOLOR 14
      wiptag.job-no2 COLUMN-LABEL "#" FORMAT ">9":U WIDTH 3.4
      wiptag.rm-i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      wiptag.fg-i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      wiptag.rm-tag-no COLUMN-LABEL "RM Tag#" FORMAT "X(20)":U
            WIDTH 26 LABEL-BGCOLOR 14
      wiptag-mch.produced-qty COLUMN-LABEL "Produced Qty" FORMAT "->>>,>>>,>>9.9<<<<<":U
            LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 14.05
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tb_tag-no AT ROW 1.71 COL 2 NO-LABEL
     tb_machine AT ROW 1.71 COL 32 COLON-ALIGNED HELP
          "Enter the plant/warehouse location" NO-LABEL WIDGET-ID 10
     tb_job-no AT ROW 1.71 COL 49.8 COLON-ALIGNED HELP
          "Job Number." NO-LABEL
     tb_job-no2 AT ROW 1.71 COL 59.8 COLON-ALIGNED HELP
          "Enter Job sub-number." NO-LABEL
     Browser-Table AT ROW 2.91 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_sortby AT ROW 17.43 COL 78 COLON-ALIGNED
     "Job" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.1 COL 54.6
          FGCOLOR 9 FONT 6
     "Machine" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 1.1 COL 36 WIDGET-ID 12
          FGCOLOR 9 FONT 6
     "Tag#" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.05 COL 16
          FGCOLOR 9 FONT 6
     RECT-5 AT ROW 1 COL 1
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
/* BROWSE-TAB Browser-Table tb_job-no2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tb_job-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_job-no2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_machine IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_tag-no IN FRAME F-Main
   ALIGN-L 1                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.wiptag,ASI.wiptag-mch OF ASI.wiptag"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _Where[1]         = "ASI.wiptag.company = g_company
 AND (ASI.wiptag.tag-no begins tb_tag-no or tb_tag-no = '')
and (asi.wiptag.job-no begins tb_job-no or tb_job-no = """")"
     _Where[2]         = "(ASI.wiptag-mch.m-code begins tb_machine or tb_machine = '')"
     _FldNameList[1]   > ASI.wiptag.tag-no
"wiptag.tag-no" ? ? "character" ? ? ? 14 ? ? no ? no no "26" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.wiptag-mch.m-code
"wiptag-mch.m-code" ? ? "character" ? ? ? 14 ? ? no ? no no "10.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = ASI.wiptag.rm-bin
     _FldNameList[4]   = ASI.wiptag.rm-whs
     _FldNameList[5]   = ASI.wiptag.wip-warehouse
     _FldNameList[6]   = ASI.wiptag.wip-rm-bin
     _FldNameList[7]   > ASI.wiptag.job-no
"wiptag.job-no" "Job" ? "character" ? ? ? 14 ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.wiptag.job-no2
"wiptag.job-no2" "#" ? "integer" ? ? ? ? ? ? no "" no no "3.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.wiptag.rm-i-no
"wiptag.rm-i-no" ? ? "character" ? ? ? 14 ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.wiptag.fg-i-no
"wiptag.fg-i-no" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.wiptag.rm-tag-no
"wiptag.rm-tag-no" "RM Tag#" ? "character" ? ? ? 14 ? ? no ? no no "26" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.wiptag-mch.produced-qty
"wiptag-mch.produced-qty" "Produced Qty" ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main B-table-Win
ON HELP OF FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR v-rec-val AS cha NO-UNDO.

  CASE FOCUS:NAME:
      WHEN "tb_machine" THEN DO:
         RUN windows/l-mach.w (g_company, g_loc, FOCUS:SCREEN-VALUE, OUTPUT char-val).
         FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
         RETURN NO-APPLY.
      END.
      WHEN "tb_tag-no" THEN DO:
        RUN windows/l-wptagst.w (g_company, FOCUS:SCREEN-VALUE,"Printed", OUTPUT char-val, OUTPUT v-rec-val).
        IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
        return no-apply.
      END.
      WHEN "tb_job-no" THEN DO:

      END.
  END CASE.


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
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN reopen-query IN WIDGET-HANDLE(char-hdl).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tag-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tag-no B-table-Win
ON RETURN OF tb_tag-no IN FRAME F-Main
,tb_job-no,tb_job-no2,tb_machine
DO:
  ASSIGN {&filterFields}.
  IF tb_job-no NE '' THEN
     tb_job-no = FILL(' ',6 - LENGTH(TRIM(tb_job-no))) + TRIM(tb_job-no).

  RUN openQuery.
  APPLY 'VALUE-CHANGED':U TO BROWSE {&BROWSE-NAME}.
  APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-mach-info B-table-Win 
FUNCTION get-mach-info RETURNS CHAR
  (OUTPUT op-mach-pal AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

find first mach 
    where mach.company eq cocode
      and mach.m-code    eq wiptag-mch.m-code
      no-lock no-error.  
IF AVAIL mach THEN 
   ASSIGN
      li-mach = mach.m-dscr .
ELSE
   li-mach = "" .

RETURN li-mach.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

