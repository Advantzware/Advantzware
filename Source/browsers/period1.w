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
&SCOPED-DEFINE yellowColumnsName period1
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE proc-init proc-init
//&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cSubLedgerAP AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubLedgerPO AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubLedgerOP AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubLedgerWIP AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubLedgerRM AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubLedgerFG AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubLedgerBR AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubLedgerAR AS CHARACTER NO-UNDO.
DEFINE VARIABLE lColMove     AS LOGICAL INIT YES NO-UNDO.

DEFINE BUFFER bf-period FOR period .
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
&Scoped-define EXTERNAL-TABLES company
&Scoped-define FIRST-EXTERNAL-TABLE company


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR company.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES period

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table period.yr period.pnum ~
period.pst period.pend period.pstat getSubLedger(period.SubLedgerAP) @ cSubLedgerAP getSubLedger(period.SubLedgerPO) @ cSubLedgerPO ~
getSubLedger(period.SubLedgerOP) @ cSubLedgerOP getSubLedger(period.SubLedgerWIP) @ cSubLedgerWIP getSubLedger(period.SubLedgerRM) @ cSubLedgerRM ~
getSubLedger(period.SubLedgerFG) @ cSubLedgerFG getSubLedger(period.SubLedgerBR) @ cSubLedgerBR getSubLedger(period.SubLedgerAR) @ cSubLedgerAR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table period
&Scoped-define QUERY-STRING-Browser-Table FOR EACH period OF company WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH period OF company WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table period
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table period


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find fi_sortby

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSubLedger B-table-Win 
FUNCTION getSubLedger RETURNS CHARACTER
  ( ipcSubLedger AS CHARACTER )  FORWARD.

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
     SIZE 15 BY 1 NO-UNDO.
     
DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.     

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      period
    FIELDS(period.yr
      period.pnum
      period.pst
      period.pend
      period.pstat
      period.subLedgerAP
      period.subLedgerPO
      period.subLedgerOP
      period.subLedgerWIP
      period.subLedgerRM
      period.subLedgerFG
      period.subLedgerBR
      period.subLedgerAR) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      period.yr FORMAT "9999":U LABEL-BGCOLOR 14
      period.pnum FORMAT ">9":U LABEL-BGCOLOR 14
      period.pst COLUMN-LABEL "Start Date" FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      period.pend COLUMN-LABEL "End Date" FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      period.pstat COLUMN-LABEL "Status" FORMAT "Open/Closed":U LABEL-BGCOLOR 14
      getSubLedger(period.SubLedgerAP) @ cSubLedgerAP COLUMN-LABEL "AP" FORMAT "x(10)":U LABEL-BGCOLOR 14
      getSubLedger(period.SubLedgerPO) @ cSubLedgerPO COLUMN-LABEL "P/O" FORMAT "x(10)":U LABEL-BGCOLOR 14
      getSubLedger(period.SubLedgerOP) @ cSubLedgerOP COLUMN-LABEL "O/P" FORMAT "x(10)":U LABEL-BGCOLOR 14
      getSubLedger(period.SubLedgerWIP) @ cSubLedgerWIP COLUMN-LABEL "WIP" FORMAT "x(10)":U LABEL-BGCOLOR 14
      getSubLedger(period.SubLedgerRM) @ cSubLedgerRM COLUMN-LABEL "R/M" FORMAT "x(10)":U LABEL-BGCOLOR 14
      getSubLedger(period.SubLedgerFG) @ cSubLedgerFG COLUMN-LABEL "F/G" FORMAT "x(10)":U LABEL-BGCOLOR 14
      getSubLedger(period.SubLedgerBR) @ cSubLedgerBR COLUMN-LABEL "B/R" FORMAT "x(10)":U LABEL-BGCOLOR 14
      getSubLedger(period.SubLedgerAR) @ cSubLedgerAR COLUMN-LABEL "A/R" FORMAT "x(10)":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.48
         FONT 2
         TITLE "Periods".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 16.71 COL 51 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     fi_sortby AT ROW 16.33 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 2          
     Btn_Clear_Find AT ROW 16.71 COL 69 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 2
     RECT-4 AT ROW 16.48 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.company
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
         WIDTH              = 86.6.
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
       
Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.       
       
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
     _TblList          = "ASI.period OF ASI.company"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   = ASI.period.yr
     _FldNameList[2]   = ASI.period.pnum
     _FldNameList[3]   > ASI.period.pst
"period.pst" "Start Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.period.pend
"period.pend" "End Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.period.pstat
"period.pstat" "Status" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"getSubLedger(period.SubLedgerAP) @ cSubLedgerAP" "AP" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"getSubLedger(period.SubLedgerPO) @ cSubLedgerPO" "P/O" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"getSubLedger(period.SubLedgerOP) @ cSubLedgerOP" "O/P" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"getSubLedger(period.SubLedgerWIP) @ cSubLedgerWIP" "WIP" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"getSubLedger(period.SubLedgerRM) @ cSubLedgerRM" "R/M" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"getSubLedger(period.SubLedgerFG) @ cSubLedgerFG" "F/G" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"getSubLedger(period.SubLedgerBR) @ cSubLedgerBR" "B/R" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"getSubLedger(period.SubLedgerAR) @ cSubLedgerAR" "A/R" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main /* Periods */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
   DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.

    IF AVAILABLE period THEN DO:
        RUN viewers/d-period.w (RECID(period),RECID(company),"view", OUTPUT lv-rowid) . 
        RUN repo-query (lv-rowid).
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main /* Periods */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main /* Periods */
DO:  
  RUN startsearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main /* Periods */
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
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

&SCOPED-DEFINE cellColumnDat browsers-period

{methods/browsers/setCellColumns.i}

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
  {src/adm/template/row-list.i "company"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "company"}

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
 DEFINE OUTPUT PARAMETER op-company AS CHARACTER NO-UNDO.

  op-company = IF AVAIL company THEN company.company ELSE g_company.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAutoFilter B-table-Win 
PROCEDURE pAutoFilter :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
      FIND LAST bf-period OF company NO-LOCK
           WHERE bf-period.pstat EQ YES  NO-ERROR .
    IF AVAIL bf-period THEN do:
        ASSIGN auto_find:SCREEN-VALUE = string(bf-period.yr) .
        browse-order:SCREEN-VALUE = "1"  .
        
        APPLY "entry" TO auto_find .
        APPLY 'ENTRY' TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
    END.
  END.
  
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
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  RUN dispatch IN THIS-PROCEDURE ("open-query").

  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

  RUN dispatch IN THIS-PROCEDURE ("row-changed").

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
  {src/adm/template/snd-list.i "company"}
  {src/adm/template/snd-list.i "period"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       oe/oe-rel.del, oe/oerelunp.p 
------------------------------------------------------------------------------*/
  
  IF NOT adm-new-record THEN DO:
        {custom/askdel.i}   
  END. 
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
  
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-init B-table-Win 
PROCEDURE proc-init :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/   

    RUN setCellColumns.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME  

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
     Browser-Table:COLUMN-MOVABLE = lColMove
     Browser-Table:COLUMN-RESIZABLE = lColMove
     lColMove = NOT lColMove.
	 
     /*FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
  DISPLAY FI_moveCol.*/
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME  


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddRecord B-table-Win 
PROCEDURE pAddRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
  DEFINE BUFFER bff-fg-rctd FOR fg-rctd .
    
   RUN viewers/d-period.w (?,RECID(company),"New", OUTPUT lv-rowid) . 
   RUN repo-query (lv-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRecord B-table-Win 
PROCEDURE pUpdateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    
    IF AVAILABLE period THEN
    DO:
       RUN viewers/d-period.w (RECID(period),RECID(company),"update", OUTPUT lv-rowid) . 
       RUN repo-query (lv-rowid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewRecord B-table-Win 
PROCEDURE pViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    
    IF AVAILABLE period THEN
    DO:
       RUN viewers/d-period.w (RECID(period),RECID(company),"view", OUTPUT lv-rowid) . 
       RUN repo-query (lv-rowid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyRecord B-table-Win 
PROCEDURE pCopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
   DEF BUFFER b-fg-rctd FOR fg-rctd.
   DEFINE BUFFER bff-fg-rctd FOR fg-rctd.
   DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    
    IF AVAILABLE period THEN
    DO:  
       RUN viewers/d-period.w (RECID(period),RECID(company),"copy", OUTPUT lv-rowid) . 
       IF lv-rowid NE ? THEN
           RUN repo-query (lv-rowid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSubLedger B-table-Win 
FUNCTION getSubLedger RETURNS CHARACTER
  (  ipcSubLedger AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
                 
    IF AVAILABLE period THEN DO: 
        lc-result = ipcSubLedger .         
        IF lc-result EQ "O" THEN
        cResult = "Open". 
        ELSE IF lc-result EQ "C" THEN
        cResult = "Close".
        ELSE IF lc-result EQ "A" THEN
        cResult = "Auto Close".
        ELSE IF lc-result EQ "X" THEN
        cResult = "NA".            
        IF lc-result EQ "" AND period.pstat EQ TRUE THEN
        cResult = "Open".
        ELSE IF lc-result EQ "" AND period.pstat EQ FALSE THEN
         cResult = "Close".       
    END.
    RETURN cResult.   /* Function return value. */
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
