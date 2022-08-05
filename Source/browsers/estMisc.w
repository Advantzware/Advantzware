&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: browsers/estMisc.w 

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
{est/ttEstMisc.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstimateNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcOnDesc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcToDesc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSourceType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcByCustomList AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdEstMiscProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE iFormNo        AS INTEGER   NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttEstMisc

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ttEstMisc.costDescription ~
ttEstMisc.estCostCalcBy ~
fGetCalcDesc(ttEstMisc.estCostCategoryID, TRUE) @ cCalcToDesc ~
fGetCalcDesc(ttEstMisc.estCostCalcSource, FALSE) @ cCalcOnDesc ~
ttEstMisc.flatFeeCharge ttEstMisc.chargePercent 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH ttEstMisc WHERE ~{&KEY-PHRASE} ~
      AND ttEstMisc.company = cCompany AND ttEstMisc.estimateNo = cEstimateNo NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH ttEstMisc WHERE ~{&KEY-PHRASE} ~
      AND ttEstMisc.company = cCompany AND ttEstMisc.estimateNo = cEstimateNo NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table ttEstMisc
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ttEstMisc


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS>
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetCalcDesc B-table-Win 
FUNCTION fGetCalcDesc RETURNS CHARACTER
  ( ipcCalcID AS CHARACTER, iplIsCategory AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      ttEstMisc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      ttEstMisc.costDescription FORMAT "x(40)":U WIDTH 32.2
      fGetCalcDesc(ttEstMisc.estCostCategoryID, TRUE) @ cCalcToDesc COLUMN-LABEL "Category" FORMAT "X(50)":U
            WIDTH 32.2
      ttEstMisc.estCostCalcBy FORMAT "x(32)":U WIDTH 19
      fGetCalcDesc(ttEstMisc.estCostCalcSource, FALSE) @ cCalcOnDesc COLUMN-LABEL "Cost Source" FORMAT "X(50)":U
            WIDTH 32.2
      ttEstMisc.flatFeeCharge FORMAT ">>,>>,>>9.99<<<<":U
      ttEstMisc.chargePercent FORMAT ">>,>>,>>9.99<<<<":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 129 BY 14.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
         HEIGHT             = 14.43
         WIDTH              = 129.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.ttEstMisc"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.ttEstMisc.company = cCompany AND ASI.ttEstMisc.estimateNo = cEstimateNo"
     _FldNameList[1]   > ASI.ttEstMisc.costDescription
"costDescription" ? ? "character" ? ? ? ? ? ? no ? no no "32.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ttEstMisc.estCostCalcBy
"estCostCalcBy" ? ? "character" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"fGetCalcDesc(ttEstMisc.estCostCategoryID, TRUE) @ cCalcToDesc" "Category" "X(50)" ? ? ? ? ? ? ? no ? no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"fGetCalcDesc(ttEstMisc.estCostCalcSource, FALSE) @ cCalcOnDesc" "Cost Source" "X(50)" ? ? ? ? ? ? ? no ? no no "26.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = ASI.ttEstMisc.flatFeeCharge
     _FldNameList[6]   = ASI.ttEstMisc.chargePercent
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetEstMisc B-table-Win
PROCEDURE GetEstMisc:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER ophdTTEstMiscHandle AS HANDLE NO-UNDO.
    
    IF AVAILABLE ttEstMisc THEN
        ophdTTEstMiscHandle = BUFFER ttEstMisc:HANDLE.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable B-table-Win
PROCEDURE local-enable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
       
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
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
    {methods/run_link.i "CONTAINER-SOURCE" "GetEstimateNo" "(OUTPUT cEstimateNo, OUTPUT iFormNo)"}
    
    RUN EstMisc_GetEstMisc IN hdEstMiscProcs (cSourceType, cCompany, cEstimateNo, OUTPUT TABLE ttEstMisc).
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReOpenQuery B-table-Win 
PROCEDURE pReOpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-rowid as rowid no-undo.           
  DEFINE BUFFER bf-ttEstMisc FOR ttEstMisc.
  
  RUN local-open-query.   
  
  FIND FIRST bf-ttEstMisc NO-LOCK
       WHERE bf-ttEstMisc.sourceRowID EQ ip-rowid NO-ERROR.
                
  IF AVAILABLE bf-ttEstMisc THEN
  reposition {&browse-name} to rowid rowid(bf-ttEstMisc) no-error.

  run dispatch in this-procedure ("row-changed").
  APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit B-table-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/run_link.i "CONTAINER-SOURCE" "GetSourceType" "(OUTPUT cSourceType)"}
    {methods/run_link.i "CONTAINER-SOURCE" "GetEstMiscProcsHandle" "(OUTPUT hdEstMiscProcs)"}
    {methods/run_link.i "CONTAINER-SOURCE" "GetEstimateNo" "(OUTPUT cEstimateNo, OUTPUT iFormNo)"}
    
    IF NOT VALID-HANDLE (hdEstMiscProcs) THEN
        RUN est/estMiscProcs.p PERSISTENT SET hdEstMiscProcs.
        
    RUN EstMisc_GetCustomList IN hdEstMiscProcs (cCompany, OUTPUT cCalcByCustomList).     
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
  {src/adm/template/snd-list.i "ttEstMisc"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetCalcDesc B-table-Win 
FUNCTION fGetCalcDesc RETURNS CHARACTER
  ( ipcCalcID AS CHARACTER, iplIsCategory AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE BUFFER bf-estCostGroupSystem      FOR estCostGroupSystem.
    DEFINE BUFFER bf-estCostGroupLevelSystem FOR estCostGroupLevelSystem.
    DEFINE BUFFER bf-estCostCategorySystem   FOR estCostCategorySystem.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    IF AVAILABLE ttEstMisc THEN DO:
        IF iplIsCategory THEN DO:
            FIND FIRST bf-estCostCategorySystem NO-LOCK
                 WHERE bf-estCostCategorySystem.estCostCategoryID EQ ipcCalcID
                 NO-ERROR.
            IF AVAILABLE bf-estCostCategorySystem THEN
                RETURN bf-estCostCategorySystem.estCostCategoryDesc.                
        END.
        ELSE DO:
            IF ttEstMisc.estCostCalcBy EQ "Group" THEN DO:
                FIND FIRST bf-estCostGroupSystem NO-LOCK
                     WHERE bf-estCostGroupSystem.estCostGroupID EQ ipcCalcID
                     NO-ERROR.
                IF AVAILABLE bf-estCostGroupSystem THEN
                    RETURN bf-estCostGroupSystem.estCostGroupDesc.        
            END.
            ELSE IF ttEstMisc.estCostCalcBy EQ "Level" THEN DO:
                FIND FIRST bf-estCostGroupLevelSystem NO-LOCK
                     WHERE bf-estCostGroupLevelSystem.estCostGroupLevelID EQ INTEGER(ipcCalcID)
                     NO-ERROR.
                IF AVAILABLE bf-estCostGroupLevelSystem THEN
                    RETURN bf-estCostGroupLevelSystem.estCostGroupLevelDesc.        
            END.
            ELSE IF ttEstMisc.estCostCalcBy EQ "Category" THEN DO:
                FIND FIRST bf-estCostCategorySystem NO-LOCK
                     WHERE bf-estCostCategorySystem.estCostCategoryID EQ ipcCalcID
                     NO-ERROR.
                IF AVAILABLE bf-estCostCategorySystem THEN
                    RETURN bf-estCostCategorySystem.estCostCategoryDesc.        
            END.
            ELSE IF ttEstMisc.estCostCalcBy EQ "Custom" THEN DO:
                 DO iCount = 1 TO NUM-ENTRIES(cCalcByCustomList):
                    IF ttEstMisc.estCostCalcSource EQ ENTRY(iCount,cCalcByCustomList) THEN
                    DO:
                        RETURN ENTRY(iCount - 1,cCalcByCustomList).
                    END.                 
                 END.
                                
            END.
        END.
    END.
    
    RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

