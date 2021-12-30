&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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

  File: browsers/estCostCategory.w

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
DEFINE VARIABLE oSetting AS system.Setting NO-UNDO.
DEFINE VARIABLE cCompany AS CHARACTER      NO-UNDO.

DEFINE VARIABLE hdScopeField1       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdScopeField2       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdScopeField3       AS HANDLE    NO-UNDO.
DEFINE VARIABLE cSaveType           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lHideSearch         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lLoadDataFromTT     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHideSettingFilter  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHideScopeFilter    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFilterType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lShowAdvancedFilter AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCOLUMN-NAME        AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cGlobalSearch  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSettingTypeID AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSettingName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCategory      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSettingType   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScope         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScopeField1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScopeField2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScopeField3   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProgram       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lStatus        AS LOGICAL   NO-UNDO.     

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

{est/ttEstSysConfig.i}

&SCOP adm-attribute-dlg browsers\setting-support.w

&IF DEFINED(adm-attribute-list) = 0 &THEN
&SCOP adm-attribute-list SAVE-TYPE,BROWSE-COLUMNS,BROWSE-COLUMNS-DISPLAY,HIDE-SEARCH,HIDE-SETTING-FILTER,HIDE-SCOPE-FILTER,SETTING-FILTER-TYPE,LOAD-DATA-FROM-TT,COLUMN-NAME
&ENDIF

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

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
&Scoped-define INTERNAL-TABLES ttEstCostCategory ttEstCostGroup ~
ttEstCostGroupLevel

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ttEstCostCategory.estCostCategoryID ttEstCostCategory.costCategoryLabel ttEstCostGroup.costGroupLabel ttEstCostGroupLevel.estCostGroupLevelDesc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH ttEstCostCategory, ~
       FIRST ttEstCostGroup WHERE ttEstCostGroup.estCostGroupID = ttEstCostCategory.estCostGroupID, ~
       FIRST ttEstCostGroupLevel WHERE ttEstCostGroupLevel.estCostGroupLevelID = ttEstCostGroup.estCostGroupLevelID
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH ttEstCostCategory, ~
       FIRST ttEstCostGroup WHERE ttEstCostGroup.estCostGroupID = ttEstCostCategory.estCostGroupID, ~
       FIRST ttEstCostGroupLevel WHERE ttEstCostGroupLevel.estCostGroupLevelID = ttEstCostGroup.estCostGroupLevelID .
&Scoped-define TABLES-IN-QUERY-br_table ttEstCostCategory ttEstCostGroup ~
ttEstCostGroupLevel
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ttEstCostCategory
&Scoped-define SECOND-TABLE-IN-QUERY-br_table ttEstCostGroup
&Scoped-define THIRD-TABLE-IN-QUERY-br_table ttEstCostGroupLevel


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* SETTING-FILTER,SCOPE-FILTER,List-3,List-4,List-5,List-6              */

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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      ttEstCostCategory, 
      ttEstCostGroup, 
      ttEstCostGroupLevel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      ttEstCostCategory.estCostCategoryID  FORMAT "X(50)" WIDTH 25
      ttEstCostCategory.costCategoryLabel  FORMAT "X(50)" WIDTH 80
      ttEstCostGroup.costGroupLabel        FORMAT "X(50)" WIDTH 25
      ttEstCostGroupLevel.estCostGroupLevelDesc  FORMAT "X(50)" WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 138 BY 27.38
         BGCOLOR 15 FGCOLOR 0 FONT 6 ROW-HEIGHT-CHARS .75 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 6 WIDGET-ID 100.


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
         HEIGHT             = 27.86
         WIDTH              = 138.
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
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttEstCostCategory,
FIRST ttEstCostGroup WHERE ttEstCostGroup.estCostGroupID = ttEstCostCategory.estCostGroupID,
FIRST ttEstCostGroupLevel WHERE ttEstCostGroupLevel.estCostGroupLevelID = ttEstCostGroup.estCostGroupLevelID
.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
  RUN set-attribute-list ("SAVE-TYPE=DATABASE, 
                           BROWSE-COLUMNS=settingName|description|settingValue|scopeTable|scopeField1|scopeField2|scopeField3|inactive|settingUser|programID,
                           BROWSE-COLUMNS-DISPLAY=settingName|description|settingValue|scopeTable|scopeField1|scopeField2|scopeField3|inactive|settingUser|programID,
                           HIDE-SEARCH=FALSE,
                           HIDE-SETTING-FILTER=FALSE,
                           HIDE-SCOPE-FILTER=FALSE,
                           SETTING-FILTER-TYPE=SettingType,
                           LOAD-DATA-FROM-TT=FALSE, 
                           COLUMN-NAME = FALSE"). 
                     
{methods/ctrl-a_browser.i}
{sys/inc/f3help.i}
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableBrowse B-table-Win 
PROCEDURE DisableBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    BROWSE {&BROWSE-NAME}:SENSITIVE = FALSE.
    
    {methods/run_link.i "SEARCH-SOURCE" "DisableAll"}    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayColumns B-table-Win 
PROCEDURE DisplayColumns :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iColumn  AS INTEGER NO-UNDO.
    DEFINE VARIABLE hdBrowse AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hdColumn AS HANDLE  NO-UNDO.
    
    DEFINE VARIABLE cBrowseCols  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dBrowseWidth AS DECIMAL   NO-UNDO.
        
    RUN get-attribute IN THIS-PROCEDURE ('SAVE-TYPE':U).

    cSaveType = RETURN-VALUE.
    IF cSaveType EQ "" OR cSaveType EQ ? THEN
        cSaveType = "DATABASE".

    RUN get-attribute IN THIS-PROCEDURE ('HIDE-SEARCH':U).

    lHideSearch = LOGICAL(RETURN-VALUE).
    IF lHideSearch  EQ ? THEN
        lHideSearch = FALSE.

    RUN get-attribute IN THIS-PROCEDURE ('HIDE-SETTING-FILTER':U).

    lHideSettingFilter = LOGICAL(RETURN-VALUE).
    IF lHideSettingFilter  EQ ? THEN
        lHideSettingFilter = FALSE.

    RUN get-attribute IN THIS-PROCEDURE ('HIDE-SCOPE-FILTER':U).

    lHideScopeFilter = LOGICAL(RETURN-VALUE).
    IF lHideScopeFilter  EQ ? THEN
        lHideScopeFilter = FALSE.
                
    RUN get-attribute IN THIS-PROCEDURE ('LOAD-DATA-FROM-TT':U).

    lLoadDataFromTT = LOGICAL(RETURN-VALUE).
    IF lLoadDataFromTT EQ ? THEN
        lLoadDataFromTT = FALSE.

    RUN get-attribute IN THIS-PROCEDURE ('COLUMN-NAME':U).

    lCOLUMN-NAME = LOGICAL(RETURN-VALUE).
    IF lCOLUMN-NAME EQ ? THEN
        lCOLUMN-NAME = FALSE.
    
    RUN get-attribute IN THIS-PROCEDURE ('SETTING-FILTER-TYPE':U).

    cFilterType = RETURN-VALUE.
    IF cFilterType EQ ? OR cFilterType EQ "" THEN
        cFilterType = "SettingType".
                         
    hdBrowse = BROWSE {&BROWSE-NAME}:HANDLE.

    RUN get-attribute IN THIS-PROCEDURE ('BROWSE-COLUMNS-DISPLAY':U).
    
    cBrowseCols = RETURN-VALUE.

    DO iColumn = 1 TO hdBrowse:NUM-COLUMNS :
        hdColumn = hdBrowse:GET-BROWSE-COLUMN (iColumn).

        hdColumn:VISIBLE = LOOKUP(hdColumn:NAME, cBrowseCols, "|") GT 0.
        
        IF hdColumn:VISIBLE THEN
            dBrowseWidth = dBrowseWidth + hdColumn:WIDTH.

    END.   

    BROWSE {&BROWSE-NAME}:WIDTH = dBrowseWidth + 15 NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableBrowse B-table-Win 
PROCEDURE EnableBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    BROWSE {&BROWSE-NAME}:SENSITIVE = TRUE.
    
    {methods/run_link.i "SEARCH-SOURCE" "EnableAll"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy B-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    IF VALID-OBJECT(oSetting) AND NOT lLoadDataFromTT THEN
        DELETE OBJECT oSetting.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable B-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    //RUN DisplayColumns.
    //RUN pUpdateColumnName.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    iSettingTypeID = 0.
    
    RUN pSearch.
        
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
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
    DEFINE VARIABLE cCategoryTagsList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScopeList        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBrowseColumn     AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    /*
    DO iBrowseColumn = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS :
        IF {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn):NAME = "scopeField1" THEN
            hdScopeField1 = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn).
        ELSE IF {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn):NAME = "scopeField2" THEN
            hdScopeField2 = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn).
        ELSE IF {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn):NAME = "scopeField3" THEN
            hdScopeField3 = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn).            
    END.
    
    IF NOT lLoadDataFromTT THEN
        oSetting = NEW system.Setting().
    ELSE DO:        
        {methods/run_link.i "CONTAINER-SOURCE" "GetSetting" "(OUTPUT oSetting)"}

        IF VALID-OBJECT(oSetting) THEN
            oSetting:GetCurrentSetting(OUTPUT TABLE ttSetting).
        
      
    END.
    
    IF VALID-OBJECT (oSetting) THEN
        ASSIGN
            cCategoryTagsList = "All," + oSetting:GetCategoryTagsList()
            cCategoryTagsList = TRIM(cCategoryTagsList, ",")
            cScopeList        = "All," + oSetting:GetScopeList(TRUE)
            cScopeList        = TRIM(cScopeList, ",")
            .

    IF cFilterType EQ "SettingType" THEN
        {methods/run_link.i "SEARCH-SOURCE" "SetCategoryList" "(INPUT cCategoryTagsList )"}
    
    IF cFilterType EQ "Setting" THEN
        {methods/run_link.i "SEARCH-SOURCE" "SetScopeList" "(INPUT cScopeList)"}
    
    IF lLoadDataFromTT THEN
        {methods/run_link.i "SEARCH-SOURCE" "DisableAdvancedFilter"}
        
    RUN pSearch.
          
    RUN spGetSessionParam (
        INPUT  "Company",
        OUTPUT cCompany
        ). 
        */       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RepositionSetting B-table-Win 
PROCEDURE RepositionSetting :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprittSetting AS ROWID NO-UNDO.
    
    REPOSITION {&BROWSE-NAME} TO ROWID iprittSetting NO-ERROR.

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
  {src/adm/template/snd-list.i "ttEstCostCategory"}
  {src/adm/template/snd-list.i "ttEstCostGroup"}
  {src/adm/template/snd-list.i "ttEstCostGroupLevel"}

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
      
        WHEN "record-update-begin" THEN DO:
            RUN DisableBrowse.
        END.
        WHEN "record-update-end" THEN
            RUN EnableBrowse.
        WHEN "Search" THEN DO:
            RUN pSearch.
        END. 
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

