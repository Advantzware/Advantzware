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

  File: browsers/materialType.w

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
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}
{methods/template/brwcustomdef.i}
{methods/defines/sortByDefs.i}

&SCOPED-DEFINE SORTBY-PHRASE BY materialType.materialType
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMaterialType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalculationType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAutoIssue       AS LOGICAL   NO-UNDO INITIAL ?.
DEFINE VARIABLE lMoveColumn      AS LOGICAL   NO-UNDO INITIAL TRUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME materialType

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES materialType

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE materialType                                  */
&Scoped-define FIELDS-IN-QUERY-materialType materialType.materialType ~
materialType.materialDescription materialType.calculationType ~
materialType.autoIssue materialType.consumedByDept materialType.materialTypeGroup
&Scoped-define ENABLED-FIELDS-IN-QUERY-materialType 
&Scoped-define QUERY-STRING-materialType FOR EACH materialType WHERE ~{&KEY-PHRASE} ~
      AND materialType.company = cCompany  ~
AND materialType.materialType BEGINS cMaterialType  ~
AND (ASI.materialType.calculationType EQ cCalculationType OR cCalculationType EQ "") ~
AND (ASI.materialType.autoIssue EQ lAutoIssue OR lAutoIssue EQ ?) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-materialType OPEN QUERY materialType FOR EACH materialType WHERE ~{&KEY-PHRASE} ~
      AND materialType.company = cCompany  ~
AND materialType.materialType BEGINS cMaterialType  ~
AND (ASI.materialType.calculationType EQ cCalculationType OR cCalculationType EQ "") ~
AND (ASI.materialType.autoIssue EQ lAutoIssue OR lAutoIssue EQ ?) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-materialType materialType
&Scoped-define FIRST-TABLE-IN-QUERY-materialType materialType


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 btSearch fiMaterialType ~
cbCalculationType cbAutoIssue materialType 
&Scoped-Define DISPLAYED-OBJECTS fiMaterialType cbCalculationType ~
cbAutoIssue 

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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btSearch 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U
     LABEL "Search" 
     SIZE 6.6 BY 1.43.

DEFINE VARIABLE cbAutoIssue AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Yes","No" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbCalculationType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 9
     LIST-ITEM-PAIRS "Item 1"," Item 1"
     DROP-DOWN-LIST
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiMaterialType AS CHARACTER FORMAT "X(32)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68.8 BY 2.19
     BGCOLOR 23 FGCOLOR 23 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE .6 BY 1.62
     BGCOLOR 24 FGCOLOR 23 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY materialType FOR 
      materialType SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE materialType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS materialType B-table-Win _STRUCTURED
  QUERY materialType NO-LOCK DISPLAY
      materialType.materialType FORMAT "x(8)":U LABEL-BGCOLOR 22
      materialType.materialDescription FORMAT "x(40)":U LABEL-BGCOLOR 22
      materialType.materialTypeGroup FORMAT "x(8)":U COLUMN-LABEL "System Type" LABEL-BGCOLOR 22
      materialType.consumedByDept FORMAT "x(2)":U COLUMN-LABEL "Department" LABEL-BGCOLOR 22
      materialType.autoIssue FORMAT "Enabled/Disabled":U LABEL-BGCOLOR 22 VIEW-AS TOGGLE-BOX
      materialType.calculationType FORMAT "x(32)":U COLUMN-LABEL "Calculation Method" LABEL-BGCOLOR 22     
      
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 165 BY 15.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btSearch AT ROW 1.57 COL 61.4 WIDGET-ID 16
     fiMaterialType AT ROW 2.1 COL 2.8 NO-LABEL WIDGET-ID 2
     cbCalculationType AT ROW 2.1 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cbAutoIssue AT ROW 2.1 COL 40.4 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     materialType AT ROW 3.57 COL 1
     "Auto Issue" VIEW-AS TEXT
          SIZE 13.6 BY .62 AT ROW 1.38 COL 42.6 WIDGET-ID 10
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "Calculation Type" VIEW-AS TEXT
          SIZE 18.4 BY .62 AT ROW 1.33 COL 21 WIDGET-ID 8
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "Material Type" VIEW-AS TEXT
          SIZE 16.8 BY .62 AT ROW 1.33 COL 3 WIDGET-ID 4
          BGCOLOR 23 FGCOLOR 24 FONT 22
     RECT-1 AT ROW 1.19 COL 1.6 WIDGET-ID 52
     RECT-2 AT ROW 1.48 COL 59.8 WIDGET-ID 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15  WIDGET-ID 100.


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
         HEIGHT             = 18.19
         WIDTH              = 170.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB materialType cbAutoIssue F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiMaterialType IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       materialType:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE materialType
/* Query rebuild information for BROWSE materialType
     _TblList          = "ASI.materialType"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.materialType.company = cCompany 
AND ASI.materialType.materialType BEGINS cMaterialType 
AND (ASI.materialType.calculationType EQ cCalculationType OR cCalculationType EQ """")
AND (ASI.materialType.autoIssue EQ lAutoIssue OR lAutoIssue EQ ?)"
     _FldNameList[1]   > ASI.materialType.materialType
"materialType" ? ? "character" ? ? ? 22 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ASI.materialType.materialDescription
"materialDescription" ? ? "character" ? ? ? 22 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no     
     _FldNameList[3]   > ASI.materialType.materialTypeGroup
"materialTypeGroup" "System Type" "x(8)" "character" ? ? ? 22 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.materialType.consumedByDept
"consumedByDept" "Department" "x(2)" "character" ? ? ? 22 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.materialType.autoIssue
"autoIssue" ? "Enabled/Disabled" "logical" ? ? ? 22 ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[6]   > ASI.materialType.calculationType
"calculationType" "Calculation Method" ? "character" ? ? ? 22 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no     
     _Query            is NOT OPENED
*/  /* BROWSE materialType */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch B-table-Win
ON CHOOSE OF btSearch IN FRAME F-Main /* Search */
DO:
    ASSIGN
        cMaterialType    = fiMaterialType:SCREEN-VALUE
        cCalculationType = cbCalculationType:SCREEN-VALUE
        lAutoIssue       = LOGICAL(cbAutoIssue:SCREEN-VALUE)                           
        NO-ERROR.
    
    IF cCalculationType EQ ? OR cCalculationType EQ "ALL" THEN
        cCalculationType = "".
    
    RUN dispatch (
        INPUT "open-query"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME materialType
&Scoped-define SELF-NAME materialType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL materialType B-table-Win
ON ROW-ENTRY OF materialType IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL materialType B-table-Win
ON ROW-LEAVE OF materialType IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL materialType B-table-Win
ON START-SEARCH OF materialType IN FRAME F-Main
DO:
    {methods/template/sortindicator.i} 

    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        
        IF cColumnLabel EQ cSaveLabel THEN
            lAscending = NOT lAscending.
        IF VALID-HANDLE(hSaveLabel) THEN
            hSaveLabel:LABEL-BGCOLOR = ?.
    
        ASSIGN
            hColumnLabel               = {&BROWSE-NAME}:CURRENT-COLUMN
            hColumnLabel:LABEL-BGCOLOR = 14
            hSaveLabel                 = hColumnLabel
            cSaveLabel                 = cColumnLabel
            .
        RUN pReopenBrowse.
    END.
        
    {methods/template/sortindicatorend.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL materialType B-table-Win
ON VALUE-CHANGED OF materialType IN FRAME F-Main
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

{sys/inc/f3help.i}
{methods/ctrl-a_browser.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

&SCOPED-DEFINE cellColumnDat materialType.w 
 {methods/browsers/setCellColumns.i}

&Scoped-define sdBrowseName materialType
{methods/sortByProc.i "pByMaterialType" "materialType.materialType"}
{methods/sortByProc.i "pByCalculationType" "materialType.calculationType"}
{methods/sortByProc.i "pByAutoIssue" "materialType.autoIssue"}
{methods/sortByProc.i "pByMaterialDescription" "materialType.materialDescription"}
{methods/sortByProc.i "pByConsumedByDept" "materialType.consumedByDept"}
{methods/sortByProc.i "pByMaterialTypeGroup" "materialType.materialTypeGroup"}

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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
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
            {&BROWSE-NAME}:COLUMN-MOVABLE   = lMoveColumn
            {&BROWSE-NAME}:COLUMN-RESIZABLE = lMoveColumn
            lMoveColumn = NOT lMoveColumn
            .
    END.
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
    DEFINE VARIABLE cCalculationTypeList AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN spGetSessionParam (
        INPUT  "Company",
        OUTPUT cCompany
        ).
        
    RUN Material_GetCalculationTypeList (
        OUTPUT cCalculationTypeList
        ).
        
    cbCalculationType:LIST-ITEM-PAIRS = "ALL,ALL," + cCalculationTypeList.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse B-table-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "materialType" THEN
            RUN pByMaterialType.
        WHEN "calculationType" THEN
            RUN pByCalculationType.
        WHEN "autoIssue" THEN
            RUN pByAutoIssue.
        WHEN "materialDescription" THEN
            RUN pByMaterialDescription. 
        WHEN "consumedByDept" THEN
            RUN pByConsumedByDept.
        WHEN "materialTypeGroup" THEN
            RUN pByMaterialTypeGroup.    
    END CASE.
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
  {src/adm/template/snd-list.i "materialType"}

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

