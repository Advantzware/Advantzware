&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: dynPageParam.w

  Description: 

  Input Parameters: Program Name, Page Number and Subject ID

  Output Parameters: <none>

  Author: Ron Stark

  Created: 11.5.2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&if defined(uib_is_running) EQ 0 &then
DEFINE INPUT PARAMETER ipcPrgmName  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiPageNo    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiSubjectID AS INTEGER   NO-UNDO.
&else
DEFINE VARIABLE ipcPrgmName  AS CHARACTER NO-UNDO INIT "".
DEFINE VARIABLE ipiPageNo    AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE ipiSubjectID AS INTEGER   NO-UNDO INIT 70.
&endif

/* Local Variable Definitions ---                                       */

{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD tableName LIKE dynSubjectTable.tableName
        INDEX ttTable IS PRIMARY tableName
        .
DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD tableName AS CHARACTER
    FIELD fieldName AS CHARACTER FORMAT "x(40)" LABEL "Field Name"
        INDEX ttField IS PRIMARY tableName fieldName
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME dynParam

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dynSubjectParamSet dynParamSetDtl ttField ~
dynPrgrmsPage ttTable

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE dynParam                                      */
&Scoped-define FIELDS-IN-QUERY-dynParam dynParamSetDtl.paramLabel ~
dynParamSetDtl.paramName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-dynParam 
&Scoped-define QUERY-STRING-dynParam FOR EACH dynSubjectParamSet WHERE ~{&KEY-PHRASE} ~
      AND dynSubjectParamSet.subjectID EQ ipiSubjectID NO-LOCK, ~
      EACH dynParamSetDtl WHERE TRUE /* Join to dynSubjectParamSet incomplete */ ~
      AND dynParamSetDtl.paramSetID EQ dynSubjectParamSet.paramSetID AND ~
dynParamSetDtl.paramLabel NE "" NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-dynParam OPEN QUERY dynParam FOR EACH dynSubjectParamSet WHERE ~{&KEY-PHRASE} ~
      AND dynSubjectParamSet.subjectID EQ ipiSubjectID NO-LOCK, ~
      EACH dynParamSetDtl WHERE TRUE /* Join to dynSubjectParamSet incomplete */ ~
      AND dynParamSetDtl.paramSetID EQ dynSubjectParamSet.paramSetID AND ~
dynParamSetDtl.paramLabel NE "" NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-dynParam dynSubjectParamSet dynParamSetDtl
&Scoped-define FIRST-TABLE-IN-QUERY-dynParam dynSubjectParamSet
&Scoped-define SECOND-TABLE-IN-QUERY-dynParam dynParamSetDtl


/* Definitions for BROWSE fieldsBrowse                                  */
&Scoped-define FIELDS-IN-QUERY-fieldsBrowse ttField.fieldName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-fieldsBrowse   
&Scoped-define SELF-NAME fieldsBrowse
&Scoped-define QUERY-STRING-fieldsBrowse FOR EACH ttField     WHERE ttField.tableName EQ ttTable.tableName
&Scoped-define OPEN-QUERY-fieldsBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttField     WHERE ttField.tableName EQ ttTable.tableName.
&Scoped-define TABLES-IN-QUERY-fieldsBrowse ttField
&Scoped-define FIRST-TABLE-IN-QUERY-fieldsBrowse ttField


/* Definitions for BROWSE pageParam                                     */
&Scoped-define FIELDS-IN-QUERY-pageParam dynPrgrmsPage.paramName ~
dynPrgrmsPage.tableName dynPrgrmsPage.fieldName ~
dynPrgrmsPage.paramInitValue 
&Scoped-define ENABLED-FIELDS-IN-QUERY-pageParam 
&Scoped-define QUERY-STRING-pageParam FOR EACH dynPrgrmsPage ~
      WHERE dynPrgrmsPage.prgmName EQ ipcPrgmName AND ~
ASI.dynPrgrmsPage.pageTab EQ ipiPageNo AND ~
ASI.dynPrgrmsPage.subjectID EQ ipiSubjectID NO-LOCK ~
    BY dynPrgrmsPage.paramName INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-pageParam OPEN QUERY pageParam FOR EACH dynPrgrmsPage ~
      WHERE dynPrgrmsPage.prgmName EQ ipcPrgmName AND ~
ASI.dynPrgrmsPage.pageTab EQ ipiPageNo AND ~
ASI.dynPrgrmsPage.subjectID EQ ipiSubjectID NO-LOCK ~
    BY dynPrgrmsPage.paramName INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-pageParam dynPrgrmsPage
&Scoped-define FIRST-TABLE-IN-QUERY-pageParam dynPrgrmsPage


/* Definitions for BROWSE tableBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-tableBrowse ttTable.tableName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-tableBrowse   
&Scoped-define SELF-NAME tableBrowse
&Scoped-define QUERY-STRING-tableBrowse FOR EACH ttTable INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-tableBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttTable INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-tableBrowse ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-tableBrowse ttTable


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-dynParam}~
    ~{&OPEN-QUERY-fieldsBrowse}~
    ~{&OPEN-QUERY-pageParam}~
    ~{&OPEN-QUERY-tableBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS pageParam dynParam tableBrowse fieldsBrowse ~
initValue 
&Scoped-Define DISPLAYED-OBJECTS initValue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE VARIABLE initValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Init Value" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY dynParam FOR 
      dynSubjectParamSet, 
      dynParamSetDtl SCROLLING.

DEFINE QUERY fieldsBrowse FOR 
      ttField SCROLLING.

DEFINE QUERY pageParam FOR 
      dynPrgrmsPage SCROLLING.

DEFINE QUERY tableBrowse FOR 
      ttTable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE dynParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS dynParam Dialog-Frame _STRUCTURED
  QUERY dynParam NO-LOCK DISPLAY
      dynParamSetDtl.paramLabel FORMAT "x(40)":U LABEL-BGCOLOR 14
      dynParamSetDtl.paramName FORMAT "x(20)":U WIDTH 41.6 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 19.05
         TITLE "Double-Click to ADD Parameter".

DEFINE BROWSE fieldsBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS fieldsBrowse Dialog-Frame _FREEFORM
  QUERY fieldsBrowse DISPLAY
      ttField.fieldName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 16.91
         TITLE "Double-Click to UPDATE Field".

DEFINE BROWSE pageParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS pageParam Dialog-Frame _STRUCTURED
  QUERY pageParam NO-LOCK DISPLAY
      dynPrgrmsPage.paramName FORMAT "x(20)":U
      dynPrgrmsPage.tableName FORMAT "x(20)":U
      dynPrgrmsPage.fieldName FORMAT "x(40)":U
      dynPrgrmsPage.paramInitValue FORMAT "x(30)":U WIDTH 71.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 159 BY 9.05
         TITLE "Double-Click to DELETE".

DEFINE BROWSE tableBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS tableBrowse Dialog-Frame _FREEFORM
  QUERY tableBrowse NO-LOCK DISPLAY
      ttTable.tableName FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 25 BY 16.91
         TITLE "Tables".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     pageParam AT ROW 1.24 COL 2 WIDGET-ID 200
     dynParam AT ROW 10.52 COL 2 WIDGET-ID 300
     tableBrowse AT ROW 10.52 COL 90 WIDGET-ID 400
     fieldsBrowse AT ROW 10.52 COL 116 WIDGET-ID 500
     initValue AT ROW 27.67 COL 99 COLON-ALIGNED WIDGET-ID 8
     "ENTER or TAB to Update Initial Value" VIEW-AS TEXT
          SIZE 37 BY .62 AT ROW 28.86 COL 112 WIDGET-ID 10
     SPACE(12.00) SKIP(0.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 
         TITLE BGCOLOR 15 FGCOLOR 1 "Program Master Dynamic Subject Initialize Parameters" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB pageParam TEXT-1 Dialog-Frame */
/* BROWSE-TAB dynParam pageParam Dialog-Frame */
/* BROWSE-TAB tableBrowse dynParam Dialog-Frame */
/* BROWSE-TAB fieldsBrowse tableBrowse Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       dynParam:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE dynParam
/* Query rebuild information for BROWSE dynParam
     _TblList          = "ASI.dynSubjectParamSet,ASI.dynParamSetDtl WHERE ASI.dynSubjectParamSet ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ","
     _Where[1]         = "dynSubjectParamSet.subjectID EQ ipiSubjectID"
     _Where[2]         = "dynParamSetDtl.paramSetID EQ dynSubjectParamSet.paramSetID AND
dynParamSetDtl.paramLabel NE """""
     _FldNameList[1]   > ASI.dynParamSetDtl.paramLabel
"dynParamSetDtl.paramLabel" ? ? "character" ? ? ? 22 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.dynParamSetDtl.paramName
"dynParamSetDtl.paramName" ? ? "character" ? ? ? 22 ? ? no ? no no "41.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE dynParam */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE fieldsBrowse
/* Query rebuild information for BROWSE fieldsBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttField
    WHERE ttField.tableName EQ ttTable.tableName.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE fieldsBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE pageParam
/* Query rebuild information for BROWSE pageParam
     _TblList          = "ASI.dynPrgrmsPage"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ASI.dynPrgrmsPage.paramName|yes"
     _Where[1]         = "ASI.dynPrgrmsPage.prgmName EQ ipcPrgmName AND
ASI.dynPrgrmsPage.pageTab EQ ipiPageNo AND
ASI.dynPrgrmsPage.subjectID EQ ipiSubjectID"
     _FldNameList[1]   = ASI.dynPrgrmsPage.paramName
     _FldNameList[2]   = ASI.dynPrgrmsPage.tableName
     _FldNameList[3]   = ASI.dynPrgrmsPage.fieldName
     _FldNameList[4]   > ASI.dynPrgrmsPage.paramInitValue
"dynPrgrmsPage.paramInitValue" ? ? "character" ? ? ? ? ? ? no ? no no "71.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE pageParam */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE tableBrowse
/* Query rebuild information for BROWSE tableBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTable INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ASI.dynSubjectTable.tableName|yes"
     _Where[1]         = "ASI.dynSubjectTable.subjectID EQ ipiSubjectID"
     _Query            is OPENED
*/  /* BROWSE tableBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Program Master Dynamic Subject Initialize Parameters */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dynParam
&Scoped-define SELF-NAME dynParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParam Dialog-Frame
ON DEFAULT-ACTION OF dynParam IN FRAME Dialog-Frame /* Double-Click to ADD Parameter */
DO:
    RUN pAddParameter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParam Dialog-Frame
ON START-SEARCH OF dynParam IN FRAME Dialog-Frame /* Double-Click to ADD Parameter */
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME fieldsBrowse
&Scoped-define SELF-NAME fieldsBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fieldsBrowse Dialog-Frame
ON DEFAULT-ACTION OF fieldsBrowse IN FRAME Dialog-Frame /* Double-Click to UPDATE Field */
DO:
    RUN pUpdateField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME initValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL initValue Dialog-Frame
ON LEAVE OF initValue IN FRAME Dialog-Frame /* Init Value */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pUpdateInitValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL initValue Dialog-Frame
ON RETURN OF initValue IN FRAME Dialog-Frame /* Init Value */
DO:
    APPLY "LEAVE":U TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME pageParam
&Scoped-define SELF-NAME pageParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pageParam Dialog-Frame
ON DEFAULT-ACTION OF pageParam IN FRAME Dialog-Frame /* Double-Click to DELETE */
DO:
    RUN pDeleteParameter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pageParam Dialog-Frame
ON DELETE-CHARACTER OF pageParam IN FRAME Dialog-Frame /* Double-Click to DELETE */
DO:
    APPLY "DEFAULT-ACTION":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pageParam Dialog-Frame
ON START-SEARCH OF pageParam IN FRAME Dialog-Frame /* Double-Click to DELETE */
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pageParam Dialog-Frame
ON VALUE-CHANGED OF pageParam IN FRAME Dialog-Frame /* Double-Click to DELETE */
DO:
    ASSIGN
        initValue:SCREEN-VALUE = dynPrgrmsPage.paramInitValue
        initValue
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME tableBrowse
&Scoped-define SELF-NAME tableBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableBrowse Dialog-Frame
ON VALUE-CHANGED OF tableBrowse IN FRAME Dialog-Frame /* Tables */
DO:
    {&OPEN-QUERY-fieldsBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dynParam
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

&Scoped-define sdBrowseName pageParam
{methods/template/brwcustom2.i 1}
&Scoped-define sdBrowseName dynParam
{methods/template/brwcustom2.i 2}
&Scoped-define sdBrowseName tableBrowse
{methods/template/brwcustom2.i 3}
&Scoped-define sdBrowseName fieldsBrowse
{methods/template/brwcustom2.i 4}

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pGetFields.
  RUN enable_UI.
  FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE
                       + (IF ipcPrgmName EQ "" THEN "" ELSE " - Program: " + ipcPrgmName)
                       + (IF ipiPageNo EQ 0 THEN "" ELSE " - Page: " + STRING(ipiPageNo))
                       + " - Subject ID: " + STRING(ipiSubjectID)
                       .
  IF AVAILABLE dynPrgrmsPage THEN
  APPLY "VALUE-CHANGED":U TO BROWSE pageParam.
  IF AVAILABLE dynSubjectTable THEN
  APPLY "VALUE-CHANGED":U TO BROWSE tableBrowse.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

&Scoped-define sdBrowseName dynParam
{methods/sortByProc.i "pByLabel" "dynParamSetDtl.paramLabel"}
{methods/sortByProc.i "pByName" "dynParamSetDtl.paramName"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY initValue 
      WITH FRAME Dialog-Frame.
  ENABLE pageParam dynParam tableBrowse fieldsBrowse initValue 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddParameter Dialog-Frame 
PROCEDURE pAddParameter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.

    IF CAN-FIND(FIRST dynPrgrmsPage
                WHERE dynPrgrmsPage.prgmName   EQ ipcPrgmName
                  AND dynPrgrmsPage.pageTab    EQ ipiPageNo
                  AND dynPrgrmsPage.subjectID  EQ ipiSubjectID
                  AND dynPrgrmsPage.paramSetID EQ dynParamSetDtl.paramSetID
                  AND dynPrgrmsPage.paramName  EQ dynParamSetDtl.paramName) THEN
    RETURN.
    CREATE dynPrgrmsPage.
    ASSIGN
        dynPrgrmsPage.prgmName   = ipcPrgmName
        dynPrgrmsPage.pageTab    = ipiPageNo
        dynPrgrmsPage.subjectID  = ipiSubjectID
        dynPrgrmsPage.paramSetID = dynParamSetDtl.paramSetID
        dynPrgrmsPage.paramName  = dynParamSetDtl.paramName
        rRowID                   = ROWID(dynPrgrmsPage)
        .
    {&OPEN-QUERY-pageParam}
    REPOSITION pageParam TO ROWID rRowID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteParameter Dialog-Frame 
PROCEDURE pDeleteParameter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE
        "Delete Page Parameter?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE lDelete AS LOGICAL.
    IF lDelete THEN DO TRANSACTION:
        FIND CURRENT dynPrgrmsPage EXCLUSIVE-LOCK.
        DELETE dynPrgrmsPage.
        BROWSE pageParam:REFRESH().
    END. /* if delete */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFields Dialog-Frame 
PROCEDURE pGetFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF ipcPrgmName EQ "" AND ipiPageNo EQ 0 THEN
    FOR EACH ASI._file NO-LOCK
        WHERE ASI._file._Tbl-type EQ "T"
        :
        CREATE ttTable.
        ttTable.tableName = ASI._file._file-name.
        FOR EACH ASI._field OF ASI._file NO-LOCK
            WHERE ASI._field._extent EQ 0
            :
            CREATE ttField.
            ASSIGN
                ttField.tableName = ASI._file._file-name
                ttField.fieldName = ASI._field._field-name
                .
        END. /* each _field */
    END. /* each _file */
    ELSE
    FOR EACH dynSubjectTable NO-LOCK
        WHERE dynSubjectTable.subjectID EQ ipiSubjectID,
        FIRST ASI._file NO-LOCK
        WHERE ASI._file._file-name EQ dynSubjectTable.tableName
        :
        CREATE ttTable.
        ttTable.tableName = dynSubjectTable.tableName.
        FOR EACH ASI._field OF ASI._file NO-LOCK
            WHERE ASI._field._extent EQ 0
            :
            CREATE ttField.
            ASSIGN
                ttField.tableName = dynSubjectTable.tableName
                ttField.fieldName = ASI._field._field-name
                .
        END. /* each _field */
    END. /* each dynsubjecttable */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse Dialog-Frame 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    CASE cColumnLabel:
        WHEN "paramLabel" THEN
        RUN pByLabel.
        WHEN "paramName" THEN
        RUN pByName.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    {AOA/includes/pReopenBrowse.i}
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateField Dialog-Frame 
PROCEDURE pUpdateField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAILABLE dynPrgrmsPage THEN
    DO TRANSACTION:
        FIND CURRENT dynPrgrmsPage EXCLUSIVE-LOCK.
        ASSIGN
            dynPrgrmsPage.tableName = ttField.tableName
            dynPrgrmsPage.fieldName = ttField.fieldName
            dynPrgrmsPage.paramInitValue = ""
            initValue = ""
            .
        FIND CURRENT dynPrgrmsPage NO-LOCK.
        BROWSE pageParam:REFRESH().
        DISPLAY initValue WITH FRAME {&FRAME-NAME}.
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateInitValue Dialog-Frame 
PROCEDURE pUpdateInitValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAILABLE dynPrgrmsPage THEN
    DO TRANSACTION:
        FIND CURRENT dynPrgrmsPage EXCLUSIVE-LOCK.
        ASSIGN
            dynPrgrmsPage.tableName = ""
            dynPrgrmsPage.fieldName = ""
            dynPrgrmsPage.paramInitValue = initValue
            .
        FIND CURRENT dynPrgrmsPage NO-LOCK.
        BROWSE pageParam:REFRESH().
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

