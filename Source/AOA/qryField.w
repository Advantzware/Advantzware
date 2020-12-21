&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: AOA/qryField.w

  Description: Indexes in Query Design

  Input Parameters: Selected Query Object, Table List

  Output Parameters: Cancel

  Author: Ron Stark

  Created: 10.22.2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER iphWidget    AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplCancel    AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE rRowID AS ROWID NO-UNDO.

{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD tableDB    AS CHARACTER FORMAT "x(10)" LABEL "Database"
    FIELD tableName  AS CHARACTER FORMAT "x(30)" LABEL "Table"
    FIELD fieldName  AS CHARACTER FORMAT "x(30)" LABEL "Field"
    FIELD fieldLabel AS CHARACTER FORMAT "X(50)" LABEL "Label"
    FIELD dataType   AS CHARACTER FORMAT "x(9)"  LABEL "Data Type"
        INDEX ttTable IS PRIMARY tableDB tableName
        INDEX ttTableDB tableDB
        INDEX ttTableName tableName
        INDEX ttFieldName fieldName
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTable

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttTable.tableName ttTable.fieldName ttTable.fieldLabel ttTable.dataType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttTable WHERE (tableMatches EQ NO AND (ttTable.tableName BEGINS tableSearch OR ttTable.fieldName BEGINS tableSearch OR ttTable.fieldLabel BEGINS tableSearch)) OR (tableMatches EQ YES AND (ttTable.tableName MATCHES "*" + tableSearch + "*" OR ttTable.fieldName MATCHES "*" + tableSearch + "*" OR ttTable.fieldLabel MATCHES "*" + tableSearch + "*"))  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttTable WHERE (tableMatches EQ NO AND (ttTable.tableName BEGINS tableSearch OR ttTable.fieldName BEGINS tableSearch OR ttTable.fieldLabel BEGINS tableSearch)) OR (tableMatches EQ YES AND (ttTable.tableName MATCHES "*" + tableSearch + "*" OR ttTable.fieldName MATCHES "*" + tableSearch + "*" OR ttTable.fieldLabel MATCHES "*" + tableSearch + "*"))  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttTable


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOK tableSearch tableMatches BROWSE-1 ~
btnCancel 
&Scoped-Define DISPLAYED-OBJECTS tableSearch tableMatches 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 RECT-TABLE tableSearch tableMatches 
&Scoped-define List-2 RECT-TABLE tableSearch tableMatches 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.png":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE tableSearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 17.8 BY 2.38.

DEFINE RECTANGLE RECT-TABLE
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 117 BY 1.43.

DEFINE VARIABLE tableMatches AS LOGICAL INITIAL yes 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttTable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      ttTable.tableName LABEL-BGCOLOR 22
ttTable.fieldName LABEL-BGCOLOR 22
ttTable.fieldLabel LABEL-BGCOLOR 22
ttTable.dataType
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 128 BY 24.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnOK AT ROW 27.43 COL 113 WIDGET-ID 6
     tableSearch AT ROW 1.48 COL 3 WIDGET-ID 62
     tableMatches AT ROW 1.48 COL 107 HELP
          "Select for Table Search Matches" WIDGET-ID 40
     BROWSE-1 AT ROW 2.67 COL 2 WIDGET-ID 200
     btnCancel AT ROW 27.43 COL 121 WIDGET-ID 4
     RECT-1 AT ROW 27.19 COL 112 WIDGET-ID 2
     RECT-TABLE AT ROW 1.24 COL 2 WIDGET-ID 46
     SPACE(10.99) SKIP(26.89)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 
         TITLE "Fields" WIDGET-ID 100.


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
/* BROWSE-TAB BROWSE-1 tableMatches Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-TABLE IN FRAME Dialog-Frame
   NO-ENABLE 1 2                                                        */
ASSIGN 
       RECT-TABLE:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tableMatches IN FRAME Dialog-Frame
   1 2                                                                  */
ASSIGN 
       tableMatches:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN tableSearch IN FRAME Dialog-Frame
   ALIGN-L 1 2                                                          */
ASSIGN 
       tableSearch:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTable
WHERE (tableMatches EQ NO
AND (ttTable.tableName BEGINS tableSearch
OR ttTable.fieldName BEGINS tableSearch
OR ttTable.fieldLabel BEGINS tableSearch))
OR (tableMatches EQ YES
AND (ttTable.tableName MATCHES "*" + tableSearch + "*"
OR ttTable.fieldName MATCHES "*" + tableSearch + "*"
OR ttTable.fieldLabel MATCHES "*" + tableSearch + "*"))
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ","
     _Where[1]         = "dynSubjectParamSet.subjectID EQ ipiSubjectID"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Fields */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
    APPLY "CHOOSE":U TO btnOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON START-SEARCH OF BROWSE-1 IN FRAME Dialog-Frame
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    oplCancel = YES.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN
        iphWidget:SCREEN-VALUE = ttTable.tableName + "." + ttTable.fieldName
        ENTRY(2,iphWidget:PRIVATE-DATA,"|") = ttTable.fieldLabel + "|" + ttTable.dataType
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tableMatches
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableMatches Dialog-Frame
ON VALUE-CHANGED OF tableMatches IN FRAME Dialog-Frame /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tableSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableSearch Dialog-Frame
ON VALUE-CHANGED OF tableSearch IN FRAME Dialog-Frame /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{methods/template/brwcustom.i}

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pGetFieldNames.
  RUN enable_UI.
  FIND FIRST ttTable
       WHERE ttTable.tableName EQ ENTRY(1,iphWidget:SCREEN-VALUE,".")
         AND ttTable.fieldName EQ ENTRY(2,iphWidget:SCREEN-VALUE,".")
       NO-ERROR.
  IF AVAILABLE ttTable THEN DO:
      rRowID = ROWID(ttTable).
      REPOSITION {&BROWSE-NAME} TO ROWID rRowID.
  END. /* if avail */
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

{methods/sortByProc.i "pByfieldLabel" "ttTable.fieldLabel"}
{methods/sortByProc.i "pByFieldName" "ttTable.fieldName"}
{methods/sortByProc.i "pByTableName" "ttTable.tableName"}

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
  DISPLAY tableSearch tableMatches 
      WITH FRAME Dialog-Frame.
  ENABLE btnOK tableSearch tableMatches BROWSE-1 btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFieldNames Dialog-Frame 
PROCEDURE pGetFieldNames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDataType   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableDB    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttTable.
    DO idx = 1 TO NUM-ENTRIES(ipcTableList):
        ASSIGN
            cTableDB   = ENTRY(1,ENTRY(idx,ipcTableList),".")
            cTableName = ENTRY(2,ENTRY(idx,ipcTableList),".")
            .
        CREATE ALIAS "dictdb" FOR DATABASE VALUE(cTableDB).
        RUN nosweat/fld_list.p (cTableName, OUTPUT cFieldList).
        DO jdx = 1 TO NUM-ENTRIES(cFieldList):
            RUN nosweat/fld_lbls.p (cTableName, ENTRY(jdx,cFieldList), OUTPUT cFieldLabel).
            RUN nosweat/get_type.p (cTableName, ENTRY(jdx,cFieldList), OUTPUT cDataType).
            IF cDataType EQ "String" THEN cDataType = "character".
            IF cFieldLabel EQ "" OR cFieldLabel EQ ? THEN
            cFieldLabel = ENTRY(jdx,cFieldList).
            CREATE ttTable.
            ASSIGN
                ttTable.tableDB    = cTableDB
                ttTable.tableName  = cTableName
                ttTable.fieldName  = ENTRY(jdx,cFieldList)
                ttTable.fieldLabel = cFieldLabel
                ttTable.dataType   = cDataType
                .
        END. /* do jdx */
    END. /* do idx */
    {&OPEN-QUERY-{&BROWSE-NAME}}
    FIND FIRST ttTable.
    APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.

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
        WHEN "fieldLabel" THEN
        RUN pByFieldLabel.
        WHEN "fieldName" THEN
        RUN pByFieldName.
        WHEN "tableName" THEN
        RUN pByTableName.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    {AOA/includes/pReopenBrowse.i}
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

