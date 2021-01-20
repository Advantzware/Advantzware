&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: dynSubTableField.w

  Description: Dynamic Subject Table / Field Lookup

  Input Parameters: <none>

  Output Parameters: Dynamic Subject Title

  Author: Ron Stark

  Created: 1.17.2021
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE OUTPUT PARAMETER opcSubjectTitle AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE opcSubjectTitle AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttTableField NO-UNDO
    FIELD subjectTitle LIKE dynSubject.subjectTitle
    FIELD subjectID    LIKE dynSubject.subjectID
    FIELD tableName    LIKE dynSubjectColumn.tableName
    FIELD fieldName    LIKE dynSubjectColumn.fieldName
    FIELD allData        AS CHARACTER
    .

{methods/defines/sortByDefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME lookupBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTableField

/* Definitions for BROWSE lookupBrowse                                  */
&Scoped-define FIELDS-IN-QUERY-lookupBrowse ttTableField.subjectTitle ttTableField.tableName ttTableField.fieldName ttTableField.subjectID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-lookupBrowse   
&Scoped-define SELF-NAME lookupBrowse
&Scoped-define QUERY-STRING-lookupBrowse FOR EACH ttTableField WHERE ttTableField.allData MATCHES "*" + SEARCHbAR + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-lookupBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttTableField WHERE ttTableField.allData MATCHES "*" + SEARCHbAR + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-lookupBrowse ttTableField
&Scoped-define FIRST-TABLE-IN-QUERY-lookupBrowse ttTableField


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-lookupBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel btnOK searchBar lookupBrowse ~
btnSearch 
&Scoped-Define DISPLAYED-OBJECTS searchBar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.png":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnSearch 
     IMAGE-UP FILE "Graphics/16x16/search.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Search" 
     SIZE 5 BY 1.19 TOOLTIP "Search".

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 48.6 BY 1 TOOLTIP "Search Bar" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY lookupBrowse FOR 
      ttTableField SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE lookupBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS lookupBrowse Dialog-Frame _FREEFORM
  QUERY lookupBrowse DISPLAY
      ttTableField.subjectTitle LABEL-BGCOLOR 14
ttTableField.tableName LABEL-BGCOLOR 14
ttTableField.fieldName LABEL-BGCOLOR 14
ttTableField.subjectID LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 26.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnCancel AT ROW 1 COL 80 WIDGET-ID 290
     btnOK AT ROW 1 COL 72 WIDGET-ID 292
     searchBar AT ROW 1.33 COL 11 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     lookupBrowse AT ROW 2.91 COL 1 WIDGET-ID 200
     btnSearch AT ROW 1.24 COL 61.6 HELP
          "Search" WIDGET-ID 288
     SPACE(21.39) SKIP(27.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Dynamic Subject Table / Field Lookup" WIDGET-ID 100.


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
/* BROWSE-TAB lookupBrowse searchBar Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       lookupBrowse:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE lookupBrowse
/* Query rebuild information for BROWSE lookupBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTableField
WHERE ttTableField.allData MATCHES "*" + SEARCHbAR + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE lookupBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Dynamic Subject Table / Field Lookup */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    opcSubjectTitle = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    opcSubjectTitle = ttTableField.subjectTitle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch Dialog-Frame
ON CHOOSE OF btnSearch IN FRAME Dialog-Frame /* Search */
DO:
    ASSIGN searchBar.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME lookupBrowse
&Scoped-define SELF-NAME lookupBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lookupBrowse Dialog-Frame
ON DEFAULT-ACTION OF lookupBrowse IN FRAME Dialog-Frame /* Dynamic Subject Table / Field */
DO:
    APPLY "CHOOSE":U TO btnOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lookupBrowse Dialog-Frame
ON START-SEARCH OF lookupBrowse IN FRAME Dialog-Frame /* Dynamic Subject Table / Field */
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchBar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar Dialog-Frame
ON RETURN OF searchBar IN FRAME Dialog-Frame /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{methods/template/brwcustom2.i}

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pGetDynColumns.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

{methods/sortByProc.i "pBySubjectTitle" "ttTableField.subjectTitle"}
{methods/sortByProc.i "pByTableName" "ttTableField.tableName"}
{methods/sortByProc.i "pByFieldName" "ttTableField.fieldName"}
{methods/sortByProc.i "pBySubjectID" "ttTableField.subjectID"}

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
  DISPLAY searchBar 
      WITH FRAME Dialog-Frame.
  ENABLE btnCancel btnOK searchBar lookupBrowse btnSearch 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDynColumns Dialog-Frame 
PROCEDURE pGetDynColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH dynSubjectColumn NO-LOCK
        WHERE dynSubjectColumn.subjectID GT 0
          AND dynSubjectColumn.tableName GT ""
          AND dynSubjectColumn.tableName LT "_"
          AND dynSubjectColumn.fieldName GT "",
        FIRST dynSubject NO-LOCK
        WHERE dynSubject.subjectID EQ dynSubjectColumn.subjectID
          AND dynSubject.isActive  EQ YES
          AND dynSubject.isLookup  EQ NO
        :
        IF NUM-ENTRIES(dynSubjectColumn.fieldName,".") LT 2 THEN NEXT.
        CREATE ttTableField.
        ASSIGN
            ttTableField.subjectTitle = dynSubject.subjectTitle
            ttTableField.tableName    = dynSubjectColumn.tableName
            ttTableField.fieldName    = ENTRY(2,dynSubjectColumn.fieldName,".")
            ttTableField.subjectID    = dynSubjectColumn.subjectID
            ttTableField.allData      = ttTableField.subjectTitle + "|"
                                      + ttTableField.tableName + "|"
                                      + ttTableField.fieldName + "|"
                                      + STRING(ttTableField.subjectID)
            .
    END. /* each dynsubject */

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
        WHEN "fieldName" THEN
        RUN pByFieldName.
        WHEN "subjectID" THEN
        RUN pBySubjectID.
        WHEN "subjectTitle" THEN
        RUN pBySubjectTitle.
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

