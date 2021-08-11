&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER p-Parent-Hdl AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE attr-list   AS CHARACTER NO-UNDO.
DEFINE VARIABLE attr-entry  AS CHARACTER NO-UNDO.
DEFINE VARIABLE attr-value  AS CHARACTER NO-UNDO.
DEFINE VARIABLE attr-name   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cntr        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cBrowseCols AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBrowseDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE sts         AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE ttColumn NO-UNDO
       FIELD colNumber AS INTEGER  FORMAT ">>9":U LABEL ""
       FIELD colName   AS CHARACTER FORMAT "X(50)":U LABEL "Column"
       FIELD isActive  AS LOGICAL  FORMAT "yes/no" LABEL "Display?"
       INDEX colNumber colNumber.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttColumn

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttColumn.colNumber ttColumn.colName ttColumn.isActive   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 isActive   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttColumn
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttColumn.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttColumn
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttColumn


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbTempTableSaveOnly tbHideSearch ~
tbLoadDataFromTT BROWSE-2 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tbTempTableSaveOnly tbHideSearch ~
tbLoadDataFromTT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE tbHideSearch AS LOGICAL INITIAL no 
     LABEL "Hide Search" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tbLoadDataFromTT AS LOGICAL INITIAL no 
     LABEL "Load Data From TT" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 TOOLTIP "Load initial data from input setting" NO-UNDO.

DEFINE VARIABLE tbTempTableSaveOnly AS LOGICAL INITIAL no 
     LABEL "Save to Temp-table" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.8 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttColumn SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttColumn.colNumber 
ttColumn.colName
ttColumn.isActive
ENABLE isActive
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86.4 BY 10.57
         FONT 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tbTempTableSaveOnly AT ROW 1.24 COL 2.2 WIDGET-ID 2
     tbHideSearch AT ROW 1.24 COL 39 WIDGET-ID 4
     tbLoadDataFromTT AT ROW 1.95 COL 2.2 WIDGET-ID 6
     BROWSE-2 AT ROW 2.86 COL 1.6 WIDGET-ID 200
     Btn_OK AT ROW 13.81 COL 18
     Btn_Cancel AT ROW 13.81 COL 49
     SPACE(24.19) SKIP(0.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FONT 6
         TITLE "Select Columsn to Display"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
/* BROWSE-TAB BROWSE-2 tbLoadDataFromTT Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttColumn.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select Columsn to Display */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN get-attribute-list IN p-Parent-Hdl (OUTPUT attr-list).     
   DO cntr = 1 TO NUM-ENTRIES(attr-list): 
      attr-entry = ENTRY(cntr, attr-list).
      attr-name = TRIM(SUBSTR(attr-entry, 1, INDEX(attr-entry,"=":U) - 1,
          "CHARACTER":U)).
      attr-value = TRIM(SUBSTR(attr-entry, INDEX(attr-entry,"=":U) + 1, -1,
          "CHARACTER":U)).
      CASE attr-name :
      WHEN "BROWSE-COLUMNS":U THEN
          cBrowseCols = attr-value.
      WHEN "BROWSE-COLUMNS-DISPLAY":U THEN
          cBrowseDisplay = attr-value.
      END CASE.
  END.
  
  DO TRANSACTION cntr = 1 TO NUM-ENTRIES(cBrowseCols,'|':U):
      CREATE ttColumn.
      ASSIGN 
          ttColumn.colNumber = cntr
          ttColumn.colName   = ENTRY(cntr, cBrowseCols, '|')
          ttColumn.isActive  = LOOKUP(ttColumn.colName, cBrowseDisplay, "|") GT 0
          .
  END.
  
  RUN get-attribute IN p-Parent-Hdl ('SAVE-TYPE').     

  tbTempTableSaveOnly = LOGICAL(RETURN-VALUE, "TEMP-TABLE/DATABASE").

  RUN get-attribute IN p-Parent-Hdl ('HIDE-SEARCH').     

  tbHideSearch = LOGICAL(RETURN-VALUE).

  RUN get-attribute IN p-Parent-Hdl ('LOAD-DATA-FROM-TT').     

  tbLoadDatafromTT = LOGICAL(RETURN-VALUE).
    
    
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.

    ASSIGN
        cBrowseCols = ""
        cBrowseDisplay = ""
        .
        
    FOR EACH ttColumn:
        cBrowseCols = cBrowseCols + "|" + ttColumn.colName.
        IF ttColumn.isActive THEN
            cBrowseDisplay = cBrowseDisplay + "|" + ttColumn.colName.
    END. 
    
    cBrowseCols = TRIM(cBrowseCols, "|").
    cBrowseDisplay = TRIM(cBrowseDisplay, "|").

  attr-list = "SAVE-TYPE=" + STRING(tbTempTableSaveOnly:CHECKED, "TEMP-TABLE/DATABASE") + ","
            + "BROWSE-COLUMNS= ":U + cBrowseCols + "," 
            + "BROWSE-COLUMNS-DISPLAY= ":U + cBrowseDisplay + ","
            + "HIDE-SEARCH= ":U + STRING(tbHideSearch:CHECKED, "TRUE/FALSE") + ","
            + "LOAD-DATA-FROM-TT= ":U + STRING(tbLoadDataFromTT:CHECKED, "TRUE/FALSE").

  RUN set-attribute-list IN p-Parent-Hdl (INPUT attr-list).

  RUN DisplayColumns IN p-Parent-Hdl.   
  
END.
RUN disable_UI.

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
  DISPLAY tbTempTableSaveOnly tbHideSearch tbLoadDataFromTT 
      WITH FRAME Dialog-Frame.
  ENABLE tbTempTableSaveOnly tbHideSearch tbLoadDataFromTT BROWSE-2 Btn_OK 
         Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

