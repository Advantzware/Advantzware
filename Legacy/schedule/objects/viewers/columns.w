&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: columns.w

  Description: allows setting resource popup column order

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.10.2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

{{&viewers}/includes/sharedVars.i}
{{&includes}/sharedVars.i}

DEFINE BUFFER bBrowseColumn FOR browseColumn.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME browseCols

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES browseColumn

/* Definitions for BROWSE browseCols                                    */
&Scoped-define FIELDS-IN-QUERY-browseCols browseColumn.colOrder browseColumn.colLabel browseColumn.colName browseColumn.colLocked browseColumn.rptLine browseColumn.rptCol browseColumn.excelCol browseColumn.colHidden   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseCols browseColumn.rptLine   browseColumn.rptCol   browseColumn.excelCol   
&Scoped-define ENABLED-TABLES-IN-QUERY-browseCols browseColumn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-browseCols browseColumn
&Scoped-define SELF-NAME browseCols
&Scoped-define QUERY-STRING-browseCols FOR EACH browseColumn
&Scoped-define OPEN-QUERY-browseCols OPEN QUERY {&SELF-NAME} FOR EACH browseColumn.
&Scoped-define TABLES-IN-QUERY-browseCols browseColumn
&Scoped-define FIRST-TABLE-IN-QUERY-browseCols browseColumn


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-browseCols}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS browseCols btnMoveUp btnMoveDown btnHide ~
EDITOR-1 btnOK btnClose 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose AUTO-END-KEY 
     LABEL "&Close" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnHide 
     LABEL "&Hide/Unhide" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnMoveDown 
     LABEL "Move &Down" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnMoveUp 
     LABEL "Move &Up" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "All Hidden Columns Must Be Sorted To The Bottom" 
     VIEW-AS EDITOR
     SIZE 15 BY 3.57
     BGCOLOR 12 FGCOLOR 15 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseCols FOR 
      browseColumn SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseCols
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseCols Dialog-Frame _FREEFORM
  QUERY browseCols DISPLAY
      browseColumn.colOrder
  browseColumn.colLabel
  browseColumn.colName
  browseColumn.colLocked
  browseColumn.rptLine
  browseColumn.rptCol
  browseColumn.excelCol
  browseColumn.colHidden
  ENABLE
    browseColumn.rptLine
    browseColumn.rptCol
    browseColumn.excelCol
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 22.86 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     browseCols AT ROW 1 COL 1
     btnMoveUp AT ROW 6.95 COL 85
     btnMoveDown AT ROW 8.38 COL 85
     btnHide AT ROW 12.91 COL 85
     EDITOR-1 AT ROW 14.33 COL 85 NO-LABEL
     btnOK AT ROW 21.24 COL 85
     btnClose AT ROW 22.67 COL 85
     SPACE(0.00) SKIP(0.05)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Resource Popup Column Order"
         DEFAULT-BUTTON btnOK CANCEL-BUTTON btnClose.


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
                                                                        */
/* BROWSE-TAB browseCols 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       EDITOR-1:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseCols
/* Query rebuild information for BROWSE browseCols
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH browseColumn.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseCols */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Resource Popup Column Order */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseCols
&Scoped-define SELF-NAME browseCols
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseCols Dialog-Frame
ON DEFAULT-ACTION OF browseCols IN FRAME Dialog-Frame
DO:
  APPLY 'CHOOSE' TO btnHide.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHide Dialog-Frame
ON CHOOSE OF btnHide IN FRAME Dialog-Frame /* Hide/Unhide */
DO:
  IF NOT browseColumn.colLocked THEN
  ASSIGN
    browseColumn.colHidden = NOT browseColumn.colHidden
    ldummy = {&BROWSE-NAME}:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown Dialog-Frame
ON CHOOSE OF btnMoveDown IN FRAME Dialog-Frame /* Move Down */
DO:
  RUN moveColumns (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp Dialog-Frame
ON CHOOSE OF btnMoveUp IN FRAME Dialog-Frame /* Move Up */
DO:
  RUN moveColumns (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
  MESSAGE 'Popup Window must be closed and'
          'restarted for changes to take effect' VIEW-AS ALERT-BOX.
  OUTPUT TO VALUE(SEARCH('{&data}/' + ID + '/columns.dat')).
  FOR EACH browseColumn NO-LOCK:
    EXPORT browseColumn.
  END.
  OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  RUN loadColumns.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
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
  DISPLAY EDITOR-1 
      WITH FRAME Dialog-Frame.
  ENABLE browseCols btnMoveUp btnMoveDown btnHide EDITOR-1 btnOK btnClose 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadColumns Dialog-Frame 
PROCEDURE loadColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/columns.dat')) NO-ECHO.
  REPEAT:
    CREATE browseColumn.
    IMPORT browseColumn.
  END.
  INPUT CLOSE.
  IF browseColumn.colOrder EQ 0 THEN
  DELETE browseColumn.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveColumns Dialog-Frame 
PROCEDURE moveColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS INTEGER NO-UNDO.

  DEFINE VARIABLE saveRowID AS ROWID NO-UNDO.

  IF browseColumn.colLocked THEN RETURN.
  FIND bBrowseColumn EXCLUSIVE-LOCK
       WHERE bBrowseColumn.colOrder EQ browseColumn.colOrder + ipMove NO-ERROR.
  IF AVAILABLE bBrowseColumn AND NOT bBrowseColumn.colLocked THEN DO:
    ASSIGN
      bBrowseColumn.colOrder = bBrowseColumn.colOrder - ipMove
      browseColumn.colOrder = browseColumn.colOrder + ipMove
      saveRowID = ROWID(browseColumn).
    {&OPEN-QUERY-{&BROWSE-NAME}}
    REPOSITION {&BROWSE-NAME} TO ROWID saveRowID.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

