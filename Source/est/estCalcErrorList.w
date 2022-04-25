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
{est/ttEstCost.i}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER TABLE FOR ttEstError.
DEFINE INPUT PARAMETER ipiDefaultFilterLevel AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME Browse-ErrList

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttEstError

/* Definitions for BROWSE Browse-ErrList                                */
&Scoped-define FIELDS-IN-QUERY-Browse-ErrList ttEstError.iFormNo ttEstError.iBlankNo ttEstError.dQuantityMaster fGetErrorLevelChar(ttEstError.iErrorLevel) ttEstError.cError   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browse-ErrList ttEstError.iFormNo   
&Scoped-define ENABLED-TABLES-IN-QUERY-Browse-ErrList ttEstError
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browse-ErrList ttEstError
&Scoped-define SELF-NAME Browse-ErrList
&Scoped-define QUERY-STRING-Browse-ErrList FOR EACH ttEstError
&Scoped-define OPEN-QUERY-Browse-ErrList OPEN QUERY {&SELF-NAME} FOR EACH ttEstError WHERE ttEstError.iErrorLevel LE ipiDefaultFilterLevel.
&Scoped-define TABLES-IN-QUERY-Browse-ErrList ttEstError
&Scoped-define FIRST-TABLE-IN-QUERY-Browse-ErrList ttEstError


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-Browse-ErrList}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 ComboBox-ErrTypes Browse-ErrList ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS ComboBox-ErrTypes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetErrorLevelChar Dialog-Frame 
FUNCTION fGetErrorLevelChar RETURNS CHARACTER
  (iErrorLevel AS INTEGER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE ComboBox-ErrTypes AS CHARACTER FORMAT "X(256)":U 
     LABEL "Error Level" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Critical Only","Important and Critical Only","All Errors and Warnings" 
     DROP-DOWN-LIST
     SIZE 38.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 153 BY 14.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browse-ErrList FOR 
      ttEstError SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browse-ErrList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browse-ErrList Dialog-Frame _FREEFORM
  QUERY Browse-ErrList DISPLAY
      ttEstError.iFormNo COLUMN-LABEL "Form" WIDTH 7
      ttEstError.iBlankNo COLUMN-LABEL "Blank" WIDTH 7
      ttEstError.dQuantityMaster FORMAT ">>>>>>>9" COLUMN-LABEL "Quantity" WIDTH 10 
      fGetErrorLevelChar(ttEstError.iErrorLevel) FORMAT "X(9)"COLUMN-LABEL "Error Level" WIDTH 13
      ttEstError.cError FORMAT "X(100)" COLUMN-LABEL "Error Description" WIDTH 30
      ENABLE ttEstError.iFormNo
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 151 BY 8.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ComboBox-ErrTypes AT ROW 2.91 COL 58 COLON-ALIGNED WIDGET-ID 2
     Browse-ErrList AT ROW 6.1 COL 2.8 WIDGET-ID 200
     Btn_OK AT ROW 14.67 COL 49.2
     Btn_Cancel AT ROW 14.67 COL 79
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 4
     SPACE(0.79) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Errors and Warnings Found During Estimate Calculation"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
/* BROWSE-TAB Browse-ErrList ComboBox-ErrTypes Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browse-ErrList
/* Query rebuild information for BROWSE Browse-ErrList
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttEstError.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Browse-ErrList */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Errors and Warnings Found During Estimate Calculation */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browse-ErrList Dialog-Frame
ON START-SEARCH OF Browse-ErrList IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE hSortColumn  AS WIDGET-HANDLE.
    
    hSortColumn = Browse-ErrList:CURRENT-COLUMN.
    CASE hSortColumn:LABEL:
        WHEN "Form" THEN
        DO:
            IF ComboBox-ErrTypes:SCREEN-VALUE EQ "Critical Only" THEN 
            OPEN QUERY Browse-ErrList FOR EACH ttEstError WHERE ttEstError.iErrorLevel EQ 1 BY iFormNo.  
            ELSE IF ComboBox-ErrTypes:SCREEN-VALUE EQ "Important and Critical Only" THEN  
            OPEN QUERY Browse-ErrList FOR EACH ttEstError WHERE ttEstError.iErrorLevel LE 2 BY iFormNo. 
            ELSE 
            OPEN QUERY Browse-ErrList FOR EACH ttEstError BY iFormNo.
        END.    
        WHEN "Blank" THEN
        DO:
            IF ComboBox-ErrTypes:SCREEN-VALUE EQ "Critical Only" THEN 
            OPEN QUERY Browse-ErrList FOR EACH ttEstError WHERE ttEstError.iErrorLevel EQ 1 BY iBlankNo.  
            ELSE IF ComboBox-ErrTypes:SCREEN-VALUE EQ "Important and Critical Only" THEN  
            OPEN QUERY Browse-ErrList FOR EACH ttEstError WHERE ttEstError.iErrorLevel LE 2 BY iBlankNo. 
            ELSE 
            OPEN QUERY Browse-ErrList FOR EACH ttEstError BY iBlankNo.
        END.    
    END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ComboBox-ErrTypes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ComboBox-ErrTypes Dialog-Frame
ON VALUE-CHANGED OF ComboBox-ErrTypes IN FRAME Dialog-Frame /* Error Level */
DO: 
    IF ComboBox-ErrTypes:SCREEN-VALUE EQ "Critical Only" THEN 
    OPEN QUERY Browse-ErrList FOR EACH ttEstError WHERE ttEstError.iErrorLevel EQ 1.  
    ELSE IF ComboBox-ErrTypes:SCREEN-VALUE EQ "Important and Critical Only" THEN  
    OPEN QUERY Browse-ErrList FOR EACH ttEstError WHERE ttEstError.iErrorLevel LE 2. 
    ELSE 
    OPEN QUERY Browse-ErrList FOR EACH ttEstError.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  RUN enable_UI.
  IF ipiDefaultFilterLevel EQ 1 THEN 
     ComboBox-ErrTypes:SCREEN-VALUE = "Critical Only".
  ELSE IF ipiDefaultFilterLevel EQ 2 THEN   
     ComboBox-ErrTypes:SCREEN-VALUE = "Important and Critical Only". 
  ELSE
     ComboBox-ErrTypes:SCREEN-VALUE = "All Errors and Warnings".   
      
  ASSIGN ttEstError.iFormNo:READ-ONLY IN BROWSE Browse-ErrList = TRUE.
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
  DISPLAY ComboBox-ErrTypes 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 ComboBox-ErrTypes Browse-ErrList Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetErrorLevelChar Dialog-Frame 
FUNCTION fGetErrorLevelChar RETURNS CHARACTER
  (iErrorLevel AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
                DEFINE VARIABLE result AS CHARACTER NO-UNDO.
                CASE iErrorLevel:
                    WHEN 1 THEN RETURN "Critical".
                    WHEN 2 THEN RETURN "Important".
                    WHEN 3 THEN RETURN "Warning".
                END CASE.   
                
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

