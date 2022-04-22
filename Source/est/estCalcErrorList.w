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
DEFINE INPUT PARAMETER ipiShowErrorandWarning AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiQuantity            AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttEstErrorList LIKE ttEstError
       FIELD iQuantity AS INTEGER
       FIELD cErrorLevel AS CHARACTER.

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
&Scoped-define INTERNAL-TABLES ttEstErrorList

/* Definitions for BROWSE Browse-ErrList                                */
&Scoped-define FIELDS-IN-QUERY-Browse-ErrList ttEstErrorList.iFormNo ttEstErrorList.iBlankNo ttEstErrorList.iQuantity ttEstErrorList.cErrorType ttEstErrorList.cError   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browse-ErrList ttEstErrorList.iFormNo   
&Scoped-define ENABLED-TABLES-IN-QUERY-Browse-ErrList ttEstErrorList
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browse-ErrList ttEstErrorList
&Scoped-define SELF-NAME Browse-ErrList
&Scoped-define QUERY-STRING-Browse-ErrList FOR EACH ttEstErrorList
&Scoped-define OPEN-QUERY-Browse-ErrList OPEN QUERY {&SELF-NAME} FOR EACH ttEstErrorList.
&Scoped-define TABLES-IN-QUERY-Browse-ErrList ttEstErrorList
&Scoped-define FIRST-TABLE-IN-QUERY-Browse-ErrList ttEstErrorList


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
      ttEstErrorList SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browse-ErrList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browse-ErrList Dialog-Frame _FREEFORM
  QUERY Browse-ErrList DISPLAY
      ttEstErrorList.iFormNo COLUMN-LABEL "Form" WIDTH 7
      ttEstErrorList.iBlankNo COLUMN-LABEL "Blank" WIDTH 7
      ttEstErrorList.iQuantity COLUMN-LABEL "Quantity" WIDTH 10
      ttEstErrorList.cErrorLevel COLUMN-LABEL "Error Level" WIDTH 13
      ttEstErrorList.cError FORMAT "X(100)" COLUMN-LABEL "Error Description" WIDTH 30
      ENABLE ttEstErrorList.iFormNo
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
OPEN QUERY {&SELF-NAME} FOR EACH ttEstErrorList.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Browse-ErrList */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* EstimateCalcErrorList */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Browse-ErrList
&Scoped-define SELF-NAME Browse-ErrList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browse-ErrList Dialog-Frame
ON START-SEARCH OF Browse-ErrList IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE hSortColumn  AS WIDGET-HANDLE.
    
    hSortColumn = Browse-ErrList:CURRENT-COLUMN.
    CASE hSortColumn:LABEL:
        WHEN "Form" THEN
            OPEN QUERY Browse-ErrList FOR EACH ttEstErrorList BY iFormNo.
        WHEN "Blank" THEN
            OPEN QUERY Browse-ErrList FOR EACH ttEstErrorList BY iBlankNo.
    END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ComboBox-ErrTypes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ComboBox-ErrTypes Dialog-Frame
ON VALUE-CHANGED OF ComboBox-ErrTypes IN FRAME Dialog-Frame /* Error Level */
DO: 
    IF ComboBox-ErrTypes:SCREEN-VALUE EQ "Critical Only" THEN 
    OPEN QUERY Browse-ErrList FOR EACH ttEstErrorList WHERE ttEstErrorList.iErrorLevel EQ 1.  
    ELSE IF ComboBox-ErrTypes:SCREEN-VALUE EQ "Important and Critical Only" THEN  
    OPEN QUERY Browse-ErrList FOR EACH ttEstErrorList WHERE ttEstErrorList.iErrorLevel EQ 1 OR ttEstErrorList.iErrorLevel EQ 2. 
    ELSE 
    OPEN QUERY Browse-ErrList FOR EACH ttEstErrorList.    
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
   IF ipiShowErrorandWarning EQ 1 THEN
   DO:
       FOR EACH ttEstError WHERE ttEstError.iErrorLevel EQ 1:
           CREATE ttEstErrorList.
           BUFFER-COPY ttEstError EXCEPT estHeaderId TO ttEstErrorList.
           ASSIGN ttEstErrorList.iQuantity = ipiQuantity
                  ttEstErrorList.cErrorLevel = "Critical".
       END.    
   END.
   IF ipiShowErrorandWarning EQ 2 THEN 
   DO:
       FOR EACH ttEstError WHERE ttEstError.iErrorLevel EQ 1 OR ttEstError.iErrorLevel EQ 2:
           CREATE ttEstErrorList.
           BUFFER-COPY ttEstError EXCEPT estHeaderId TO ttEstErrorList.
           ASSIGN ttEstErrorList.iQuantity = ipiQuantity
                  ttEstErrorList.cErrorLevel = (IF ttEstError.iErrorLevel EQ 1 THEN "Critical" ELSE "Important"  ).
       END.  
   END.
   IF ipiShowErrorandWarning EQ 3 THEN 
   DO:
       FOR EACH ttEstError:
           CREATE ttEstErrorList.
           BUFFER-COPY ttEstError EXCEPT estHeaderId TO ttEstErrorList.
           ASSIGN ttEstErrorList.iQuantity = ipiQuantity
           ttEstErrorList.cErrorLevel = (IF ttEstError.iErrorLevel EQ 1 THEN "Critical" 
                                         ELSE IF ttEstError.iErrorLevel EQ 2 THEN "Important"
                                         ELSE "Warning").
       END.  
   END.

  RUN enable_UI.
  ASSIGN ttEstErrorList.iFormNo:READ-ONLY IN BROWSE Browse-ErrList = TRUE.
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

