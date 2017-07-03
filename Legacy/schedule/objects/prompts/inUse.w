&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: inUse.w

  Description: Check for In Use by others

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 4.26.2017 (changed to a dialog module)
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipID AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipID AS CHARACTER NO-UNDO INITIAL 'ASI/Folding'.
&ENDIF

/* Local Variable Definitions ---                                       */

{schedule/scopDir.i}

DEFINE VARIABLE inUseList AS CHARACTER NO-UNDO.

SESSION:SET-WAIT-STATE('').

{{&includes}/findProgram.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IDList btnRemove btnContinue 
&Scoped-Define DISPLAYED-OBJECTS IDList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnContinue AUTO-GO 
     LABEL "&Continue" 
     SIZE 12 BY 1.

DEFINE BUTTON btnRemove 
     LABEL "&Remove" 
     SIZE 12.2 BY 1.

DEFINE VARIABLE IDList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 55 BY 7.62
     FONT 2 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     IDList AT ROW 1.71 COL 1 NO-LABEL WIDGET-ID 6
     btnRemove AT ROW 9.57 COL 30 WIDGET-ID 4
     btnContinue AT ROW 9.57 COL 43 WIDGET-ID 2
     " Date         Time           User" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 1 COL 1 WIDGET-ID 8
          FONT 2
     SPACE(3.99) SKIP(8.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "In Use List"
         DEFAULT-BUTTON btnContinue WIDGET-ID 100.


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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* In Use List */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove Dialog-Frame
ON CHOOSE OF btnRemove IN FRAME Dialog-Frame /* Remove */
DO:
  IF IDList:SCREEN-VALUE EQ '' OR
     IDList:SCREEN-VALUE EQ ?  THEN
  RETURN NO-APPLY.
  MESSAGE 'Remove "In Use File: ' + IDList:SCREEN-VALUE + '"?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE removeFile AS LOGICAL.
  IF removeFile THEN DO:
    OS-DELETE VALUE(IDList:SCREEN-VALUE).
    RUN showInUseFiles (ipID,OUTPUT inUseList).
  END.
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
  RUN showInUseFiles (ipID,OUTPUT inUseList).
  RUN enable_UI.
  IF inUseList EQ '' THEN RETURN.
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
  DISPLAY IDList 
      WITH FRAME Dialog-Frame.
  ENABLE IDList btnRemove btnContinue 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getInUseFiles Dialog-Frame 
PROCEDURE getInUseFiles :
/*------------------------------------------------------------------------------
  Purpose:     get installation identifier dir names
  Parameters:  starting directory, return directory name
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipSearchDir AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opListItems AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE fileName AS CHARACTER FORMAT 'X(40)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)'  NO-UNDO.

  IF INDEX(PROPATH,'schedule') NE 0 AND
     INDEX(PROPATH,':') EQ 0 THEN
  ipSearchDir = '../' + ipSearchDir.

  RUN disable_UI.
  INPUT FROM OS-DIR(ipSearchDir) NO-ECHO.
  REPEAT:
      SET fileName ^ attrList.
      IF attrList NE 'f' OR INDEX(fileName,'inUse') EQ 0 THEN
      NEXT.
      opListItems = IF opListItems EQ '' THEN fileName
                    ELSE opListItems + ',' +  fileName.
  END. /* repeat */
  INPUT CLOSE.
  RUN enable_UI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showInUseFiles Dialog-Frame 
PROCEDURE showInUseFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipID AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opInUseList AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE inUseFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE inUseDate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE inUseTime AS CHARACTER NO-UNDO.
  DEFINE VARIABLE inUseUser AS CHARACTER NO-UNDO.
  DEFINE VARIABLE startDir AS CHARACTER NO-UNDO.
  
  ASSIGN
    startDir = clientDat + '{&data}/' + ipID
    IDList:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = ?
    .
  RUN getInUseFiles (startDir,OUTPUT opInUseList).
  IF opInUseList NE '' AND
     opInUseList NE ?  THEN
  DO i = 1 TO NUM-ENTRIES(opInUseList):
    inUseFile = startDir + '/' + ENTRY(i,opInUseList).
    INPUT FROM VALUE(inUseFile) NO-ECHO.
    IMPORT inUseDate inUseTime inUseUser.
    IDList:ADD-LAST(inUseDate + ' @ ' + inUseTime + ' by ' + inUseUser,inUseFile).
    INPUT CLOSE.
  END. /* do i */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

