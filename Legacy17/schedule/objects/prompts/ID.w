&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: ID.w

  Description: Identifier

  Input Parameters: <none>

  Output Parameters: ID

  Author: Ron Stark

  Created: 5.12.2004 (changed from window to dialog 3.3.2017)
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE OUTPUT PARAMETER opID AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE opID AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{schedule/scopDir.i}

DEFINE VARIABLE dirList AS CHARACTER NO-UNDO.
DEFINE VARIABLE ID AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE startDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE validIDList AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS IDList btnOK btnCancel 
&Scoped-Define DISPLAYED-OBJECTS IDList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 9.2 BY 1.

DEFINE BUTTON btnOK AUTO-GO 
     LABEL "&OK" 
     SIZE 9.2 BY 1.

DEFINE VARIABLE IDList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 39 BY 8.33 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     IDList AT ROW 1 COL 1 NO-LABEL WIDGET-ID 6
     btnOK AT ROW 9.57 COL 20 WIDGET-ID 4
     btnCancel AT ROW 9.57 COL 30 WIDGET-ID 2
     SPACE(0.80) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select ID" WIDGET-ID 100.


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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select ID */
DO:
  opID = ''.
  APPLY 'GO':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
  opID = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IDList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IDList Dialog-Frame
ON DEFAULT-ACTION OF IDList IN FRAME Dialog-Frame
DO:
  APPLY 'GO':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IDList Dialog-Frame
ON RETURN OF IDList IN FRAME Dialog-Frame
DO:
  APPLY 'DEFAULT-ACTION':U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IDList Dialog-Frame
ON VALUE-CHANGED OF IDList IN FRAME Dialog-Frame
DO:
  opID = IDList:SCREEN-VALUE.
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
    RUN getValidIDList.
    startDir = REPLACE(clientDat + '{&scenarios}','\','/').
    RUN getID (startDir,OUTPUT dirList).
    DO i = 1 TO NUM-ENTRIES(dirList):
      RUN getID (startDir + '/' + ENTRY(i,dirList),OUTPUT ID).
      IF ID EQ '' AND CAN-DO(validIDList,ENTRY(i,dirList)) THEN
      IDList:ADD-LAST(ENTRY(i,dirList)).
      ELSE
      DO j = 1 TO NUM-ENTRIES(ID):
        IF CAN-DO(validIDList,ENTRY(i,dirList) + '/' + ENTRY(j,ID)) THEN
        IDList:ADD-LAST(ENTRY(i,dirList) + '/' + ENTRY(j,ID)).
      END.
    END.
    ASSIGN
      opID = IDList:ENTRY(1)
      IDList = IDList:ENTRY(1)
      .
    RUN enable_UI.
    IF IDList:NUM-ITEMS EQ 1 THEN
    RETURN.
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
  ENABLE IDList btnOK btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getID Dialog-Frame 
PROCEDURE getID :
/*------------------------------------------------------------------------------
  Purpose:     get installation identifier dir names
  Parameters:  starting directory, return directory name
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/getID.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getValidIDList Dialog-Frame 
PROCEDURE getValidIDList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE validID AS CHARACTER NO-UNDO.

  validIDList = ''.
  IF SEARCH('{&data}/validID.dat') NE ? THEN DO:
    INPUT FROM VALUE(SEARCH('{&data}/validID.dat')) NO-ECHO.
    REPEAT:
      IMPORT validID.
      validIDList = validIDList + (IF validIDList NE '' THEN ',' ELSE '') + validID.
    END.
    INPUT CLOSE.
  END.
  IF validIDlist EQ '' THEN validIDList = '*'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

