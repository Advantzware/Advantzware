&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
DEFINE INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcSlsList AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE gopcSlsListStart AS CHARACTER   NO-UNDO.
DEFINE VARIABLE char-val AS CHARACTER   NO-UNDO.
DEF VAR iCnt AS INT NO-UNDO.
DEF VAR cEdScrn AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS edSlsList btn-OK btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS edSlsList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validSlsList Dialog-Frame 
FUNCTION validSlsList RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-GO 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-OK 
     LABEL "&OK" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE edSlsList AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 56 BY 3.81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     edSlsList AT ROW 2.43 COL 3 NO-LABEL WIDGET-ID 2
     btn-OK AT ROW 6.43 COL 10.6
     btn-cancel AT ROW 6.43 COL 29.8
     "Enter Sales Rep IDs to Include (separated by a comma):" VIEW-AS TEXT
          SIZE 56 BY 1.43 AT ROW 1 COL 3 WIDGET-ID 4
     SPACE(1.99) SKIP(5.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Sales Rep Inclusion List"
         DEFAULT-BUTTON btn-OK.


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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Sales Rep Inclusion List */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    iopcSlsList = gopcSlsListStart.
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-OK Dialog-Frame
ON CHOOSE OF btn-OK IN FRAME Dialog-Frame /* OK */
DO:
    IF validSlsList() THEN
        APPLY "GO" TO FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edSlsList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edSlsList Dialog-Frame
ON HELP OF edSlsList IN FRAME Dialog-Frame
DO: 
  run windows/l-smanm.w (ipcCompany, output char-val).

  if char-val ne "" THEN DO:
    DO iCnt = NUM-ENTRIES(char-val) TO 1 BY -1 :

      edSlsList:INSERT-STRING("," + ENTRY(iCnt, char-val) + ",").
    END.
    
  END.
     
  cEdScrn = edSlsList:SCREEN-VALUE.
  edSlsList:SCREEN-VALUE = "".

  cEdScrn = TRIM(cEdScrn, ",").
  edSlsList:SET-SELECTION(1, 200).
  edSlsList:EDIT-CLEAR().
  
  DO WHILE INDEX(cEdScrn, ",,") GT 0:
          cEdScrn = REPLACE(cEdScrn, ",,",",").
  END.


  edSlsList:INSERT-STRING(cEdScrn).
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
  gopcSlsListStart = iopcSlsList.
  RUN enable_UI.
  edSlsList:SCREEN-VALUE IN FRAME {&FRAME-NAME} = gopcSlsListStart.
  APPLY "entry" TO edSlsList IN FRAME {&FRAME-NAME}.
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
  DISPLAY edSlsList 
      WITH FRAME Dialog-Frame.
  ENABLE edSlsList btn-OK btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validSlsList Dialog-Frame 
FUNCTION validSlsList RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VARIABLE cSlsList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSlsID AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.

DEFINE BUFFER bf-sman FOR sman.

cSlsList  = edSlsList:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
cSlsList = TRIM(cSlsList,",").
lOK = YES.
IF NUM-ENTRIES(cSlsList) GT 0 THEN
    DO i = 1 TO NUM-ENTRIES(cSlsList):
        cSlsID = ENTRY(i,cSlsList).
        FIND FIRST bf-sman
            WHERE bf-sman.company EQ ipcCompany
              AND bf-sman.sman EQ cSlsID
            NO-LOCK NO-ERROR.
        IF NOT AVAIL bf-sman THEN DO:
            MESSAGE cSlsID " is not a valid Sales Rep ID"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            lOK = NO.
            APPLY "entry" TO edSlsList.
            LEAVE.
        END.
    END.

IF lOK THEN
    iopcSlsList = cSlsList.
ELSE 
    iopcSlsList = "".
  
RETURN lOK.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

