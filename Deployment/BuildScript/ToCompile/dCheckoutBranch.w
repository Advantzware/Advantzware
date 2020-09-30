&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* Local Variable Definitions ---                                       */
DEF VAR cRawValue AS CHAR NO-UNDO.
DEF VAR iTimeLeft AS INT NO-UNDO.
DEF VAR iHrsLeft AS INT NO-UNDO.
DEF VAR iMinsLeft AS INT NO-UNDO.
DEF VAR iSecsLeft AS INT NO-UNDO.
DEF VAR cTimeLeft AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS eInstructions fiTimeLimit slBranchList ~
btnCheckOut Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS eInstructions fiTimeLimit slBranchList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCheckOut AUTO-GO 
     LABEL "Check Out" 
     SIZE 15 BY 2.14
     BGCOLOR 8 FONT 6.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 2.14
     BGCOLOR 8 FONT 6.

DEFINE VARIABLE eInstructions AS CHARACTER INITIAL "Select a branch from the list below to check out and test." 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 119 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiTimeLimit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 119 BY 1
     BGCOLOR 14 FONT 17 NO-UNDO.

DEFINE VARIABLE slBranchList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 119 BY 14.29 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     eInstructions AT ROW 1.48 COL 11 NO-LABEL
     fiTimeLimit AT ROW 3.38 COL 9 COLON-ALIGNED NO-LABEL
     slBranchList AT ROW 4.81 COL 11 NO-LABEL
     btnCheckOut AT ROW 14.1 COL 136
     Btn_Cancel AT ROW 16.95 COL 136
     SPACE(4.99) SKIP(1.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Check Out GIT Branch"
         DEFAULT-BUTTON btnCheckOut CANCEL-BUTTON Btn_Cancel.


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

ASSIGN 
       eInstructions:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Check Out GIT Branch */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCheckOut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCheckOut Dialog-Frame
ON CHOOSE OF btnCheckOut IN FRAME Dialog-Frame /* Check Out */
DO:
    OS-COMMAND VALUE ("c:\asigui\build\checkoutBranch.bat " + REPLACE(slBranchList:SCREEN-VALUE,"remotes/","")).
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
  RUN enable_UI.
  
    ASSIGN 
        iTimeLeft = TIME.
    IF TIME GT 54000 THEN ASSIGN 
        iTimeLeft = (86400 - TIME) + 36000.
    ELSE IF TIME GT 36000 THEN ASSIGN 
        iTimeLeft = 54000 - TIME.
    ELSE ASSIGN 
        iTimeLeft = 36000 - TIME.
    ASSIGN 
        isecsLeft = iTimeLeft MODULO 60
        iTimeLeft = (iTimeLeft - iSecsLeft) / 60 
        iMinsLeft = iTimeLeft MODULO 60
        iHrsLeft = (iTimeLeft - iMinsLeft) / 60.
    ASSIGN 
        cTimeLeft = STRING(iHrsLeft,"99") + ":" + STRING(iMinsLeft,"99") + ":" + STRING(iSecsLeft,"99")
        fiTimeLimit:SCREEN-VALUE = "You have " + cTimeLeft + " until the next automatic build.". 
        
    ASSIGN 
        eInstructions:SCREEN-VALUE = "Select a branch from the list below to check out and test." + CHR(10) +
                                     "NOTE: Files may be overwritten during daily builds (10am and 3pm).".

    INPUT FROM "c:\tmp\branchlist.txt".
    REPEAT:
        IMPORT cRawValue.
        IF cRawValue NE "*" THEN
            slBranchList:ADD-LAST(REPLACE(cRawValue,"remotes/","")).
    END.
    INPUT CLOSE.
                                        
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
  DISPLAY eInstructions fiTimeLimit slBranchList 
      WITH FRAME Dialog-Frame.
  ENABLE eInstructions fiTimeLimit slBranchList btnCheckOut Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

