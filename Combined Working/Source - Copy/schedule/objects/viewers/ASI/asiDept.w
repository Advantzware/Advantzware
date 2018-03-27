&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: asiDept.w

  Description: maintain ASI department codes

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 10.9.2005
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&viewers}/includes/asiDept.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS availDept department selectedDept btnAdd ~
btnDelete btnSave btnReset btnExit 
&Scoped-Define DISPLAYED-OBJECTS availDept department selectedDept 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "schedule/images/next.bmp":U
     LABEL "&Add" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "schedule/images/prev.bmp":U
     LABEL "&Delete" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "E&xit" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "&Reset" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "&Save" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE department AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Type" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Empty" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE availDept AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 20 BY 12.62 NO-UNDO.

DEFINE VARIABLE selectedDept AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 20 BY 10 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     availDept AT ROW 1 COL 1 NO-LABEL
     department AT ROW 1 COL 20.6
     selectedDept AT ROW 2.19 COL 29 NO-LABEL
     btnAdd AT ROW 3.62 COL 23
     btnDelete AT ROW 5.05 COL 23
     btnSave AT ROW 12.43 COL 32
     btnReset AT ROW 12.43 COL 38
     btnExit AT ROW 12.43 COL 44
     SPACE(0.20) SKIP(0.05)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "ASI Department Codes"
         CANCEL-BUTTON btnExit.


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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX department IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* ASI Department Codes */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME availDept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL availDept Dialog-Frame
ON DEFAULT-ACTION OF availDept IN FRAME Dialog-Frame
DO:
  IF NOT CAN-DO(selectedDept:LIST-ITEMS,SELF:SCREEN-VALUE) OR
     selectedDept:NUM-ITEMS EQ 0 THEN
  selectedDept:ADD-LAST(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd Dialog-Frame
ON CHOOSE OF btnAdd IN FRAME Dialog-Frame /* Add */
DO:
  APPLY 'DEFAULT-ACTION':U TO availDept.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete Dialog-Frame
ON CHOOSE OF btnDelete IN FRAME Dialog-Frame /* Delete */
DO:
  APPLY 'DEFAULT-ACTION':U TO selectedDept.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame /* Reset */
DO:
  RUN asiDeptLoad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
DO:
  RUN asiDeptSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME department
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL department Dialog-Frame
ON VALUE-CHANGED OF department IN FRAME Dialog-Frame /* Type */
DO:
  ASSIGN {&SELF-NAME}.
  RUN asiDeptList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedDept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedDept Dialog-Frame
ON VALUE-CHANGED OF selectedDept IN FRAME Dialog-Frame
DO:
  SELF:DELETE(SELF:SCREEN-VALUE).
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
  RUN asiDeptBuild.
  RUN enable_UI.
  RUN asiDeptLoad.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asiDeptList Dialog-Frame 
PROCEDURE asiDeptList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  FIND FIRST asiDept NO-LOCK WHERE asiDept.rptName EQ department NO-ERROR.
  IF NOT AVAILABLE asiDept THEN RETURN.
  selectedDept:LIST-ITEMS IN FRAME {&FRAME-NAME} = ''.
  DO i = 1 TO NUM-ENTRIES(asiDept.asiDept):
    IF CAN-DO(availDept:LIST-ITEMS,ENTRY(i,asiDept.asiDept)) THEN
    selectedDept:ADD-LAST(ENTRY(i,asiDept.asiDept)).
  END. /* do i */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asiDeptLoad Dialog-Frame 
PROCEDURE asiDeptLoad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    department:LIST-ITEMS IN FRAME {&FRAME-NAME} = ''
    availDept:LIST-ITEMS = departmentList.
  FOR EACH asiDept NO-LOCK:
    department:ADD-LAST(asiDept.rptName).
  END. /* each asidept */
  department:SCREEN-VALUE = department:ENTRY(1).
  APPLY 'VALUE-CHANGED':U TO department.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asiDeptSave Dialog-Frame 
PROCEDURE asiDeptSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST asiDept EXCLUSIVE-LOCK WHERE asiDept.rptName EQ department NO-ERROR.
  IF AVAILABLE asiDept THEN
  asiDept.asiDept = selectedDept:LIST-ITEMS IN FRAME {&FRAME-NAME}.
  OUTPUT TO VALUE(findProgram('{&data}/',ID,'/asiDept.dat')).
  FOR EACH asiDept NO-LOCK:
    EXPORT asiDept.
  END. /* each asidept */
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY availDept department selectedDept 
      WITH FRAME Dialog-Frame.
  ENABLE availDept department selectedDept btnAdd btnDelete btnSave btnReset 
         btnExit 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

