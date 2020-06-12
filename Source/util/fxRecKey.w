&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: fxRecKey.p

  Description: fix blank rec_key fields in all ASI database tables

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 12.28.2005
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS msg btnStartFix btnCancel 
&Scoped-Define DISPLAYED-OBJECTS msg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD nextRecKey Dialog-Frame 
FUNCTION nextRecKey RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnStartFix 
     LABEL "&Start Fix" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE msg AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 84 BY 11.19
     BGCOLOR 15 FONT 2 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     msg AT ROW 1 COL 1 NO-LABEL
     btnStartFix AT ROW 1.24 COL 86
     btnCancel AT ROW 2.48 COL 86
     SPACE(0.79) SKIP(8.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Fix Blank Note Dates & Rec_Key Values"
         DEFAULT-BUTTON btnStartFix CANCEL-BUTTON btnCancel.


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

ASSIGN 
       msg:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Fix Blank Note Dates  Rec_Key Values */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartFix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartFix Dialog-Frame
ON CHOOSE OF btnStartFix IN FRAME Dialog-Frame /* Start Fix */
DO:
  DISABLE {&SELF-NAME} btnCancel WITH FRAME {&FRAME-NAME}.
  RUN msg ('Checking for Blank Rec_Key Values ...' + CHR(10)).
  RUN fixTables.
  RUN msg ('Done.' + CHR(10)).
  RUN msg ('' + CHR(10)).
  RUN msg ('Fixing Blank Note Dates ...' + CHR(10)).
  RUN fixNotes.
  RUN msg ('Done.').
  btnCancel:LABEL = '&Close'.
  ENABLE btnCancel WITH FRAME {&FRAME-NAME}.
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
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

{util/fxRecKey.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createRecKey Dialog-Frame 
PROCEDURE createRecKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRecKey AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTableName AS CHARACTER NO-UNDO.

/*  CREATE rec_key.                    */
/*  ASSIGN                             */
/*    rec_key.rec_key = ipRecKey       */
/*    rec_key.table_name = ipTableName.*/

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
  DISPLAY msg 
      WITH FRAME Dialog-Frame.
  ENABLE msg btnStartFix btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixNotes Dialog-Frame 
PROCEDURE fixNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DISABLE TRIGGERS FOR LOAD OF notes.                                      */
/*                                                                           */
/*  FOR EACH notes EXCLUSIVE-LOCK WHERE notes.note_date EQ ?:                */
/*    FIND rec_key NO-LOCK WHERE rec_key.rec_key EQ notes.rec_key NO-ERROR.  */
/*    IF rec_key.table_name EQ 'cust' THEN                                   */
/*    FIND FIRST cust NO-LOCK WHERE cust.rec_key EQ rec_key.rec_key NO-ERROR.*/
/*    notes.note_date = IF AVAILABLE rec_key AND                             */
/*                         rec_key.table_name EQ 'cust' AND                  */
/*                         AVAILABLE cust THEN cust.date-field[1]            */
/*                      ELSE 1.1.2006.                                       */
/*  END. /* each notes */                                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE msg Dialog-Frame 
PROCEDURE msg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipText AS CHARACTER NO-UNDO.

  msg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = msg:SCREEN-VALUE + ipText.
  msg:MOVE-TO-EOF().
  PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION nextRecKey Dialog-Frame 
FUNCTION nextRecKey RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  RETURN DYNAMIC-FUNCTION("sfGetNextRecKey").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

