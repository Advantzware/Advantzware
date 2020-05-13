&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: fg/d-fgStatusUpdate.w

  Description: Dialog box to update status and onhold of an fg bin

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
DEFINE INPUT  PARAMETER ipcStatusID   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplOnHold     AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSave       AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcStatusID   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplOnHold     AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */
{inventory/ttinventory.i "NEW SHARED"}.
{custom/globdefs.i}

DEFINE VARIABLE hdInventoryProcs AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-20 fiTagStatus rsOnHold Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiTagStatus fiTagDesc rsOnHold 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U
     LABEL "OK" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE fiTagDesc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 73 BY 1 NO-UNDO.

DEFINE VARIABLE fiTagStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tag Status" 
     VIEW-AS FILL-IN 
     SIZE 19.2 BY 1 NO-UNDO.

DEFINE VARIABLE rsOnHold AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Yes", yes,
"No", no
     SIZE 31 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19 BY 2.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 109.4 BY 3.1
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiTagStatus AT ROW 1.57 COL 16 COLON-ALIGNED WIDGET-ID 10
     fiTagDesc AT ROW 1.57 COL 35.4 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     rsOnHold AT ROW 2.91 COL 17.6 NO-LABEL WIDGET-ID 20
     Btn_OK AT ROW 4.67 COL 93.6 WIDGET-ID 30
     Btn_Cancel AT ROW 4.67 COL 103 WIDGET-ID 28
     "On Hold:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 3.14 COL 7.4 WIDGET-ID 24
     RECT-3 AT ROW 1.24 COL 2 WIDGET-ID 26
     RECT-20 AT ROW 4.48 COL 92.6 WIDGET-ID 32
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 FONT 6
         TITLE "Update Bin Status" WIDGET-ID 100.


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

/* SETTINGS FOR FILL-IN fiTagDesc IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update Bin Status */
DO:
    oplSave = FALSE.

    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE PROCEDURE hdInventoryProcs.

    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    oplSave = FALSE.  

    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE PROCEDURE hdInventoryProcs.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN
        oplSave     = TRUE
        oplOnHold   = LOGICAL(rsOnHold:SCREEN-VALUE)
        opcStatusID = fiTagStatus:SCREEN-VALUE
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTagStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTagStatus Dialog-Frame
ON LEAVE OF fiTagStatus IN FRAME Dialog-Frame /* Tag Status */
DO:
    DEFINE VARIABLE cStatusDesc  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidStatus AS LOGICAL   NO-UNDO.
    
    IF SELF:SCREEN-VALUE NE "" THEN DO:
        RUN Inventory_ValidateStatusID IN hdInventoryProcs (
            INPUT  SELF:SCREEN-VALUE,
            OUTPUT lValidStatus
            ).
        IF NOT lValidStatus THEN DO:
            MESSAGE "Invalid Tag Status '" + SELF:SCREEN-VALUE + "'. Please enter a valid tag status"
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.    
        END.
        
        RUN Inventory_GetStatusDescription IN hdInventoryProcs (
            INPUT  SELF:SCREEN-VALUE,
            OUTPUT cStatusDesc 
            ).  
        
        fiTagDesc:SCREEN-VALUE = cStatusDesc.
    END. /* if not blank */
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
    RUN pInit.
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
  DISPLAY fiTagStatus fiTagDesc rsOnHold 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-3 RECT-20 fiTagStatus rsOnHold Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit Dialog-Frame 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
    DEFINE VARIABLE cStatusDesc AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    
    IF ipcStatusID NE "" THEN
        RUN Inventory_GetStatusDescription IN hdInventoryProcs (
            INPUT  ipcStatusID,
            OUTPUT cStatusDesc 
            ).
    
    ASSIGN    
        fiTagStatus:SCREEN-VALUE = ipcStatusID
        rsOnHold:SCREEN-VALUE    = STRING(iplOnHold)
        fiTagDesc:SCREEN-VALUE   = cStatusDesc
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

