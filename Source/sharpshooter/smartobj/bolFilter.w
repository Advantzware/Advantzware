&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/smartobj/bolFilter.w

  Description: 
  Author:
  Created:

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.
DEFINE VARIABLE lShowErrorAsAlert  AS LOGICAL   NO-UNDO INITIAL TRUE.

DEFINE VARIABLE oBOLHeader AS bol.BOLHeader    NO-UNDO.
DEFINE VARIABLE oKeyboard  AS system.Keyboard  NO-UNDO.

oBOLHeader = NEW bol.BOLHeader().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btFind btnKeyboardBOL fiBOL 
&Scoped-Define DISPLAYED-OBJECTS fiBOL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btFind 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Find" 
     SIZE 8 BY 1.91.
     
DEFINE BUTTON btnKeyboardBOL 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE VARIABLE fiBOL AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "BOL" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.48
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btFind AT ROW 1 COL 44 WIDGET-ID 4
     btnKeyboardBOL AT ROW 1.24 COL 43.8 WIDGET-ID 136 NO-TAB-STOP 
     fiBOL AT ROW 1.24 COL 8.6 COLON-ALIGNED WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 2.91
         WIDTH              = 51.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       btnKeyboardBOL:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btFind
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFind s-object
ON CHOOSE OF btFind IN FRAME F-Main /* Find */
DO:
    APPLY "HELP" TO fiBOL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnKeyboardBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardBOL s-object
ON CHOOSE OF btnKeyboardBOL IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiBOL.    
    
    oKeyboard:OpenKeyboardOverride (fiBOL:HANDLE, "Qwerty"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBOL s-object
ON TAB OF fiBOL IN FRAME F-Main OR ENTER OF fiBOL /* BOL */
DO:
    APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBOL s-object
ON ENTRY OF fiBOL IN FRAME F-Main /* BOL */
DO:
    SELF:SET-SELECTION(1, -1).  
    
    SELF:BGCOLOR = 30.

    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:OpenKeyboard (SELF, "Qwerty").    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBOL s-object
ON HELP OF fiBOL IN FRAME F-Main /* BOL */
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        INPUT  "",  /* company */ 
        INPUT  "",  /* lookup field */
        INPUT  38, /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        SELF:SCREEN-VALUE = IF NUM-ENTRIES(returnFields,"|") GE 2 THEN
                                ENTRY(2, returnFields, "|")
                            ELSE
                                "".
        
        APPLY "LEAVE" TO SELF.
    END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBOL s-object
ON LEAVE OF fiBOL IN FRAME F-Main /* BOL */
DO:
    IF (((LASTKEY LT 609 OR LASTKEY GT 652) AND LASTKEY NE -1) OR (VALID-OBJECT (oKeyboard) AND oKeyboard:IsKeyboardOpen())) AND SELF:SCREEN-VALUE NE "" THEN    
        RUN pBOLScan (
            INPUT INTEGER(fiBOL:SCREEN-VALUE)
            ).
    ELSE        
        SELF:BGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableErrorAlerts s-object 
PROCEDURE DisableErrorAlerts :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    lShowErrorAsAlert = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmptyBOL s-object 
PROCEDURE EmptyBOL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    fiBOL:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetBOLHeader s-object 
PROCEDURE GetBOLHeader :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoBOLHeader AS bol.BOLHeader NO-UNDO.

    opoBOLHeader = oBOLHeader.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMessageAndType s-object 
PROCEDURE GetMessageAndType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcStatusMessage     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiStatusMessageType AS INTEGER   NO-UNDO.
    
    ASSIGN
        opcStatusMessage     = cStatusMessage
        opiStatusMessageType = iStatusMessageType
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable s-object 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBOLScan s-object 
PROCEDURE pBOLScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiBOLID AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE lValidBOL AS LOGICAL NO-UNDO.

    ASSIGN
        cStatusMessage     = ""
        iStatusMessageType = 0
        .
        
    RUN new-state (
        INPUT "bol-invalid"
        ).
        
    lValidBOL = oBOLHeader:SetContext(cCompany, ipiBOLID).
    
    IF lValidBOL THEN
        RUN new-state (
            INPUT "bol-valid"
             ).
    ELSE
        RUN pSendError ("INVALID BOL '" + STRING(ipiBOLID) + "'").
               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit s-object 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSendError s-object 
PROCEDURE pSendError PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcStatusMessage AS CHARACTER NO-UNDO.
    
    IF lShowErrorAsAlert THEN DO:
        MESSAGE ipcStatusMessage
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.
    
    ASSIGN
        cStatusMessage     = ipcStatusMessage
        iStatusMessageType = 3
        .
        
    RUN new-state (
        "bol-error"
        ).

    ASSIGN
        cStatusMessage     = ""
        iStatusMessageType = 0
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus s-object 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    APPLY "ENTRY" TO fiBOL.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetKeyboard s-object
PROCEDURE SetKeyboard:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipoKeyboard AS system.Keyboard NO-UNDO.
    
    oKeyboard = ipoKeyboard.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowKeyboard s-object
PROCEDURE ShowKeyboard:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        btnKeyboardBOL:VISIBLE   = TRUE
        btnKeyboardBOL:SENSITIVE = TRUE
        .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

