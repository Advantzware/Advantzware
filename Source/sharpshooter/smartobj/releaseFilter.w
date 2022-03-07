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

  File: sharpshooter/smartobj/releaseFilter.w

  Description: from SMART.W - Template for basic SmartObject

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

DEFINE VARIABLE lReleasePrintedValidation AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lReleasePostedValidation  AS LOGICAL NO-UNDO INITIAL FALSE.

DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
DEFINE VARIABLE oKeyboard      AS system.Keyboard  NO-UNDO.

oReleaseHeader = NEW oe.ReleaseHeader().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btFind fiRelease btnKeyboardRelease 
&Scoped-Define DISPLAYED-OBJECTS fiRelease 

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

DEFINE BUTTON btnKeyboardRelease 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE VARIABLE fiRelease AS CHARACTER FORMAT "X(7)":U 
     LABEL "RELEASE" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.38 TOOLTIP "Enter release #"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btFind AT ROW 1 COL 49 WIDGET-ID 4
     fiRelease AT ROW 1.24 COL 19 COLON-ALIGNED WIDGET-ID 2
     btnKeyboardRelease AT ROW 1.19 COL 57.2 WIDGET-ID 136 NO-TAB-STOP 
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
         HEIGHT             = 6.52
         WIDTH              = 66.4.
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
       btnKeyboardRelease:HIDDEN IN FRAME F-Main           = TRUE.

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
    APPLY "HELP" TO fiRelease.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardRelease s-object
ON CHOOSE OF btnKeyboardRelease IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiRelease.    
    
    oKeyboard:OpenKeyboardOverride (fiRelease:HANDLE, "Qwerty"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRelease s-object
ON ENTRY OF fiRelease IN FRAME F-Main /* RELEASE */
DO:
    SELF:SET-SELECTION ( 1, -1).    
    SELF:BGCOLOR = 30.

    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:OpenKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRelease s-object
ON HELP OF fiRelease IN FRAME F-Main /* RELEASE */
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        INPUT  "",  /* company */ 
        INPUT  "",  /* lookup field */
        INPUT  160, /* Subject ID */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRelease s-object
ON LEAVE OF fiRelease IN FRAME F-Main /* RELEASE */
DO:
    /* If last key is not button choose or mouse click event */
    IF (((LASTKEY LT 609 OR LASTKEY GT 652) AND LASTKEY NE -1) OR (VALID-OBJECT (oKeyboard) AND oKeyboard:IsKeyboardOpen())) AND SELF:SCREEN-VALUE NE "" THEN
        RUN pReleaseScan.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRelease s-object
ON TAB OF fiRelease IN FRAME F-Main /* RELEASE */
DO:
    APPLY "LEAVE" TO SELF.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableRelease s-object 
PROCEDURE DisableRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    fiRelease:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
    
    btFind:SENSITIVE = FALSE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmptyRelease s-object 
PROCEDURE EmptyRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    fiRelease:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableRelease s-object 
PROCEDURE EnableRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    fiRelease:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.

    btFind:SENSITIVE = TRUE.
        
    APPLY "ENTRY" TO fiRelease.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMessageAndType s-object 
PROCEDURE GetMessageAndType :
/*------------------------------------------------------------------------------
 Purpose:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetReleaseID s-object 
PROCEDURE GetReleaseID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoReleaseHeader AS oe.ReleaseHeader NO-UNDO.
    
    opoReleaseHeader = oReleaseHeader.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit s-object 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.
    
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReleaseScan s-object 
PROCEDURE pReleaseScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    ASSIGN
        cStatusMessage     = ""
        iStatusMessageType = 0
        .
    
    RUN new-state (
        INPUT "release-invalid"
        ).
    
    IF SELF:SCREEN-VALUE EQ "EXIT" THEN DO:
        RUN new-state (
            INPUT "release-exit"
            ).        
        RETURN.        
    END.
    
    INTEGER(fiRelease:SCREEN-VALUE) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        RUN pSendError ("INVALID RELEASE '" + fiRelease:SCREEN-VALUE + "'").
        
        fiRelease:SCREEN-VALUE = "".
        
        RETURN.    
    END.
            
    oReleaseHeader:SetContext(cCompany, INTEGER(fiRelease:SCREEN-VALUE)).
    
    IF NOT oReleaseHeader:IsAvailable() THEN DO:
        RUN pSendError ("INVALID RELEASE '" + fiRelease:SCREEN-VALUE + "'").
        
        fiRelease:SCREEN-VALUE = "".
        
        RETURN.
    END.
    
    IF lReleasePrintedValidation AND NOT LOGICAL(oReleaseHeader:GetValue("Printed")) THEN DO:
        RUN pSendError ("RELEASE '" + oReleaseHeader:GetValue("ReleaseID") + "' IS NOT PRINTED").
        
        fiRelease:SCREEN-VALUE = "".
            
        RETURN.    
    END.

    IF lReleasePostedValidation AND LOGICAL(oReleaseHeader:GetValue("Posted")) THEN DO:
        RUN pSendError ("RELEASE '" + oReleaseHeader:GetValue("ReleaseID") + "' IS ALREADY POSTED").
        
        fiRelease:SCREEN-VALUE = "".
            
        RETURN.    
    END.
    
    RUN new-state (
        INPUT "release-valid"
        ).    
                    
    SELF:BGCOLOR = 15.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSendError s-object 
PROCEDURE pSendError :
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
        "release-error"
        ).

    ASSIGN
        cStatusMessage     = ""
        iStatusMessageType = 0
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReleasePostedValidation s-object 
PROCEDURE ReleasePostedValidation :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplReleasePostedValidation AS LOGICAL NO-UNDO.
    
    lReleasePostedValidation = iplReleasePostedValidation.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReleasePrintedValidation s-object 
PROCEDURE ReleasePrintedValidation :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplReleasePrintedValidation AS LOGICAL NO-UNDO.
    
    lReleasePrintedValidation = iplReleasePrintedValidation.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus s-object 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    APPLY "ENTRY" TO fiRelease.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetKeyboard s-object 
PROCEDURE SetKeyboard :
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
        btnKeyboardRelease:VISIBLE   = TRUE
        btnKeyboardRelease:SENSITIVE = TRUE
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

