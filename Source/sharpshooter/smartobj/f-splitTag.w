&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/smartobj/f-splitTag.w

  Description: from cntnrfrm.w - ADM SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
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

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.

DEFINE VARIABLE oKeyboard AS system.Keyboard   NO-UNDO.
DEFINE VARIABLE oLoadtag  AS inventory.Loadtag NO-UNDO.
DEFINE VARIABLE oFGBin    AS fg.FGBin          NO-UNDO.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnKeyboardQuantity fiQuantity 
&Scoped-Define DISPLAYED-OBJECTS fiQuantity 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_tagfilter AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnKeyboardQuantity 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btSplit 
     LABEL "SPLIT TAG" 
     SIZE 22 BY 1.62.

DEFINE VARIABLE fiQuantity AS INT64 FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "QUANTITY" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1.38 TOOLTIP "Enter Quantity to Split"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnKeyboardQuantity AT ROW 3.91 COL 60.8 WIDGET-ID 136 NO-TAB-STOP 
     fiQuantity AT ROW 4 COL 21.8 COLON-ALIGNED WIDGET-ID 2
     btSplit AT ROW 5.76 COL 23.6 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.6 BY 6.95
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 5
   Other Settings: PERSISTENT-ONLY
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
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 6.95
         WIDTH              = 113.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       btnKeyboardQuantity:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btSplit IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnKeyboardQuantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardQuantity F-Frame-Win
ON CHOOSE OF btnKeyboardQuantity IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiQuantity.    
    
    oKeyboard:OpenKeyboardOverride (fiQuantity:HANDLE, "Numeric"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSplit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSplit F-Frame-Win
ON CHOOSE OF btSplit IN FRAME F-Main /* SPLIT TAG */
DO:
    RUN new-state ("split-tag").
    
    {methods/run_link.i "SPLIT-SOURCE" "EmptyTag"}

    {methods/run_link.i "SPLIT-SOURCE" "ScanNextTag"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiQuantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiQuantity F-Frame-Win
ON ENTRY OF fiQuantity IN FRAME F-Main /* QUANTITY */
DO:
    SELF:BGCOLOR = 30.  

    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:OpenKeyboard (SELF, "Numeric").     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiQuantity F-Frame-Win
ON LEAVE OF fiQuantity IN FRAME F-Main /* QUANTITY */
DO:
    SELF:BGCOLOR = 15.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/tagfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_tagfilter ).
       RUN set-position IN h_tagfilter ( 1.48 , 12.80 ) NO-ERROR.
       /* Size in UIB:  ( 2.29 , 85.40 ) */

       /* Links to SmartObject h_tagfilter. */
       RUN add-link IN adm-broker-hdl ( h_tagfilter , 'SPLIT':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_tagfilter , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win  _DEFAULT-ENABLE
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
  DISPLAY fiQuantity 
      WITH FRAME F-Main.
  ENABLE btnKeyboardQuantity fiQuantity 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMessageAndType F-Frame-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetQuantity F-Frame-Win 
PROCEDURE GetQuantity :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opdSplitQuantity AS DECIMAL NO-UNDO.
    
    opdSplitQuantity = DECIMAL(fiQuantity:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTag F-Frame-Win 
PROCEDURE GetTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoLoadtag AS Inventory.Loadtag NO-UNDO.
    
    {methods/run_link.i "SPLIT-SOURCE" "GetTag" "(OUTPUT opoLoadtag)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy F-Frame-Win
PROCEDURE local-destroy:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    
    IF VALID-OBJECT (oFGBin) THEN
        DELETE OBJECT oFGBin.
        
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable F-Frame-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit F-Frame-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lShowKeyboard AS LOGICAL NO-UNDO.
    
    {methods/run_link.i "CONTAINER-SOURCE" "GetKeyboard" "(OUTPUT oKeyboard)"}
    {methods/run_link.i "SPLIT-SOURCE" "DisableErrorAlerts"}
    {methods/run_link.i "CONTAINER-SOURCE" "ShowKeyboard" "(OUTPUT lShowKeyboard)"}
    {methods/run_link.i "SPLIT-SOURCE" "SetKeyboard" "(INPUT oKeyboard)"}
    
    IF lShowKeyboard THEN
        RUN ShowKeyboard.
    
    RUN Set-Focus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSendError F-Frame-Win 
PROCEDURE pSendError :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN new-state (
        "split-error"
        ).

    ASSIGN
        cStatusMessage     = ""
        iStatusMessageType = 0
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reset F-Frame-Win 
PROCEDURE Reset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    {methods/run_link.i "SPLIT-SOURCE" "EmptyTag"}
    {methods/run_link.i "SPLIT-SOURCE" "ScanNextTag"}
    
    fiQuantity:SCREEN-VALUE = "0.00".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartFrame, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus F-Frame-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {methods/run_link.i "SPLIT-SOURCE" "Set-Focus"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowKeyboard F-Frame-Win 
PROCEDURE ShowKeyboard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {methods/run_link.i "SPLIT-SOURCE" "ShowKeyboard"}

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        btnKeyboardQuantity:VISIBLE   = TRUE
        btnKeyboardQuantity:SENSITIVE = TRUE
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN new-state ("empty-message").
        
    CASE p-state:
        WHEN "tag-valid" THEN DO:
            {methods/run_link.i "SPLIT-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}
            
            fiQuantity:SCREEN-VALUE = "0".
            
            IF VALID-OBJECT(oLoadtag) AND NOT LOGICAL(oLoadTag:GetValue("ItemType")) THEN DO:
                IF NOT VALID-OBJECT(oFGBin) THEN
                    oFGBin = NEW fg.FGBin().
                    
                oFGBin:SetContext(oLoadTag:GetValue("Company"), oLoadtag:GetValue("ItemID"), oLoadtag:GetValue("Tag")).
                IF oFGBin:IsAvailable() THEN
                    fiQuantity:SCREEN-VALUE = oFGBin:GetValue("Quantity").
            END.
            
            btSplit:SENSITIVE = TRUE.
        END.
        WHEN "tag-invalid" THEN DO:
            btSplit:SENSITIVE = FALSE.
        END.
        WHEN "tag-error" THEN DO:
            {methods/run_link.i "SPLIT-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pSendError.
        END.        
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

