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

  File: sharpshooter/smartobj/adjustWindowSize.w

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
DEFINE VARIABLE iScreenHeight            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iScreenWidth             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iScreenTop               AS INTEGER   NO-UNDO.
DEFINE VARIABLE iScreenLeft              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWorkingAreaHeight       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWorkingAreaWidth        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFullScreenHeight        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFullScreenWidth         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWindowHeight            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWindowWidth             AS INTEGER   NO-UNDO.
DEFINE VARIABLE hdWindow                 AS HANDLE    NO-UNDO.
DEFINE VARIABLE cWindowName              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE dCharsToPixelRatioHeight AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCharsToPixelRatioWidth  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iWindowsHeightChange     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWindowsWidthChange      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cShowWindowControls      AS CHARACTER NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btFullScreen btFullScreenWorkingArea btMinus ~
btPlus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btFullScreen 
     IMAGE-UP FILE "Graphics/16x16/fullscreen.png":U NO-FOCUS FLAT-BUTTON
     LABEL "FullScreen" 
     SIZE 8 BY 1.91 TOOLTIP "Full Screen".

DEFINE BUTTON btFullScreenWorkingArea 
     IMAGE-UP FILE "Graphics/16x16/fullscreen_workingarea.png":U NO-FOCUS FLAT-BUTTON
     LABEL "WorkingArea" 
     SIZE 8 BY 1.91 TOOLTIP "Full Screen (Except taskbar)".

DEFINE BUTTON btMinus 
     IMAGE-UP FILE "Graphics/16x16/minus_sign.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Minus" 
     SIZE 8 BY 1.91 TOOLTIP "Decrease Screen Size".

DEFINE BUTTON btPlus 
     IMAGE-UP FILE "Graphics/16x16/plus_sign.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Plus" 
     SIZE 8 BY 1.91 TOOLTIP "Increase Screen Size".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btFullScreen AT ROW 1 COL 25 WIDGET-ID 12
     btFullScreenWorkingArea AT ROW 1 COL 17 WIDGET-ID 10
     btMinus AT ROW 1 COL 1 WIDGET-ID 2
     btPlus AT ROW 1 COL 9 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15  WIDGET-ID 100.


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
         WIDTH              = 50.
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

&Scoped-define SELF-NAME btFullScreen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFullScreen s-object
ON CHOOSE OF btFullScreen IN FRAME F-Main /* FullScreen */
DO:
    {methods/run_link.i "CONTAINER-SOURCE" "ChangeWindowSize" "(iFullScreenHeight, iFullScreenWidth, 0,0)"}
    
    RUN pSaveWindowSize(iFullScreenHeight, iFullScreenWidth).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFullScreenWorkingArea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFullScreenWorkingArea s-object
ON CHOOSE OF btFullScreenWorkingArea IN FRAME F-Main /* WorkingArea */
DO:
    RUN pUpdateScreenTopLeftPostion.
    
    {methods/run_link.i "CONTAINER-SOURCE" "ChangeWindowSize" "(iWorkingAreaHeight, iWorkingAreaWidth, iScreenTop, iScreenLeft)"}
    
    RUN pSaveWindowSize(iWorkingAreaHeight, iWorkingAreaWidth).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMinus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMinus s-object
ON CHOOSE OF btMinus IN FRAME F-Main /* Minus */
DO:
    IF iWindowHeight - iWindowsHeightChange LT iWorkingAreaHeight * 0.6 THEN
        RETURN.
    
    IF iWindowWidth - iWindowsWidthChange LT iWorkingAreaWidth * 0.6 THEN
        RETURN.
    
    /* To make sure we always decrease window size based on working area */
    IF iWindowWidth GT iWorkingAreaWidth THEN
        iWindowWidth = iWorkingAreaWidth.

    IF iWindowHeight GT iWorkingAreaHeight THEN
        iWindowHeight = iWorkingAreaHeight.
            
    ASSIGN
        iWindowHeight = iWindowHeight - iWindowsHeightChange
        iWindowWidth  = iWindowWidth - iWindowsWidthChange
        .
    
    {methods/run_link.i "CONTAINER-SOURCE" "ChangeWindowSize" "(iWindowHeight, iWindowWidth, ?, ?)"}
    
    RUN pSaveWindowSize(iWindowHeight, iWindowWidth).           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPlus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPlus s-object
ON CHOOSE OF btPlus IN FRAME F-Main /* Plus */
DO:
    IF iWindowHeight + iWindowsHeightChange GT iWorkingAreaHeight THEN
        RETURN.

    IF iWindowWidth + iWindowsWidthChange GT iWorkingAreaWidth THEN
        RETURN.
                
    ASSIGN
        iWindowHeight = iWindowHeight + iWindowsHeightChange
        iWindowWidth  = iWindowWidth + iWindowsWidthChange
        .
    
    {methods/run_link.i "CONTAINER-SOURCE" "ChangeWindowSize" "(iWindowHeight, iWindowWidth, ?, ?)"}
    
    RUN pSaveWindowSize(iWindowHeight, iWindowWidth).     
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
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN spGetSessionParam ("UserID", OUTPUT cUser).
    RUN spGetSettingByName ("ShowWindowControls", OUTPUT cShowWindowControls).
    
    RUN ShowHideWindowControls(INPUT cShowWindowControls EQ "YES").
    
    DEFINE BUFFER bf-userWindow FOR userWindow.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'CONTAINER-SOURCE':U,OUTPUT char-hdl).
    
    hdWindow = HANDLE(char-hdl) NO-ERROR.
    
    IF VALID-HANDLE (hdWindow) THEN
        cWindowName = hdWindow:NAME.

    RUN spGetScreenWorkingAreaSize (OUTPUT iWorkingAreaWidth, OUTPUT iWorkingAreaHeight).
    RUN spGetScreenSize (OUTPUT iFullScreenWidth, OUTPUT iFullScreenHeight).
    RUN spGetScreenStartPosition (OUTPUT iScreenTop, OUTPUT iScreenLeft).

    ASSIGN
        dCharsToPixelRatioWidth  = btFullScreen:WIDTH-PIXELS / btFullScreen:WIDTH-CHARS
        dCharsToPixelRatioHeight = btFullScreen:HEIGHT-PIXELS / btFullScreen:HEIGHT-CHARS
        iWindowsHeightChange     = iWorkingAreaHeight * 0.05
        iWindowsWidthChange      = iWorkingAreaWidth * 0.05
        .
      
    FIND FIRST bf-userWindow NO-LOCK 
         WHERE bf-userWindow.usrId       EQ cUser
           AND bf-userWindow.programName EQ cWindowName
         NO-ERROR.      
    IF NOT AVAILABLE bf-userWindow THEN DO:                   
        CREATE bf-userWindow.
        ASSIGN 
            bf-userWindow.usrId         = cUser
            bf-userWindow.programName   = cWindowName
            bf-userWindow.sessionWidth  = SESSION:WIDTH-PIXELS
            bf-userWindow.sessionHeight = SESSION:HEIGHT-PIXELS
            bf-userWindow.winWidth      = iWorkingAreaWidth / dCharsToPixelRatioWidth 
            bf-userWindow.winHeight     = iWorkingAreaHeight / dCharsToPixelRatioHeight  
            bf-userWindow.winXpos       = iScreenLeft
            bf-userWindow.winYpos       = iScreenTop  
            bf-userWindow.state         = 1
            .
    END.     

    IF (bf-userWindow.winHeight EQ ? OR bf-userWindow.winHeight EQ 0) OR 
       (bf-userWindow.winWidth EQ ? OR bf-userWindow.winWidth EQ 0) THEN DO:
        FIND CURRENT bf-userWindow EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bf-userWindow THEN DO:
            ASSIGN 
                bf-userWindow.winWidth      = iWorkingAreaWidth / dCharsToPixelRatioWidth 
                bf-userWindow.winHeight     = iWorkingAreaHeight / dCharsToPixelRatioHeight  
                bf-userWindow.winXpos       = iScreenLeft
                bf-userWindow.winYpos       = iScreenTop  
                .
        END.
    END.
    
    ASSIGN 
        iScreenHeight = bf-userWindow.winHeight * dCharsToPixelRatioHeight
        iScreenWidth  = bf-userWindow.winWidth * dCharsToPixelRatioWidth
        iScreenTop    = bf-userWindow.winYpos
        iScreenLeft   = bf-userWindow.winXpos
        iWindowHeight = iScreenHeight
        iWindowWidth  = iScreenWidth
        .   
    
    IF iScreenHeight LT iWorkingAreaHeight * 0.60 THEN
        iScreenHeight = iWorkingAreaHeight * 0.60.

    IF iScreenWidth LT iWorkingAreaWidth * 0.60 THEN
        iScreenWidth = iWorkingAreaWidth * 0.60.

    IF iScreenHeight GT iWorkingAreaHeight THEN
        iScreenHeight = iWorkingAreaHeight.
    
    IF iScreenWidth GT iWorkingAreaWidth THEN
        iScreenWidth = iWorkingAreaWidth.

    {methods/run_link.i "CONTAINER-SOURCE" "ChangeWindowSize" "(iScreenHeight, iScreenWidth, iScreenTop, iScreenLeft)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveWindowSize s-object 
PROCEDURE pSaveWindowSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiScreenHeight AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiScreenWidth  AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-userWindow FOR userWindow.

    FIND FIRST bf-userWindow EXCLUSIVE-LOCK 
         WHERE bf-userWindow.usrId       EQ cUser
           AND bf-userWindow.programName EQ cWindowName
         NO-ERROR.      
    IF AVAILABLE bf-userWindow THEN DO: 
        ASSIGN
            bf-userWindow.winWidth      = ipiScreenWidth / dCharsToPixelRatioWidth 
            bf-userWindow.winHeight     = ipiScreenHeight / dCharsToPixelRatioHeight
            iWindowHeight               = ipiScreenHeight
            iWindowWidth                = ipiScreenWidth
            .  
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateScreenTopLeftPostion s-object
PROCEDURE pUpdateScreenTopLeftPostion:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN spGetScreenStartPosition (OUTPUT iScreenTop, OUTPUT iScreenLeft).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowHideWindowControls s-object
PROCEDURE ShowHideWindowControls:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplShowHide AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
            
    ASSIGN
        btFullScreen:VISIBLE            = iplShowHide
        btFullScreenWorkingArea:VISIBLE = iplShowHide
        btMinus:VISIBLE                 = iplShowHide
        btPlus:VISIBLE                  = iplShowHide
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

