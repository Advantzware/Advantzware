&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: paramSetBldr.w

  Description: Parameter Set Builder

  Input Parameters: Parent Handle, Type, Subject ID

  Output Parameters: <none>

  Author: Ron Stark

  Created: 2.25.2019

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER iphParent AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER ipcType   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiID     AS INTEGER   NO-UNDO.
&ELSE
DEFINE VARIABLE iphParent AS HANDLE    NO-UNDO.
DEFINE VARIABLE ipcType   AS CHARACTER NO-UNDO INITIAL "Set".
DEFINE VARIABLE ipiID     AS INTEGER   NO-UNDO INITIAL 3.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cPoolName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE hCalendar      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynInitProc   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hRectangle     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hWidget        AS HANDLE    NO-UNDO.
DEFINE VARIABLE svSetAlignment AS CHARACTER NO-UNDO INITIAL "Custom".

{AOA/tempTable/ttDynAction.i}
{AOA/includes/dynFuncs.i}

RUN AOA/spDynInitializeProc.p  PERSISTENT SET hDynInitProc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME paramFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cParamID 
&Scoped-Define DISPLAYED-OBJECTS cParamID cParamIDText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDateOptions C-Win 
FUNCTION fDateOptions RETURNS CHARACTER
  (iphWidget AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.ico":U NO-FOCUS
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/floppy_disk_disabled.ico":U NO-FOCUS
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 18 BY 2.38
     BGCOLOR 15 FGCOLOR 15 .

DEFINE VARIABLE cParamIDText AS CHARACTER FORMAT "X(256)":U INITIAL "Available Parameters (double click)" 
      VIEW-AS TEXT 
     SIZE 35 BY .62 NO-UNDO.

DEFINE VARIABLE cParamID AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SORT 
     SIZE 38 BY 17.14 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME paramFrame
     cParamID AT ROW 5.52 COL 121 NO-LABEL WIDGET-ID 2
     cParamIDText AT ROW 4.81 COL 122 COLON-ALIGNED NO-LABEL WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 158 BY 21.71
         FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME outputFrame
     btnReset AT ROW 1.48 COL 11 HELP
          "Reset" WIDGET-ID 8
     btnSave AT ROW 1.48 COL 3 HELP
          "Save" WIDGET-ID 6
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 139 ROW 1
         SIZE 20 BY 3.81
         BGCOLOR 1 FGCOLOR 15 
         TITLE BGCOLOR 15 FGCOLOR 1 " Save / Reset" WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Dynamic Parameter Set Builder"
         HEIGHT             = 21.71
         WIDTH              = 158
         MAX-HEIGHT         = 21.71
         MAX-WIDTH          = 158
         VIRTUAL-HEIGHT     = 21.71
         VIRTUAL-WIDTH      = 158
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics/32x32/jss_icon_32.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/32x32/jss_icon_32.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME outputFrame:FRAME = FRAME paramFrame:HANDLE.

/* SETTINGS FOR FRAME outputFrame
                                                                        */
/* SETTINGS FOR BUTTON btnReset IN FRAME outputFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSave IN FRAME outputFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME outputFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME paramFrame
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME outputFrame:MOVE-BEFORE-TAB-ITEM (cParamID:HANDLE IN FRAME paramFrame)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN cParamIDText IN FRAME paramFrame
   NO-ENABLE                                                            */
ASSIGN 
       cParamIDText:HIDDEN IN FRAME paramFrame           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME outputFrame
/* Query rebuild information for FRAME outputFrame
     _Query            is NOT OPENED
*/  /* FRAME outputFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME paramFrame
/* Query rebuild information for FRAME paramFrame
     _Query            is NOT OPENED
*/  /* FRAME paramFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dynamic Parameter Set Builder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamic Parameter Set Builder */
DO:
  IF VALID-HANDLE(iphParent) AND ipcType EQ "Set" AND
     INDEX(iphParent:INTERNAL-ENTRIES,"pGetParamSetID") NE 0 THEN
  RUN pGetParamSetID IN iphParent.
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME outputFrame /* Reset */
DO:
    RUN pReset (ipiID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME outputFrame /* Save */
DO:
    RUN pSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME cParamID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cParamID C-Win
ON DEFAULT-ACTION OF cParamID IN FRAME paramFrame
DO:
    RUN pAddParameter (INTEGER(SELF:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pSetFrameGrid (FRAME paramFrame:HANDLE).
  RUN enable_UI.
  ASSIGN
    cParamID:HIDDEN = ipcType EQ "Param"
    cParamIDText:HIDDEN = cParamID:HIDDEN
    .
  IF ipcType EQ "Set" THEN
  RUN pGetDynParam.
  APPLY "CHOOSE":U TO BtnReset.
  FRAME {&FRAME-NAME}:HIDDEN = NO.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{AOA/includes/dynWidgets.i "dyn" "ParamBuilder"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY cParamID cParamIDText 
      WITH FRAME paramFrame IN WINDOW C-Win.
  ENABLE cParamID 
      WITH FRAME paramFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
  VIEW FRAME outputFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-outputFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddParameter C-Win 
PROCEDURE pAddParameter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiParamID AS INTEGER NO-UNDO.

    RUN pSave.
    FIND FIRST dynParam NO-LOCK
         WHERE dynParam.paramID EQ ipiParamID
         NO-ERROR.
    IF AVAILABLE dynParam THEN DO:
        CREATE dynParamSetDtl.
        ASSIGN
            dynParamSetDtl.paramSetID     = ipiID
            dynParamSetDtl.paramID        = ipiParamID
            dynParamSetDtl.paramName      = dynParam.paramName
            dynParamSetDtl.paramLabel     = dynParam.paramLabel
            dynParamSetDtl.initialItems   = dynParam.initialItems
            dynParamSetDtl.initialValue   = dynParam.initialValue
            dynParamSetDtl.initializeProc = dynParam.initializeProc
            dynParamSetDtl.validateProc   = dynParam.validateProc
            dynParamSetDtl.paramHeight    = dynParam.paramHeight
            dynParamSetDtl.paramWidth     = dynParam.paramWidth
            dynParamSetDtl.action         = dynParam.action
            dynParamSetDtl.paramCol       = 20
            dynParamSetDtl.paramRow       = 1
            .
        RUN pReset (ipiID).
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDynParameter C-Win 
PROCEDURE pDynParameter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiParamID AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE dCol AS DECIMAL NO-UNDO INITIAL 30.
    DEFINE VARIABLE dRow AS DECIMAL NO-UNDO INITIAL 4.
    
    FIND FIRST dynParam NO-LOCK
         WHERE dynParam.paramID EQ ipiParamID
         NO-ERROR.
    IF NOT AVAILABLE dynParam THEN RETURN.
    ASSIGN
        cAction       = dynParam.action
        cInitialItems = dynParam.initialItems
        cInitialValue = dynParam.initialValue
        cParamLabel   = dynParam.paramLabel
        cParamName    = dynParam.paramName
        dParamHeight  = dynParam.paramHeight
        dParamWidth   = dynParam.paramWidth
        lMovable      = NO
        lResizable    = YES
        lSelectable   = YES
        lSensitive    = YES
        .
    RUN pViewAs (FRAME paramFrame:HANDLE, dCol, dRow).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDynParameterSet C-Win 
PROCEDURE pDynParameterSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiParamSetID AS INTEGER NO-UNDO.

    DEFINE VARIABLE dCol   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE hFrame AS HANDLE  NO-UNDO.
    
    FIND FIRST dynParamSet NO-LOCK
         WHERE dynParamSet.paramSetID EQ ipiParamSetID
         NO-ERROR.
    IF NOT AVAILABLE dynParamSet THEN RETURN.
    ASSIGN
        lMovable      = YES
        lResizable    = YES
        lSelectable   = YES
        lSensitive    = YES
        lIsVisible    = YES
        .
    RUN pFrame (
        cPoolName,
        FRAME paramFrame:HANDLE,
        STRING(dynParamSet.paramSetID),
        1,
        1,
        dynParamSet.setWidth,
        dynParamSet.setHeight,
        NO,
        YES,
        dynParamSet.setName,
        lIsVisible,
        OUTPUT hFrame
        ).
    RUN pSetFrameGrid (hFrame:HANDLE).
    hFrame:HIDDEN = NO.
    hFrame:MOVE-TO-TOP().
    IF dynParamSet.setRectangle THEN DO:
        RUN pRectangle (
            cPoolName,
            hFrame,
            "1",
            hFrame:COL + 1,
            hFrame:ROW + .48,
            dynParamSet.setWidth - 2.2,
            dynParamSet.setHeight - 1.6,
            lIsVisible,
            OUTPUT hRectangle
            ).
        hRectangle:HIDDEN = NO.
        hRectangle:MOVE-TO-TOP().
        IF dynParamSet.setTitle NE "" THEN
        RUN pText (
            cPoolName,
            hFrame,
            hRectangle:COL + 2,
            hRectangle:ROW - .24,
            dynParamSet.setTitle,
            lIsVisible
            ).
    END. /* if rectangle */
    FOR EACH dynParamSetDtl NO-LOCK
        WHERE dynParamSetDtl.paramSetID EQ dynParamSet.paramSetID,
        FIRST dynParam NO-LOCK
        WHERE dynParam.paramID EQ dynParamSetDtl.paramID
        :
        ASSIGN
            cAction       = dynParamSetDtl.action
            cInitialItems = dynParamSetDtl.initialItems
            cInitialValue = dynParamSetDtl.initialValue
            cParamLabel   = dynParamSetDtl.paramLabel
            cParamName    = dynParamSetDtl.paramName
            dParamHeight  = dynParamSetDtl.paramHeight
            dParamWidth   = dynParamSetDtl.paramWidth
            dCol          = dynParamSetDtl.paramCol
            dRow          = dynParamSetDtl.paramRow
            .
        RUN pViewAs (hFrame:HANDLE, dCol, dRow).
    END. /* each dynparamsetdtl */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFrameResize C-Win 
PROCEDURE pFrameResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.
    
    RUN pSetSaveReset (YES).
    ASSIGN
        iphFrame:VIRTUAL-WIDTH  = iphFrame:WIDTH
        iphFrame:VIRTUAL-HEIGHT = iphFrame:HEIGHT
        .
    IF dynParamSet.setRectangle THEN
    ASSIGN
        hRectangle:WIDTH  = iphFrame:WIDTH  - 2.2
        hRectangle:HEIGHT = iphFrame:HEIGHT - 1.6
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDynParam C-Win 
PROCEDURE pGetDynParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        cParamID:LIST-ITEM-PAIRS = ?.
        FOR EACH dynParam NO-LOCK:
            cParamID:ADD-LAST(
                dynParam.paramLabel + " (" +
                dynParam.paramName + ")",
                STRING(dynParam.paramID)
                ).
        END. /* each dynparam */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParamValidate C-Win 
PROCEDURE pParamValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReset C-Win 
PROCEDURE pReset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiValue AS INTEGER NO-UNDO.
    
    DELETE WIDGET-POOL cPoolName NO-ERROR.
    cPoolName = "ParameterPool" + STRING(TIME).
    CREATE WIDGET-POOL cPoolName PERSISTENT.

    ipiID = ipiValue.
    CASE ipcType:
        WHEN "Param" THEN
        RUN pDynParameter (ipiID).
        WHEN "Set" THEN
        RUN pDynParameterSet (ipiID).
    END CASE.
    RUN pSetSaveReset (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSave C-Win 
PROCEDURE pSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hObject AS HANDLE NO-UNDO EXTENT 2.
    DEFINE VARIABLE rRowID  AS ROWID  NO-UNDO.

    CASE ipcType:
        WHEN "Param" THEN DO TRANSACTION:
            FIND CURRENT dynParam EXCLUSIVE-LOCK.
            ASSIGN
                dynParam.paramWidth  = hWidget:WIDTH
                dynParam.paramHeight = hWidget:HEIGHT
                .
            FIND CURRENT dynParam NO-LOCK.
        END. /* param */
        WHEN "Set" THEN DO TRANSACTION:
            ASSIGN
                hObject[1] = FRAME paramFrame:HANDLE
                hObject[1] = hObject[1]:FIRST-CHILD
                hObject[1] = hObject[1]:FIRST-CHILD
                .
            DO WHILE VALID-HANDLE(hObject[1]):
                IF hObject[1]:TYPE EQ "FRAME" AND
                   hObject[1]:NAME BEGINS "SetFrame" THEN DO:
                    FIND CURRENT dynParamSet EXCLUSIVE-LOCK.
                    ASSIGN
                        dynParamSet.setWidth  = hObject[1]:WIDTH
                        dynParamSet.setHeight = hObject[1]:HEIGHT
                        hObject[2] = hObject[1]:HANDLE
                        hObject[2] = hObject[2]:FIRST-CHILD
                        hObject[2] = hObject[2]:FIRST-CHILD
                        .
                    FIND CURRENT dynParamSet NO-LOCK.
                    DO WHILE VALID-HANDLE(hObject[2]):
                        rRowID = TO-ROWID(hObject[2]:PRIVATE-DATA).
                        IF rRowID NE ? THEN DO:
                            FIND FIRST dynParamSetDtl EXCLUSIVE-LOCK
                                 WHERE ROWID(dynParamSetDtl) EQ rRowID
                                 NO-ERROR.
                            IF AVAILABLE dynParamSetDtl THEN DO:
                                ASSIGN
                                    dynParamSetDtl.paramCol    = hObject[2]:COL
                                    dynParamSetDtl.paramRow    = hObject[2]:ROW
                                    dynParamSetDtl.paramWidth  = hObject[2]:WIDTH
                                    dynParamSetDtl.paramHeight = hObject[2]:HEIGHT
                                    .
                                RELEASE dynParamSetDtl.
                            END. /* if avail */
                        END. /* if rrowid */
                        hObject[2] = hObject[2]:NEXT-SIBLING.
                    END. /* do while */
                END. /* if type frame */
                hObject[1] = hObject[1]:NEXT-SIBLING.
            END. /* do while */
        END. /* set */
    END CASE.
    IF VALID-HANDLE(iphParent) THEN
    RUN pRefresh IN iphParent.
    RUN pSetSaveReset (NO).
    RUN pReset (ipiID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetFrameGrid C-Win 
PROCEDURE pSetFrameGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.

    ASSIGN
        iphFrame:GRID-FACTOR-VERTICAL    = 4
        iphFrame:GRID-FACTOR-HORIZONTAL  = 10
        iphFrame:GRID-UNIT-WIDTH-PIXELS  = 5
        iphFrame:GRID-UNIT-HEIGHT-PIXELS = 5
        iphFrame:GRID-SNAP               = YES
        iphFrame:GRID-VISIBLE            = YES
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetSaveReset C-Win 
PROCEDURE pSetSaveReset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplSensitive AS LOGICAL NO-UNDO.
    
    DO WITH FRAME outputFrame:
        ASSIGN
            btnSave:SENSITIVE  = iplSensitive
            btnReset:SENSITIVE = iplSensitive
            .
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewAs C-Win 
PROCEDURE pViewAs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol   AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow   AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE jdx AS INTEGER NO-UNDO.
    DEFINE VARIABLE kdx AS INTEGER NO-UNDO.

    ASSIGN
        lIsVisible = YES
        lShowLabel = YES
        .
    IF CAN-DO(cAction,"LIST-ITEM-PAIRS") AND cInitialItems EQ "" THEN
    cInitialItems = ",". 
    CASE dynParam.viewAs:
        WHEN "COMBO-BOX" THEN
        RUN pComboBox (
            cPoolName,
            iphFrame,
            cParamLabel,
            cParamName,
            ipdCol,
            ipdRow,
            dParamWidth,
            CAN-DO(cAction,"LIST-ITEM-PAIRS"),
            cInitialItems,
            dynParam.paramFormat,
            cInitialValue,
            dynParam.innerLines,
            lSensitive,
            lIsVisible,
            OUTPUT hWidget
            ).
        WHEN "EDITOR" THEN DO:
            RUN pEditor (
                cPoolName,
                iphFrame,
                cParamLabel,
                cParamName,
                ipdCol,
                ipdRow,
                dParamWidth,
                dParamHeight,
                CAN-DO(cAction,"HORIZONTAL"),
                CAN-DO(cAction,"VERTICAL"),
                cInitialValue,
                lSensitive,
                lIsVisible,
                OUTPUT hWidget
                ).
            IF CAN-DO(cAction,"EMAIL") THEN DO:
                ASSIGN
                    ipdCol = ipdCol - 5
                    ipdRow = ipdRow + .95
                    .
                RUN pButtonEmail (
                    cPoolName,
                    iphFrame,
                    ipdCol,
                    ipdRow,
                    NO,
                    hWidget,
                    OUTPUT hCalendar
                    ).
            END. /* if use a calendar */
        END. /* editor */
        WHEN "FILL-IN" THEN DO:
            RUN pFillIn (
                cPoolName,
                iphFrame,
                cParamLabel,
                cParamName,
                dynParam.dataType,
                dynParam.paramFormat,
                ipdCol,
                ipdRow,
                dParamWidth,
                dParamHeight,
                cInitialValue,
                lSensitive,
                lIsVisible,
                OUTPUT ipdCol,
                OUTPUT hWidget
                ).
            hCalendar = ?.
            IF dynParam.dataType EQ "DATE" THEN DO:
                IF CAN-DO(cAction,"CALENDAR") THEN DO:
                    jdx = jdx + 1.
                    RUN pButtonCalendar (
                        cPoolName,
                        iphFrame,
                        STRING(jdx),
                        ipdCol,
                        ipdRow,
                        NO,
                        lIsVisible,
                        hWidget,
                        OUTPUT ipdCol,
                        OUTPUT hCalendar
                        ).
                END. /* if use a calendar */
                IF CAN-DO(cAction,"DATEPICKLIST") THEN DO:
                    kdx = kdx + 1.
                    RUN pPickList (
                        cPoolName,
                        iphFrame,
                        STRING(kdx),
                        ipdCol,
                        ipdRow,
                        NO,
                        lIsVisible,
                        hWidget,
                        hCalendar
                        ).
                END. /* if date pick list */
            END. /* if date type */
        END. /* fill-in */
        WHEN "RADIO-SET" THEN
        RUN pRadioSet (
            cPoolName,
            iphFrame,
            cParamLabel,
            cParamName,
            cInitialItems,
            CAN-DO(cAction,"HORIZONTAL"),
            ipdCol,
            ipdRow,
            dParamWidth,
            dParamHeight,
            cInitialValue,
            lSensitive,
            lIsVisible,
            OUTPUT hWidget
            ).
        WHEN "SELECTION-LIST" THEN DO:
        RUN pSelectionList (
            cPoolName,
            iphFrame,
            cParamLabel,
            cParamName,
            ipdCol,
            ipdRow,
            dParamWidth,
            dParamHeight,
            CAN-DO(cAction,"MULTISELECT"),
            cInitialItems,
            cInitialValue,
            lSensitive,
            lIsVisible,
            OUTPUT hWidget
            ).
        END. /* selection-list */
        WHEN "TOGGLE-BOX" THEN
        RUN pToggleBox (
            cPoolName,
            iphFrame,
            cParamLabel,
            cParamName,
            ipdCol,
            ipdRow,
            dParamWidth,
            dParamHeight,
            cInitialValue,
            lSensitive,
            lIsVisible,
            OUTPUT hWidget
            ).
    END CASE.
    IF VALID-HANDLE(hWidget) THEN DO:
        ASSIGN
            hWidget:HIDDEN   = NO
            hWidget:SELECTED = ipcType EQ "Param"
            hWidget:PRIVATE-DATA = IF ipcType EQ "Param" THEN STRING(ROWID(dynParam))
                                   ELSE STRING(ROWID(dynParamSetDtl))
            .
        hWidget:MOVE-TO-TOP().
    END. /* if valid handle */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDateOptions C-Win 
FUNCTION fDateOptions RETURNS CHARACTER
  (iphWidget AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    iphWidget:LIST-ITEMS = "Date Options".

    RETURN iphWidget:LIST-ITEMS.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

