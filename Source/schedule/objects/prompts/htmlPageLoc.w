&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: htmlPageLoc.w

  Description: Prompts for HTML Page Location

  Input Parameters: <none>

  Output Parameters: opcHTMLPageLocation

  Author: Ron Stark

  Created: 11.1.2017
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT        PARAMETER ipcID                AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcHTMLPageLocation AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiHTMLCapacityDays AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiHTMLPopupWidth   AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiHTMLPopupHeight  AS INTEGER   NO-UNDO.
&ELSE
DEFINE VARIABLE ipcID                AS CHARACTER NO-UNDO INITIAL "ASI/Folding".
DEFINE VARIABLE iopcHTMLPageLocation AS CHARACTER NO-UNDO INITIAL "C:\tmp".
DEFINE VARIABLE iopiHTMLCapacityDays AS INTEGER   NO-UNDO INITIAL 7.
DEFINE VARIABLE iopiHTMLPopupWidth   AS INTEGER   NO-UNDO INITIAL 900.
DEFINE VARIABLE iopiHTMLPopupHeight  AS INTEGER   NO-UNDO INITIAL 600.
&ENDIF

/* Local Variable Definitions ---                                       */

{schedule/scopDir.i}
{{&includes}/defBoard.i}

DEFINE VARIABLE idx AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttAvailFields NO-UNDO
    FIELD fieldLabel  AS CHARACTER
    FIELD fieldName   AS CHARACTER
    FIELD fieldFormat AS CHARACTER
        INDEX ttAvailFields IS PRIMARY fieldLabel fieldName
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnExit btnReset btnSave htmlPageLocation ~
btnDir htmlPopupWidth htmlPopupHeight htmlCapacityDays availableFields ~
htmlJobFields btnUp-1 btnDown-1 htmlPendingFields btnUp-2 btnDown-2 ~
htmlFieldsFocus 
&Scoped-Define DISPLAYED-OBJECTS htmlPageLocation htmlPopupWidth ~
htmlPopupHeight htmlCapacityDays availableFields htmlJobFields ~
htmlPendingFields htmlFieldsFocus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDir 
     LABEL "..." 
     SIZE 5 BY 1.

DEFINE BUTTON btnDown-1 
     IMAGE-UP FILE "schedule/images/down.bmp":U
     LABEL "&DN" 
     SIZE 5 BY 1.1 TOOLTIP "Move Current Column and Row Down (Alt-D)".

DEFINE BUTTON btnDown-2 
     IMAGE-UP FILE "schedule/images/down.bmp":U
     LABEL "&DN" 
     SIZE 5 BY 1.1 TOOLTIP "Move Current Column and Row Down (Alt-D)".

DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 6 BY 1.43 TOOLTIP "Exit"
     BGCOLOR 8 .

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 6 BY 1.43 TOOLTIP "Reset"
     BGCOLOR 8 .

DEFINE BUTTON btnSave AUTO-GO 
     IMAGE-UP FILE "schedule/images/commit.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 6 BY 1.43 TOOLTIP "Save"
     BGCOLOR 8 .

DEFINE BUTTON btnUp-1 
     IMAGE-UP FILE "schedule/images/up.bmp":U
     LABEL "&UP" 
     SIZE 5 BY 1.1 TOOLTIP "Move Current Column and Row Up (Alt-U)".

DEFINE BUTTON btnUp-2 
     IMAGE-UP FILE "schedule/images/up.bmp":U
     LABEL "&UP" 
     SIZE 5 BY 1.1 TOOLTIP "Move Current Column and Row Up (Alt-U)".

DEFINE VARIABLE htmlCapacityDays AS INTEGER FORMAT ">>9":U INITIAL 7 
     LABEL "Capacity Days to View" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "Number of Capacity Days to View" NO-UNDO.

DEFINE VARIABLE htmlPageLocation AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 78 BY 1 NO-UNDO.

DEFINE VARIABLE htmlPopupHeight AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Popup Height" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "Popup Height" NO-UNDO.

DEFINE VARIABLE htmlPopupWidth AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Popup Width" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "Popup Width" NO-UNDO.

DEFINE VARIABLE htmlFieldsFocus AS CHARACTER INITIAL "Job" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Job Fields", "Job",
"Pending Fields", "Pending"
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE availableFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "","""" 
     SIZE 39 BY 20.95 NO-UNDO.

DEFINE VARIABLE htmlJobFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "","""" 
     SIZE 39 BY 10 NO-UNDO.

DEFINE VARIABLE htmlPendingFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "","""" 
     SIZE 39 BY 10
     BGCOLOR 8  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnExit AT ROW 25.52 COL 80
     btnReset AT ROW 25.52 COL 66 WIDGET-ID 34
     btnSave AT ROW 25.52 COL 73
     htmlPageLocation AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 2
     btnDir AT ROW 1.24 COL 81 WIDGET-ID 4
     htmlPopupWidth AT ROW 2.43 COL 14 COLON-ALIGNED WIDGET-ID 36
     htmlPopupHeight AT ROW 2.43 COL 38 COLON-ALIGNED WIDGET-ID 38
     htmlCapacityDays AT ROW 2.43 COL 71 COLON-ALIGNED HELP
          "Enter Number of Capacity Days to View" WIDGET-ID 6
     availableFields AT ROW 4.33 COL 1 NO-LABEL WIDGET-ID 8
     htmlJobFields AT ROW 4.33 COL 41 NO-LABEL WIDGET-ID 10
     btnUp-1 AT ROW 7.91 COL 81 HELP
          "Click to Move Current Column and Row Up" WIDGET-ID 28
     btnDown-1 AT ROW 9.1 COL 81 HELP
          "Click to Move Current Column and Row Down" WIDGET-ID 26
     htmlPendingFields AT ROW 15.29 COL 41 NO-LABEL WIDGET-ID 12
     btnUp-2 AT ROW 19.1 COL 81 HELP
          "Click to Move Current Column and Row Up" WIDGET-ID 32
     btnDown-2 AT ROW 20.29 COL 81 HELP
          "Click to Move Current Column and Row Down" WIDGET-ID 30
     htmlFieldsFocus AT ROW 25.76 COL 31 NO-LABEL WIDGET-ID 20
     "Add Available Fields To HTML" VIEW-AS TEXT
          SIZE 28 BY 1 AT ROW 25.76 COL 2 WIDGET-ID 24
     "HTML Pending Fields" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 14.57 COL 42 WIDGET-ID 18
     "HTML Job Fields" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 3.62 COL 42 WIDGET-ID 16
     "Available Fields" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 3.62 COL 2 WIDGET-ID 14
     SPACE(68.99) SKIP(22.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Set HTML Page Location and Layout"
         CANCEL-BUTTON btnExit WIDGET-ID 100.


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

/* SETTINGS FOR FILL-IN htmlPageLocation IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Set HTML Page Location and Layout */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME availableFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL availableFields Dialog-Frame
ON DEFAULT-ACTION OF availableFields IN FRAME Dialog-Frame
DO:
    idx = LOOKUP(availableFields:SCREEN-VALUE,availableFields:LIST-ITEM-PAIRS).
    CASE htmlFieldsFocus:
        WHEN "Job" THEN DO:
            IF CAN-DO(htmlJobFields:LIST-ITEM-PAIRS,availableFields:SCREEN-VALUE) THEN
            RETURN NO-APPLY.
            htmlJobFields:ADD-LAST(ENTRY(idx - 1,availableFields:LIST-ITEM-PAIRS),
                                   ENTRY(idx,availableFields:LIST-ITEM-PAIRS)).
        END.
        WHEN "Pending" THEN DO:
            IF CAN-DO(htmlPendingFields:LIST-ITEM-PAIRS,availableFields:SCREEN-VALUE) THEN
            RETURN NO-APPLY.
            htmlPendingFields:ADD-LAST(ENTRY(idx - 1,availableFields:LIST-ITEM-PAIRS),
                                       ENTRY(idx,availableFields:LIST-ITEM-PAIRS)).
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDir Dialog-Frame
ON CHOOSE OF btnDir IN FRAME Dialog-Frame /* ... */
DO:
    DEFINE VARIABLE cHTMLPageLocation AS CHARACTER NO-UNDO.

    SYSTEM-DIALOG GET-DIR cHTMLPageLocation TITLE "HTML Page Location".
    IF cHTMLPageLocation NE ? THEN
    ASSIGN
        htmlPageLocation:SCREEN-VALUE = cHTMLPageLocation
        htmlPageLocation
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown-1 Dialog-Frame
ON CHOOSE OF btnDown-1 IN FRAME Dialog-Frame /* DN */
DO:
    RUN pMoveField (htmlJobFields:HANDLE,2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown-2 Dialog-Frame
ON CHOOSE OF btnDown-2 IN FRAME Dialog-Frame /* DN */
DO:
    RUN pMoveField (htmlPendingFields:HANDLE,2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame
DO:
    RUN pGetHTMLFields (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame
DO:
    ASSIGN
        htmlPageLocation
        iopcHTMLPageLocation = htmlPageLocation
        htmlCapacityDays
        iopiHTMLCapacityDays = htmlCapacityDays
        htmlPopupWidth
        iopiHTMLPopupWidth   = htmlPopupWidth
        htmlPopupHeight
        iopiHTMLPopupHeight  = htmlPopupHeight
        .
    RUN pSaveHTMLFields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUp-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp-1 Dialog-Frame
ON CHOOSE OF btnUp-1 IN FRAME Dialog-Frame /* UP */
DO:
    RUN pMoveField (htmlJobFields:HANDLE,-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp-2 Dialog-Frame
ON CHOOSE OF btnUp-2 IN FRAME Dialog-Frame /* UP */
DO:
    RUN pMoveField (htmlPendingFields:HANDLE,-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME htmlFieldsFocus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL htmlFieldsFocus Dialog-Frame
ON VALUE-CHANGED OF htmlFieldsFocus IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    CASE {&SELF-NAME}:
        WHEN "Job" THEN
        ASSIGN
            htmlJobFields:BGCOLOR = ?
            htmlPendingFields:BGCOLOR = 8
            .
        WHEN "Pending" THEN
        ASSIGN
            htmlJobFields:BGCOLOR = 8
            htmlPendingFields:BGCOLOR = ?
            .
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME htmlJobFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL htmlJobFields Dialog-Frame
ON DEFAULT-ACTION OF htmlJobFields IN FRAME Dialog-Frame
DO:
    SELF:DELETE(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL htmlJobFields Dialog-Frame
ON ENTRY OF htmlJobFields IN FRAME Dialog-Frame
DO:
    ASSIGN
        SELF:BGCOLOR = ?
        htmlPendingFields:BGCOLOR = 8
        htmlFieldsFocus:SCREEN-VALUE = "Job"
        htmlFieldsFocus
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME htmlPendingFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL htmlPendingFields Dialog-Frame
ON DEFAULT-ACTION OF htmlPendingFields IN FRAME Dialog-Frame
DO:
    SELF:DELETE(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL htmlPendingFields Dialog-Frame
ON ENTRY OF htmlPendingFields IN FRAME Dialog-Frame
DO:
    ASSIGN
        SELF:BGCOLOR = ?
        htmlJobFields:BGCOLOR = 8
        htmlFieldsFocus:SCREEN-VALUE = "Pending"
        htmlFieldsFocus
        .
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
  RUN pGetHTMLFields (YES).
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
  DISPLAY htmlPageLocation htmlPopupWidth htmlPopupHeight htmlCapacityDays 
          availableFields htmlJobFields htmlPendingFields htmlFieldsFocus 
      WITH FRAME Dialog-Frame.
  ENABLE btnExit btnReset btnSave htmlPageLocation btnDir htmlPopupWidth 
         htmlPopupHeight htmlCapacityDays availableFields htmlJobFields btnUp-1 
         btnDown-1 htmlPendingFields btnUp-2 btnDown-2 htmlFieldsFocus 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetHTMLFields Dialog-Frame 
PROCEDURE pGetHTMLFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplInit AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldLabel  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHTMLFields  AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            htmlPageLocation:SCREEN-VALUE = iopcHTMLPageLocation
            htmlPageLocation
            htmlCapacityDays:SCREEN-VALUE = STRING(iopiHTMLCapacityDays)
            htmlCapacityDays
            htmlPopupWidth:SCREEN-VALUE   = STRING(iopiHTMLPopupWidth)
            htmlPopupWidth
            htmlPopupHeight:SCREEN-VALUE  = STRING(iopiHTMLPopupHeight)
            htmlPopupHeight
            .
    END. /* do frame */
    IF iplInit THEN DO:
        availableFields:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = ?.
        INPUT FROM VALUE(SEARCH("{&data}/rptFields.dat")) NO-ECHO.
        REPEAT:
            IMPORT cID cFieldLabel cFieldName cFieldFormat.
            IF cID EQ "" OR ipcID BEGINS cID THEN DO:
                CREATE ttAvailFields.
                ASSIGN
                    ttAvailFields.fieldLabel  = cFieldLabel
                    ttAvailFields.fieldName   = cFieldName
                    ttAvailFields.fieldFormat = cFieldFormat
                    .
                availableFields:ADD-LAST(cFieldLabel + " ("
                              + cFieldName + ")",
                                STRING(ROWID(ttAvailFields))).
            END. /* if cid */
        END. /* repeat */
        INPUT CLOSE.
    END. /* if init */
    RUN pSetHTMLFields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMoveField Dialog-Frame 
PROCEDURE pMoveField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphHTMLFields AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER ipiMove       AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSV AS CHARACTER NO-UNDO.

    IF iphHTMLFields:SCREEN-VALUE EQ ? THEN RETURN.
    DO jdx = 1 TO iphHTMLFields:NUM-ITEMS:
        IF iphHTMLFields:IS-SELECTED(jdx) THEN LEAVE.
    END.
    IF ipiMove LT 0 AND jdx EQ 1 THEN RETURN.
    IF ipiMove GT 0 AND jdx EQ iphHTMLFields:NUM-ITEMS THEN RETURN.
    ASSIGN
        cSV = iphHTMLFields:SCREEN-VALUE
        idx = LOOKUP(iphHTMLFields:SCREEN-VALUE,iphHTMLFields:LIST-ITEM-PAIRS)
        .
    iphHTMLFields:INSERT(ENTRY(idx - 1,iphHTMLFields:LIST-ITEM-PAIRS),
                         ENTRY(idx,iphHTMLFields:LIST-ITEM-PAIRS),
                         jdx + ipiMove).
    IF ipiMove LT 0 THEN iphHTMLFields:DELETE(jdx + 1).
    IF ipiMove GT 0 THEN iphHTMLFields:DELETE(jdx).
    iphHTMLFields:SCREEN-VALUE = cSV.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveHTMLFields Dialog-Frame 
PROCEDURE pSaveHTMLFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cHTMLFields  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.

    cHTMLFields = "{&data}/" + ipcID + "/htmlFields.dat".
    OUTPUT TO VALUE(cHTMLFields).
    DO idx = 1 TO htmlJobFields:NUM-ITEMS IN FRAME {&FRAME-NAME}:
        FIND FIRST ttAvailFields
             WHERE ROWID(ttAvailFields) EQ TO-ROWID(htmlJobFields:ENTRY(idx))
             NO-ERROR.
        IF AVAILABLE ttAvailFields THEN
        EXPORT
            "Job"
            ttAvailFields.fieldLabel
            ttAvailFields.fieldName
            ttAvailFields.fieldFormat
            .
    END. /* do idx */
    DO idx = 1 TO htmlPendingFields:NUM-ITEMS IN FRAME {&FRAME-NAME}:
        FIND FIRST ttAvailFields
             WHERE ROWID(ttAvailFields) EQ TO-ROWID(htmlPendingFields:ENTRY(idx))
             NO-ERROR.
        IF AVAILABLE ttAvailFields THEN
        EXPORT
            "Pending"
            ttAvailFields.fieldLabel
            ttAvailFields.fieldName
            ttAvailFields.fieldFormat
            .
    END. /* do idx */
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetHTMLFields Dialog-Frame 
PROCEDURE pSetHTMLFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFieldType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHTMLFields AS CHARACTER NO-UNDO.

    cHTMLFields = SEARCH("{&data}/" + ipcID + "/htmlFields.dat").
    IF cHTMLFields NE ? THEN DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            htmlJobFields:LIST-ITEM-PAIRS = ?
            htmlPendingFields:LIST-ITEM-PAIRS = ?
            .
        INPUT FROM VALUE(cHTMLFields) NO-ECHO.
        REPEAT:
            IMPORT cFieldType cFieldLabel cFieldName.
            FIND FIRST ttAvailFields
                 WHERE ttAvailFields.fieldLabel EQ cFieldLabel
                   AND ttAvailFields.fieldName  EQ cFieldName
                 NO-ERROR.
            IF AVAILABLE ttAvailFields THEN
            CASE cFieldType:
                WHEN "Job" THEN
                htmlJobFields:ADD-LAST(cFieldLabel + " ("
                            + cFieldName + ")",
                              STRING(ROWID(ttAvailFields))).
                WHEN "Pending" THEN
                htmlPendingFields:ADD-LAST(cFieldLabel + " ("
                                + cFieldName + ")",
                                  STRING(ROWID(ttAvailFields))).
            END CASE.
        END. /* repeat */
        INPUT CLOSE.
    END. /* if ne ? */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

