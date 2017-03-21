&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: mftaborder.w

  Description: module to select tab order for misc fields

  Input Parameters: frame handle of fields, tab label, current tab order

  Output Parameters: tab order selected

  Author: Ron Stark

  Created: 2.15.2012 (updated 11.29.2016)
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipFrameHandle AS WIDGET NO-UNDO.
DEFINE INPUT PARAMETER ipTabLabel AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopttTabOrder AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplSavePrompt AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

{UDF/mfttdefs.i &NEW="SHARED"}

DEFINE TEMP-TABLE ttTabOrder NO-UNDO
  FIELD objOrder AS INTEGER
  FIELD objX AS INTEGER
  FIELD objY AS INTEGER
  FIELD objName AS CHARACTER
  FIELD objRowID AS ROWID
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
&Scoped-Define ENABLED-OBJECTS btnSave btnExit tabOptions tabObjects 
&Scoped-Define DISPLAYED-OBJECTS tabLabel tabOptions tabObjects 

/* Custom List Definitions                                              */
/* moveButtons,topButtons,bottomButtons,List-4,List-5,List-6            */
&Scoped-define moveButtons btnMoveDown btnMoveFirst btnMoveLast btnMoveUp 
&Scoped-define topButtons btnMoveFirst btnMoveUp 
&Scoped-define bottomButtons btnMoveDown btnMoveLast 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "E&xit" 
     SIZE 8 BY 1.91 TOOLTIP "Exit"
     FONT 4.

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "Graphics/16x16/down.jpg":U
     IMAGE-INSENSITIVE FILE "Graphics/16x16/sign_forbidden.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Move &Down" 
     SIZE 4.2 BY 1.

DEFINE BUTTON btnMoveFirst 
     IMAGE-UP FILE "Graphics/16x16/nav_up.gif":U
     IMAGE-INSENSITIVE FILE "Graphics/16x16/sign_forbidden.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Move &First" 
     SIZE 4.2 BY 1.

DEFINE BUTTON btnMoveLast 
     IMAGE-UP FILE "Graphics/16x16/nav_down.gif":U
     IMAGE-INSENSITIVE FILE "Graphics/16x16/sign_forbidden.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Move &Last" 
     SIZE 4.2 BY 1.

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "Graphics/16x16/up.jpg":U
     IMAGE-INSENSITIVE FILE "Graphics/16x16/sign_forbidden.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Move &Up" 
     SIZE 4.2 BY 1.

DEFINE BUTTON btnSave AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
     FONT 4.

DEFINE VARIABLE tabOptions AS CHARACTER FORMAT "X(256)":U INITIAL "Default" 
     LABEL "&Tabbing Options" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "Default","Custom","Left to Right By Columns","Left to Right By Rows","Right to Left By Columns","Right to Left By Rows" 
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tabLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tab" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE tabObjects AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 78 BY 25.24 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnSave AT ROW 25.76 COL 80 HELP
          "Save" WIDGET-ID 8
     btnExit AT ROW 25.76 COL 88 HELP
          "Exit Design Layout Window" WIDGET-ID 4
     tabLabel AT ROW 1.24 COL 5 COLON-ALIGNED WIDGET-ID 2
     tabOptions AT ROW 1.24 COL 67 COLON-ALIGNED HELP
          "Select Tabbing Option"
     tabObjects AT ROW 2.43 COL 2 HELP
          "Select Tabbable Object to Move" NO-LABEL
     btnMoveDown AT ROW 8.86 COL 81 HELP
          "Move Selected Tabbable Object Down"
     btnMoveFirst AT ROW 6.48 COL 81 HELP
          "Move Selected Tabbable Object to First Object"
     btnMoveLast AT ROW 10.05 COL 81 HELP
          "Move Selected Tabbable Object to Last Object"
     btnMoveUp AT ROW 7.67 COL 81 HELP
          "Move Selected Tabbable Object Up"
     SPACE(10.80) SKIP(19.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "User Defined Fields Tab Order"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnMoveDown IN FRAME Dialog-Frame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnMoveFirst IN FRAME Dialog-Frame
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR BUTTON btnMoveLast IN FRAME Dialog-Frame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnMoveUp IN FRAME Dialog-Frame
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN tabLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* User Defined Fields Tab Order */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown Dialog-Frame
ON CHOOSE OF btnMoveDown IN FRAME Dialog-Frame /* Move Down */
DO:
  RUN moveObject ('Down').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveFirst Dialog-Frame
ON CHOOSE OF btnMoveFirst IN FRAME Dialog-Frame /* Move First */
DO:
  RUN moveObject ('First').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveLast Dialog-Frame
ON CHOOSE OF btnMoveLast IN FRAME Dialog-Frame /* Move Last */
DO:
  RUN moveObject ('Last').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp Dialog-Frame
ON CHOOSE OF btnMoveUp IN FRAME Dialog-Frame /* Move Up */
DO:
  RUN moveObject ('Up').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
DO:
    RUN setTabOrder.
    ASSIGN
        iopttTabOrder = tabOptions
        ioplSavePrompt = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tabObjects
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tabObjects Dialog-Frame
ON VALUE-CHANGED OF tabObjects IN FRAME Dialog-Frame
DO:
  IF tabOptions:SCREEN-VALUE EQ 'Custom' THEN DO:
    ENABLE {&moveButtons} WITH FRAME {&FRAME-NAME}.
    IF SELF:SCREEN-VALUE EQ SELF:ENTRY(1) THEN
    DISABLE {&topButtons} WITH FRAME {&FRAME-NAME}.
    ELSE IF SELF:SCREEN-VALUE EQ SELF:ENTRY(SELF:NUM-ITEMS) THEN
    DISABLE {&bottomButtons} WITH FRAME {&FRAME-NAME}.
  END.
  ELSE
  DISABLE {&moveButtons} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tabOptions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tabOptions Dialog-Frame
ON VALUE-CHANGED OF tabOptions IN FRAME Dialog-Frame /* Tabbing Options */
DO:
  ASSIGN {&SELF-NAME}.
  RUN setObjOrder.
  IF tabObjects:NUM-ITEMS GT 0 THEN
  tabObjects:SCREEN-VALUE = tabObjects:ENTRY(1).
  APPLY 'VALUE-CHANGED':U TO tabObjects.
  RETURN NO-APPLY.
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
  ASSIGN
    tabObjects:DELIMITER = '|'
    tabOptions = iopttTabOrder
    .
  RUN enable_UI.
  tabLabel:SCREEN-VALUE = ipTabLabel.
  RUN loadObjects.
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
  DISPLAY tabLabel tabOptions tabObjects 
      WITH FRAME Dialog-Frame.
  ENABLE btnSave btnExit tabOptions tabObjects 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadObjects Dialog-Frame 
PROCEDURE loadObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.

  EMPTY TEMP-TABLE ttTabOrder.
  ASSIGN
    current-widget = ipFrameHandle
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD
    .
  DO WHILE current-widget NE ?:
    IF NOT CAN-DO('Image,Rectangle,Text',current-widget:TYPE) THEN DO:
      CREATE ttTabOrder.
      ASSIGN
        ttTabOrder.objName = current-widget:NAME
        ttTabOrder.objX = current-widget:X
        ttTabOrder.objY = current-widget:Y
        ttTabOrder.objRowID = TO-ROWID(ENTRY(1,current-widget:PRIVATE-DATA,'|'))
        .
    END.
    current-widget = current-widget:NEXT-SIBLING.
  END.
  RUN setObjOrder.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveObject Dialog-Frame 
PROCEDURE moveObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS CHARACTER NO-UNDO.

  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    DO idx = 1 TO tabObjects:NUM-ITEMS:
      IF tabObjects:IS-SELECTED(idx) THEN LEAVE.
    END.
    FIND FIRST ttAttrb
         WHERE ROWID(ttAttrb) EQ TO-ROWID(tabObjects:ENTRY(idx)).
    CASE ipMove:
      WHEN 'First' THEN DO:
        tabObjects:DELETE(idx).
        tabObjects:ADD-FIRST(ttAttrb.attr_name,STRING(ROWID(ttAttrb))).
        idx = 1.
      END.
      WHEN 'Up' THEN DO:
        tabObjects:DELETE(idx).
        idx = idx - 1.
        tabObjects:INSERT(ttAttrb.attr_name,STRING(ROWID(ttAttrb)),idx).
      END.
      WHEN 'Down' THEN DO:
        tabObjects:DELETE(idx).
        idx = idx + 1.
        tabObjects:INSERT(ttAttrb.attr_name,STRING(ROWID(ttAttrb)),idx).
      END.
      WHEN 'Last' THEN DO:
        tabObjects:DELETE(idx).
        tabObjects:ADD-LAST(ttAttrb.attr_name,STRING(ROWID(ttAttrb))).
        idx = tabObjects:NUM-ITEMS.
      END.
    END CASE.
    tabObjects:SCREEN-VALUE = tabObjects:ENTRY(idx).
    APPLY 'VALUE-CHANGED':U TO tabObjects.
  END.

END PROCEDURE.

/*  tabObjects:ADD-LAST(ttTabOrder.objName,STRING(ttTabOrder.objRowID)).*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setObjOrder Dialog-Frame 
PROCEDURE setObjOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE objAssign ASSIGN ttTabOrder.objOrder = idx idx = idx + 1.

  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      tabObjects:LIST-ITEM-PAIRS = ?
      idx = 1.
    CASE tabOptions:
      WHEN 'Default' THEN
      FOR EACH ttTabOrder:
        {&objAssign}
      END.
      WHEN 'Custom' THEN
      FOR EACH ttTabOrder BY ttTabOrder.objOrder:
        {&objAssign}
      END.
      WHEN 'Left to Right By Columns' THEN
      FOR EACH ttTabOrder BY ttTabOrder.objX BY ttTabOrder.objY:
        {&objAssign}
      END.
      WHEN 'Left to Right By Rows' THEN
      FOR EACH ttTabOrder BY ttTabOrder.objY BY ttTabOrder.objX:
        {&objAssign}
      END.
      WHEN 'Right to Left By Columns' THEN
      FOR EACH ttTabOrder BY ttTabOrder.objX DESC BY ttTabOrder.objY:
        {&objAssign}
      END.
      WHEN 'Right to Left By Rows' THEN
      FOR EACH ttTabOrder BY ttTabOrder.objY BY ttTabOrder.objX DESC:
        {&objAssign}
      END.
    END CASE.
    FOR EACH ttTabOrder BY ttTabOrder.objOrder:
      tabObjects:ADD-LAST(ttTabOrder.objName,STRING(ttTabOrder.objRowID)).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTabOrder Dialog-Frame 
PROCEDURE setTabOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  DO idx = 1 TO tabObjects:NUM-ITEMS IN FRAME {&FRAME-NAME}:
    FIND FIRST ttAttrb
         WHERE ROWID(ttAttrb) EQ TO-ROWID(tabObjects:ENTRY(idx)).
    ttAttrb.attr_order = idx.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

