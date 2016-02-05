&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: Ron Stark

  Description: module to select tab order for misc fields

  Input Parameters: frame handle of fields, tab label, current tab order

  Output Parameters: tab order selected

  Author: Ron Stark

  Created: 2.15.2012
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipFrameHandle AS WIDGET NO-UNDO.
DEFINE INPUT PARAMETER ipTabLabel AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopTabOrder AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

{methods/defines/miscflds.i}

DEFINE TEMP-TABLE tabOrder NO-UNDO
  FIELD objOrder AS INTEGER
  FIELD objX AS INTEGER
  FIELD objY AS INTEGER
  FIELD objName AS CHARACTER
  FIELD objRowID AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tabOptions tabObjects btnCancel btnOK 
&Scoped-Define DISPLAYED-OBJECTS tabLabel tabOptions tabObjects 

/* Custom List Definitions                                              */
/* moveButtons,topButtons,bottomButtons,List-4,List-5,List-6            */
&Scoped-define moveButtons btnMoveFirst btnMoveUp btnMoveDown btnMoveLast 
&Scoped-define topButtons btnMoveFirst btnMoveUp 
&Scoped-define bottomButtons btnMoveDown btnMoveLast 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnMoveDown 
     LABEL "Move &Down" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnMoveFirst 
     LABEL "Move &First" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnMoveLast 
     LABEL "Move &Last" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnMoveUp 
     LABEL "Move &Up" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE tabOptions AS CHARACTER FORMAT "X(256)":U INITIAL "Default" 
     LABEL "&Tabbing Options" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "Default","Custom","Left to Right By Columns","Left to Right By Rows","Right to Left By Columns","Right to Left By Rows" 
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tabLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tab" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE tabObjects AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 78 BY 21.43 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tabLabel AT ROW 1.24 COL 5 COLON-ALIGNED WIDGET-ID 2
     tabOptions AT ROW 1.24 COL 67 COLON-ALIGNED HELP
          "Select Tabbing Option"
     tabObjects AT ROW 2.43 COL 2 HELP
          "Select Tabbable Object to Move" NO-LABEL
     btnMoveFirst AT ROW 2.43 COL 81 HELP
          "Move Selected Tabbable Object to First Object"
     btnMoveUp AT ROW 3.86 COL 81 HELP
          "Move Selected Tabbable Object Up"
     btnMoveDown AT ROW 5.29 COL 81 HELP
          "Move Selected Tabbable Object Down"
     btnMoveLast AT ROW 6.71 COL 81 HELP
          "Move Selected Tabbable Object to Last Object"
     btnCancel AT ROW 21.24 COL 81
     btnOK AT ROW 22.67 COL 81
     SPACE(0.00) SKIP(0.05)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Misc Fields Tab Order"
         DEFAULT-BUTTON btnOK CANCEL-BUTTON btnCancel.


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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Misc Fields Tab Order */
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


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
  RUN setTabOrder.
  iopTabOrder = tabOptions.
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
    tabOptions = iopTabOrder.
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
  ENABLE tabOptions tabObjects btnCancel btnOK 
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

  EMPTY TEMP-TABLE tabOrder.
  ASSIGN
    current-widget = ipFrameHandle
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF NOT CAN-DO('Image,Rectangle,Text',current-widget:TYPE) THEN DO:
      CREATE tabOrder.
      ASSIGN
        tabOrder.objName = current-widget:NAME
        tabOrder.objX = current-widget:X
        tabOrder.objY = current-widget:Y
        tabOrder.objRowID = TO-ROWID(ENTRY(1,current-widget:PRIVATE-DATA,'|')).
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
    FIND FIRST attrb EXCLUSIVE-LOCK
         WHERE ROWID(attrb) EQ TO-ROWID(tabObjects:ENTRY(idx)).
    CASE ipMove:
      WHEN 'First' THEN DO:
        tabObjects:DELETE(idx).
        tabObjects:ADD-FIRST(attrb.attr_name,STRING(ROWID(attrb))).
        idx = 1.
      END.
      WHEN 'Up' THEN DO:
        tabObjects:DELETE(idx).
        idx = idx - 1.
        tabObjects:INSERT(attrb.attr_name,STRING(ROWID(attrb)),idx).
      END.
      WHEN 'Down' THEN DO:
        tabObjects:DELETE(idx).
        idx = idx + 1.
        tabObjects:INSERT(attrb.attr_name,STRING(ROWID(attrb)),idx).
      END.
      WHEN 'Last' THEN DO:
        tabObjects:DELETE(idx).
        tabObjects:ADD-LAST(attrb.attr_name,STRING(ROWID(attrb))).
        idx = tabObjects:NUM-ITEMS.
      END.
    END CASE.
    tabObjects:SCREEN-VALUE = tabObjects:ENTRY(idx).
    APPLY 'VALUE-CHANGED':U TO tabObjects.
  END.

END PROCEDURE.

/*  tabObjects:ADD-LAST(tabOrder.objName,STRING(tabOrder.objRowID)).*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setObjOrder Dialog-Frame 
PROCEDURE setObjOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE objAssign ASSIGN   tabOrder.objOrder = idx   idx = idx + 1.

  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      tabObjects:LIST-ITEM-PAIRS = ?
      idx = 1.
    CASE tabOptions:
      WHEN 'Default' THEN
      FOR EACH tabOrder:
        {&objAssign}
      END.
      WHEN 'Custom' THEN
      FOR EACH tabOrder BY tabOrder.objOrder:
        {&objAssign}
      END.
      WHEN 'Left to Right By Columns' THEN
      FOR EACH tabOrder BY tabOrder.objX BY tabOrder.objY:
        {&objAssign}
      END.
      WHEN 'Left to Right By Rows' THEN
      FOR EACH tabOrder BY tabOrder.objY BY tabOrder.objX:
        {&objAssign}
      END.
      WHEN 'Right to Left By Columns' THEN
      FOR EACH tabOrder BY tabOrder.objX DESC BY tabOrder.objY:
        {&objAssign}
      END.
      WHEN 'Right to Left By Rows' THEN
      FOR EACH tabOrder BY tabOrder.objY BY tabOrder.objX DESC:
        {&objAssign}
      END.
    END CASE.
    FOR EACH tabOrder BY tabOrder.objOrder:
      tabObjects:ADD-LAST(tabOrder.objName,STRING(tabOrder.objRowID)).
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
    FIND FIRST attrb EXCLUSIVE-LOCK
         WHERE ROWID(attrb) EQ TO-ROWID(tabObjects:ENTRY(idx)).
    attrb.attr_order = idx.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

