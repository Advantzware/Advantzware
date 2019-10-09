&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: AOA/jasperGroupCalc.w

  Description: Jasper Groups & Calc Types

  Input Parameters: Label, Field, Group List, Field List, Variables, Group Calc

  Output Parameters: Group Calc List and Save/Cancel

  Author: Ron Stark

  Created: 10.19.2018
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT        PARAMETER ipcLabel      AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcField      AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcGroups     AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcFields     AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcVariables  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcGrougCalc AS CHARACTER NO-UNDO.
DEFINE       OUTPUT PARAMETER oplSave       AS LOGICAL   NO-UNDO.
&ELSE
DEFINE VARIABLE ipcLabel      AS CHARACTER NO-UNDO INITIAL "ttLabel".
DEFINE VARIABLE ipcField      AS CHARACTER NO-UNDO INITIAL "ttField".
DEFINE VARIABLE ipcGroups     AS CHARACTER NO-UNDO INITIAL
    "Column,[Group] Group1,[Group] Group2,Page,Report".
DEFINE VARIABLE ipcFields     AS CHARACTER NO-UNDO INITIAL
    "$F~{Field1},$F~{Field2},$F~{Field3},$F~{Field4},$F~{Field5}".
DEFINE VARIABLE ipcVariables  AS CHARACTER NO-UNDO INITIAL
    "$V~{Variable1},$V~{Variable2},$V~{Variable3},$V~{Variable4},$V~{Variable5}".
DEFINE VARIABLE iopcGrougCalc AS CHARACTER NO-UNDO INITIAL
    "Column,Calculated|COLUMN,Page,Calculated|PAGE,Report,Sum,[Group] Group1,Sum".
DEFINE VARIABLE oplSave       AS LOGICAL   NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hActiveEditor AS HANDLE NO-UNDO.

CREATE WIDGET-POOL "GroupPool".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSave cField cVariable btnCancel btnReset 
&Scoped-Define DISPLAYED-OBJECTS cField cVariable 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel"
     BGCOLOR 8 .

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset"
     BGCOLOR 8 .

DEFINE BUTTON btnSave AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
     BGCOLOR 8 .

DEFINE IMAGE IMAGE-1
     FILENAME "AOA/images/fx_16xlg.gif":U TRANSPARENT
     SIZE 4 BY .71.

DEFINE RECTANGLE RECT-0
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 25.8 BY 2.33
     BGCOLOR 15 .

DEFINE RECTANGLE RECTCALC
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 75 BY 30.24.

DEFINE VARIABLE cField AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 30 BY 29.05 NO-UNDO.

DEFINE VARIABLE cVariable AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 42 BY 29.05 NO-UNDO.

DEFINE VARIABLE cFocus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE .2 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnSave AT ROW 29.33 COL 55 HELP
          "Click to Save"
     cField AT ROW 2.19 COL 87 NO-LABEL WIDGET-ID 10
     cVariable AT ROW 2.19 COL 118 NO-LABEL WIDGET-ID 12
     btnCancel AT ROW 29.33 COL 71 HELP
          "Click to Cancel"
     btnReset AT ROW 29.33 COL 63 HELP
          "Click to Reset" WIDGET-ID 2
     "Variables" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.48 COL 122 WIDGET-ID 14
     "Fields" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.48 COL 87 WIDGET-ID 8
     RECT-0 AT ROW 29.1 COL 54 WIDGET-ID 4
     RECTCALC AT ROW 1.24 COL 86 WIDGET-ID 6
     IMAGE-1 AT ROW 1.48 COL 118 WIDGET-ID 16
     SPACE(38.99) SKIP(29.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 
         TITLE FGCOLOR 1 "Jasper Groups/Calc Types for Column:"
         CANCEL-BUTTON btnCancel WIDGET-ID 100.

DEFINE FRAME calcGroupFrame
     cFocus AT ROW 1 COL 1 NO-LABEL WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84 BY 27.86 WIDGET-ID 200.


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
/* REPARENT FRAME */
ASSIGN FRAME calcGroupFrame:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR FRAME calcGroupFrame
                                                                        */
ASSIGN 
       FRAME calcGroupFrame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cFocus IN FRAME calcGroupFrame
   ALIGN-L                                                              */
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-0 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECTCALC IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME calcGroupFrame
/* Query rebuild information for FRAME calcGroupFrame
     _Query            is NOT OPENED
*/  /* FRAME calcGroupFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Jasper Groups/Calc Types for Column: */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame /* Reset */
DO:
    RUN pSetGroupCalcValues (iopcGrougCalc).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
DO:
    oplSave = YES.
    RUN pGetGroupCalcValues (OUTPUT iopcGrougCalc).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cField Dialog-Frame
ON DEFAULT-ACTION OF cField IN FRAME Dialog-Frame
DO:
    IF VALID-HANDLE(hActiveEditor) AND hActiveEditor:SENSITIVE THEN
    hActiveEditor:INSERT-STRING(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cVariable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cVariable Dialog-Frame
ON DEFAULT-ACTION OF cVariable IN FRAME Dialog-Frame
DO:
    IF VALID-HANDLE(hActiveEditor) AND hActiveEditor:SENSITIVE THEN
    hActiveEditor:INSERT-STRING(SELF:SCREEN-VALUE).
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
  FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + " "
                            + ipcLabel + " (" + ipcField + ")"
                            .
  RUN pCreateGroups (ipcGroups).
  RUN pSetGroupCalcValues (iopcGrougCalc).
  RUN enable_UI.
  APPLY "ENTRY":U TO cFocus.
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
  HIDE FRAME calcGroupFrame.
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
  DISPLAY cField cVariable 
      WITH FRAME Dialog-Frame.
  ENABLE btnSave cField cVariable btnCancel btnReset 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  DISPLAY cFocus 
      WITH FRAME calcGroupFrame.
  ENABLE cFocus 
      WITH FRAME calcGroupFrame.
  VIEW FRAME calcGroupFrame.
  {&OPEN-BROWSERS-IN-QUERY-calcGroupFrame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateGroups Dialog-Frame 
PROCEDURE pCreateGroups :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcGroups AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hRect      AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hRectCalc  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hToggleBox AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hRadioSet  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hEditor    AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hDataType  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE dRow       AS DECIMAL NO-UNDO INITIAL 1.24.
    DEFINE VARIABLE idx        AS INTEGER NO-UNDO.
    
    DO idx = 1 TO NUM-ENTRIES(ipcGroups):
        CREATE RECTANGLE hRect IN WIDGET-POOL "GroupPool"
            ASSIGN
                FRAME = FRAME calcGroupFrame:HANDLE
                NAME = "RECT-" + STRING(idx)
                GRAPHIC-EDGE = YES
                ROUNDED = YES
                WIDTH-PIXELS = 1
                COL = 2
                ROW = dRow
                SENSITIVE = NO
                HIDDEN = YES
                WIDTH = 77
                HEIGHT = 5
                FILLED = NO
                .
        ASSIGN
            hRect:HIDDEN = NO
            dRow         = dRow + 2.62
            .
        CREATE EDITOR hEditor IN WIDGET-POOL "GroupPool"
            ASSIGN
                FRAME = FRAME calcGroupFrame:HANDLE
                NAME = "Editor" + REPLACE(ENTRY(idx,ipcGroups)," ","")
                COL = 3
                ROW = dRow
                SENSITIVE = NO
                HIDDEN = YES
                WIDTH = 63
                HEIGHT = 2.14
                FGCOLOR = 1
                BGCOLOR = 15
                SCROLLBAR-HORIZONTAL = NO
                SCROLLBAR-VERTICAL = YES
                WORD-WRAP = YES
                BOX = YES
                SCREEN-VALUE = ""
                PRIVATE-DATA = ENTRY(idx,ipcGroups)
            TRIGGERS:
              ON ENTRY
                PERSISTENT RUN pEditorEntry IN THIS-PROCEDURE (hEditor:HANDLE).
            END TRIGGERS.
        CREATE RADIO-SET hDataType IN WIDGET-POOL "GroupPool"
            ASSIGN
                FRAME = FRAME calcGroupFrame:HANDLE
                NAME = "DataType" + REPLACE(ENTRY(idx,ipcGroups)," ","")
                RADIO-BUTTONS = "String,String,Integer,Integer,Double,Double"
                COL = hEditor:COL + hEditor:WIDTH + 1
                ROW = dRow
                SENSITIVE = NO
                HIDDEN = YES
                WIDTH = 10
                HEIGHT = 2.14
                SCREEN-VALUE = "String"
                PRIVATE-DATA = ENTRY(idx,ipcGroups)
                .
        CREATE RADIO-SET hRadioSet IN WIDGET-POOL "GroupPool"
            ASSIGN
                FRAME = FRAME calcGroupFrame:HANDLE
                NAME = "RadioSet" + REPLACE(ENTRY(idx,ipcGroups)," ","")
                RADIO-BUTTONS = "Sum,Sum,Count,Count,Average,Averag,Lowest,Lowest,Highest,Highest,Calculated,Calculated"
                HORIZONTAL = YES
                COL = 8
                ROW = dRow - 1.19
                SENSITIVE = NO
                HIDDEN = YES
                WIDTH = 70
                HEIGHT = 1
                SCREEN-VALUE = "Sum"
                PRIVATE-DATA = ENTRY(idx,ipcGroups)
            TRIGGERS:
              ON VALUE-CHANGED
                PERSISTENT RUN pRadioSet IN THIS-PROCEDURE (hRadioSet:HANDLE,hEditor:HANDLE,hDataType:handle).
            END TRIGGERS.
        CREATE TOGGLE-BOX hToggleBox IN WIDGET-POOL "GroupPool"
            ASSIGN
                FRAME = FRAME calcGroupFrame:HANDLE
                NAME = "Toggle" + REPLACE(ENTRY(idx,ipcGroups)," ","")
                LABEL = ENTRY(idx,ipcGroups)
                COL = 3
                ROW = dRow - 2.38
                SENSITIVE = YES
                HIDDEN = YES
                WIDTH = 55
                HEIGHT = 1
                PRIVATE-DATA = ENTRY(idx,ipcGroups)
            TRIGGERS:
              ON VALUE-CHANGED
                PERSISTENT RUN pToggleBox IN THIS-PROCEDURE (hToggleBox:HANDLE,hRadioSet:HANDLE,hDataType:HANDLE).
            END TRIGGERS.
        ASSIGN
            hToggleBox:HIDDEN = NO
            hRadioSet:HIDDEN  = NO
            hEditor:HIDDEN    = NO
            hDataType:HIDDEN  = NO
            dRow              = dRow + 2.6
            .
    END. /* do idx */
    ASSIGN
        cField:LIST-ITEMS IN FRAME {&FRAME-NAME} = ipcFields
        cVariable:LIST-ITEMS = ipcVariables
        FRAME calcGroupFrame:VIRTUAL-HEIGHT = dRow - 1
        FRAME calcGroupFrame:HIDDEN = NO
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEditorEntry Dialog-Frame 
PROCEDURE pEditorEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphEditor AS HANDLE NO-UNDO.
    
    IF VALID-HANDLE(iphEditor) THEN
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            hActiveEditor       = iphEditor
            cField:SENSITIVE    = YES
            cVariable:SENSITIVE = YES
            .
    END. /* with frame */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetGroupCalcValues Dialog-Frame 
PROCEDURE pGetGroupCalcValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcGroupCalc AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hEditor   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hRadioSet AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hDataType AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER NO-UNDO.
    
    ASSIGN
        hWidget = FRAME calcGroupFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        CASE hWidget:TYPE:
            WHEN "EDITOR" THEN
            hEditor = hWidget.
            WHEN "RADIO-SET" THEN
                IF hWidget:NAME BEGINS "RadioSet" THEN
                hRadioSet = hWidget.
                ELSE IF hWidget:NAME BEGINS "DataType" THEN
                hDataType = hWidget.
            WHEN "TOGGLE-BOX" THEN
            IF hWidget:SCREEN-VALUE EQ "yes" THEN DO:
                ASSIGN
                    opcGroupCalc = opcGroupCalc
                                 + hwidget:PRIVATE-DATA + ","
                    opcGroupCalc = opcGroupCalc
                                 + hRadioSet:SCREEN-VALUE
                                 .
                IF hRadioSet:SCREEN-VALUE EQ "Calculated" THEN
                opcGroupCalc = opcGroupCalc + "|"
                             + hEditor:SCREEN-VALUE + "|"
                             + hDataType:SCREEN-VALUE
                             .
                opcGroupCalc = opcGroupCalc + ",".
            END. /* if */
        END CASE.
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    opcGroupCalc = TRIM(opcGroupCalc,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRadioSet Dialog-Frame 
PROCEDURE pRadioSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphRadioSet AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphEditor   AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphDataType  AS HANDLE NO-UNDO.
    
    ASSIGN
        iphEditor:SENSITIVE   = iphRadioSet:SCREEN-VALUE EQ "Calculated"
        iphDataType:SENSITIVE = iphEditor:SENSITIVE
        .    
    IF iphEditor:SENSITIVE THEN
    hActiveEditor = iphEditor.
    ELSE
    hActiveEditor = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetGroupCalcValues Dialog-Frame 
PROCEDURE pSetGroupCalcValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcGroupCalc AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCalcType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hWidget   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    
    ASSIGN
        hWidget = FRAME calcGroupFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        hActiveEditor = ?
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF CAN-DO("EDITOR,RADIO-SET,TOGGLE-BOX",hWidget:TYPE) THEN DO:
            idx = LOOKUP(hWidget:PRIVATE-DATA,ipcGroupCalc).
            IF idx NE 0 THEN DO:
                cCalcType = ENTRY(1,ENTRY(idx + 1,ipcGroupCalc),"|").
                IF cCalcType EQ "Calculated" AND NUM-ENTRIES(ENTRY(idx + 1,ipcGroupCalc),"|") LT 3 THEN
                ENTRY(idx + 1,ipcGroupCalc) = ENTRY(idx + 1,ipcGroupCalc) + "|String".
                CASE hWidget:TYPE:
                    WHEN "TOGGLE-BOX" THEN
                    hWidget:SCREEN-VALUE = "yes".
                    WHEN "RADIO-SET" THEN
                        IF hWidget:NAME BEGINS "RadioSet" THEN
                        ASSIGN
                            hWidget:SCREEN-VALUE = cCalcType
                            hWidget:SENSITIVE    = YES
                            .
                        ELSE IF hWidget:NAME BEGINS "DataType" THEN DO:
                            hWidget:SENSITIVE = cCalcType EQ "Calculated".
                            IF cCalcType EQ "Calculated" THEN
                            hWidget:SCREEN-VALUE = ENTRY(3,ENTRY(idx + 1,ipcGroupCalc),"|").
                        END.
                    WHEN "EDITOR" THEN DO:
                        hWidget:SENSITIVE = cCalcType EQ "Calculated".
                        IF cCalcType EQ "Calculated" THEN
                        ASSIGN
                            hWidget:SCREEN-VALUE = ENTRY(2,ENTRY(idx + 1,ipcGroupCalc),"|")
                            hActiveEditor = hWidget
                            .
                    END.
                END CASE.
            END. /* if lookup */
            ELSE DO:
                CASE hWidget:TYPE:
                    WHEN "TOGGLE-BOX" THEN
                    hWidget:SCREEN-VALUE = "no".
                    WHEN "RADIO-SET" THEN DO:
                        IF hWidget:NAME BEGINS "RadioSet" THEN
                        hWidget:SCREEN-VALUE = "Sum".
                        ELSE IF hWidget:NAME BEGINS "DataType" THEN
                        hWidget:SCREEN-VALUE = "String".
                        hWidget:SENSITIVE    = NO.
                    END.
                    WHEN "EDITOR" THEN
                    hWidget:SENSITIVE = NO.
                END CASE.
            END. /* else */
        END. /* if toggle-box, radio-set */
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    ASSIGN
        cField:LIST-ITEMS IN FRAME {&FRAME-NAME} = ipcFields
        cVariable:LIST-ITEMS = ipcVariables
        .
    IF VALID-HANDLE(hActiveEditor) THEN
    APPLY "ENTRY":U TO hActiveEditor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pToggleBox Dialog-Frame 
PROCEDURE pToggleBox :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphToggleBox AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphRadioSet  AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphDataType  AS HANDLE NO-UNDO.

    DEFINE VARIABLE cVarName AS CHARACTER NO-UNDO.
    
    ASSIGN
        iphRadioSet:SENSITIVE = iphToggleBox:SCREEN-VALUE EQ "yes"
        cVarName = "$V~{" + ipcField + "_"
                 + (IF iphToggleBox:LABEL BEGINS "[Group] " THEN
                   REPLACE(REPLACE(iphToggleBox:LABEL,"[Group] ","")," ","_")
                 + "_Group" ELSE iphToggleBox:LABEL)
                 + "Footer}"
                 .
    IF iphRadioSet:SENSITIVE THEN DO:
        cVariable:ADD-LAST(cVarName) IN FRAME {&FRAME-NAME}.
        iphDataType:SENSITIVE = iphRadioSet:SCREEN-VALUE EQ "Calculated".
    END. /* if */
    ELSE DO:
        iphDataType:SENSITIVE = NO.
        cVariable:DELETE(cVarName).
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

