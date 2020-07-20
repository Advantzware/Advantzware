&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: dynCalcField.w

  Description: Dynamic Calculated Fields

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 2.21.2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT        PARAMETER iphDynCalcField AS HANDLE    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcFieldName   AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcFieldLabel  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcDataType    AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcFieldFormat AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcCalcProc    AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcCalcParam   AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcCalcFormula AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcFieldList    AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcParamList    AS CHARACTER NO-UNDO.
DEFINE       OUTPUT PARAMETER oplSave         AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */

IF NOT VALID-HANDLE(iphDynCalcField) THEN
RUN AOA/spDynCalcField.p PERSISTENT SET iphDynCalcField.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOK btnAddParameter btnCancel cFieldLabel ~
cDataType cFieldFormat cCalcParam cParamList cAvailFields cFieldType ~
btnClearCalculator btnClearFormula cAvailCalcProc cCalculator cCalcFormula ~
btn7 btn4 btn1 btnLeftParenthesis btn8 btn5 btn2 btnRightParenthesis btn0 ~
btn9 btn6 btn3 btnPeriod btnMinus btnPlus btnDivide btnMultiply 
&Scoped-Define DISPLAYED-OBJECTS cFieldName cFieldLabel cDataType ~
cFieldFormat cCalcParam cParamList cAvailFields cFieldType cCalcProc ~
cAvailCalcProc cCalculator cCalcFormula 

/* Custom List Definitions                                              */
/* calcFields,List-2,List-3,List-4,List-5,List-6                        */
&Scoped-define calcFields cFieldName cFieldLabel cFieldFormat cCalcParam ~
cCalcProc cCalculator cCalcFormula 
&Scoped-define List-3 btnAddParameter 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn0 
     LABEL "0" 
     SIZE 4 BY .95 TOOLTIP "Zero".

DEFINE BUTTON btn1 
     LABEL "1" 
     SIZE 4 BY .95 TOOLTIP "One".

DEFINE BUTTON btn2 
     LABEL "2" 
     SIZE 4 BY .95 TOOLTIP "Two".

DEFINE BUTTON btn3 
     LABEL "3" 
     SIZE 4 BY .95 TOOLTIP "Three".

DEFINE BUTTON btn4 
     LABEL "4" 
     SIZE 4 BY .95 TOOLTIP "Four".

DEFINE BUTTON btn5 
     LABEL "5" 
     SIZE 4 BY .95 TOOLTIP "Five".

DEFINE BUTTON btn6 
     LABEL "6" 
     SIZE 4 BY .95 TOOLTIP "Six".

DEFINE BUTTON btn7 
     LABEL "7" 
     SIZE 4 BY .95 TOOLTIP "Seven".

DEFINE BUTTON btn8 
     LABEL "8" 
     SIZE 4 BY .95 TOOLTIP "Eight".

DEFINE BUTTON btn9 
     LABEL "9" 
     SIZE 4 BY .95 TOOLTIP "Nine".

DEFINE BUTTON btnAddParameter 
     IMAGE-UP FILE "Graphics/16x16/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Parameter" 
     SIZE 5 BY 1.05 TOOLTIP "Add Parameter".

DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnClearCalculator 
     LABEL "Clear" 
     SIZE 8 BY .91 TOOLTIP "Clear Free Form Formula".

DEFINE BUTTON btnClearFormula 
     LABEL "Clear" 
     SIZE 8 BY .91 TOOLTIP "Clear Free Form Formula".

DEFINE BUTTON btnDivide 
     LABEL "/" 
     SIZE 4 BY .95 TOOLTIP "Divide".

DEFINE BUTTON btnLeftParenthesis 
     LABEL "(" 
     SIZE 2 BY .95 TOOLTIP "Left Parenthesis".

DEFINE BUTTON btnMinus 
     LABEL "-" 
     SIZE 4 BY .95 TOOLTIP "Minus".

DEFINE BUTTON btnMultiply 
     LABEL "*" 
     SIZE 4 BY .95 TOOLTIP "Multiply".

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnPeriod 
     LABEL "." 
     SIZE 4 BY .95 TOOLTIP "Period (Decimal)".

DEFINE BUTTON btnPlus 
     LABEL "+" 
     SIZE 4 BY .95 TOOLTIP "Plus".

DEFINE BUTTON btnRightParenthesis 
     LABEL ")" 
     SIZE 2 BY .95 TOOLTIP "Right Parenthesis".

DEFINE VARIABLE cDataType AS CHARACTER FORMAT "X(256)":U INITIAL "Character" 
     LABEL "Data Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Character","Date","Decimal","Integer","Logical" 
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cParamList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Parameters" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cCalcFormula AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 87 BY 6.91 NO-UNDO.

DEFINE VARIABLE cCalculator AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 68 BY 4.29 NO-UNDO.

DEFINE VARIABLE cCalcProc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Calc Procedure" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cFieldFormat AS CHARACTER FORMAT "X(256)":U INITIAL "x(256)" 
     LABEL "Field Format" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cFieldLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field Label" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cFieldName AS CHARACTER FORMAT "X(256)":U INITIAL "Calc" 
     LABEL "Field Name" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cFieldType AS CHARACTER INITIAL "Formula" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Calc Params", "Parameter",
"Calculator", "Calculator",
"Calc Formula", "Formula"
     SIZE 18 BY 3.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 17.8 BY 2.38.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 18 BY 4.29.

DEFINE VARIABLE cAvailCalcProc AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 87 BY 15 TOOLTIP "Available Calculated Procedures" NO-UNDO.

DEFINE VARIABLE cAvailFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 36 BY 16.43 NO-UNDO.

DEFINE VARIABLE cCalcParam AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 36 BY 5.71 TOOLTIP "Selected Parameters" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnOK AT ROW 27.43 COL 56
     btnAddParameter AT ROW 11.95 COL 54 WIDGET-ID 208
     btnCancel AT ROW 27.43 COL 64
     cFieldName AT ROW 1.24 COL 16 COLON-ALIGNED WIDGET-ID 4
     cFieldLabel AT ROW 2.43 COL 16 COLON-ALIGNED WIDGET-ID 6
     cDataType AT ROW 3.62 COL 16 COLON-ALIGNED WIDGET-ID 210
     cFieldFormat AT ROW 4.81 COL 16 COLON-ALIGNED WIDGET-ID 8
     cCalcParam AT ROW 6 COL 18 NO-LABEL WIDGET-ID 16
     cParamList AT ROW 11.95 COL 16 COLON-ALIGNED WIDGET-ID 24
     cAvailFields AT ROW 13.14 COL 18 NO-LABEL WIDGET-ID 22
     cFieldType AT ROW 14.33 COL 55 NO-LABEL WIDGET-ID 216
     btnClearCalculator AT ROW 19.57 COL 66 HELP
          "Clear Free Form Calculator" WIDGET-ID 232
     btnClearFormula AT ROW 24.57 COL 66 HELP
          "Clear Free Form Formula" WIDGET-ID 230
     cCalcProc AT ROW 1.24 COL 72 COLON-ALIGNED WIDGET-ID 10
     cAvailCalcProc AT ROW 2.43 COL 74 NO-LABEL WIDGET-ID 12
     cCalculator AT ROW 17.67 COL 74 NO-LABEL WIDGET-ID 224
     cCalcFormula AT ROW 22.67 COL 74 NO-LABEL WIDGET-ID 212
     btn7 AT ROW 17.91 COL 144 WIDGET-ID 260
     btn4 AT ROW 18.86 COL 144 WIDGET-ID 254
     btn1 AT ROW 19.81 COL 144 WIDGET-ID 248
     btnLeftParenthesis AT ROW 20.76 COL 144 HELP
          "Left Parenthesis" WIDGET-ID 234
     btn8 AT ROW 17.91 COL 148 WIDGET-ID 262
     btn5 AT ROW 18.86 COL 148 WIDGET-ID 256
     btn2 AT ROW 19.81 COL 148 WIDGET-ID 250
     btnRightParenthesis AT ROW 20.76 COL 146 HELP
          "Right Parenthesis" WIDGET-ID 244
     btn0 AT ROW 20.76 COL 148 WIDGET-ID 266
     btn9 AT ROW 17.91 COL 152 WIDGET-ID 264
     btn6 AT ROW 18.86 COL 152 WIDGET-ID 258
     btn3 AT ROW 19.81 COL 152 WIDGET-ID 252
     btnPeriod AT ROW 20.76 COL 152 WIDGET-ID 268
     btnMinus AT ROW 17.91 COL 156 HELP
          "Minus" WIDGET-ID 238
     btnPlus AT ROW 18.86 COL 156 HELP
          "Plus" WIDGET-ID 236
     btnDivide AT ROW 19.81 COL 156 HELP
          "Divide" WIDGET-ID 242
     btnMultiply AT ROW 20.76 COL 156 HELP
          "Multiply" WIDGET-ID 240
     "Available Fields:" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 13.14 COL 2 WIDGET-ID 20
     "Avail Calc Proc:" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 2.43 COL 59 WIDGET-ID 14
     "Calculator:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 18.62 COL 64 WIDGET-ID 228
     "Free Form" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 17.67 COL 64 WIDGET-ID 226
     "^^ each calculator element must be SPACE delimited ^^" VIEW-AS TEXT
          SIZE 53 BY .62 AT ROW 21.95 COL 81 WIDGET-ID 246
     "Free Form" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 22.67 COL 64 WIDGET-ID 222
     "Calc Formula:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 23.62 COL 61 WIDGET-ID 214
     "Calc Params:" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 6 COL 5 WIDGET-ID 18
     "Copy Field To:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 13.38 COL 55 WIDGET-ID 220
     RECT-1 AT ROW 27.19 COL 55 WIDGET-ID 2
     RECT-2 AT ROW 17.67 COL 143 WIDGET-ID 270
     SPACE(0.00) SKIP(7.60)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 
         TITLE "Calculated Field" WIDGET-ID 100.


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
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnAddParameter IN FRAME Dialog-Frame
   3                                                                    */
ASSIGN 
       btnAddParameter:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR EDITOR cCalcFormula IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR SELECTION-LIST cCalcParam IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cCalcProc IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR EDITOR cCalculator IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cFieldFormat IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cFieldLabel IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cFieldName IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Calculated Field */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn0 Dialog-Frame
ON CHOOSE OF btn0 IN FRAME Dialog-Frame /* 0 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn1 Dialog-Frame
ON CHOOSE OF btn1 IN FRAME Dialog-Frame /* 1 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2 Dialog-Frame
ON CHOOSE OF btn2 IN FRAME Dialog-Frame /* 2 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn3 Dialog-Frame
ON CHOOSE OF btn3 IN FRAME Dialog-Frame /* 3 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn4 Dialog-Frame
ON CHOOSE OF btn4 IN FRAME Dialog-Frame /* 4 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn5 Dialog-Frame
ON CHOOSE OF btn5 IN FRAME Dialog-Frame /* 5 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn6 Dialog-Frame
ON CHOOSE OF btn6 IN FRAME Dialog-Frame /* 6 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn7 Dialog-Frame
ON CHOOSE OF btn7 IN FRAME Dialog-Frame /* 7 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn8 Dialog-Frame
ON CHOOSE OF btn8 IN FRAME Dialog-Frame /* 8 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn9 Dialog-Frame
ON CHOOSE OF btn9 IN FRAME Dialog-Frame /* 9 */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddParameter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddParameter Dialog-Frame
ON CHOOSE OF btnAddParameter IN FRAME Dialog-Frame /* Add Parameter */
DO:
    IF cParamList NE "" AND cParamList NE ? THEN
    cCalcParam:ADD-LAST("[[" + ENTRY(2,cParamList,"|") + "]]").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    oplSave = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearCalculator
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearCalculator Dialog-Frame
ON CHOOSE OF btnClearCalculator IN FRAME Dialog-Frame /* Clear */
DO:
    cCalculator:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearFormula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearFormula Dialog-Frame
ON CHOOSE OF btnClearFormula IN FRAME Dialog-Frame /* Clear */
DO:
    cCalcFormula:SCREEN-VALUE = "".
    APPLY "ENTRY":U TO cCalcParam.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDivide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDivide Dialog-Frame
ON CHOOSE OF btnDivide IN FRAME Dialog-Frame /* / */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
    APPLY "ENTRY":U TO cCalculator.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLeftParenthesis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLeftParenthesis Dialog-Frame
ON CHOOSE OF btnLeftParenthesis IN FRAME Dialog-Frame /* ( */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMinus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMinus Dialog-Frame
ON CHOOSE OF btnMinus IN FRAME Dialog-Frame /* - */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMultiply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMultiply Dialog-Frame
ON CHOOSE OF btnMultiply IN FRAME Dialog-Frame /* * */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN {&calcFields}.
    IF cFieldLabel EQ "" THEN DO:
        MESSAGE
            "Field Label cannot be Blank"
        VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY":U TO cFieldLabel.
        RETURN NO-APPLY.
    END. /* if cfieldlabel */
    IF cFieldFormat EQ "" THEN DO:
        MESSAGE
            "Field Format cannot be Blank"
        VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY":U TO cFieldFormat.
        RETURN NO-APPLY.
    END. /* if cfieldlabel */
    IF cCalcProc EQ "" AND cCalculator EQ "" AND cCalcFormula EQ "" THEN DO:
        MESSAGE
            "Calculation " + cFieldType + " cannot be Blank."
        VIEW-AS ALERT-BOX ERROR.
        CASE cFieldType:
            WHEN "Calculator" THEN
            APPLY "ENTRY":U TO cCalculator.
            WHEN "Formula" THEN
            APPLY "ENTRY":U TO cCalcFormula.
            WHEN "Parameter" THEN
            APPLY "ENTRY":U TO cAvailCalcProc.
        END CASE.
        RETURN NO-APPLY.
    END. /* if cfieldlabel */
    IF cCalculator NE "" AND cCalcFormula NE "" THEN DO:
        MESSAGE
            "Both Free Form Values are not Blank, Please clear one."
        VIEW-AS ALERT-BOX ERROR.
        CASE cFieldType:
            WHEN "Calculator" THEN
            APPLY "ENTRY":U TO cCalculator.
            OTHERWISE
            APPLY "ENTRY":U TO cCalcFormula.
        END CASE.
        RETURN NO-APPLY.
    END. /* if cfieldlabel */
    ASSIGN
        iopcFieldName   = cFieldName
        iopcFieldLabel  = cFieldLabel
        iopcDataType    = cDataType
        iopcFieldFormat = cFieldFormat
        iopcCalcProc    = cCalcProc
        iopcCalcParam   = cCalcParam:LIST-ITEMS
        iopcCalcFormula = IF cCalcFormula NE "" THEN cCalcFormula
                     ELSE IF cCalculator  NE "" THEN REPLACE(cCalculator," ","|")
                     ELSE ""
        oplSave         = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPeriod Dialog-Frame
ON CHOOSE OF btnPeriod IN FRAME Dialog-Frame /* . */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPlus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPlus Dialog-Frame
ON CHOOSE OF btnPlus IN FRAME Dialog-Frame /* + */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRightParenthesis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRightParenthesis Dialog-Frame
ON CHOOSE OF btnRightParenthesis IN FRAME Dialog-Frame /* ) */
DO:
    cCalculator:INSERT-STRING(SELF:LABEL + " ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cAvailCalcProc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cAvailCalcProc Dialog-Frame
ON DEFAULT-ACTION OF cAvailCalcProc IN FRAME Dialog-Frame
DO:
    ASSIGN
        cCalcProc:SCREEN-VALUE = ENTRY(1,SELF:SCREEN-VALUE," ")
        cCalcProc
        .
    IF cCalcProc EQ "<None>" THEN
    cCalcProc:SCREEN-VALUE = "". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cAvailFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cAvailFields Dialog-Frame
ON DEFAULT-ACTION OF cAvailFields IN FRAME Dialog-Frame
DO:
    CASE cFieldType:
        WHEN "Calculator" THEN
        cCalculator:INSERT-STRING(SELF:SCREEN-VALUE + " ").
        WHEN "Formula" THEN
        cCalcFormula:INSERT-STRING("$F~{" + REPLACE(SELF:SCREEN-VALUE,".","__") + "}").
        WHEN "Parameter" THEN
        IF INDEX(cCalcParam:LIST-ITEMS,SELF:SCREEN-VALUE) EQ 0 OR
           cCalcParam:LIST-ITEMS EQ ? THEN
        cCalcParam:ADD-LAST(SELF:SCREEN-VALUE).
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCalcFormula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCalcFormula Dialog-Frame
ON ENTRY OF cCalcFormula IN FRAME Dialog-Frame
DO:
    ASSIGN
        cFieldType:SCREEN-VALUE = "Formula"
        cFieldType
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCalcParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCalcParam Dialog-Frame
ON DEFAULT-ACTION OF cCalcParam IN FRAME Dialog-Frame
DO:
    SELF:DELETE(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCalcParam Dialog-Frame
ON ENTRY OF cCalcParam IN FRAME Dialog-Frame
DO:
    ASSIGN
        cFieldType:SCREEN-VALUE = "Parameter"
        cFieldType
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCalculator
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCalculator Dialog-Frame
ON ENTRY OF cCalculator IN FRAME Dialog-Frame
DO:
    ASSIGN
        cFieldType:SCREEN-VALUE = "Calculator"
        cFieldType
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cDataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDataType Dialog-Frame
ON VALUE-CHANGED OF cDataType IN FRAME Dialog-Frame /* Data Type */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFieldLabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFieldLabel Dialog-Frame
ON VALUE-CHANGED OF cFieldLabel IN FRAME Dialog-Frame /* Field Label */
DO:
    cFieldName:SCREEN-VALUE = "Calc" + REPLACE(SELF:SCREEN-VALUE," ","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFieldType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFieldType Dialog-Frame
ON VALUE-CHANGED OF cFieldType IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cParamList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cParamList Dialog-Frame
ON VALUE-CHANGED OF cParamList IN FRAME Dialog-Frame /* Parameters */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pGetCalcProc.
  RUN enable_UI.
  IF cCalcProc NE "" THEN
  ASSIGN
    cFieldType:SCREEN-VALUE = "Parameter"
    cFieldType
    .
  APPLY "VALUE-CHANGED":U TO cFieldName.
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
  DISPLAY cFieldName cFieldLabel cDataType cFieldFormat cCalcParam cParamList 
          cAvailFields cFieldType cCalcProc cAvailCalcProc cCalculator 
          cCalcFormula 
      WITH FRAME Dialog-Frame.
  ENABLE btnOK btnAddParameter btnCancel cFieldLabel cDataType cFieldFormat 
         cCalcParam cParamList cAvailFields cFieldType btnClearCalculator 
         btnClearFormula cAvailCalcProc cCalculator cCalcFormula btn7 btn4 btn1 
         btnLeftParenthesis btn8 btn5 btn2 btnRightParenthesis btn0 btn9 btn6 
         btn3 btnPeriod btnMinus btnPlus btnDivide btnMultiply 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCalcProc Dialog-Frame 
PROCEDURE pGetCalcProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCalcProcSign AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcProcList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSignature    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx           AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cFieldName                 = iopcFieldName
            cFieldLabel                = iopcFieldLabel
            cDataType                  = iopcDataType
            cFieldFormat               = iopcFieldFormat
            cCalcProc                  = iopcCalcProc
            cCalcParam:DELIMITER       = "|"
            cCalcParam:LIST-ITEMS      = iopcCalcParam
            cAvailFields:LIST-ITEMS    = ipcFieldList
            cAvailCalcProc:DELIMITER   = "|"
            cAvailCalcProc:LIST-ITEMS  = "<None>"
            cParamList:LIST-ITEM-PAIRS = ipcParamList
            cCalcProcList              = iphDynCalcField:INTERNAL-ENTRIES
            cCalculator                = IF INDEX(iopcCalcFormula,"$") EQ 0 THEN REPLACE(iopcCalcFormula,"|"," ") ELSE ""
            cCalcFormula               = IF INDEX(iopcCalcFormula,"$") NE 0 THEN iopcCalcFormula ELSE ""
            .
        IF cParamList:NUM-ITEMS GT 0 THEN
        ASSIGN
            cParamList:INNER-LINES  = cParamList:NUM-ITEMS
            cParamList:SCREEN-VALUE = cParamList:ENTRY(1)
            .
        DO idx = 1 TO NUM-ENTRIES(cCalcProcList):
            IF ENTRY(idx,cCalcProcList) BEGINS "Calc" THEN DO:
                ASSIGN
                    cCalcProcSign  = ENTRY(idx,cCalcProcList)
                    cSignature     = LC(iphDynCalcField:GET-SIGNATURE(cCalcProcSign))
                    cCalcProcSign  = cCalcProcSign + " ("
                    .
                DO jdx = 3 TO NUM-ENTRIES(cSignature):
                    cCalcProcSign  = cCalcProcSign
                                   + (IF ENTRY(1,ENTRY(jdx,cSignature)," ") EQ "OUTPUT" THEN "OUT " ELSE "")
                                   + ENTRY(3,ENTRY(jdx,cSignature)," ")
                                   + ", "
                                   .
                END. /* do jdx */
                ASSIGN
                    cCalcProcSign = TRIM(cCalcProcSign,", ")
                    cCalcProcSign = cCalcProcSign + ")"
                    .
                cAvailCalcProc:ADD-LAST(cCalcProcSign).
            END. /* if calc proc */
        END. /* do idx */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

