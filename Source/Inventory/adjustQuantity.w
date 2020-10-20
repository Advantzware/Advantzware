&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: inventory/adjustQuantity.w

  Description: Calculator Numpad

  Input Parameters:
      <none>

  Output Parameters:
      oplValueReturned : Logical value to see if a valid value is returned
      opdValue         : Returned Decimal Value

  Author: Mithun Porandla

  Created: 04/30/2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER ipdTotalQuantity    AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER ipdSubUnitCount     AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER ipdSubUnitsPerUnit  AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER iplReqAdjReason     AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opdTotalQuantity    AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opdSubUnitCount     AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opdSubUnitsPerUnit  AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opdPartialQuantity  AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcAdjReasonCode    AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplValueReturned    AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opdValue            AS DECIMAL    NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdEntryField     AS HANDLE    NO-UNDO.
DEFINE VARIABLE dPrevValue       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cValueChanged    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOperation       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lEmptyResult     AS LOGICAL   NO-UNDO INITIAL FALSE.

{inventory/ttInventory.i "NEW SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-30 RECT-31 fiTotalQty fiSubUnits btnDel ~
btnClear btnDiv fiSubUnitCount btn7 btn8 btn9 btnMult fiSubUnitsPerUnit ~
btn4 btn5 btn6 btnMinus btn1 btn2 btn3 btnPlus fiPartial btnZero btnPeriod ~
btnEqual Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiText fiTotalQty fiResult fiSubUnits ~
fiSubUnitCount fiSubUnitsPerUnit fiUnitCount fiPartial fiUnits cbReasonCode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn1 
     LABEL "1" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btn2 
     LABEL "2" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btn3 
     LABEL "3" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btn4 
     LABEL "4" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btn5 
     LABEL "5" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btn6 
     LABEL "6" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btn7 
     LABEL "7" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btn8 
     LABEL "8" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btn9 
     LABEL "9" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btnClear 
     LABEL "CLEAR" 
     SIZE 10 BY 2
     FONT 35.

DEFINE BUTTON btnDel 
     LABEL "Backspace" 
     SIZE 20.2 BY 2
     FONT 35.

DEFINE BUTTON btnDiv 
     LABEL "/" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btnEqual 
     LABEL "=" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btnMinus 
     LABEL "-" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btnMult 
     LABEL "x" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btnPeriod 
     LABEL "." 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btnPlus 
     LABEL "+" 
     SIZE 10 BY 2
     FONT 37.

DEFINE BUTTON btnZero DEFAULT 
     LABEL "0" 
     SIZE 20.2 BY 2
     FONT 37.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 2
     BGCOLOR 8 FONT 37.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 2
     BGCOLOR 8 FONT 37.

DEFINE VARIABLE cbReasonCode AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 61.4 BY 1.29
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiPartial AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiResult AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.6 BY 1.76
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiSubUnitCount AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiSubUnits AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiSubUnitsPerUnit AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiText AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.6 BY 1.19
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiTotalQty AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiUnitCount AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiUnits AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41.2 BY 2.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 1.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiText AT ROW 1.1 COL 144.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 44
     fiTotalQty AT ROW 1.95 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     fiResult AT ROW 2.57 COL 144.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 50
     fiSubUnits AT ROW 3.71 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     btnDel AT ROW 4.52 COL 105.2 WIDGET-ID 4 NO-TAB-STOP 
     btnClear AT ROW 4.52 COL 125.6 WIDGET-ID 2 NO-TAB-STOP 
     btnDiv AT ROW 4.52 COL 136 WIDGET-ID 6 NO-TAB-STOP 
     fiSubUnitCount AT ROW 5.52 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     btn7 AT ROW 6.57 COL 105.2 WIDGET-ID 18 NO-TAB-STOP 
     btn8 AT ROW 6.57 COL 115.4 WIDGET-ID 10 NO-TAB-STOP 
     btn9 AT ROW 6.57 COL 125.6 WIDGET-ID 12 NO-TAB-STOP 
     btnMult AT ROW 6.57 COL 136 WIDGET-ID 14 NO-TAB-STOP 
     fiSubUnitsPerUnit AT ROW 7.38 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     btn4 AT ROW 8.67 COL 105.2 WIDGET-ID 20 NO-TAB-STOP 
     btn5 AT ROW 8.67 COL 115.4 WIDGET-ID 22 NO-TAB-STOP 
     btn6 AT ROW 8.67 COL 125.6 WIDGET-ID 24 NO-TAB-STOP 
     btnMinus AT ROW 8.67 COL 136 WIDGET-ID 26 NO-TAB-STOP 
     fiUnitCount AT ROW 9.19 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     btn1 AT ROW 10.71 COL 105.2 WIDGET-ID 28 NO-TAB-STOP 
     btn2 AT ROW 10.71 COL 115.4 WIDGET-ID 30 NO-TAB-STOP 
     btn3 AT ROW 10.71 COL 125.6 WIDGET-ID 32 NO-TAB-STOP 
     btnPlus AT ROW 10.76 COL 136 WIDGET-ID 34 NO-TAB-STOP 
     fiPartial AT ROW 11.1 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     btnZero AT ROW 12.81 COL 105.2 WIDGET-ID 38 NO-TAB-STOP 
     btnPeriod AT ROW 12.81 COL 125.6 WIDGET-ID 40 NO-TAB-STOP 
     btnEqual AT ROW 12.81 COL 136 WIDGET-ID 42 NO-TAB-STOP 
     fiUnits AT ROW 12.91 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     cbReasonCode AT ROW 14.67 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     Btn_OK AT ROW 15.24 COL 105.4
     Btn_Cancel AT ROW 15.24 COL 131
     "Adjust  Reason Code:" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 14.67 COL 2 WIDGET-ID 116
          FONT 36
     "Units:" VIEW-AS TEXT
          SIZE 9.4 BY 1.33 AT ROW 12.91 COL 24.2 WIDGET-ID 112
          FONT 36
     "Unit Count:" VIEW-AS TEXT
          SIZE 17.4 BY 1.33 AT ROW 9.19 COL 16.2 WIDGET-ID 104
          FONT 36
     "Sub-Units / Unit:" VIEW-AS TEXT
          SIZE 24.4 BY 1.33 AT ROW 7.38 COL 8.8 WIDGET-ID 100
          FONT 36
     "Sub-Unit Count:" VIEW-AS TEXT
          SIZE 24.2 BY 1.33 AT ROW 5.52 COL 9.8 WIDGET-ID 96
          FONT 36
     "Sub Units:" VIEW-AS TEXT
          SIZE 15.4 BY 1.33 AT ROW 3.71 COL 17.8 WIDGET-ID 92
          FONT 36
     "Total Quantity:" VIEW-AS TEXT
          SIZE 21.4 BY 1.33 AT ROW 1.95 COL 11.2 WIDGET-ID 86
          FONT 36
     "Partial:" VIEW-AS TEXT
          SIZE 11.4 BY 1.33 AT ROW 11.1 COL 22.6 WIDGET-ID 108
          FONT 36
     RECT-30 AT ROW 2.48 COL 105 WIDGET-ID 52
     RECT-31 AT ROW 1 COL 105 WIDGET-ID 54
     SPACE(4.39) SKIP(14.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Insert Value"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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

/* SETTINGS FOR COMBO-BOX cbReasonCode IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiResult IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       fiResult:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiText IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       fiText:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiUnitCount IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUnits IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Insert Value */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.

    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn1 Dialog-Frame
ON CHOOSE OF btn1 IN FRAME Dialog-Frame /* 1 */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2 Dialog-Frame
ON CHOOSE OF btn2 IN FRAME Dialog-Frame /* 2 */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn3 Dialog-Frame
ON CHOOSE OF btn3 IN FRAME Dialog-Frame /* 3 */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn4 Dialog-Frame
ON CHOOSE OF btn4 IN FRAME Dialog-Frame /* 4 */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn5 Dialog-Frame
ON CHOOSE OF btn5 IN FRAME Dialog-Frame /* 5 */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn6 Dialog-Frame
ON CHOOSE OF btn6 IN FRAME Dialog-Frame /* 6 */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn7 Dialog-Frame
ON CHOOSE OF btn7 IN FRAME Dialog-Frame /* 7 */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn8 Dialog-Frame
ON CHOOSE OF btn8 IN FRAME Dialog-Frame /* 8 */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn9 Dialog-Frame
ON CHOOSE OF btn9 IN FRAME Dialog-Frame /* 9 */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear Dialog-Frame
ON CHOOSE OF btnClear IN FRAME Dialog-Frame /* CLEAR */
DO:
    ASSIGN 
        fiText:SCREEN-VALUE       = ""
        fiResult:SCREEN-VALUE     = ""
        dPrevValue                = 0
        hdEntryField:SCREEN-VALUE = fiResult:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDel Dialog-Frame
ON CHOOSE OF btnDel IN FRAME Dialog-Frame /* Backspace */
DO: 
    IF LENGTH(fiResult:SCREEN-VALUE) > 0 THEN
        ASSIGN
            fiResult:SCREEN-VALUE     = SUBSTRING(fiResult:SCREEN-VALUE,1, LENGTH(fiResult:SCREEN-VALUE) - 1)
            hdEntryField:SCREEN-VALUE = fiResult:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDiv Dialog-Frame
ON CHOOSE OF btnDiv IN FRAME Dialog-Frame /* / */
DO:
    ASSIGN 
        dPrevValue = DECIMAL(fiResult:SCREEN-VALUE)
        cOperation = SELF:LABEL
        .
    
    RUN pDisplayValues (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEqual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEqual Dialog-Frame
ON CHOOSE OF btnEqual IN FRAME Dialog-Frame /* = */
DO:
    RUN pGetResult.
        
    ASSIGN 
        fiText:SCREEN-VALUE        = ""
        dPrevValue                 = DECIMAL( fiResult:SCREEN-VALUE ).
    
    IF VALID-HANDLE(hdEntryField) THEN DO:
        hdEntryField:SCREEN-VALUE = fiResult:SCREEN-VALUE.
        APPLY "LEAVE" TO hdEntryField.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMinus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMinus Dialog-Frame
ON CHOOSE OF btnMinus IN FRAME Dialog-Frame /* - */
DO:
    ASSIGN 
        dPrevValue = DECIMAL(fiResult:SCREEN-VALUE)
        cOperation = SELF:LABEL
        .
    
    RUN pDisplayValues (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMult
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMult Dialog-Frame
ON CHOOSE OF btnMult IN FRAME Dialog-Frame /* x */
DO:
    ASSIGN 
        dPrevValue = DECIMAL(fiResult:SCREEN-VALUE)
        cOperation = SELF:LABEL
        .
    
    RUN pDisplayValues (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPeriod Dialog-Frame
ON CHOOSE OF btnPeriod IN FRAME Dialog-Frame /* . */
DO:
    RUN pResultDisplay (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPlus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPlus Dialog-Frame
ON CHOOSE OF btnPlus IN FRAME Dialog-Frame /* + */
DO:
    ASSIGN 
        dPrevValue = DECIMAL(fiResult:SCREEN-VALUE)
        cOperation = SELF:LABEL
        .
    
    RUN pDisplayValues (
        SELF:LABEL
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnZero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnZero Dialog-Frame
ON CHOOSE OF btnZero IN FRAME Dialog-Frame /* 0 */
DO:
    IF INTEGER ( fiResult:SCREEN-VALUE ) GT 0 THEN
        RUN pResultDisplay (
            SELF:LABEL
            ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    ASSIGN
        oplValueReturned = FALSE
        opdValue         = 0.0
        .  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    IF iplReqAdjReason AND (cbReasonCode:SCREEN-VALUE EQ "" OR cbReasonCode:SCREEN-VALUE EQ ?) THEN DO:
        MESSAGE "Adjust Reason code is required"
        VIEW-AS ALERT-BOX ERROR.    
        
        RETURN NO-APPLY.
    END.
    
    ASSIGN
        opdTotalQuantity   = DECIMAL(fiTotalQty:SCREEN-VALUE)
        opdSubUnitCount    = DECIMAL(fiSubUnitCount:SCREEN-VALUE)
        opdSubUnitsPerUnit = DECIMAL(fiSubUnitsPerUnit:SCREEN-VALUE)
        opdPartialQuantity = DECIMAL(fiPartial:SCREEN-VALUE)
        opcAdjReasonCode   = cbReasonCode:SCREEN-VALUE
        oplValueReturned   = TRUE
        opdValue           = DECIMAL(fiResult:SCREEN-VALUE)
        .  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPartial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPartial Dialog-Frame
ON ENTRY OF fiPartial IN FRAME Dialog-Frame
DO:
    ASSIGN
        hdEntryField  = SELF:HANDLE
        cValueChanged = SELF:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPartial Dialog-Frame
ON LEAVE OF fiPartial IN FRAME Dialog-Frame
DO:
    IF cValueChanged NE SELF:SCREEN-VALUE THEN DO:
        fiTotalQty:SCREEN-VALUE = STRING(
                                  DYNAMIC-FUNCTION (
                                  "fCalculateQuantityTotal" IN hdInventoryProcs,
                                  DECIMAL(fiSubUnits:SCREEN-VALUE),
                                  DECIMAL(fiSubUnitCount:SCREEN-VALUE),
                                  DECIMAL(SELF:SCREEN-VALUE)
                                  )).
        RUN pCalculateQuantities (
            DECIMAL(fiTotalQty:SCREEN-VALUE),
            DECIMAL(fiSubUnitCount:SCREEN-VALUE),
            DECIMAL(fiSubUnitsPerUnit:SCREEN-VALUE)
            ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSubUnitCount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSubUnitCount Dialog-Frame
ON ENTRY OF fiSubUnitCount IN FRAME Dialog-Frame
DO:
    ASSIGN
        hdEntryField  = SELF:HANDLE
        cValueChanged = SELF:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSubUnitCount Dialog-Frame
ON LEAVE OF fiSubUnitCount IN FRAME Dialog-Frame
DO:
    SELF:SCREEN-VALUE = STRING(MAX(1,DECIMAL(SELF:SCREEN-VALUE))).
    
    IF cValueChanged NE SELF:SCREEN-VALUE THEN DO:
        ASSIGN
            fiPartial:SCREEN-VALUE = "0"
            fiTotalQty:SCREEN-VALUE = STRING(
                                      DYNAMIC-FUNCTION (
                                      "fCalculateQuantityTotal" IN hdInventoryProcs,
                                      DECIMAL(fiSubUnits:SCREEN-VALUE),
                                      DECIMAL(fiSubUnitCount:SCREEN-VALUE),
                                      DECIMAL(fiPartial:SCREEN-VALUE)
                                      )).
        RUN pCalculateQuantities (
            DECIMAL(fiTotalQty:SCREEN-VALUE),
            DECIMAL(fiSubUnitCount:SCREEN-VALUE),
            DECIMAL(fiSubUnitsPerUnit:SCREEN-VALUE)
            ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSubUnits
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSubUnits Dialog-Frame
ON ENTRY OF fiSubUnits IN FRAME Dialog-Frame
DO:
    ASSIGN
        hdEntryField   = SELF:HANDLE
        cValueChanged  = SELF:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSubUnits Dialog-Frame
ON LEAVE OF fiSubUnits IN FRAME Dialog-Frame
DO:    
    SELF:SCREEN-VALUE = STRING(MAX(1,DECIMAL(SELF:SCREEN-VALUE))).
    
    IF cValueChanged NE SELF:SCREEN-VALUE THEN DO:
        ASSIGN
            fiPartial:SCREEN-VALUE = "0"
            fiTotalQty:SCREEN-VALUE = STRING(
                                      DYNAMIC-FUNCTION (
                                      "fCalculateQuantityTotal" IN hdInventoryProcs,
                                      DECIMAL(SELF:SCREEN-VALUE),
                                      DECIMAL(fiSubUnitCount:SCREEN-VALUE),
                                      DECIMAL(fiPartial:SCREEN-VALUE)
                                      )).
        RUN pCalculateQuantities (
            DECIMAL(fiTotalQty:SCREEN-VALUE),
            DECIMAL(fiSubUnitCount:SCREEN-VALUE),
            DECIMAL(fiSubUnitsPerUnit:SCREEN-VALUE)
            ).                   
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSubUnitsPerUnit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSubUnitsPerUnit Dialog-Frame
ON ENTRY OF fiSubUnitsPerUnit IN FRAME Dialog-Frame
DO:
    hdEntryField = SELF:HANDLE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSubUnitsPerUnit Dialog-Frame
ON LEAVE OF fiSubUnitsPerUnit IN FRAME Dialog-Frame
DO:
    SELF:SCREEN-VALUE = STRING(MAX(1,DECIMAL(SELF:SCREEN-VALUE))).
    RUN pCalculateQuantities (
        DECIMAL(fiTotalQty:SCREEN-VALUE),
        DECIMAL(fiSubUnitCount:SCREEN-VALUE),
        DECIMAL(fiSubUnitsPerUnit:SCREEN-VALUE)
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTotalQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTotalQty Dialog-Frame
ON ENTRY OF fiTotalQty IN FRAME Dialog-Frame
DO:
    hdEntryField = SELF:HANDLE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTotalQty Dialog-Frame
ON LEAVE OF fiTotalQty IN FRAME Dialog-Frame
DO:
    RUN pCalculateQuantities (
        DECIMAL(fiTotalQty:SCREEN-VALUE),
        DECIMAL(fiSubUnitCount:SCREEN-VALUE),
        DECIMAL(fiSubUnitsPerUnit:SCREEN-VALUE)
        ).    
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
    RUN pInit.
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
  DISPLAY fiText fiTotalQty fiResult fiSubUnits fiSubUnitCount fiSubUnitsPerUnit 
          fiUnitCount fiPartial fiUnits cbReasonCode 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-30 RECT-31 fiTotalQty fiSubUnits btnDel btnClear btnDiv 
         fiSubUnitCount btn7 btn8 btn9 btnMult fiSubUnitsPerUnit btn4 btn5 btn6 
         btnMinus btn1 btn2 btn3 btnPlus fiPartial btnZero btnPeriod btnEqual 
         Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalculateQuantities Dialog-Frame 
PROCEDURE pCalculateQuantities :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdTotalQuantity   AS DECIMAL    NO-UNDO.
    DEFINE INPUT PARAMETER ipdSubUnitCount    AS DECIMAL    NO-UNDO.
    DEFINE INPUT PARAMETER ipdSubUnitsPerUnit AS DECIMAL    NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiSubUnits:SCREEN-VALUE  = STRING(DYNAMIC-FUNCTION(
                                       "fCalculateQuantitySubUnits" IN hdInventoryProcs,
                                       ipdTotalQuantity,
                                       ipdSubUnitCount)
                                       )
        fiUnitCount:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION(
                                       "fCalculateQuantityUnitCount" IN hdInventoryProcs,
                                       ipdSubUnitCount,
                                       ipdSubUnitsPerUnit)
                                       )
        fiPartial:SCREEN-VALUE   = STRING(DYNAMIC-FUNCTION(
                                       "fCalculateQuantityPartialSubUnit" IN hdInventoryProcs,
                                       ipdTotalQuantity,
                                       DECIMAL(fiSubUnits:SCREEN-VALUE),
                                       ipdSubUnitCount)
                                       )
        fiUnits:SCREEN-VALUE     = STRING(DYNAMIC-FUNCTION(
                                       "fCalculateQuantityUnits" IN hdInventoryProcs,
                                       DECIMAL(fiSubUnits:SCREEN-VALUE),
                                       ipdSubUnitsPerUnit,
                                       DECIMAL(fiPartial:SCREEN-VALUE))
                                       )
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayValues Dialog-Frame 
PROCEDURE pDisplayValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcSymbol AS CHARACTER NO-UNDO.
    
    lEmptyResult = TRUE.
    
    DO WITH FRAME {&FRAME-NAME}:                     
        IF DECIMAL(fiResult:SCREEN-VALUE) NE 0 THEN
            fiText:SCREEN-VALUE = fiResult:SCREEN-VALUE + ipcSymbol.
        ELSE IF fiText:SCREEN-VALUE NE "" THEN
            fiText:SCREEN-VALUE = REPLACE (fiText:SCREEN-VALUE,
                                  SUBSTRING(fiText:SCREEN-VALUE, LENGTH(fiText:SCREEN-VALUE), 1),
                                  ipcSymbol).
    END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetResult Dialog-Frame 
PROCEDURE pGetResult :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    CASE cOperation:
        WHEN "+" THEN
            fiResult:SCREEN-VALUE = STRING(dPrevValue + DECIMAL(fiResult:SCREEN-VALUE)).
        WHEN "-" THEN
            fiResult:SCREEN-VALUE = STRING(dPrevValue - DECIMAL(fiResult:SCREEN-VALUE)). 
        WHEN "X" THEN
            fiResult:SCREEN-VALUE = STRING(dPrevValue * DECIMAL(fiResult:SCREEN-VALUE)). 
        WHEN "/" THEN
            IF DECIMAL(fiResult:SCREEN-VALUE) NE 0 THEN
                fiResult:SCREEN-VALUE = STRING(dPrevValue / DECIMAL(fiResult:SCREEN-VALUE)).
    END CASE.
    
    lEmptyResult = TRUE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit Dialog-Frame 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cComboList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdReasonCode AS HANDLE    NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    
    ASSIGN
        fiTotalQty:SCREEN-VALUE        = STRING(ipdTotalQuantity)
        fiSubUnitCount:SCREEN-VALUE    = STRING(MAX(1,ipdSubUnitCount))
        fiSubUnitsPerUnit:SCREEN-VALUE = STRING(MAX(1,ipdSubUnitsPerUnit))
        .
    
    RUN pCalculateQuantities (
        DECIMAL(fiTotalQty:SCREEN-VALUE),
        DECIMAL(fiSubUnitCount:SCREEN-VALUE),
        DECIMAL(fiSubUnitsPerUnit:SCREEN-VALUE)
        ).

    IF iplReqAdjReason THEN DO:     
        RUN fg/ReasonCode.p PERSISTENT SET hdReasonCode.
        
        RUN pBuildReasonCode IN hdReasonCode (
            INPUT "ADJ",
            OUTPUT cComboList
            ).
            
        DELETE OBJECT hdReasonCode.
    
        IF cComboList EQ "" THEN 
            cComboList = ?.
     
        ASSIGN
            cbReasonCode:SENSITIVE       = TRUE
            cbReasonCode:LIST-ITEM-PAIRS = cComboList
            .
    END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResultDisplay Dialog-Frame 
PROCEDURE pResultDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcValue AS CHARACTER NO-UNDO. 
     
    DO WITH FRAME {&FRAME-NAME}:
        IF lEmptyResult THEN
            ASSIGN
                fiResult:SCREEN-VALUE = ""
                lEmptyResult          = FALSE
                .
       
        fiResult:SCREEN-VALUE = fiResult:SCREEN-VALUE + ipcValue.
        
        IF VALID-HANDLE(hdEntryField) THEN    
            hdEntryField:SCREEN-VALUE = fiResult:SCREEN-VALUE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

