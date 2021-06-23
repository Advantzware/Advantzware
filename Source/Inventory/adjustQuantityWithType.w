&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: inventory/adjustQuantityWithType.w

  Description: Calculator Numpad

  Input Parameters:
      <none>

  Output Parameters:
      oplValueReturned : Logical value to see if a valid value is returned
      opdValue         : Returned Decimal Value

  Author: Mithun Porandla

  Created: 11/04/2020
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER ipcItemID           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemName         AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipdJobQuantity      AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER ipdRcvdQuantity     AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER ipdTotalQuantity    AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER ipdSubUnitCount     AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER ipdSubUnitsPerUnit  AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER iplReqAdjReason     AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER iplAllowFractions   AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opdTotalQuantity    AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opdSubUnitCount     AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opdSubUnitsPerUnit  AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opdPartialQuantity  AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcAdjustType       AS CHARACTER  NO-UNDO.
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
DEFINE VARIABLE cRtnChar         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lReqReasonCode   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lDisplayUnits    AS LOGICAL   NO-UNDO.

{custom/globdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-30 RECT-31 RECT-32 rsAdjustType ~
fiTotalQty btnDel btnClear btnDiv fiSubUnits btn7 btn8 btn9 btnMult ~
fiSubUnitCount fiSubUnitsPerUnit btn4 btn5 btn6 btnMinus btn1 btn2 btn3 ~
btnPlus fiPartial btnZero btnPeriod btnEqual Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiItem fiItemName fiJobQty fiRcvdQty ~
fiOnHandQty fiText rsAdjustType fiResult fiTotalQtyLabel fiTotalQty ~
fiSubUnitsLabel fiSubUnits fiSubUnitCountLabel fiSubUnitCount ~
fiSubUnitsPerUnitLabel fiSubUnitsPerUnit fiUnitCountLabel fiUnitCount ~
fiPartialLabel fiPartial fiUnitsLabel fiUnits fiReasonCodeLabel ~
cbReasonCode 

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
     SIZE 61.4 BY 1
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item #" 
     VIEW-AS FILL-IN 
     SIZE 41.4 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiItemName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 77 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiJobQty AS DECIMAL FORMAT "->>>,>>>,>>9.99<<<<":U INITIAL 0 
     LABEL "Job Qty" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiOnHandQty AS DECIMAL FORMAT "->>>,>>>,>>9.99<<<<":U INITIAL 0 
     LABEL "On Hand Qty" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiPartial AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiPartialLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Partial:" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.43
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiRcvdQty AS DECIMAL FORMAT "->>>,>>>,>>9.99<<<<":U INITIAL 0 
     LABEL "Received Qty" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiReasonCodeLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Adjust Reason Code:" 
     VIEW-AS FILL-IN 
     SIZE 30.6 BY 1.43
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiResult AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.6 BY 1.76
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiSubUnitCount AS DECIMAL FORMAT ">,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiSubUnitCountLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Case/Bundle Count:" 
     VIEW-AS FILL-IN 
     SIZE 28.8 BY 1.43
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiSubUnits AS DECIMAL FORMAT ">,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiSubUnitsLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Case/Bundle:" 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1.43
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiSubUnitsPerUnit AS DECIMAL FORMAT ">,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiSubUnitsPerUnitLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Case/Bundle Per Pallet:" 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1.43
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiText AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.6 BY 1.19
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiTotalQty AS DECIMAL FORMAT ">,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiTotalQtyLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Quantity:" 
     VIEW-AS FILL-IN 
     SIZE 14.2 BY 1.43
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiUnitCount AS DECIMAL FORMAT ">,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiUnitCountLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Pallet Count:" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.43
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiUnits AS DECIMAL FORMAT ">,>>>,>>9.99<<<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiUnitsLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Pallets:" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.43
     FONT 36 NO-UNDO.

DEFINE VARIABLE rsAdjustType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Reduce", "Reduce",
"Count", "Count"
     SIZE 61 BY 1.43
     FONT 36 NO-UNDO.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41.2 BY 2.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 1.38.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 148 BY 3.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiItem AT ROW 1.33 COL 15.6 COLON-ALIGNED WIDGET-ID 154 NO-TAB-STOP 
     fiItemName AT ROW 1.33 COL 68 COLON-ALIGNED WIDGET-ID 156 NO-TAB-STOP 
     fiJobQty AT ROW 2.91 COL 15.6 COLON-ALIGNED WIDGET-ID 158 NO-TAB-STOP 
     fiRcvdQty AT ROW 2.91 COL 65 COLON-ALIGNED WIDGET-ID 160 NO-TAB-STOP 
     fiOnHandQty AT ROW 2.91 COL 114.6 COLON-ALIGNED WIDGET-ID 162 NO-TAB-STOP 
     fiText AT ROW 4.91 COL 144.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 44
     rsAdjustType AT ROW 5 COL 18.6 NO-LABEL WIDGET-ID 148 NO-TAB-STOP 
     fiResult AT ROW 6.38 COL 144.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 50
     fiTotalQtyLabel AT ROW 6.95 COL 22.4 COLON-ALIGNED NO-LABEL WIDGET-ID 128
     fiTotalQty AT ROW 6.95 COL 37.2 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     btnDel AT ROW 8.33 COL 105.2 WIDGET-ID 4 NO-TAB-STOP 
     btnClear AT ROW 8.33 COL 125.6 WIDGET-ID 2 NO-TAB-STOP 
     btnDiv AT ROW 8.33 COL 136 WIDGET-ID 6 NO-TAB-STOP 
     fiSubUnitsLabel AT ROW 8.71 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     fiSubUnits AT ROW 8.71 COL 37.2 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     btn7 AT ROW 10.38 COL 105.2 WIDGET-ID 18 NO-TAB-STOP 
     btn8 AT ROW 10.38 COL 115.4 WIDGET-ID 10 NO-TAB-STOP 
     btn9 AT ROW 10.38 COL 125.6 WIDGET-ID 12 NO-TAB-STOP 
     btnMult AT ROW 10.38 COL 136 WIDGET-ID 14 NO-TAB-STOP 
     fiSubUnitCountLabel AT ROW 10.52 COL 7.8 COLON-ALIGNED NO-LABEL WIDGET-ID 136
     fiSubUnitCount AT ROW 10.52 COL 37.2 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     fiSubUnitsPerUnitLabel AT ROW 12.38 COL 5 NO-LABEL WIDGET-ID 138
     fiSubUnitsPerUnit AT ROW 12.38 COL 37.2 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     btn4 AT ROW 12.48 COL 105.2 WIDGET-ID 20 NO-TAB-STOP 
     btn5 AT ROW 12.48 COL 115.4 WIDGET-ID 22 NO-TAB-STOP 
     btn6 AT ROW 12.48 COL 125.6 WIDGET-ID 24 NO-TAB-STOP 
     btnMinus AT ROW 12.48 COL 136 WIDGET-ID 26 NO-TAB-STOP 
     fiUnitCountLabel AT ROW 14.19 COL 17.8 COLON-ALIGNED NO-LABEL WIDGET-ID 140
     fiUnitCount AT ROW 14.19 COL 37.2 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     btn1 AT ROW 14.52 COL 105.2 WIDGET-ID 28 NO-TAB-STOP 
     btn2 AT ROW 14.52 COL 115.4 WIDGET-ID 30 NO-TAB-STOP 
     btn3 AT ROW 14.52 COL 125.6 WIDGET-ID 32 NO-TAB-STOP 
     btnPlus AT ROW 14.57 COL 136 WIDGET-ID 34 NO-TAB-STOP 
     fiPartialLabel AT ROW 16.1 COL 25.8 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     fiPartial AT ROW 16.1 COL 37.2 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     btnZero AT ROW 16.62 COL 105.2 WIDGET-ID 38 NO-TAB-STOP 
     btnPeriod AT ROW 16.62 COL 125.6 WIDGET-ID 40 NO-TAB-STOP 
     btnEqual AT ROW 16.62 COL 136 WIDGET-ID 42 NO-TAB-STOP 
     fiUnitsLabel AT ROW 17.91 COL 25.6 COLON-ALIGNED NO-LABEL WIDGET-ID 144
     fiUnits AT ROW 17.91 COL 37.2 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     Btn_OK AT ROW 19.05 COL 105.4
     Btn_Cancel AT ROW 19.05 COL 131
     fiReasonCodeLabel AT ROW 19.57 COL 6 COLON-ALIGNED NO-LABEL WIDGET-ID 146
     cbReasonCode AT ROW 19.67 COL 37.2 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     RECT-30 AT ROW 6.29 COL 105 WIDGET-ID 52
     RECT-31 AT ROW 4.81 COL 105 WIDGET-ID 54
     RECT-32 AT ROW 1.14 COL 2 WIDGET-ID 152
     SPACE(0.39) SKIP(16.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FONT 36
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
/* SETTINGS FOR FILL-IN fiItem IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiItemName IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiJobQty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOnHandQty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPartialLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRcvdQty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiReasonCodeLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiResult IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       fiResult:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiSubUnitCountLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSubUnitsLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSubUnitsPerUnitLabel IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiText IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       fiText:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiTotalQtyLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUnitCount IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUnitCountLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUnits IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUnitsLabel IN FRAME Dialog-Frame
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
    IF lReqReasonCode AND iplReqAdjReason AND (cbReasonCode:SCREEN-VALUE EQ "" OR cbReasonCode:SCREEN-VALUE EQ ?) THEN DO:
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
        opcAdjustType      = rsAdjustType:SCREEN-VALUE
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
  DISPLAY fiItem fiItemName fiJobQty fiRcvdQty fiOnHandQty fiText rsAdjustType 
          fiResult fiTotalQtyLabel fiTotalQty fiSubUnitsLabel fiSubUnits 
          fiSubUnitCountLabel fiSubUnitCount fiSubUnitsPerUnitLabel 
          fiSubUnitsPerUnit fiUnitCountLabel fiUnitCount fiPartialLabel 
          fiPartial fiUnitsLabel fiUnits fiReasonCodeLabel cbReasonCode 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-30 RECT-31 RECT-32 rsAdjustType fiTotalQty btnDel btnClear btnDiv 
         fiSubUnits btn7 btn8 btn9 btnMult fiSubUnitCount fiSubUnitsPerUnit 
         btn4 btn5 btn6 btnMinus btn1 btn2 btn3 btnPlus fiPartial btnZero 
         btnPeriod btnEqual Btn_OK Btn_Cancel 
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

    IF NOT lDisplayUnits THEN
        RETURN.

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

    DEFINE VARIABLE cSSJobInquiryAdjust AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    ASSIGN
        fiItem:SCREEN-VALUE      = ipcItemID
        fiItemName:SCREEN-VALUE  = ipcItemName    
        fiJobQty:SCREEN-VALUE    = STRING(ipdJobQuantity)
        fiRcvdQty:SCREEN-VALUE   = STRING(ipdRcvdQuantity)
        fiOnHandQty:SCREEN-VALUE = STRING(ipdTotalQuantity)
        .

    RUN sys/ref/nk1look.p (
        INPUT  g_company,            /* Company Code */ 
        INPUT  "SSJobInquiryAdjust", /* sys-ctrl name */
        INPUT  "C",                  /* Output return value */
        INPUT  NO,                   /* Use ship-to */
        INPUT  NO,                   /* ship-to vendor */
        INPUT  "",                   /* ship-to vendor value */
        INPUT  "",                   /* ship-id value */
        OUTPUT cSSJobInquiryAdjust, 
        OUTPUT lRecFound
        ).
    
    lDisplayUnits = TRUE.
     
    IF cSSJobInquiryAdjust EQ "Simple with options" THEN
        lDisplayUnits = NOT lDisplayUnits AND FALSE.
    ELSE IF cSSJobInquiryAdjust EQ "Simple - Reduce Only" THEN
        ASSIGN
            rsAdjustType:SENSITIVE    = FALSE
            rsAdjustType:SCREEN-VALUE = "Reduce"
            lDisplayUnits             = FALSE
            .            
            
    IF NOT lDisplayUnits THEN
        ASSIGN
            fiSubUnits:HIDDEN             = TRUE
            fiSubUnitCount:HIDDEN         = TRUE
            fiSubUnitsPerUnit:HIDDEN      = TRUE
            fiUnitCount:HIDDEN            = TRUE
            fiPartial:HIDDEN              = TRUE
            fiUnits:HIDDEN                = TRUE
            fiSubUnitsLabel:HIDDEN        = TRUE
            fiSubUnitCountLabel:HIDDEN    = TRUE
            fiSubUnitsPerUnitLabel:HIDDEN = TRUE
            fiUnitCountLabel:HIDDEN       = TRUE
            fiPartialLabel:HIDDEN         = TRUE
            fiUnitsLabel:HIDDEN           = TRUE
            fiReasonCodeLabel:ROW         = fiSubUnits:ROW
            cbReasonCode:ROW              = fiSubUnits:ROW
            .
    
    IF NOT iplAllowFractions THEN
        ASSIGN
            fiTotalQty:FORMAT           = ">,>>>,>>>,>>9"
            fiSubUnits:FORMAT           = ">,>>>,>>>,>>9"
            fiSubUnitCount:FORMAT       = ">,>>>,>>>,>>9"
            fiSubUnitsPerUnit:FORMAT    = ">,>>>,>>>,>>9"
            fiUnitCount:FORMAT          = ">,>>>,>>>,>>9"
            fiPartial:FORMAT            = ">,>>>,>>>,>>9"
            fiUnits:FORMAT              = ">,>>>,>>>,>>9"
            NO-ERROR.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

    RUN sys/ref/nk1look.p (
        INPUT  g_company, 
        INPUT  "AdjustReason", 
        INPUT  "L" /* Logical */, 
        INPUT  NO /* check by cust */, 
        INPUT  YES /* use cust not vendor */, 
        INPUT  "" /* cust */, 
        INPUT  "" /* ship-to*/,
        OUTPUT cRtnChar, 
        OUTPUT lRecFound
        ).
    IF lRecFound THEN
        lReqReasonCode = LOGICAL(cRtnChar) NO-ERROR.
    
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

