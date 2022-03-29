&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/OrderSurchargeTester.w

  Description: Utility to test the Order surcharge configuration evaluation

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Mithun Porandla

  Created: 11/13/2020

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

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}

DEFINE VARIABLE hdOrderProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.

RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fiOrder btCalculate fiOrderDate ~
fiOrderTimeHours fiOrderTimeMinutes fiExpectedDelDate slNK1Config 
&Scoped-Define DISPLAYED-OBJECTS fiOrder fiOrderDate fiOrderTimeHours ~
fiOrderTimeMinutes fiExpectedDelDate fiOrderDateWeekday ~
fiExpectedDelWeekday fiEvaluationOrderDate slNK1Config fiIsWeekendOrder ~
fiIsSameDayOrder fiIsNextDayOrder fiIsWeekendDelivery fiSurchargeItem ~
fiPrepCost 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCalculate 
     LABEL "Calculate" 
     SIZE 18 BY 1.91.

DEFINE VARIABLE fiEvaluationOrderDate AS CHARACTER FORMAT "X(256)":U 
     LABEL "Evaluation Order Date" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiExpectedDelDate AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiExpectedDelWeekday AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiIsNextDayOrder AS CHARACTER FORMAT "X(256)":U INITIAL "NO" 
     LABEL "Is Next day Order?" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiIsSameDayOrder AS CHARACTER FORMAT "X(256)":U INITIAL "NO" 
     LABEL "Is Same day Order?" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiIsWeekendDelivery AS CHARACTER FORMAT "X(256)":U INITIAL "NO" 
     LABEL "Is Weekend Delivery?" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiIsWeekendOrder AS CHARACTER FORMAT "X(256)":U INITIAL "NO" 
     LABEL "Is Weekend Order?" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiOrder AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Order #" 
     VIEW-AS FILL-IN 
     SIZE 29.8 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiOrderDate AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiOrderDateWeekday AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiOrderTimeHours AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiOrderTimeMinutes AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiPrepCost AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cost" 
     VIEW-AS FILL-IN 
     SIZE 37.2 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiSurchargeItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Surcharge Misc Item" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 150 BY 7.57.

DEFINE VARIABLE slNK1Config AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 69 BY 7.62
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiOrder AT ROW 2 COL 21.2 COLON-ALIGNED WIDGET-ID 42
     btCalculate AT ROW 2.1 COL 128 WIDGET-ID 36
     fiOrderDate AT ROW 5.52 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fiOrderTimeHours AT ROW 5.52 COL 41.2 COLON-ALIGNED WIDGET-ID 4
     fiOrderTimeMinutes AT ROW 5.52 COL 47.2 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiExpectedDelDate AT ROW 5.52 COL 62 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fiOrderDateWeekday AT ROW 7 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fiExpectedDelWeekday AT ROW 7 COL 62 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fiEvaluationOrderDate AT ROW 10.67 COL 39 COLON-ALIGNED WIDGET-ID 22
     slNK1Config AT ROW 10.91 COL 84 NO-LABEL WIDGET-ID 38
     fiIsWeekendOrder AT ROW 12.24 COL 39 COLON-ALIGNED WIDGET-ID 28
     fiIsSameDayOrder AT ROW 13.81 COL 39 COLON-ALIGNED WIDGET-ID 30
     fiIsNextDayOrder AT ROW 15.43 COL 39 COLON-ALIGNED WIDGET-ID 32
     fiIsWeekendDelivery AT ROW 17 COL 39 COLON-ALIGNED WIDGET-ID 34
     fiSurchargeItem AT ROW 19.14 COL 39 COLON-ALIGNED WIDGET-ID 46
     fiPrepCost AT ROW 19.14 COL 91.8 COLON-ALIGNED WIDGET-ID 48
     "OR" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 3.67 COL 23 WIDGET-ID 54
     "Order Date" VIEW-AS TEXT
          SIZE 17 BY .86 AT ROW 4.57 COL 11 WIDGET-ID 14
     ":" VIEW-AS TEXT
          SIZE 1 BY .86 AT ROW 5.76 COL 48.2 WIDGET-ID 6
     "(24 hours format)" VIEW-AS TEXT
          SIZE 25 BY .86 AT ROW 4.57 COL 35.4 WIDGET-ID 10
     "Evaluated NK1 Configuration" VIEW-AS TEXT
          SIZE 50 BY .86 AT ROW 9.81 COL 98 WIDGET-ID 40
     "Promised Date" VIEW-AS TEXT
          SIZE 34 BY .86 AT ROW 4.57 COL 63.8 WIDGET-ID 16
     RECT-1 AT ROW 1.52 COL 4 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.4 BY 21.52
         BGCOLOR 21 FGCOLOR 15 FONT 20 WIDGET-ID 100.


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
         TITLE              = "Order Surcharge Evaluation Tester"
         HEIGHT             = 21.52
         WIDTH              = 155.6
         MAX-HEIGHT         = 21.52
         MAX-WIDTH          = 158.8
         VIRTUAL-HEIGHT     = 21.52
         VIRTUAL-WIDTH      = 158.8
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fiEvaluationOrderDate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiExpectedDelWeekday IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiIsNextDayOrder IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiIsSameDayOrder IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiIsWeekendDelivery IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiIsWeekendOrder IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOrderDateWeekday IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPrepCost IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSurchargeItem IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Order Surcharge Evaluation Tester */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Order Surcharge Evaluation Tester */
DO:
  /* This event will close the window and terminate the procedure.  */
  DELETE PROCEDURE hdOrderProcs.
    
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCalculate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCalculate C-Win
ON CHOOSE OF btCalculate IN FRAME DEFAULT-FRAME /* Calculate */
DO:
    RUN pCalculate.
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
  RUN enable_UI.
  RUN pInit.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY fiOrder fiOrderDate fiOrderTimeHours fiOrderTimeMinutes 
          fiExpectedDelDate fiOrderDateWeekday fiExpectedDelWeekday 
          fiEvaluationOrderDate slNK1Config fiIsWeekendOrder fiIsSameDayOrder 
          fiIsNextDayOrder fiIsWeekendDelivery fiSurchargeItem fiPrepCost 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 fiOrder btCalculate fiOrderDate fiOrderTimeHours 
         fiOrderTimeMinutes fiExpectedDelDate slNK1Config 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalculate C-Win 
PROCEDURE pCalculate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dtOrderDate           AS DATETIME  NO-UNDO.
    DEFINE VARIABLE dtOrderDeliveryDate   AS DATETIME  NO-UNDO.
    DEFINE VARIABLE cSurchargeConfigList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEvaluationOrderDate AS DATETIME  NO-UNDO.
    DEFINE VARIABLE lIsWeekendOrder       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lIsHolidayOrder       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lIsWeekendDelivery    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lIsSameDayDelivery    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lIsNextDayDelivery    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lIsHolidayDelivery    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSurchargeConfig      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrepCode             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound                AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    DEFINE BUFFER bf-prep   FOR prep.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    IF INTEGER(fiOrder:SCREEN-VALUE) NE 0 THEN DO:
        FIND FIRST bf-oe-ord NO-LOCK
             WHERE bf-oe-ord.company EQ cCompany
               AND bf-oe-ord.ord-no  EQ INTEGER(fiOrder:SCREEN-VALUE)
               NO-ERROR.
        IF NOT AVAILABLE bf-oe-ord THEN DO:
            MESSAGE "Invalid order # '" + STRING(fiOrder:SCREEN-VALUE) + "'" 
            VIEW-AS ALERT-BOX ERROR.
            fiOrder:SCREEN-VALUE = "0".
            RETURN.
        END.
        
        IF bf-oe-ord.ord-date EQ ? THEN DO:
            MESSAGE "Order date is not set. Defaulting to today"
            VIEW-AS ALERT-BOX ERROR.
        END.

        IF bf-oe-ord.promiseDate EQ ? THEN DO:
            MESSAGE "Promise date is not set. Defaulting to order date + 10"
            VIEW-AS ALERT-BOX ERROR.
        END.
        
        ASSIGN
            fiOrderDate:SCREEN-VALUE       = IF bf-oe-ord.ord-date EQ ? THEN STRING(TODAY) ELSE STRING(bf-oe-ord.ord-date)
            fiExpectedDelDate:SCREEN-VALUE = IF bf-oe-ord.promiseDate EQ ? THEN STRING(bf-oe-ord.ord-date + 10) ELSE STRING(bf-oe-ord.promiseDate)
            .
    END.
    
    ASSIGN
        fiOrderDateWeekday:SCREEN-VALUE   = DYNAMIC-FUNCTION("sfCommon_GetWeekDayInText", DYNAMIC-FUNCTION("sfCommon_GetWeekDay",DATE(fiOrderDate:SCREEN-VALUE)))
        fiExpectedDelWeekday:SCREEN-VALUE = DYNAMIC-FUNCTION("sfCommon_GetWeekDayInText", DYNAMIC-FUNCTION("sfCommon_GetWeekDay",DATE(fiExpectedDelDate:SCREEN-VALUE)))        
        .

    ASSIGN
        dtOrderDate         = DATETIME(STRING(fiOrderDate:SCREEN-VALUE) + " " + STRING(fiOrderTimeHours:SCREEN-VALUE,'99') + ":" + STRING(fiOrderTimeMinutes:SCREEN-VALUE,'99') + ":00"  )
        dtOrderDeliveryDate = DATE(fiExpectedDelDate:SCREEN-VALUE)
        .
        
    RUN Order_GetSurchargeConfig IN hdOrderProcs (
        INPUT  dtOrderDate,
        INPUT  dtOrderDeliveryDate,
        OUTPUT cSurchargeConfigList
        ).    
    
    RUN Order_GetOrderEvaluationParams IN hdOrderProcs (
        INPUT  dtOrderDate,
        INPUT  dtOrderDeliveryDate,
        OUTPUT dtEvaluationOrderDate,
        OUTPUT lIsWeekendOrder,
        OUTPUT lIsHolidayOrder,
        OUTPUT lIsWeekendDelivery,
        OUTPUT lIsSameDayDelivery,
        OUTPUT lIsNextDayDelivery,
        OUTPUT lIsHolidayDelivery
        ).
    
    IF DYNAMIC-FUNCTION("sfCommon_GetDifferenceDays", dtOrderDeliveryDate, dtEvaluationOrderDate) LT 0 THEN DO:
        MESSAGE "Order Evaluation date" DATE(dtEvaluationOrderDate) 
                ", cannot be greater than Expected delivery date" DATE(dtOrderDeliveryDate)
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.
    
    ASSIGN
        fiEvaluationOrderDate:SCREEN-VALUE = STRING(DATE(dtEvaluationOrderDate)) 
                                           + " (" + DYNAMIC-FUNCTION("sfCommon_GetWeekDayInText", DYNAMIC-FUNCTION("sfCommon_GetWeekDay",dtEvaluationOrderDate))
                                           + ")"
        fiIsWeekendOrder:SCREEN-VALUE      = STRING(lIsWeekendOrder, "YES/NO")
        fiIsSameDayOrder:SCREEN-VALUE      = STRING(lIsSameDayDelivery, "YES/NO")
        fiIsNextDayOrder:SCREEN-VALUE      = STRING(lIsNextDayDelivery, "YES/NO")
        fiIsWeekendDelivery:SCREEN-VALUE   = STRING(lIsWeekendDelivery, "YES/NO") 
        slNK1Config:LIST-ITEMS             = cSurchargeConfigList               
        cSurchargeConfig                   = ENTRY(1, cSurchargeConfigList).        
        .
    
    IF cSurchargeConfig NE "" THEN DO:
        RUN sys/ref/nk1look.p (
            INPUT  cCompany,
            INPUT  cSurchargeConfig,
            INPUT  "C",
            INPUT  YES,
            INPUT  YES,
            INPUT  IF AVAILABLE bf-oe-ord THEN bf-oe-ord.cust-no ELSE "",
            INPUT  IF AVAILABLE bf-oe-ord THEN bf-oe-ord.ship-id ELSE "",
            OUTPUT cPrepCode,
            OUTPUT lFound
            ).
        IF lFound AND cPrepCode NE "" THEN DO:
            FIND FIRST bf-prep NO-LOCK
                 WHERE bf-prep.company EQ cCompany
                   AND bf-prep.code    EQ cPrepCode
                 NO-ERROR.
            IF NOT AVAILABLE bf-prep THEN DO:
                MESSAGE "Invalid surcharge code in NK1 config '" + cSurchargeConfig "'"
                VIEW-AS ALERT-BOX ERROR.  
                RETURN.
            END.    
            
            ASSIGN
                fiSurchargeItem:SCREEN-VALUE = bf-prep.code
                fiPrepCost:SCREEN-VALUE      = STRING(bf-prep.cost)    
                .    
        END.
    END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit C-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiOrderDate:SCREEN-VALUE       = STRING(TODAY)
        fiExpectedDelDate:SCREEN-VALUE = STRING(TODAY)
        .
    
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    
    RUN pCalculate.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

