&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: est/dPriceMatrixChecker.w
  
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
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.

/* Local Variable Definitions ---                                       */

{oe/ttPriceHold.i "NEW SHARED"}
DEFINE TEMP-TABLE ttPriceResults
    LIKE ttPriceHold
    FIELD dPriceOld AS DECIMAL 
    FIELD cPriceOldUOM AS CHARACTER 
    FIELD dPrice AS DECIMAL
    FIELD cPriceUOM AS CHARACTER 
    .

DEFINE VARIABLE hdPriceProcs AS HANDLE.
RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME bPriceHold

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttPriceResults

/* Definitions for BROWSE bPriceHold                                    */
&Scoped-define FIELDS-IN-QUERY-bPriceHold ttPriceResults.cFGItemID ttPriceResults.dQuantity ttPriceResults.lPriceHold ttPriceResults.cPriceHoldReason ttPriceResults.lMatrixMatch ttPriceResults.cMatrixMatch   
&Scoped-define ENABLED-FIELDS-IN-QUERY-bPriceHold   
&Scoped-define SELF-NAME bPriceHold
&Scoped-define QUERY-STRING-bPriceHold FOR EACH ttPriceResults
&Scoped-define OPEN-QUERY-bPriceHold OPEN QUERY {&SELF-NAME} FOR EACH ttPriceResults.
&Scoped-define TABLES-IN-QUERY-bPriceHold ttPriceResults
&Scoped-define FIRST-TABLE-IN-QUERY-bPriceHold ttPriceResults


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-bPriceHold}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiOrder btnLookup btnCheck bPriceHold ~
btnClose 
&Scoped-Define DISPLAYED-OBJECTS fiOrder fiHold fiReason 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCheck 
     LABEL "Check" 
     SIZE 15 BY 1.

DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btnLookup 
     IMAGE-UP FILE "images/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.

DEFINE VARIABLE fiHold AS CHARACTER FORMAT "X(256)":U 
     LABEL "Should Order Be On Hold?" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiOrder AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enter Order Number" 
     VIEW-AS FILL-IN 
     SIZE 78 BY 1
     FONT 1 NO-UNDO.

DEFINE VARIABLE fiReason AS CHARACTER FORMAT "X(256)":U 
     LABEL "Why?" 
     VIEW-AS FILL-IN 
     SIZE 119 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY bPriceHold FOR 
      ttPriceResults SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE bPriceHold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS bPriceHold Dialog-Frame _FREEFORM
  QUERY bPriceHold DISPLAY
      ttPriceResults.cFGItemID FORMAT "x(15)" COLUMN-LABEL "FG Item" WIDTH 20
      ttPriceResults.dQuantity FORMAT ">,>>>,>>9" COLUMN-LABEL "Quantity"
      ttPriceResults.dPriceOld FORMAT ">>>,>>9.99" COLUMN-LABEL "Old Price"
      ttPriceResults.dPrice FORMAT ">>>,>>9.99" COLUMN-LABEL "New Price"
      ttPriceResults.lMatrixMatch FORMAT "Y/N" COLUMN-LABEL "Matrix Found"
      ttPriceResults.cMatrixMatch FORMAT "x(300)" COLUMN-LABEL "Matrix Match Detail" WIDTH 300
      ttPriceResults.lPriceHold FORMAT "Y/N" COLUMN-LABEL "Price Hold?"
      ttPriceResults.cPriceHoldReason FORMAT "x(100)" COLUMN-LABEL "Price Hold Reason" WIDTH 40
      ttPriceResults.cPriceHoldDetail FORMAT "x(300)" COLUMN-LABEL "Price Hold Detail" WIDTH 300
      
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 177 BY 15.48
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiOrder AT ROW 1.95 COL 27 COLON-ALIGNED WIDGET-ID 6
     btnLookup AT ROW 1.95 COL 107 WIDGET-ID 4
     btnCheck AT ROW 1.95 COL 113 WIDGET-ID 164
     fiHold AT ROW 3.14 COL 37 COLON-ALIGNED WIDGET-ID 166
     fiReason AT ROW 4.33 COL 37 COLON-ALIGNED WIDGET-ID 168
     bPriceHold AT ROW 5.52 COL 2 WIDGET-ID 100
     btnClose AT ROW 21.71 COL 79 WIDGET-ID 2
     SPACE(75.19) SKIP(0.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Check Price Matrix for Order"
         CANCEL-BUTTON btnClose.


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
/* BROWSE-TAB bPriceHold fiReason Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN fiHold IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiReason IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bPriceHold
/* Query rebuild information for BROWSE bPriceHold
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttPriceResults
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE bPriceHold */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Check Price Matrix for Order */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCheck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCheck Dialog-Frame
ON CHOOSE OF btnCheck IN FRAME Dialog-Frame /* Check */
DO:
    SESSION:SET-WAIT-STATE("GENERAL").
    ASSIGN fiOrder.
    IF fiOrder NE "" THEN DO:
        RUN BuildResults(fiOrder).
        {&CLOSE-QUERY-bPriceHold}   
        {&OPEN-QUERY-bPriceHold}
    END.
    ELSE DO:
        MESSAGE "No order number entered."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "entry" TO fiOrder.
    END.
    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose Dialog-Frame
ON CHOOSE OF btnClose IN FRAME Dialog-Frame /* Close */
DO:

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLookup Dialog-Frame
ON CHOOSE OF btnLookup IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE cOrder AS CHARACTER   NO-UNDO.

    ASSIGN fiOrder:SCREEN-VALUE = cOrder.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bPriceHold
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
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildResults Dialog-Frame 
PROCEDURE BuildResults :
/*------------------------------------------------------------------------------
  Purpose: Builds the Attributes temp table for the cadFile passed in    
  Parameters:  ipcCadFile as character
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcOrderNo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lHold AS LOGICAL NO-UNDO.
DEFINE VARIABLE cReason AS CHARACTER NO-UNDO.

EMPTY TEMP-TABLE ttPriceResults.
FIND FIRST oe-ord NO-LOCK 
    WHERE oe-ord.company EQ ipcCompany
    AND oe-ord.ord-no EQ INTEGER(ipcOrderNo)
    NO-ERROR.    
IF AVAILABLE oe-ord THEN DO:
    /*builds the shared temp table ttPriceHold*/
    RUN CheckPriceHoldForOrder IN hdPriceProcs (ROWID(oe-ord),  "", "", "",  0,  
    NO, NO, OUTPUT lHold, OUTPUT cReason).
    ASSIGN 
        fiHold = STRING(lHold)
        fiReason = STRING(cReason)
        fiHold:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(lHold)
        fiReason:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cReason 
        .
    FOR EACH ttPriceHold NO-LOCK:
        CREATE ttPriceResults.
        BUFFER-COPY ttPriceHold TO ttPriceResults.
        FIND FIRST oe-ordl NO-LOCK 
            WHERE ROWID(oe-ordl) EQ ttPriceResults.riLine
        NO-ERROR.
        IF AVAILABLE oe-ordl THEN 
            ASSIGN 
                ttPriceResults.dPrice = oe-ordl.price
                ttPriceResults.dPriceOld = oe-ordl.price
                ttPriceResults.cPriceUOM = oe-ordl.pr-uom
                ttPriceResults.cPriceOldUOM = oe-ordl.pr-uom
                .
        RUN CalculateLinePrice IN hdPriceProcs (ttPriceResults.riLine, ttPriceResults.cFGItemID, ttPriceResults.cCustID, ttPriceResults.cShipID, ttPriceResults.dQuantity, NO,
                            OUTPUT ttPriceResults.lMatrixMatch, INPUT-OUTPUT ttPriceResults.dPrice, INPUT-OUTPUT ttPriceResults.cPriceUOM).
        
    END.
END. 
ELSE 
    MESSAGE "Invalid Order" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY fiOrder fiHold fiReason 
      WITH FRAME Dialog-Frame.
  ENABLE fiOrder btnLookup btnCheck bPriceHold btnClose 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

