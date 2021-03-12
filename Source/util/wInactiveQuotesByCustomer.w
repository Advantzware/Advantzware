&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/wInactiveQuotesByCustomer.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Rahul Rawat

  Created: 9th March, 2021

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
DEFINE VARIABLE cLocation       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs   AS HANDLE    NO-UNDO.
DEFINE VARIABLE cocode          AS CHARACTER NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

{util/ttInactiveQuotes.i}

RUN spGetSessionParam (
    INPUT "Company", 
    OUTPUT cocode
    ).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btSimulate btExit RECT-1 RECT-2 ~
fiBeginingCustomer fiEndingCustomer tbPriceMatrix tbQuotes tbVendItemCost ~
fiDirectory tbOpenFiles btExecute 
&Scoped-Define DISPLAYED-OBJECTS fiBeginingCustomer fiEndingCustomer ~
tbPriceMatrix tbQuotes tbVendItemCost fiDirectory tbOpenFiles 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pfSetDirectory C-Win 
FUNCTION pfSetDirectory RETURNS LOGICAL PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExecute 
     IMAGE-UP FILE "Graphics/32x32/execute.png":U
     LABEL "Execute" 
     SIZE 16 BY 1.43.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Exit" 
     SIZE 7.2 BY 1.71.

DEFINE BUTTON btSimulate 
     IMAGE-UP FILE "Graphics/32x32/simulate.png":U NO-FOCUS
     LABEL "Simulate" 
     SIZE 16 BY 1.43.

DEFINE VARIABLE fiBeginingCustomer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Begining Customer" 
     VIEW-AS FILL-IN 
     SIZE 19.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiDirectory AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndingCustomer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ending Customer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 86.6 BY 2.86
     BGCOLOR 21 FGCOLOR 21 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86.6 BY 6.67.

DEFINE VARIABLE tbOpenFiles AS LOGICAL INITIAL no 
     LABEL "Open Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81
     FONT 22 NO-UNDO.

DEFINE VARIABLE tbPriceMatrix AS LOGICAL INITIAL no 
     LABEL "Inactive Price Matrix" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.8 BY .81 NO-UNDO.

DEFINE VARIABLE tbQuotes AS LOGICAL INITIAL no 
     LABEL "Inactive Quotes" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE tbVendItemCost AS LOGICAL INITIAL no 
     LABEL "Inactive Vend Item Cost" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btSimulate AT ROW 11.1 COL 23 WIDGET-ID 16
     btExit AT ROW 1.48 COL 77.6 WIDGET-ID 20
     fiBeginingCustomer AT ROW 4.71 COL 22 COLON-ALIGNED WIDGET-ID 2
     fiEndingCustomer AT ROW 4.71 COL 63 COLON-ALIGNED WIDGET-ID 4
     tbPriceMatrix AT ROW 6.48 COL 8.2 WIDGET-ID 22
     tbQuotes AT ROW 6.48 COL 33 WIDGET-ID 24
     tbVendItemCost AT ROW 6.48 COL 54 WIDGET-ID 26 NO-TAB-STOP 
     fiDirectory AT ROW 8.95 COL 6.2 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     tbOpenFiles AT ROW 9 COL 70.8 WIDGET-ID 12
     btExecute AT ROW 11.1 COL 50 WIDGET-ID 18
     "Simulated Data will get stored in directory:" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 8.14 COL 14.4 WIDGET-ID 8
          BGCOLOR 15 FONT 22
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 6
     RECT-2 AT ROW 3.86 COL 1 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.6 BY 12.62
         BGCOLOR 15  WIDGET-ID 100.


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
         TITLE              = "Mass Inactive Quotes for Inactive Customers"
         HEIGHT             = 12.62
         WIDTH              = 86.6
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
ASSIGN 
       fiDirectory:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Mass Inactive Quotes for Inactive Customers */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mass Inactive Quotes for Inactive Customers */
DO:
  IF VALID-HANDLE(hdOutputProcs) THEN 
      DELETE PROCEDURE hdOutputProcs.   
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON HELP OF FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    CASE FOCUS:NAME :
        WHEN "fiBeginingCustomer" OR 
        WHEN "fiEndingCustomer"  THEN DO:
            RUN system/openLookup.p (
            INPUT  cocode, 
            INPUT  "",  /* Lookup ID */
            INPUT  23,  /* Subject ID */
            INPUT  "",  /* User ID */
            INPUT  0,   /* Param Value ID */
            OUTPUT cFieldsValue, 
            OUTPUT cFoundValue, 
            OUTPUT recFoundRecID
            ).   
            IF cFoundValue <> "" THEN 
                ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
        END.
    END CASE.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExecute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExecute C-Win
ON CHOOSE OF btExecute IN FRAME DEFAULT-FRAME /* Execute */
DO:
    RUN pRunProcess(
        INPUT YES
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Exit */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulate C-Win
ON CHOOSE OF btSimulate IN FRAME DEFAULT-FRAME /* Simulate */
DO:
    RUN pRunProcess(
        INPUT NO
        ).  
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
  {sys/inc/f3helpw.i}  
  RUN enable_UI.
  pfSetDirectory(). 
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
  DISPLAY fiBeginingCustomer fiEndingCustomer tbPriceMatrix tbQuotes 
          tbVendItemCost fiDirectory tbOpenFiles 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btSimulate btExit RECT-1 RECT-2 fiBeginingCustomer fiEndingCustomer 
         tbPriceMatrix tbQuotes tbVendItemCost fiDirectory tbOpenFiles 
         btExecute 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplProcess AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            tbOpenFiles
            tbPriceMatrix
            tbQuotes
            tbVendItemCost
            .          
    END.    
    
    IF NOT (tbPriceMatrix:CHECKED OR tbQuotes:CHECKED OR tbVendItemCost) THEN DO:
        MESSAGE "Atleast one checkbox should be selected"
        VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.    
        
    MESSAGE "Do you want to start the process with the selected parameters?"
        VIEW-AS ALERT-BOX QUESTION
        BUTTON YES-NO
        UPDATE lResponse AS LOGICAL.

    IF NOT lResponse THEN 
        RETURN.



    SESSION:SET-WAIT-STATE("General"). 
    STATUS DEFAULT "Processing...".  
    
    IF NOT iplProcess THEN
        RUN FileSys_CreateDirectory(
            INPUT  cLocation,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ). 
        
    FOR EACH cust NO-LOCK 
        WHERE cust.company EQ cocode
          AND cust.cust-no GE fiBeginingCustomer:SCREEN-VALUE
          AND cust.cust-no LE fiEndingCustomer:SCREEN-VALUE 
          AND cust.active  EQ "I":
              
        IF tbPriceMatrix:CHECKED THEN DO:
            IF iplProcess THEN 
                RUN Price_ExpirePriceMatrixByCust(
                    INPUT cust.company,
                    INPUT cust.cust-no
                    ).
             ELSE DO:
                RUN Price_ExpirePriceMatrixByCustTT(
                    INPUT cust.company,
                    INPUT cust.cust-no,
                    INPUT-OUTPUT TABLE ttOePrmtxCsv
                    ).  
                RUN Output_TempTableToCSV IN hdOutputProcs (
                    INPUT TEMP-TABLE ttOePrmtxCsv:HANDLE,
                    INPUT cLocation + "\PriceMatrix.csv",
                    INPUT TRUE,  /* Export Header */
                    INPUT FALSE, /* Auto increment File name */
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).                      
            END.                      
        END. 
        IF tbQuotes:CHECKED THEN DO:
            IF iplProcess THEN 
                RUN Price_ExpireQuoteByCust(
                    INPUT cust.company,
                    INPUT cust.cust-no
                    ).
             ELSE DO:
                RUN Price_ExpireQuoteByCustTT(
                    INPUT cust.company,
                    INPUT cust.cust-no,
                    INPUT-OUTPUT TABLE ttQuoteHdCsv
                    ).  
                RUN Output_TempTableToCSV IN hdOutputProcs (
                    INPUT TEMP-TABLE ttQuoteHdCsv:HANDLE,
                    INPUT cLocation + "\Quotes.csv",
                    INPUT TRUE,  /* Export Header */
                    INPUT FALSE, /* Auto increment File name */
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).                      
            END.                      
        END.
        IF tbVendItemCost:CHECKED THEN DO:
            IF iplProcess THEN 
                RUN Vendor_ExpirePriceByCust(
                    INPUT cust.company,
                    INPUT cust.cust-no
                    ).
             ELSE DO:
                RUN Vendor_ExpirePriceByCustTT(
                    INPUT cust.company,
                    INPUT cust.cust-no,
                    INPUT-OUTPUT TABLE ttVendItemCostCsv
                    ).  
                RUN Output_TempTableToCSV IN hdOutputProcs (
                    INPUT TEMP-TABLE ttVendItemCostCsv:HANDLE,
                    INPUT cLocation + "\VendItemCost.csv",
                    INPUT TRUE,  /* Export Header */
                    INPUT FALSE, /* Auto increment File name */
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).                      
            END.                      
        END.                             
    END.  
    IF iplProcess THEN 
        MESSAGE "Process Completed"
            VIEW-AS ALERT-BOX INFORMATION. 
    ELSE 
        MESSAGE "Simulation Completed." SKIP
            "Check " + TRIM(cLocation) + " directory for the CSV file."
            VIEW-AS ALERT-BOX INFORMATION.    
    IF tbOpenFiles:CHECKED AND NOT iplProcess THEN DO: 
        IF tbQuotes:CHECKED THEN 
            RUN OS_RunFile(
                INPUT cLocation + "\Quotes.csv",
                OUTPUT lSuccess,
                OUTPUT cMessage
                ). 
        IF tbPriceMatrix:CHECKED THEN          
            RUN OS_RunFile(
                INPUT cLocation + "\PriceMatrix.csv",
                OUTPUT lSuccess,
                OUTPUT cMessage
                ).
        IF tbVendItemCost:CHECKED THEN       
            RUN OS_RunFile(
                INPUT cLocation + "\VendItemCost.csv",
                OUTPUT lSuccess,
                OUTPUT cMessage
                ). 
    END.
    
    pfSetDirectory(). 
     
    EMPTY TEMP-TABLE ttOePrmtxCsv. 
    EMPTY TEMP-TABLE ttQuoteHdCsv.
    EMPTY TEMP-TABLE ttVendItemCostCsv.
              
    SESSION:SET-WAIT-STATE("").   
    STATUS DEFAULT "".                                             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pfSetDirectory C-Win 
FUNCTION pfSetDirectory RETURNS LOGICAL PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN FileSys_GetTempDirectory(
        OUTPUT cLocation
        ).
    cLocation = cLocation + "\ExpirePriceMatrix-" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")+ STRING(DAY(TODAY),"99") + "-" + STRING(TIME,"99999").  
    DO WITH FRAME {&FRAME-NAME}:
        fiDirectory:SCREEN-VALUE = cLocation.
    END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

