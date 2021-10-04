&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: oe/dSelEStItem.w
  
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
 {oe\ttInputOrd.i} 
 
/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcSourceValue AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustomerPo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdPrice AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipcPrUom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiQty AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiQuoteNumber AS INTEGER NO-UNDO. 
DEFINE OUTPUT PARAMETER oplBack AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplCancel AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttEstItem.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}               

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSelectTrigger AS LOGICAL NO-UNDO.
DEFINE VARIABLE iLine AS INTEGER NO-UNDO.
RUN spGetSessionParam ("Company", OUTPUT cCompany).
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttEstItem

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttEstItem.isSelected ttEstItem.estLine ttEstItem.estCust ttEstItem.estItem ttEstItem.estPart ttEstItem.estDesc ttEstItem.estQty ttEstItem.estQtyUom ttEstItem.estPrice ttEstItem.estPrUom ttEstItem.estPo ttEstItem.estTotal ttEstItem.estQuote ttEstItem.estPriceMatrix   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 ttEstItem.isSelected ttEstItem.estQty ttEstItem.estQtyUom  
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttEstItem WHERE ttEstItem.Company = cCompany ~         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttEstItem WHERE ttEstItem.Company = cCompany ~         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttEstItem
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttEstItem


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Back Btn_OK Btn_Cancel BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS cEstNo cCustNo ship-to cCustPo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
    LABEL "&Cancel" 
    SIZE 15 BY 1.29
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "&Next" 
    SIZE 15 BY 1.29
    BGCOLOR 8 .
  
DEFINE BUTTON Btn_Back AUTO-GO 
    LABEL "&Back" 
    SIZE 15 BY 1.29
    BGCOLOR 8 .

DEFINE VARIABLE cCustNo AS CHARACTER FORMAT "X(8)":U 
    LABEL "Customer ID#" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustPo AS CHARACTER FORMAT "X(15)":U 
    LABEL "Customer PO#" 
    VIEW-AS FILL-IN 
    SIZE 20.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cEstNo  AS CHARACTER FORMAT "X(8)":U 
    LABEL "Estimate #" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-to AS CHARACTER FORMAT "X(8)":U 
    LABEL "Ship To" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 240.6 BY 22.33
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-5
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 237.2 BY 17.57
    BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
    ttEstItem SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _FREEFORM
    QUERY BROWSE-1 DISPLAY
    ttEstItem.isSelected COLUMN-LABEL '[]'  VIEW-AS TOGGLE-BOX 
    ttEstItem.estLine  WIDTH 5 FORMAT ">9"
    ttEstItem.estCust LABEL-BGCOLOR 14 FORMAT "x(8)"
    ttEstItem.estItem FORMAT "x(15)" WIDTH 21 
    ttEstItem.estPart FORMAT "x(15)" WIDTH 18 
    ttEstItem.estDesc FORMAT "x(30)" WIDTH 32 
    ttEstItem.estQty FORMAT "->>,>>>,>>9" WIDTH 16 
    ttEstItem.estQtyUom  WIDTH 8 
    ttEstItem.estPrice FORMAT "->>,>>>,>>9.99" WIDTH 16 
    ttEstItem.estPrUom WIDTH 8 
    ttEstItem.estPo FORMAT "x(15)" WIDTH 18 
    ttEstItem.estTotal FORMAT "->>,>>>,>>>,>>9.99" WIDTH 21 
    ttEstItem.estQuote WIDTH 22 FORMAT "->>>,>>>,>>9.99" 
    ttEstItem.estPriceMatrix WIDTH 15 FORMAT "Yes/No" 
    ENABLE ttEstItem.isSelected
           ttEstItem.estQty
           ttEstItem.estQtyUom
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 235.4 BY 16.52
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
    cEstNo AT ROW 2.24 COL 19.6 COLON-ALIGNED WIDGET-ID 266
    cCustNo AT ROW 2.24 COL 56.8 COLON-ALIGNED WIDGET-ID 176
    ship-to AT ROW 2.24 COL 92 COLON-ALIGNED WIDGET-ID 272
    cCustPo AT ROW 2.24 COL 132.2 COLON-ALIGNED WIDGET-ID 274
    Btn_Back AT ROW 21.99 COL 12.2
    Btn_OK AT ROW 21.99 COL 192.2
    Btn_Cancel AT ROW 21.99 COL 208.2
    BROWSE-1 AT ROW 4.52 COL 5.6
    "Estimate Header" VIEW-AS TEXT
    SIZE 20 BY .71 AT ROW 1.19 COL 5 WIDGET-ID 206
    "Estimate Item" VIEW-AS TEXT
    SIZE 16 BY .71 AT ROW 3.62 COL 7.4 WIDGET-ID 264
    RECT-4 AT ROW 1.48 COL 2 WIDGET-ID 236
    RECT-5 AT ROW 4 COL 4.4 WIDGET-ID 262
    SPACE(2.39) SKIP(2.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE ""
    CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-1 Btn_Cancel D-Dialog */
ASSIGN 
    FRAME D-Dialog:SCROLLABLE = FALSE
    FRAME D-Dialog:HIDDEN     = TRUE.
    
    ASSIGN 
       BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME D-Dialog = TRUE.

/* SETTINGS FOR FILL-IN cCustNo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cCustPo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cEstNo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship-to IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttEstItem WHERE ttEstItem.Company = cCompany ~
        ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Add Order */
    DO:                    
        oplCancel = YES.        
        APPLY "END-ERROR":U TO SELF.       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
    DO:            
        oplCancel = YES.           
        APPLY "END-ERROR":U TO SELF.      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Back D-Dialog
ON CHOOSE OF Btn_Back IN FRAME D-Dialog /* Back */
    DO:
        
        oplBack = YES.
        
        APPLY "close" TO THIS-PROCEDURE.        
         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Save */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
        iCount = 0.
        FOR EACH ttEstItem NO-LOCK
            WHERE ttEstItem.isSelected:
            iCount = iCount + 1.
        END.

        IF iCount LE 0 THEN
        DO:
            MESSAGE "Please select atleast one item..." VIEW-AS ALERT-BOX INFORMATION .             
            RETURN NO-APPLY.
        END.       
        oplBack = NO.
        APPLY "close" TO THIS-PROCEDURE.        
         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttEstItem.isSelected BROWSE-1 _BROWSE-COLUMN D-Dialog
ON VALUE-CHANGED OF ttEstItem.isSelected IN BROWSE BROWSE-1 /* select */
    DO:
        ASSIGN 
            ttEstItem.isSelected = LOGICAL(ttEstItem.isSelected:SCREEN-VALUE IN BROWSE {&browse-name}).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttEstItem.estQty BROWSE-1 _BROWSE-COLUMN D-Dialog
ON VALUE-CHANGED OF ttEstItem.estQty IN BROWSE BROWSE-1 /* qty */
    DO:
        ASSIGN 
            ttEstItem.estQty = INTEGER(ttEstItem.estQty:SCREEN-VALUE IN BROWSE {&browse-name}).
            
         RUN Conv_CalcTotalPrice(cCompany,            
                        ttEstItem.estItem,
                        DECIMAL(ttEstItem.estQty),
                        DECIMAL(ttEstItem.estPrice),
                        ttEstItem.estPrUom,
                        0,
                        0,    
                        OUTPUT ttEstItem.estTotal).  
         ttEstItem.estTotal:SCREEN-VALUE IN BROWSE {&browse-name} =  string(ttEstItem.estTotal).               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME 

&Scoped-define BROWSE-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttEstItem.estQtyUom BROWSE-1 _BROWSE-COLUMN D-Dialog
ON VALUE-CHANGED OF ttEstItem.estQtyUom IN BROWSE BROWSE-1 /* qty-uom */
    DO:
        ASSIGN 
            ttEstItem.estQtyUom = ttEstItem.estQtyUom:SCREEN-VALUE IN BROWSE {&browse-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttEstItem.estQty BROWSE-1 _BROWSE-COLUMN D-Dialog
ON HELP OF ttEstItem.estQty IN BROWSE BROWSE-1 /* qty */
    DO:
        DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
        RUN windows/l-ordqty.w (cCompany, ttEstItem.estNo, "", OUTPUT cReturnValue).
                
        ttEstItem.estQty:SCREEN-VALUE IN BROWSE {&browse-name} =   ENTRY(1,cReturnValue).       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 D-Dialog
ON START-SEARCH OF BROWSE-1 IN FRAME D-Dialog
DO:  
    IF SELF:CURRENT-COLUMN:NAME EQ "isSelected" THEN DO:
        lSelectTrigger = NOT lSelectTrigger.
                     
        FOR EACH ttEstItem:
            ttEstItem.isSelected = lSelectTrigger.  
        END.

        SELF:CURRENT-COLUMN:LABEL = IF lSelectTrigger THEN
                                        "[*] "
                                    ELSE
                                        "[ ] ".

        DO WITH FRAME {&FRAME-NAME}:
         
         {&open-query-{&browse-name}}
        END. 
    END.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpw.i}
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /*{src/adm/template/dialogmn.i}*/
    RUN enable_UI.
    {methods/nowait.i}     
    DO WITH FRAME {&frame-name}:   
        RUN pBuildTable .              
    END. 
    
    IF iLine EQ 1 THEN
    DO:
         FOR EACH ttEstItem:
            ttEstItem.isSelected = YES.  
         END.
        oplBack = NO.        
        //RETURN NO-APPLY.
        //APPLY "close" TO THIS-PROCEDURE.
    END.
   
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.           
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
    /*------------------------------------------------------------------------------
      Purpose:     Dispatched to this procedure when the Record-
                   Source has a new row available.  This procedure
                   tries to get the new row (or foriegn keys) from
                   the Record-Source and process it.
      Parameters:  <none>
    ------------------------------------------------------------------------------*/

    /* Define variables needed by this internal procedure.             */
    {src/adm/template/row-head.i}

    /* Process the newly available records (i.e. display fields,
       open queries, and/or pass records on to any RECORD-TARGETS).    */
    {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
    HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
    DISPLAY cEstNo cCustNo ship-to cCustPo 
        WITH FRAME D-Dialog.
    ENABLE Btn_Back Btn_OK Btn_Cancel BROWSE-1 
        WITH FRAME D-Dialog.
    VIEW FRAME D-Dialog.
    {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildTable D-Dialog 
PROCEDURE pBuildTable :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iLevel   AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-ttEstItem FOR ttEstItem.
    
    DO WITH FRAME {&FRAME-NAME}:
    
        FIND FIRST bf-ttEstItem NO-LOCK 
             WHERE trim(bf-ttEstItem.estNo) EQ trim(ipcSourceValue) NO-ERROR .
                      
        cEstNo:SCREEN-VALUE = ipcSourceValue .
        cCustPo:SCREEN-VALUE = ipcCustomerPo. 
        IF AVAIL bf-ttEstItem THEN
        ASSIGN
            cCustNo:SCREEN-VALUE = bf-ttEstItem.estCust
            ship-to:SCREEN-VALUE = bf-ttEstItem.estShipId.
        
      
        IF AVAIL bf-ttEstItem THEN RETURN.
        
        FIND FIRST est NO-LOCK
            WHERE est.company EQ cCompany
            AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(cEstNo:SCREEN-VALUE))) + TRIM(cEstNo:SCREEN-VALUE)
            NO-ERROR.
     
        IF AVAILABLE est THEN
        DO:
            EMPTY TEMP-TABLE ttEstItem .
            iLine = 0.   
            FOR EACH eb NO-LOCK
                WHERE eb.company EQ cCompany
                AND eb.est-no EQ est.est-no:
                iLine = iLine + 1.
              
                cCustNo:SCREEN-VALUE = eb.cust-no.
                ship-to:SCREEN-VALUE = eb.ship-id.
              
                CREATE ttEstItem.
                ASSIGN
                    ttEstItem.company     = cCompany
                    ttEstItem.estLine    = iLine
                    ttEstItem.estCust    = eb.cust-no
                    ttEstItem.estShipId  = eb.ship-id
                    ttEstItem.estItem    = eb.stock-no
                    ttEstItem.estPart    = eb.part-no
                    ttEstItem.estDesc    = eb.part-dscr1
                    ttEstItem.estQty     = eb.eqty
                    ttEstItem.estQtyUom = "EA"
                    ttEstItem.estTotal   = 0  
                    ttEstItem.estRowid   = ROWID(eb) 
                    ttEstItem.estNo      = eb.est-no
                    .
                  
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ eb.company
                    AND itemfg.i-no    EQ eb.stock-no
                    NO-ERROR.
     
                IF AVAILABLE itemfg THEN 
                DO:
                    //RUN Price_GetPriceMatrix (cCompany, eb.stock-no, eb.cust-no, eb.ship-id, itemfg.i-no, OUTPUT lTaxable).
                    RUN Price_GetPriceMatrixLevel(cCompany, eb.stock-no, eb.cust-no, eb.ship-id, eb.eqty, OUTPUT iLevel).
                    ASSIGN               
                        ttEstItem.estPriceMatrix = IF iLevel NE 0 THEN YES ELSE NO
                        ttEstItem.estPrUom       = itemfg.sell-uom
                        ttEstItem.estPo           = ipcCustomerPo
                        ttEstItem.estPrice        = itemfg.sell-price 
                        . 
                END. 
                
                IF ipiQuoteNumber GT 0 THEN
                DO:
                   ASSIGN
                    ttEstItem.estQty     = ipiQty
                    ttEstItem.estQtyUom  = "EA" 
                    ttEstItem.estPrUom   = ipcPrUom
                    ttEstItem.estPrice   = ipdPrice.
                END.
             
                
                RUN Conv_CalcTotalPrice(eb.company,            
                        ttEstItem.estItem,
                        DECIMAL(ttEstItem.estQty),
                        DECIMAL(ttEstItem.estPrice),
                        ttEstItem.estPrUom,
                        0,
                        0,    
                        OUTPUT ttEstItem.estTotal).               
            END.
            
            FOR EACH est-prep WHERE est-prep.company EQ est.company
                      AND est-prep.est-no EQ est.est-no
                      AND est-prep.simon EQ "S" 
                      AND est-prep.orderID EQ ""  NO-LOCK :
                      
                      FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ est.company NO-LOCK NO-ERROR.
                      FIND FIRST prep NO-LOCK
                           WHERE prep.company EQ est.company 
                           AND prep.code = est-prep.CODE NO-ERROR.
                           
                      iLine = iLine + 1. 
                      
                      CREATE ttEstItem.
                ASSIGN
                    ttEstItem.company     = cCompany
                    ttEstItem.estLine    = iLine
                    ttEstItem.estCust    = ""
                    ttEstItem.estShipId  = ""
                    ttEstItem.estItem    = est-prep.code
                    ttEstItem.estPart    = ""
                    ttEstItem.estDesc    = IF est-prep.dscr <> "" THEN est-prep.dscr ELSE prep.dscr
                    ttEstItem.estQty     = est-prep.qty 
                    ttEstItem.estPrice   = est-prep.cost
                    ttEstItem.estQtyUom  = "EA"
                    ttEstItem.estTotal   = 0 
                    ttEstItem.estPrUom   = "EA"
                    ttEstItem.estRowid   = ROWID(est-prep) 
                    ttEstItem.estNo      = est.est-no
                    .
                    
                    RUN Conv_CalcTotalPrice(cCompany,            
                        ttEstItem.estItem,
                        DECIMAL(ttEstItem.estQty),
                        DECIMAL(ttEstItem.estPrice),
                        ttEstItem.estPrUom,
                        0,
                        0,    
                        OUTPUT ttEstItem.estTotal).
            END.  
          
        END.
        {&open-query-{&browse-name}}     
    END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query D-Dialog 
PROCEDURE repo-query :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.     
    

    CLOSE QUERY BROWSE-1.
    DO WITH FRAME {&FRAME-NAME}:
         
        OPEN QUERY BROWSE-1 FOR EACH ttEstItem
            NO-LOCK BY ttEstItem.estLine.              

        REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

