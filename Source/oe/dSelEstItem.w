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

/* Parameters Definitions ---                                           */


DEFINE INPUT PARAMETER ipcSourceType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcSourceValue AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustomerPo AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplCancel AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}

{methods/defines/globdefs.i}

{oe\ttInputOrd.i }
           
{sys/inc/var.i shared}
{custom/gcompany.i}  

gcompany = cocode.  

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
&Scoped-define INTERNAL-TABLES tt-est-item

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-est-item.IS-SELECTED tt-est-item.est-line tt-est-item.est-cust tt-est-item.est-item tt-est-item.est-part tt-est-item.est-desc tt-est-item.est-qty tt-est-item.est-qty-uom tt-est-item.est-price tt-est-item.est-pr-uom tt-est-item.est-po tt-est-item.est-total tt-est-item.est-quote tt-est-item.est-price-matrix   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt-est-item.IS-SELECTED   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-est-item WHERE tt-est-item.Company = cocode ~         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-est-item WHERE tt-est-item.Company = cocode ~         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-est-item
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-est-item


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
    SIZE 233 BY 22.33
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-5
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 229.6 BY 17.57
    BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
    tt-est-item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _FREEFORM
    QUERY BROWSE-1 DISPLAY
    tt-est-item.IS-SELECTED COLUMN-LABEL ''  VIEW-AS TOGGLE-BOX 
    tt-est-item.est-line  WIDTH 10 FORMAT ">9"
    tt-est-item.est-cust LABEL-BGCOLOR 14 FORMAT "x(8)"
    tt-est-item.est-item FORMAT "x(15)" WIDTH 18 
    tt-est-item.est-part FORMAT "x(15)" WIDTH 18 
    tt-est-item.est-desc FORMAT "x(30)" WIDTH 32 
    tt-est-item.est-qty FORMAT "->>,>>>,>>9" WIDTH 16 
    tt-est-item.est-qty-uom  WIDTH 8 
    tt-est-item.est-price FORMAT "->>,>>>,>>9.99" WIDTH 16 
    tt-est-item.est-pr-uom WIDTH 8 
    tt-est-item.est-po FORMAT "x(15)" WIDTH 18 
    tt-est-item.est-total FORMAT "->>,>>>,>>>,>>9.99" WIDTH 21 
    tt-est-item.est-quote WIDTH 22 FORMAT "->>>,>>>,>>9.99" 
    tt-est-item.est-price-matrix WIDTH 8 FORMAT "Yes/No" 
    ENABLE tt-est-item.IS-SELECTED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 228.4 BY 16.52
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
OPEN QUERY {&SELF-NAME} FOR EACH tt-est-item WHERE tt-est-item.Company = cocode ~
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
       // RUN oe/dSelOrdType.w(OUTPUT cSourceType,OUTPUT cSourceValue, OUTPUT cCustomerPo, OUTPUT lCancel) .
        
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
        FOR EACH tt-est-item NO-LOCK
            WHERE tt-est-item.IS-SELECTED:
            iCount = iCount + 1.
        END.

        IF iCount LE 0 THEN
        DO:
            MESSAGE "Please select atleast one item..." VIEW-AS ALERT-BOX INFORMATION .             
            RETURN NO-APPLY.
        END.       
        
        APPLY "close" TO THIS-PROCEDURE.        
         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-est-item.IS-SELECTED BROWSE-1 _BROWSE-COLUMN D-Dialog
ON VALUE-CHANGED OF tt-est-item.IS-SELECTED IN BROWSE BROWSE-1 /* select */
    DO:
        ASSIGN 
            tt-est-item.IS-SELECTED = LOGICAL(tt-est-item.IS-SELECTED:SCREEN-VALUE IN BROWSE {&browse-name}).
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
        IF ipcSourceType EQ "Estimate" THEN 
        DO:
            RUN pBuildTable .              
        END.    
                
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
    DEFINE VARIABLE iLine    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        cEstNo:SCREEN-VALUE = ipcSourceValue .
        cCustPo:SCREEN-VALUE = ipcCustomerPo.        
      
        FIND FIRST est NO-LOCK
            WHERE est.company EQ g_company
            AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(cEstNo:SCREEN-VALUE))) + TRIM(cEstNo:SCREEN-VALUE)
            NO-ERROR.
     
        IF AVAILABLE est THEN
        DO:
            EMPTY TEMP-TABLE tt-est-item .
            iLine = 0.   
            FOR EACH eb NO-LOCK
                WHERE eb.company EQ cocode
                AND eb.est-no EQ est.est-no:
                iLine = iLine + 1.
              
                cCustNo:SCREEN-VALUE = eb.cust-no.
                ship-to:SCREEN-VALUE = eb.ship-id.
              
                CREATE tt-est-item.
                ASSIGN
                    tt-est-item.company     = cocode
                    tt-est-item.est-line    = iLine
                    tt-est-item.est-cust    = eb.cust-no
                    tt-est-item.est-item    = eb.stock-no
                    tt-est-item.est-part    = eb.part-no
                    tt-est-item.est-desc    = eb.part-dscr1
                    tt-est-item.est-qty     = eb.eqty
                    tt-est-item.est-qty-uom = "EA"
                    tt-est-item.est-total   = 0  
                    tt-est-item.est-rowid   = ROWID(eb) 
                    .
                  
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ eb.company
                    AND itemfg.i-no    EQ eb.stock-no
                    NO-ERROR.
     
                IF AVAILABLE itemfg THEN 
                DO:
                    RUN Tax_GetTaxableAR  (cocode, cCustNo:SCREEN-VALUE, ship-to:SCREEN-VALUE, itemfg.i-no, OUTPUT lTaxable).
               
                    ASSIGN               
                        tt-est-item.est-price-matrix = lTaxable
                        tt-est-item.est-pr-uom       = itemfg.sell-uom
                        tt-est-item.est-po           = ipcCustomerPo
                        tt-est-item.est-price        = itemfg.sell-price 
                        . 
                END. 
             
                IF tt-est-item.est-pr-uom EQ "EA" THEN
                DO:
                    tt-est-item.est-total = tt-est-item.est-price * tt-est-item.est-qty . 
                END.
                                  
              
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
         
        OPEN QUERY BROWSE-1 FOR EACH tt-est-item
            NO-LOCK BY tt-est-item.est-line.              

        REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

