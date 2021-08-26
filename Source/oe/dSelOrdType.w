&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: oe/dAddOrder.w
  
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
DEFINE OUTPUT PARAMETER opcSourceType AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcSourceValue AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcCustomerPo AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplCancel AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}

{methods/defines/globdefs.i}

DEFINE TEMP-TABLE tt-order-type 
    FIELD company     AS CHARACTER
    FIELD ord-type    AS INTEGER   LABEL "Order Type"
    FIELD ord-source  AS CHARACTER LABEL "Order Source"
    FIELD type-desc   AS CHARACTER LABEL "Type Description"
    FIELD system-type AS CHARACTER LABEL "System Type"
    FIELD cColor      AS CHARACTER LABEL "Color" 
    FIELD lActive     AS LOGICAL   LABEL "Active".
           
{sys/inc/var.i shared}
{custom/gcompany.i}  

gcompany = cocode.  

DEFINE VARIABLE ll-valid-po-no AS LOG     NO-UNDO.
DEFINE VARIABLE lOeprompt      AS LOGICAL NO-UNDO.
DEFINE VARIABLE cRtnChar       AS LOGICAL NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL NO-UNDO.


RUN sys/ref/nk1look.p (INPUT cocode, "OEPROMPT", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lOeprompt = LOGICAL(cRtnChar) NO-ERROR.

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
&Scoped-define INTERNAL-TABLES tt-order-type

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-order-type.ord-type tt-order-type.ord-source tt-order-type.type-desc tt-order-type.system-type tt-order-type.cColor tt-order-type.lActive 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-order-type WHERE tt-order-type.Company = cocode ~         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-order-type WHERE tt-order-type.Company = cocode ~         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-order-type
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-order-type


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS iOrderType cOrderSource cCustPo Btn_OK Btn_Cancel ~
BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS iOrderType cOrderSource cCustPo lblLabel ~
lblLabel-2 

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
    LABEL "&OK" 
    SIZE 15 BY 1.29
    BGCOLOR 8 .

DEFINE VARIABLE cOrderSource AS CHARACTER FORMAT "X(8)":U 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustPo      AS CHARACTER FORMAT "X(15)":U 
    LABEL "Customer PO#" 
    VIEW-AS FILL-IN 
    SIZE 20.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iOrderType   AS INTEGER   FORMAT ">>>>>>>":U INITIAL 0 
    LABEL "Order Type" 
    VIEW-AS FILL-IN 
    SIZE 15.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE lblLabel     AS CHARACTER FORMAT "X(15)":U 
    VIEW-AS FILL-IN 
    SIZE 12.8 BY 1
    FONT 1 NO-UNDO.

DEFINE VARIABLE lblLabel-2   AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 71.4 BY 1
    FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 159 BY 18.33
    BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
    tt-order-type SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _FREEFORM
    QUERY BROWSE-1 DISPLAY
    tt-order-type.ord-type WIDTH 12 LABEL-BGCOLOR 14 FORMAT ">9"
    tt-order-type.ord-source LABEL-BGCOLOR 14 FORMAT "x(20)"
    tt-order-type.type-desc  FORMAT "x(30)" WIDTH 35 LABEL-BGCOLOR 14
    tt-order-type.system-type FORMAT "x(15)" WIDTH 22 LABEL-BGCOLOR 14
    tt-order-type.cColor  FORMAT "x(15)" WIDTH 22 LABEL-BGCOLOR 14
    tt-order-type.lActive  FORMAT "Yes/No" WIDTH 10 LABEL-BGCOLOR 14    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 155.4 BY 14.52
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
    iOrderType AT ROW 2.57 COL 19.6 COLON-ALIGNED WIDGET-ID 268
    lblLabel AT ROW 2.57 COL 40.6 COLON-ALIGNED NO-LABELS WIDGET-ID 278
    cOrderSource AT ROW 2.57 COL 52 COLON-ALIGNED NO-LABELS WIDGET-ID 176
    cCustPo AT ROW 2.57 COL 100.6 COLON-ALIGNED WIDGET-ID 274         
    Btn_OK AT ROW 2.48 COL 129
    Btn_Cancel AT ROW 2.48 COL 145
    BROWSE-1 AT ROW 4.91 COL 3.8
    lblLabel-2 AT ROW 3.71 COL 54.6 COLON-ALIGNED NO-LABELS WIDGET-ID 280
    "Enter New Order" VIEW-AS TEXT
    SIZE 20 BY .71 AT ROW 1.19 COL 5 WIDGET-ID 206
    RECT-4 AT ROW 1.48 COL 2 WIDGET-ID 236
    SPACE(1.19) SKIP(0.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Add Order"
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

/* SETTINGS FOR FILL-IN lblLabel IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lblLabel-2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-order-type WHERE tt-order-type.Company = cocode ~
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
        EMPTY TEMP-TABLE tt-order-type .
        ASSIGN
            opcSourceType  = ""
            opcSourceValue = ""
            opcCustomerPo  = ""
            oplCancel      = YES.
            
        APPLY "END-ERROR":U TO SELF.
      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 D-Dialog
ON VALUE-CHANGED OF BROWSE-1 IN FRAME D-Dialog
    DO:
        RUN pValueChange.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
    DO:         
        EMPTY TEMP-TABLE tt-order-type .
        ASSIGN
            opcSourceType  = ""
            opcSourceValue = ""
            opcCustomerPo  = ""
            oplCancel      = YES.
        APPLY "END-ERROR":U TO SELF.
       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
        
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
           
        IF tt-order-type.ord-source EQ "Customer" THEN
        DO:   
            RUN valid-cust-no(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY .
        END.
        ELSE IF tt-order-type.ord-source EQ "Estimate" THEN
            DO:   
                RUN valid-est-no(OUTPUT lError) NO-ERROR.
                IF lError THEN RETURN NO-APPLY .
            END.
        
        RUN valid-po-no(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.        
        
        ASSIGN
            opcSourceType  = tt-order-type.ord-source 
            opcSourceValue = cOrderSource:SCREEN-VALUE  IN FRAME {&FRAME-NAME}
            opcCustomerPo  = cCustPo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}.
                
        APPLY "close" TO THIS-PROCEDURE.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cOrderSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cOrderSource D-Dialog
ON HELP OF cOrderSource IN FRAME D-Dialog /* Customer ID# */
    DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
        IF tt-order-type.ord-source EQ "Customer" THEN
        DO:          
            RUN windows/l-custact.w (gcompany,"", OUTPUT char-val, OUTPUT look-recid).
            IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
                ASSIGN
                    SELF:screen-value = ENTRY(1,char-val)                
                    .     
        END.
        ELSE IF tt-order-type.ord-source EQ "Estimate" THEN
            DO:
                RUN windows/l-est.w (g_company,g_loc,"", OUTPUT char-val).
                FIND FIRST eb NO-LOCK WHERE RECID(eb) = INT(char-val) NO-ERROR.
                IF AVAILABLE eb THEN 
                DO:
                    ASSIGN
                        cOrderSource:screen-value = eb.est-no  .
                                
                END.
            END.
        APPLY "entry" TO cOrderSource IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cOrderSource D-Dialog
ON LEAVE OF cOrderSource IN FRAME D-Dialog /* Customer ID# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN pCheckPoValidation NO-ERROR.            
        END.
           
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cOrderSource D-Dialog
ON VALUE-CHANGED OF cOrderSource IN FRAME D-Dialog /* Customer ID# */
    DO:     
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
                                              
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCustPo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustPo D-Dialog
ON LEAVE OF cCustPo IN FRAME D-Dialog /* Customer PO# */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:          
        //RUN valid-po-no(OUTPUT lError) NO-ERROR.
        //IF lError THEN RETURN NO-APPLY.         
        END.                                                                  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpw.i}
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    
    RUN pBuildTempTable. 
    
    RUN enable_UI.
    {methods/nowait.i}     
    DO WITH FRAME {&frame-name}:  
        APPLY "value-changed" TO BROWSE {&browse-name}.
        DISABLE iOrderType. 
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
    DISPLAY iOrderType cOrderSource cCustPo lblLabel lblLabel-2 
        WITH FRAME D-Dialog.
    ENABLE iOrderType cOrderSource cCustPo Btn_OK Btn_Cancel BROWSE-1 
        WITH FRAME D-Dialog.
    VIEW FRAME D-Dialog.
    {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildTempTable D-Dialog 
PROCEDURE pBuildTempTable :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:
    
        FOR EACH orderType NO-LOCK
            WHERE NOT orderType.inactive:
          
            CREATE tt-order-type.
            ASSIGN
                tt-order-type.company     = cocode
                tt-order-type.ord-type    = orderType.orderTypeID
                tt-order-type.ord-source  = orderType.orderTypeSource
                tt-order-type.type-desc   = orderType.orderTypeDescription
                tt-order-type.system-type = "Normal"
                tt-order-type.cColor      = STRING(orderType.orderTypeColor)
                tt-order-type.lActive     = YES.
           
        END.                       
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValueChange D-Dialog 
PROCEDURE pValueChange :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
   
    DO WITH FRAME {&FRAME-NAME}:
        lblLabel-2:SCREEN-VALUE = "". 
        IF AVAILABLE tt-order-type THEN
            ASSIGN
                iOrderType:SCREEN-VALUE = STRING(tt-order-type.ord-type)
                lblLabel:SCREEN-VALUE   = tt-order-type.ord-source + ":".     
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
         
        OPEN QUERY BROWSE-1 FOR EACH tt-order-type
            NO-LOCK BY tt-order-type.ord-type.              

        REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckPoValidation D-Dialog 
PROCEDURE pCheckPoValidation :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:
        lblLabel-2:SCREEN-VALUE = "".
        IF tt-order-type.ord-source EQ "Estimate" THEN
        DO:
            FIND FIRST eb NO-LOCK
                WHERE eb.company EQ g_company
                AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(cOrderSource:SCREEN-VALUE))) + TRIM(cOrderSource:SCREEN-VALUE)
                NO-ERROR.
            IF AVAILABLE eb THEN
            DO:
                FIND FIRST cust NO-LOCK
                    WHERE cust.company EQ eb.company
                    AND cust.cust-no EQ eb.cust-no
                    AND cust.po-mandatory 
                    NO-ERROR.
                IF AVAILABLE cust THEN
                    ASSIGN  lblLabel-2:SCREEN-VALUE = "PO is mandatory".
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME  

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no D-Dialog 
PROCEDURE valid-cust-no :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST cust
            WHERE cust.company  EQ gcompany
            AND cust.cust-no   EQ cOrderSource:SCREEN-VALUE)  THEN 
        DO:
            MESSAGE "Invalid Customer, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cOrderSource .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no D-Dialog 
PROCEDURE valid-est-no :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:  
        IF cOrderSource:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST est NO-LOCK
                WHERE est.company EQ g_company
                AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(cOrderSource:SCREEN-VALUE))) + TRIM(cOrderSource:SCREEN-VALUE)
                NO-ERROR.
            IF NOT AVAILABLE est THEN 
            DO:
                MESSAGE "Invalid Estimate#, try help..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO cOrderSource.
                oplOutError = YES.
                RETURN.
            END. 
      
        END.  
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no D-Dialog 
PROCEDURE valid-po-no :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cCustomerNo AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.
    
    DO WITH FRAME {&FRAME-NAME}:     
         
        IF tt-order-type.ord-source EQ "Estimate" THEN
        DO:
            FIND FIRST eb NO-LOCK
                WHERE eb.company EQ g_company
                AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(cOrderSource:SCREEN-VALUE))) + TRIM(cOrderSource:SCREEN-VALUE)
                NO-ERROR.
            IF AVAILABLE eb THEN
                cCustomerNo = eb.cust-no.
        END.
        ELSE cCustomerNo = cOrderSource:SCREEN-VALUE.        
    
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ cCustomerNo 
            AND cust.po-mandatory
            NO-ERROR.
        
        IF AVAILABLE cust AND TRIM(cCustPO:SCREEN-VALUE) EQ "" THEN 
        DO:
            MESSAGE "PO# is mandatory for Customer " + cust.cust-no
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cCustPO.
            oplOutError = YES . 
        END.
        
        IF NOT ll-valid-po-no AND lOeprompt AND cCustPO:SCREEN-VALUE NE "" THEN
            FIND FIRST b-oe-ordl
                WHERE b-oe-ordl.company EQ cocode
                AND b-oe-ordl.po-no   EQ cCustPO:SCREEN-VALUE
                AND b-oe-ordl.cust-no EQ cOrderSource:SCREEN-VALUE            
                NO-LOCK NO-ERROR.

        IF AVAILABLE b-oe-ordl THEN 
        DO:
            MESSAGE "Customer PO already exists for Order/Item - " + 
                TRIM(STRING(b-oe-ordl.ord-no,">>>>>>>>")) + "/" +
                TRIM(b-oe-ordl.i-no) " ." SKIP
                "Do you want to continue?"
                VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF NOT ll-ans THEN 
            DO:
                APPLY "entry" TO cCustPO.
                oplOutError = YES.
            END.
            ELSE ll-valid-po-no = YES.
        END.          
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



