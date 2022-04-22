&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/CloseOrder.p
    Purpose     : Handle all business logic for closing an order line  

    Syntax      :  Run oe/CloseOrder.p (INPUT ROWID(oe-ordl),
                                        INPUT No/Yes - update close status, 
                                        OUTPUT Logical - Status,
                                        OUTPUT Character Reason)

    Description : 

    Author(s)   : BV
    Created     : 1/12/15
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipriOrder AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER iplUpdate AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcStatus AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcReason AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdOrderProcs AS HANDLE NO-UNDO.

RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

IF NOT THIS-PROCEDURE:PERSISTENT THEN
    RUN CloseOrder(INPUT ipriOrder,
        INPUT iplUpdate,
        OUTPUT opcStatus,
        OUTPUT opcReason).
DELETE PROCEDURE hdOrderProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CloseOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseOrder Procedure 
PROCEDURE CloseOrder :
    /*------------------------------------------------------------------------------
      Purpose:  Checks to see if an order can be closed.  Closes if update flag is set.
      Parameters:  oe-ord row-id, update logical, 
                output status, output reason.
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriOrder AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iplUpdate AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER opcStatus AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReason AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-oe-ord FOR oe-ord.

    DEFINE VARIABLE cOEClose AS CHARACTER NO-UNDO.

    FIND FIRST bf-oe-ord 
        WHERE ROWID(bf-oe-ord) EQ ipriOrder
        NO-LOCK NO-ERROR.
    IF AVAILABLE bf-oe-ord THEN 
    DO:
        RUN OrderCloseCheck(BUFFER bf-oe-ord,
            OUTPUT opcStatus,
            OUTPUT opcReason).
    /*     IF iplUpdate AND opcStatus NE bf-oe-ord.stat THEN */
    /*         RUN OrderCloseSet(BUFFER bf-oe-ord,           */
    /*                           INPUT-OUTPUT opcStatus      */
    /*                           INPUT-OUTPUT opcReason).    */
    END.
    ELSE /*line row-id passed through*/
        RUN CloseOrderLine(INPUT ipriOrder,
            INPUT iplUpdate,
            OUTPUT opcStatus,
            OUTPUT opcReason).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseOrderLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseOrderLine Procedure 
PROCEDURE CloseOrderLine :
    /*------------------------------------------------------------------------------
      Purpose:  Checks to see if an order line can be closed.  Closes if update flag is set.
      Parameters:  oe-ordl row-id, update logical, 
                output status, output reason.
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriOrderLine AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iplUpdate AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER opcStatus AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReason AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE VARIABLE cOEClose AS CHARACTER NO-UNDO.

    FIND FIRST bf-oe-ordl 
        WHERE ROWID(bf-oe-ordl) EQ ipriOrderLine
        NO-LOCK NO-ERROR.
    IF AVAILABLE bf-oe-ordl THEN 
    DO:
        RUN Order_OrderLineCloseCheck IN hdOrderProcs (BUFFER bf-oe-ordl,
            OUTPUT opcStatus,
            OUTPUT opcReason).
        IF iplUpdate AND opcStatus NE bf-oe-ordl.stat THEN
            RUN OrderLineCloseSet(INPUT ROWID(bf-oe-ordl),
                INPUT YES).

    END.
    ELSE 
        ASSIGN
            opcStatus = ''
            opcReason = 'Order record not found'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOEClose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOEClose Procedure 
PROCEDURE GetOEClose PRIVATE :
    /*------------------------------------------------------------------------------
      Purpose:  Get the value of OEClose N-K-1   
      Parameters:  outputs the char value
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCharValue AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (INPUT ipcCompany, 
        INPUT 'OECLOSE', 
        INPUT 'C', 
        INPUT NO, 
        INPUT NO, 
        INPUT '', 
        INPUT '', 
        OUTPUT cReturn, 
        OUTPUT lFound).

    IF lFound THEN
        opcCharValue = cReturn.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OrderCloseCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrderCloseCheck Procedure 
PROCEDURE OrderCloseCheck :
    /*------------------------------------------------------------------------------
      Purpose: Checks to see if an order can be closed    
      Parameters:  order buffer - > Outputs order status and reason
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER opcStatus AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReason AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    ASSIGN
        opcStatus = 'C'
        opcReason = 'Closed. All order lines closed for Order #' + STRING(ipbf-oe-ord.ord-no,'>>>>>9') + '.'
        .

    FOR EACH bf-oe-ordl 
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
        AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no 
        NO-LOCK:
        IF bf-oe-ordl.stat NE 'C' THEN 
        DO:
            ASSIGN 
                opcStatus = ipbf-oe-ord.stat
                opcReason = 'Not closed. Order line #' + STRING(bf-oe-ordl.LINE,'>>9') + ' of Order #' + STRING(ipbf-oe-ord.ord-no,'>>>>>9') + ' is still open.'
                .
            LEAVE.
        END.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OrderCloseSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrderCloseSet Procedure 
PROCEDURE OrderCloseSet :
/* /*------------------------------------------------------------------------------                                         */
/*   Purpose: Makes the update to the order tweak related records                                                           */
/*   Parameters:  rowid of order line and open/close logical (supports a re-open)                                           */
/*   Notes: replaces oe/closelin.p                                                                                          */
/* ------------------------------------------------------------------------------*/                                         */
/* DEFINE INPUT  PARAMETER ipriOrder AS ROWID       NO-UNDO.                                                                */
/* DEFINE INPUT  PARAMETER iplClose AS LOGICAL     NO-UNDO.                                                                 */
/*                                                                                                                          */
/* DEFINE VARIABLE iNegativeIfClosing AS INTEGER     NO-UNDO.                                                               */
/* DEFINE VARIABLE dUnshippedOrderLineQty AS DECIMAL     NO-UNDO.                                                           */
/* DEFINE VARIABLE dTaxRate AS DECIMAL     NO-UNDO.                                                                         */
/* DEFINE VARIABLE dTaxRateFreight AS DECIMAL     NO-UNDO.                                                                  */
/* DEFINE VARIABLE dUnshippedOrderLineQtyValue AS DECIMAL INIT 0    NO-UNDO.                                                */
/* DEFINE VARIABLE iUomMult AS INTEGER     NO-UNDO.                                                                         */
/*                                                                                                                          */
/* DISABLE TRIGGERS FOR LOAD OF itemfg.                                                                                     */
/*                                                                                                                          */
/* iNegativeIfClosing = IF iplClose THEN -1 ELSE 1.                                                                         */
/*                                                                                                                          */
/* FIND oe-ordl                                                                                                             */
/*     WHERE ROWID(oe-ordl) EQ ipriOrderLine                                                                                */
/*     EXCLUSIVE-LOCK NO-ERROR.                                                                                             */
/* IF AVAIL oe-ordl THEN                                                                                                    */
/*     FIND FIRST oe-ord                                                                                                    */
/*         WHERE oe-ord.company EQ oe-ordl.company                                                                          */
/*           AND oe-ord.ord-no  EQ oe-ordl.ord-no                                                                           */
/*         NO-LOCK NO-ERROR.                                                                                                */
/* IF NOT AVAIL oe-ord THEN RETURN.                                                                                         */
/*                                                                                                                          */
/* dUnshippedOrderLineQty = oe-ordl.qty - oe-ordl.ship-qty.                                                                 */
/* IF dUnshippedOrderLineQty LT 0 THEN dUnshippedOrderLineQty = 0.                                                          */
/*                                                                                                                          */
/* IF dUnshippedOrderLineQty NE 0 THEN DO:  /*UPDATE BALANCES*/                                                             */
/*     /*FG ITEM UPDATES*/                                                                                                  */
/*     FIND FIRST itemfg                                                                                                    */
/*         WHERE itemfg.company eq oe-ordl.company                                                                          */
/*           AND itemfg.i-no    eq oe-ordl.i-no                                                                             */
/*         NO-ERROR.                                                                                                        */
/*     IF AVAIL itemfg THEN DO:                                                                                             */
/*         IF oe-ord.type NE 'T' THEN                                                                                       */
/*             ASSIGN                                                                                                       */
/*                 itemfg.q-alloc = itemfg.q-alloc + (dUnshippedOrderLineQty * iNegativeIfClosing)                          */
/*                 itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc                                            */
/*                 .                                                                                                        */
/*         ASSIGN                                                                                                           */
/*             itemfg.q-ptd     = itemfg.q-ptd     + (dUnshippedOrderLineQty * iNegativeIfClosing)                          */
/*             itemfg.q-ord-ytd = itemfg.q-ord-ytd + (dUnshippedOrderLineQty * iNegativeIfClosing)                          */
/*             .                                                                                                            */
/*         IF itemfg.q-alloc LT 0 THEN itemfg.q-alloc = 0.                                                                  */
/*         IF itemfg.q-avail LT 0 THEN itemfg.q-avail = 0.                                                                  */
/*                                                                                                                          */
/*         FIND FIRST itemfg-loc                                                                                            */
/*             WHERE itemfg-loc.company EQ oe-ordl.company                                                                  */
/*               AND itemfg-loc.i-no    EQ oe-ordl.i-no                                                                     */
/*               AND itemfg-loc.loc     EQ oe-ord.loc                                                                       */
/*             EXCLUSIVE-LOCK NO-ERROR.                                                                                     */
/*         IF AVAIL itemfg-loc THEN DO:                                                                                     */
/*             IF oe-ord.type NE 'T' THEN                                                                                   */
/*                 ASSIGN                                                                                                   */
/*                     itemfg-loc.q-alloc = itemfg-loc.q-alloc + (dUnshippedOrderLineQty * iNegativeIfClosing)              */
/*                     itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc                        */
/*                     .                                                                                                    */
/*             ASSIGN                                                                                                       */
/*                 itemfg-loc.q-ptd     = itemfg-loc.q-ptd     + (dUnshippedOrderLineQty * iNegativeIfClosing)              */
/*                 itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd + (dUnshippedOrderLineQty * iNegativeIfClosing)              */
/*                 .                                                                                                        */
/*             IF itemfg-loc.q-alloc LT 0 THEN                                                                              */
/*                 itemfg-loc.q-alloc = 0.                                                                                  */
/*             IF itemfg-loc.q-avail LT 0 THEN                                                                              */
/*                 itemfg-loc.q-avail = 0.                                                                                  */
/*         END. /*avail itemfg-loc*/                                                                                        */
/*     END. /*avail itemfg*/                                                                                                */
/*                                                                                                                          */
/*     /*CUSTOMER UPDATES*/                                                                                                 */
/*     FIND FIRST cust                                                                                                      */
/*         WHERE cust.company EQ oe-ord.company                                                                             */
/*           AND cust.cust-no EQ oe-ord.cust-no                                                                             */
/*         EXCLUSIVE-LOCK NO-ERROR.                                                                                         */
/*     IF AVAIL cust THEN DO:                                                                                               */
/*     ASSIGN                                                                                                               */
/*         iUomMult =                                                                                                       */
/*             IF oe-ordl.pr-uom EQ "M"  THEN 1000                                                                          */
/*             ELSE IF oe-ordl.pr-uom EQ "C"  THEN 100                                                                      */
/*             ELSE IF AVAIL itemfg AND oe-ordl.pr-uom  EQ "CS" THEN itemfg.case-count                                      */
/*             ELSE 1                                                                                                       */
/*         dUnshippedOrderLineQtyValue  = (dUnshippedOrderLineQty / iUomMult) * oe-ordl.price                               */
/*         dUnshippedOrderLineQtyValue  = dUnshippedOrderLineQtyValue - (dUnshippedOrderLineQtyValue * oe-ordl.disc / 100)  */
/*         .                                                                                                                */
/*     IF oe-ordl.tax THEN DO:                                                                                              */
/*         RUN ar/cctaxrt.p (INPUT oe-ord.company, oe-ord.tax-gr,                                                           */
/*                     OUTPUT dTaxRate, OUTPUT dTaxRateFreight).                                                            */
/*         dUnshippedOrderLineQtyValue = dUnshippedOrderLineQtyValue + (dUnshippedOrderLineQtyValue * dTaxRate / 100).      */
/*     END.                                                                                                                 */
/*             cust.ord-bal = cust.ord-bal + (dUnshippedOrderLineQtyValue * iNegativeIfClosing).                            */
/*     END. /*avail cust*/                                                                                                  */
/*                                                                                                                          */
/* END. /* Unshipped Qty ne 0 */                                                                                            */
/*                                                                                                                          */
/* /*FINAL STATUS UPDATE*/                                                                                                  */
/* IF iplClose THEN                                                                                                         */
/*     oe-ordl.stat = 'C'.                                                                                                  */
/* ELSE                                                                                                                     */
/*     oe-ordl.stat = ''.                                                                                                   */
/*                                                                                                                          */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-OrderLineCloseSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrderLineCloseSet Procedure 
PROCEDURE OrderLineCloseSet :
    /*------------------------------------------------------------------------------
      Purpose: Makes the update to the order line and tweaks related order balances   
      Parameters:  rowid of order line and open/close logical (supports a re-open)
      Notes: replaces oe/closelin.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriOrderLine AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iplClose AS LOGICAL     NO-UNDO.

    DEFINE VARIABLE iNegativeIfClosing          AS INTEGER NO-UNDO.
    DEFINE VARIABLE dUnshippedOrderLineQty      AS DECIMAL NO-UNDO.    
    DEFINE VARIABLE dUnshippedOrderLineQtyValue AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE iUomMult                    AS INTEGER NO-UNDO.
    DEFINE VARIABLE dTaxAmount                  AS DECIMAL NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF itemfg.

    iNegativeIfClosing = IF iplClose THEN -1 ELSE 1.

    FIND oe-ordl 
        WHERE ROWID(oe-ordl) EQ ipriOrderLine 
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE oe-ordl THEN
        FIND FIRST oe-ord
            WHERE oe-ord.company EQ oe-ordl.company
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-LOCK NO-ERROR.
    IF NOT AVAILABLE oe-ord THEN RETURN.

    dUnshippedOrderLineQty = oe-ordl.qty - oe-ordl.ship-qty.
    IF dUnshippedOrderLineQty LT 0 THEN dUnshippedOrderLineQty = 0.

    IF dUnshippedOrderLineQty NE 0 THEN 
    DO:  /*UPDATE BALANCES*/
        /*FG ITEM UPDATES*/
        FIND FIRST itemfg
            WHERE itemfg.company EQ oe-ordl.company
            AND itemfg.i-no    EQ oe-ordl.i-no
            NO-ERROR.
        IF AVAILABLE itemfg THEN 
        DO:
            IF oe-ord.type NE 'T' THEN 
                ASSIGN 
                    itemfg.q-alloc = itemfg.q-alloc + (dUnshippedOrderLineQty * iNegativeIfClosing)
                    itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
                    .
            ASSIGN 
                itemfg.q-ptd     = itemfg.q-ptd     + (dUnshippedOrderLineQty * iNegativeIfClosing)
                itemfg.q-ord-ytd = itemfg.q-ord-ytd + (dUnshippedOrderLineQty * iNegativeIfClosing)
                .
            IF itemfg.q-alloc LT 0 THEN itemfg.q-alloc = 0.
            IF itemfg.q-avail LT 0 THEN itemfg.q-avail = 0.       

            FIND FIRST itemfg-loc
                WHERE itemfg-loc.company EQ oe-ordl.company
                AND itemfg-loc.i-no    EQ oe-ordl.i-no
                AND itemfg-loc.loc     EQ oe-ord.loc
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE itemfg-loc THEN 
            DO:
                IF oe-ord.type NE 'T' THEN
                    ASSIGN 
                        itemfg-loc.q-alloc = itemfg-loc.q-alloc + (dUnshippedOrderLineQty * iNegativeIfClosing)
                        itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
                        .
                ASSIGN
                    itemfg-loc.q-ptd     = itemfg-loc.q-ptd     + (dUnshippedOrderLineQty * iNegativeIfClosing)
                    itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd + (dUnshippedOrderLineQty * iNegativeIfClosing)
                    .
                IF itemfg-loc.q-alloc LT 0 THEN
                    itemfg-loc.q-alloc = 0.
                IF itemfg-loc.q-avail LT 0 THEN 
                    itemfg-loc.q-avail = 0.     
            END. /*avail itemfg-loc*/
        END. /*avail itemfg*/

        /*CUSTOMER UPDATES*/ 
        FIND FIRST cust
            WHERE cust.company EQ oe-ord.company
            AND cust.cust-no EQ oe-ord.cust-no
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE cust THEN 
        DO:
            ASSIGN
                iUomMult                    = IF oe-ordl.pr-uom EQ "M"  THEN 1000 
            ELSE IF oe-ordl.pr-uom EQ "C"  THEN 100  
            ELSE IF AVAILABLE itemfg AND oe-ordl.pr-uom  EQ "CS" THEN itemfg.case-count 
            ELSE 1
                dUnshippedOrderLineQtyValue = (dUnshippedOrderLineQty / iUomMult) * oe-ordl.price
                dUnshippedOrderLineQtyValue = dUnshippedOrderLineQtyValue - (dUnshippedOrderLineQtyValue * oe-ordl.disc / 100)
                .
            IF oe-ordl.tax THEN 
            DO:                 
                dTaxAmount = 0.
                RUN Tax_Calculate(INPUT oe-ord.company, 
                                  INPUT oe-ord.tax-gr,
                                  INPUT FALSE,
                                  INPUT dUnshippedOrderLineQtyValue,
                                  INPUT oe-ordl.i-no, 
                                  OUTPUT dTaxAmount).    
                dUnshippedOrderLineQtyValue = dUnshippedOrderLineQtyValue + (dTaxAmount).
            END.
            cust.ord-bal = cust.ord-bal + (dUnshippedOrderLineQtyValue * iNegativeIfClosing).
        END. /*avail cust*/

    END. /* Unshipped Qty ne 0 */

    /*FINAL STATUS UPDATE*/
    IF iplClose THEN 
        oe-ordl.stat = 'C'.
    ELSE 
        oe-ordl.stat = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

