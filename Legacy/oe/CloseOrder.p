&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/CloseOrderLine.p
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
DEFINE INPUT PARAMETER ipriOrder AS ROWID NO-UNDO.
DEFINE INPUT  PARAMETER iplUpdate AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER opcStatus AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcReason AS CHARACTER   NO-UNDO.

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

DEFINE VARIABLE cOEClose AS CHARACTER   NO-UNDO.

FIND FIRST bf-oe-ord 
    WHERE ROWID(bf-oe-ord) EQ ipriOrder
    NO-LOCK NO-ERROR.
IF AVAIL bf-oe-ord THEN DO:
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
DEFINE VARIABLE cOEClose AS CHARACTER   NO-UNDO.

FIND FIRST bf-oe-ordl 
    WHERE ROWID(bf-oe-ordl) EQ ipriOrderLine
    NO-LOCK NO-ERROR.
IF AVAIL bf-oe-ordl THEN DO:
    RUN OrderLineCloseCheck(BUFFER bf-oe-ordl,
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

DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.

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
    IF bf-oe-ordl.stat NE 'C' THEN DO:
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

&IF DEFINED(EXCLUDE-OrderLineCloseCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrderLineCloseCheck Procedure 
PROCEDURE OrderLineCloseCheck :
/*------------------------------------------------------------------------------
  Purpose: Checks to see if an order line can be closed    
  Parameters:  order line buffer - > Outputs order status and reason
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-oe-ordl FOR oe-ordl.
DEFINE OUTPUT PARAMETER opcStatus AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcReason AS CHARACTER   NO-UNDO.

DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

DEFINE VARIABLE lAllTransfers AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dAllowableUnderrun LIKE oe-ordl.qty     NO-UNDO.
DEFINE VARIABLE lInvQtyMet AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lShipQtyMet AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lUnassembledSetComponent AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lUnassembledSetHeader AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lNotStocked AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cQtyMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOEClose AS CHARACTER   NO-UNDO.

FIND FIRST oe-ord NO-LOCK OF ipbf-oe-ordl NO-ERROR.
IF NOT AVAIL oe-ord THEN DO:
    ASSIGN
        opcStatus = ipbf-oe-ordl.stat
        opcReason = 'Order Header does not exist.'
        .
    RETURN.
END.
IF ipbf-oe-ordl.stat EQ "C" THEN DO:
    ASSIGN
        opcStatus = ipbf-oe-ordl.stat
        opcReason = 'Order line #' + STRING(ipbf-oe-ordl.LINE,'>>9') + 'of Order #' + STRING(ipbf-oe-ordl.ord-no,'>>>>>9') + ' is already closed.'
        .
    RETURN.
END.

/*Transfer types*/
lAllTransfers = YES.
FOR EACH oe-boll
    WHERE oe-boll.company  EQ ipbf-oe-ordl.company
      AND oe-boll.ord-no   EQ ipbf-oe-ordl.ord-no
      AND oe-boll.line     EQ ipbf-oe-ordl.line
      AND oe-boll.i-no     EQ ipbf-oe-ordl.i-no
    USE-INDEX ord-no
    NO-LOCK:

    IF oe-boll.s-code NE 'T' THEN DO:
        lAllTransfers = NO.
    END.
END.
IF oe-ord.type EQ "T" 
    OR lAllTransfers THEN DO:

    FOR EACH oe-boll
        WHERE oe-boll.company  EQ ipbf-oe-ordl.company
          AND oe-boll.ord-no   EQ ipbf-oe-ordl.ord-no
          AND oe-boll.line     EQ ipbf-oe-ordl.line
          AND oe-boll.i-no     EQ ipbf-oe-ordl.i-no
          AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no
                                       AND oe-bolh.posted EQ YES)
        USE-INDEX ord-no
        NO-LOCK:
      ACCUMULATE oe-boll.qty (TOTAL).
    END.
    IF (ipbf-oe-ordl.qty GT 0 
        AND (ACCUM TOTAL oe-boll.qty) GE ipbf-oe-ordl.qty) 
        OR
       (ipbf-oe-ordl.qty LT 0 
        AND (ACCUM TOTAL oe-boll.qty) LE ipbf-oe-ordl.qty) 
    THEN
        ASSIGN 
            opcStatus = 'C'
            opcReason = 'Closed. Transfer order requirements met. BOL Qty Shipped: ' + STRING(ACCUM TOTAL oe-boll.qty) + ' OrderLine Qty: ' + STRING(ipbf-oe-ordl.qty) 
            .
    ELSE
        ASSIGN
            opcStatus = ipbf-oe-ordl.stat
            opcReason = 'Not closed. Transfer order requirements not met. BOL Qty Shipped: ' + STRING(ACCUM TOTAL oe-boll.qty) + ' OrderLine Qty: ' + STRING(ipbf-oe-ordl.qty) 
            .
END. /* Transfer order */
ELSE DO: /*all other cases*/
    FIND FIRST itemfg
        WHERE itemfg.company EQ ipbf-oe-ordl.company
          AND itemfg.i-no    EQ ipbf-oe-ordl.i-no
        NO-LOCK NO-ERROR.
    RUN GetOEClose(INPUT ipbf-oe-ordl.company,
                   OUTPUT cOEClose).
    ASSIGN
        dAllowableUnderrun = ipbf-oe-ordl.qty * (1 - (ipbf-oe-ordl.under-pct / 100))
        lUnassembledSetComponent = ipbf-oe-ordl.is-a-component
        lInvQtyMet = ipbf-oe-ordl.inv-qty  GE dAllowableUnderrun
        lShipQtyMet = ipbf-oe-ordl.ship-qty GE dAllowableUnderrun
        lUnassembledSetHeader = CAN-FIND(FIRST bf-oe-ordl {sys/inc/ordlcomp.i bf-oe-ordl ipbf-oe-ordl})
        lNotStocked = AVAIL itemfg AND NOT itemfg.stocked
        cQtyMessage = ' Allowable Underrun Qty: ' + STRING(dAllowableUnderrun) + 
            ' Inv Qty: ' + STRING(ipbf-oe-ordl.inv-qty) + 
            ' Ship Qty: ' + STRING(ipbf-oe-ordl.ship-qty).
        .

    IF (lInvQtyMet OR lUnassembledSetComponent) /*if component, don't check inv qty*/
        AND 
       (lShipQtyMet OR lUnassembledSetHeader /*OR lNotStocked*/ ) /*if UnAssembled Set Header, don't check ship qty*/
        THEN DO:
        
        /*additional check for job bin qty = 0*/
        IF cOEClose EQ "OnHand=0" 
            AND ipbf-oe-ordl.job-no NE ""
            AND AVAIL itemfg 
            AND itemfg.pur-man EQ NO
            THEN DO:

            FIND FIRST fg-bin
                WHERE fg-bin.company EQ ipbf-oe-ordl.company
                  AND fg-bin.i-no EQ ipbf-oe-ordl.i-no
                  AND fg-bin.job-no EQ ipbf-oe-ordl.job-no
                  AND fg-bin.job-no2 EQ ipbf-oe-ordl.job-no2
                  AND fg-bin.qty GT 0
                NO-LOCK NO-ERROR.
            IF NOT AVAIL fg-bin THEN
                ASSIGN
                    opcStatus = 'C'
                    opcReason = 'Closed. Requirements for allowable underrun met and job inventory is 0.' + cQtyMessage
                    .
            ELSE
                ASSIGN 
                    opcStatus = ipbf-oe-ordl.stat
                    opcReason = 'Not Closed. Requirements for allowable underrun met but job inventory still exists.' + cQtyMessage
                    .
        END.
        ELSE do:
            FIND FIRST inv-line NO-LOCK 
                 WHERE inv-line.company EQ ipbf-oe-ordl.company
                   AND inv-line.i-no EQ ipbf-oe-ordl.i-no
                   AND inv-line.ord-no EQ ipbf-oe-ordl.ord-no
                   AND inv-line.job-no EQ ipbf-oe-ordl.job-no
                   AND inv-line.job-no2 EQ ipbf-oe-ordl.job-no2
                   NO-ERROR.
            IF AVAIL inv-line  THEN
                ASSIGN 
                    opcStatus = ipbf-oe-ordl.stat
                    opcReason = 'Not Closed. Requirements for allowable underrun met but job inventory still exists.' + cQtyMessage  .
            
            ELSE ASSIGN 
                opcStatus = 'C'
                opcReason = 'Closed. Requirements for allowable underrun met.' + cQtyMessage  .
        END.  /* else do  */
               
    END.
    ELSE
        ASSIGN 
            opcStatus = ipbf-oe-ordl.stat
            opcReason = 'Not closed. Requirements for allowable underrun not met.' + cQtyMessage
            .
END.

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

DEFINE VARIABLE iNegativeIfClosing AS INTEGER     NO-UNDO.
DEFINE VARIABLE dUnshippedOrderLineQty AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTaxRate AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTaxRateFreight AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dUnshippedOrderLineQtyValue AS DECIMAL INIT 0    NO-UNDO.
DEFINE VARIABLE iUomMult AS INTEGER     NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF itemfg.

iNegativeIfClosing = IF iplClose THEN -1 ELSE 1.

FIND oe-ordl 
    WHERE ROWID(oe-ordl) EQ ipriOrderLine 
    EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL oe-ordl THEN
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ oe-ordl.company
          AND oe-ord.ord-no  EQ oe-ordl.ord-no
        NO-LOCK NO-ERROR.
IF NOT AVAIL oe-ord THEN RETURN.

dUnshippedOrderLineQty = oe-ordl.qty - oe-ordl.ship-qty.
IF dUnshippedOrderLineQty LT 0 THEN dUnshippedOrderLineQty = 0.

IF dUnshippedOrderLineQty NE 0 THEN DO:  /*UPDATE BALANCES*/
    /*FG ITEM UPDATES*/
    FIND FIRST itemfg
        WHERE itemfg.company eq oe-ordl.company
          AND itemfg.i-no    eq oe-ordl.i-no
        NO-ERROR.
    IF AVAIL itemfg THEN DO:
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
        IF AVAIL itemfg-loc THEN DO:
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
    IF AVAIL cust THEN DO:
    ASSIGN
        iUomMult = 
            IF oe-ordl.pr-uom EQ "M"  THEN 1000 
            ELSE IF oe-ordl.pr-uom EQ "C"  THEN 100  
            ELSE IF AVAIL itemfg AND oe-ordl.pr-uom  EQ "CS" THEN itemfg.case-count 
            ELSE 1
        dUnshippedOrderLineQtyValue  = (dUnshippedOrderLineQty / iUomMult) * oe-ordl.price
        dUnshippedOrderLineQtyValue  = dUnshippedOrderLineQtyValue - (dUnshippedOrderLineQtyValue * oe-ordl.disc / 100)
        .
    IF oe-ordl.tax THEN DO:
        RUN ar/cctaxrt.p (INPUT oe-ord.company, oe-ord.tax-gr,
                    OUTPUT dTaxRate, OUTPUT dTaxRateFreight).
        dUnshippedOrderLineQtyValue = dUnshippedOrderLineQtyValue + (dUnshippedOrderLineQtyValue * dTaxRate / 100).
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

