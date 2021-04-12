/*-----------------------------------------------------------------------------
    File        : oe/spValidate.p
    Copyright   : (c)1985-2019 Advantzware, Inc. All rights reserved.
    Description : Performs all validations for OE module auto-hold processing
    Author(s)   : MYT
    Created     : Apr 23, 2019 3:18:57 PM
    Notes       : Run as a persistent procedure to access these routines
                 
---------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/globdefs.i}
{sys/inc/var.i SHARED}

DEFINE TEMP-TABLE ttValidation
    FIELD cProgram     AS CHARACTER
    FIELD cHoldOrInfo  AS CHARACTER
    FIELD lHoldResult  AS LOGICAL 
    FIELD cHoldMessage AS CHARACTER 
    FIELD cNotes       AS CHARACTER
    .
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */



/* ***************************  Main Block  *************************** */
    
/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddHold PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Adds a hold via the TagProcs.
 Notes:
------------------------------------------------------------------------------*/

END PROCEDURE.

PROCEDURE pBuildValidationsToRun PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Based on NK1 Settings, creates the ttTempTable to direct validation
     execution.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iCtr      AS INTEGER   NO-UNDO.
    /* Create/setup NK1s if not already there */
    /* I know there is a "standard" for this, but we're pulling two values from eight records,
       This is much more compact. */
    DEFINE VARIABLE cTestList AS CHARACTER INITIAL "CreditHold,CustomerPN,CustomerPO,PriceGtCost,PriceHold,UniquePO,ValidShipTo,ValidUom,OnHandInventory,ItemHold,DuplicateItem,EstimateExists,FullPalletOnly" NO-UNDO.    
    DEFINE VARIABLE cReqdList AS CHARACTER INITIAL "TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE" NO-UNDO.    
    DEFINE VARIABLE cTypeList AS CHARACTER INITIAL "HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,INFO,HOLD" NO-UNDO.
    DEFINE VARIABLE cDescList AS CHARACTER INITIAL "Credit check fails,Customer Part # invalid,PO number is blank,Extended Sell < Extended Cost,Order is on Price Hold,Customer PO number is not unique,Shipto is invalid,Price UOM is invalid,Sufficient Inventory OH,Items review hold,Duplicate line item Hold,Estimate Exists,Customer Orders must be full pallet quantity" NO-UNDO.    
    
    EMPTY TEMP-TABLE ttValidation.
    DO iCtr = 1 TO NUM-ENTRIES(cTestList):
        FIND FIRST sys-ctrl NO-LOCK
            WHERE sys-ctrl.company EQ ipcCompany 
            AND sys-ctrl.name EQ ENTRY(iCtr,cTestList)
            NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN 
        DO:
            CREATE sys-ctrl.
            ASSIGN 
                sys-ctrl.company  = ipcCompany
                sys-ctrl.module   = "VAL"
                sys-ctrl.name     = ENTRY(iCtr,cTestList)
                sys-ctrl.log-fld  = LOGICAL(ENTRY(iCtr,cReqdList))
                sys-ctrl.descrip  = ENTRY(iCtr,cDescList)
                sys-ctrl.char-fld = ENTRY(iCtr,cTypeList).
            FIND CURRENT sys-ctrl NO-LOCK.
        END.
        IF sys-ctrl.log-fld THEN 
        DO:
            CREATE ttValidation.
            ASSIGN 
                ttValidation.cProgram    = "p" + sys-ctrl.name
                ttValidation.cHoldOrInfo = sys-ctrl.char-fld
                .
        END.
    END.

END PROCEDURE.

PROCEDURE pCreditHold PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Runs "standard" credit check procedure and formats for validation output
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bcust FOR cust.
    
    FIND FIRST bcust NO-LOCK WHERE 
        bcust.company EQ ipboe-ord.company AND 
        bcust.cust-no EQ ipboe-ord.cust-no  
        NO-ERROR.
    IF NOT AVAILABLE bcust THEN 
    DO:
        ASSIGN 
            oplHold    = TRUE 
            opcMessage = "Unable to locate a customer for this order.".
        RETURN.
    END. 
    ELSE 
    DO:
        RUN oe/creditck.p (ROWID(bcust), NO).
        FIND CURRENT bcust NO-LOCK NO-ERROR.
        IF AVAILABLE bcust AND bcust.cr-hold THEN ASSIGN 
                oplHold    = TRUE 
                opcMessage = "Customer failed credit check.".  
    END.
    
END PROCEDURE.


PROCEDURE pCustomerPN PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Tests for a valid customer part number on each line
     Notes:     "Valid" means itemfg assigned to customer for this part-no and not Inactive 
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER boe-ordl FOR oe-ordl.
    DEFINE BUFFER bitemfg  FOR itemfg.
    
    DEFINE VARIABLE cBadLines AS CHARACTER NO-UNDO.

    FOR EACH boe-ordl NO-LOCK WHERE 
        boe-ordl.company EQ ipboe-ord.company AND 
        boe-ordl.ord-no EQ ipboe-ord.ord-no AND 
        boe-ordl.line NE 0:
        FIND FIRST bitemfg NO-LOCK WHERE
            bitemfg.company EQ boe-ordl.company AND 
            bitemfg.cust-no EQ boe-ordl.cust-no AND 
            bitemfg.i-no EQ boe-ordl.i-no AND 
            bitemfg.part-no EQ boe-ordl.part-no AND 
            bitemfg.stat NE "I"
            NO-ERROR.
        IF NOT AVAILABLE bitemfg THEN 
        DO: 
            FIND FIRST cust-part NO-LOCK 
                WHERE cust-part.company EQ boe-ordl.company
                AND cust-part.i-no EQ boe-ordl.i-no
                AND cust-part.cust-no EQ boe-ordl.cust-no
                AND cust-part.part-no EQ boe-ordl.part-no
                NO-ERROR.
            IF NOT AVAILABLE cust-part THEN  
                ASSIGN 
                    cBadLines = cBadLines + STRING(boe-ordl.line) + ","
                    opcNotes  = IF opcNotes EQ "" THEN boe-ordl.i-no ELSE opcNotes + ", " + boe-ordl.i-no
                    .
        END.
    END.
    IF cBadLines NE "" THEN ASSIGN 
            cBadLines  = TRIM(cBadLines,",")
            oplHold    = TRUE 
            opcMessage = "Unable to locate valid customer part number for line" +
                     IF NUM-ENTRIES(cBadLines) GT 1 THEN ("s " + cBadLines)
                     ELSE (" " + cBadLines).

END PROCEDURE.


PROCEDURE pCustomerPO PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Verifies a customer PO has been entered on the order header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bcust FOR cust.
    
    FIND FIRST bcust NO-LOCK WHERE 
        bcust.company EQ ipboe-ord.company AND 
        bcust.cust-no EQ ipboe-ord.cust-no  
        NO-ERROR.
    IF NOT AVAILABLE bcust THEN 
    DO:
        ASSIGN 
            oplHold    = TRUE 
            opcMessage = "Unable to locate a customer for this order.".
        RETURN.
    END. 
    ELSE 
    DO:
        IF bCust.po-mandatory 
            AND ipboe-ord.po-no = "" THEN ASSIGN 
                oplHold    = TRUE 
                opcMessage = "PO Number may not be blank for this customer".
    END.

END PROCEDURE.


PROCEDURE pFullPalletOnly PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Verifies if the quantity in order line is a multiplier of full pallet quantity
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    
    DEFINE VARIABLE cFullPalletOnly   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFullPalletOnly   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRecFound         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cQtyMismatchLines AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT  ipbf-oe-ord.company, /* Company Code */ 
        INPUT  "FullPalletOnly",    /* sys-ctrl name */
        INPUT  "L",                 /* Output return value */
        INPUT  TRUE,                /* Use ship-to */
        INPUT  YES,                 /* ship-to vendor */
        INPUT  ipbf-oe-ord.cust-no, /* ship-to vendor value */
        INPUT  "",                  /* shi-id value */
        OUTPUT cFullPalletOnly, 
        OUTPUT lRecFound
        ).
    IF lRecFound THEN
        lFullPalletOnly = LOGICAL(cFullPalletOnly).
    
    IF NOT lFullPalletOnly THEN
        RETURN.
        
    FOR EACH bf-oe-ordl NO-LOCK 
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
          AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no
          AND bf-oe-ordl.line    NE 0:               
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ bf-oe-ordl.company
               AND bf-itemfg.i-no    EQ bf-oe-ordl.i-no
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN
            NEXT.
        
        IF bf-oe-ordl.qty MOD ((bf-itemfg.case-count * bf-itemfg.case-pall) + bf-itemfg.quantityPartial) EQ 0 THEN
            NEXT.

        cQtyMismatchLines = cQtyMismatchLines + "Item:" + STRING(bf-oe-ordl.i-no) + " " 
                  + "Quantity:" + STRING(bf-oe-ordl.qty) + ",".
    END.

    IF cQtyMismatchLines NE "" THEN 
        ASSIGN 
            cQtyMismatchLines = TRIM(cQtyMismatchLines,",")
            oplHold           = TRUE 
            opcMessage        = cQtyMismatchLines + " does not have full pallet quantity"
            .
END PROCEDURE.

PROCEDURE pItemHold PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Verifies if an item in the order needs review prior approval
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-sys-ctrl-shipto FOR sys-ctrl-shipto.
    
    DEFINE VARIABLE cBadLines AS CHARACTER NO-UNDO.

    FOR EACH bf-oe-ordl NO-LOCK 
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
          AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no
          AND bf-oe-ordl.line    NE 0:
        
        FIND FIRST bf-sys-ctrl-shipto NO-LOCK
             WHERE bf-sys-ctrl-shipto.company      EQ bf-oe-ordl.company
               AND bf-sys-ctrl-shipto.name         EQ "ItemHold"
               AND bf-sys-ctrl-shipto.cust-vend    EQ TRUE
               AND bf-sys-ctrl-shipto.cust-vend-no EQ ""
               AND bf-sys-ctrl-shipto.ship-id      EQ ""
               AND bf-sys-ctrl-shipto.char-fld     EQ bf-oe-ordl.i-no 
             NO-ERROR.
        IF AVAILABLE bf-sys-ctrl-shipto THEN
            cBadLines = cBadLines + "Item:" + STRING(bf-oe-ordl.i-no) + " in " 
                      + "Line:" + STRING(bf-oe-ordl.line) + ",".
    END. 
                    
    IF cBadLines NE "" THEN 
        ASSIGN 
            cBadLines  = TRIM(cBadLines,",")
            oplHold    = TRUE 
            opcMessage = cBadLines + " need review prior approval"
            .
END PROCEDURE.

PROCEDURE pDuplicateItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Verifies if a duplicate item exists in the order
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bfDuplicate-oe-ordl FOR oe-ordl.
    
    DEFINE VARIABLE cBadLines AS CHARACTER NO-UNDO.

    FOR EACH bf-oe-ordl NO-LOCK 
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
          AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no
          AND bf-oe-ordl.line    NE 0:

        FIND FIRST bfDuplicate-oe-ordl NO-LOCK 
             WHERE bfDuplicate-oe-ordl.company EQ bf-oe-ordl.company
               AND bfDuplicate-oe-ordl.ord-no  EQ bf-oe-ordl.ord-no
               AND bfDuplicate-oe-ordl.line    NE 0
               AND bfDuplicate-oe-ordl.i-no    EQ bf-oe-ordl.i-no
               AND ROWID(bfDuplicate-oe-ordl)  NE ROWID(bf-oe-ordl)
             NO-ERROR.
        IF AVAILABLE bfDuplicate-oe-ordl THEN
            cBadLines = cBadLines + "Item: " + STRING(bf-oe-ordl.i-no) + " in " 
                      + "Line: " + STRING(bf-oe-ordl.line) + ",".
    END. 
                    
    IF cBadLines NE "" THEN 
        ASSIGN 
            cBadLines  = TRIM(cBadLines,",")
            oplHold    = TRUE 
            opcMessage = "Duplicate items exists for " + cBadLines
            .
END PROCEDURE.

PROCEDURE pEstimateExists PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Verifies if an estimate exists in an order
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    DEFINE VARIABLE cBadLines AS CHARACTER NO-UNDO.

    FOR EACH bf-oe-ordl NO-LOCK 
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
          AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no
          AND bf-oe-ordl.line    NE 0:
        IF bf-oe-ordl.est-no NE "" THEN
            ASSIGN
                cBadLines = cBadLines + STRING(bf-oe-ordl.line) + ","
                opcNotes  = IF opcNotes EQ "" THEN bf-oe-ordl.i-no ELSE opcNotes + ", " + bf-oe-ordl.i-no
                .
    END.
                    
    IF cBadLines NE "" THEN 
        ASSIGN 
            cBadLines  = TRIM(cBadLines,",")
            oplHold    = TRUE 
            opcMessage = "Estimate exists for line" + 
                         IF NUM-ENTRIES(cBadLines) GT 1 THEN 
                             ("s " + cBadLines)
                         ELSE
                             (" " + cBadLines).
            .
END PROCEDURE.

PROCEDURE pOnHandInventory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*------------------------------------------------------------------------------
     Purpose:   Verifies a customer PO has been entered on the order header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes AS CHARACTER NO-UNDO.

    DEFINE BUFFER boe-ordl FOR oe-ordl.
    DEFINE VARIABLE cBadLines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOnHand AS INT NO-UNDO.

    DEFINE BUFFER bcust   FOR cust.
    DEFINE BUFFER bshipto FOR shipto.
    DEFINE VARIABLE lActive AS LOG NO-UNDO.
    
    FOR EACH boe-ordl NO-LOCK WHERE 
        boe-ordl.company EQ ipboe-ord.company AND 
        boe-ordl.ord-no EQ ipboe-ord.ord-no AND 
        boe-ordl.line NE 0:

        iOnHand = 0.

        FIND FIRST bshipto NO-LOCK WHERE 
            bshipto.company EQ ipboe-ord.company AND 
            bshipto.cust-no EQ ipboe-ord.cust-no AND 
            bshipto.ship-id EQ boe-ordl.ship-id
            NO-ERROR.
        IF NOT AVAILABLE bshipto THEN
            RUN oe/custxship.p(
                INPUT ipboe-ord.company,
                INPUT ipboe-ord.cust-no,
                INPUT ipboe-ord.ship-id,
                BUFFER bshipto
                ).        
        IF AVAILABLE bshipto THEN DO:
            FOR EACH itemfg-loc NO-LOCK WHERE 
                itemfg-loc.company EQ boe-ordl.company AND 
                itemfg-loc.i-no    EQ boe-ordl.i-no AND 
                itemfg-loc.loc     EQ bshipto.loc:
                iOnHand = iOnHand + itemfg-loc.q-onh - itemfg-loc.q-alloc.
            END.
        END.
        
        IF iOnHand LT boe-ordl.qty THEN 
            ASSIGN              
                cBadLines = cBadLines + STRING(boe-ordl.line) + ","
                opcNotes  = IF opcNotes EQ "" THEN boe-ordl.i-no ELSE opcNotes + ", " + boe-ordl.i-no
                 .
    END. 
                    
    IF cBadLines NE "" THEN ASSIGN 
            cBadLines  = TRIM(cBadLines,",")
            oplHold    = TRUE 
            opcMessage = "Insufficient Avail Inventory for line" + 
                     IF NUM-ENTRIES(cBadLines) GT 1 THEN ("s " + cBadLines)
                     ELSE (" " + cBadLines).


END PROCEDURE.

PROCEDURE pPriceGtCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Verifies that price > cost for each line on order
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER boe-ordl FOR oe-ordl.
    DEFINE VARIABLE cBadLines AS CHARACTER NO-UNDO.

    FOR EACH boe-ordl NO-LOCK WHERE 
        boe-ordl.company EQ ipboe-ord.company AND 
        boe-ordl.ord-no EQ ipboe-ord.ord-no AND 
        boe-ordl.line NE 0:
        IF boe-ordl.t-price LT boe-ordl.t-cost THEN
            ASSIGN 
                cBadLines = cBadLines + STRING(boe-ordl.line) + ","
                opcNotes  = IF opcNotes EQ "" THEN boe-ordl.i-no ELSE opcNotes + ", " + boe-ordl.i-no
                .
    END.
    IF cBadLines NE "" THEN ASSIGN 
            cBadLines  = TRIM(cBadLines,",")
            oplHold    = TRUE 
            opcMessage = "Cost is greater than price for line" + 
                     IF NUM-ENTRIES(cBadLines) GT 1 THEN ("s " + cBadLines)
                     ELSE (" " + cBadLines).


END PROCEDURE.


PROCEDURE pPriceHold PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Runs "standard" price check for order and formats results
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.
    
    RUN Price_CheckPriceHoldForOrder(ROWID(ipboe-ord), NO, NO, OUTPUT oplHold, OUTPUT opcMessage).

        
END PROCEDURE.


PROCEDURE pUniquePO PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Verifies that the PO entered for this order has not been used on other orders for this customer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER coe-ord FOR oe-ord.
    
    FOR EACH coe-ord NO-LOCK 
        WHERE coe-ord.company EQ ipboe-ord.company 
          AND coe-ord.cust-no EQ ipboe-ord.cust-no 
          AND coe-ord.ord-no  NE ipboe-ord.ord-no  
          AND coe-ord.po-no   EQ ipboe-ord.po-no:
        ASSIGN 
            oplHold    = TRUE
            opcMessage = "Not a unique PO number for this customer.".
        RETURN.
    END. 

END PROCEDURE.


PROCEDURE pValidShipTo PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Determines if a valid ShipTo record exists for this customer
     Notes:     Since oe-ord is created on ADD button, we can depend on rec_key availability
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bcust   FOR cust.
    DEFINE BUFFER bshipto FOR shipto.
    DEFINE VARIABLE lActive AS LOG NO-UNDO.
    
    FIND FIRST bcust NO-LOCK WHERE 
        bcust.company EQ ipboe-ord.company AND 
        bcust.cust-no EQ ipboe-ord.cust-no  
        NO-ERROR.
    IF NOT AVAILABLE bcust THEN ASSIGN 
            oplHold    = TRUE 
            opcMessage = "Unable to locate a customer for this order.". 
    ELSE 
    DO:
        FIND FIRST bshipto NO-LOCK WHERE 
            bshipto.company EQ ipboe-ord.company AND 
            bshipto.cust-no EQ ipboe-ord.cust-no AND 
            bshipto.ship-id EQ ipboe-ord.ship-id
            NO-ERROR.
        IF NOT AVAILABLE bshipto THEN ASSIGN 
                oplHold    = TRUE 
                opcMessage = "Unable to locate shipto record " + ipboe-ord.ship-id + " for customer " + bcust.cust-no.
        ELSE 
        DO:
            ASSIGN 
                lActive = IF bshipto.statusCode NE "I" THEN YES ELSE NO.
            IF NOT lActive THEN ASSIGN 
                    oplHold    = TRUE 
                    opcMessage = "Specified shipto " + ipboe-ord.ship-id + "is INACTIVE for customer " + bcust.cust-no.
                
        END.
    END. 
    
END PROCEDURE.


PROCEDURE pValidUoM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Verifies that UoM on each line is not blank and can be found in UoM table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes   AS CHARACTER NO-UNDO.

    DEFINE BUFFER boe-ordl FOR oe-ordl.
    
    DEFINE VARIABLE cBadLines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValidUOMs AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    FOR EACH boe-ordl NO-LOCK WHERE 
        boe-ordl.company EQ ipboe-ord.company AND 
        boe-ordl.ord-no EQ ipboe-ord.ord-no AND 
        boe-ordl.line NE 0,
     FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ boe-ordl.company
           AND itemfg.i-no EQ boe-ordl.i-no:
              
        IF boe-ordl.pr-uom EQ "" THEN 
            ASSIGN 
                cBadLines = cBadLines + STRING(boe-ordl.line) + ","
                opcNotes  = IF opcNotes EQ "" THEN boe-ordl.i-no ELSE opcNotes + ", " + boe-ordl.i-no
                .
        ELSE DO:
                RUN Conv_GetValidPriceUOMsForItem(ROWID(itemfg),OUTPUT cValidUOMs, OUTPUT lError, OUTPUT cMessage).
                IF LOOKUP(boe-ordl.pr-uom,cValidUOMs) EQ 0 THEN
                    ASSIGN 
                        cBadLines = cBadLines + STRING(boe-ordl.line) + ","
                        opcNotes  = IF opcNotes EQ "" THEN boe-ordl.i-no ELSE opcNotes + ", " + boe-ordl.i-no
                        .
        END.
    END.
    IF cBadLines NE "" THEN ASSIGN 
            cBadLines  = TRIM(cBadLines,",")
            oplHold    = TRUE 
            opcMessage = "Unable to locate valid UoM for line" + 
                     IF NUM-ENTRIES(cBadLines) GT 1 THEN ("s " + cBadLines)
                     ELSE (" " + cBadLines).

END PROCEDURE.


PROCEDURE RemoveManualRelease:
    /*------------------------------------------------------------------------------
     Purpose:   Removes manual release tag from order
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
    DEFINE  OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE  OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN ClearTagsRelease(
        INPUT ipcRecKey
        ).
        
    ASSIGN 
        oplHold    = FALSE 
        opcMessage = "Manual Release removed".        

END PROCEDURE.


PROCEDURE SetManualRelease:
    /*------------------------------------------------------------------------------
     Purpose:   Removes manual release tag from order
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE  INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
    DEFINE  OUTPUT PARAMETER oplHold AS LOG NO-UNDO.
    DEFINE  OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN addTagRelease (
        INPUT ipcRecKey,"oe-ord"
        ).
        
    ASSIGN 
        oplHold    = FALSE 
        opcMessage = "Order manually released".        

END PROCEDURE.



PROCEDURE ValidateOrder:
    /*------------------------------------------------------------------------------
     Purpose:   Validates a given order by rowid.  Returns single hold indicator
                and simplified hold message.  Adds Hold Tags as appropriate.
     Notes:     Syntax is
                RUN ValidateOrder IN hdOeValidate (
                    INPUT ROWID(oe-ord), 
                    OUTPUT lHold, 
                    OUTPUT cHoldReason
                    ).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrd AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplHold AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE VARIABLE iCountHold AS INTEGER.
       
    FIND FIRST bf-oe-ord NO-LOCK 
        WHERE ROWID(bf-oe-ord) EQ ipriOeOrd 
        NO-ERROR.
    IF NOT AVAILABLE bf-oe-ord THEN 
    DO:
        ASSIGN 
            oplHold    = TRUE
            opcMessage = "Error: Invalid Order RowID provided to Validate Order"
            .
        RUN AddTagHold (
            INPUT bf-oe-ord.rec_key,
            INPUT "oe-ord",
            INPUT opcMessage,
            INPUT ""
            ).
        RETURN.  
    END.
   
    RUN pBuildValidationsToRun(bf-oe-ord.company).    
                    
    RUN ClearTagsHold (
        INPUT bf-oe-ord.rec_key
        ).
    iCountHold = 0.
    FOR EACH ttValidation NO-LOCK:
        RUN VALUE(ttValidation.cProgram) IN THIS-PROCEDURE(
            BUFFER bf-oe-ord,
            OUTPUT ttValidation.lHoldResult,
            OUTPUT ttValidation.cHoldMessage,
            OUTPUT ttValidation.cNotes
            ).
        IF ttValidation.lHoldResult THEN 
        DO:
            IF ttValidation.cHoldOrInfo EQ "HOLD" THEN 
            DO:
                RUN AddTagHold (
                    INPUT bf-oe-ord.rec_key,
                    INPUT "oe-ord",
                    INPUT ttValidation.cHoldMessage,
                    INPUT ttValidation.cNotes
                    ).
                ASSIGN
                    iCountHold = iCountHold + 1
                    oplHold = TRUE
                    opcMessage = opcMessage + "|" + ttValidation.cHoldMessage.
            END.    
            ELSE 
            DO:
                RUN AddTagInfo (
                    INPUT bf-oe-ord.rec_key,
                    INPUT "oe-ord",
                    INPUT ttValidation.cHoldMessage,
                    INPUT ttValidation.cNotes
                    ).
                ttValidation.lHoldResult = FALSE. 
            END.          
        END.
    END.
    ASSIGN 
        opcMessage = TRIM(opcMessage,"|").
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */
