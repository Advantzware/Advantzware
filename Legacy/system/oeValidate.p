/*-----------------------------------------------------------------------------
    File        : oe/spValidate.p
    Copyright   : (c)1985-2019 Advantzware, Inc. All rights reserved.
    Description : Performs all validations for OE module
    Author(s)   : MYT
    Created     : Apr 23, 2019 3:18:57 PM
    Notes       : Run as a persistent procedure to access these routines
                 
---------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    DEF VAR hTagProcs AS HANDLE NO-UNDO.
    DEF VAR hdPriceProcs AS HANDLE NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
RUN system/tagprocs.p PERSISTENT SET hTagProcs.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pCreditHold:
/*------------------------------------------------------------------------------
 Purpose:   Runs "standard" credit check procedure and formats for validation output
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER b-oeord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    FIND FIRST cust NO-LOCK WHERE 
        cust.cust-no EQ b-oeord.cust-no  
        NO-ERROR.
    IF NOT AVAIL cust THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate a customer for this order.".
        RETURN.
    END. 
    ELSE DO:
          RUN oe/creditck.p (ROWID(cust), NO).
          FIND CURRENT cust NO-LOCK NO-ERROR.
          IF AVAIL cust AND cust.cr-hold THEN ASSIGN 
            oplError = TRUE 
            opcMessage = "Customer failed credit check.".  
    END.
    
END PROCEDURE.

PROCEDURE pCustomerPN:
/*------------------------------------------------------------------------------
 Purpose:   Tests for a valid customer part number on each line
 Notes:     "Valid" means itemfg assigned to customer for this part-no and not Inactive 
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER b-oeord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF VAR cBadLines AS CHAR NO-UNDO.

    FOR EACH oe-ordl NO-LOCK OF b-oeord:
        FIND FIRST itemfg NO-LOCK WHERE
            itemfg.cust-no EQ oe-ordl.cust-no AND 
            itemfg.part-no EQ oe-ordl.part-no AND 
            itemfg.stat NE "I"
            NO-ERROR.
        IF NOT AVAIL itemfg THEN ASSIGN 
            cBadLines = cBadLines + STRING(oe-ordl.line) + ",".
    END.
    IF cBadLines NE "" THEN ASSIGN 
        cBadLines = TRIM(cBadLines,",")
        oplError = TRUE 
        opcMessage = "Unable to locate valid customer part number for line(s) " + cBadLines + ".".

END PROCEDURE.

PROCEDURE pCustomerPO:
/*------------------------------------------------------------------------------
 Purpose:   Verifies a customer PO has been entered on the order header
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER b-oeord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    IF b-oeord.po-no = "" THEN ASSIGN 
        oplError = TRUE 
        opcMessage = "PO Number may not be blank".

END PROCEDURE.

PROCEDURE pPriceGtCost:
/*------------------------------------------------------------------------------
 Purpose:   Verifies that price > cost for each line on order
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER b-oeord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF VAR cBadLines AS CHAR NO-UNDO.

    FOR EACH oe-ordl NO-LOCK OF b-oeord:
        IF oe-ordl.price LT oe-ordl.cost THEN ASSIGN 
            cBadLines = cBadLines + STRING(oe-ordl.line) + ",".
    END.
    IF cBadLines NE "" THEN ASSIGN 
        cBadLines = TRIM(cBadLines,",")
        oplError = TRUE 
        opcMessage = "Cost is greater than price for line(s) " + cBadLines + ".".


END PROCEDURE.

PROCEDURE pPriceHold:
/*------------------------------------------------------------------------------
 Purpose:   Runs "standard" price check for order and formats results
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER b-oeord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF VAR lPriceHold AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    
    RUN CheckPriceHoldForOrder IN hdPriceProcs (ROWID(b-oeord), NO, NO, OUTPUT lPriceHold, OUTPUT cMessage).
    
    IF lPriceHold THEN ASSIGN 
        oplError = TRUE 
        opcMessage = cMessage.
        
END PROCEDURE.

PROCEDURE pUniquePO:
/*------------------------------------------------------------------------------
 Purpose:   Verifies that the PO entered for this order has not been used on other orders for this customer
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER b-oeord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF BUFFER c-oeord FOR oe-ord.
    FOR EACH c-oeord NO-LOCK WHERE 
        c-oeord.company EQ b-oeord.company AND 
        c-oeord.cust-no EQ b-oeord.cust-no:
        IF c-oeord.po-no EQ b-oeord.po-no 
        AND c-oeord.ord-no NE b-oeord.ord-no THEN DO:
            ASSIGN 
                oplError = TRUE
                opcMessage = "Not a unique PO number for this customer.".
            RETURN.
        END.
    END. 

END PROCEDURE.

PROCEDURE pValidate:
/*------------------------------------------------------------------------------
 Purpose:   Entry point to all procs in this procedure
 Notes:     Syntax is
            RUN pValidate IN hspValidate (
                INPUT testName
                INPUT tablename
                INPUT rec_key to test
                INPUT if no rec_key yet, then key values
                OUTPUT logical error (yes/no)
                OUTPUT error message to display
                ).
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTestProc AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcLinkTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcKeyVals AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    DEF BUFFER b-oeord FOR oe-ord.
    DEF BUFFER b-oeordl FOR oe-ordl.
    
    DEF VAR cTestProc AS CHAR NO-UNDO.
    DEF VAR lError AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    
    CASE ipcLinkTable:
        WHEN "oe-ord" THEN FIND FIRST b-oeord NO-LOCK WHERE b-oeord.rec_key EQ ipcRecKey NO-ERROR.
        WHEN "oe-ordl" THEN FIND FIRST b-oeordl NO-LOCK WHERE b-oeordl.rec_key EQ ipcRecKey NO-ERROR.
        /* Note: this can be expanded if other tables need to be tested */
    END CASE. 
        
    /* Can convert test name to proc name if needed */
    IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,ipcTestProc) THEN ASSIGN 
        cTestProc = ipcTestProc.
    ELSE IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,"p" + ipcTestProc) THEN ASSIGN 
        cTestProc = "p" + ipcTestProc.
    ELSE DO:
        ASSIGN 
            oplError = TRUE
            opcMessage = "Unable to locate this test in system/oeValidate.p".
        RETURN.
    END.   
    
    RUN VALUE(cTestProc) IN THIS-PROCEDURE (BUFFER b-oeord, OUTPUT lError, OUTPUT cMessage).
    
    ASSIGN 
        oplError = lError
        opcMessage = cMessage.
        
    /* Set/Clear Hold tags on record where appropriate (test is defined in validationTest table) */
    IF CAN-FIND(FIRST validationTest WHERE 
                    validationTest.testType EQ "Validate" AND 
                    validationTest.testName EQ ipcTestProc AND
                    validationTest.testTable EQ ipcLinkTable AND 
                    validationTest.testRequired EQ TRUE) THEN DO:
        IF lError EQ TRUE THEN RUN addTag IN hTagProcs (ipcRecKey,
                                                        ipcLinkTable,
                                                        "HoldStatus",
                                                        "Validate",
                                                        ipcTestProc,
                                                        opcMessage,
                                                        "N",
                                                        "stat",
                                                        "H",
                                                        "").
        ELSE RUN clearTagsByName IN hTagProcs (ipcRecKey,
                                               ipcTestProc).
    END. 

END PROCEDURE.

PROCEDURE pValidShipTo:
/*------------------------------------------------------------------------------
 Purpose:   Determines if a valid ShipTo record exists for this customer
 Notes:     Since oe-ord is created on ADD button, we can depend on rec_key availability
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER b-oeord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    FIND FIRST cust NO-LOCK WHERE 
        cust.cust-no EQ b-oeord.cust-no  
        NO-ERROR.
    IF NOT AVAIL cust THEN ASSIGN 
        oplError = TRUE 
        opcMessage = "Unable to locate a customer for this order.". 
    
    ELSE DO:
        FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
        IF NOT AVAIL shipto THEN ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable TO locate a shipto record for customer " + cust.cust-no.
    END. 
    
END PROCEDURE.

PROCEDURE pValidUoM:
/*------------------------------------------------------------------------------
 Purpose:   Verifies that UoM on each line is not blank and can be found in UoM table
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER b-oeord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF VAR cBadLines AS CHAR NO-UNDO.

    FOR EACH oe-ordl NO-LOCK OF b-oeord:
        IF oe-ordl.pr-uom EQ "" THEN ASSIGN 
            cBadLines = cBadLines + STRING(oe-ordl.line) + ",".
        ELSE IF NOT CAN-FIND(FIRST uom WHERE uom.uom EQ oe-ordl.pr-uom) THEN ASSIGN 
            cBadLines = cBadLines + STRING(oe-ordl.line) + ",".
    END.
    IF cBadLines NE "" THEN ASSIGN 
        cBadLines = TRIM(cBadLines,",")
        oplError = TRUE 
        opcMessage = "Unable to locate valid UoM for line(s) " + cBadLines + ".".

END PROCEDURE.
