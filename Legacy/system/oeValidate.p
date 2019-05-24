/*-----------------------------------------------------------------------------
    File        : oe/spValidate.p
    Copyright   : (c)1985-2019 Advantzware, Inc. All rights reserved.
    Description : Performs all validations for OE module auto-hold processing
    Author(s)   : MYT
    Created     : Apr 23, 2019 3:18:57 PM
    Notes       : Run as a persistent procedure to access these routines
                 
---------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    DEF VAR hTagProcs AS HANDLE NO-UNDO.
    DEF VAR hdPriceProcs AS HANDLE NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fSetTag RETURNS LOGICAL 
    ( ipcRecKey AS CHARACTER,
    ipcLinkTable AS CHARACTER,
    ipcGroup AS CHARACTER,
    ipcTestProc AS CHARACTER,
    ipcMessage AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */
IF NOT VALID-HANDLE(hdPriceProcs) THEN 
    RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
IF NOT VALID-HANDLE(hTagProcs) THEN 
    RUN system/tagprocs.p PERSISTENT SET hTagProcs.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pCreditHold PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:   Runs "standard" credit check procedure and formats for validation output
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF BUFFER bcust FOR cust.
    
    FIND FIRST bcust NO-LOCK WHERE 
        bcust.company EQ ipboe-ord.company AND 
        bcust.cust-no EQ ipboe-ord.cust-no  
        NO-ERROR.
    IF NOT AVAIL bcust THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate a customer for this order.".
        RETURN.
    END. 
    ELSE DO:
          RUN oe/creditck.p (ROWID(bcust), NO).
          FIND CURRENT bcust NO-LOCK NO-ERROR.
          IF AVAIL bcust AND bcust.cr-hold THEN ASSIGN 
            oplError = TRUE 
            opcMessage = "Customer failed credit check.".  
    END.
    
END PROCEDURE.


PROCEDURE pCustomerPN PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:   Tests for a valid customer part number on each line
 Notes:     "Valid" means itemfg assigned to customer for this part-no and not Inactive 
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF BUFFER boe-ordl FOR oe-ordl.
    DEF BUFFER bitemfg FOR itemfg.
    
    DEF VAR cBadLines AS CHAR NO-UNDO.

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
        IF NOT AVAIL bitemfg THEN ASSIGN 
            cBadLines = cBadLines + STRING(boe-ordl.line) + ",".
    END.
    IF cBadLines NE "" THEN ASSIGN 
        cBadLines = TRIM(cBadLines,",")
        oplError = TRUE 
        opcMessage = "Unable to locate valid customer part number for line" +
                     IF NUM-ENTRIES(cBadLines) GT 1 THEN ("s " + cBadLines)
                     ELSE (" " + cBadLines).

END PROCEDURE.


PROCEDURE pCustomerPO PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:   Verifies a customer PO has been entered on the order header
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF BUFFER bcust FOR cust.
    
    FIND FIRST bcust NO-LOCK WHERE 
        bcust.company EQ ipboe-ord.company AND 
        bcust.cust-no EQ ipboe-ord.cust-no  
        NO-ERROR.
    IF NOT AVAIL bcust THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate a customer for this order.".
        RETURN.
    END. 
    ELSE DO:
        IF bCust.po-mandatory 
        AND ipboe-ord.po-no = "" THEN ASSIGN 
            oplError = TRUE 
            opcMessage = "PO Number may not be blank for this customer".
    END.

END PROCEDURE.


PROCEDURE pPriceGtCost PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:   Verifies that price > cost for each line on order
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF BUFFER boe-ordl FOR oe-ordl.
    DEF VAR cBadLines AS CHAR NO-UNDO.

    FOR EACH boe-ordl NO-LOCK WHERE 
        boe-ordl.company EQ ipboe-ord.company AND 
        boe-ordl.ord-no EQ ipboe-ord.ord-no AND 
        boe-ordl.line NE 0:
        IF boe-ordl.t-price LT boe-ordl.t-cost THEN ASSIGN 
            cBadLines = cBadLines + STRING(boe-ordl.line) + ",".
    END.
    IF cBadLines NE "" THEN ASSIGN 
        cBadLines = TRIM(cBadLines,",")
        oplError = TRUE 
        opcMessage = "Cost is greater than price for line" + 
                     IF NUM-ENTRIES(cBadLines) GT 1 THEN ("s " + cBadLines)
                     ELSE (" " + cBadLines).


END PROCEDURE.


PROCEDURE pPriceHold PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:   Runs "standard" price check for order and formats results
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF VAR lPriceHold AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    
    RUN CheckPriceHoldForOrder IN hdPriceProcs (ROWID(ipboe-ord), NO, NO, OUTPUT lPriceHold, OUTPUT cMessage).
    
    IF lPriceHold THEN ASSIGN 
        oplError = TRUE 
        opcMessage = cMessage.
        
END PROCEDURE.


PROCEDURE pUniquePO PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:   Verifies that the PO entered for this order has not been used on other orders for this customer
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF BUFFER coe-ord FOR oe-ord.
    
    FOR EACH coe-ord NO-LOCK WHERE 
        coe-ord.company EQ ipboe-ord.company AND 
        coe-ord.cust-no EQ ipboe-ord.cust-no:
        IF coe-ord.po-no EQ ipboe-ord.po-no 
        AND coe-ord.ord-no NE ipboe-ord.ord-no THEN DO:
            ASSIGN 
                oplError = TRUE
                opcMessage = "Not a unique PO number for this customer.".
            RETURN.
        END.
    END. 

END PROCEDURE.


PROCEDURE pValidShipTo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:   Determines if a valid ShipTo record exists for this customer
 Notes:     Since oe-ord is created on ADD button, we can depend on rec_key availability
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF BUFFER bcust FOR cust.
    DEF BUFFER bshipto FOR shipto.
    DEF VAR lActive AS LOG NO-UNDO.
    
    FIND FIRST bcust NO-LOCK WHERE 
        bcust.company EQ ipboe-ord.company AND 
        bcust.cust-no EQ ipboe-ord.cust-no  
        NO-ERROR.
    IF NOT AVAIL bcust THEN ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate a customer for this order.". 
    ELSE 
    DO:
        FIND FIRST bshipto NO-LOCK WHERE 
            bshipto.company EQ ipboe-ord.company AND 
            bshipto.cust-no EQ ipboe-ord.cust-no AND 
            bshipto.ship-id EQ ipboe-ord.ship-id
            NO-ERROR.
        IF NOT AVAIL bshipto THEN ASSIGN 
                oplError = TRUE 
                opcMessage = "Unable to locate shipto record " + ipboe-ord.ship-id + " for customer " + bcust.cust-no.
        ELSE DO:
            ASSIGN 
                lActive = DYNAMIC-FUNCTION("isActive",bshipto.rec_key).
            IF NOT lActive THEN ASSIGN 
                oplError = TRUE 
                opcMessage = "Specified shipto " + ipboe-ord.ship-id + "is INACTIVE for customer " + bcust.cust-no.
                
        END.
    END. 
    
END PROCEDURE.


PROCEDURE pValidUoM PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:   Verifies that UoM on each line is not blank and can be found in UoM table
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF BUFFER boe-ordl FOR oe-ordl.
    
    DEF VAR cBadLines AS CHAR NO-UNDO.

    FOR EACH boe-ordl NO-LOCK WHERE 
        boe-ordl.company EQ ipboe-ord.company AND 
        boe-ordl.ord-no EQ ipboe-ord.ord-no AND 
        boe-ordl.line NE 0:
        IF boe-ordl.pr-uom EQ "" THEN ASSIGN 
                cBadLines = cBadLines + STRING(boe-ordl.line) + ",".
        ELSE IF NOT CAN-FIND(FIRST uom WHERE uom.uom EQ boe-ordl.pr-uom) THEN ASSIGN 
                    cBadLines = cBadLines + STRING(boe-ordl.line) + ",".
    END.
    IF cBadLines NE "" THEN ASSIGN 
            cBadLines = TRIM(cBadLines,",")
            oplError = TRUE 
            opcMessage = "Unable to locate valid UoM for line" + 
                     IF NUM-ENTRIES(cBadLines) GT 1 THEN ("s " + cBadLines)
                     ELSE (" " + cBadLines).

END PROCEDURE.


PROCEDURE removeManualRelease:
/*------------------------------------------------------------------------------
 Purpose:   Removes manual release tag from order
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    RUN clearTagsOfType IN hTagProcs (ipcRecKey,
                                      "Release").
        
    ASSIGN 
        oplError = FALSE 
        opcMessage = "Manual Release removed".        

END PROCEDURE.


PROCEDURE setManualRelease:
/*------------------------------------------------------------------------------
 Purpose:   Removes manual release tag from order
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    RUN addTagRelease IN hTagProcs (ipcRecKey).
        
    ASSIGN 
        oplError = FALSE 
        opcMessage = "Order manually released".        

END PROCEDURE.


PROCEDURE validateSingle:
/*------------------------------------------------------------------------------
 Purpose:   Entry point to all validation procs in this procedure
 Notes:     Syntax is
            RUN ValidateSingle IN hspValidate (
                INPUT rec_key to test
                INPUT tablename
                INPUT testName
                OUTPUT logical error (yes/no)
                OUTPUT error message to display
                ).
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcLinkTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcTestName AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    DEF BUFFER boe-ord FOR oe-ord.
    DEF BUFFER boe-ordl FOR oe-ordl.
    
    DEF VAR cTestProc AS CHAR NO-UNDO.
    DEF VAR lError AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    DEF VAR lTag AS LOG NO-UNDO.
    
    CASE ipcLinkTable:
        WHEN "oe-ord" THEN FIND FIRST boe-ord NO-LOCK WHERE boe-ord.rec_key EQ ipcRecKey NO-ERROR.
        WHEN "oe-ordl" THEN FIND FIRST boe-ordl NO-LOCK WHERE boe-ordl.rec_key EQ ipcRecKey NO-ERROR.
        /* Note: this can be expanded if other tables need to be tested */
    END CASE. 
        
    FIND FIRST sys-ctrl NO-LOCK WHERE    /* one test only */ 
        sys-ctrl.module EQ "VAL" AND
        sys-ctrl.name EQ ipcTestName
        NO-ERROR.
    IF NOT AVAIL sys-ctrl THEN FIND FIRST sys-ctrl NO-LOCK WHERE    /* did the test name have a 'p' prefix? */ 
        sys-ctrl.module EQ "VAL" AND
        sys-ctrl.name EQ SUBSTRING(ipcTestName,2)
        NO-ERROR.      
    IF NOT AVAIL sys-ctrl THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate a test with name " + ipcTestName.
        RETURN.
    END.
    ASSIGN 
        cTestProc = "p" + sys-ctrl.name.
    IF NOT CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,cTestProc) THEN DO:
        ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable to locate a test procedure with name " + cTestProc.
        RETURN.
    END.
        
    RUN clearTagsByTestName IN hTagProcs (ipcRecKey, ipcTestName).
    
    RUN VALUE(cTestProc) IN THIS-PROCEDURE (BUFFER boe-ord, OUTPUT lError, OUTPUT cMessage).
    ASSIGN 
        oplError = lError
        opcMessage = cMessage. 
    IF lError THEN DO:
        lTag = fSetTag(ipcRecKey,"oe-ord",sys-ctrl.char-fld,sys-ctrl.name,cMessage).
    END.
    
END PROCEDURE.


PROCEDURE validateAll:
/*------------------------------------------------------------------------------
 Purpose:   Entry point to all validation procs in this procedure
 Notes:     Syntax is
            RUN Validate IN hspValidate (
                INPUT testName or "ALL"
                INPUT tablename
                INPUT rec_key to test
                OUTPUT logical error (yes/no)
                OUTPUT error message to display
                ).
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcLinkTable AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    DEF BUFFER boe-ord FOR oe-ord.
    DEF BUFFER boe-ordl FOR oe-ordl.
    
    DEF VAR cTestProc AS CHAR NO-UNDO.
    DEF VAR lError AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    DEF VAR lTag AS LOG NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    
    CASE ipcLinkTable:
        WHEN "oe-ord" THEN 
            FIND FIRST boe-ord NO-LOCK WHERE boe-ord.rec_key EQ ipcRecKey NO-ERROR.
        WHEN "oe-ordl" THEN 
            FIND FIRST boe-ordl NO-LOCK WHERE boe-ordl.rec_key EQ ipcRecKey NO-ERROR.
    /* Note: this can be expanded if other tables need to be tested */
    END CASE. 
        
    /* Create/setup NK1s if not already there */
    /* I know there is a "standard" for this, but we're pulling two values from eight records,
       This is much more compact. */
    DEF VAR cTestList AS CHAR INITIAL "CreditHold,CustomerPN,CustomerPO,PriceGtCost,PriceHold,UniquePO,ValidShipTo,ValidUom" NO-UNDO.    
    DEF VAR cReqdList AS CHAR INITIAL "TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE" NO-UNDO.    
    DEF VAR cTypeList AS CHAR INITIAL "HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD" NO-UNDO.    
    DO iCtr = 1 TO NUM-ENTRIES(cTestList):
        IF NOT CAN-FIND(FIRST sys-ctrl WHERE 
            sys-ctrl.company EQ boe-ord.company AND
            sys-ctrl.module EQ "VAL" AND  
            sys-ctrl.name EQ ENTRY(iCtr,cTestList)) THEN DO:
            CREATE sys-ctrl.
            ASSIGN 
                sys-ctrl.company = boe-ord.company
                sys-ctrl.module = "VAL"
                sys-ctrl.name = ENTRY(iCtr,cTestList)
                sys-ctrl.log-fld = LOGICAL(ENTRY(iCtr,cReqdList))
                sys-ctrl.char-fld = ENTRY(iCtr,cTypeList).
        END.
    END.
                
    RUN clearTagsHold IN hTagProcs (ipcRecKey).
    
    FOR EACH sys-ctrl NO-LOCK WHERE    /* ALL tests requested */ 
        sys-ctrl.module EQ "VAL" AND 
        sys-ctrl.log-fld EQ TRUE:
        ASSIGN 
            cTestProc = "p" + sys-ctrl.name.
        RUN VALUE(cTestProc) IN THIS-PROCEDURE (BUFFER boe-ord, OUTPUT lError, OUTPUT cMessage).
        IF lError THEN DO:
            lTag = fSetTag(ipcRecKey,"oe-ord",sys-ctrl.char-fld,sys-ctrl.name,cMessage).
        END.
        IF sys-ctrl.char-fld EQ "INFO" THEN ASSIGN 
            lError = FALSE.
        ASSIGN 
            oplError = IF NOT oplError THEN lError ELSE oplError
            opcMessage = IF lError THEN opcMessage + "   " + cMessage + chr(10) ELSE opcMessage. 
    END.
    ASSIGN 
        opcMessage = TRIM(opcMessage,CHR(10)).
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fSetTag RETURNS LOGICAL 
	( ipcRecKey AS CHARACTER,
	  ipcLinkTable AS CHARACTER,
	  ipcGroup AS CHARACTER,
	  ipcTestProc AS CHARACTER,
	  ipcMessage AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:   Set Hold tags on record where appropriate (test is defined in sys-ctrl table)
 Notes:
------------------------------------------------------------------------------*/	
    RUN addTagHold IN hTagProcs (ipcRecKey,
                                 ipcLinkTable,
                                 ipcGroup,
                                 ipcTestProc,
                                 ipcMessage).
    RETURN TRUE.
		
END FUNCTION.
