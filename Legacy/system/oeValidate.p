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

/* ************************  Function Prototypes ********************** */


FUNCTION fSetTag RETURNS LOGICAL 
	( ipcTestProc AS CHARACTER,
        ipcRecKey AS CHARACTER,
        ipcLinkTable AS CHARACTER, 
        iplError AS LOG,
        ipcMessage AS CHAR ) FORWARD.


/* ***************************  Main Block  *************************** */
RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
RUN system/tagprocs.p PERSISTENT SET hTagProcs.


/* **********************  Internal Procedures  *********************** */

PROCEDURE HoldCheck:
    /*------------------------------------------------------------------------------
     Purpose:   Tests order for existence of hold tag without manual release tag
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTestProc AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcLinkTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    IF CAN-FIND(FIRST tag WHERE  
                tag.linkReckey  EQ ipcRecKey AND 
                tag.linkTable   EQ ipcLinkTable AND 
                tag.tagGroup    EQ "HoldStatus" AND 
                tag.tagType     EQ "Validate") 
    AND NOT CAN-FIND(FIRST tag WHERE  
                tag.linkReckey  EQ ipcRecKey AND 
                tag.linkTable   EQ ipcLinkTable AND 
                tag.tagGroup    EQ "HoldStatus" AND 
                tag.tagType     EQ "Validate" AND  
                tag.tagName     EQ "ManualRelease") 
    THEN ASSIGN 
        oplError = TRUE 
        opcMessage = "Order is on HOLD".
    ELSE ASSIGN 
        oplError = FALSE 
        opcMessage = "Order is NOT on hold".
    
END PROCEDURE.

PROCEDURE ManualRelease:
/*------------------------------------------------------------------------------
 Purpose:   Applies manual release tag to held order
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTestProc AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcLinkTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    RUN addTag IN hTagProcs (ipcRecKey,
        ipcLinkTable,
        "HoldStatus",
        "Validate",
        "ManualRelease",
        "Manual Release by " + USERID("asi") + " on " + STRING(DATE(TODAY)),
        "N",
        "stat",
        "H",
        "").

    ASSIGN 
        oplError = FALSE 
        opcMessage = "Manual Release applied".        
        
END PROCEDURE.

PROCEDURE pCreditHold:
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

PROCEDURE pCustomerPN:
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
        boe-ordl.ord-no EQ ipboe-ord.ord-no:
        FIND FIRST bitemfg NO-LOCK WHERE
            bitemfg.company EQ boe-ordl.company AND 
            bitemfg.cust-no EQ boe-ordl.cust-no AND 
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

PROCEDURE pCustomerPO:
/*------------------------------------------------------------------------------
 Purpose:   Verifies a customer PO has been entered on the order header
 Notes:
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    IF ipboe-ord.po-no = "" THEN ASSIGN 
        oplError = TRUE 
        opcMessage = "PO Number may not be blank".

END PROCEDURE.

PROCEDURE pPriceGtCost:
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
        boe-ordl.ord-no EQ ipboe-ord.ord-no:
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

PROCEDURE pPriceHold:
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

PROCEDURE pUniquePO:
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


PROCEDURE ReleaseCheck:
    /*------------------------------------------------------------------------------
     Purpose:   Tests order for existence of hold tag without manual release tag
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTestProc AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcLinkTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    FIND FIRST tag NO-LOCK WHERE  
        tag.linkReckey  EQ ipcRecKey AND 
        tag.linkTable   EQ ipcLinkTable AND 
        tag.tagGroup    EQ "HoldStatus" AND 
        tag.tagType     EQ "Validate" AND  
        tag.tagName     EQ "ManualRelease"
        NO-ERROR. 
    IF AVAIL tag THEN ASSIGN 
            oplError = TRUE 
            opcMessage = tag.description.
    ELSE ASSIGN 
            oplError = FALSE 
            opcMessage = "No manual release for this order".
    
END PROCEDURE.

PROCEDURE RemoveRelease:
/*------------------------------------------------------------------------------
 Purpose:   Removes manual release tag from order
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTestProc AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcLinkTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    RUN clearTagsByName IN hTagProcs (ipcRecKey,
        "ManualRelease").
        
    ASSIGN 
        oplError = FALSE 
        opcMessage = "Manual Release removed".        

END PROCEDURE.

PROCEDURE pValidate:
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
    DEF INPUT PARAMETER ipcTestProc AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcLinkTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
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
        
    IF ipcTestProc NE "ALL" THEN DO: /* Individual Test requested */
        /* Can convert test name to proc name if needed */
        IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,ipcTestProc) THEN ASSIGN 
            cTestProc = ipcTestProc.
        ELSE DO:
            ASSIGN 
                oplError = TRUE
                opcMessage = "Unable to locate this test in system/oeValidate.p".
            RETURN.
        END.   
        RUN VALUE(cTestProc) IN THIS-PROCEDURE (BUFFER boe-ord, OUTPUT lError, OUTPUT cMessage).
        ASSIGN 
            oplError = lError
            opcMessage = cMessage.
        IF lError THEN lTag = fSetTag(ipcTestProc,ipcRecKey,ipcLinkTable,oplError,opcMessage).
    END.
    ELSE DO:
        FOR EACH sys-ctrl NO-LOCK WHERE    /* ALL tests requested */ 
            sys-ctrl.module EQ "VAL" AND 
            sys-ctrl.char-fld EQ ipcLinkTable AND 
            sys-ctrl.log-fld EQ TRUE:
            ASSIGN 
                cTestProc = "p" + sys-ctrl.name.
            RUN VALUE(cTestProc) IN THIS-PROCEDURE (BUFFER boe-ord, OUTPUT lError, OUTPUT cMessage).
            ASSIGN 
                oplError = IF NOT oplError THEN lError ELSE oplError
                opcMessage = IF lError THEN opcMessage + "   " + cMessage + chr(10) ELSE opcMessage. 
            IF lError THEN lTag = fSetTag(SUBSTRING(cTestProc,2),ipcRecKey,ipcLinkTable,lError,cMessage).
        END.
        ASSIGN 
            opcMessage = TRIM(opcMessage,CHR(10)).
    END.
END PROCEDURE.

PROCEDURE pValidShipTo:
/*------------------------------------------------------------------------------
 Purpose:   Determines if a valid ShipTo record exists for this customer
 Notes:     Since oe-ord is created on ADD button, we can depend on rec_key availability
------------------------------------------------------------------------------*/
    DEF PARAMETER BUFFER ipboe-ord FOR oe-ord.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.

    DEF BUFFER bcust FOR cust.
    DEF BUFFER bshipto FOR shipto.
    
    FIND FIRST bcust NO-LOCK WHERE 
        bcust.company EQ ipboe-ord.company AND 
        bcust.cust-no EQ ipboe-ord.cust-no  
        NO-ERROR.
    IF NOT AVAIL bcust THEN ASSIGN 
        oplError = TRUE 
        opcMessage = "Unable to locate a customer for this order.". 
    ELSE DO:
        FIND FIRST bshipto OF bcust NO-LOCK NO-ERROR.
        IF NOT AVAIL bshipto THEN ASSIGN 
            oplError = TRUE 
            opcMessage = "Unable TO locate a shipto record for customer " + bcust.cust-no.
    END. 
    
END PROCEDURE.

PROCEDURE pValidUoM:
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
        boe-ordl.ord-no EQ ipboe-ord.ord-no:
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


/* ************************  Function Implementations ***************** */

FUNCTION fSetTag RETURNS LOGICAL 
	( ipcTestProc AS CHARACTER,
	  ipcRecKey AS CHARACTER, 
	  ipcLinkTable AS CHARACTER, 
	  iplError AS LOG,
	  ipcMessage AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

    /* Set/Clear Hold tags on record where appropriate (test is defined in sys-ctrl table) */
    IF CAN-FIND(FIRST sys-ctrl WHERE 
        sys-ctrl.module EQ "VAL" AND 
        sys-ctrl.name EQ ipcTestProc AND
        sys-ctrl.char-fld EQ ipcLinkTable AND 
        sys-ctrl.log-fld EQ TRUE) THEN DO:
        IF iplError EQ TRUE THEN RUN addTag IN hTagProcs (ipcRecKey,
                ipcLinkTable,
                "HoldStatus",
                "Validate",
                ipcTestProc,
                ipcMessage,
                "N",
                "stat",
                "H",
                "").
        ELSE RUN clearTagsByName IN hTagProcs (ipcRecKey,
                ipcTestProc).
    END. 
    RETURN TRUE.
		
END FUNCTION.
