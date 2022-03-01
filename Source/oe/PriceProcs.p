
/*------------------------------------------------------------------------
    File        : PriceProcs.p
    Purpose     : Replaces oe/GetPriceMatrix.p  
                           oe/GetPriceMatrixPrice.p 
                           oe/GetPriceTotal.p
                           oe/oe-price.i (contents)
                           oe/oe-pric1.i
                           oe/oe-pric2.i
                           oe/oe-rpric.i (contents)
                           

    Syntax      :

    Description : Persistent Procedure for housing logic related to 
    returning the correct price given inputs for finished goods

    Author(s)   : BV
    Created     : Mon Apr 30 15:40:43 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFGItemClass
    FIELD cClassID       AS CHARACTER 
    FIELD dClassQuantity AS DECIMAL
    .
     
DEFINE TEMP-TABLE ttItemLines
    FIELD riLine          AS ROWID
    FIELD lIsPrimary      AS LOGICAL
    FIELD cCompany        AS CHARACTER 
    FIELD cFGItemID       AS CHARACTER
    FIELD cCustID         AS CHARACTER 
    FIELD cShipID         AS CHARACTER  
    FIELD cFGItemClass    AS CHARACTER 
    FIELD dQuantity       AS DECIMAL  
    FIELD dQuantityLookup AS DECIMAL
    FIELD dPrice          AS DECIMAL 
    FIELD cPriceUOM       AS CHARACTER
    FIELD dPriceTotal     AS DECIMAL 
    FIELD lMatrixExists   AS LOGICAL
    FIELD dDiscount       AS DECIMAL 
    FIELD iCaseCount      AS INTEGER
    FIELD cTableType      AS CHARACTER 
    .
DEFINE TEMP-TABLE ttOePrmtx
    FIELD company       AS CHARACTER LABEL "Company"
    FIELD custNo        AS CHARACTER LABEL "Customer"
    FIELD custype       AS CHARACTER LABEL "Cust Type"
    FIELD custShipId    AS CHARACTER LABEL "ShipID"
    FIELD itemID        AS CHARACTER LABEL "Item" FORMAT "X(15)"
    FIELD procat        AS CHARACTER LABEL "Category"
    FIELD effectiveDate AS DATE      LABEL "Effective Date"
    FIELD oldExpiryDate AS DATE      LABEL "Old Expiry Date"
    FIELD newExpiryDate AS DATE      LABEL "New Expiry Date"
    FIELD quantity1     AS INTEGER   LABEL "Quantity 1"  FORMAT ">>,>>>,>>9" 
    FIELD price1        AS DECIMAL   LABEL "Price 1"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity2     AS INTEGER   LABEL "Quantity 2"  FORMAT ">>,>>>,>>9" 
    FIELD price2        AS DECIMAL   LABEL "Price 2"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity3     AS INTEGER   LABEL "Quantity 3"  FORMAT ">>,>>>,>>9" 
    FIELD price3        AS DECIMAL   LABEL "Price 3"     FORMAT "->>,>>>,>>9.99<<<<"    
    FIELD quantity4     AS INTEGER   LABEL "Quantity 4"  FORMAT ">>,>>>,>>9" 
    FIELD price4        AS DECIMAL   LABEL "Price 4"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity5     AS INTEGER   LABEL "Quantity 5"  FORMAT ">>,>>>,>>9" 
    FIELD price5        AS DECIMAL   LABEL "Price 5"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity6     AS INTEGER   LABEL "Quantity 6"  FORMAT ">>,>>>,>>9" 
    FIELD price6        AS DECIMAL   LABEL "Price 6"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity7     AS INTEGER   LABEL "Quantity 7"  FORMAT ">>,>>>,>>9" 
    FIELD price7        AS DECIMAL   LABEL "Price 7"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity8     AS INTEGER   LABEL "Quantity 8"  FORMAT ">>,>>>,>>9" 
    FIELD price8        AS DECIMAL   LABEL "Price 8"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity9     AS INTEGER   LABEL "Quantity 9"  FORMAT ">>,>>>,>>9" 
    FIELD price9        AS DECIMAL   LABEL "Price 9"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity10    AS INTEGER   LABEL "Quantity 10" FORMAT ">>,>>>,>>9" 
    FIELD price10       AS DECIMAL   LABEL "Price 10"    FORMAT "->>,>>>,>>9.99<<<<"
    .  

DEFINE TEMP-TABLE ttOePrmtxCSV LIKE ttOePrmtx.

DEFINE TEMP-TABLE ttQuoteHd
    FIELD company       AS CHARACTER              LABEL "Company"  
    FIELD loc           AS CHARACTER              LABEL "Location"
    FIELD quoteNo       AS INTEGER FORMAT ">>>>9" LABEL "Quote"
    FIELD estimate      AS CHARACTER              LABEL "Estimate"
    FIELD custID        AS CHARACTER              LABEL "Customer"
    FIELD quoteDate     AS DATE                   LABEL "Quote Date"
    FIELD deliveryDate  AS DATE                   LABEL "DeliveryDate"
    FIELD oldExpiryDate AS DATE                   LABEL "Old Expiry Date"
    FIELD newExpiryDate AS DATE                   LABEL "New Expiry Date"
    .
    
DEFINE TEMP-TABLE tt-VendItemCost
    FIELD company           AS CHARACTER                LABEL "Company"
    FIELD estimate          AS CHARACTER                LABEL "Estimate"
    FIELD formNo            AS INTEGER   FORMAT ">9"    LABEL "Form"
    FIELD blankNo           AS INTEGER   FORMAT ">9"    LABEL "Blank"
    FIELD itemID            AS CHARACTER FORMAT "X(20)" LABEL "Item"
    FIELD vendorID          AS CHARACTER FORMAT "X(10)" LABEL "Vendor"
    FIELD customerID        AS CHARACTER FORMAT "X(10)" LABEL "Customer"
    FIELD itemType          AS CHARACTER                LABEL "Item Type"
    FIELD UOM               AS CHARACTER FORMAT "X(5)"  LABEL "Vendor UOM"
    FIELD effectiveDate     AS DATE                     LABEL "Effective Date"
    FIELD oldExpirationDate AS DATE                     LABEL "Old Expiry Date"
    FIELD newExpirationDate AS DATE                     LABEL "New Expiry Date"
    .
      
{oe/ttPriceHold.i} 
{system/ttPriceMatrix.i}
/* {util/ttInactiveQuotes.i} */

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fUseLastPrice RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION fGetNk1PriceMatrixPricingMethod RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.  

FUNCTION fGetNk1OEUseMatrixForNonstock RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD. 

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE pCreateOePrmtxTT PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustNo         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustType       AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttOePrmtx.
    
    FOR EACH oe-prmtx NO-LOCK 
        WHERE oe-prmtx.company    EQ ipcCompany
          AND oe-prmtx.cust-no    EQ ipcCustNo
          AND oe-prmtx.i-no       EQ ipcItemID
          AND oe-prmtx.custype    EQ ipcCustType
          AND oe-prmtx.custShipID EQ ipcShipID:          
          
          IF NOT(oe-prmtx.exp-date EQ ? OR oe-prmtx.exp-date GT TODAY) THEN 
            NEXT.             
                      
        CREATE ttOePrmtx.        
        ASSIGN
            ttOePrmtx.company       = oe-prmtx.company       
            ttOePrmtx.custNo        = oe-prmtx.cust-no
            ttOePrmtx.custype       = oe-prmtx.custype
            ttOePrmtx.custShipId    = oe-prmtx.custShipID
            ttOePrmtx.itemID        = oe-prmtx.i-no
            ttOePrmtx.procat        = oe-prmtx.procat
            ttOePrmtx.effectiveDate = oe-prmtx.eff-date
            ttOePrmtx.oldExpiryDate = oe-prmtx.exp-date
            ttOePrmtx.newExpiryDate = TODAY - 1
            ttOePrmtx.quantity1     = oe-prmtx.qty[1]
            ttOePrmtx.price1        = oe-prmtx.price[1]
            ttOePrmtx.quantity2     = oe-prmtx.qty[2]
            ttOePrmtx.price2        = oe-prmtx.price[2]
            ttOePrmtx.quantity3     = oe-prmtx.qty[3]
            ttOePrmtx.price3        = oe-prmtx.price[3]
            ttOePrmtx.quantity4     = oe-prmtx.qty[4]
            ttOePrmtx.price4        = oe-prmtx.price[4]
            ttOePrmtx.quantity5     = oe-prmtx.qty[5]
            ttOePrmtx.price5        = oe-prmtx.price[5]
            ttOePrmtx.quantity6     = oe-prmtx.qty[6]
            ttOePrmtx.price6        = oe-prmtx.price[6]
            ttOePrmtx.quantity7     = oe-prmtx.qty[7]
            ttOePrmtx.price7        = oe-prmtx.price[7]
            ttOePrmtx.quantity8     = oe-prmtx.qty[8]
            ttOePrmtx.price8        = oe-prmtx.price[8]
            ttOePrmtx.quantity9     = oe-prmtx.qty[9]
            ttOePrmtx.price9        = oe-prmtx.price[9]
            ttOePrmtx.quantity10    = oe-prmtx.qty[10]
            ttOePrmtx.price10       = oe-prmtx.price[10].
    END.              

END PROCEDURE.

PROCEDURE pCheckDuplicateQuoteEntry PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE INPUT PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustNo         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustType       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcProcat         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iQuoteNo AS INT64 NO-UNDO.
    DEFINE BUFFER bf-oe-prmtx  FOR oe-prmtx. 
    
    FOR EACH bf-oe-prmtx EXCLUSIVE-LOCK 
        WHERE bf-oe-prmtx.company    EQ ipcCompany
          AND bf-oe-prmtx.cust-no    EQ ipcCustNo
          AND bf-oe-prmtx.i-no       EQ ipcItemID
          AND bf-oe-prmtx.custype    EQ ipcCustType
          AND bf-oe-prmtx.custShipID EQ ipcShipID
          AND bf-oe-prmtx.procat     EQ ipcProcat 
          AND bf-oe-prmtx.quoteID    NE 0
          BREAK BY bf-oe-prmtx.cust-no DESC
          BY bf-oe-prmtx.i-no DESC
          BY bf-oe-prmtx.custype DESC
          BY bf-oe-prmtx.custShipID DESC
          BY bf-oe-prmtx.procat DESC
          BY bf-oe-prmtx.eff-date DESC
          BY bf-oe-prmtx.quoteID DESC:
          
          IF FIRST-OF(bf-oe-prmtx.procat) THEN
           iQuoteNo = bf-oe-prmtx.quoteID.
           
/*          IF NOT FIRST-OF(bf-oe-prmtx.procat) AND iQuoteNo NE bf-oe-prmtx.quoteID THEN*/
          IF NOT FIRST-OF(bf-oe-prmtx.procat) AND iQuoteNo EQ bf-oe-prmtx.quoteID THEN
          bf-oe-prmtx.quoteID = 0.
    END.          
    RELEASE bf-oe-prmtx.      
END PROCEDURE.          

PROCEDURE pExpireOldPrices PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplExpire         AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustNo         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustType       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcProcat         AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOePrmtxCsv.

    DEFINE BUFFER bf-oe-prmtx  FOR oe-prmtx.
    
    DEFINE VARIABLE dtEffectiveDate AS DATE    NO-UNDO.
    DEFINE VARIABLE iCount          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount1         AS INTEGER NO-UNDO.
         
      RUN pCreateOePrmtxTT(
          INPUT ipcCompany,
          INPUT ipcItemID,
          INPUT ipcShipID,
          INPUT ipcCustNo,
          INPUT ipcCustType
          ).
          
      IF iplExpire THEN    
      RUN pCheckDuplicateQuoteEntry(
                   INPUT ipcCompany,
                   INPUT ipcItemID,
                   INPUT ipcShipID,
                   INPUT ipcCustNo,
                   INPUT ipcCustType,
                   INPUT ipcProcat
                   ).
                   
    FOR EACH bf-oe-prmtx EXCLUSIVE-LOCK 
        WHERE bf-oe-prmtx.company    EQ ipcCompany
          AND bf-oe-prmtx.cust-no    EQ ipcCustNo
          AND bf-oe-prmtx.i-no       EQ ipcItemID
          AND bf-oe-prmtx.custype    EQ ipcCustType
          AND bf-oe-prmtx.custShipID EQ ipcShipID
          AND bf-oe-prmtx.procat     EQ ipcProcat         
          BY bf-oe-prmtx.eff-date
          BY bf-oe-prmtx.exp-date:        
            
        IF NOT(bf-oe-prmtx.exp-date EQ ? OR bf-oe-prmtx.exp-date GT TODAY) THEN 
            NEXT.
                       
        ASSIGN 
            iCount = iCount + 1
            iCount1 = 0
            .
            
        FOR EACH ttOePrmtx NO-LOCK
            WHERE ttOePrmtx.company    EQ ipcCompany
              AND ttOePrmtx.custno     EQ ipcCustNo
              AND ttOePrmtx.itemID     EQ ipcItemID
              AND ttOePrmtx.custype    EQ ipcCustType
              AND ttOePrmtx.custShipID EQ ipcShipID
              AND ttOePrmtx.procat     EQ ipcProcat
              BY ttoeprmtx.effectiveDate 
              BY ttoeprmtx.oldExpiryDate:

            iCount1 = iCount1 + 1.
            IF iCount1 EQ iCount + 1 THEN DO: 
                IF NOT iplExpire THEN DO:
                    CREATE ttOePrmtxCsv.
                    ASSIGN 
                        ttOePrmtxCsv.company       = ipcCompany
                        ttOePrmtxCsv.custNo        = ipcCustNo
                        ttOePrmtxCsv.itemID        = ipcItemID
                        ttOePrmtxCsv.custShipId    = ipcShipID
                        ttOePrmtxCsv.custype       = ipcCustType
                        ttOePrmtxCsv.procat        = ipcProcat
                        ttOePrmtxCsv.effectiveDate = bf-oe-prmtx.eff-date
                        ttOePrmtxCsv.oldExpiryDate = bf-oe-prmtx.exp-date
                        ttOePrmtxCsv.Price1        = bf-oe-prmtx.price[1]
                        ttOePrmtxCsv.Price2        = bf-oe-prmtx.price[2] 
                        ttOePrmtxCsv.Price3        = bf-oe-prmtx.price[3] 
                        ttOePrmtxCsv.Price4        = bf-oe-prmtx.price[4] 
                        ttOePrmtxCsv.Price5        = bf-oe-prmtx.price[5] 
                        ttOePrmtxCsv.Price6        = bf-oe-prmtx.price[6] 
                        ttOePrmtxCsv.Price7        = bf-oe-prmtx.price[7] 
                        ttOePrmtxCsv.Price8        = bf-oe-prmtx.price[8] 
                        ttOePrmtxCsv.Price9        = bf-oe-prmtx.price[9] 
                        ttOePrmtxCsv.Price10       = bf-oe-prmtx.price[10] 
                        ttOePrmtxCsv.Quantity1     = bf-oe-prmtx.qty[1]
                        ttOePrmtxCsv.Quantity2     = bf-oe-prmtx.qty[2]
                        ttOePrmtxCsv.Quantity3     = bf-oe-prmtx.qty[3]
                        ttOePrmtxCsv.Quantity4     = bf-oe-prmtx.qty[4]
                        ttOePrmtxCsv.Quantity5     = bf-oe-prmtx.qty[5]
                        ttOePrmtxCsv.Quantity6     = bf-oe-prmtx.qty[6]
                        ttOePrmtxCsv.Quantity7     = bf-oe-prmtx.qty[7]
                        ttOePrmtxCsv.Quantity8     = bf-oe-prmtx.qty[8]
                        ttOePrmtxCsv.Quantity9     = bf-oe-prmtx.qty[9]
                        ttOePrmtxCsv.Quantity10    = bf-oe-prmtx.qty[10]
                        .
                END.                  
                      
                IF ttOeprmtx.effectiveDate - 1  LT bf-oe-prmtx.eff-date THEN DO:
                    IF iplExpire THEN
                        bf-oe-prmtx.exp-date = ttOeprmtx.effectiveDate.
                    ELSE 
                        ttOePrmtxCsv.newExpiryDate = ttOeprmtx.effectiveDate.                           
                END.                 
                ELSE DO:
                    IF iplExpire THEN
                    do:
                     ASSIGN
                        bf-oe-prmtx.exp-date = ttOeprmtx.effectiveDate - 1.                        
                    END.    
                    ELSE 
                        ttOePrmtxCsv.newExpiryDate = ttOeprmtx.effectiveDate - 1.                         
                END.            
                LEAVE.  
            END.                              
        END.                                        
    END.
END PROCEDURE.


PROCEDURE pExpirePriceMatrixByCust PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplProcess  AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOePrmtx.
    
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    
    EMPTY TEMP-TABLE ttOePrmtx.
    
    FOR EACH bf-oe-prmtx NO-LOCK 
        WHERE bf-oe-prmtx.company    EQ ipcCompany
          AND bf-oe-prmtx.cust-no    EQ ipcCustomer
          AND (bf-oe-prmtx.exp-date GT TODAY OR bf-oe-prmtx.exp-date EQ ?) :
        IF iplProcess THEN DO:
            FIND CURRENT bf-oe-prmtx EXCLUSIVE-LOCK NO-ERROR. 
            bf-oe-prmtx.exp-date = TODAY.
        END.    
        ELSE DO:
            CREATE ttOePrmtx.
            ASSIGN 
                ttOePrmtx.company       = bf-oe-prmtx.company
                ttOePrmtx.custNo        = bf-oe-prmtx.cust-no
                ttOePrmtx.itemID        = bf-oe-prmtx.i-no
                ttOePrmtx.custShipId    = bf-oe-prmtx.custShipId
                ttOePrmtx.custype       = bf-oe-prmtx.custype
                ttOePrmtx.procat        = bf-oe-prmtx.procat
                ttOePrmtx.effectiveDate = bf-oe-prmtx.eff-date
                ttOePrmtx.oldExpiryDate = bf-oe-prmtx.exp-date
                ttOePrmtx.newExpiryDate = TODAY
                ttOePrmtx.Price1        = bf-oe-prmtx.price[1]
                ttOePrmtx.Price2        = bf-oe-prmtx.price[2] 
                ttOePrmtx.Price3        = bf-oe-prmtx.price[3] 
                ttOePrmtx.Price4        = bf-oe-prmtx.price[4] 
                ttOePrmtx.Price5        = bf-oe-prmtx.price[5] 
                ttOePrmtx.Price6        = bf-oe-prmtx.price[6] 
                ttOePrmtx.Price7        = bf-oe-prmtx.price[7] 
                ttOePrmtx.Price8        = bf-oe-prmtx.price[8] 
                ttOePrmtx.Price9        = bf-oe-prmtx.price[9] 
                ttOePrmtx.Price10       = bf-oe-prmtx.price[10] 
                ttOePrmtx.Quantity1     = bf-oe-prmtx.qty[1]
                ttOePrmtx.Quantity2     = bf-oe-prmtx.qty[2]
                ttOePrmtx.Quantity3     = bf-oe-prmtx.qty[3]
                ttOePrmtx.Quantity4     = bf-oe-prmtx.qty[4]
                ttOePrmtx.Quantity5     = bf-oe-prmtx.qty[5]
                ttOePrmtx.Quantity6     = bf-oe-prmtx.qty[6]
                ttOePrmtx.Quantity7     = bf-oe-prmtx.qty[7]
                ttOePrmtx.Quantity8     = bf-oe-prmtx.qty[8]
                ttOePrmtx.Quantity9     = bf-oe-prmtx.qty[9]
                ttOePrmtx.Quantity10    = bf-oe-prmtx.qty[10]
                .            
        END.                            
    END.         

END PROCEDURE.

PROCEDURE pExpireQuoteByCust PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplProcess  AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteHd.
    
    DEFINE BUFFER bf-quotehd FOR quotehd.
    
    FOR EACH bf-quotehd NO-LOCK 
        WHERE bf-quotehd.company EQ ipcCompany
          AND bf-quotehd.cust-no EQ ipcCustomer
           AND (bf-quotehd.expiredate GT TODAY OR bf-quotehd.expiredate EQ ?):
        IF iplProcess THEN DO:
            FIND CURRENT bf-quotehd EXCLUSIVE-LOCK NO-ERROR.
            bf-quotehd.expiredate = TODAY.
        END.
        ELSE DO:
            CREATE ttQuoteHd.
            ASSIGN 
                ttQuoteHd.company       = bf-quotehd.company
                ttQuoteHd.custID        = bf-quotehd.cust-no
                ttQuoteHd.deliveryDate  = bf-quotehd.del-date
                ttQuoteHd.estimate      = bf-quotehd.est-no 
                ttQuoteHd.loc           = bf-quotehd.loc 
                ttQuoteHd.newExpiryDate = TODAY 
                ttQuoteHd.oldExpiryDate = bf-quotehd.expireDate
                ttQuoteHd.quoteDate     = bf-quotehd.quo-date
                ttQuoteHd.quoteNo       = bf-quotehd.q-no
                .
        END.                       
    END.           
END PROCEDURE.

PROCEDURE Price_CheckPriceHoldForCustShip:
    /*------------------------------------------------------------------------------
     Purpose: Checks Price Hold for passed criteria.  Adds record to ttPriceHold table.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceHold AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceHoldActive AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cPriceHoldSetting     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPriceHoldSet         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyInRange           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lEffectiveDateAge     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iEffectiveDateAgeDays AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lQtyQuoted            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyAboveMin          AS LOGICAL   NO-UNDO.
    DEFINE BUFFER bf-oe-ord FOR oe-ord.

    
    RUN pGetPriceHoldCriteria(ipcCompany,ipcCustID,ipcShipID, OUTPUT oplPriceHoldActive, 
        OUTPUT oplPriceHold, OUTPUT lQtyInRange, OUTPUT lQtyMatch, OUTPUT lEffectiveDateAge, OUTPUT iEffectiveDateAgeDays, OUTPUT lQtyQuoted, OUTPUT lQtyAboveMin).
            
END PROCEDURE.

PROCEDURE Price_CheckPriceHoldForOrder:
    /*------------------------------------------------------------------------------
     Purpose: Given an oe-ord rowid, check all order lines to see if Price Hold criteria
     is met.  Return price hold.  
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrd AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplPrompt AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUpdateDB AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceHold AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPriceHoldReason AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE cPriceHoldSetting     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPriceHoldActive      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPriceHoldSet         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyInRange           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lEffectiveDateAge     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iEffectiveDateAgeDays AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lQtyQuoted            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyAboveMin          AS LOGICAL   NO-UNDO.
 
    FIND FIRST bf-oe-ord NO-LOCK 
        WHERE ROWID(bf-oe-ord) EQ ipriOeOrd
        NO-ERROR.
                     
    RUN pGetPriceHoldCriteria(bf-oe-ord.company,bf-oe-ord.cust-no,bf-oe-ord.ship-id, OUTPUT lPriceHoldActive,
        OUTPUT lPriceHoldSet, OUTPUT lQtyInRange, OUTPUT lQtyMatch, OUTPUT lEffectiveDateAge, OUTPUT iEffectiveDateAgeDays, OUTPUT lQtyQuoted, OUTPUT lQtyAboveMin).
    IF NOT lPriceHoldActive THEN 
    DO:
        ASSIGN 
            oplPriceHold       = NO
            opcPriceHoldReason = "OEPriceHold Not Activated"
            .
        RETURN.
    END.
    IF NOT lPriceHoldSet THEN 
    DO:
        ASSIGN 
            oplPriceHold       = NO
            opcPriceHoldReason = "OEPriceHold Not Configured for Customer & Ship"
            .
        RETURN.
    END.
    IF AVAILABLE bf-oe-ord THEN 
    DO:
        EMPTY TEMP-TABLE ttPriceHold.
        FOR EACH bf-oe-ordl OF bf-oe-ord WHERE bf-oe-ordl.i-no NE "" NO-LOCK:

            RUN pAddPriceHold(ROWID(bf-oe-ordl), bf-oe-ordl.company, bf-oe-ordl.est-no, bf-oe-ordl.i-no, bf-oe-ordl.cust-no, bf-oe-ordl.ship-id, bf-oe-ordl.qty,
                lQtyMatch, lQtyInRange, lEffectiveDateAge, iEffectiveDateAgeDays, lQtyQuoted, lQtyAboveMin).
        END.
    END.
    FIND FIRST ttPriceHold NO-LOCK
        WHERE ttPriceHold.lPriceHold
        NO-ERROR.
    IF AVAILABLE ttPriceHold THEN
        ASSIGN 
            oplPriceHold       = YES
            opcPriceHoldReason = ttPriceHold.cPriceHoldReason
            .
    ELSE 
        ASSIGN 
            oplPriceHold       = NO
            opcPriceHoldReason = ""
            .
    IF iplPrompt AND oplPriceHold THEN 
        RUN oe/dPriceHoldPrompt.w (INPUT TABLE ttPriceHold).
    IF iplUpdateDB THEN 
    DO:
        FIND FIRST bf-oe-ord EXCLUSIVE-LOCK 
            WHERE ROWID(bf-oe-ord) EQ ipriOeOrd
            NO-ERROR.
        IF AVAILABLE bf-oe-ord THEN 
            ASSIGN          
                bf-oe-ord.priceHold       = oplPriceHold
                bf-oe-ord.priceHoldReason = opcPriceHoldReason
                .
        FIND CURRENT bf-oe-ord NO-LOCK.
    END.
    RELEASE bf-oe-ord.
    
END PROCEDURE.

PROCEDURE Price_CheckPriceHoldForOrderReturnTT:
/*------------------------------------------------------------------------------
 Purpose: Wrapper of Price_CheckPriceHoldForOrder that returns the Temp-table to the
 caller.
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrd AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplPrompt AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUpdateDB AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceHold AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPriceHoldReason AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttPriceHold.
    
   RUN Price_CheckPriceHoldForOrder(ipriOeOrd, iplPrompt, iplUpdateDB, OUTPUT oplPriceHold, OUTPUT opcPriceHoldReason). 

END PROCEDURE.

PROCEDURE Price_CheckPriceMatrix:
    /*------------------------------------------------------------------------------
     Purpose:  Performs Check based on OEPriceMatrixCheck NK1
        Returns information to prompt or not and to block entry to just warn
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  /*Ship to Scope of Customer  - optional*/
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO. /*to get price matrix appropriate for qty*/
    DEFINE INPUT PARAMETER ipdPrice AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPrompt AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplBlockEntry AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
        
    DEFINE VARIABLE lMatrixFound   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLevel         AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE lCheckActive   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAllowMax      AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE cCheckCriteria AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lLessThanMax   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lBlockEntry    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dPriceMtx      AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE cPriceUOM      AS CHARACTER NO-UNDO.
    
    RUN pGetOEPriceMatrixCheckSettings(ipcCompany, OUTPUT oplPrompt, OUTPUT lAllowMax, OUTPUT oplBlockEntry).
    IF oplPrompt THEN 
    DO:
        RUN pSetBuffers(ipcCompany, ipcFGItemID, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).
        RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, OUTPUT lMatrixFound, OUTPUT cMessage).
        IF lMatrixFound THEN 
        DO:
            RUN pGetQtyMatchInfo(BUFFER bf-oe-prmtx, ipdQuantity, 0, OUTPUT iLevel, OUTPUT lQtyMatch).
            RUN pGetPriceAtLevel(BUFFER bf-oe-prmtx, iLevel, bf-itemfg.sell-price, bf-itemfg.sell-uom, OUTPUT dPriceMtx, OUTPUT cPriceUOM).
            IF dPriceMtx NE ipdPrice THEN 
                opcMessage = cMessage + " but price should be " + STRING(dPriceMtx) + " not " + STRING(ipdPrice).
            ELSE 
            DO:                
                IF NOT lQtyMatch THEN 
                DO:
                    IF bf-oe-prmtx.qty[iLevel] GE 99999999 AND lAllowMax THEN 
                        ASSIGN 
                            opcMessage    = cMessage + " and Quantity of " + STRING(ipdQuantity) + " matched at max level"
                            oplPrompt     = NO
                            oplBlockEntry = NO.
                    ELSE 
                        ASSIGN 
                            opcMessage = cMessage + " but Quantity of " + STRING(ipdQuantity) + " not matched at any level". 
                END.    
                ELSE 
                    ASSIGN 
                        opcMessage    = cMessage + " and Quantity of " + STRING(ipdQuantity) + " matched at level " + string(iLevel)
                        oplPrompt     = NO
                        oplBlockEntry = NO.
            END.
        END.
        ELSE 
            opcMessage = cMessage.
    END.
    ELSE 
        ASSIGN 
            oplPrompt     = NO
            opcMessage    = "OEPriceMatrixCheck not active"
            oplBlockEntry = NO.
       

END PROCEDURE.

PROCEDURE Price_CalculateLinePrice:
    /*------------------------------------------------------------------------------
     Purpose: Given an order line rowid, determine appropriate price from price matrix
     Can also pass FG Item ID and Qty since these may not be saved to the oe-ordl yet
     Notes: originally oe-price.i - replaces oe-price.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO. /*main rowid of order or invoice line*/ 
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUpdateDB AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMatrixExists AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcPriceUOM AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE cType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lReprice AS LOGICAL   NO-UNDO.

    /*Build the ttItemLines table - will only be one record if not auto-reprice - only FG Items of "Stock"*/
    RUN pBuildLineTable(ipriLine, ipcFGITemID, ipcCustID, ipcShipID, ipdQuantity, OUTPUT cType, OUTPUT lReprice).
    
    /*Build table that holds total quantity for each common FG Item Class amongst siblings,if cust.auto-reprice*/
    IF lReprice THEN RUN pBuildClassQuantityTable.  
    
    /*Go through each item line that is i-code "S" (including primary)*/ 
    FOR EACH ttItemLines:
        IF fUseLastPrice(ttItemLines.cCompany) THEN /*NK1 LastPric Rules*/
        DO:
            RUN pGetLastPrice(ttItemLines.riLine, 
                ttItemLines.cCompany, 
                ttItemLines.cFGItemID, 
                INPUT-OUTPUT ttItemLines.dPrice, 
                INPUT-OUTPUT ttItemLines.cPriceUOM).
        END.
        
        /*Continue to Reprice based on price matrix*/
        IF lReprice THEN /*Cust.auto-reprice*/
        DO:
            /*Get combined quantity for item class*/ 
            FIND FIRST ttFGItemClass NO-LOCK 
                WHERE ttFGItemClass.cClassID EQ ttItemLines.cFGItemClass
                NO-ERROR.
            IF AVAILABLE ttFGItemClass THEN 
                ttItemLines.dQuantityLookup = ttFGItemClass.dClassQuantity.
        END.
        IF ttItemLines.dQuantityLookup EQ 0 THEN 
            ttItemLines.dQuantityLookup = ttItemLines.dQuantity.
        
        /*Get Price from Matching Price Matrix (note if no match found, price not changed*/    
        RUN Price_GetPriceMatrixPriceSimple (ttItemLines.cCompany,
            ttItemLines.cFGItemID, 
            ttItemLines.cCustID, 
            ttItemLines.cShipID,
            ttItemLines.dQuantityLookup,  
            OUTPUT ttItemLines.lMatrixExists, 
            INPUT-OUTPUT ttItemLines.dPrice, 
            INPUT-OUTPUT ttItemLines.cPriceUOM ).    
                
        IF ttItemLines.lIsPrimary THEN 
            ASSIGN 
                oplMatrixExists = ttItemLines.lMatrixExists
                iopdPrice       = ttItemLines.dPrice
                iopcPriceUOM    = ttItemLines.cPriceUOM
                .
        RUN Conv_CalcTotalPrice (
            ttItemLines.cCompany,
            ttItemLines.cFGItemID,
            ttItemLines.dQuantity,
            ttItemLines.dPrice,
            ttItemLines.cPriceUOM,
            ttItemLines.dDiscount,
            ttItemLines.iCaseCount,
            OUTPUT ttItemLines.dPriceTotal).   
    END.
    IF iplUpdateDB THEN 
    DO:
        /*Assign order from ttItemLines*/
        FOR EACH ttItemLines NO-LOCK 
            WHERE ttItemLines.cTableType EQ "oe-ordl":
            FIND FIRST oe-ordl EXCLUSIVE-LOCK 
                WHERE ROWID(oe-ordl) EQ ttItemLines.riLine
                NO-ERROR.
            IF AVAILABLE oe-ordl THEN 
            DO: 
                ASSIGN
                    oe-ordl.price   = ttItemLines.dPrice
                    oe-ordl.pr-uom  = ttItemLines.cPriceUOM
                    oe-ordl.t-price = ttItemLines.dPriceTotal
                    .
                RELEASE oe-ordl.
            END.
        END.
        FOR EACH ttItemLines NO-LOCK 
            WHERE ttItemLines.cTableType EQ "inv-line":
            FIND FIRST inv-line EXCLUSIVE-LOCK 
                WHERE ROWID(inv-line) EQ ttItemLines.riLine
                NO-ERROR.
            IF AVAILABLE inv-line THEN 
            DO: 
                ASSIGN
                    inv-line.price   = ttItemLines.dPrice
                    inv-line.pr-uom  = ttItemLines.cPriceUOM
                    inv-line.t-price = ttItemLines.dPriceTotal
                    .
                RELEASE inv-line.
            END.
        END.
    END. /*iplUpdateDB*/

END PROCEDURE.

PROCEDURE Price_ExpireOldPrice:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustNo         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustType       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcProcat         AS CHARACTER NO-UNDO.
    
    RUN pExpireOldPrices(
        INPUT YES, 
        INPUT ipcCompany,
        INPUT ipcItemID,
        INPUT ipcShipID,
        INPUT ipcCustNo,
        INPUT ipcCustType,
        INPUT ipcProcat,
        INPUT-OUTPUT TABLE ttOePrmtxCsv
        ).
    
END PROCEDURE.

PROCEDURE Price_ExpireOldPriceTT:
/*------------------------------------------------------------------------------
 Purpose: 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustNo         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustType       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcProcat         AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOePrmtxCsv.
    
    EMPTY TEMP-TABLE ttOePrmtxCsv.
    
    RUN pExpireOldPrices(
        INPUT NO, 
        INPUT ipcCompany,
        INPUT ipcItemID,
        INPUT ipcShipID,
        INPUT ipcCustNo,
        INPUT ipcCustType,
        INPUT ipcProcat,
        INPUT-OUTPUT TABLE ttOePrmtxCsv
        ).
END PROCEDURE.

PROCEDURE Price_ExpirePriceMatrixByCust:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    
    RUN pExpirePriceMatrixByCust(
        INPUT ipcCompany,
        INPUT ipcCustomer,
        INPUT YES,
        INPUT-OUTPUT TABLE ttOePrmtx
        ).


END PROCEDURE.

PROCEDURE Price_ExpirePriceMatrixByCustTT:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOePrmtx.
    
    RUN pExpirePriceMatrixByCust(
        INPUT ipcCompany,
        INPUT ipcCustomer,
        INPUT NO,
        INPUT-OUTPUT TABLE ttOePrmtx
        ).

END PROCEDURE.

PROCEDURE Price_ExpireQuoteByCust:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    
    RUN pExpireQuoteByCust(
        INPUT ipcCompany,
        INPUT ipcCustomer,
        INPUT YES,
        INPUT-OUTPUT TABLE ttQuoteHd
        ).


END PROCEDURE.

PROCEDURE Price_ExpireQuoteByCustTT:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteHd.
    
    RUN pExpireQuoteByCust(
        INPUT ipcCompany,
        INPUT ipcCustomer,
        INPUT NO,
        INPUT-OUTPUT TABLE ttQuoteHd
        ).

END PROCEDURE.

PROCEDURE Price_GetPriceMatrix:
    /*------------------------------------------------------------------------------
     Purpose: Returns a Rowid of a valid price matrix, given 3 key inputs
     Notes: Replaces oe/GetPriceMatrix.p - NOTE
     Prior versions of the price matrix captured 108 characters in the .i-no field and
     characters 101-108 where converted to the "Effective Date".  This should be converted
     to use the true .eff-date field on the price matrix.  The assumption of this program
     is that this conversion has taken place and that the price matrix .i-no field is 
     equivalent to the itemfg.i-no field.
     
     Syntax Example:
    RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
    RUN Price_GetPriceMatrix in hdPriceProcs (cocode, oe-ordl.i-no, oe-ord.cust-no, oe-ord.ship-id,
                                        OUTPUT riMatrix, OUTPUT lFound, OUTPUT cMessage).
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany     AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT        PARAMETER ipcFGItemID    AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT        PARAMETER ipcCustID      AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT        PARAMETER ipcShipID      AS CHARACTER NO-UNDO.  /*Ship to Scope of Customer  - optional*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttPriceMatrix.              /*Outputs the rowid for the price matrix that matches*/
    DEFINE OUTPUT       PARAMETER oplMatchFound  AS LOGICAL NO-UNDO.    /*Logical that can determine if find on rowid should be done*/
    DEFINE OUTPUT       PARAMETER opcMatchDetail AS CHARACTER NO-UNDO.  /*Clarifies the match criteria or failure to match*/
    
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.

    RUN pSetBuffers(ipcCompany, ipcFGItemId, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).

    /*Find match given buffers */  
    RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, OUTPUT oplMatchFound, OUTPUT opcMatchDetail).

END PROCEDURE.

PROCEDURE Price_GetPriceMatrixLevel:
    /*------------------------------------------------------------------------------
     Purpose: Returns a price level based on matrix information provided
     Notes:
         Syntax Example:
    RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
    RUN Price_GetPriceMatrixLevel in hdPriceProcs (cocode, itemfg.i-no, cust.cust-no, shipto.ship-id,
                                             oe-ordl.qty,
                                             OUTPUT iLevel).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  /*Ship to Scope of Customer  - optional*/
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO. /*to get price matrix appropriate for qty*/
    DEFINE OUTPUT PARAMETER opiLevel AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
    
    DEFINE VARIABLE lMatrixFound AS LOGICAL.
    DEFINE VARIABLE lQtyMatch    AS LOGICAL.
    DEFINE VARIABLE cMessage     AS CHARACTER. 
    
    RUN pSetBuffers(ipcCompany, ipcFGItemId, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).
    RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, OUTPUT lMatrixFound, OUTPUT cMessage).
    RUN pGetQtyMatchInfo(BUFFER bf-oe-prmtx, ipdQuantity, 1, OUTPUT opiLevel, OUTPUT lQtyMatch).

END PROCEDURE.

PROCEDURE Price_GetPriceMatrixPrice:
    /*------------------------------------------------------------------------------
     Purpose: Returns a Price and Price UOM, given Item, Customer and ShipTo criteria.
     Also allows for override of a 
     Notes:
    Syntax Example:
    RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
    RUN Price_GetPriceMatrixPrice in hdPriceProcs (cocode, oe-ordl.i-no, oe-ord.cust-no, oe-ord.ship-id,
                                             oe-ordl.qty, 0,
                                             OUTPUT lMatrixMatchFound, OUTPUT cMatrixMatchMessage,
                                             OUTPUT oe-ordl.price, OUTPUT oe-ordl.pr-uom, 
                                             OUTPUT lQtyMatchFound, OUTPUT lQtyWithinMatrixRange).
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  /*Ship to Scope of Customer  - optional*/
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO. /*to get price matrix appropriate for qty*/
    DEFINE INPUT PARAMETER ipiLevelOverride AS INTEGER NO-UNDO.  /*for use to get specific price level from matrix - set to 0 to determine based on qty*/

    /*Outputs*/
    DEFINE OUTPUT PARAMETER oplMatrixMatchFound AS LOGICAL NO-UNDO.  /*Logical that specifies if a matrix was found, at all*/
    DEFINE OUTPUT PARAMETER opcMatrixMatchDetail AS CHARACTER NO-UNDO.  /*Clarifies the match criteria or failure to match*/
    DEFINE INPUT-OUTPUT PARAMETER iopdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcUom AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyDistinctMatch AS LOGICAL NO-UNDO.  /*match on exact quantity*/
    DEFINE OUTPUT PARAMETER oplQtyWithinRange AS LOGICAL NO-UNDO. /*quantity within range*/

    /*main matrix buffer*/
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.

    DEFINE VARIABLE iLevel            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLevelStart       AS INTEGER   NO-UNDO.
    /*for calculation of discount method*/
    DEFINE VARIABLE dItemSellPrice    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cItemSellPriceUOM AS CHARACTER NO-UNDO.
    
    
    RUN pSetBuffers(ipcCompany, ipcFGItemId, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).
        
    /*use internal procedure to find the matching matrix*/
    RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, OUTPUT oplMatrixMatchFound, OUTPUT opcMatrixMatchDetail).
    
    IF NOT oplMatrixMatchFound OR  NOT AVAILABLE bf-oe-prmtx THEN RETURN.
    
    /*Set the default starting level to the customer specific starting level*/
    IF AVAILABLE bf-cust THEN 
        iLevelStart = MAXIMUM(1, bf-cust.cust-level).
    ELSE 
        iLevelStart = 1.
    
    IF AVAILABLE bf-itemfg THEN  /*Set the starting sell price and UOM if the matrix is discount method*/ 
        ASSIGN 
            dItemSellPrice    = bf-itemfg.sell-price
            cItemSellPriceUOM = bf-itemfg.sell-uom
            .       
             
    RUN pGetQtyMatchInfo(BUFFER bf-oe-prmtx, ipdQuantity, iLevelStart, OUTPUT iLevel, OUTPUT oplQtyDistinctMatch).
    IF iLevel GT 0 THEN 
        oplQtyWithinRange = YES.
    
    IF ipiLevelOverride NE 0 THEN 
    DO:
        IF iLevelStart LE ipiLevelOverride THEN
            iLevel = ipiLevelOverride.
        ELSE 
            iLevel = iLevelStart.
    END.
    IF oplQtyWithinRange THEN
        RUN pGetPriceAtLevel(BUFFER bf-oe-prmtx, iLevel, dItemSellPrice, cItemSellPriceUom, OUTPUT iopdPrice, OUTPUT iopcUom).
    ELSE 
        ASSIGN 
            oplMatrixMatchFound  = NO
            opcMatrixMatchDetail = opcMatrixMatchDetail + " but price level " + STRING(iLevel) + " not valid."
            .

END PROCEDURE.

PROCEDURE Price_GetPriceMatrixPriceSimple:
    /*------------------------------------------------------------------------------
        Purpose: Simpler version of GetPriceMatrixPrice with wrapper and defined variables
        Notes:
       Syntax Example:
       RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
       RUN Price_GetPriceMatrixPriceSimple in hdPriceProcs (cocode, oe-ordl.i-no, oe-ord.cust-no,
                                                      oe-ordl.qty,
                                                       OUTPUT lMatrixMatchFound,
                                                       OUTPUT oe-ordl.price, OUTPUT oe-ordl.pr-uom).
           
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  /*ShipID */
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO. /*to get price matrix appropriate for qty*/
    
    /*Outputs*/
    DEFINE OUTPUT PARAMETER oplMatrixMatchFound AS LOGICAL NO-UNDO.  /*Logical that specifies if a matrix was found, at all*/
    DEFINE INPUT-OUTPUT PARAMETER iopdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcUom AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lQtyMatchFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyWithinMatrixRange AS LOGICAL   NO-UNDO.
    
    RUN Price_GetPriceMatrixPrice(ipcCompany, ipcFGITemID, ipcCustID, ipcShipID, ipdQuantity, 0,
        OUTPUT oplMatrixMatchFound, OUTPUT cMessage, 
        INPUT-OUTPUT iopdPrice, INPUT-OUTPUT iopcUOM, 
        OUTPUT lQtyMatchFound, OUTPUT lQtyWithinMatrixRange).

END PROCEDURE.

PROCEDURE pAddLineTableItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds a line table Item to temp-table
     Notes: Agnostic of data type.
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg. 
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplIsPrimary AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcPriceUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDiscount AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableType AS CHARACTER NO-UNDO.
    
    FIND FIRST ttItemLines
        WHERE ttItemLines.riLine EQ ipriLine
        NO-ERROR.
    IF NOT AVAILABLE ttItemLines THEN 
    DO:
        CREATE ttItemLines.
        ASSIGN 
            ttItemLines.riLine       = ipriLine
            ttItemLines.lIsPrimary   = iplIsPrimary
            ttItemLines.cCompany     = ipbf-itemfg.company
            ttItemLines.cFGItemID    = ipcFGItemID
            ttItemLines.cCustID      = ipcCustID
            ttItemLines.cShipID      = ipcShipID
            ttItemLines.cFGItemClass = ipbf-itemfg.class
            ttItemLines.dQuantity    = ipdQuantity
            ttItemLines.dPrice       = ipdPrice
            ttItemLines.cPriceUOM    = ipcPriceUOM
            ttItemLines.iCaseCount   = ipbf-itemfg.case-count
            ttItemLines.dDiscount    = ipdDiscount
            ttItemLines.cTableType   = ipcTableType
            .
    END.

END PROCEDURE.

PROCEDURE pAddPriceHold PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds a price hold record, given subject parameters and settings 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iplQuantityMatch AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplQuantityInRange AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplEffectiveDateAge AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiEffectiveDateAgeDays AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplQuantityQuoted AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplQtyAboveMin AS LOGICAL NO-UNDO. 

    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    DEFINE VARIABLE lFoundQuoteForQty AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lFoundAnyQuote AS LOGICAL NO-UNDO.

    CREATE ttPriceHold.
    ASSIGN 
        ttPriceHold.riLine    = ipriLine
        ttPriceHold.cFGItemID = ipcFGItemID
        ttPriceHold.cCustID   = ipcCustID
        ttPriceHold.cShipID   = ipcShipID
        ttPriceHold.dQuantity = ipdQuantity
        .
                       
    RUN pSetBuffers(ipcCompany, ipcFGItemID, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).            
    IF NOT AVAILABLE bf-itemfg THEN  
    DO:
        ASSIGN 
            ttPriceHold.lPriceHold       = YES
            ttPriceHold.cPriceHoldDetail = ttPriceHold.cFGItemID + " is invalid"
            ttPriceHold.cPriceHoldReason = "Invalid FG Item"
            .
        RETURN.
    END.           
    IF ipcEstNo EQ "" THEN 
    DO:  /*use internal procedure to find the matching matrix*/  
        RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, 
            OUTPUT ttPriceHold.lMatrixMatch, OUTPUT ttPriceHold.cMatrixMatch).
        IF NOT ttPriceHold.lMatrixMatch OR  NOT AVAILABLE bf-oe-prmtx THEN 
        DO:
            ASSIGN 
                ttPriceHold.lPriceHold       = YES
                ttPriceHold.cPriceHoldDetail = ttPriceHold.cMatrixMatch
                ttPriceHold.cPriceHoldReason = "No matrix found".
            RETURN.           
        END.
        
        /*Test Effective Date Age if activated*/
        ttPriceHold.dtEffectiveDate = bf-oe-prmtx.eff-date.
        IF NOT ttPriceHold.lPriceHold AND iplEffectiveDateAge AND (TODAY - bf-oe-prmtx.eff-date) GT ipiEffectiveDateAgeDays THEN 
        DO:
            ASSIGN 
                ttPriceHold.lPriceHold           = YES
                ttPriceHold.lEffectiveDateTooOld = YES 
                ttPriceHold.cPriceHoldDetail     = "Price found in Matrix but Effective date of " + STRING(bf-oe-prmtx.eff-date,"99/99/9999") + " older than " + STRING(ipiEffectiveDateAgeDays) + " days"
                ttPriceHold.cPriceHoldReason     = "Eff. date too old".           .
        END. 
        
        /*Test mini order qty*/        
        IF NOT ttPriceHold.lPriceHold AND iplQtyAboveMin AND ipdQuantity LT bf-oe-prmtx.minOrderQty THEN 
        DO:
            ASSIGN 
                ttPriceHold.lPriceHold           = YES                
                ttPriceHold.cPriceHoldDetail     = "Order quantity " + STRING(ipdQuantity) + " below minimum order quantity " + STRING(bf-oe-prmtx.minOrderQty)
                ttPriceHold.cPriceHoldReason     = "Order quantity below minimum order quantity".          
        END. 
        
        /*Test Price Level if activated*/
        IF NOT ttPriceHold.lPriceHold THEN 
            RUN pGetQtyMatchInfo(BUFFER bf-oe-prmtx, ipdQuantity, 0, OUTPUT ttPriceHold.iQuantityLevel, OUTPUT ttPriceHold.lQuantityMatch).
            
        IF NOT ttPriceHold.lPriceHold AND iplQuantityMatch AND NOT ttPriceHold.lQuantityMatch THEN 
        DO:
            ASSIGN 
                ttPriceHold.lPriceHold       = YES
                ttPriceHold.cPriceHoldDetail = "Price found in Matrix but no distinct price level found for quantity of " + STRING(ipdQuantity)  
                ttPriceHold.cPriceHoldReason = "Quantity not matched".
        END.
        
        IF NOT ttPriceHold.lPriceHold AND iplQuantityInRange AND ttPriceHold.iQuantityLevel EQ 0 THEN 
        DO:
            ASSIGN 
                ttPriceHold.lPriceHold          = YES
                ttPriceHold.lQuantityOutOfRange = YES 
                ttPriceHold.cPriceHoldDetail    = "Quantity of " + STRING(ipdQuantity) + " out of range of price matrix levels"
                ttPriceHold.cPriceHoldReason    = "Quantity not in range".
        END.     
            
    END.
    ELSE /*QtyQuoted for custom items/estimated & quoted*/
    DO:
        IF iplQuantityQuoted THEN 
        DO:
            RUN pFindQuoteForQuantity(ipcCompany, ipcEstNo, ipcFGItemID, bf-itemfg.part-no, ipdQuantity, OUTPUT lFoundAnyQuote, OUTPUT lFoundQuoteForQty).
            IF NOT lFoundAnyQuote THEN DO: 
                ASSIGN
                    ttPriceHold.lPriceHold       = YES
                    ttPriceHold.cPriceHoldDetail = "Estimate # " + ipcEstNo + " does not have a valid quote available."
                    ttPriceHold.cPriceHoldReason = "No Quote for Estimate"
                    .
            END.                    
            ELSE IF NOT lFoundQuoteForQty THEN 
                ASSIGN 
                    ttPriceHold.lPriceHold       = YES
                    ttPriceHold.cPriceHoldDetail = "Not quote found for Estimate # " + ipcEstNo + ", Item # " + ttPriceHold.cFGItemID + " with Quantity " + STRING(ipdQuantity)
                    ttPriceHold.cPriceHoldReason = "No Quote for Quantity"
                    .
        END.
    END.
    

END PROCEDURE.

PROCEDURE pBuildClassQuantityTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a RowID this will build a temp-table for FG Item Classes 
     and quantities
     Notes: Works for RI of Oe-ordl, inv-line or ar-invl
    ------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE ttFGItemClass.
    FOR EACH ttItemLines NO-LOCK:
        FIND FIRST ttFGItemClass EXCLUSIVE-LOCK  
            WHERE ttFGItemClass.cClassID EQ ttItemLines.cFGItemClass
            NO-ERROR.
        IF NOT AVAILABLE ttFGItemClass THEN 
        DO: 
            CREATE ttFGItemClass.
            ASSIGN 
                ttFGItemClass.cClassID = ttItemLines.cFGItemClass.
        END.
        ttFGItemClass.dClassQuantity = ttFGItemClass.dClassQuantity + ttItemLines.dQuantity.    
    END.    

END PROCEDURE.

PROCEDURE pBuildLineTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the agnostic temp-table for processing based on Row ID
     Notes: in addition to building temp-table, also returns itemfg and cust buffers
     and "type"
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcType AS CHARACTER NO-UNDO.  /*oe-ordl, ar-invl, inv-line*/ 
    DEFINE OUTPUT PARAMETER oplReprice AS LOGICAL NO-UNDO.
    DEF VAR lFound AS LOG NO-UNDO.
    DEF VAR cUseMatrix AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE ttItemLines.
    FIND FIRST oe-ordl NO-LOCK 
        WHERE ROWID(oe-ordl) EQ ipriLine
        NO-ERROR.
    FIND FIRST inv-line NO-LOCK
        WHERE ROWID(inv-line) EQ ipriLine
        NO-ERROR. 
    IF AVAILABLE oe-ordl THEN 
    DO:
        {oe/PriceProcsLineBuilder.i &HeaderTable="oe-ord" &LineTable="oe-ordl" &LineQuantity="qty"}
    END.
    ELSE IF AVAILABLE inv-line THEN 
        DO:
            {oe/PriceProcsLineBuilder.i &HeaderTable="inv-head" &LineTable="inv-line" &LineQuantity="inv-qty"}
        END.     

END PROCEDURE.

PROCEDURE pAddPriceMatrix PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: creates ttPriceMatrix records
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipToID    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiLevel       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdPrice       AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcPriceUOM    AS CHARACTER NO-UNDO.
            
    FIND FIRST ttPriceMatrix
         WHERE ttPriceMatrix.company  = ipcCompany
           AND ttPriceMatrix.itemID   = ipcItemID
           AND ttPriceMatrix.custID   = ipcCustID
           AND ttPriceMatrix.shiptoID = ipcShipToID
           AND ttPriceMatrix.level    = ipiLevel
         NO-ERROR.
    IF NOT AVAILABLE ttPriceMatrix THEN DO:
        CREATE ttPriceMatrix.
        ASSIGN
            ttPriceMatrix.company     = ipcCompany
            ttPriceMatrix.itemID      = ipcItemID
            ttPriceMatrix.custID      = ipcCustID
            ttPriceMatrix.shipToID    = ipcShipToID
            ttPriceMatrix.level       = ipiLevel
            ttPriceMatrix.quantity    = ipdQuantity
            ttPriceMatrix.quantityUOM = ipcQuantityUOM
            ttPriceMatrix.price       = ipdPrice
            ttPriceMatrix.priceUOM    = ipcPriceUOM
            .
    END. 
END PROCEDURE.

PROCEDURE pFindQuoteForQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an item and quantity, validate that a quote exists
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPartNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAnyQuoteFound AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQuoteFoundForQuantity AS LOGICAL NO-UNDO.

    FOR EACH quotehd NO-LOCK 
        WHERE quotehd.company EQ ipcCompany
        AND quotehd.est-no EQ ipcEstNo
        AND quotehd.quo-date LE TODAY 
        AND (quotehd.expireDate GT TODAY OR quotehd.expireDate EQ ?) 
        ,
        EACH quoteitm OF quotehd NO-LOCK 
        WHERE quoteitm.company EQ quotehd.company
        AND (quoteitm.i-no EQ ipcFGItemID
        OR quoteitm.part-no EQ ipcPartNo),
        EACH quoteqty NO-LOCK 
        WHERE quoteqty.company EQ quoteitm.company
        AND quoteqty.line EQ quoteitm.line
        AND quoteqty.loc EQ quoteitm.loc
        AND quoteqty.q-no EQ quoteitm.q-no
        :
        oplAnyQuoteFound = YES.
        IF ipdQuantity EQ quoteqty.qty THEN 
        DO:
            oplQuoteFoundForQuantity = YES.
            RETURN.
        END.
    END.   

END PROCEDURE.

PROCEDURE pGetLastPrice PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the "Last Price" logic - updates the parameter values only successful
     Agnostic to the rowid passed in (could be oe-ordl, inv-line, ar-invl)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcPriceUOM AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
    DEFINE BUFFER bf-ar-invl  FOR ar-invl.
    DEFINE BUFFER bf-ar-inv   FOR ar-inv.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-head FOR inv-head.
    
    DEFINE VARIABLE dtLast AS DATE NO-UNDO.

    dtLast = 01/01/0001.
    
    /*find last order line for the item*/
    FOR EACH bf-oe-ordl
        WHERE bf-oe-ordl.company EQ ipcCompany
        AND bf-oe-ordl.i-no    EQ ipcFGItemID
        AND ROWID(bf-oe-ordl)  NE ipriLine
        NO-LOCK,
        FIRST bf-oe-ord
        WHERE bf-oe-ord.company  EQ bf-oe-ordl.company
        AND bf-oe-ord.ord-no   EQ bf-oe-ordl.ord-no
        AND bf-oe-ord.cust-no  EQ bf-oe-ordl.cust-no
        AND bf-oe-ord.ord-date GT dtLast
        NO-LOCK
        BY bf-oe-ord.ord-date DESCENDING:
        LEAVE.
    END.
    IF AVAILABLE bf-oe-ordl THEN
        ASSIGN
            dtLast      = bf-oe-ord.ord-date
            opdPrice    = bf-oe-ordl.price
            opcPriceUOM = bf-oe-ordl.pr-uom.

    FOR EACH bf-ar-invl
        WHERE bf-ar-invl.company EQ ipcCompany
        AND bf-ar-invl.i-no    EQ ipcFGItemID
        AND ROWID(bf-ar-invl)  NE ipriLine
        NO-LOCK,
        FIRST bf-ar-inv
        WHERE bf-ar-inv.x-no     EQ bf-ar-invl.x-no
        AND bf-ar-inv.cust-no  EQ bf-ar-invl.cust-no
        AND bf-ar-inv.inv-date GT dtLast
        NO-LOCK
        BY bf-ar-inv.inv-date:
        LEAVE.
    END.
    IF AVAILABLE bf-ar-invl THEN
        ASSIGN
            dtLast      = bf-ar-inv.inv-date
            opdPrice    = bf-ar-invl.unit-pr
            opcPriceUOM = bf-ar-invl.pr-qty-uom.

    FOR EACH bf-inv-line
        WHERE bf-inv-line.company EQ ipcCompany
        AND bf-inv-line.i-no    EQ ipcFGItemID
        AND ROWID(bf-inv-line)    NE ipriLine
        NO-LOCK,
        FIRST bf-inv-head
        WHERE bf-inv-head.r-no     EQ bf-inv-line.r-no
        AND bf-inv-head.cust-no  EQ bf-inv-line.cust-no
        AND bf-inv-head.inv-date GT dtLast
        NO-LOCK
        BY bf-inv-head.inv-date:
        LEAVE.
    END.
    IF AVAILABLE bf-inv-line THEN
        ASSIGN
            dtLast      = bf-inv-head.inv-date
            opdPrice    = bf-inv-line.price
            opcPriceUOM = bf-inv-line.pr-uom.

END PROCEDURE.

PROCEDURE pGetOEPriceMatrixCheckSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*------------------------------------------------------------------------------
     Purpose: Returns Price hold Criteria Settings
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCheckMatrix AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAllowMax AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplBlockEntry AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO. 
   
    
    RUN sys/ref/nk1look.p (ipcCompany, 
        "OEPriceMatrixCheck",
        "L" /* Logical */,
        NO /* check by cust */,
        YES /* use cust not vendor */,
        "" /* cust */,
        "" /* ship-to*/,
        OUTPUT cReturn, 
        OUTPUT lFound).
    oplCheckMatrix = (lFound AND cReturn EQ "YES").
    IF oplCheckMatrix THEN 
    DO:
        RUN sys/ref/nk1look.p (ipcCompany,
            "OEPriceMatrixCheck", 
            "I" /* Logical */,
            NO /* check by cust */,
            YES /* use cust not vendor */,
            "" /* cust */,
            "" /* ship-to*/,
            OUTPUT cReturn,
            OUTPUT lFound).
        oplAllowMax = (lFound AND INT(cReturn) EQ 1).
        RUN sys/ref/nk1look.p (ipcCompany, 
            "OEPriceMatrixCheck", 
            "C" /* Logical */,
            NO /* check by cust */,
            YES /* use cust not vendor */,
            "" /* cust */,
            "" /* ship-to*/,
            OUTPUT cReturn,
            OUTPUT lFound).
        oplBlockEntry =  (lFound AND cReturn EQ "Block Entry").
    END.
END PROCEDURE.

PROCEDURE pGetPriceAtLevel PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Accepts a matrix buffer and level (and starting sell price or uom if discount method)
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-prmtx FOR oe-prmtx.
    DEFINE INPUT PARAMETER ipiLevel AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdItemSellPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemSellPriceUom AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPrice AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUom AS CHARACTER NO-UNDO.

    IF ipiLevel GT 0 AND ipiLevel LE EXTENT(ipbf-oe-prmtx.price) THEN 
    DO:  /*31620 - protect against a level request out of range of the array*/
           
        IF ipbf-oe-prmtx.meth THEN
            ASSIGN 
                opdPrice = ipbf-oe-prmtx.price[ipiLevel]
                opcUom   = ipbf-oe-prmtx.uom[ipiLevel].
        ELSE /*discount method - discount off of item price*/
            ASSIGN 
                opdPrice = ipdItemSellPrice - 
                ROUND((ipdItemSellPrice * ipbf-oe-prmtx.discount[ipiLevel]) / 100, 2)
                opcUom   = ipcItemSellPriceUom.
            
    END.

END PROCEDURE.

PROCEDURE pGetPriceMatrix PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Private Buffer parameter based call to Get the Price Matrix
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-itemfg   FOR itemfg.
    DEFINE PARAMETER BUFFER ipbf-cust     FOR cust.
    DEFINE PARAMETER BUFFER opbf-oe-prmtx FOR oe-prmtx.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMatchFound AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMatchDetail AS CHARACTER NO-UNDO.
 
    DEFINE BUFFER bf-shipto FOR shipto.
  
    DEFINE VARIABLE cMsgItemFG     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgItemProcat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgCustID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgCustType   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgShipID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cMsgBlankInd   AS CHARACTER NO-UNDO INIT "[blank]".
    DEFINE VARIABLE lMatrixFound   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lMtxForNonstock AS LOGICAL  NO-UNDO.
             
    IF NOT AVAILABLE ipbf-itemfg THEN 
    DO:
        ASSIGN 
            opcMatchDetail = "Invalid FG Item ID"
            oplMatchFound  = NO
            .
        RETURN. 
    END. 
    IF NOT AVAILABLE ipbf-cust THEN 
    DO:
        ASSIGN 
            opcMatchDetail = "Invalid Cust ID"
            oplMatchFound  = NO 
            .
        RETURN.
    END.
    
    lMtxForNonstock = fGetNk1OEUseMatrixForNonstock(ipbf-cust.company).
    
    IF ipcShipID NE "" THEN 
    DO:
        FIND FIRST bf-shipto NO-LOCK 
            WHERE bf-shipto.company EQ ipbf-cust.company
            AND bf-shipto.cust-no EQ ipbf-cust.cust-no
            AND bf-shipto.ship-id EQ ipcShipID
            NO-ERROR.
        IF NOT AVAILABLE bf-shipto THEN 
        DO:
            ASSIGN 
                opcMatchDetail = "Ship To ID provided is not valid for Cust " + ipbf-cust.cust-no
                oplMatchFound  = NO 
                .
            RETURN.
        END.
    END.
    IF ipbf-itemfg.i-code NE "S" THEN 
    DO:
        /* Use matrix for non-stock items ONLY if NK1 "OEUseMatrixForNonstock" logical eq true 
        Also referenced in oe/PriceProcsLineBuilder.i   */         
        IF lMtxForNonstock THEN.
        ELSE DO:
            ASSIGN 
                opcMatchDetail = "This FG item is configured as a non-inventoried (Not stocked) item"
                oplMatchFound  = NO 
                .
            RETURN.
        END.
    END.

    /* To improve performance of the query, the query to find the matrix is split into two queries.
       The first one will run the query with item match and the other one with blank item # */    
    /*Find match */  
    FOR EACH opbf-oe-prmtx NO-LOCK 
        WHERE opbf-oe-prmtx.company EQ ipbf-itemfg.company
        /*Match on item # */
        AND opbf-oe-prmtx.i-no EQ ipbf-itemfg.i-no
        /*Match on cust-no or match matrix with blank cust-no*/
        AND (opbf-oe-prmtx.cust-no EQ ipbf-cust.cust-no OR opbf-oe-prmtx.cust-no EQ "")
        /*Match on customer type or match on blank customer type - disregard this criteria if the matrix has a customer number at which
        point only the customer number match matters.  Cust Type only applicable when customer number is blank.
        This allows for customer type to change in AF1 without having to update the Price Matrix type for that customer.*/
        AND (opbf-oe-prmtx.custype EQ ipbf-cust.type OR opbf-oe-prmtx.custype EQ "" OR opbf-oe-prmtx.cust-no NE "")
        /*Match on product category or match on blank product category - disregard this criteria if the matrix has a FG Item # at which
        point only the FG Item # match matters.  Product Category only applicable when customer number is blank.
        This allows for Product Category to change in IF1 without having to update the Price Matrix for that item.*/
        AND (opbf-oe-prmtx.procat EQ ipbf-itemfg.procat OR opbf-oe-prmtx.procat EQ "" OR opbf-oe-prmtx.i-no NE "")
        /*Match on ship ID or match matrix with blank ship ID*/
        AND (opbf-oe-prmtx.custShipID EQ ipcShipID OR opbf-oe-prmtx.custShipID EQ "")
        /*Must be effecitve*/
        AND (opbf-oe-prmtx.eff-date LE TODAY)
        /*must not be expired*/
        AND (opbf-oe-prmtx.exp-date GE TODAY OR opbf-oe-prmtx.exp-date EQ ?)
        /* Can't be all blank */
        AND NOT (opbf-oe-prmtx.cust-no EQ "" AND opbf-oe-prmtx.i-no EQ "" AND opbf-oe-prmtx.procat EQ "" AND opbf-oe-prmtx.custype EQ "" 
        AND opbf-oe-prmtx.custShipID EQ "")
    /*Sort the resulting data set so that actual matches take priority over blank matches*/
        BY opbf-oe-prmtx.cust-no DESCENDING 
        BY opbf-oe-prmtx.procat DESCENDING 
        BY opbf-oe-prmtx.custype DESCENDING 
        BY opbf-oe-prmtx.custShipID DESCENDING
        BY opbf-oe-prmtx.eff-date DESCENDING 
        :
        lMatrixFound = TRUE.
        LEAVE.  /*After first/best match, leave*/
    END.
    
    /* Find the matrix only if the above query failed to return a record */
    IF NOT lMatrixFound THEN DO:
        FOR EACH opbf-oe-prmtx NO-LOCK 
            WHERE opbf-oe-prmtx.company EQ ipbf-itemfg.company
            /* Match matrix with blank item #*/
            AND opbf-oe-prmtx.i-no EQ ""
            /*Match on cust-no or match matrix with blank cust-no*/
            AND (opbf-oe-prmtx.cust-no EQ ipbf-cust.cust-no OR opbf-oe-prmtx.cust-no EQ "")
            /*Match on customer type or match on blank customer type - disregard this criteria if the matrix has a customer number at which
            point only the customer number match matters.  Cust Type only applicable when customer number is blank.
            This allows for customer type to change in AF1 without having to update the Price Matrix type for that customer.*/
            AND (opbf-oe-prmtx.custype EQ ipbf-cust.type OR opbf-oe-prmtx.custype EQ "" OR opbf-oe-prmtx.cust-no NE "")
            /*Match on product category or match on blank product category - disregard this criteria if the matrix has a FG Item # at which
            point only the FG Item # match matters.  Product Category only applicable when customer number is blank.
            This allows for Product Category to change in IF1 without having to update the Price Matrix for that item.*/
            AND (opbf-oe-prmtx.procat EQ ipbf-itemfg.procat OR opbf-oe-prmtx.procat EQ "" OR opbf-oe-prmtx.i-no NE "")
            /*Match on ship ID or match matrix with blank ship ID*/
            AND (opbf-oe-prmtx.custShipID EQ ipcShipID OR opbf-oe-prmtx.custShipID EQ "")
            /*Must be effecitve*/
            AND (opbf-oe-prmtx.eff-date LE TODAY)
            /*must not be expired*/
            AND (opbf-oe-prmtx.exp-date GE TODAY OR opbf-oe-prmtx.exp-date EQ ?)
            /* Can't be all blank */
            AND NOT (opbf-oe-prmtx.cust-no EQ "" AND opbf-oe-prmtx.i-no EQ "" AND opbf-oe-prmtx.procat EQ "" AND opbf-oe-prmtx.custype EQ "" 
            AND opbf-oe-prmtx.custShipID EQ "")
            /*Sort the resulting data set so that actual matches take priority over blank matches*/
            BY opbf-oe-prmtx.cust-no DESCENDING 
            BY opbf-oe-prmtx.procat DESCENDING 
            BY opbf-oe-prmtx.custype DESCENDING 
            BY opbf-oe-prmtx.custShipID DESCENDING
            BY opbf-oe-prmtx.eff-date DESCENDING 
            :
            LEAVE.  /*After first/best match, leave*/
        END.
    END.
    
    /*Initialize return message for match*/
    ASSIGN 
        opcMatchDetail = "Match "
        cMsgItemFG     = "FGItemID=" + ipbf-itemfg.i-no
        cMsgItemProcat = "FGProdCat=" + ipbf-itemfg.procat
        cMsgCustID     = "CustID=" + ipbf-cust.cust-no
        cMsgCustType   = "CustType=" + ipbf-cust.type
        cMsgShipID     = "ShipID=" + ipcShipID
        .   
        
    /*Determine if match found from query and use matrix result to fill message criteria*/
    IF AVAILABLE opbf-oe-prmtx THEN 
    DO:
        ASSIGN 
            oplMatchFound = YES
            .
        ASSIGN 
            opcMatchDetail = "Match "
            cMsgItemFG     = "FGItemID="
            cMsgItemProcat = "FGProdCat="
            cMsgCustID     = "CustID="
            cMsgCustType   = "CustType="
            cMsgShipID     = "ShipID="
            cMsgCustID     = IF opbf-oe-prmtx.cust-no EQ "" THEN cMsgCustID + cMsgBlankInd ELSE cMsgCustID + opbf-oe-prmtx.cust-no
            cMsgItemFG     = IF opbf-oe-prmtx.i-no EQ "" THEN cMsgItemFG + cMsgBlankInd ELSE cMsgItemFG + opbf-oe-prmtx.i-no
            cMsgItemProcat = IF opbf-oe-prmtx.procat EQ "" THEN cMsgItemProcat + cMsgBlankInd ELSE cMsgItemProcat + opbf-oe-prmtx.procat
            cMsgCustType   = IF opbf-oe-prmtx.custype EQ "" THEN cMsgCustType + cMsgBlankInd ELSE cMsgCustType + opbf-oe-prmtx.custype 
            cMsgShipID     = IF opbf-oe-prmtx.custShipID EQ "" THEN cMsgShipID + cMsgBlankInd ELSE cMsgShipID + opbf-oe-prmtx.custShipID
            .

        DO iIndex = 1 TO EXTENT(opbf-oe-prmtx.qty):
            IF opbf-oe-prmtx.qty[iIndex] EQ 0 THEN
                NEXT.
            
            RUN pAddPricematrix (
                INPUT ipbf-itemfg.company,
                INPUT ipbf-itemfg.i-no,
                INPUT ipbf-cust.cust-no,
                INPUT ipcShipID,
                INPUT iIndex,   /* Level */
                INPUT opbf-oe-prmtx.qty[iIndex],
                INPUT "EA",     /* Quantity UOM */
                INPUT opbf-oe-prmtx.price[iIndex],
                INPUT opbf-oe-prmtx.uom[iIndex]
                ).
        END.
    END.    
    ELSE 
        ASSIGN 
            oplMatchFound  = NO
            opcMatchDetail = opcMatchDetail + "not " 
            .
 
    /*Build final return message*/
    opcMatchDetail = opcMatchDetail + "found: " + cMsgItemFG + " " + cMsgItemProcat + " " + cMsgCustID + " " + cMsgCustType + " " + cMsgShipID.


END PROCEDURE.

PROCEDURE pGetQtyMatchInfo PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a matrix buffer and a quantity target, determine:
         1. Price Level
         2. Price at Price Level
         3. Uom at Price Level
         4. If Quantity has a distinct match
         5. If Quantity is within the range of quantities listed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-prmtx FOR oe-prmtx.
    DEFINE INPUT PARAMETER ipdQuantityTarget AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLevelStart AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQtyLevel AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyDistinctMatch AS LOGICAL NO-UNDO.

    DEFINE VARIABLE iLevel AS INTEGER NO-UNDO.
    DEFINE VARIABLE cPriceMatrixPricingMethod AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLevelQty AS INTEGER NO-UNDO.
    
    ASSIGN 
        opiQtyLevel         = 0
        oplQtyDistinctMatch = NO 
        ipiLevelStart       = IF ipiLevelStart EQ 0 THEN 1 ELSE ipiLevelStart
        .
    IF NOT AVAIL ipbf-oe-prmtx THEN RETURN .
    
    cPriceMatrixPricingMethod = fGetNk1PriceMatrixPricingMethod(ipbf-oe-prmtx.company).
      
    IF cPriceMatrixPricingMethod EQ "From" AND ipdQuantityTarget LT ipbf-oe-prmtx.minOrderQty THEN RETURN.        
      
    /*process matrix array completely, one time*/
    DO iLevel = ipiLevelStart TO EXTENT(ipbf-oe-prmtx.qty): /* IF customer has higher starting level set otherwise start with 1st level*/
        IF cPriceMatrixPricingMethod EQ "From" THEN 
        DO:
            IF ipdQuantityTarget EQ ipbf-oe-prmtx.qty[iLevel] AND ipbf-oe-prmtx.qty[iLevel] NE 0 THEN 
                oplQtyDistinctMatch = YES. 
            
            IF ipdQuantityTarget GE ipbf-oe-prmtx.qty[iLevel] THEN
            iLevelQty = ipbf-oe-prmtx.qty[iLevel] .
            
            IF ipdQuantityTarget LE (ipbf-oe-prmtx.qty[iLevel] - 1) AND ipdQuantityTarget GE iLevelQty AND iLevelQty GT 0 THEN
            DO:                          
                IF opiQtyLevel = 0 THEN 
                    opiQtyLevel = (iLevel - 1).
            END.        
        END. /* cPriceMatrixPricingMethod EQ "From" */
        ELSE IF cPriceMatrixPricingMethod EQ "Up To" AND ipdQuantityTarget LE ipbf-oe-prmtx.qty[iLevel] THEN /*As soon as a qty level is found, greater than qty, all set*/
        DO:
            IF ipdQuantityTarget EQ ipbf-oe-prmtx.qty[iLevel] AND ipbf-oe-prmtx.qty[iLevel] NE 0 THEN 
                oplQtyDistinctMatch = YES.
            IF opiQtyLevel = 0 THEN 
                opiQtyLevel = iLevel.
        END. /*Qty LE oe-prmtx qty*/
    END.
          
END PROCEDURE.

PROCEDURE Price_ExpirePriceMatrixByItem:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    
    FOR EACH bf-oe-prmtx EXCLUSIVE-LOCK 
        WHERE bf-oe-prmtx.company   EQ ipcCompany
          AND bf-oe-prmtx.i-no      EQ ipcItemID
          AND (bf-oe-prmtx.exp-date GT TODAY OR bf-oe-prmtx.exp-date EQ ?):
              
        bf-oe-prmtx.exp-date = TODAY.              
    END.          

END PROCEDURE.

PROCEDURE Price_ExpireQuotesByItem:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-quoteitm FOR quoteitm.
    DEFINE BUFFER bf-quotehd  FOR quotehd.
    
    FOR EACH bf-quoteitm NO-LOCK
        WHERE bf-quoteitm.company EQ ipcCompany
          AND bf-quoteitm.i-no    EQ ipcItemID
        BREAK BY bf-quoteitm.q-no DESCENDING:
        IF FIRST-OF (bf-quoteitm.q-no) THEN DO:     
            FIND FIRST bf-quotehd EXCLUSIVE-LOCK 
                 WHERE bf-quotehd.company EQ ipcCompany
                   AND bf-quotehd.loc     EQ bf-quoteitm.loc 
                   AND bf-quotehd.q-no    EQ bf-quoteitm.q-no
                   AND (bf-quotehd.expiredate GT TODAY OR bf-quotehd.expiredate EQ ?)
                 NO-ERROR.       
            IF AVAILABLE bf-quotehd THEN 
                bf-quotehd.expiredate = TODAY.
        END.                         
     END.        
END PROCEDURE.

PROCEDURE Price_GetPriceMatrixTaxBasis:
    /*------------------------------------------------------------------------------
     Purpose: Returns a Rowid of a valid price matrix, given 3 key inputs
     Notes: Returns the tax basis of the available oe-prmtx record
     Syntax Example:
    RUN Price_GetPriceMatrixTaxBasis (cocode, oe-ordl.i-no, oe-ord.cust-no, oe-ord.ship-id,
                                        OUTPUT opiTaxBasis, OUTPUT lFound, OUTPUT cMessage).
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT  PARAMETER ipcFGItemID    AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT  PARAMETER ipcCustID      AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT  PARAMETER ipcShipID      AS CHARACTER NO-UNDO.  /*Ship to Scope of Customer  - optional*/
    DEFINE OUTPUT PARAMETER oplMatchFound  AS LOGICAL   NO-UNDO.  /*Logical that can determine if find on rowid should be done*/
    DEFINE OUTPUT PARAMETER opcMatchDetail AS CHARACTER NO-UNDO.  /*Clarifies the match criteria or failure to match*/
    DEFINE OUTPUT PARAMETER opiTaxBasis    AS INTEGER   NO-UNDO.  /* Return oe-prmtx.taxBasis */

    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.

    RUN pSetBuffers(ipcCompany, ipcFGItemId, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).

    /*Find match given buffers */  
    RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, OUTPUT oplMatchFound, OUTPUT opcMatchDetail).
    
    IF AVAILABLE bf-oe-prmtx THEN
        opiTaxBasis = bf-oe-prmtx.taxBasis.
END PROCEDURE.

PROCEDURE pSetBuffers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets Buffers for FG Item and Customers
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-itemfg FOR itemfg.
    DEFINE PARAMETER BUFFER opbf-cust   FOR cust.

    FIND FIRST opbf-itemfg NO-LOCK 
        WHERE opbf-itemfg.company EQ ipcCompany
        AND opbf-itemfg.i-no EQ ipcFGItemID
        NO-ERROR.
     
    FIND FIRST opbf-cust NO-LOCK 
        WHERE opbf-cust.company EQ ipcCompany
        AND opbf-cust.cust-no EQ ipcCustID
        NO-ERROR.
    
END PROCEDURE.

PROCEDURE pGetPriceHoldCriteria PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns Price hold Criteria Settings
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcCustNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcShipId     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceHoldActive AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceCheck AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyInRange AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyMatch AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplEffectiveDateAge AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEffectiveDateAgeDays AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyQuoted AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyAboveMin AS LOGICAL NO-UNDO.

    DEFINE VARIABLE lFound    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCriteria AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cAge      AS CHARACTER NO-UNDO.
   
    RUN sys/ref/nk1look.p (ipcCompany, "OEPriceHold", "L", NO, NO, "", "", OUTPUT cCriteria, OUTPUT lFound).
    oplPriceHoldActive = lFound AND cCriteria EQ "YES".
    RUN sys/ref/nk1look.p (ipcCompany, "OEPriceHold", "C", YES, YES, ipcCustNo, ipcShipId, OUTPUT cCriteria, OUTPUT lFound).
    
    IF lFound AND cCriteria NE "" THEN 
    DO: 
        ASSIGN 
            oplPriceCheck       = YES
            oplQtyInRange       = LOOKUP("QtyInRange", cCriteria) GT 0
            oplQtyMatch         = LOOKUP("QtyMatch", cCriteria) GT 0
            oplEffectiveDateAge = LOOKUP("EffDateAge", cCriteria) GT 0
            oplQtyQuoted        = LOOKUP("QtyQuoted", cCriteria) GT 0
            oplQtyAboveMin      = LOOKUP("QtyAboveMin", cCriteria) GT 0
            .
    END.
    IF oplEffectiveDateAge THEN 
    DO:
        RUN sys/ref/nk1look.p (ipcCompany, "OEPriceHold", "I", YES, YES, ipcCustNo, ipcShipId, OUTPUT cAge, OUTPUT lFound).
        IF lFound AND cAge NE "" THEN 
            opiEffectiveDateAgeDays = INTEGER(cAge).
        IF opiEffectiveDateAgeDays EQ 0 THEN 
            opiEffectiveDateAgeDays = 365.
    END.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fUseLastPrice RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns whether LastPrice is option set for NK1 SELLPRIC 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lUseLastPrice AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn       AS CHARACTER NO-UNDO. 
    
    RUN sys/ref/nk1look.p (ipcCompany,
        "SellPric",
        "C",
        NO,
        NO,
        "",
        "",
        OUTPUT cReturn,
        OUTPUT lFound).

    lUseLastPrice = lFound AND cReturn EQ "LastPric".
    RETURN lUseLastPrice.
	
END FUNCTION.

FUNCTION fGetNk1PriceMatrixPricingMethod RETURNS CHARACTER PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns whether LastPrice is option set for NK1 SELLPRIC 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cPriceMatrixPricingMethod AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lFound                    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn                   AS CHARACTER NO-UNDO. 
    
    RUN sys/ref/nk1look.p (ipcCompany,
        "PriceMatrixPricingMethod",
        "C",
        NO,
        NO,
        "",
        "",
        OUTPUT cReturn,
        OUTPUT lFound).

    cPriceMatrixPricingMethod = cReturn.
    RETURN cPriceMatrixPricingMethod.
	
END FUNCTION.

FUNCTION fGetNk1OEUseMatrixForNonstock RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns whether LastPrice is option set for NK1 SELLPRIC 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lOEUseMatrixForNonstock   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound                    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn                   AS CHARACTER NO-UNDO. 
    
    
    RUN sys/ref/nk1look.p (ipcCompany, 
        "OEUseMatrixForNonstock",
        "L",
        NO,
        NO,
        "",
        "",
        OUTPUT cReturn,
        OUTPUT lFound).    

    lOEUseMatrixForNonstock = logical(cReturn) NO-ERROR.
    RETURN lOEUseMatrixForNonstock.
	
END FUNCTION.
