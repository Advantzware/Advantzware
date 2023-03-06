
/*------------------------------------------------------------------------
    File        : BuildFarmForLogistics.p
    Purpose     : 

    Syntax      :

    Description : Given an Eb ROWID, build the VendItemCost information for 

    Author(s)   : 
    Created     : Sun Apr 26 21:45:36 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriEb AS ROWID.
DEFINE INPUT PARAMETER iplPromptForQuotes AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER iplSubAssembly AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE ttQuantityCost
    FIELD iQty       AS INTEGER
    FIELD dCostPerEA AS DECIMAL
    .
DEFINE VARIABLE cVendor             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorItem         AS CHARACTER NO-UNDO INITIAL "In-house Manufacture".
DEFINE VARIABLE lError              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMiscEstimateSource AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount              AS INTEGER   NO-UNDO.

DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  ********************   ******* */

FIND FIRST eb NO-LOCK
    WHERE ROWID(eb) EQ ipriEb
    NO-ERROR.
IF NOT AVAILABLE eb THEN RETURN.

cVendorItem = eb.sourceEstimate.

FIND FIRST cust NO-LOCK 
    WHERE cust.company EQ eb.company
    AND cust.active EQ "X"
    NO-ERROR.
IF AVAILABLE cust THEN 
    cVendor = cust.cust-no.
    
RUN pCopyPrepCharge(BUFFER eb).                      

RUN pCopyInksAndDesign(BUFFER eb).
    
RUN pGetCostFrom(INPUT eb.company,OUTPUT cMiscEstimateSource). 

IF cMiscEstimateSource EQ "Estimate" THEN
    RUN pBuildQuantitiesAndCostsFromEstimate(BUFFER eb, iplSubAssembly).
ELSE IF cMiscEstimateSource EQ "Quote" THEN
    DO:
        RUN pBuildQuantitiesAndCostsFromQuote(BUFFER eb).    
    END.

FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK
    WHERE bf-vendItemCost.company EQ eb.company
    AND bf-vendItemCost.estimateNo EQ eb.est-no
    AND bf-vendItemCost.formNo EQ eb.form-no
    AND bf-vendItemCost.blankNo EQ eb.blank-no
    AND bf-vendItemCost.itemID EQ eb.stock-no
    AND bf-vendItemCost.itemType EQ "FG"
    AND bf-vendItemCost.vendorID NE ""
    NO-ERROR.
IF NOT AVAIL bf-vendItemCost THEN
    FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK
        WHERE bf-vendItemCost.company EQ eb.company
        AND bf-vendItemCost.estimateNo EQ eb.est-no
        AND bf-vendItemCost.formNo EQ eb.form-no
        AND bf-vendItemCost.blankNo EQ eb.blank-no
        AND bf-vendItemCost.itemID EQ eb.stock-no
        AND bf-vendItemCost.itemType EQ "FG"
        AND bf-vendItemCost.vendorID EQ ""
        NO-ERROR.
IF NOT AVAILABLE bf-vendItemCost THEN 
DO:
    CREATE bf-vendItemCost.
    ASSIGN
        bf-vendItemCost.Company        = eb.company
        bf-vendItemCost.ItemID         = eb.stock-no
        bf-vendItemCost.itemType       = "FG"
        bf-vendItemCost.estimateNO     = eb.est-no
        bf-vendItemCost.formNo         = eb.form-no
        bf-vendItemCost.blankNo        = eb.blank-no
        bf-vendItemCost.vendorID       = cVendor
        bf-vendItemCost.VendorUOM      = "EA"
        bf-vendItemCost.vendorItemID   = cVendorItem
        bf-vendItemCost.effectiveDate  = TODAY 
        bf-vendItemCost.expirationDate = 12/31/2099
        .
END.
iCount = 0.
FOR EACH ttQuantityCost:
iCount = iCount + 1.
END.
FOR EACH ttQuantityCost:
    FIND FIRST bf-vendItemCostLevel EXCLUSIVE-LOCK
        WHERE bf-vendItemCostLevel.vendItemCostID EQ bf-vendItemCost.vendItemCostID
        AND bf-vendItemCostLevel.quantityBase EQ ttQuantityCost.iQty
        NO-ERROR.
    IF NOT AVAILABLE bf-vendItemCostLevel THEN 
    DO:
        CREATE bf-vendItemCostLevel.
        ASSIGN 
            bf-vendItemCostLevel.vendItemCostID = bf-vendItemCost.vendItemCostID
            bf-vendItemCostLevel.quantityBase   = ttQuantityCost.iQty
            .
    END.
    bf-vendItemCostLevel.costPerUOM = ttQuantityCost.dCostPerEA.
    IF iCount EQ 1 THEN bf-vendItemCostLevel.quantityBase = DYNAMIC-FUNCTION("VendCost_GetUnlimitedQuantity").
END.
RUN RecalculateFromAndTo (bf-vendItemCost.vendItemCostID, OUTPUT lError, OUTPUT cMessage).

RELEASE bf-vendItemCost.
RELEASE bf-vendItemCostLevel.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildQuantitiesAndCostsFromEstimate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Queries each probe for an estimate, building a tt of unique quantities, 
     starting with the latest probe for a given quantity
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT PARAMETER iplSubAssembly AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cEstNo AS CHARACTER.
    
    EMPTY TEMP-TABLE ttQuantityCost.
    cEstNo = ipbf-eb.sourceEstimate.
    RUN util/rjust.p (INPUT-OUTPUT cEstNo,8).
       
    FOR EACH probe NO-LOCK 
        WHERE probe.company EQ ipbf-eb.company
        AND probe.est-no EQ cEstNo
        BY probe.probe-date DESCENDING
        BY probe.probe-time DESCENDING:
        FIND FIRST ttQuantityCost
            WHERE ttQuantityCost.iQty EQ probe.est-qty
            NO-ERROR.
        IF NOT AVAILABLE ttQuantityCost THEN 
        DO:
            CREATE ttQuantityCost.
            ASSIGN 
                ttQuantityCost.iQty       = probe.est-qty
                ttQuantityCost.dCostPerEA = IF iplSubAssembly THEN probe.fact-cost / 1000 ELSE probe.sell-price / 1000
                .  
        END.            
    END.
            

END PROCEDURE.  

PROCEDURE pBuildQuantitiesAndCostsFromQuote PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Queries each valid/non-expired quote for an estimate 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE cEstNo      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iQuoteNo    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQuotePrice AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cQuoteUom   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cChoice     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iQuoteQty   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE rwRowid     AS ROWID     NO-UNDO.
    
    EMPTY TEMP-TABLE ttQuantityCost.
    cEstNo = ipbf-eb.sourceEstimate.
    RUN util/rjust.p (INPUT-OUTPUT cEstNo,8). 
    
    IF iplPromptForQuotes AND CAN-FIND(FIRST quotehd WHERE quotehd.company EQ ipbf-eb.company AND quotehd.est-no EQ cEstNo) THEN
        RUN oe/QuotePopup.w("",ipbf-eb.company,
            ipbf-eb.loc,
            cEstNo,
            ipbf-eb.part-no,
            INPUT-OUTPUT dQuotePrice,
            INPUT-OUTPUT cQuoteUom,
            INPUT-OUTPUT iQuoteQty,
            INPUT-OUTPUT iQuoteNo,
            OUTPUT cChoice,
            OUTPUT rwRowid). 
            
    IF NOT iplPromptForQuotes THEN
     RUN pGetLastQuoteNO(INPUT ipbf-eb.company, INPUT ipbf-eb.loc, INPUT cEstNo, OUTPUT iQuoteNo) .
       
    FOR EACH quotehd NO-LOCK
        WHERE quotehd.company  EQ ipbf-eb.company
        AND (quotehd.q-no EQ iQuoteNo OR iQuoteNo EQ 0)        
        AND quotehd.est-no    EQ cEstNo 
        AND (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?)
        AND quotehd.quo-date LE TODAY
        ,
        FIRST quoteitm NO-LOCK 
        WHERE quoteitm.company EQ quotehd.company         
        AND quoteitm.q-no EQ quotehd.q-no
        AND quoteitm.part-no EQ ipbf-eb.part-no
        ,
        EACH quoteqty NO-LOCK
        WHERE quoteqty.company EQ quoteitm.company         
        AND quoteqty.q-no EQ quoteitm.q-no
        AND quoteqty.line EQ quoteitm.line
        BY quotehd.quo-date DESCENDING:
            
        FIND FIRST ttQuantityCost 
            WHERE ttQuantityCost.iQty EQ quoteqty.qty
            NO-ERROR.
        IF NOT AVAILABLE ttQuantityCost THEN 
        DO:
            CREATE ttQuantityCost.
            ASSIGN 
                ttQuantityCost.iQty = quoteqty.qty
                ttQuantityCost.dCostPerEA = quoteqty.price .
            IF quoteqty.uom NE "EA" THEN 
                RUN Conv_ValueToEA(quoteqty.company, quoteitm.i-no, quoteqty.price, quoteqty.uom, 0, OUTPUT ttQuantityCost.dCostPerEA).
        END.               
    END.

END PROCEDURE. 

PROCEDURE pCopyPrepCharge PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE cEstNo AS CHARACTER.
    DEFINE VARIABLE iLine AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-est-prep FOR est-prep.
    
    cEstNo = ipbf-eb.sourceEstimate.
    RUN util/rjust.p (INPUT-OUTPUT cEstNo,8).
    
    FOR EACH bf-est-prep
        WHERE bf-est-prep.company EQ ipbf-eb.company 
        AND bf-est-prep.est-no  EQ ipbf-eb.est-no                      
        USE-INDEX est-qty NO-LOCK
        BY bf-est-prep.line DESCENDING:
        LEAVE.
    END.

    iLine = (IF AVAILABLE bf-est-prep THEN bf-est-prep.line ELSE 0) + 1.      
   
    FOR EACH est-prep NO-LOCK
        WHERE est-prep.company EQ ipbf-eb.company 
        AND est-prep.est-no EQ cEstNo 
        AND est-prep.simon EQ "S":
        
        CREATE bf-est-prep.
        BUFFER-COPY est-prep EXCEPT e-num rec_key est-no LINE TO bf-est-prep.
        ASSIGN
           bf-est-prep.e-num  = ipbf-eb.e-num
           bf-est-prep.est-no = ipbf-eb.est-no
           bf-est-prep.LINE   = iLine. 
           iLine = iLine + 1.
    END.
    RELEASE bf-est-prep. 
END PROCEDURE.  
          
PROCEDURE pGetCostFrom PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcMiscEstimateSource AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lRecFound AS LOGICAL.
    DEFINE VARIABLE cRtnChar  AS CHARACTER.
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "MiscEstimateSource", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cRtnChar, OUTPUT lRecFound). 
    opcMiscEstimateSource = STRING(cRtnChar) NO-ERROR .

END PROCEDURE.     

PROCEDURE pGetLastQuoteNO PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcloc      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimate AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opiQuoteNO AS INTEGER   NO-UNDO.    
        
    FOR EACH quotehd FIELDS(q-no) NO-LOCK  
        WHERE quotehd.company EQ ipcCompany         
        AND quotehd.quo-date LE TODAY 
        AND (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?)
        AND quotehd.est-no EQ ipcEstimate  
        BREAK BY quotehd.q-no DESC:
        opiQuoteNO = quotehd.q-no .
        LEAVE.
    END.

END PROCEDURE.   

PROCEDURE pCopyInksAndDesign PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE cEstNo AS CHARACTER.
    DEFINE VARIABLE li AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-eb FOR eb.
    DEFINE BUFFER bf-box-design-hdr  FOR box-design-hdr.
    DEFINE BUFFER bf-box-design-line  FOR box-design-line.
    
    cEstNo = ipbf-eb.sourceEstimate.
    RUN util/rjust.p (INPUT-OUTPUT cEstNo,8).
    
    FIND FIRST bf-eb NO-LOCK
        WHERE bf-eb.company EQ ipbf-eb.company 
        AND bf-eb.est-no  EQ cEstNo                     
        USE-INDEX est-no NO-ERROR.         
            
    IF AVAIL bf-eb THEN
    DO:
        FIND CURRENT ipbf-eb EXCLUSIVE-LOCK NO-ERROR.
        DO li = 1 TO EXTENT(bf-eb.i-code):
          ASSIGN
              ipbf-eb.i-ps[li]   = bf-eb.i-ps[li]
              ipbf-eb.i-code[li] = bf-eb.i-code[li]
              ipbf-eb.i-dscr[li] = bf-eb.i-dscr[li]
              ipbf-eb.i-%[li]    = bf-eb.i-%[li].
        END.
        DO li = 1 TO 17:
          ASSIGN
              ipbf-eb.i-ps2[li]   = bf-eb.i-ps2[li]
              ipbf-eb.i-code2[li] = bf-eb.i-code2[li]
              ipbf-eb.i-dscr2[li] = bf-eb.i-dscr2[li]
              ipbf-eb.i-%2[li]    = bf-eb.i-%2[li]
              ipbf-eb.side[li]    = bf-eb.side[li]
              ipbf-eb.unitNo[li]  = bf-eb.unitNo[li].
        END.
        ASSIGN
             ipbf-eb.i-col       = bf-eb.i-col
             ipbf-eb.i-pass      = bf-eb.i-pass
             ipbf-eb.i-coat      = bf-eb.i-coat
             ipbf-eb.i-coat-p    = bf-eb.i-coat-p
             ipbf-eb.i-coldscr   = bf-eb.i-coldscr
             ipbf-eb.casNoCharge = YES
             ipbf-eb.trNoCharge  = YES
             ipbf-eb.inkNoCharge = YES. 
        FIND CURRENT ipbf-eb NO-LOCK NO-ERROR.     
    END.   
    
    FOR EACH box-design-hdr
      WHERE box-design-hdr.design-no eq 0
        AND box-design-hdr.company   eq bf-eb.company
        AND box-design-hdr.est-no    eq bf-eb.est-no
        AND box-design-hdr.form-no   EQ bf-eb.form-no
        AND box-design-hdr.blank-no  EQ bf-eb.blank-no
      NO-LOCK:

    IF NOT CAN-FIND(FIRST bf-box-design-hdr WHERE
       bf-box-design-hdr.design-no = 0 AND
       bf-box-design-hdr.company EQ ipbf-eb.company AND
       bf-box-design-hdr.est-no EQ ipbf-eb.est-no AND
       bf-box-design-hdr.eqty EQ box-design-hdr.eqty AND
       bf-box-design-hdr.form-no EQ box-design-hdr.form-no AND
       bf-box-design-hdr.blank-no EQ box-design-hdr.blank-no) THEN
       DO:
          create bf-box-design-hdr.
          buffer-copy box-design-hdr except rec_key to bf-box-design-hdr
          assign
             bf-box-design-hdr.design-no = 0
             bf-box-design-hdr.company   = ipbf-eb.company
             bf-box-design-hdr.est-no    = ipbf-eb.est-no.
       END.

    for each box-design-line of box-design-hdr no-lock:

          IF NOT CAN-FIND(FIRST bf-box-design-line WHERE
             bf-box-design-line.design-no EQ 0 AND
             bf-box-design-line.company EQ ipbf-eb.company AND
             bf-box-design-line.est-no EQ ipbf-eb.est-no AND
             bf-box-design-line.eqty EQ box-design-line.eqty AND
             bf-box-design-line.form-no EQ box-design-line.form-no AND
             bf-box-design-line.blank-no EQ box-design-line.blank-no AND
             bf-box-design-line.line-no  EQ box-design-line.line-no) THEN
             DO:
                create bf-box-design-line.
                buffer-copy box-design-line except rec_key to bf-box-design-line
                assign
                   bf-box-design-line.design-no = 0
                   bf-box-design-line.company   = ipbf-eb.company
                   bf-box-design-line.est-no    = ipbf-eb.est-no.
             END.
      end.
    end.
    
    
END PROCEDURE.
