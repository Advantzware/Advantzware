/* r-invstat.i - Brad Vigrass - 2.26.2020 */

DEFINE TEMP-TABLE ttJobItem NO-UNDO
    FIELD cCompany                   AS CHARACTER LABEL "Company"                FORMAT "x(3)"
    FIELD cCustomerID                AS CHARACTER LABEL "Cust ID"                FORMAT "x(8)"
    FIELD cCustomerName              AS CHARACTER LABEL "Customer Name"          FORMAT "x(30)"
    FIELD cItemID                    AS CHARACTER LABEL "Item ID"                FORMAT "x(15)"
    FIELD cItemDescription           AS CHARACTER LABEL "Item Description"       FORMAT "x(20)"
    FIELD cJob                       AS CHARACTER LABEL "Job"                    FORMAT "x(9)"
    FIELD xxcJobID                   AS CHARACTER LABEL "Job"                    FORMAT "x(6)"
    FIELD xxiJobID2                  AS INTEGER   LABEL "Job Rev"                FORMAT "99"
    FIELD cProductCategory           AS CHARACTER LABEL "Product"                FORMAT "x(7)"
    FIELD cProductDescription        AS CHARACTER LABEL "Product Description"    FORMAT "x(20)"
    FIELD dtOrderDate                AS DATE      LABEL "Order Date"             FORMAT "99/99/9999"
    FIELD dtDueDate                  AS DATE      LABEL "Due Date"               FORMAT "99/99/9999"
    FIELD cSalesRep                  AS CHARACTER LABEL "SalesRep"               FORMAT "x(3)"
    FIELD cSalesRepName              AS CHARACTER LABEL "Sales Rep Name"         FORMAT "x(20)"
    FIELD dQuantityOrdered           AS DECIMAL   LABEL "Qty Ordered"            FORMAT "->,>>>,>>9.99"
    FIELD dQuantityProduced          AS DECIMAL   LABEL "Qty Produced"           FORMAT "->,>>>,>>9.99"
    FIELD dQuantityShipped           AS DECIMAL   LABEL "Qty Shipped"            FORMAT "->,>>>,>>9.99"
    FIELD dQuantityInvoiced          AS DECIMAL   LABEL "Qty Invoiced"           FORMAT "->,>>>,>>9.99"
    FIELD dQuantityOnHand            AS DECIMAL   LABEL "Qty On-Hand"            FORMAT "->,>>>,>>9.99"
    FIELD dQuantityBalanceToRun      AS DECIMAL   LABEL "Qty To Run"             FORMAT "->,>>>,>>9.99"
    FIELD dPricePerEA                AS DECIMAL   LABEL "Price Per EA"           FORMAT "->,>>>,>>9.99"
    FIELD dPriceTotalOnHand          AS DECIMAL   LABEL "Sell Value of On-Hand"  FORMAT "->,>>>,>>9.99"
    FIELD dPriceTotalOrdered         AS DECIMAL   LABEL "Sell Value of Ordered"  FORMAT "->,>>>,>>9.99"
    FIELD dPriceTotalProduced        AS DECIMAL   LABEL "Sell Value of Produced" FORMAT "->,>>>,>>9.99"
    FIELD dPriceTotalShipped         AS DECIMAL   LABEL "Sell Value of Shipped"  FORMAT "->,>>>,>>9.99"
    FIELD dPriceTotalBalanceToRun    AS DECIMAL   LABEL "Sell Value to Run"      FORMAT "->,>>>,>>9.99"
    FIELD dtAsOfDate                 AS DATE      LABEL "As Of Date"             FORMAT "99/99/9999"
    FIELD cAsOfDateOption            AS CHARACTER LABEL "As Of Date Option"      FORMAT "x(20)"
    FIELD cSource                    AS CHARACTER
    FIELD cLineType                  AS CHARACTER
    FIELD lNoMake                    AS LOGICAL 
    FIELD lIsComponent               AS LOGICAL 
    FIELD lHasOrder                  AS LOGICAL
    FIELD iCountVF                   AS INTEGER 
    FIELD iCountBD                   AS INTEGER 
    FIELD iCountWrap                 AS INTEGER 
    FIELD iCountLiner                AS INTEGER
    FIELD iCountOther                AS INTEGER
    FIELD dQuantityOrderedVF         AS DECIMAL
    FIELD dQuantityProducedVF        AS DECIMAL
    FIELD dQuantityShippedVF         AS DECIMAL 
    FIELD dQuantityInvoicedVF        AS DECIMAL
    FIELD dQuantityOnHandVF          AS DECIMAL
    FIELD dQuantityBalanceToRunVF    AS DECIMAL 
    FIELD dQuantityOrderedBD         AS DECIMAL
    FIELD dQuantityProducedBD        AS DECIMAL
    FIELD dQuantityShippedBD         AS DECIMAL 
    FIELD dQuantityInvoicedBD        AS DECIMAL
    FIELD dQuantityOnHandBD          AS DECIMAL
    FIELD dQuantityBalanceToRunBD    AS DECIMAL 
    FIELD dQuantityOrderedWrap       AS DECIMAL
    FIELD dQuantityProducedWrap      AS DECIMAL
    FIELD dQuantityShippedWrap       AS DECIMAL 
    FIELD dQuantityInvoicedWrap      AS DECIMAL
    FIELD dQuantityOnHandWrap        AS DECIMAL
    FIELD dQuantityBalanceToRunWrap  AS DECIMAL 
    FIELD dQuantityOrderedLiner      AS DECIMAL
    FIELD dQuantityProducedLiner     AS DECIMAL
    FIELD dQuantityShippedLiner      AS DECIMAL 
    FIELD dQuantityInvoicedLiner     AS DECIMAL
    FIELD dQuantityOnHandLiner       AS DECIMAL
    FIELD dQuantityBalanceToRunLiner AS DECIMAL
    FIELD dQuantityOrderedOther      AS DECIMAL
    FIELD dQuantityProducedOther     AS DECIMAL
    FIELD dQuantityShippedOther      AS DECIMAL 
    FIELD dQuantityInvoicedOther     AS DECIMAL
    FIELD dQuantityOnHandOther       AS DECIMAL
    FIELD dQuantityBalanceToRunOther AS DECIMAL
    .
DEFINE TEMP-TABLE ttProdSum NO-UNDO
    FIELD cCompany                LIKE ttJobItem.cCompany
    FIELD cProductCategory        LIKE ttJobItem.cProductCategory
    FIELD cProductDescription     LIKE ttJobItem.cProductDescription
    FIELD dQuantityOnHand         LIKE ttJobItem.dQuantityOnHand
    FIELD dPriceTotalOnHand       LIKE ttJobItem.dPriceTotalOnHand
    FIELD dQuantityBalanceToRun   LIKE ttJobItem.dQuantityBalanceToRun
    FIELD dPriceTotalBalanceToRun LIKE ttJobItem.dPriceTotalBalanceToRun
    .
{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */

{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

FUNCTION fSalesRepName RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcSalesRep AS CHARACTER):
    FIND FIRST sman NO-LOCK
         WHERE sman.company EQ ipcCompany
           AND sman.sman EQ ipcSalesRep
         NO-ERROR.
    RETURN IF AVAILABLE sman THEN REPLACE(sman.sname,",","") ELSE "".
END FUNCTION.

FUNCTION fItemDescription RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER):
        
    DEFINE VARIABLE cItemDescription AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bItemFG FOR itemfg.
    DEFINE BUFFER bPrep   FOR prep.
    
    FIND FIRST bItemFG NO-LOCK
         WHERE bItemFG.company EQ ipcCompany
           AND bItemFG.i-no    EQ ipcItemID
         NO-ERROR.
    IF AVAILABLE bItemFG THEN
    cItemDescription = bItemFG.i-name.
    ELSE DO:
        FIND FIRST bPrep NO-LOCK
             WHERE bPrep.company EQ ipcCompany
               AND bPrep.code    EQ ipcItemID
             NO-ERROR.
        IF AVAILABLE bPrep THEN
        cItemDescription = bPrep.dscr.
    END.

    RETURN cItemDescription.

END FUNCTION.

FUNCTION fProductDescription RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcProductCategory AS CHARACTER):
    FIND FIRST fgcat NO-LOCK
         WHERE fgcat.company EQ ipcCompany
           AND fgcat.procat  EQ ipcProductCategory
         NO-ERROR.
    RETURN IF AVAILABLE fgcat THEN fgcat.dscr ELSE "".
END FUNCTION.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    CASE cAsOfDateOption:
        WHEN "Prior Month" THEN
        dtAsOfDate = DYNAMIC-FUNCTION("sfCommon_DateOptionDate", "Date Prior Month", dtAsOfDate).
        WHEN "Prior Year" THEN
        dtAsOfDate = DYNAMIC-FUNCTION("sfCommon_DateOptionDate", "Date Prior Year", dtAsOfDate).
    END CASE.
    RUN pBuildJobItem (
        cCompany, 
        dtAsOfDate,
        cStartCustNo,
        cEndCustNo,
        cStartFGItem,
        cEndFGItem
        ).
END PROCEDURE.

PROCEDURE pAddJobItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given inputs, adds a temp-table record for 
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobID2           AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcProductCategory  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityOrdered  AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityProduced AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityShipped  AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityInvoiced AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityOnHand   AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcSource           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdPricePerEA       AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplNoMake           AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplIsComponent      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplHasOrder         AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdtOrderDate       AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDueDate         AS DATE      NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttJobItem FOR ttJobItem.
    
    &IF {&subjectID} EQ 94 &THEN
    IF lIncludeZeroPricePer EQ NO AND ipdPricePerEA EQ 0 THEN RETURN.
    &ENDIF

    CREATE opbf-ttJobItem.
    ASSIGN 
        opbf-ttJobItem.cCompany                = ipcCompany
        opbf-ttJobItem.cCustomerID             = CAPS(ipcCustomerID)
        opbf-ttJobItem.cItemID                 = CAPS(ipcItemID)
        opbf-ttJobItem.cItemDescription        = fItemDescription (ipcCompany, ipcItemID)
        opbf-ttJobItem.xxcJobID                = ipcJobID
        opbf-ttJobItem.xxiJobID2               = ipiJobID2
        opbf-ttJobItem.cProductCategory        = CAPS(ipcProductCategory)
        opbf-ttJobItem.cProductDescription     = fProductDescription (ipcCompany, ipcProductCategory)
        opbf-ttJobItem.dQuantityOrdered        = ipdQuantityOrdered
        opbf-ttJobItem.dQuantityProduced       = ipdQuantityProduced
        opbf-ttJobItem.dQuantityShipped        = ipdQuantityShipped
        opbf-ttJobItem.dQuantityInvoiced       = ipdQuantityInvoiced
        opbf-ttJobItem.cSource                 = ipcSource
        opbf-ttJobItem.dQuantityOnHand         = ipdQuantityOnHand
        opbf-ttJobItem.dPricePerEA             = ipdPricePerEA
        opbf-ttJobItem.lNoMake                 = iplNoMake
        opbf-ttJobItem.lIsComponent            = iplIsComponent
        opbf-ttJobItem.lHasOrder               = iplHasOrder
        opbf-ttJobItem.dPriceTotalOnHand       = opbf-ttJobItem.dPricePerEA      * opbf-ttJobItem.dQuantityOnHand
        opbf-ttJobItem.dQuantityBalanceToRun   = opbf-ttJobItem.dQuantityOrdered - opbf-ttJobItem.dQuantityProduced
        opbf-ttJobItem.dPriceTotalOrdered      = opbf-ttJobItem.dPricePerEA      * opbf-ttJobItem.dQuantityOrdered
        opbf-ttJobItem.dPriceTotalProduced     = opbf-ttJobItem.dPricePerEA      * opbf-ttJobItem.dQuantityProduced
        opbf-ttJobItem.dPriceTotalShipped      = opbf-ttJobItem.dPricePerEA      * opbf-ttJobItem.dQuantityShipped
        opbf-ttJobItem.dPriceTotalBalanceToRun = opbf-ttJobItem.dPricePerEA      * opbf-ttJobItem.dQuantityBalanceToRun
        opbf-ttJobItem.cJob                    = ipcJobID + (IF ipiJobID2 NE ? THEN "-" + STRING(ipiJobID2,"99") ELSE "")
        opbf-ttJobItem.dtOrderDate             = ipdtOrderDate
        opbf-ttJobItem.dtDueDate               = ipdtDueDate
        opbf-ttJobItem.dtAsOfDate              = dtAsOfDate
        opbf-ttJobItem.cAsOfDateOption         = cAsOfDateOption
        .
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ ipcCompany
           AND cust.cust-no EQ ipcCustomerID
         NO-ERROR.
    IF AVAILABLE cust THEN
    ASSIGN
        opbf-ttJobItem.cCustomerName = REPLACE(cust.name,",","")
        opbf-ttJobItem.cSalesRep     = cust.sman
        opbf-ttJobItem.cSalesRepName = fSalesRepName (ipcCompany, cust.sman)
        .
    FIND FIRST ttProdSum
         WHERE ttProdSum.cCompany         EQ ipcCompany
           AND ttProdSum.cProductCategory EQ ipcProductCategory
         NO-ERROR.
    IF NOT AVAILABLE ttProdSum THEN DO:
        CREATE ttProdSum.
        ASSIGN
            ttProdSum.cCompany            = ipcCompany
            ttProdSum.cProductCategory    = ipcProductCategory
            ttProdSum.cProductDescription = fProductDescription (ipcCompany, ipcProductCategory)
            .
    END.
    ASSIGN
        ttProdSum.dQuantityOnHand         = ttProdSum.dQuantityOnHand         + opbf-ttJobItem.dQuantityOnHand
        ttProdSum.dPriceTotalOnHand       = ttProdSum.dPriceTotalOnHand       + opbf-ttJobItem.dPriceTotalOnHand
        ttProdSum.dQuantityBalanceToRun   = ttProdSum.dQuantityBalanceToRun   + opbf-ttJobItem.dQuantityBalanceToRun
        ttProdSum.dPriceTotalBalanceToRun = ttProdSum.dPriceTotalBalanceToRun + opbf-ttJobItem.dPriceTotalBalanceToRun
        .
END PROCEDURE.

PROCEDURE pAnalyzeItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given a Set Header temp-table buffer, increment counts and quantities
 based on product category
 ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttJobItem FOR ttJobItem.
    DEFINE INPUT PARAMETER ipcProductCategory AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiQuantityPerSet  AS INTEGER   NO-UNDO.

    CASE ipcProductCategory:
        WHEN "VF" OR 
        WHEN "THERM" THEN 
            ASSIGN 
                ipbf-ttJobItem.iCountVF                = ipbf-ttJobItem.iCountVF + ipiQuantityPerSet
                ipbf-ttJobItem.dQuantityOrderedVF      = ipbf-ttJobItem.dQuantityOrdered * ipbf-ttJobItem.iCountVF
                ipbf-ttJobItem.dQuantityProducedVF     = ipbf-ttJobItem.dQuantityProduced * ipbf-ttJobItem.iCountVF
                ipbf-ttJobItem.dQuantityShippedVF      = ipbf-ttJobItem.dQuantityShipped * ipbf-ttJobItem.iCountVF
                ipbf-ttJobItem.dQuantityInvoicedVF     = ipbf-ttJobItem.dQuantityInvoiced * ipbf-ttJobItem.iCountVF
                ipbf-ttJobItem.dQuantityOnHandVF       = ipbf-ttJobItem.dQuantityOnHand * ipbf-ttJobItem.iCountVF
                ipbf-ttJobItem.dQuantityBalanceToRunVF = ipbf-ttJobItem.dQuantityBalanceToRun * ipbf-ttJobItem.iCountVF           
                . 
        WHEN "BWRAP" OR 
        WHEN "LWRAP" THEN 
            ASSIGN 
                ipbf-ttJobItem.iCountWrap                = ipbf-ttJobItem.iCountWrap + ipiQuantityPerSet
                ipbf-ttJobItem.dQuantityOrderedWrap      = ipbf-ttJobItem.dQuantityOrdered * ipbf-ttJobItem.iCountWrap
                ipbf-ttJobItem.dQuantityProducedWrap     = ipbf-ttJobItem.dQuantityProduced * ipbf-ttJobItem.iCountWrap
                ipbf-ttJobItem.dQuantityShippedWrap      = ipbf-ttJobItem.dQuantityShipped * ipbf-ttJobItem.iCountWrap
                ipbf-ttJobItem.dQuantityInvoicedWrap     = ipbf-ttJobItem.dQuantityInvoiced * ipbf-ttJobItem.iCountWrap
                ipbf-ttJobItem.dQuantityOnHandWrap       = ipbf-ttJobItem.dQuantityOnHand * ipbf-ttJobItem.iCountWrap
                ipbf-ttJobItem.dQuantityBalanceToRunWrap = ipbf-ttJobItem.dQuantityBalanceToRun * ipbf-ttJobItem.iCountWrap           
                .             
        WHEN "BLINE" OR 
        WHEN "LLINE" THEN 
            ASSIGN 
                ipbf-ttJobItem.iCountLiner                = ipbf-ttJobItem.iCountLiner + ipiQuantityPerSet
                ipbf-ttJobItem.dQuantityOrderedLiner      = ipbf-ttJobItem.dQuantityOrdered * ipbf-ttJobItem.iCountLiner
                ipbf-ttJobItem.dQuantityProducedLiner     = ipbf-ttJobItem.dQuantityProduced * ipbf-ttJobItem.iCountLiner
                ipbf-ttJobItem.dQuantityShippedLiner      = ipbf-ttJobItem.dQuantityShipped * ipbf-ttJobItem.iCountLiner
                ipbf-ttJobItem.dQuantityInvoicedLiner     = ipbf-ttJobItem.dQuantityInvoiced * ipbf-ttJobItem.iCountLiner
                ipbf-ttJobItem.dQuantityOnHandLiner       = ipbf-ttJobItem.dQuantityOnHand * ipbf-ttJobItem.iCountLiner
                ipbf-ttJobItem.dQuantityBalanceToRunLiner = ipbf-ttJobItem.dQuantityBalanceToRun * ipbf-ttJobItem.iCountLiner           
                . 
        WHEN "BBRD" OR 
        WHEN "LBRD" THEN 
            ASSIGN 
                ipbf-ttJobItem.iCountBD                = ipbf-ttJobItem.iCountBD + ipiQuantityPerSet
                ipbf-ttJobItem.dQuantityOrderedBD      = ipbf-ttJobItem.dQuantityOrdered * ipbf-ttJobItem.iCountBD
                ipbf-ttJobItem.dQuantityProducedBD     = ipbf-ttJobItem.dQuantityProduced * ipbf-ttJobItem.iCountBD
                ipbf-ttJobItem.dQuantityShippedBD      = ipbf-ttJobItem.dQuantityShipped * ipbf-ttJobItem.iCountBD
                ipbf-ttJobItem.dQuantityInvoicedBD     = ipbf-ttJobItem.dQuantityInvoiced * ipbf-ttJobItem.iCountBD
                ipbf-ttJobItem.dQuantityOnHandBD       = ipbf-ttJobItem.dQuantityOnHand * ipbf-ttJobItem.iCountBD
                ipbf-ttJobItem.dQuantityBalanceToRunBD = ipbf-ttJobItem.dQuantityBalanceToRun * ipbf-ttJobItem.iCountBD           
                .
        OTHERWISE 
        ASSIGN 
            ipbf-ttJobItem.iCountOther                = ipbf-ttJobItem.iCountOther + ipiQuantityPerSet
            ipbf-ttJobItem.dQuantityOrderedOther      = ipbf-ttJobItem.dQuantityOrdered * ipbf-ttJobItem.iCountOther
            ipbf-ttJobItem.dQuantityProducedOther     = ipbf-ttJobItem.dQuantityProduced * ipbf-ttJobItem.iCountOther
            ipbf-ttJobItem.dQuantityShippedOther      = ipbf-ttJobItem.dQuantityShipped * ipbf-ttJobItem.iCountOther
            ipbf-ttJobItem.dQuantityInvoicedOther     = ipbf-ttJobItem.dQuantityInvoiced * ipbf-ttJobItem.iCountOther
            ipbf-ttJobItem.dQuantityOnHandOther       = ipbf-ttJobItem.dQuantityOnHand * ipbf-ttJobItem.iCountOther
            ipbf-ttJobItem.dQuantityBalanceToRunOther = ipbf-ttJobItem.dQuantityBalanceToRun * ipbf-ttJobItem.iCountOther           
            .
    END CASE.

END PROCEDURE.

PROCEDURE pBuildJobItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf       AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustStart   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustEnd     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemEnd   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-comp-itemfg    FOR itemfg.
    DEFINE BUFFER bf-job-hdr        FOR job-hdr.
    DEFINE BUFFER bf-ttJobItem      FOR ttJobItem.
    DEFINE BUFFER bf-comp-ttJobItem FOR ttJobItem.
    
    DEFINE VARIABLE cSource     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dInvAmt     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPricePer   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyOrd     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyProd    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyShip    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyInv     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyOnHand  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dtOrderDate AS DATE      NO-UNDO.
    DEFINE VARIABLE dtDueDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE lHasOrder   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lIsComp     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lNoMake     AS LOGICAL   NO-UNDO.
    
    EMPTY TEMP-TABLE ttJobItem.
    FOR EACH job-hdr NO-LOCK  /*Go through all job-hdrs*/
        WHERE job-hdr.company EQ ipcCompany
          AND job-hdr.cust-no GE ipcCustStart
          AND job-hdr.cust-no LE ipcCustEnd
          AND job-hdr.i-no    GE ipcFGItemStart
          AND job-hdr.i-no    LE ipcFGItemEnd,
        FIRST job OF job-hdr  NO-LOCK
        WHERE job.create-date LE ipdtAsOf,
        FIRST itemfg NO-LOCK
        WHERE itemfg.company  EQ job-hdr.company
          AND itemfg.i-no     EQ job-hdr.i-no
        :
        ASSIGN 
            dQtyOrd     = job-hdr.qty
            dQtyProd    = 0
            dQtyShip    = 0
            dQtyInv     = 0
            dQtyOnHand  = 0
            dPricePer   = 0
            lHasOrder   = NO
            lIsComp     = NO
            lNoMake     = NO
            dtOrderDate = ?
            dtDueDate   = ?
            .
        RUN pGetQuantityMadeAsOf (
            job-hdr.company,
            job-hdr.job-no,
            job-hdr.job-no2,
            job-hdr.i-no,
            ipdtAsOf,
            OUTPUT dQtyProd
            ).                
        IF job-hdr.ord-no NE 0 THEN DO: /*This will be the case for all non-stock jobs*/
            FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ job-hdr.company
                  AND oe-ordl.ord-no  EQ job-hdr.ord-no
                  AND oe-ordl.i-no    EQ job-hdr.i-no
                NO-ERROR.
            IF AVAILABLE oe-ordl THEN DO:
                RUN pGetQuantityInvShipAsOf (
                    ROWID(oe-ordl),
                    ipdtAsOf,
                    OUTPUT dQtyInv,
                    OUTPUT dQtyShip
                    ).
                ASSIGN 
                    dQtyOnHand = dQtyProd - dQtyShip
                    dPricePer  = IF oe-ordl.pr-uom EQ "M" THEN oe-ordl.price / 1000 ELSE oe-ordl.price
                    lHasOrder  = YES
                    dQtyOrd    = oe-ordl.qty
                    .
            END.
            FIND FIRST oe-ord NO-LOCK
                 WHERE oe-ord.company EQ job-hdr.company
                   AND oe-ord.ord-no  EQ job-hdr.ord-no
                 NO-ERROR.
            IF AVAILABLE oe-ord THEN
            ASSIGN
                dtOrderDate = oe-ord.ord-date
                dtDueDate   = oe-ord.due-date
                .
        END.
        ELSE DO:
            RUN pGetQuantityOnHandAsOf (
                job-hdr.company,
                job-hdr.job-no,
                job-hdr.job-no2,
                job-hdr.i-no,
                ipdtAsOf,
                OUTPUT dQtyOnHand
                ).
            ASSIGN 
                dQtyShip  = dQtyProd - dQtyOnHand
                dQtyInv   = dQtyShip
                lHasOrder = NO
                .
        END.
        
        cSource = "Job Header - "
                + IF itemfg.isaset AND NOT CAN-DO(cProductCategoryList,itemfg.procat) THEN "Set"
                  ELSE "Single".
              
        IF dQtyInv GE dQtyOrd THEN NEXT. /* No Backlog at time of "as of" */
        
        RUN pAddJobItem (
            job-hdr.company,
            job-hdr.cust-no,
            job-hdr.i-no,
            job-hdr.job-no,
            job-hdr.job-no2,
            itemfg.procat, 
            dQtyOrd,
            dQtyProd,
            dQtyShip,
            dQtyInv,
            dQtyOnHand, 
            cSource,
            dPricePer,
            NO,
            NO,
            lHasOrder,
            dtOrderDate,
            dtDueDate,
            BUFFER bf-ttJobItem
            ).
        IF itemfg.isaset THEN DO:
            FOR EACH fg-set NO-LOCK 
                WHERE fg-set.company EQ itemfg.company
                  AND fg-set.set-no  EQ itemfg.i-no,
                FIRST bf-comp-itemfg NO-LOCK 
                WHERE bf-comp-itemfg.company EQ fg-set.company
                  AND bf-comp-itemfg.i-no    EQ fg-set.part-no
                BY fg-set.line
                :
                ASSIGN 
                    dQtyInv    = 0
                    dQtyShip   = 0
                    dQtyProd   = 0
                    dQtyOnHand = 0
                    dPricePer  = 0
                    lNoMake    = fg-set.noReceipt
                    .
                IF AVAILABLE bf-ttJobItem THEN 
                RUN pAnalyzeItem (
                    BUFFER bf-ttJobItem,
                    bf-comp-itemfg.procat,
                    fg-set.part-qty
                    ).
                RUN pGetQuantityMadeAsOf (
                    job-hdr.company,
                    job-hdr.job-no,
                    job-hdr.job-no2,
                    fg-set.part-no,
                    ipdtAsOf,
                    OUTPUT dQtyProd
                    ).
                RUN pGetQuantityOnHandAsOf (
                    job-hdr.company,
                    job-hdr.job-no,
                    job-hdr.job-no2,
                    fg-set.part-no,
                    ipdtAsOf,
                    OUTPUT dQtyOnHand
                    ).
                RUN pAddJobItem (
                    job-hdr.company,
                    job-hdr.cust-no,
                    fg-set.part-no,
                    job-hdr.job-no,
                    job-hdr.job-no2,
                    bf-comp-itemfg.procat, 
                    dQtyOrd * fg-set.qtyPerSet,
                    dQtyProd,
                    dQtyProd - dQtyOnHand,
                    dQtyProd - dQtyOnHand,
                    dQtyOnHand, 
                    "Component",
                    dPricePer,
                    lNoMake,
                    YES,
                    NO,
                    dtOrderDate,
                    dtDueDate,
                    BUFFER bf-comp-ttJobItem
                    ).
            END.            
        END.
        ELSE IF AVAILABLE bf-ttJobItem THEN 
                RUN pAnalyzeItem (
                    BUFFER bf-ttJobItem,
                    itemfg.procat,
                    1
                    ).
    END.

    FOR EACH oe-ord NO-LOCK 
        WHERE oe-ord.company EQ ipcCompany
        AND oe-ord.cust-no   GE ipcCustStart
        AND oe-ord.cust-no   LE ipcCustEnd
        AND oe-ord.ord-date  LE ipdtAsOf,
        EACH oe-ordm NO-LOCK
        WHERE oe-ordm.company EQ oe-ord.company
          AND oe-ordm.ord-no  EQ oe-ord.ord-no
          AND oe-ordm.charge  GE ipcFGItemStart
          AND oe-ordm.charge  LE ipcFGItemEnd
          AND oe-ordm.bill    EQ "Y",
        FIRST prep NO-LOCK 
        WHERE prep.company EQ oe-ordm.company
          AND prep.code    EQ oe-ordm.charge
        :
        ASSIGN 
            dQtyOrd       = 1
            dQtyProd      = 0
            dQtyShip      = 0
            dQtyInv       = 0
            dQtyOnHand    = 0
            dPricePer     = oe-ordm.amt
            dInvAmt       = 0
            lHasOrder     = YES
            lIsComp       = NO
            lNoMake       = NO
            dtOrderDate   = oe-ord.ord-date
            dtDueDate     = oe-ord.due-date
            .
        RUN pGetInvoicedAmountForMiscAsOf (
            oe-ord.company,
            oe-ord.ord-no,
            oe-ordm.charge,
            ipdtAsOf,
            OUTPUT dInvAmt
            ).
        
        IF dPricePer LT dInvAmt THEN NEXT.  /* already invoiced at time of "as of" */
        
        RUN pAddJobItem (
            oe-ord.company,
            oe-ord.cust-no,
            oe-ordm.charge,
            STRING(oe-ord.ord-no),
            ?,
            prep.fgcat,
            dQtyOrd,
            dQtyProd,
            dQtyShip,
            dQtyInv,
            dQtyOnHand, 
            "Misc/Prep",
            dPricePer,
            NO,
            NO,
            lHasOrder,
            dtOrderDate,
            dtDueDate,
            BUFFER bf-ttJobItem
            ).
    END.
    
END PROCEDURE.

PROCEDURE pGetInvoicedAmountForMiscAsOf PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given a prep charge from an order, find the quantity invoiced for that charge
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrdNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcCharge  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf   AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceAmount AS DECIMAL NO-UNDO.

    FOR EACH ar-invl NO-LOCK 
        WHERE ar-invl.company EQ ipcCompany
          AND ar-invl.misc    EQ YES
          AND ar-invl.ord-no  EQ ipiOrdNo
          AND ar-invl.i-name  EQ ipcCharge,
        FIRST ar-inv NO-LOCK 
        WHERE ar-inv.company  EQ ar-invl.company 
          AND ar-inv.x-no     EQ ar-invl.x-no
          AND ar-inv.inv-date LE ipdtAsOf,
        FIRST prep NO-LOCK 
        WHERE prep.company EQ ar-invl.company
          AND prep.code    EQ ar-invl.i-name
        :
        opdInvoiceAmount = ar-invl.amt.
        LEAVE.
    END.

END PROCEDURE.

PROCEDURE pGetQuantityInvShipAsOf PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given an order line and as of date, output invoiced and shipped quantities
 Notes: From ordlsqty.p
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrdl AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf   AS DATE  NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityInvoiced LIKE oe-ordl.inv-qty  NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityShipped  LIKE oe-ordl.ship-qty NO-UNDO.

    DEFINE VARIABLE lInvQty AS LOG NO-UNDO.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    FIND oe-ordl WHERE ROWID(oe-ordl) EQ ipriOeOrdl NO-LOCK NO-ERROR.

    IF AVAIL oe-ordl THEN DO:
        lInvQty = NO.
        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.company  EQ oe-ordl.company
              AND ar-invl.ord-no   EQ oe-ordl.ord-no
              AND ar-invl.i-no     EQ oe-ordl.i-no,
            FIRST ar-inv NO-LOCK 
            WHERE ar-inv.x-no     EQ ar-invl.x-no
              AND ar-inv.inv-date LE ipdtAsOf
            :
            ASSIGN 
                lInvQty             = YES
                opdQuantityInvoiced = opdQuantityInvoiced + ar-invl.inv-qty
                .
        END.
   
        /* if client only wants posted, remove this for each block */
        FOR EACH inv-head NO-LOCK 
            WHERE inv-head.company  EQ oe-ordl.company
              AND inv-head.cust-no  EQ oe-ordl.cust-no
              AND inv-head.inv-date LE ipdtAsOf
              AND inv-head.printed  EQ YES,
            EACH inv-line NO-LOCK 
            WHERE inv-line.r-no   EQ inv-head.r-no
              AND inv-line.ord-no EQ oe-ordl.ord-no
              AND inv-line.i-no   EQ oe-ordl.i-no
              AND inv-line.line   EQ oe-ordl.line
            :
            ASSIGN 
                lInvQty             = YES
                opdQuantityInvoiced = opdQuantityInvoiced + inv-line.inv-qty
                .
        END.

        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-ordl.company
              AND oe-boll.ord-no  EQ oe-ordl.ord-no
              AND oe-boll.i-no    EQ oe-ordl.i-no
              AND oe-boll.line    EQ oe-ordl.line
              AND oe-boll.s-code  NE "T"
            USE-INDEX ord-no,
            FIRST oe-bolh NO-LOCK
            WHERE oe-bolh.b-no     EQ oe-boll.b-no
              AND oe-bolh.posted   EQ YES
              AND oe-bolh.bol-date LE ipdtAsOf
            USE-INDEX b-no
            :
            IF oe-boll.s-code NE "S" AND NOT oe-ordl.is-a-component AND NOT lInvQty THEN
            opdQuantityInvoiced = opdQuantityInvoiced + oe-boll.qty.    
            IF (oe-boll.s-code NE "I" OR
                CAN-FIND(FIRST bf-oe-ordl 
                         WHERE bf-oe-ordl.company        EQ oe-ordl.company
                           AND bf-oe-ordl.ord-no         EQ oe-ordl.ord-no
                           AND bf-oe-ordl.is-a-component EQ YES
                           AND bf-oe-ordl.set-hdr-line   EQ oe-ordl.line)) THEN                
            opdQuantityShipped = opdQuantityShipped + oe-boll.qty.
        END.
    END.

END PROCEDURE.

PROCEDURE pGetQuantityMadeAsOf PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given Job inputs, determine the production quantity as of a particular date
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcINo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf   AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQty    AS DECIMAL   NO-UNDO.        
    
    FOR EACH fg-rcpth  
        FIELDS(r-no rita-code) NO-LOCK
        WHERE fg-rcpth.company    EQ ipcCompany
          AND fg-rcpth.job-no     EQ ipcJobNo
          AND fg-rcpth.job-no2    EQ ipiJobNo2
          AND fg-rcpth.i-no       EQ ipcINo
          AND fg-rcpth.rita-code  EQ 'R' 
          AND fg-rcpth.trans-date LE ipdtAsOf
        USE-INDEX job,
        EACH fg-rdtlh FIELDS(qty) NO-LOCK
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        :            
        opdQty = opdQty + fg-rdtlh.qty.
    END.  /*each fg history*/    

END PROCEDURE.

PROCEDURE pGetQuantityOnHandAsOf PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given Job inputs, determine the production quantity as of a particular date
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcINo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf   AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQty    AS DECIMAL   NO-UNDO.        
    
    FOR EACH fg-rcpth NO-LOCK
        WHERE fg-rcpth.company    EQ ipcCompany
          AND fg-rcpth.i-no       EQ ipcINo
          AND fg-rcpth.job-no     EQ ipcJobNo
          AND fg-rcpth.job-no2    EQ ipiJobNo2
          AND fg-rcpth.trans-date LE ipdtAsOf
        USE-INDEX tran,
        EACH fg-rdtlh NO-LOCK
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
           BY fg-rcpth.trans-date
           BY fg-rdtlh.trans-time
           BY fg-rcpth.r-no
        :
        CASE fg-rcpth.rita-code:
            WHEN "S" OR WHEN "s" THEN
            opdQty = opdQty - fg-rdtlh.qty.
            OTHERWISE 
            opdQty = opdQty + fg-rdtlh.qty.
        END CASE.    
    END.  /*each fg history*/    

END PROCEDURE.

{AOA/BL/pBuildCustList.i}
