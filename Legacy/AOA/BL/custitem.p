/*------------------------------------------------------------------------
  File: custitem.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Customer Item By Month.rpa */
{aoa/tempTable/ttCustomerItemByMonth.i}
{aoa/tempTable/ttCustomerItemByMonthDetail.i}
{aoa/tempTable/ttCustomerItemByMonthSummary.i}

{sys/ref/CustList.i NEW}

/* Parameters Definitions */
DEFINE OUTPUT PARAMETER TABLE FOR ttCustomerItemByMonth.
{aoa/includes/pCustomerItemByMonth.i}

/* Local Variables */

/* Business Logic */       
FOR EACH ar-inv NO-LOCK 
    WHERE ar-inv.company  EQ ipcCompany
      AND ar-inv.posted   EQ YES
      AND ar-inv.inv-date GE dtStartInvoiceDate
      AND ar-inv.inv-date LE dtEndInvoiceDate
      AND ar-inv.cust-no  GE cStartCustNo
      AND ar-inv.cust-no  LE cEndCustNo
    USE-INDEX inv-date,
    FIRST cust NO-LOCK 
    WHERE cust.company EQ ar-inv.company
      AND cust.cust-no EQ ar-inv.cust-no,
    EACH  ar-invl NO-LOCK 
    WHERE ar-invl.x-no EQ ar-inv.x-no
      AND ar-invl.i-no GE cStartItemNo
      AND ar-invl.i-no LE cEndItemNo
      AND (ar-invl.billable EQ YES OR ar-invl.misc EQ NO)
    :
    IF lCustList AND
       NOT CAN-FIND(FIRST ttCustList
                    WHERE ttCustList.cust-no EQ ar-inv.cust-no
                      AND ttCustList.log-fld EQ TRUE) THEN
    NEXT.
    FIND FIRST itemfg NO-LOCK 
         WHERE itemfg.company EQ ar-inv.company
           AND itemfg.i-no    EQ ar-invl.i-no
         NO-ERROR.
    CREATE ttCustomerItemByMonthDetail.
    ASSIGN 
        ttCustomerItemByMonthDetail.custNo      = ar-inv.cust-no
        ttCustomerItemByMonthDetail.custName    = cust.name
        ttCustomerItemByMonthDetail.itemNo      = ar-invl.i-no
        ttCustomerItemByMonthDetail.itemDscr    = IF AVAILABLE itemfg THEN itemfg.i-name  ELSE ""
        ttCustomerItemByMonthDetail.custPart    = IF AVAILABLE itemfg THEN itemfg.part-no ELSE ""
        ttCustomerItemByMonthDetail.invDate     = ar-inv.inv-date
        ttCustomerItemByMonthDetail.invTotal    = ar-inv.gross
        ttCustomerItemByMonthDetail.qtyInvoiced = ar-invl.qty
        ttCustomerItemByMonthDetail.qtyShipped  = ar-invl.ship-qty
        .
END. /* each ar-inv */

RUN pCustomerItemByMonthSummary.
RUN pCustomerItemByMonth.

PROCEDURE pCreatettCustomerItemByMonthSummary:
    DEFINE INPUT PARAMETER ipcCustNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustName    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemDscr    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustPart    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiYear        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiMonth       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiInvTotal    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiQtyInvoiced AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiQtyShipped  AS INTEGER   NO-UNDO.
    
    FIND FIRST ttCustomerItemByMonthSummary
         WHERE ttCustomerItemByMonthSummary.custNo   EQ ipcCustNo
           AND ttCustomerItemByMonthSummary.itemNo   EQ ipcItemNo
           AND ttCustomerItemByMonthSummary.invYear  EQ ipiYear
           AND ttCustomerItemByMonthSummary.invMonth EQ ipiMonth
         NO-ERROR.
    IF NOT AVAILABLE ttCustomerItemByMonthSummary THEN DO:
        CREATE ttCustomerItemByMonthSummary.
        ASSIGN 
            ttCustomerItemByMonthSummary.custNo   = ipcCustNo
            ttCustomerItemByMonthSummary.custName = ipcCustName
            ttCustomerItemByMonthSummary.itemNo   = ipcItemNo
            ttCustomerItemByMonthSummary.itemDscr = ipcItemDscr
            ttCustomerItemByMonthSummary.custPart = ipcCustPart
            ttCustomerItemByMonthSummary.invYear  = ipiYear
            ttCustomerItemByMonthSummary.invMonth = ipiMonth
            .
    END. /* not avail */
    ASSIGN
        ttCustomerItemByMonthSummary.invTotal    = ttCustomerItemByMonthSummary.invTotal    + ipiInvTotal
        ttCustomerItemByMonthSummary.qtyInvoiced = ttCustomerItemByMonthSummary.qtyInvoiced + ipiQtyInvoiced
        ttCustomerItemByMonthSummary.qtyShipped  = ttCustomerItemByMonthSummary.qtyShipped  + ipiQtyShipped
        .
END PROCEDURE.

PROCEDURE pCustomerItemByMonth:
    DEFINE VARIABLE cMonth  AS CHARACTER NO-UNDO INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".
    DEFINE VARIABLE iOrder  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iExtent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemNo AS CHARACTER NO-UNDO.

    FIND FIRST ttCustomerItemByMonthSummary
         WHERE ttCustomerItemByMonthSummary.rowType EQ "Data"
         NO-ERROR.
    IF NOT AVAILABLE ttCustomerItemByMonthSummary THEN RETURN.
    ASSIGN 
        cCustNo = ttCustomerItemByMonthSummary.custNo
        cItemNo = ttCustomerItemByMonthSummary.itemNo        
        .    
    /* header line 1 */
    CREATE ttCustomerItemByMonth.
    ASSIGN 
        iOrder                        = iOrder + 1
        ttCustomerItemByMonth.xxOrder = iOrder
        iExtent                       = 0
        .
    FOR EACH ttCustomerItemByMonthSummary
        WHERE ttCustomerItemByMonthSummary.rowType  EQ "Data"
          AND ttCustomerItemByMonthSummary.custNo   EQ cCustNo
          AND ttCustomerItemByMonthSummary.itemNo   EQ cItemNo
           BY ttCustomerItemByMonthSummary.invYear  DESCENDING
           BY ttCustomerItemByMonthSummary.invMonth DESCENDING
        :
        iExtent = iExtent + 1.
        IF ttCustomerItemByMonthSummary.invMonth EQ 99 THEN
        RUN pMonthlyValues (iExtent, STRING(ttCustomerItemByMonthSummary.invYear,"9999"), "", "").
        IF ttCustomerItemByMonthSummary.invYear  EQ 0 THEN
        RUN pMonthlyValues (iExtent, "Grand Inv", "Grand Qty", "Grand Ship").
    END. /* each ttCustomerItemByMonth */ 

    /* header line 2 */
    CREATE ttCustomerItemByMonth.
    ASSIGN 
        iOrder                         = iOrder + 1
        ttCustomerItemByMonth.xxOrder  = iOrder
        ttCustomerItemByMonth.custNo   = "Customer"
        ttCustomerItemByMonth.custName = "Customer Name"
        ttCustomerItemByMonth.itemNo   = "FG Item"
        ttCustomerItemByMonth.itemDscr = "FG Description"
        ttCustomerItemByMonth.custPart = "Customer Part"
        iExtent                        = 0
        .
    FOR EACH ttCustomerItemByMonthSummary
        WHERE ttCustomerItemByMonthSummary.rowType  EQ "Data"
          AND ttCustomerItemByMonthSummary.custNo   EQ cCustNo
          AND ttCustomerItemByMonthSummary.itemNo   EQ cItemNo
           BY ttCustomerItemByMonthSummary.invYear  DESCENDING
           BY ttCustomerItemByMonthSummary.invMonth DESCENDING
        :
        iExtent = iExtent + 1.
        IF ttCustomerItemByMonthSummary.invMonth EQ 99 THEN
        RUN pMonthlyValues (iExtent, "Total Inv", "Total Qty", "Total Ship").
        IF ttCustomerItemByMonthSummary.invMonth NE 0 AND ttCustomerItemByMonthSummary.invMonth NE 99 THEN
        RUN pMonthlyValues (iExtent,
            ENTRY(ttCustomerItemByMonthSummary.invMonth,cMonth) + " Inv",
            ENTRY(ttCustomerItemByMonthSummary.invMonth,cMonth) + " Qty",
            ENTRY(ttCustomerItemByMonthSummary.invMonth,cMonth) + " Ship"
            ).
        IF ttCustomerItemByMonthSummary.invYear EQ 0 THEN
        RUN pMonthlyValues (iExtent, "Total Inv", "Total Qty", "Total Ship").
    END. /* each ttCustomerItemByMonth */ 

    /* detail lines */
    FOR EACH ttCustomerItemByMonthSummary
        WHERE ttCustomerItemByMonthSummary.rowType EQ "Data"
        BREAK BY ttCustomerItemByMonthSummary.custNo
              BY ttCustomerItemByMonthSummary.itemNo
              BY ttCustomerItemByMonthSummary.invYear  DESCENDING
              BY ttCustomerItemByMonthSummary.invMonth DESCENDING
        :
        IF FIRST-OF(ttCustomerItemByMonthSummary.itemNo) THEN DO:
            CREATE ttCustomerItemByMonth.
            ASSIGN 
                iOrder                         = iOrder + 1
                ttCustomerItemByMonth.xxOrder  = iOrder
                ttCustomerItemByMonth.custNo   = ttCustomerItemByMonthSummary.custNo 
                ttCustomerItemByMonth.custName = ttCustomerItemByMonthSummary.custName
                ttCustomerItemByMonth.itemNo   = ttCustomerItemByMonthSummary.itemNo 
                ttCustomerItemByMonth.itemDscr = ttCustomerItemByMonthSummary.itemDscr
                ttCustomerItemByMonth.custPart = ttCustomerItemByMonthSummary.custPart
                iExtent                        = 0
                .
            IF ttCustomerItemByMonthSummary.custNo EQ "Grand Totals" THEN
            ttCustomerItemByMonth.xxOrder = 999999.
        END. /* first-of */
        iExtent = iExtent + 1.
        RUN pMonthlyValues (iExtent,
            IF ttCustomerItemByMonthSummary.invTotal NE 0 THEN
            STRING(ttCustomerItemByMonthSummary.invTotal,"->>,>>>,>>9.99") ELSE "",
            IF ttCustomerItemByMonthSummary.qtyInvoiced NE 0 THEN 
            STRING(ttCustomerItemByMonthSummary.qtyInvoiced,"->,>>>,>>>,>>9") ELSE "",
            IF ttCustomerItemByMonthSummary.qtyShipped NE 0 THEN 
            STRING(ttCustomerItemByMonthSummary.qtyShipped,"->,>>>,>>>,>>9") ELSE ""
            ).
    END. /* each ttCustomerItemByMonth */
END PROCEDURE.

PROCEDURE pCustomerItemByMonthSummary:
    DEFINE VARIABLE dDate AS DATE NO-UNDO.
    
    FOR EACH ttCustomerItemByMonthDetail
        BREAK BY ttCustomerItemByMonthDetail.custNo
              BY ttCustomerItemByMonthDetail.itemNo
        :
        IF FIRST-OF(ttCustomerItemByMonthDetail.itemNo) THEN DO:
            /* prebuild all the records needed from 1st month to last month */
            DO dDate = dtEndInvoiceDate TO dtStartInvoiceDate BY -1:
                /* individual month/year cell for an item */
                RUN pCreatettCustomerItemByMonthSummary (
                    ttCustomerItemByMonthDetail.custNo,
                    ttCustomerItemByMonthDetail.custName,
                    ttCustomerItemByMonthDetail.itemNo,
                    ttCustomerItemByMonthDetail.itemDscr,
                    ttCustomerItemByMonthDetail.custPart,
                    YEAR(dDate),MONTH(dDate),0,0,0
                    ).
                /* year total for an item */
                RUN pCreatettCustomerItemByMonthSummary (
                    ttCustomerItemByMonthDetail.custNo,
                    ttCustomerItemByMonthDetail.custName,
                    ttCustomerItemByMonthDetail.itemNo,
                    ttCustomerItemByMonthDetail.itemDscr,
                    ttCustomerItemByMonthDetail.custPart,
                    YEAR(dDate),99,0,0,0
                    ).
                /* grand total for an item */
                RUN pCreatettCustomerItemByMonthSummary (
                    ttCustomerItemByMonthDetail.custNo,
                    ttCustomerItemByMonthDetail.custName,
                    ttCustomerItemByMonthDetail.itemNo,
                    ttCustomerItemByMonthDetail.itemDscr,
                    ttCustomerItemByMonthDetail.custPart,
                    0,0,0,0,0
                    ).
                /* grand totals for all items */
                RUN pCreatettCustomerItemByMonthSummary (
                    "Grand Totals","","","","",
                    YEAR(dDate),MONTH(dDate),0,0,0
                    ).
                RUN pCreatettCustomerItemByMonthSummary (
                    "Grand Totals","","","","",
                    YEAR(dDate),99,0,0,0
                    ).
                RUN pCreatettCustomerItemByMonthSummary (
                    "Grand Totals","","","","",
                    0,0,0,0,0
                    ).
            END. /* do ddate */
        END. /* if first-of */
        /* individual month/year cell for an item */
        RUN pCreatettCustomerItemByMonthSummary (
            ttCustomerItemByMonthDetail.custNo,
            ttCustomerItemByMonthDetail.custName,
            ttCustomerItemByMonthDetail.itemNo,
            ttCustomerItemByMonthDetail.itemDscr,
            ttCustomerItemByMonthDetail.custPart,
            YEAR(ttCustomerItemByMonthDetail.invDate),
            MONTH(ttCustomerItemByMonthDetail.invDate),
            ttCustomerItemByMonthDetail.invTotal,
            ttCustomerItemByMonthDetail.qtyInvoiced,
            ttCustomerItemByMonthDetail.qtyShipped
            ).
        /* year total for an item */
        RUN pCreatettCustomerItemByMonthSummary (
            ttCustomerItemByMonthDetail.custNo,
            ttCustomerItemByMonthDetail.custName,
            ttCustomerItemByMonthDetail.itemNo,
            ttCustomerItemByMonthDetail.itemDscr,
            ttCustomerItemByMonthDetail.custPart,
            YEAR(ttCustomerItemByMonthDetail.invDate),99,
            ttCustomerItemByMonthDetail.invTotal,
            ttCustomerItemByMonthDetail.qtyInvoiced,
            ttCustomerItemByMonthDetail.qtyShipped
            ).
        /* grand total for an item */
        RUN pCreatettCustomerItemByMonthSummary (
            ttCustomerItemByMonthDetail.custNo,
            ttCustomerItemByMonthDetail.custName,
            ttCustomerItemByMonthDetail.itemNo,
            ttCustomerItemByMonthDetail.itemDscr,
            ttCustomerItemByMonthDetail.custPart,0,0,
            ttCustomerItemByMonthDetail.invTotal,
            ttCustomerItemByMonthDetail.qtyInvoiced,
            ttCustomerItemByMonthDetail.qtyShipped
            ).
        /* grand totals for all items */
        RUN pCreatettCustomerItemByMonthSummary (
            "Grand Totals","","","","",
            YEAR(ttCustomerItemByMonthDetail.invDate),
            MONTH(ttCustomerItemByMonthDetail.invDate),
            ttCustomerItemByMonthDetail.invTotal,
            ttCustomerItemByMonthDetail.qtyInvoiced,
            ttCustomerItemByMonthDetail.qtyShipped
            ).
        RUN pCreatettCustomerItemByMonthSummary (
            "Grand Totals","","","","",
            YEAR(ttCustomerItemByMonthDetail.invDate),99,
            ttCustomerItemByMonthDetail.invTotal,
            ttCustomerItemByMonthDetail.qtyInvoiced,
            ttCustomerItemByMonthDetail.qtyShipped
            ).
        RUN pCreatettCustomerItemByMonthSummary (
            "Grand Totals","","","","",0,0,
            ttCustomerItemByMonthDetail.invTotal,
            ttCustomerItemByMonthDetail.qtyInvoiced,
            ttCustomerItemByMonthDetail.qtyShipped
            ).
    END. /* each ttCustomerItemByMonthDetail */ 
END PROCEDURE.

PROCEDURE pMonthlyValues:
    DEFINE INPUT PARAMETER ipiExtent      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcInvTotal    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcQtyInvoiced AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcQtyShipped  AS CHARACTER NO-UNDO.
    
    CASE ipiExtent:
        {AOA/BL/custitem.i 1}
        {AOA/BL/custitem.i 2}
        {AOA/BL/custitem.i 3}
        {AOA/BL/custitem.i 4}
        {AOA/BL/custitem.i 5}
        {AOA/BL/custitem.i 6}
        {AOA/BL/custitem.i 7}
        {AOA/BL/custitem.i 8}
        {AOA/BL/custitem.i 9}
        {AOA/BL/custitem.i 10}
        {AOA/BL/custitem.i 11}
        {AOA/BL/custitem.i 12}
        {AOA/BL/custitem.i 13}
        {AOA/BL/custitem.i 14}
        {AOA/BL/custitem.i 15}
        {AOA/BL/custitem.i 16}
        {AOA/BL/custitem.i 17}
        {AOA/BL/custitem.i 18}
        {AOA/BL/custitem.i 19}
        {AOA/BL/custitem.i 20}
        {AOA/BL/custitem.i 21}
        {AOA/BL/custitem.i 22}
        {AOA/BL/custitem.i 23}
        {AOA/BL/custitem.i 24}
        {AOA/BL/custitem.i 25}
        {AOA/BL/custitem.i 26}
        {AOA/BL/custitem.i 27}
    END CASE.
END PROCEDURE. 
