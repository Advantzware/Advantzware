/*------------------------------------------------------------------------
    File        : testers/FGInventoryRptTester.p
    Purpose     : 

    Syntax      :

    Description : Tester to screen to replicate the IR2 report with new 
                  inventoryStock tables. This uses dynamic query to browse
                  through records, as dynamic query optimizes (which will
                  help Prgress in picking the right index) the query
                  before executing.
    Author(s)   : Mithun Porandla
    Created     : 01/16/2020
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttBins No-UNDO    
    FIELD company AS CHARACTER
    FIELD i-no    AS CHARACTER
    FIELD tag     AS CHARACTER
    FIELD loc     AS CHARACTER
    FIELD loc-bin AS CHARACTER
    FIELD cust-no AS CHARACTER
    FIELD stockID AS CHARACTER
    FIELD qty     AS DECIMAL
    INDEX tt-fg-bin IS PRIMARY
        company
        i-no
        tag
        loc
        loc-bin
        cust-no    
    .

DEFINE VARIABLE daAsOfDate      AS DATE      NO-UNDO INITIAL 01/18/2020.
DEFINE VARIABLE cCompany        AS CHARACTER NO-UNDO INITIAL "001".
DEFINE VARIABLE cStartItem      AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE cEndItem        AS CHARACTER NO-UNDO INITIAL "ZZZZZZZZZZZZ".
DEFINE VARIABLE cStartWarehouse AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE cEndWarehouse   AS CHARACTER NO-UNDO INITIAL "ZZZZZ".
DEFINE VARIABLE cStartLocation  AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE cEndLocation    AS CHARACTER NO-UNDO INITIAL "ZZZZZZZZZZ".
DEFINE VARIABLE cStartCustomer  AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE cEndCustomer    AS CHARACTER NO-UNDO INITIAL "ZZZZZZZZZZZZ".

DEFINE VARIABLE hdQuery      AS HANDLE    NO-UNDO.
DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtAsOfDate   AS DATETIME  NO-UNDO.
DEFINE VARIABLE dTotalQty    AS DECIMAL   NO-UNDO.

DEFINE BUFFER inventoryStock FOR inventoryStock.

CREATE QUERY hdQuery.

hdQuery:SET-BUFFERS(BUFFER inventoryStock:HANDLE).

dtAsOfDate = DATETIME(daAsOfDate, 86399).

/* Code to build the query for the given criteria */
cQueryString = "FOR EACH inventoryStock NO-LOCK WHERE "
             + "inventoryStock.company EQ '" + cCompany + "'".

IF cStartItem EQ cEndItem THEN
    cQueryString = cQueryString + " AND inventoryStock.fgItemID EQ '" + cStartItem  + "'".
ELSE
    cQueryString = cQueryString + " AND inventoryStock.fgItemID GE '" + cStartItem  + "'"
                 + " AND inventoryStock.fgItemID LE '" + cEndItem + "'".

IF cStartWarehouse EQ cEndWarehouse THEN
    cQueryString = cQueryString + " AND inventoryStock.warehouseID EQ '" + cStartWarehouse  + "'".
ELSE
    cQueryString = cQueryString + " AND inventoryStock.warehouseID GE '" + cStartWarehouse + "'"
                 + " AND inventoryStock.warehouseID LE '" + cEndWarehouse + "'".

IF cStartLocation EQ cEndLocation THEN
    cQueryString = cQueryString + " AND inventoryStock.LocationID EQ '" + cStartLocation  + "'".
ELSE
    cQueryString = cQueryString + " AND inventoryStock.LocationID GE '" + cStartLocation  + "'"
                 + " AND inventoryStock.LocationID LE '" + cEndLocation  + "'".

IF cStartCustomer EQ cEndCustomer THEN
    cQueryString = cQueryString + " AND inventoryStock.CustomerID EQ '" + cStartCustomer  + "'".
ELSE
    cQueryString = cQueryString + " AND inventoryStock.CustomerID GE '" + cStartCustomer  + "'"
                 + " AND inventoryStock.CustomerID LE '" + cEndCustomer  + "'".

IF daAsOfDate LT TODAY THEN
    cQueryString = cQueryString + " AND inventoryStock.createdTime LE DATETIME('" + STRING(dtAsOfDate) + "')".

hdQuery:QUERY-PREPARE(cQueryString).
hdQuery:QUERY-OPEN().

ETIME(YES).

hdQuery:GET-FIRST.

REPEAT:
    IF hdQuery:QUERY-OFF-END THEN
        LEAVE.
        
    FIND FIRST ttBins
         WHERE ttBins.company EQ inventoryStock.company
           AND ttBins.i-no    EQ inventoryStock.fgItemID
           AND ttBins.tag     EQ inventoryStock.stockIDAlias           
           AND ttBins.loc     EQ inventoryStock.warehouseID
           AND ttBins.loc-bin EQ inventoryStock.locationID
           AND ttBins.cust-no EQ inventoryStock.customerID
         NO-ERROR.
    IF NOT AVAILABLE ttBins THEN DO:
        CREATE ttBins.
        ASSIGN
            ttBins.company = inventoryStock.company
            ttBins.i-no    = inventoryStock.fgItemID
            ttBins.tag     = inventoryStock.stockIDAlias
            ttBins.loc     = inventoryStock.warehouseID
            ttBins.loc-bin = inventoryStock.locationID
            ttBins.cust-no = inventoryStock.customerID
            ttBins.stockID = inventoryStock.inventoryStockID
            .                    
    END.    
    
    IF daAsOfDate LT TODAY AND 
       (inventoryStock.consumedTime EQ ? OR inventoryStock.consumedTime GT dtAsOfDate) THEN DO:
        FOR EACH inventoryTransaction NO-LOCK
            WHERE inventoryTransaction.company           EQ inventoryStock.company
              AND inventoryTransaction.inventoryStockID  EQ inventoryStock.inventoryStockID
              AND inventoryTransaction.transactionStatus EQ "Posted"
              AND inventoryTransaction.transactionTime   LE dtAsOfDate
               BY inventoryTransaction.transactionTime
              :
            CASE inventoryTransaction.transactionType:
                WHEN "S" THEN
                    ttBins.qty = ttBins.qty - inventoryTransaction.quantityChange.
                WHEN "C" THEN
                    ttBins.qty = inventoryTransaction.quantityChange.
                OTHERWISE
                    ttBins.qty = ttBins.qty + inventoryTransaction.quantityChange.
            END CASE.
        END.
    END.
    ELSE
        ttBins.qty = inventoryStock.quantity.
    
    hdQuery:GET-NEXT.     
END.

MESSAGE "Process Complete" SKIP 
    "Total: " dTotalQty SKIP
    "Time: " STRING(ETIME / 1000) + "s"
    VIEW-AS ALERT-BOX.
