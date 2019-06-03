
/*------------------------------------------------------------------------
    File        : ttInventory.i
    Purpose     : 

    Syntax      :

    Description : Temp-table definition for ttInventoryTag used in LoadtagProcs.p for generating loadtags

    Author(s)   : BV
    Created     : Sun Mar 03 18:38:28 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    
DEFINE {1} TEMP-TABLE ttInventoryStockPreLoadtag
    LIKE inventoryStock
    FIELD quantityTotal AS DECIMAL  
    FIELD quantityTotalRunning AS DECIMAL 
    FIELD countOfLoadtags AS INTEGER 
    FIELD countOfLabelsPerLoadtag AS INTEGER
    FIELD labelTemplate AS CHARACTER 
    FIELD labelProgram AS CHARACTER 
    .   

DEFINE {1} TEMP-TABLE ttInventoryStockLoadtag
     LIKE ttInventoryStockPreLoadtag
     FIELD countOfLabels AS INTEGER
     .    
     
DEFINE {1} TEMP-TABLE ttInventoryStockLoadtagWIP
    LIKE ttInventoryStockLoadtag
    FIELD nextMachineID AS CHARACTER 
    FIELD nextMachineName AS CHARACTER
    FIELD orderCustomerID AS CHARACTER
    FIELD orderCustomerName AS CHARACTER
    FIELD lastMachineName AS CHARACTER 
    FIELD rmItemName AS CHARACTER 
    .

DEFINE {1} TEMP-TABLE ttPrintInventoryStock
    FIELD inventoryStockID AS CHARACTER LABEL "InventoryStockID"
    FIELD stockIDAlias AS CHARACTER	LABEL "WIPTag"
    FIELD wipItemID AS CHARACTER LABEL "WIPItemID"
    FIELD jobNumber AS CHARACTER LABEL "JobNumber"
    FIELD jobRunNumber AS INTEGER LABEL "JobRunNumber"
    FIELD jobID AS CHARACTER LABEL "JobID"
    FIELD jobIDTrimmed AS CHARACTER LABEL "JobIDTrimmed"
    FIELD jobIDFullTrimmed AS CHARACTER LABEL "JobIDFullTrimmed"
    FIELD formNo AS INTEGER LABEL "Form"
    FIELD blankNo AS INTEGER LABEL "Blank"
    FIELD customerID AS CHARACTER LABEL "CustomerID"
    FIELD customerName AS CHARACTER LABEL "CustomerName"
    FIELD machineID AS CHARACTER LABEL "MachineID"
    FIELD machineName AS CHARACTER LABEL "MachineName"
    FIELD passNo AS INTEGER LABEL "Pass"
    FIELD nextMachineID AS CHARACTER LABEL "NextMachineID"
    FIELD nextMachineName AS CHARACTER LABEL "NextMachineName"
    FIELD quantity AS DECIMAL LABEL "Quantity"
    FIELD quantityPerSubUnit AS DECIMAL LABEL "QuantityPerSubUnit"
    FIELD quantitySubUnitsPerUnit AS INTEGER LABEL "QuantitySubUnitsPerUnit"
    FIELD quantityOfSubUnits AS INTEGER LABEL "QuantityOfSubUnits"
    FIELD quantityOfUnits AS INTEGER LABEL "QuantityOfUnits"
    FIELD quantityPartial AS DECIMAL LABEL "QuantityPartial"
    FIELD quantityUOM AS CHARACTER LABEL "QuantityUOM"
    FIELD copyNo AS INTEGER LABEL "CopyNo"
    FIELD copies AS CHARACTER LABEL "Copies"
    FIELD rmItemID AS CHARACTER LABEL "RMItemID"
    FIELD rmItemName AS CHARACTER LABEL "RMItemName"
    FIELD company AS CHARACTER LABEL "Company"
    .

DEFINE {1} TEMP-TABLE ttBrowseInventory
    LIKE ttInventoryStockLoadtagWIP
    .

DEFINE {1} TEMP-TABLE ttPhysicalBrowseInventory
    FIELD company                AS CHARACTER
    FIELD inventoryStockID       AS CHARACTER
    FIELD stockIDAlias           AS CHARACTER
    FIELD itemID                 AS CHARACTER
    FIELD quantity               AS DECIMAL
    FIELD origQuantity           AS DECIMAL
    FIELD locationID             AS CHARACTER
    FIELD warehouseID            AS CHARACTER
    FIELD location               AS CHARACTER
    FIELD origLocationID         AS CHARACTER
    FIELD origWarehouseID        AS CHARACTER
    FIELD origLocation           AS CHARACTER
    FIELD inventoryStatus        AS CHARACTER
    FIELD itemType               AS CHARACTER
    FIELD customerID             AS CHARACTER
    FIELD lastTransTime          AS DATETIME
    .

DEFINE TEMP-TABLE ttInventoryStockDetails NO-UNDO
    LIKE inventoryStock
    .

DEFINE {1} TEMP-TABLE ttPrintInventoryStockFG NO-UNDO
    FIELDS customerName       AS CHARACTER FORMAT "X(30)"       LABEL "CUSTOMER"
    FIELDS orderID            AS INTEGER   FORMAT ">>>>>9"      LABEL "ORDNUMBER"
    FIELDS jobID              AS CHARACTER                      LABEL "JOBNUMBER"
    FIELDS fgItemID           AS CHARACTER                      LABEL "ITEM"
    FIELDS custPartID         AS CHARACTER                      LABEL "CUSTPARTNO"
    FIELDS custPOID           AS CHARACTER                      LABEL "CUSTPONO"
    FIELDS quantityPerSubUnit AS INTEGER   FORMAT ">>>>9"       LABEL "PCS"
    FIELDS quantityOfSubUnits AS INTEGER   FORMAT "->,>>>,>>9"  LABEL "BUNDLE"
    FIELDS quantity           AS INTEGER   FORMAT "->,>>>,>>9"  LABEL "TOTAL"
    FIELDS shipNo             AS CHARACTER                      LABEL "SHIPCODE"
    FIELDS shipName           AS CHARACTER                      LABEL "SHIPNAME"
    FIELDS shipAdd1           AS CHARACTER                      LABEL "SHIPADD1"
    FIELDS shipAdd2           AS CHARACTER                      LABEL "SHIPADD2"
    FIELDS shipCity           AS CHARACTER                      LABEL "SHIPCITY"
    FIELDS shipState          AS CHARACTER                      LABEL "SHIPSTATE"
    FIELDS shipCtry           AS CHARACTER                      LABEL "SHIPCOUNTRY"
    FIELDS shipZip            AS CHARACTER                      LABEL "SHIPZIP"
    FIELDS soldCode           AS CHARACTER FORMAT ">>9"         LABEL "SOLDCODE"
    FIELDS soldName           AS CHARACTER                      LABEL "SOLDNAME"
    FIELDS soldAdd1           AS CHARACTER                      LABEL "SOLDADD1"
    FIELDS soldAdd2           AS CHARACTER                      LABEL "SOLDADD2"
    FIELDS soldCity           AS CHARACTER                      LABEL "SOLDCITY"
    FIELDS soldState          AS CHARACTER                      LABEL "SOLDSTATE"
    FIELDS soldCtry           AS CHARACTER                      LABEL "SOLDCOUNTRY"
    FIELDS soldZip            AS CHARACTER                      LABEL "SOLDZIP"
    FIELDS fgItemName         AS CHARACTER                      LABEL "INAME"
    FIELDS dueDate            AS DATE                           LABEL "DUEDATE"
    FIELDS relDate            AS DATE                           LABEL "RELDATE"
    FIELDS upcID              AS CHARACTER                      LABEL "UPCNO"
    FIELDS inventoryStockLen  AS DECIMAL   FORMAT ">>>9.99<<<"  LABEL "LENGTH"
    FIELDS inventoryStockWid  AS DECIMAL   FORMAT ">>>9.99<<<"  LABEL "WIDTH"
    FIELDS inventoryStockDep  AS DECIMAL   FORMAT ">>>9.99<<<"  LABEL "DEPTH"
    FIELDS flute              AS CHARACTER                      LABEL "FLUTE"
    FIELDS test               AS CHARACTER                      LABEL "TEST"
    FIELDS vendor             AS CHARACTER                      LABEL "VENDOR"
    FIELDS weightTotal        AS DECIMAL   FORMAT ">>>>9.99"    LABEL "GROSSWGT"
    FIELDS weightTare         AS DECIMAL   FORMAT ">>>>9.99"    LABEL "TAREWGT"
    FIELDS weightNet          AS DECIMAL   FORMAT ">>>>9.99"    LABEL "NETWGT"
    FIELDS sheetWt            AS DECIMAL   FORMAT ">>>9.99"     LABEL "SHEETWGT"
    FIELDS uom                AS CHARACTER                      LABEL "UOM"
    FIELDS style              AS CHARACTER                      LABEL "STYLE"
    FIELDS styleDesc          AS CHARACTER FORMAT "X(30)"       LABEL "STYLEDESC"
    FIELDS relLotNo           AS CHARACTER                      LABEL "RELLOTNO"
    FIELDS middleSexJobID     AS CHARACTER                      LABEL "MIDDLESEXJOBNUMBER"
    FIELDS middleSexCustPOID  AS CHARACTER                      LABEL "MIDDLESEXCUSTPONO"
    FIELDS stockIDAlias       AS CHARACTER                      LABEL "TAG#"
    FIELDS quantityPartial    AS INTEGER   FORMAT ">>>,>>9"     LABEL "PARTIAL"
    FIELDS caseID             AS CHARACTER                      LABEL "CASECODE"
    FIELDS sn                 AS CHARACTER                      LABEL "SN"                 EXTENT 8
    FIELDS poID               AS INTEGER   FORMAT ">>>>>9"      LABEL "PONO"
    FIELDS dn                 AS CHARACTER                      LABEL "DN"                 EXTENT 10
    FIELDS estID              AS CHARACTER                      LABEL "EST#"
    FIELDS partDesc1          AS CHARACTER FORMAT "X(30)"       LABEL "ORDDESC1"
    FIELDS partDesc2          AS CHARACTER FORMAT "X(30)"       LABEL "ORDDESC2"
    FIELDS counterID          AS INTEGER                        LABEL "COUNTER#"
    FIELDS rfIDTag            AS CHARACTER                      LABEL "RFIDTag"
    FIELDS dueDateJobHdr      AS CHARACTER                      LABEL "DUEDATEJOBLINE"
    FIELDS dueDateJob         AS CHARACTER                      LABEL "DUEDATEJOB"
    FIELDS linenum            AS INTEGER   FORMAT ">>>>>9"      LABEL "LINE#"
    FIELDS unitWeight         AS DECIMAL   FORMAT "->>,>>9.99"  LABEL "UnitWt"
    FIELDS palletWeight       AS DECIMAL   FORMAT "->>,>>9.99"  LABEL "PalletWt"
    FIELDS fgPartDesc1        AS CHARACTER                      LABEL "FGdesc1"
    FIELDS fgPartDesc2        AS CHARACTER                      LABEL "FGdesc2"
    FIELDS fgPartDesc3        AS CHARACTER                      LABEL "FGdesc3"
    FIELDS fgLotNo            AS CHARACTER                      LABEL "FG Lot#"
    FIELDS palletNo           AS CHARACTER                      LABEL "PalletCode"
    FIELDS palletID           AS INTEGER   FORMAT "->,>>>,>>9"  LABEL "PalletID"
    FIELDS tagCounter         AS INTEGER                        LABEL "TagCounter"
    FIELDS tagCountTotal      AS INTEGER                        LABEL "TagCountTotal"
    FIELDS rn                 AS CHARACTER                      LABEL "RN"                 EXTENT 4
    FIELDS warehouseID        AS CHARACTER                      LABEL "WareHouse"
    FIELDS locationID         AS CHARACTER                      LABEL "Bin"
    FIELDS quantityJob        AS DECIMAL   FORMAT ">>,>>>,>>9"  LABEL "JobQty"
    FIELDS runShip            AS CHARACTER                      LABEL "RunShip"
    FIELDS palletType         AS CHARACTER                      LABEL "Pallet type"
    FIELDS zone               AS CHARACTER                      LABEL "Zone"
    FIELDS sscc               AS CHARACTER                      LABEL "SSCC"
    .
    		       	    
DEFINE VARIABLE gcStatusStockPreLoadtag    AS CHARACTER INITIAL "PreLoadtag".
DEFINE VARIABLE gcStatusStockLoadtag       AS CHARACTER INITIAL "Loadtag".
DEFINE VARIABLE gcStatusStockInitial       AS CHARACTER INITIAL "Created".
DEFINE VARIABLE gcStatusStockReceived      AS CHARACTER INITIAL "On-Hand".
DEFINE VARIABLE gcStatusStockConsumed      AS CHARACTER INITIAL "Consumed".

DEFINE VARIABLE gcStatusSnapshotNotScanned      AS CHARACTER INITIAL "Not Scanned".
DEFINE VARIABLE gcStatusSnapshotNotScannedConf  AS CHARACTER INITIAL "Not Scanned - Confirmed".
DEFINE VARIABLE gcStatusSnapshotCompleteMatch   AS CHARACTER INITIAL "Complete Match".
DEFINE VARIABLE gcStatusSnapshotLocChange       AS CHARACTER INITIAL "Location Change".
DEFINE VARIABLE gcStatusSnapshotQtyChange       AS CHARACTER INITIAL "Quantity Change".
DEFINE VARIABLE gcStatusSnapshotQtyAndLocChange AS CHARACTER INITIAL "Quantity and Location Change".
DEFINE VARIABLE gcStatusSnapshotTagNotFound     AS CHARACTER INITIAL "Tag Not Found".

DEFINE VARIABLE gcSourceTypeSnapshot       AS CHARACTER INITIAL "Snapshot".

DEFINE VARIABLE gcStatusTransactionInitial AS CHARACTER INITIAL "Pending".
DEFINE VARIABLE gcStatusTransactionPosted  AS CHARACTER INITIAL "Posted".

DEFINE VARIABLE gcTransactionTypeReceive   AS CHARACTER INITIAL "R".
DEFINE VARIABLE gcTransactionTypeTransfer  AS CHARACTER INITIAL "T".
DEFINE VARIABLE gcTransactionTypeAdjustQty AS CHARACTER INITIAL "A".
DEFINE VARIABLE gcTransactionTypeConsume   AS CHARACTER INITIAL "I".
DEFINE VARIABLE gcTransactionTypeShip      AS CHARACTER INITIAL "S".
DEFINE VARIABLE gcTransactionTypeCompare   AS CHARACTER INITIAL "C".

DEFINE VARIABLE gcSnapshotTypeCount        AS CHARACTER INITIAL "C". /* Count */
DEFINE VARIABLE gcSnapshotTypeCapture      AS CHARACTER INITIAL "R". /* Report Capture */
DEFINE VARIABLE gcSnapshotTypeArchive      AS CHARACTER INITIAL "A". /* Archive */

DEFINE VARIABLE gcItemTypeWIP              AS CHARACTER INITIAL "WP".
DEFINE VARIABLE gcItemTypeFG               AS CHARACTER INITIAL "FG".
DEFINE VARIABLE gcItemTypeRM               AS CHARACTER INITIAL "RM".

DEFINE VARIABLE gcDBUser                   AS CHARACTER INITIAL "asi".    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
