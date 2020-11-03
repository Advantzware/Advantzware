
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
    FIELD tag AS CHARACTER	LABEL "WIPTag"
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
    FIELD locDscr           AS CHARACTER
    FIELD leadDays          AS INTEGER
    FIELD orderLevel        AS INTEGER
    FIELD orderMax          AS INTEGER
    FIELD orderMin          AS INTEGER
    FIELD quantityOnHand    AS DECIMAL
    FIELD quantityOnOrder   AS DECIMAL
    FIELD quantityAllocated AS DECIMAL
    FIELD quantityBackOrder AS DECIMAL
    FIELD quantityAvailable AS DECIMAL
    .

DEFINE {1} TEMP-TABLE ttPhysicalBrowseInventory
    FIELD company                AS CHARACTER
    FIELD inventoryStockID       AS CHARACTER
    FIELD tag                    AS CHARACTER
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
    LIKE ttBrowseInventory
    .

DEFINE TEMP-TABLE ttPOOrderLineDetails NO-UNDO
    LIKE po-ordl
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
    FIELDS tag                AS CHARACTER                      LABEL "TAG#"
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

DEFINE {1} TEMP-TABLE ttPrintInventoryStockRM NO-UNDO
    FIELDS tagID                  AS CHARACTER LABEL 'Tag#'                                                                  
    FIELDS acknowledgement        AS LOGICAL   LABEL 'Acknowledgement'           FORMAT "Yes/No"                             
    FIELDS accountNo              AS CHARACTER LABEL 'Account No'                                                            
    FIELDS shipToAddress          AS CHARACTER LABEL 'Ship To Address'                                                       EXTENT 2
    FIELDS blankNo                AS INTEGER   LABEL 'Blank #'                   FORMAT ">9"                                 
    FIELDS billTo                 AS CHARACTER LABEL 'Bill to'                                                               
    FIELDS buyer                  AS CHARACTER LABEL 'Buyer'                                                                 
    FIELDS shippingCarrier        AS CHARACTER LABEL 'Shipping Carrier'                                                      
    FIELDS city                   AS CHARACTER LABEL 'City'                                                                  
    FIELDS company                AS CHARACTER LABEL 'Company'                                                               
    FIELDS companyName            AS CHARACTER LABEL 'Company Name'                                                          
    FIELDS costs                  AS DECIMAL   LABEL 'Costs'                     FORMAT "->>,>>9.99<<<<"                     
    FIELDS consumptionQuantity    AS DECIMAL   LABEL 'Consumption Quantity'      FORMAT "->>,>>>,>>9.9<<<<<"                 
    FIELDS uom                    AS CHARACTER LABEL 'Unit of Measure'                                                       
    FIELDS contact                AS CHARACTER LABEL 'Contact'                                                               
    FIELDS unitCost               AS DECIMAL   LABEL 'Unit Cost'                 FORMAT "->>>,>>9.99<<<<"                    
    FIELDS currencyCode           AS CHARACTER LABEL 'Currency Code'                                                         EXTENT 2
    FIELDS customerNumber         AS CHARACTER LABEL 'Customer Number'                                                       
    FIELDS deleted                AS LOGICAL   LABEL 'Deleted?'                  FORMAT "Y/N"                                
    FIELDS discount               AS DECIMAL   LABEL 'Discount'                  FORMAT "->>>,>>9.99"                        
    FIELDS description            AS CHARACTER LABEL 'Description'                                                           EXTENT 2
    FIELDS requiredDate           AS DATE      LABEL 'Required Date'                                                         
    FIELDS exRate                 AS DECIMAL   LABEL 'ex-rate'                   FORMAT "->>,>>9.99<<<<<"                    
    FIELDS fobOriginDest          AS CHARACTER LABEL 'FOB Origin/Dest'                                                       
    FIELDS freightPayment         AS CHARACTER LABEL 'Freight Payment'                                                       
    FIELDS name                   AS CHARACTER LABEL 'Name'                                                                  
    FIELDS itemID                 AS CHARACTER LABEL 'Item#'                                                                 
    FIELDS itemType               AS LOGICAL   LABEL 'Item Type'                 FORMAT "R/F"                  INITIAL "Y"   
    FIELDS internalJobNumber      AS INTEGER   LABEL 'Internal Job Number'       FORMAT ">>>>>>9"                            
    FIELDS jobID                  AS CHARACTER LABEL 'Job Number'                                                            
    FIELDS jobID2                 AS INTEGER   LABEL 'Run #'                     FORMAT ">9"                                 
    FIELDS lastShipDate           AS DATE      LABEL 'Last Ship Date'                                                        
    FIELDS line                   AS INTEGER   LABEL 'Line'                      FORMAT "99"                                 
    FIELDS warehouseID            AS CHARACTER LABEL 'Whse'                                                                  
    FIELDS locationID             AS CHARACTER LABEL 'Bin'                                                                   
    FIELDS opened                 AS LOGICAL   LABEL 'opened'                    FORMAT "Open/Closed"                        
    FIELDS customerOrderNo        AS INTEGER   LABEL 'Customer Order Number'     FORMAT ">>>>>9"                             
    FIELDS orderQuantity          AS DECIMAL   LABEL 'Order Quantity'            FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS overrunPct             AS DECIMAL   LABEL 'Overrun %'                 FORMAT ">>9.99%"                            
    FIELDS partialQty             AS INTEGER   LABEL 'Partial Qty'               FORMAT ">>>,>>9"                            
    FIELDS dateChanged            AS DATE      LABEL 'Date Changed'                                                          
    FIELDS poDate                 AS DATE      LABEL 'PO Date'                                                               
    FIELDS poID                   AS INTEGER   LABEL 'PO Number'                 FORMAT ">>>>>9"                             
    FIELDS purchaseQuantityUom    AS CHARACTER LABEL 'Purchase Quantity Uom'                                                 
    FIELDS purchasedUOM           AS CHARACTER LABEL 'Purchased UOM'                                                         
    FIELDS printed                AS LOGICAL   LABEL 'Printed?'                  FORMAT "y/n"                                
    FIELDS purchaseCount          AS INTEGER   LABEL 'Purchase Count'            FORMAT ">,>>9"                              
    FIELDS receiptQty             AS DECIMAL   LABEL 'Receipt Qty'               FORMAT "->>>>>>9.9<<<<<"                    
    FIELDS received               AS LOGICAL   LABEL 'Received'                  FORMAT "y/n"                                
    FIELDS releaseQuantity        AS DECIMAL   LABEL 'Release Quantity'          FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS sheetLen               AS DECIMAL   LABEL 'Sheet Len'                 FORMAT ">>9.9999"                           
    FIELDS sheetNo                AS INTEGER   LABEL 'Sheet #'                   FORMAT ">9"                                 
    FIELDS sheetWid               AS DECIMAL   LABEL 'Sheet Wid'                 FORMAT ">>9.9999"                           
    FIELDS setupCharge            AS DECIMAL   LABEL 'Setup Charge'              FORMAT ">>,>>9.99"                          
    FIELDS shippingAddress        AS CHARACTER LABEL 'Shipping Address'                                                      EXTENT 2
    FIELDS shippingCity           AS CHARACTER LABEL 'Shipping City'                                                         
    FIELDS shippingInstructions   AS CHARACTER LABEL 'Shipping Instructions'                                                 EXTENT 4
    FIELDS shipTo                 AS CHARACTER LABEL 'Ship To'                                                               
    FIELDS shippingName           AS CHARACTER LABEL 'Shipping Name'                                                         
    FIELDS shipToNumber           AS INTEGER   LABEL 'Ship To Number'            FORMAT ">>9"                                
    FIELDS shippingState          AS CHARACTER LABEL 'Shipping State'                                                        
    FIELDS shippingZip            AS CHARACTER LABEL 'Shipping Zip'              FORMAT "xxxxx-xxxx"                         
    FIELDS specialInstructions    AS CHARACTER LABEL 'Special Instructions'                                                  EXTENT 4
    FIELDS stat                   AS CHARACTER LABEL 'Status'                                                                
    FIELDS state                  AS CHARACTER LABEL 'State'                                                                 
    FIELDS totalCost              AS DECIMAL   LABEL 'Total Cost'                FORMAT "->,>>>,>>9.99<<"                    
    FIELDS totalFreight           AS DECIMAL   LABEL 'Total Freight'             FORMAT "->>,>>9.99"                         
    FIELDS totalInvoiced          AS DECIMAL   LABEL 'Total Invoiced'            FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS totalReceived          AS DECIMAL   LABEL 'Total Received'            FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS totalQuantityReleased  AS DECIMAL   LABEL 'Total Quantity Released'   FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS tagDate                AS DATE      LABEL 'Tag Date'                                                              
    FIELDS tax                    AS DECIMAL   LABEL 'Tax'                       FORMAT "->,>>9.99"                          
    FIELDS salesTaxGroup          AS CHARACTER LABEL 'Sales Tax Group'                                                       
    FIELDS taxExemptNo            AS CHARACTER LABEL 'Tax Exempt No.'                                                        
    FIELDS paymentTerms           AS CHARACTER LABEL 'Payment Terms'                                                         
    FIELDS totalTags              AS INTEGER   LABEL 'Total Tags'                                                            
    FIELDS type                   AS CHARACTER LABEL 'Type'                                                    INITIAL "R"   
    FIELDS underrunPct            AS DECIMAL   LABEL 'Underrun %'                FORMAT ">>9.99%"                            
    FIELDS updatedDate            AS DATE      LABEL 'Updated Date'                                                          
    FIELDS updatedTime            AS INTEGER   LABEL 'Updated Time'              FORMAT "->,>>>,>>9"                         
    FIELDS vendorItemID           AS CHARACTER LABEL 'Vendor Item #'                                                         
    FIELDS vendorName             AS CHARACTER LABEL 'Vendor Name'                                                           
    FIELDS vendor                 AS CHARACTER LABEL 'Vendor'                                                                
    FIELDS zipCode                AS CHARACTER LABEL 'Zip Code'                                                              
    FIELDS firstMachine           AS CHARACTER LABEL 'First Machine'                                           INITIAL " "   
    FIELDS firstInternalMachine   AS CHARACTER LABEL 'First Internal Machine'                                                
    FIELDS firstPress             AS CHARACTER LABEL 'First Press'                                                           
    .
            		       	    
DEFINE VARIABLE gcStatusStockPreLoadtag    AS CHARACTER NO-UNDO INITIAL "PreLoadtag".
DEFINE VARIABLE gcStatusStockLoadtag       AS CHARACTER NO-UNDO INITIAL "Loadtag".
DEFINE VARIABLE gcStatusStockInitial       AS CHARACTER NO-UNDO INITIAL "Created".
DEFINE VARIABLE gcStatusStockReceived      AS CHARACTER NO-UNDO INITIAL "On-Hand".
DEFINE VARIABLE gcStatusStockConsumed      AS CHARACTER NO-UNDO INITIAL "Consumed".
DEFINE VARIABLE gcStatusStockScanned       AS CHARACTER NO-UNDO INITIAL "Scanned".

DEFINE TEMP-TABLE ttRawMaterialsToPost NO-UNDO 
    FIELD parentRowID AS ROWID
    FIELD rmRctdRowID AS ROWID
    FIELD ritaCode    AS CHARACTER
    FIELD itemID      AS CHARACTER
    FIELD poID        AS CHARACTER
    FIELD formNo      AS INTEGER
    FIELD quantity    AS DECIMAL 
    FIELD vendorTag   AS CHARACTER
    FIELD sequenceID  AS INTEGER
    FIELD processed   AS LOGICAL
    .

DEFINE TEMP-TABLE ttRawMaterialsGLTransToPost NO-UNDO
    FIELD accountNo     AS CHARACTER
    FIELD job           AS INTEGER
    FIELD jobNo         AS CHARACTER
    FIELD jobNo2        AS INTEGER
    FIELD errorDesc     AS CHARACTER
    FIELD debitsAmount  AS DECIMAL
    FIELD creditsAmount AS DECIMAL
    INDEX accountNo accountNo
    .

DEFINE VARIABLE gcStatusSnapshotNotScanned      AS CHARACTER NO-UNDO INITIAL "Not Scanned".
DEFINE VARIABLE gcStatusSnapshotNotScannedConf  AS CHARACTER NO-UNDO INITIAL "Not Scanned - Confirmed".
DEFINE VARIABLE gcStatusSnapshotCompleteMatch   AS CHARACTER NO-UNDO INITIAL "Complete Match".
DEFINE VARIABLE gcStatusSnapshotLocChange       AS CHARACTER NO-UNDO INITIAL "Location Change".
DEFINE VARIABLE gcStatusSnapshotQtyChange       AS CHARACTER NO-UNDO INITIAL "Quantity Change".
DEFINE VARIABLE gcStatusSnapshotQtyAndLocChange AS CHARACTER NO-UNDO INITIAL "Quantity and Location Change".
DEFINE VARIABLE gcStatusSnapshotTagNotFound     AS CHARACTER NO-UNDO INITIAL "Tag Not Found".

DEFINE VARIABLE gcSourceTypeSnapshot       AS CHARACTER NO-UNDO INITIAL "Snapshot".

DEFINE VARIABLE gcStatusTransactionInitial AS CHARACTER NO-UNDO INITIAL "Pending".
DEFINE VARIABLE gcStatusTransactionPosted  AS CHARACTER NO-UNDO INITIAL "Posted".

DEFINE VARIABLE gcTransactionTypeReceive   AS CHARACTER NO-UNDO INITIAL "R".
DEFINE VARIABLE gcTransactionTypeTransfer  AS CHARACTER NO-UNDO INITIAL "T".
DEFINE VARIABLE gcTransactionTypeAdjustQty AS CHARACTER NO-UNDO INITIAL "A".
DEFINE VARIABLE gcTransactionTypeConsume   AS CHARACTER NO-UNDO INITIAL "I".
DEFINE VARIABLE gcTransactionTypeShip      AS CHARACTER NO-UNDO INITIAL "S".
DEFINE VARIABLE gcTransactionTypeCompare   AS CHARACTER NO-UNDO INITIAL "C".
DEFINE VARIABLE gcTransactionTypeReturns   AS CHARACTER NO-UNDO INITIAL "E".

DEFINE VARIABLE gcSnapshotTypeCount        AS CHARACTER NO-UNDO INITIAL "C". /* Count */
DEFINE VARIABLE gcSnapshotTypeCapture      AS CHARACTER NO-UNDO INITIAL "R". /* Report Capture */
DEFINE VARIABLE gcSnapshotTypeArchive      AS CHARACTER NO-UNDO INITIAL "A". /* Archive */

DEFINE VARIABLE gcItemTypeWIP              AS CHARACTER NO-UNDO INITIAL "WP".
DEFINE VARIABLE gcItemTypeFG               AS CHARACTER NO-UNDO INITIAL "FG".
DEFINE VARIABLE gcItemTypeRM               AS CHARACTER NO-UNDO INITIAL "RM".

DEFINE VARIABLE gcDBUser                   AS CHARACTER NO-UNDO INITIAL "asi".
DEFINE VARIABLE gcFGUOM                    AS CHARACTER NO-UNDO INITIAL "EA".
DEFINE VARIABLE gcUOMInches                AS CHARACTER NO-UNDO INITIAL "IN".
DEFINE VARIABLE gcUOMWeightBasis           AS CHARACTER NO-UNDO INITIAL "C".
DEFINE VARIABLE gcUOMWeightBasisLBSPerSQFT AS CHARACTER NO-UNDO INITIAL "LBS/SQFT".
DEFINE VARIABLE gcUOMWeightPound           AS CHARACTER NO-UNDO INITIAL "LBS".
DEFINE VARIABLE gcUOMWeight                AS CHARACTER NO-UNDO INITIAL "LB".

DEFINE VARIABLE gcInventorySourceTypeJob   AS CHARACTER NO-UNDO INITIAL "Job".
DEFINE VARIABLE gcInventorySourceTypePO    AS CHARACTER NO-UNDO INITIAL "PO".
DEFINE VARIABLE gcInventorySourceTypeFG    AS CHARACTER NO-UNDO INITIAL "FG".

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
