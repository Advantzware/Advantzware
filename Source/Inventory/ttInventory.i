
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
    
DEFINE VARIABLE gcStatusStockPreLoadtag    AS CHARACTER NO-UNDO INITIAL "PreLoadtag".
DEFINE VARIABLE gcStatusStockLoadtag       AS CHARACTER NO-UNDO INITIAL "Loadtag".
DEFINE VARIABLE gcStatusStockInitial       AS CHARACTER NO-UNDO INITIAL "Created".
DEFINE VARIABLE gcStatusStockReceived      AS CHARACTER NO-UNDO INITIAL "On-Hand".
DEFINE VARIABLE gcStatusStockConsumed      AS CHARACTER NO-UNDO INITIAL "Consumed".
DEFINE VARIABLE gcStatusStockScanned       AS CHARACTER NO-UNDO INITIAL "Scanned".

DEFINE VARIABLE gcStatusSnapshotNotScanned             AS CHARACTER NO-UNDO INITIAL "In Snapshot - Not Scanned".
DEFINE VARIABLE gcStatusSnapshotNotScannedConf         AS CHARACTER NO-UNDO INITIAL "Not Scanned - Confirmed".
DEFINE VARIABLE gcStatusSnapshotNotScannedCountingZero AS CHARACTER NO-UNDO INITIAL "Not Scanned - Counting 0".
DEFINE VARIABLE gcStatusSnapshotCompleteMatch          AS CHARACTER NO-UNDO INITIAL "Confirmed".
DEFINE VARIABLE gcStatusSnapshotLocChange              AS CHARACTER NO-UNDO INITIAL "Location Mismatch".
DEFINE VARIABLE gcStatusSnapshotQtyChange              AS CHARACTER NO-UNDO INITIAL "Quantity Mismatch".
DEFINE VARIABLE gcStatusSnapshotQtyAndLocChange        AS CHARACTER NO-UNDO INITIAL "Quantity and Location Mismatch".
DEFINE VARIABLE gcStatusSnapshotTagNotOnHand           AS CHARACTER NO-UNDO INITIAL "Tag not on hand".
DEFINE VARIABLE gcStatusSnapshotTagNotFound            AS CHARACTER NO-UNDO INITIAL "Error - Invalid Loadtag".

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

DEFINE VARIABLE gcInventorySourceTypeRMBIN     AS CHARACTER NO-UNDO INITIAL "RMBIN".
DEFINE VARIABLE gcInventorySourceTypeRMHISTORY AS CHARACTER NO-UNDO INITIAL "RMHISTORY".
DEFINE VARIABLE gcInventorySourceTypeRMRCTD    AS CHARACTER NO-UNDO INITIAL "RMRCTD".


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
