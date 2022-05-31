
/*------------------------------------------------------------------------
    File        : itemToPurchase.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Apr 28 05:48:25 EDT 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    
DEFINE TEMP-TABLE ttJobMaterial NO-UNDO LIKE job-mat
    FIELD CreatePO      AS LOGICAL
    FIELD w-rowid       AS ROWID
    FIELD this-is-a-rm  AS LOGICAL
    FIELD isaset        AS LOGICAL
    FIELD isacomponent  AS LOGICAL
    FIELD fg-i-no       LIKE job-hdr.i-no
    FIELD est-no        LIKE eb.est-no
    FIELD eqty          LIKE eb.eqty
    FIELD prep          AS LOGICAL
    FIELD estPrepEQty   AS DECIMAL
    FIELD estPrepLine   AS INTEGER
    FIELD miscType      AS INTEGER
    FIELD miscInd       AS CHARACTER
    FIELD fg-part-no    AS CHARACTER
    FIELD CostPerUOM    AS DECIMAL 
    FIELD CostUOM       AS CHARACTER
    FIELD CostSetup     AS DECIMAL 
    FIELD CostTotal     AS DECIMAL
    FIELD IsValid       AS LOGICAL 
    FIELD InvalidReason AS CHARACTER
    FIELD ItemName      AS CHARACTER
    FIELD ItemType      AS CHARACTER
    FIELD DropShipment  AS LOGICAL 
    FIELD vendorID      AS CHARACTER
    FIELD VendorName    AS CHARACTER    
    FIELD PODate        AS DATE
    FIELD PODueDate     AS DATE
    FIELD DropCustNo    AS CHARACTER 
    FIELD ShipChoice    AS CHARACTER
    FIELD shiptoRecId   AS RECID
    FIELD ShipToVendId  AS CHARACTER
    FIELD ShipId        AS CHARACTER 
    FIELD locode        AS CHARACTER.