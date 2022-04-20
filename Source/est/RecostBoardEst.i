
/*------------------------------------------------------------------------
    File        : RecostBoardEst.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Varun
    Created     : Fri Feb 04 02:39:14 EST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttRecostBoardGroups NO-UNDO
    FIELD VendNo         LIKE estCostMaterial.vendorID
    FIELD INo            LIKE estCostMaterial.itemID
    FIELD ItemName       AS CHARACTER
    FIELD Len            LIKE eb.len
    FIELD Wid            LIKE eb.Wid
    FIELD Dep            LIKE eb.dep
    FIELD Scores         AS CHARACTER
    FIELD Adders         AS CHARACTER
    FIELD AdderCost      AS DECIMAL
    FIELD TotalQty       AS DECIMAL
    FIELD TotalQtyUOM    AS CHARACTER
    FIELD NewCost        LIKE estCostMaterial.costTotal
    FIELD NewCostUOM     LIKE estCostMaterial.costUOM
    FIELD NewSetup       LIKE estCostMaterial.costSetup
    FIELD UOM            LIKE estCostMaterial.dimUOM
    FIELD Multi          AS LOG
    FIELD UpdateCost     AS LOG
    FIELD BasisWeight    LIKE estCostMaterial.basisWeight
    FIELD BasisWeightUOM LIKE estCostMaterial.basisWeightUOM
    FIELD LineCount      AS INTEGER
    FIELD itemType       AS CHARACTER
    FIELD customerID     AS CHARACTER
    FIELD quantityMaster AS INTEGER
    FIELD CompanyId      AS CHARACTER
    FIELD FormIdList     AS CHARACTER
    .

DEFINE TEMP-TABLE ttRecostBoardLineXRef /*allows for easy re-finding of EstCostMaterial*/ NO-UNDO 
    FIELD RecostBoardGroupRowId AS ROWID
    FIELD EstCostMaterialID     AS INT64
    .
        
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
