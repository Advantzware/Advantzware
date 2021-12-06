
/*------------------------------------------------------------------------
    File        : VendorCostProcs.i
    Purpose     : 

    Syntax      :

    Description : Include File for temp-table definitions for VendorCostProcs.p

    Author(s)   : BV
    Created     : Tue Sep 24 14:51:55 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttVendItemCost LIKE vendItemCost
    FIELD isExpired AS LOGICAL
    FIELD isNotEffective AS LOGICAL
    FIELD quantityTarget AS DECIMAL 
    FIELD quantityTargetUOM AS CHARACTER
    FIELD quantityTargetInVendorUOM AS DECIMAL 
    FIELD costPerVendorUOM AS DECIMAL
    FIELD costPerVendorUOMBase AS DECIMAL 
    FIELD costPerVendorUOMUpcharge AS DECIMAL
    FIELD costSetup AS DECIMAL
    FIELD costTotal AS DECIMAL
    FIELD dimLengthInVendorDimUOM AS DECIMAL
    FIELD dimWidthInVendorDimUOM AS DECIMAL
    FIELD dimDepthInVendorDimUOM AS DECIMAL
    FIELD costDeviation AS DECIMAL
    FIELD isValid AS LOGICAL
    FIELD isSelected AS LOGICAL 
    FIELD reasonNotValid AS CHARACTER
    .
    
    /*Table used to get the cost and the cost will be appended to the existing vedor record*/
DEFINE TEMP-TABLE ttVendItemCostFC LIKE vendItemCost
    FIELD isExpired AS LOGICAL
    FIELD isNotEffective AS LOGICAL
    FIELD quantityTarget AS DECIMAL 
    FIELD quantityTargetUOM AS CHARACTER
    FIELD quantityTargetInVendorUOM AS DECIMAL 
    FIELD costPerVendorUOM AS DECIMAL
    FIELD costPerVendorUOMBase AS DECIMAL 
    FIELD costPerVendorUOMUpcharge AS DECIMAL
    FIELD costSetup AS DECIMAL
    FIELD costTotal AS DECIMAL
    FIELD dimLengthInVendorDimUOM AS DECIMAL
    FIELD dimWidthInVendorDimUOM AS DECIMAL
    FIELD dimDepthInVendorDimUOM AS DECIMAL
    FIELD costDeviation AS DECIMAL
    FIELD isValid AS LOGICAL
    FIELD isSelected AS LOGICAL 
    FIELD reasonNotValid AS CHARACTER
    .
    
DEFINE TEMP-TABLE ttItemAvail
    FIELD ItemID    AS CHARACTER
    FIELD vendorID  AS CHARACTER
    FIELD vendCount AS INTEGER.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
