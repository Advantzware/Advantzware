
/*------------------------------------------------------------------------
    File        : ttInputOrd.i
    Purpose     : Include file that houses ttInputOrd

    Syntax      :

    Description : Include file for declaration of shared temp tables for 
                  order

    Author(s)   : Sewa Singh
    Created     : Thur Aug 19 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInputOrd like oe-ord
    .

DEFINE TEMP-TABLE ttInputOrdLine like oe-ordl
         FIELD cItemType as CHARACTER LABEL "Item Type" 
         FIELD cQtyUom AS CHARACTER LABEL "UOM" 
         FIELD lCreateRel AS LOGICAL LABEL "Create Release"
         FIELD lCreateJob AS LOGICAL LABEL "Create Job"
         FIELD lCreatePo AS LOGICAL LABEL "Create Po"
    .

DEFINE TEMP-TABLE ttEstItem 
           FIELD company AS CHARACTER
           FIELD estNo AS CHARACTER LABEL "Estimate"
           FIELD estLine AS INTEGER LABEL "Lin"
           FIELD estCust AS CHARACTER LABEL "Customer"
           FIELD estItem AS CHARACTER LABEL "Item Id / Misc"
           FIELD estPart AS CHARACTER LABEL "Customer Part"
           FIELD estDesc AS CHARACTER LABEL "Description" 
           FIELD estQty AS INTEGER LABEL "Qty"
           FIELD estQtyUom AS CHARACTER LABEL "Uom"
           FIELD estPrice AS DECIMAL LABEL "Price"
           FIELD estPrUom AS CHARACTER LABEL "Uom" 
           FIELD estPo AS CHARACTER LABEL "Po"
           FIELD estTotal AS DECIMAL LABEL "Total"
           FIELD estQuote AS INTEGER LABEL "Quote" 
           FIELD estPriceMatrix AS LOGICAL LABEL "Price Matrix"
           FIELD estRowid AS ROWID 
           FIELD isSelected AS LOG       COLUMN-LABEL "" VIEW-AS TOGGLE-BOX .




/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
