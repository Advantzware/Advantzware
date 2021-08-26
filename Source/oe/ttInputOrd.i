
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
DEFINE {1} SHARED TEMP-TABLE ttInputOrd like oe-ord
    .

DEFINE {1} SHARED TEMP-TABLE ttInputOrdLine like oe-ordl
         FIELD cItemType as CHARACTER LABEL "Item Type" 
         FIELD cQtyUom AS CHARACTER LABEL "UOM" 
         FIELD lCreateRel AS LOGICAL LABEL "Create Release"
         FIELD lCreateJob AS LOGICAL LABEL "Create Job"
         FIELD lCreatePo AS LOGICAL LABEL "Create Po"
    .

DEFINE {1} SHARED TEMP-TABLE tt-est-item 
           FIELD company AS CHARACTER
           FIELD est-line AS INTEGER LABEL "Lin"
           FIELD est-cust AS CHARACTER LABEL "Customer"
           FIELD est-item AS CHARACTER LABEL "Item Id / Misc"
           FIELD est-part AS CHARACTER LABEL "Customer Part"
           FIELD est-desc AS CHARACTER LABEL "Description" 
           FIELD est-qty AS INTEGER LABEL "Qty"
           FIELD est-qty-uom AS CHARACTER LABEL "Uom"
           FIELD est-price AS DECIMAL LABEL "Price"
           FIELD est-pr-uom AS CHARACTER LABEL "Uom" 
           FIELD est-po AS CHARACTER LABEL "Po"
           FIELD est-total AS DECIMAL LABEL "Total"
           FIELD est-quote AS INTEGER LABEL "Quote" 
           FIELD est-price-matrix AS LOGICAL LABEL "Price Matrix"
           FIELD est-rowid AS ROWID 
           FIELD IS-SELECTED AS LOG       COLUMN-LABEL "" VIEW-AS TOGGLE-BOX .




/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
