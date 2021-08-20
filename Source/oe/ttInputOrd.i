
/*------------------------------------------------------------------------
    File        : ttInputEst.i
    Purpose     : Include file that houses ttInputEstimate

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




/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
