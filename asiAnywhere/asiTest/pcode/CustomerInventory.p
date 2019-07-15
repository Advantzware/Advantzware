
/*------------------------------------------------------------------------
    File        : CustomerInventory.p
    Purpose     : Customer Inventory
    Syntax      :

    Description : Return a Dataset of Customer Inventory
    Author(s)   : 
    Created     : 30 Dec 2008 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
MESSAGE "test1" .
DEFINE TEMP-TABLE ttCustInventory NO-UNDO
        FIELD vCustNum              AS CHAR    
        FIELD vRevision             AS CHAR 
        FIELD vFgItmNum             AS CHAR
        FIELD vCustPartNum          AS CHAR            
        FIELD vCustVenCode          AS CHAR 
        FIELD vVendPlantId          AS CHAR  
        FIELD vVendorDeptCode       AS CHAR 
        FIELD vOsoleteDate          AS CHAR            
        FIELD vAnnUsage             AS DEC 
        FIELD vCustHandQty          AS DEC 
        FIELD vRecKey               AS CHAR
        .
DEFINE DATASET dsCustInventory FOR ttCustInventory .

DEFINE INPUT PARAMETER prmUser          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustNum       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustPartNum   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItmNum      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendCode      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendPlantId   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAnnUsageQty   AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmOnHandQty     AS DEC         NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustInventory.

IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmAction      = ? THEN ASSIGN prmAction      = "Select".
IF prmComp        = ? THEN ASSIGN prmComp        = "".
IF prmCustNum     = ? THEN ASSIGN prmCustNum     = "".
IF prmCustPartNum = ? THEN ASSIGN prmCustPartNum = "".
IF prmFgItmNum    = ? THEN ASSIGN prmFgItmNum    = "".
IF prmVendCode    = ? THEN ASSIGN prmVendCode    = "".
IF prmVendPlantId = ? THEN ASSIGN prmVendPlantId = "".
IF prmAnnUsageQty = ? THEN ASSIGN prmAnnUsageQty = 0.
IF prmOnHandQty   = ? THEN ASSIGN prmOnHandQty   = 0.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "Select" THEN DO:
    FOR EACH vend-whse-item WHERE  vend-whse-item.company = prmComp NO-LOCK:
            create ttCustInventory.
            assign
                ttCustInventory.vCustNum        = vend-whse-item.cust-no
                ttCustInventory.vRevision       = vend-whse-item.revision
                ttCustInventory.vFgItmNum       = vend-whse-item.fg-item-no
                ttCustInventory.vCustPartNum    = vend-whse-item.cust-part-no
                ttCustInventory.vCustVenCode    = vend-whse-item.vendor-code
                ttCustInventory.vVendPlantId    = vend-whse-item.vendor-plant-code
                ttCustInventory.vVendorDeptCode = vend-whse-item.vendor-dept-code
                ttCustInventory.vOsoleteDate    = STRING(vend-whse-item.obsolete-date)
                ttCustInventory.vAnnUsage       = vend-whse-item.est-annual-usage
                ttCustInventory.vCustHandQty    = vend-whse-item.plant-tot-oh-qty
                ttCustInventory.vRecKey         = vend-whse-item.rec_key    
                .
       END. /*FOR EACH vend-whse-item*/
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/

    
IF prmAction = "Search" THEN DO:
    FOR EACH vend-whse-item WHERE (vend-whse-item.cust-no BEGINS prmCustNum OR prmCustNum = "" )
                        AND (vend-whse-item.fg-item-no  BEGINS prmFgItmNum OR prmFgItmNum = "" )
                        AND (vend-whse-item.cust-part-no BEGINS prmCustPartNum OR prmCustPartNum = "" )
                        AND (vend-whse-item.vendor-code BEGINS prmVendCode OR prmVendCode = ""  ) NO-LOCK:
          CREATE ttCustInventory.
            assign
                ttCustInventory.vCustNum        = vend-whse-item.cust-no
                ttCustInventory.vRevision       = vend-whse-item.revision
                ttCustInventory.vFgItmNum       = vend-whse-item.fg-item-no
                ttCustInventory.vCustPartNum    = vend-whse-item.cust-part-no
                ttCustInventory.vCustVenCode    = vend-whse-item.vendor-code
                ttCustInventory.vVendPlantId    = vend-whse-item.vendor-plant-code
                ttCustInventory.vVendorDeptCode = vend-whse-item.vendor-dept-code
                ttCustInventory.vOsoleteDate    = STRING(vend-whse-item.obsolete-date)
                ttCustInventory.vAnnUsage       = vend-whse-item.est-annual-usage
                ttCustInventory.vCustHandQty    = vend-whse-item.plant-tot-oh-qty
                ttCustInventory.vRecKey         = vend-whse-item.rec_key
                .
               
                END. /*FOR EACH vend-whse-item*/
END. /* IF prmAction = "Search" THEN DO: */

/*************************************************/
                   



