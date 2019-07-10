

/*------------------------------------------------------------------------
    File        : UpdateCustInventory.p
    Purpose     : Customer Inventory
    Syntax      :

    Description : Return a Dataset of Customer Inventory
    Author(s)   : 
    Created     : 30 Dec 2008 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUpdateCustInv  NO-UNDO
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
        FIELD vAbc                  AS CHAR   
        .
DEFINE DATASET dsUpdateCustInv FOR ttUpdateCustInv .

DEFINE INPUT PARAMETER prmUser          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustNum       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRevision      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItmNum      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustPartNum   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendCode      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendPlantId   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendDeptCode  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmOsoleteDate   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAnnUsageQty   AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmOnHandQty     AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmRecId         AS CHAR       NO-UNDO.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEF VAR v-recid            AS RECID NO-UNDO. 

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUpdateCustInv.

IF prmUser          = ? THEN ASSIGN prmUser        = "".
IF prmAction        = ? THEN ASSIGN prmAction      = "Select".
IF prmComp          = ? THEN ASSIGN prmComp        = "".
IF prmCustNum       = ? THEN ASSIGN prmCustNum     = "".
IF prmRevision      = ? THEN ASSIGN prmRevision     = "".
IF prmFgItmNum      = ? THEN ASSIGN prmFgItmNum    = "".
IF prmCustPartNum   = ? THEN ASSIGN prmCustPartNum = "".
IF prmVendCode      = ? THEN ASSIGN prmVendCode    = "".
IF prmVendPlantId   = ? THEN ASSIGN prmVendPlantId = "".
IF prmVendDeptCode  = ? THEN ASSIGN prmVendDeptCode = "".
IF prmOsoleteDate   = ? THEN ASSIGN prmOsoleteDate = "".
IF prmAnnUsageQty   = ? THEN ASSIGN prmAnnUsageQty = 0.
IF prmOnHandQty     = ? THEN ASSIGN prmOnHandQty   = 0.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
 
/***********Validations***************/

IF prmAction = "Update" THEN DO:   
    
    FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.cust-no = prmCustNum AND vend-code-cust-xref.company = prmComp NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vend-code-cust-xref THEN DO:
        ASSIGN cError  = "Invalid Suppliers A/R Code".
        RETURN.
    END.

    FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.cust-no = prmCustNum AND vend-code-cust-xref.company = prmComp 
                                     AND vend-code-cust-xref.vendor-code = prmVendCode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vend-code-cust-xref THEN DO:
        ASSIGN cError  = "Invalid Customers A/P Code  ".
        RETURN.
    END.
        
    FIND FIRST itemfg WHERE itemfg.i-no = prmFgItmNum AND itemfg.company = prmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN cError  = "Invalid Suppliers FG Item     ".
        RETURN.
    END.    
    
    FIND FIRST itemfg WHERE itemfg.part-no = prmCustPartNum AND itemfg.company = prmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN cError  = "Invalid Customers Part Number    ".
        RETURN.
    END.     

    FIND FIRST vend-plant WHERE vend-plant.plant-id = prmVendPlantId AND vend-plant.company = prmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vend-plant THEN DO:
        ASSIGN cError  = "Invalid Customers Plant ID".
        RETURN.
    END.
    
    FIND FIRST vend-plant WHERE vend-plant.plant-id = prmVendPlantId AND vend-plant.vendor-dept-code = prmVendDeptCode AND vend-plant.company = prmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vend-plant THEN DO:
        ASSIGN cError  = "Invalid Customers Dept Code      ".
        RETURN.
    END.
END.


IF prmAction = "Add" THEN DO:
   FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.cust-no = prmCustNum AND vend-code-cust-xref.company = prmComp NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vend-code-cust-xref THEN DO:
        ASSIGN cError  = "Invalid Suppliers A/R Code".
        RETURN.
    END.

     FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.cust-no = prmCustNum AND vend-code-cust-xref.company = prmComp 
                                     AND vend-code-cust-xref.vendor-code = prmVendCode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vend-code-cust-xref THEN DO:
        ASSIGN cError  = "Invalid Customers A/P Code  ".
        RETURN.
    END.
        
    FIND FIRST itemfg WHERE itemfg.i-no = prmFgItmNum AND itemfg.company = prmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN cError  = "Invalid Suppliers FG Item     ".
        RETURN.
    END.    
    
    FIND FIRST itemfg WHERE itemfg.part-no = prmCustPartNum AND itemfg.company = prmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN cError  = "Invalid Customers Part Number    ".
        RETURN.
    END.     

    FIND FIRST vend-plant WHERE vend-plant.plant-id = prmVendPlantId AND vend-plant.company = prmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vend-plant THEN DO:
        ASSIGN cError  = "Invalid Customers Plant ID".
        RETURN.
    END.
    
    FIND FIRST vend-plant WHERE vend-plant.plant-id = prmVendPlantId AND vend-plant.vendor-dept-code = prmVendDeptCode AND vend-plant.company = prmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vend-plant THEN DO:
        ASSIGN cError  = "Invalid Customers Dept Code      ".
        RETURN.
    END.

END.

/*************Add******************************/

IF prmAction = "Add" THEN DO:
  FIND LAST vend-whse-item WHERE  vend-whse-item.company = prmComp   NO-LOCK NO-ERROR.
    CREATE vend-whse-item.
            ASSIGN
               vend-whse-item.cust-no            = prmCustNum 
               vend-whse-item.revision           = prmRevision 
               vend-whse-item.cust-part-no       = prmCustPartNum
               vend-whse-item.fg-item-no         = prmFgItmNum
               vend-whse-item.vendor-code        = prmVendCode
               vend-whse-item.vendor-plant-code  = prmVendPlantId
               vend-whse-item.vendor-dept-code   = prmVendDeptCode
               vend-whse-item.obsolete-date      = DATE(prmOsoleteDate)
               vend-whse-item.est-annual-usage   = prmAnnUsageQty
               vend-whse-item.plant-tot-oh-qty   = prmOnHandQty
               vend-whse-item.company            = prmComp 
               vend-whse-item.rec_key            = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99")+ STRING(DAY(TODAY), "99") + STRING(TIME).
                .
            ASSIGN 
                prmRecId = vend-whse-item.rec_key .
                prmAction = "Select".
               
END. /*IF prmAction = "Add" THEN DO:*/

/**********************Update**********************************/
IF prmAction = "Update" THEN DO:        
    FIND FIRST vend-whse-item WHERE vend-whse-item.company = prmComp AND vend-whse-item.rec_key = prmRecId EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL vend-whse-item THEN          
         ASSIGN
               vend-whse-item.cust-no            = prmCustNum 
               vend-whse-item.revision           = prmRevision 
               vend-whse-item.cust-part-no       = prmCustPartNum
               vend-whse-item.fg-item-no         = prmFgItmNum
               vend-whse-item.vendor-code        = prmVendCode
               vend-whse-item.vendor-plant-code  = prmVendPlantId
               vend-whse-item.vendor-dept-code   = prmVendDeptCode
               vend-whse-item.obsolete-date      = datetime(prmOsoleteDate)
               vend-whse-item.est-annual-usage   = prmAnnUsageQty
               vend-whse-item.plant-tot-oh-qty   = prmOnHandQty
               
                .

                ASSIGN 
                    prmAction = "Select".
            
END.

/**********************delete*******************************************************/

IF prmAction = "Delete"  THEN DO:
    FIND vend-whse-item WHERE vend-whse-item.rec_key = prmRecId EXCLUSIVE-LOCK NO-ERROR.
      
    IF AVAIL vend-whse-item THEN DO:
      DELETE vend-whse-item.
    END.
        FIND LAST vend-whse-item WHERE vend-whse-item.company = prmComp NO-LOCK NO-ERROR.
        IF AVAIL vend-whse-item THEN
            ASSIGN 
           prmRecId = vend-whse-item.rec_key 
           prmAction = "Select" .
END. /*IF prmAction = "delete"*/ 

/*****************Select********************/


IF prmAction = "Select" THEN DO:
    FIND FIRST vend-whse-item WHERE  vend-whse-item.rec_key = prmRecId  NO-LOCK NO-ERROR.
            create ttUpdateCustInv.
            assign
                ttUpdateCustInv.vCustNum        = vend-whse-item.cust-no
                ttUpdateCustInv.vRevision       = vend-whse-item.revision
                ttUpdateCustInv.vFgItmNum       = vend-whse-item.fg-item-no
                ttUpdateCustInv.vCustPartNum    = vend-whse-item.cust-part-no
                ttUpdateCustInv.vCustVenCode    = vend-whse-item.vendor-code
                ttUpdateCustInv.vVendPlantId    = vend-whse-item.vendor-plant-code                
                ttUpdateCustInv.vVendorDeptCode = vend-whse-item.vendor-dept-code
                ttUpdateCustInv.vOsoleteDate    = STRING(vend-whse-item.obsolete-date)
                ttUpdateCustInv.vAnnUsage       = vend-whse-item.est-annual-usage
                ttUpdateCustInv.vCustHandQty    = vend-whse-item.plant-tot-oh-qty
                ttUpdateCustInv.vRecKey         = vend-whse-item.rec_key
                .
       
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/



