
/*------------------------------------------------------------------------
    File        : Inventory.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{Inventory.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.

DEFINE INPUT PARAMETER prmItemNum   AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmName      AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER prmVendor1   AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER prmVendItem1 AS CHARACTER  NO-UNDO.   
DEFINE INPUT PARAMETER prmVendor2   AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmVendItem2 AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER prmOrdPolicy AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER prmStocked   AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmPurchased AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmIsSet     AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmalloc     AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmordlevel AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmordmin   AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmordmax   AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmpuruom   AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmleaddays  AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER prmbegdate  AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmbegbal   AS CHARACTER  NO-UNDO.  

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInventory.
    DEFINE BUFFER buff_itemfg FOR itemfg.
DEFINE BUFFER buff_oe-ordl FOR oe-ordl.

    IF prmUser     = ? THEN ASSIGN prmUser     = "".
    IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
    IF prmItemNum = ? THEN ASSIGN prmItemNum = "".
    IF prmName     = ? THEN ASSIGN prmName     = "".
    IF prmVendor1 = ? THEN ASSIGN prmVendor1 = "".
    IF prmVendItem1 = ? THEN ASSIGN prmVendItem1 = "".
    IF prmVendor2     = ? THEN ASSIGN prmVendor2     = "".
    IF prmVendItem2 = ? THEN ASSIGN prmVendItem2 = "".
    IF prmOrdPolicy = ? THEN ASSIGN prmOrdPolicy = "".
    IF prmStocked     = ? THEN ASSIGN prmStocked     = "".
    IF prmPurchased = ? THEN ASSIGN prmPurchased = "".
    IF prmIsSet = ? THEN ASSIGN prmIsSet = "".
    IF prmalloc     = ? THEN ASSIGN prmalloc     = "".
    IF prmordlevel = ? THEN ASSIGN prmordlevel = "".
    IF prmordmin = ? THEN ASSIGN prmordmin = "".
    IF prmordmax = ? THEN ASSIGN prmordmax = "".
    IF prmpuruom = ? THEN ASSIGN prmpuruom = "".
    IF prmleaddays = ? THEN ASSIGN prmleaddays = "".
    IF prmbegdate = ? THEN ASSIGN prmbegdate = "".
    IF prmbegbal = ? THEN ASSIGN prmbegbal = "".
    /* ********************  Preprocessor Definitions  ******************** */
    /* ***************************  Main Block  *************************** */

    /* ***************************  Procedures  *************************** */

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction  = "select" THEN DO:
    
/*FIND FIRST oe-ordl where oe-ordl.company EQ prmComp AND
     oe-ordl.ord-no = int(prmOrderNum) AND oe-ordl.LINE = int(prmItemNum)  NO-LOCK NO-ERROR. */
FIND FIRST itemfg where itemfg.company EQ prmComp AND itemfg.i-no = TRIM(prmItemNum) NO-LOCK NO-ERROR.
IF AVAILABLE itemfg THEN DO:
    
    create ttInventory.
    assign  
        ttInventory.Item        = itemfg.i-no
        ttInventory.NAME        = itemfg.i-name
        ttInventory.Vendor1     = itemfg.vend-no
        ttInventory.VendItem1   = itemfg.vend-item
        ttInventory.Vendor2     = itemfg.vend2-no
        ttInventory.VendItem2   = itemfg.vend2-item
        ttInventory.OrdPolicy   = itemfg.ord-policy
        ttInventory.Stocked     = itemfg.stocked
        ttInventory.Purchased   = itemfg.pur-man
        ttInventory.IsSet       = itemfg.isaset   
        ttInventory.alloc       =  itemfg.alloc
        ttInventory.ord-level   = itemfg.ord-level
        ttInventory.ord-min     = itemfg.ord-min
        ttInventory.ord-max     = itemfg.ord-max
        ttInventory.pur-uom     = itemfg.pur-uom
        ttInventory.lead-days   = itemfg.lead-days
        ttInventory.beg-date    = itemfg.beg-date
        ttInventory.beg-bal     = itemfg.beg-bal
        ttInventory.q-ono       = itemfg.q-ono
        ttInventory.q-onh       = itemfg.q-onh
        ttInventory.q-alloc     = itemfg.q-alloc
        ttInventory.q-avail     = itemfg.q-avail
        ttInventory.q-back      = itemfg.q-back 
        /*ttInventory.Dscr      = string(itemfg.part-dscr1 + ", " + itemfg.part-dscr2 + ", " + itemfg.part-dscr3)*/
        .
   
    END.  /*IF AVAILABLE itemfg THEN DO:*/

END.  /*IF prmAction  = "select" THEN DO:*/
/*******************************************************************************************************************/

 IF prmAction = "Save" THEN DO:
     FIND FIRST buff_oe-ordl where buff_oe-ordl.company EQ prmComp AND
          buff_oe-ordl.ord-no = int(prmOrderNum) AND buff_oe-ordl.i-no = prmItemNum  NO-LOCK NO-ERROR. 
     FIND FIRST buff_itemfg where buff_itemfg.company EQ prmComp AND
          buff_itemfg.i-no = buff_oe-ordl.i-no NO-LOCK NO-ERROR.

     /*I think below is wrong*/
     IF NOT AVAILABLE buff_itemfg THEN DO:
        CREATE buff_itemfg.
        ASSIGN
            itemfg.i-no         = prmItemNum       
            itemfg.i-name       = prmName     
            itemfg.vend-no      = prmVendor1    
            itemfg.vend-item    = prmVendItem1 
            itemfg.vend2-no     = prmVendor2 
            itemfg.vend2-item   = prmVendItem2
            itemfg.ord-policy   = IF prmOrdPolicy = "yes" THEN TRUE ELSE FALSE
            itemfg.stocked      = IF prmStocked  = "yes" THEN TRUE ELSE FALSE
            itemfg.pur-man      = IF prmPurchased  = "yes" THEN TRUE ELSE FALSE
            itemfg.isaset       = IF prmIsSet = "yes" THEN TRUE ELSE FALSE
            itemfg.alloc        = IF prmalloc = "yes" THEN TRUE ELSE FALSE
            itemfg.ord-level    = dec(prmordlevel)
            itemfg.ord-min      = dec(prmordmin)
            itemfg.ord-max      = dec(prmordmax)
            itemfg.pur-uom      = prmpuruom 
            itemfg.lead-days    = INT(prmleaddays)
            itemfg.beg-date     = DATE(prmbegdate)
            itemfg.beg-bal      = dec(prmbegbal)

            .
     END.  /*IF NOT AVAILABLE buff_itemfg THEN DO:*/
     RELEASE buff_itemfg.
 END.  /*IF prmAction = "Save" THEN DO:*/

 /******************************************************************/
 IF prmAction = "Update" THEN DO:
     FIND FIRST buff_oe-ordl WHERE buff_oe-ordl.company EQ prmComp AND buff_oe-ordl.ord-no = int(prmOrderNum) AND buff_oe-ordl.i-no = prmItemNum  NO-LOCK NO-ERROR. 
     FIND FIRST buff_itemfg where buff_itemfg.company EQ prmComp AND buff_itemfg.i-no = buff_oe-ordl.i-no NO-LOCK NO-ERROR.

     /*this is wrong here*/
     IF  AVAILABLE buff_itemfg THEN DO:
         ASSIGN
            itemfg.i-no         = prmItemNum       
            itemfg.i-name       = prmName     
            itemfg.vend-no      = prmVendor1    
            itemfg.vend-item    = prmVendItem1 
            itemfg.vend2-no     = prmVendor2 
            itemfg.vend2-item   = prmVendItem2
            itemfg.ord-policy   = IF prmOrdPolicy = "yes" THEN TRUE ELSE FALSE
            itemfg.stocked      = IF prmStocked  = "yes" THEN TRUE ELSE FALSE
            itemfg.pur-man      = IF prmPurchased  = "yes" THEN TRUE ELSE FALSE
            itemfg.isaset       = IF prmIsSet = "yes" THEN TRUE ELSE FALSE
            itemfg.alloc        = IF prmalloc = "yes" THEN TRUE ELSE FALSE
            itemfg.ord-level    = dec(prmordlevel)
            itemfg.ord-min      = dec(prmordmin)
            itemfg.ord-max      = dec(prmordmax)
            itemfg.pur-uom      = prmpuruom 
            itemfg.lead-days    = INT(prmleaddays)
            itemfg.beg-date     = DATE(prmbegdate)
            itemfg.beg-bal      = dec(prmbegbal)
            .
     END.  /*IF NOT AVAILABLE buff_itemfg THEN DO:*/
     RELEASE buff_itemfg.
 END.  /*IF prmAction = "Save" THEN DO:*/


