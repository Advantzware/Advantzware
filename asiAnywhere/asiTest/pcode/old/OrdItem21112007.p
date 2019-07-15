
/*------------------------------------------------------------------------
    File        : OrdItem.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{OrdItem.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.

DEFINE INPUT PARAMETER prmPart AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmName AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr AS char NO-UNDO.
DEFINE INPUT PARAMETER prmDscr2 AS char NO-UNDO.
DEFINE INPUT PARAMETER prmDscr3 AS char NO-UNDO.
DEFINE INPUT PARAMETER prmIsSet AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCust AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCustName AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTax AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPurchase AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmEstimate  as Character NO-UNDO.
DEFINE INPUT PARAMETER prmStyle as Character NO-UNDO.
DEFINE INPUT PARAMETER prmdie as Char NO-UNDO.
DEFINE INPUT PARAMETER prmPlate as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCad  as Char NO-UNDO.
DEFINE INPUT PARAMETER prmSPC as Char NO-UNDO.
DEFINE INPUT PARAMETER prmUPC as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmSell as CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUOM as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCurr as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCateg as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmRpt as Character NO-UNDO.
DEFINE INPUT PARAMETER prmWareHouse as Char NO-UNDO.
DEFINE INPUT PARAMETER prmBin as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCount as CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmWeight as CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFreight as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmFreClass as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmInventory as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCycle as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmProduction as Char NO-UNDO.
DEFINE INPUT PARAMETER prmPacking as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmStat AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmstock AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmcasepal AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmtypecode  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrdItem.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEFINE STREAM s1.
DEFINE BUFFER buff_itemfg FOR itemfg.
DEFINE BUFFER buff_oe-ordl FOR oe-ordl.

IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
  IF prmAction  = "select" THEN DO:

  
    FIND FIRST oe-ordl where oe-ordl.ord-no = int(prmOrderNum)  NO-LOCK NO-ERROR.
        For each itemfg where itemfg.i-no = oe-ordl.i-no no-lock:
            create ttOrdItem.
            assign  
                ttOrdItem.Item1         = itemfg.i-no
                ttOrdItem.Part          = itemfg.part-no
                ttOrdItem.name1         = itemfg.i-name
                ttOrdItem.Dscr          = itemfg.part-dscr1 
                ttOrdItem.Dscr2         = itemfg.part-dscr2
                ttOrdItem.Dscr3         = itemfg.part-dscr3
                ttOrdItem.IsSet         = itemfg.isaset
                ttOrdItem.Cust          = itemfg.cust-no
                ttOrdItem.CustName      = itemfg.cust-name
                ttOrdItem.Tax           = itemfg.taxable
                ttOrdItem.Purchase      = itemfg.pur-man
                ttOrdItem.Estimate      = itemfg.est-no
                ttOrdItem.Style         = itemfg.style + "," + itemfg.style-desc
                ttOrdItem.die           = itemfg.die-no
                ttOrdItem.Plate         = itemfg.plate-no
                ttOrdItem.Cad           = itemfg.cad-no
                ttOrdItem.SPC           = itemfg.spc-no
                ttOrdItem.UPC           = itemfg.upc-no
                ttOrdItem.Sell          = itemfg.sell-price
                ttOrdItem.UOM           = itemfg.sell-uom
                ttOrdItem.Curr          = itemfg.curr-code[1]
                ttOrdItem.Categ         = itemfg.procat
                ttOrdItem.Rpt           = itemfg.procat-desc
                ttOrdItem.WareHouse     = itemfg.def-loc
                ttOrdItem.Bin           = itemfg.def-loc-bin
                ttOrdItem.Count         = itemfg.case-count
                ttOrdItem.Weight        = itemfg.weight-100
                ttOrdItem.Freight       = itemfg.frt-class
                ttOrdItem.FreClass      = itemfg.frt-class-dscr 
                ttOrdItem.Inventory     = itemfg.class
                ttOrdItem.Cycle         = itemfg.cc-code
                ttOrdItem.Production    = itemfg.prod-code
                ttOrdItem.Packing       = itemfg.prod-notes
                ttOrdItem.StdMat        = itemfg.std-mat-cost
                ttOrdItem.StdLab        = itemfg.std-lab-cost
                ttOrdItem.StdVar        = itemfg.std-var-cost
                ttOrdItem.StdFix        = itemfg.std-fix-cost
                ttOrdItem.StdTot        = itemfg.total-std-cost
                ttOrdItem.Average       = itemfg.avg-cost
                ttOrdItem.LastCost      = itemfg.last-cost
                ttOrdItem.LastUom       = itemfg.std-uom
                ttOrdItem.Exempt        = itemfg.exempt-disc
                ttOrdItem.stock         = itemfg.i-code
                ttOrdItem.casepal       = itemfg.ship-meth
                ttOrdItem.typecode      = itemfg.type-code
                .
            
            FIND FIRST reftable NO-ERROR.
            IF  AVAIL reftable THEN DO:
                ASSIGN
                    ttOrdItem.stat = reftable.code2.
            END.
  
          END.   /*FOR EACH Itemfg*/
  END.


/*******************************************************************************************************************/

 IF prmAction = "Save" THEN DO:
     
     FIND FIRST buff_oe-ordl where buff_oe-ordl.ord-no = int(prmOrderNum)   NO-LOCK NO-ERROR.
    FIND FIRST buff_itemfg where buff_itemfg.i-no = prmItemNum NO-LOCK NO-ERROR.
    IF NOT AVAILABLE buff_itemfg THEN DO:
        CREATE buff_itemfg.
        ASSIGN
            buff_itemfg.i-no         = prmItemNum
            buff_itemfg.part-no      = prmPart
            buff_itemfg.i-name       = prmName
            buff_itemfg.part-dscr1   = prmDscr
            buff_itemfg.part-dscr2   = prmDscr2
            buff_itemfg.part-dscr3   = prmDscr3
            buff_itemfg.isaset       = IF prmIsSet = "yes" THEN TRUE ELSE FALSE
            buff_itemfg.cust-no      = prmIsCust
            buff_itemfg.cust-name    = prmCustName
            buff_itemfg.taxable      = IF prmTax = "yes" THEN TRUE ELSE FALSE
            buff_itemfg.pur-man      = IF prmPurchase = "Yes" then true else false
            buff_itemfg.est-no       = prmEstimate
            buff_itemfg.style        = prmStyle
            buff_itemfg.style-desc   = prmStyle
            buff_itemfg.die-no       = prmdie
            buff_itemfg.plate-no     = prmPlate
            buff_itemfg.cad-no       = prmCad    
            buff_itemfg.spc-no       = prmSPC    
            buff_itemfg.upc-no       = prmUPC    
            buff_itemfg.sell-price   = DEC(prmSell)   
            buff_itemfg.sell-uom     = prmUom   
            uff_itemfg.curr-code[1]  = prmCurr 
            buff_itemfg.procat       = prmCateg 
            buff_itemfg.procat-desc  = prmRpt 
            buff_itemfg.def-loc      = prmWareHouse 
            buff_itemfg.def-loc-bin  = prmBin
            buff_itemfg.case-count   = INT(prmCount)
            buff_itemfg.weight-100   = DEC(prmWeight) 
            buff_itemfg.frt-class    = prmFreight
            buff_itemfg.frt-class-dscr = prmFreClass
            buff_itemfg.class        = prmInventory     
            buff_itemfg.cc-code      = prmCycle     
            buff_itemfg.prod-code    = prmProduction    
            buff_itemfg.prod-notes   = prmPacking     
            buff_itemfg.exempt-disc  = IF prmExempt = "Yes" THEN TRUE ELSE FALSE
            /* buff_itemfg.stat = prmStat  */
            buff_itemfg.i-code       = prmstock 
            buff_itemfg.ship-meth    = IF prmcasepal = "Yes" THEN TRUE ELSE FALSE  
            buff_itemfg.type-code    = IF prmtypecode= "Yes" THEN TRUE ELSE FALSE .
        END.   /*if not available buff_itemfg*/
        RELEASE buff_itemfg.
        
 END.
        
        /******************************************************************/
        IF prmAction = "Update" THEN DO:
        
            
            FIND FIRST buff_oe-ordl where buff_oe-ordl.ord-no = int(prmOrderNum)   NO-LOCK NO-ERROR.
            FIND FIRST buff_itemfg where buff_itemfg.i-no = prmItemNum no-lock:
            IF  AVAILABLE buff_itemfg THEN DO:
                ASSIGN
                    buff_itemfg.part-no      = prmPart
                    buff_itemfg.i-name       = prmName
                    buff_itemfg.part-dscr1   = prmDscr
                    buff_itemfg.part-dscr2   = prmDscr2
                    buff_itemfg.part-dscr3   = prmDscr3
                    buff_itemfg.isaset       = IF prmIsSet = "yes" THEN TRUE ELSE FALSE
                    buff_itemfg.cust-no      = prmIsCust
                    buff_itemfg.cust-name    = prmCustName
                    buff_itemfg.taxable      = IF prmTax = "yes" THEN TRUE ELSE FALSE
                    buff_itemfg.pur-man      = IF prmPurchase = "Yes" then true else false
                    buff_itemfg.est-no       = prmEstimate
                    buff_itemfg.style        = prmStyle
                    buff_itemfg.style-desc   = prmStyle
                    buff_itemfg.die-no       = prmdie
                    buff_itemfg.plate-no     = prmPlate
                    buff_itemfg.cad-no       = prmCad    
                    buff_itemfg.spc-no       = prmSPC    
                    buff_itemfg.upc-no       = prmUPC    
                    buff_itemfg.sell-price   = int(prmSell)   
                    buff_itemfg.sell-uom     = prmUom   
                    uff_itemfg.curr-code[1]  = prmCurr 
                    buff_itemfg.procat       = prmCateg 
                    buff_itemfg.procat-desc  = prmRpt 
                    buff_itemfg.def-loc      = prmWareHouse 
                    buff_itemfg.def-loc-bin  = prmBin
                    buff_itemfg.case-count   = int(prmCount)
                    buff_itemfg.weight-100   = int(prmWeight) 
                    buff_itemfg.frt-class    = prmFreight
                    buff_itemfg.frt-class-dscr = prmFreClass
                    buff_itemfg.class        = prmInventory     
                    buff_itemfg.cc-code      = prmCycle     
                    buff_itemfg.prod-code    = prmProduction    
                    buff_itemfg.prod-notes   = prmPacking     
                    buff_itemfg.exempt-disc  = IF prmExempt = "Yes" THEN TRUE ELSE FALSE
                    /* buff_itemfg.stat = prmStat  */
                    buff_itemfg.i-code       = prmstock 
                    buff_itemfg.ship-meth    = IF prmcasepal = "Yes" THEN TRUE ELSE FALSE  
                    buff_itemfg.type-code    = IF prmtypecode= "Yes" THEN TRUE ELSE FALSE .
               END.
               RELEASE buff_itemfg.
 END.
                


