
/*------------------------------------------------------------------------
    File        : OrdItem.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{OrdItem.i}

DEFINE INPUT PARAMETER prmCust          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum      as Character no-undo.  
DEFINE INPUT PARAMETER prmItemNum       as Character no-undo.
DEFINE INPUT PARAMETER prmNewItem       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPart          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmName          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr          AS char NO-UNDO.
DEFINE INPUT PARAMETER prmDscr2         AS char NO-UNDO.
DEFINE INPUT PARAMETER prmDscr3         AS char NO-UNDO.
DEFINE INPUT PARAMETER prmIsSet         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmIsCust        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCustName      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTax           AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPurchase      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmEstimate      as Character NO-UNDO.
DEFINE INPUT PARAMETER prmStyle         as Character NO-UNDO.
DEFINE INPUT PARAMETER prmdie           as Char NO-UNDO.
DEFINE INPUT PARAMETER prmPlate         as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCad           as Char NO-UNDO.
DEFINE INPUT PARAMETER prmSPC            as Char NO-UNDO.
DEFINE INPUT PARAMETER prmUPC           as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmSell          as CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUOM           as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCurr          as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCateg         as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmRpt           as Character NO-UNDO.
DEFINE INPUT PARAMETER prmWareHouse      as Char NO-UNDO.
DEFINE INPUT PARAMETER prmBin           as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCount         as CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmWeight        as CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFreight       as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmFreClass      as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmInventory          as Char NO-UNDO.
DEFINE INPUT PARAMETER prmCycle         as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmProduction    as Char NO-UNDO.
DEFINE INPUT PARAMETER prmPacking       as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmMat          as Char  NO-UNDO. 
DEFINE INPUT PARAMETER prmLab          as Char  NO-UNDO. 
DEFINE INPUT PARAMETER prmVar          as Char  NO-UNDO.  
DEFINE INPUT PARAMETER prmFix          as Char  NO-UNDO. 
DEFINE INPUT PARAMETER prmStd          as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmAvg          as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmLast         as Char  NO-UNDO.  
DEFINE INPUT PARAMETER prmlUom          as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmExempt        as Char  NO-UNDO.
DEFINE INPUT PARAMETER prmStat          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmstock         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmcasepal       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmtypecode      AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrdItem.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEFINE STREAM s1.
DEFINE BUFFER buff_itemfg FOR itemfg.
DEFINE BUFFER buff_oe-ordl FOR oe-ordl.
DEFINE BUFFER b-eb FOR eb.
def var uom-list as cha init "C,CS,EA,L,M,LB,DRM,ROL,PKG,SET,DOZ,BDL" no-undo.
DEF VAR lv-type-codes AS CHAR NO-UNDO.
DEF VAR lv-type-dscrs AS CHAR NO-UNDO.

DEF var prmComp AS CHARACTER  NO-UNDO.

IF prmCust          = ? THEN ASSIGN prmCust       = "".
IF prmUser          = ? THEN ASSIGN prmUser       = "".
IF prmAction        = ? THEN ASSIGN prmAction     = "".
IF prmOrderNum      = ? THEN ASSIGN prmOrderNum   = "".
IF prmItemNum       = ? THEN ASSIGN prmItemNum    = "".
IF prmNewItem       = ? THEN ASSIGN prmNewItem    = "".
IF prmPart          = ? THEN ASSIGN prmPart       = "".
IF prmName          = ? THEN ASSIGN prmName       = "".
IF prmDscr          = ? THEN ASSIGN prmDscr       = "".
IF prmDscr2         = ? THEN ASSIGN prmDscr2      = "".
IF prmDscr3         = ? THEN ASSIGN prmDscr3      = "".
IF prmIsSet         = ? THEN ASSIGN prmIsSet      = "".
IF prmIsCust        = ? THEN ASSIGN prmIsCust     = "".
IF prmCustName      = ? THEN ASSIGN prmCustName   = "".
IF prmTax           = ? THEN ASSIGN prmTax        = "".
IF prmPurchase      = ? THEN ASSIGN prmPurchase   = "".
IF prmEstimate      = ? THEN ASSIGN prmEstimate   = "".
IF prmStyle         = ? THEN ASSIGN prmStyle      = "".
IF prmdie           = ? THEN ASSIGN prmdie        = "".
IF prmPlate         = ? THEN ASSIGN prmPlate      = "".
IF prmCad           = ? THEN ASSIGN prmCad        = "".
IF prmSPC           = ? THEN ASSIGN prmSPC        = "".
IF prmUPC           = ? THEN ASSIGN prmUPC        = "".
IF prmSell          = ? THEN ASSIGN prmSell       = "".
IF prmUOM           = ? THEN ASSIGN prmUOM        = "".
IF prmCurr          = ? THEN ASSIGN prmCurr       = "".
IF prmCateg         = ? THEN ASSIGN prmCateg      = "".
IF prmRpt           = ? THEN ASSIGN prmRpt        = "".
IF prmWareHouse     = ? THEN ASSIGN prmWareHouse  = "".
IF prmBin           = ? THEN ASSIGN prmBin        = "".
IF prmCount         = ? THEN ASSIGN prmCount      = "".
IF prmWeight        = ? THEN ASSIGN prmWeight     = "".
IF prmFreight       = ? THEN ASSIGN prmFreight    = "".
IF prmFreClass      = ? THEN ASSIGN prmFreClass   = "".
IF prmInventory     = ? THEN ASSIGN prmInventory  = "".
IF prmCycle         = ? THEN ASSIGN prmCycle      = "".
IF prmProduction    = ? THEN ASSIGN prmProduction = "".
IF prmPacking       = ? THEN ASSIGN prmPacking    = "".
IF prmMat           = ? THEN ASSIGN prmMat        = "".
IF prmLab           = ? THEN ASSIGN prmLab        = "".
IF prmVar           = ? THEN ASSIGN prmVar        = "".
IF prmFix           = ? THEN ASSIGN prmFix        = "".
IF prmStd           = ? THEN ASSIGN prmStd        = "".
IF prmAvg           = ? THEN ASSIGN prmAvg        = "".
IF prmLast          = ? THEN ASSIGN prmLast       = "".
IF prmlUom          = ? THEN ASSIGN prmlUom       = "".
IF prmExempt        = ? THEN ASSIGN prmExempt     = "".
IF prmStat          = ? THEN ASSIGN prmStat       = "".
IF prmstock         = ? THEN ASSIGN prmstock      = "".
IF prmcasepal       = ? THEN ASSIGN prmcasepal    = "".
IF prmtypecode      = ? THEN ASSIGN prmtypecode   = "".
                                             
         


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
    cocode = prmComp 
    g_company = prmComp .

RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
   FOR EACH ttOrdItem NO-LOCK:
       DELETE ttOrdItem.
   END.


 IF prmAction  = "select" THEN DO:
     RUN build-query.

END.

IF prmAction = "Recalcost" THEN DO:
           
           for each itemfg
               where itemfg.company eq cocode
               and itemfg.i-no eq prmItemNum no-lock:

               IF itemfg.isaset AND itemfg.alloc THEN
                   RUN util/fixfgcst.p (ROWID(itemfg)).
               ELSE
                   RUN fg/updfgcs1.p (recid(itemfg), YES).
             end.
           ASSIGN
               cError = "Recalc Costs completed....".
              RUN  build-query.
 END.

/*******************************************************************************************************************/
IF prmAction = "Delete" THEN DO:
        FIND FIRST oe-ordl 
                   WHERE oe-ordl.company EQ prmComp AND
                         oe-ordl.i-no    EQ prmItemNum NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN DO:
                ASSIGN
                    cError = "Can not delete Item Order line exist ".
                RETURN.
            END.
        
        
        IF NOT AVAIL oe-ordl  THEN  DO:
            FIND FIRST eb WHERE eb.company = prmComp AND eb.stock = prmItemNum NO-LOCK NO-ERROR .
                IF AVAIL eb THEN  DO:
                     ASSIGN
                    cError = "FG still exists in Estimate file, cannot delete... ".
                     RETURN.
                END.

                IF NOT AVAIL eb  THEN  DO:
                FIND FIRST buff_itemfg WHERE buff_itemfg.company = prmComp AND 
                    buff_itemfg.i-no = prmItemNum EXCLUSIVE-LOCK .
                IF AVAIL buff_itemfg THEN
                    DELETE buff_itemfg .
                END.

            END.
           
        FIND LAST itemfg WHERE itemfg.company = prmComp NO-LOCK NO-ERROR.
        IF AVAIL itemfg THEN DO:
            ASSIGN
                prmItemNum = itemfg.i-no .
                RUN build-query.
        END.
    
  END.

IF prmAction = "Save" THEN DO:

    IF INT(prmCount) < 1  then do:
        cError =  "Case count can not less than ONE !!! " .
        return .
       end.

    FIND  FIRST cust WHERE cust.company EQ prmComp AND cust.cust-no EQ prmIsCust NO-LOCK NO-ERROR.
     IF NOT AVAIL cust THEN DO:
         cError =  "Invalid Customer , try help..." .
           RETURN .
       END.

  IF prmWareHouse <> "" THEN do:
    FIND first loc where loc.company = prmComp  and loc.loc = prmWareHouse NO-LOCK NO-ERROR.
      IF NOT AVAIL loc then do:
        cError =  "Invalid Warehouse. Try Help."  .
        return .
       end.
   END.

     if prmBin  <> "" and
         not can-find(first fg-bin where fg-bin.company = prmComp and fg-bin.loc = prmWareHouse and
                      fg-bin.loc-bin = prmBin )
         then do:
         cError = "Invalid Warehouse Bin. Try Help." .
         return .
        end.

      IF INT(prmEstimate) GT 0 AND
      NOT CAN-FIND(FIRST eb WHERE eb.company   EQ prmComp
                              AND eb.est-no    EQ STRING(INT(prmEstimate),">>>>>>>>")
                              AND (eb.part-no  EQ prmPart OR
                                   eb.stock-no EQ prmItemNum))
          THEN DO:
          cError =  "Invalid entry, try help..."  .
          RETURN .
          END.

   if  prmCateg <> "" and
      not can-find(first fgcat where fgcat.company = prmComp and
                                     fgcat.procat = prmCateg )
       then do:
       cError =  "Invalid Product Category. Try Help." .
        return .
   end.

  if prmUom <> "" and
      not can-find(first uom where uom.uom = prmUom and
                                   lookup(uom.uom, uom-list) > 0 )
   then do:
        cError =  "Invalid Unit of Measure. Try help."  .
        return .
   end.

    if prmStyle <> "" and
      not can-find(first style where style.company = prmComp and
                                     style.style = prmStyle )
   then do:
        cError =  "Invalid Style. Try Help."  .
        return .
   end.

    IF TRIM(prmtypecode) NE ""                AND
      LOOKUP(prmtypecode,lv-type-codes) LE 0 THEN DO:
        ASSIGN
            cError =  "Invalid Type, try help..." .
       RETURN .
   END.
IF prmItemNum = "" THEN DO:
    ASSIGN
        cError = "FG Item May not be blank".
        RETURN.
END.
    
END.
 

 IF prmAction = "Save" THEN DO:
  
    FIND FIRST buff_itemfg where buff_itemfg.company EQ prmComp AND
         buff_itemfg.i-no = prmNewItem NO-LOCK NO-ERROR.
    IF AVAIL buff_itemfg THEN DO:
        ASSIGN
            cError = "FG Item# already exist" .
            RETURN.
    END.
    IF NOT AVAILABLE buff_itemfg THEN DO:
        CREATE buff_itemfg.
        ASSIGN
            buff_itemfg.company      = prmComp
            buff_itemfg.loc          = "MAIN"
            buff_itemfg.i-no         = prmNewItem
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
           
            buff_itemfg.die-no       = prmdie
            buff_itemfg.plate-no     = prmPlate
            buff_itemfg.cad-no       = prmCad    
            buff_itemfg.spc-no       = prmSPC    
            buff_itemfg.upc-no       = prmUPC    
            buff_itemfg.sell-price   = DEC(prmSell)   
            buff_itemfg.sell-uom     = prmUom   
            buff_itemfg.curr-code[1]  = prmCurr 
            buff_itemfg.procat       = prmCateg 
           
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
            buff_itemfg.std-mat-cost = dec(prmMat)    
            buff_itemfg.std-lab-cost = dec(prmLab)    
            buff_itemfg.std-var-cost = dec(prmVar)    
            buff_itemfg.std-fix-cost = DEC(prmFix)    
            buff_itemfg.total-std-cost = DEC(prmStd)  
            buff_itemfg.avg-cost     = dec(prmAvg)
            buff_itemfg.last-cost    = dec(prmLast)       
            buff_itemfg.std-uom      = prmlUom   
             buff_itemfg.exempt-disc  = IF prmExempt = "Yes" THEN TRUE ELSE FALSE
             buff_itemfg.stat = prmStat  
            buff_itemfg.i-code       = prmstock 
            buff_itemfg.ship-meth    = IF prmcasepal = "Yes" THEN TRUE ELSE FALSE  
             buff_itemfg.type-code    =  prmtypecode    .

            find first fgcat where fgcat.company = prmComp and
                fgcat.procat = prmCateg  no-lock no-error.
            buff_itemfg.procat-desc = if avail fgcat then fgcat.dscr else "".

            find first style where style.company = prmComp and
                style.style = prmStyle no-lock no-error.
            buff_itemfg.style-desc  = if avail style then style.dscr else "".

          /*  buff_itemfg.type-code    = IF prmtypecode= "Yes" THEN TRUE ELSE FALSE .*/
        END.   /*if not available buff_itemfg*/
        RELEASE buff_itemfg.

        
            ASSIGN 
                prmItemNum  = prmNewItem
                prmAction = "Select".

        RUN  build-query.          
        
 END.
        
        /******************************************************************/
 IF prmAction = "Update" THEN DO:
     IF INT(prmCount) < 1  then do:
         cError =  "Case count can not less than ONE !!! " .
         return .
        end.

     FIND  FIRST cust WHERE cust.company EQ prmComp AND cust.cust-no EQ prmIsCust NO-LOCK NO-ERROR.
      IF NOT AVAIL cust THEN DO:
          cError =  "Invalid Customer , try help..." .
            RETURN .
        END.
    
   IF prmWareHouse <> "" THEN do:
     FIND first loc where loc.company = prmComp  and loc.loc = prmWareHouse NO-LOCK NO-ERROR.
       IF NOT AVAIL loc then do:
         cError =  "Invalid Warehouse. Try Help."  .
         return .
        end.
    END.

      if prmBin  <> "" and
          not can-find(first fg-bin where fg-bin.company = prmComp and fg-bin.loc = prmWareHouse and
                       fg-bin.loc-bin = prmBin )
          then do:
          cError = "Invalid Warehouse Bin. Try Help." .
          return .
         end.

       IF INT(prmEstimate) GT 0 AND
       NOT CAN-FIND(FIRST eb WHERE eb.company   EQ prmComp
                               AND eb.est-no    EQ STRING(INT(prmEstimate),">>>>>>>>")
                               AND (eb.part-no  EQ prmPart OR
                                    eb.stock-no EQ prmItemNum))
           THEN DO:
           cError =  "Invalid entry, try help..."  .
           RETURN .
           END.

    if  prmCateg <> "" and
       not can-find(first fgcat where fgcat.company = prmComp and
                                      fgcat.procat = prmCateg )
        then do:
        cError =  "Invalid Product Category. Try Help." .
         return .
    end.

   if prmUom <> "" and
       not can-find(first uom where uom.uom = prmUom and
                                    lookup(uom.uom, uom-list) > 0 )
    then do:
         cError =  "Invalid Unit of Measure. Try help."  .
         return .
    end.
    
     if prmStyle <> "" and
       not can-find(first style where style.company = prmComp and
                                      style.style = prmStyle )
    then do:
         cError =  "Invalid Style. Try Help."  .
         return .
    end.

     IF TRIM(prmtypecode) NE ""                AND
       LOOKUP(prmtypecode,lv-type-codes) LE 0 THEN DO:
         ASSIGN
             cError =  "Invalid Type, try help..." .
        RETURN .
    END.
    
 END.

        IF prmAction = "Update" THEN DO:

            MESSAGE dec(prmMat)  
                    dec(prmLab)  
                    dec(prmVar)  
                    DEC(prmFix)  
                    DEC(prmStd)
                    dec(prmAvg)  
                    dec(prmLast) 
                    prmlUom   .   

            
            FIND FIRST buff_itemfg where buff_itemfg.company EQ prmComp AND
                 buff_itemfg.i-no = prmItemNum EXCLUSIVE-LOCK NO-ERROR.
            IF  AVAILABLE buff_itemfg THEN DO:
                ASSIGN
                    buff_itemfg.part-no      = prmPart
                    buff_itemfg.i-name       = prmName 
                    buff_itemfg.part-dscr1   = prmDscr
                    buff_itemfg.part-dscr2   = prmDscr2
                    buff_itemfg.part-dscr3   = prmDscr3
                    buff_itemfg.isaset       = IF prmIsSet = "Yes" THEN TRUE ELSE FALSE
                    buff_itemfg.cust-no      = prmIsCust
                    buff_itemfg.cust-name    = prmCustName
                    buff_itemfg.taxable      = IF prmTax = "Yes" THEN TRUE ELSE FALSE
                    buff_itemfg.pur-man      = IF prmPurchase = "Yes" then true else false
                    buff_itemfg.est-no       = prmEstimate
                    buff_itemfg.style        = prmStyle
                   
                    buff_itemfg.die-no       = prmdie
                    buff_itemfg.plate-no     = prmPlate
                    buff_itemfg.cad-no       = prmCad    
                    buff_itemfg.spc-no       = prmSPC    
                    buff_itemfg.upc-no       = prmUPC    
                    buff_itemfg.sell-price   = int(prmSell)   
                    buff_itemfg.sell-uom     = prmUom   
                    buff_itemfg.curr-code[1]  = prmCurr 
                    buff_itemfg.procat       = prmCateg 
                     
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
                     buff_itemfg.stat        = prmStat 
                    buff_itemfg.i-code       = prmstock 
                    buff_itemfg.ship-meth    = IF prmcasepal = "Yes" THEN TRUE ELSE FALSE  
                    buff_itemfg.type-code    =  prmtypecode 
                    buff_itemfg.std-mat-cost = dec(prmMat)    
                    buff_itemfg.std-lab-cost = dec(prmLab)    
                    buff_itemfg.std-var-cost = dec(prmVar)    
                    buff_itemfg.std-fix-cost = DEC(prmFix)    
                    buff_itemfg.total-std-cost = DEC(prmStd)  
                    buff_itemfg.avg-cost     = dec(prmAvg)
                    buff_itemfg.last-cost    = dec(prmLast)       
                    buff_itemfg.std-uom      = prmlUom  

                        .
                   
                    find first fgcat where fgcat.company = prmComp and
                        fgcat.procat = prmCateg  no-lock no-error.
                   
                      buff_itemfg.procat-desc = if avail fgcat then fgcat.dscr else "".
                        
                    find first style where style.company = prmComp and
                        style.style = prmStyle no-lock no-error.
                     buff_itemfg.style-desc  = if avail style then style.dscr else "".
                   
                    


               END.  /*if available itemfg*/
/*******************************************************************************************/
               FIND FIRST buff_itemfg where buff_itemfg.company EQ prmComp AND
                    buff_itemfg.i-no = prmItemNum EXCLUSIVE-LOCK NO-ERROR.
               FOR EACH oe-ordl  WHERE oe-ordl.company EQ prmComp AND
                         oe-ordl.i-no    EQ buff_itemfg.i-no EXCLUSIVE-LOCK:

                   /*FIND buff_oe-ordl WHERE buff_oe-ordl.company EQ prmComp AND
                        buff_oe-ordl.i-no EQ oe-ordl.i-no EXCLUSIVE NO-WAIT NO-ERROR.*/
                   IF AVAIL oe-ordl THEN DO:
                       ASSIGN
                           oe-ordl.i-name = buff_itemfg.i-name
                           oe-ordl.part-no = buff_itemfg.part-no
                           oe-ordl.part-dscr1 = buff_itemfg.part-dscr1
                           oe-ordl.part-dscr2 = buff_itemfg.part-dscr2.
                       
                   END.

               END.
/**********************************************************************************************/
                   
               FIND FIRST buff_itemfg where buff_itemfg.company EQ prmComp AND
                        buff_itemfg.i-no = prmItemNum EXCLUSIVE-LOCK NO-ERROR.
               IF TRIM(buff_itemfg.est-no) GT "" THEN DO:
                 IF AVAILABLE buff_itemfg THEN DO:
                       FOR EACH eb
                           WHERE eb.company EQ buff_itemfg.company
                           AND eb.cust-no EQ buff_itemfg.cust-no
                           AND ((eb.part-no EQ buff_itemfg.part-no
                           AND   eb.stock-no EQ "")
                           OR   eb.stock-no EQ buff_itemfg.i-no) EXCLUSIVE-LOCK:
                          
                           IF AVAIL eb THEN DO:
                               ASSIGN
                               eb.part-no = buff_itemfg.part-no
                               eb.part-dscr1 = buff_itemfg.i-name
                               eb.part-dscr2 = buff_itemfg.part-dscr1
                               eb.plate-no = buff_itemfg.plate-no
                               eb.spc-no = buff_itemfg.spc-no
                               eb.upc-no = buff_itemfg.upc-no.
                             
                           END.
                       END. /* each eb */
                    END. /* if avail */
             END.

      RUN  build-query.
     
     /* RELEASE buff_itemfg.*/
 END.   /*if prmAction = "Update"*/


 IF prmAction = "Recalcost" THEN DO:
           
           for each itemfg
               where itemfg.company eq cocode
               and itemfg.i-no eq prmItemNum no-lock:

               IF itemfg.isaset AND itemfg.alloc THEN
                   RUN util/fixfgcst.p (ROWID(itemfg)).
               ELSE
                   RUN fg/updfgcs1.p (recid(itemfg), YES).
             end.
           ASSIGN
               cError = "Recalc Costs completed....".
              RUN  build-query.
 END.
                        
  /*********************************************************************************************************************************************/              
 
PROCEDURE build-query:
           
                 FIND FIRST itemfg where itemfg.company EQ prmComp AND
                     itemfg.i-no = prmItemNum NO-LOCK NO-ERROR.
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
                         ttOrdItem.Style         = itemfg.style  
                         ttOrdItem.styledesc     = itemfg.style-desc
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
                         ttOrdItem.LastUom       = itemfg.prod-uom
                         ttOrdItem.Exempt        = itemfg.exempt-disc
                         ttOrdItem.stock         = itemfg.i-code
                         ttOrdItem.casepal       = itemfg.ship-meth
                         ttOrdItem.typecode      = itemfg.type-code
                         .

               FOR FIRST reftable WHERE reftable.reftable = "FGSTATUS" 
                                    AND  reftable.code = itemfg.i-no
                                    AND reftable.loc = "" AND reftable.company = itemfg.company NO-LOCK :
                     ASSIGN
                             ttOrdItem.stat = reftable.code2.
                     END.  /**IF  AVAIL reftable THEN DO:*/
                     
                    DO TRANSACTION:
                        {sys/inc/graphic.i}
                            {sys/inc/fgsecur.i}
                            END.
                     IF fgsecurity-log THEN
                         DO:
                         FIND FIRST usergrps WHERE
                             usergrps.usergrps = fgsecurity-char
                             NO-LOCK NO-ERROR.
                         IF AVAIL usergrps AND (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
                                                TRIM(usergrps.users) NE "*") THEN
                             ASSIGN  ttOrdItem.usercostfram = "Yes"  .
             
                                END.


                             
 END PROCEDURE.




