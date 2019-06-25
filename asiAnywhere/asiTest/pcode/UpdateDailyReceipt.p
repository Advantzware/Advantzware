

/*------------------------------------------------------------------------
    File        : UpdateDailyReceipt.p
    Purpose     : Add,Update and Delete Daily Receipt Item

    Syntax      :

    Description : Return a Dataset to Update Daily Receipt

    Author(s)   : 
    Created     : 7 Jan 2009
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUpdateReceipt NO-UNDO
        FIELD vSeqNum            AS INT 
        FIELD vVendBolNo         AS INT 
        FIELD vCustPartNum       AS CHAR
        FIELD vQtyUsed           AS DEC
        FIELD vOnHandQty         AS DEC 
        FIELD vUsgDate           AS DATE FORMAT "99/99/99"  
        FIELD vCustVenCode       AS CHAR 
        FIELD vCustPlantId       AS CHAR 
        FIELD vDeptCode          AS CHAR 
        FIELD vItemFgNum         AS CHAR
        FIELD vVenOrdNum         AS INT
        FIELD vCustPoLineNum     AS INT          
        FIELD vCustPoNum         AS CHAR                                                                    
        FIELD vVenJobNum         AS CHAR
        FIELD vVenJob2Num        AS INT    
        FIELD vSellPrice         AS DEC               
        FIELD vTransType         AS CHAR 
        FIELD vXyz               AS CHAR 
        .
DEFINE DATASET dsUpdateReceipt FOR ttUpdateReceipt .

DEFINE INPUT PARAMETER prmUser         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqNum       AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmVendBolNo    AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTranDate     AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER prmQtyUsed      AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmCustPoNum    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustLineNum  AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmPartNum      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItmNum     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustVnCode   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustPlantId  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustDptCode  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustOrdNum   AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCustJobNum   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustJob2Num  AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmSellingPrice AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmOnHandQty    AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmTranType     AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError         AS CHAR        NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUpdateReceipt.

IF prmUser        = ?  THEN ASSIGN prmUser         = "".
IF prmAction      = ?  THEN ASSIGN prmAction       = "".
IF prmSeqNum      = ?  THEN ASSIGN prmSeqNum       = 0.   
IF prmVendBolNo   = ?  THEN ASSIGN prmVendBolNo    = 0. 
IF prmComp        = ?  THEN ASSIGN prmComp         = "".
IF prmQtyUsed     = ?  THEN ASSIGN prmQtyUsed      = 0 .
IF prmCustPoNum   = ?  THEN ASSIGN prmCustPoNum    = "".
IF prmCustLineNum = ?  THEN ASSIGN prmCustLineNum  = 0.
IF prmPartNum     = ?  THEN ASSIGN prmPartNum      = "".
IF prmFgItmNum    = ?  THEN ASSIGN prmFgItmNum     = "".
IF prmCustVnCode  = ?  THEN ASSIGN prmCustVnCode   = "".
IF prmCustPlantId = ?  THEN ASSIGN prmCustPlantId  = "".
IF prmCustDptCode = ?  THEN ASSIGN prmCustDptCode  = "".
IF prmCustOrdNum  = ?  THEN ASSIGN prmCustOrdNum   = 0.
IF prmCustJobNum  = ?  THEN ASSIGN prmCustJobNum   = "".
IF prmCustJob2Num = ?  THEN ASSIGN prmCustJob2Num  = 0.
IF prmSellingPrice = ? THEN ASSIGN prmSellingPrice = 0.
IF prmOnHandQty    = ? THEN ASSIGN prmOnHandQty    = 0.
IF prmTranType     = ? THEN ASSIGN prmTranType     = "".
IF cError          = ? THEN ASSIGN cError          = "".


DEF VAR v-r-no   LIKE vend-whse-trans.r-no NO-UNDO.
DEF BUFFER b-vend-whse-item FOR vend-whse-item.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".



/***********Validation*********/
IF prmAction = "ValidateUpdate" THEN DO:
    MESSAGE "update" prmCustVnCode .
    IF prmCustVnCode <> "" THEN DO:
    FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.vendor-code = prmCustVnCode AND vend-code-cust-xref.company = prmComp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vend-code-cust-xref THEN DO:
        ASSIGN cError  = "Invalid Vendor Code".
        RETURN.
      END.
      END.
    
    FIND FIRST vend-plant WHERE vend-plant.plant-id = prmCustPlantId AND vend-plant.company = prmComp   
                                AND vend-plant.vendor-code = prmCustVnCode  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vend-plant THEN DO:
          ASSIGN cError  = "Invalid Plant ID #.".
          RETURN.
      END.
    
     FIND FIRST vend-plant WHERE  vend-plant.company = prmComp AND vend-plant.vendor-code = prmCustVnCode AND
         vend-plant.plant-id = prmCustPlantId AND  vend-plant.vendor-dept-code = prmCustDptCode NO-LOCK NO-ERROR.
      IF NOT AVAIL vend-plant THEN DO:
          ASSIGN
              cError =  "Invalid Customers Plant Dept Code." .
          RETURN .
     END.
    
  
END.



IF prmAction = "ValidateAdd" THEN DO:
   IF prmCustVnCode <> "" THEN DO:
    FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.vendor-code = prmCustVnCode AND vend-code-cust-xref.company = prmComp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vend-code-cust-xref THEN DO:
        ASSIGN cError  = "Invalid Vendor Code".
        RETURN.
      END.
      END.
    FIND FIRST vend-plant WHERE vend-plant.plant-id = prmCustPlantId AND vend-plant.company = prmComp   
                                AND vend-plant.vendor-code = prmCustVnCode  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vend-plant THEN DO:
          ASSIGN cError  = "Invalid Plant ID #.".
          RETURN.
      END.
    
     FIND FIRST vend-plant WHERE  vend-plant.company = prmComp AND vend-plant.vendor-code = prmCustVnCode AND
         vend-plant.plant-id = prmCustPlantId AND  vend-plant.vendor-dept-code = prmCustDptCode NO-LOCK NO-ERROR.
      IF NOT AVAIL vend-plant THEN DO:
          ASSIGN
              cError =  "Invalid Customers Plant Dept Code." .
          RETURN .
     END.
     
    
     IF prmFgItmNum <> "" THEN DO:
    FIND FIRST itemfg WHERE itemfg.i-no = prmFgItmNum AND itemfg.company = prmComp  NO-LOCK NO-ERROR.
       IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN cError  = "Invalid Item #.".
        RETURN.
       END.
     END.
     IF prmCustLineNum <> 0 THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.line = prmCustLineNum AND oe-ordl.company = prmComp AND oe-ordl.ord-no = prmCustOrdNum NO-LOCK NO-ERROR.
    IF NOT AVAILABLE oe-ordl THEN DO:
        ASSIGN cError  = "Invalid Line #.".
        RETURN.
     END.
    END.
    IF prmCustPoNum <> "" THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.po-no = prmCustPoNum AND oe-ordl.company = prmComp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE oe-ordl THEN DO:
          ASSIGN cError  = "Invalid PO #.".
          RETURN.
      END.
    END.
    /* IF prmCustJobNum <> "" THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.job-no = prmCustJobNum AND oe-ordl.company = prmComp  NO-LOCK NO-ERROR.
       IF NOT AVAILABLE oe-ordl THEN DO:
           ASSIGN cError  = "Invalid Job #.".
           RETURN.
      END.
     END.
                                
      IF prmCustJob2Num <> 0 THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.job-no2 = prmCustJob2Num AND oe-ordl.company = prmComp  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE oe-ordl THEN DO:
             ASSIGN cError  = "Invalid Job # 2.".
             RETURN.
         END.
      END.
    IF prmSellingPrice <> 0 THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.price = prmSellingPrice AND oe-ordl.company = prmComp  NO-LOCK NO-ERROR.
       IF NOT AVAILABLE oe-ordl THEN DO:
           ASSIGN cError  = "Invalid Selling Price.".
           RETURN.
       END.
    END.*/
    IF prmCustOrdNum <> 0 THEN DO:
     FIND FIRST oe-ordl WHERE oe-ordl.ord-no = prmCustOrdNum AND oe-ordl.company = prmComp  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE oe-ordl THEN DO:
            ASSIGN cError  = "Invalid Order #.".
            RETURN.
         END.
    END.
END. 
/*************Add******************************/

IF prmAction = "Add" THEN DO:
  FIND LAST vend-whse-trans WHERE   vend-whse-trans.company = prmComp   NO-LOCK NO-ERROR.
  
    IF AVAIL vend-whse-trans THEN
        
     ASSIGN    v-r-no = vend-whse-trans.r-no + 1.
    ELSE
     ASSIGN    v-r-no = 1.

       CREATE vend-whse-trans.
            ASSIGN
               vend-whse-trans.r-no               = v-r-no
               vend-whse-trans.vend-bol-no        = prmVendBolNo
               vend-whse-trans.trans-date         = prmTranDate
               vend-whse-trans.trans-qty          = prmQtyUsed
               vend-whse-trans.item-po-no         = prmCustPoNum
               vend-whse-trans.item-line-no       = prmCustLineNum
               vend-whse-trans.cust-part-no       = prmPartNum
               vend-whse-trans.fg-item-no         = prmFgItmNum
               vend-whse-trans.vendor-code        = prmCustVnCode
               vend-whse-trans.vendor-plant-code  = prmCustPlantId
               vend-whse-trans.vendor-dept-code   = prmCustDptCode
               vend-whse-trans.vend-ord-no        = prmCustOrdNum
               vend-whse-trans.vend-job-no        = prmCustJobNum
               vend-whse-trans.vend-job-no2       = prmCustJob2Num
               vend-whse-trans.sell-price         = prmSellingPrice
               vend-whse-trans.plant-tot-oh-qty   = prmQtyUsed
               vend-whse-trans.trans-type         = "R"
               vend-whse-trans.create-date        = TODAY                 
               vend-whse-trans.create-time        = TIME                  
               vend-whse-trans.create-userid      = USERID("nosweat")     
               vend-whse-trans.fg-item-no         = prmFgItmNum           
               vend-whse-trans.rec_key            = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME) 
               vend-whse-trans.trans-date         = TODAY                 
               vend-whse-trans.upd-date           = TODAY                 
               vend-whse-trans.upd-time           = TIME  
               vend-whse-trans.company            = prmComp 
                .


            ASSIGN 
                prmSeqNum = v-r-no
                prmAction = "Select".

               
END. /*IF prmAction = "Add" THEN DO:*/

/**********************Update**********************************/


IF prmAction = "Update" THEN DO:
    FIND FIRST vend-whse-trans WHERE vend-whse-trans.r-no = prmSeqNum AND  vend-whse-trans.trans-type = "R" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL vend-whse-trans THEN
          ASSIGN
               vend-whse-trans.trans-date         = prmTranDate
               vend-whse-trans.trans-qty          = prmQtyUsed
               vend-whse-trans.item-po-no         = prmCustPoNum
               vend-whse-trans.item-line-no       = prmCustLineNum
               vend-whse-trans.cust-part-no       = prmPartNum
               vend-whse-trans.fg-item-no         = prmFgItmNum
               vend-whse-trans.vendor-code        = prmCustVnCode
               vend-whse-trans.vendor-plant-code  = prmCustPlantId
               vend-whse-trans.vendor-dept-code   = prmCustDptCode
               vend-whse-trans.vend-ord-no        = prmCustOrdNum
               vend-whse-trans.vend-job-no        = prmCustJobNum
               vend-whse-trans.vend-job-no2       = prmCustJob2Num
               vend-whse-trans.sell-price         = prmSellingPrice
               /*vend-whse-trans.plant-tot-oh-qty   = prmOnHandQty*/
               vend-whse-trans.fg-item-no         = prmFgItmNum  .

           ASSIGN
               vend-whse-trans.upd-date     = TODAY
               vend-whse-trans.upd-time     = TIME
               vend-whse-trans.upd-userid   = USERID("nosweat").

           FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = prmComp
                                 AND b-vend-whse-item.vendor-code = vend-whse-trans.vendor-code 
                                 AND b-vend-whse-item.vendor-plant-code = vend-whse-trans.vendor-plant-code
                                 AND b-vend-whse-item.fg-item-no = vend-whse-trans.cust-part-no
                                 AND b-vend-whse-item.cust-part-no = vend-whse-trans.cust-part-no
                                 AND b-vend-whse-item.vendor-dept-code = vend-whse-trans.vendor-dept-code NO-LOCK NO-ERROR.

           IF AVAILABLE(b-vend-whse-item) THEN DO:
               ASSIGN
                   vend-whse-trans.plant-tot-oh-qty = b-vend-whse-item.plant-tot-oh-qty + DECI(vend-whse-trans.trans-qty).
   END.
                ASSIGN 
                    prmAction = "Select".
            
END.

/**********************delete*******************************************************/

IF prmAction = "Delete"  THEN DO:
    FIND FIRST vend-whse-trans WHERE vend-whse-trans.r-no = prmSeqNum AND vend-whse-trans.trans-type = "R" AND vend-whse-trans.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
      
    IF AVAIL vend-whse-trans THEN DO:
      DELETE vend-whse-trans.
    END.
    FIND LAST vend-whse-trans WHERE vend-whse-trans.trans-type = "R" AND vend-whse-trans.company = prmComp  NO-LOCK NO-ERROR.
     IF AVAIL vend-whse-trans  THEN
         ASSIGN 
         prmSeqNum = vend-whse-trans.r-no
         prmAction = "Select" .
END. /*IF prmAction = "delete"*/

/*****************Select********************/

IF prmAction = "Select" THEN DO:
 FIND FIRST vend-whse-trans WHERE vend-whse-trans.trans-type = "R" AND vend-whse-trans.r-no = prmSeqNum AND vend-whse-trans.company = prmComp NO-LOCK NO-ERROR.
     create ttUpdateReceipt.
            assign
                ttUpdateReceipt.vSeqNum            = vend-whse-trans.r-no
                ttUpdateReceipt.vVendBolNo         = vend-whse-trans.vend-bol-no
                ttUpdateReceipt.vUsgDate           = vend-whse-trans.trans-date
                ttUpdateReceipt.vQtyUsed           = vend-whse-trans.trans-qty
                ttUpdateReceipt.vCustPoNum         = vend-whse-trans.item-po-no
                ttUpdateReceipt.vCustPoLineNum     = vend-whse-trans.item-line-no
                ttUpdateReceipt.vCustPartNum       = vend-whse-trans.cust-part-no
                ttUpdateReceipt.vItemFgNum         = vend-whse-trans.fg-item-no
                ttUpdateReceipt.vCustVenCode       = vend-whse-trans.vendor-code
                ttUpdateReceipt.vCustPlantId       = vend-whse-trans.vendor-plant-code
                ttUpdateReceipt.vDeptCode          = vend-whse-trans.vendor-dept-code
                ttUpdateReceipt.vVenOrdNum         = vend-whse-trans.vend-ord-no
                ttUpdateReceipt.vVenJobNum         = vend-whse-trans.vend-job-no
                ttUpdateReceipt.vVenJob2Num        = vend-whse-trans.vend-job-no2
                ttUpdateReceipt.vSellPrice         = vend-whse-trans.sell-price
                ttUpdateReceipt.vOnHandQty         = vend-whse-trans.plant-tot-oh-qty
                ttUpdateReceipt.vTransType         = vend-whse-trans.trans-type
              .
       END. /*FOR EACH vend-whse-trans*/




