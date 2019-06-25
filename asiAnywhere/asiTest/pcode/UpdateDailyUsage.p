
/*------------------------------------------------------------------------
    File        : UpdateDailyUsage.p
    Purpose     : Add,Update and Delete Daily Usage Item

    Syntax      :

    Description : Return a Dataset to Update Daily Usage

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUpdateUsage NO-UNDO
        FIELD vSeqNum            AS INT 
        FIELD vUsgDate           AS DATE FORMAT "99/99/99"  
        FIELD vQtyUsed           AS DEC   
        FIELD vCustPoNum         AS CHAR    
        FIELD vCustPoLineNum     AS INT    
        FIELD vCustPartNum       AS CHAR    
        FIELD vFgItmNum          AS CHAR 
        FIELD vCustVenCode       AS CHAR 
        FIELD vCustPlantId       AS CHAR  
        FIELD vCustDptCod        AS CHAR   
        FIELD vVenOrdNum         AS INT 
        FIELD vVenJobNum         AS CHAR
        FIELD vVenJob2Num        AS INT    
        FIELD vItmSelPrice       AS DEC    
        FIELD vCustHandQty       AS DEC    
        FIELD vTransType         AS CHAR 
        FIELD vXyz               AS CHAR 
        .
DEFINE DATASET dsUpdateUsage FOR ttUpdateUsage .

DEFINE INPUT PARAMETER prmUser         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqNum       AS INT         NO-UNDO.
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

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUpdateUsage.

IF prmUser        = ?  THEN ASSIGN prmUser         = "".
IF prmAction      = ?  THEN ASSIGN prmAction       = "".
IF prmSeqNum      = ?  THEN ASSIGN prmSeqNum       = 0.   
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
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-oe-ordl FOR oe-ordl.
   DEF BUFFER b-vend-whse-item FOR vend-whse-item.
   DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.
DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR v-cnt AS INTEGER NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp .

/***********Validation*********/
IF prmAction = "Update" THEN DO:


    if prmCustVnCode <> "" then DO:
    FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.vendor-code = prmCustVnCode AND vend-code-cust-xref.company = prmComp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vend-code-cust-xref THEN DO:
        ASSIGN cError  = "Invalid Customers A/P Code ".
        RETURN.
      END.
    END.
     IF prmPartNum <> "" THEN DO:
    FIND FIRST itemfg WHERE itemfg.part-no = prmPartNum AND itemfg.company = prmComp  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE itemfg THEN DO:
            ASSIGN cError  = "Invalid Customer Part #.".
            RETURN.
       END.
       FIND FIRST vend-whse-item WHERE vend-whse-item.company = cocode
                                               AND TRIM(vend-whse-item.cust-part-no) = TRIM(prmPartNum) NO-LOCK NO-ERROR.
        IF NOT AVAIL vend-whse-item THEN DO:
         ASSIGN cError  =  "Customers Part Number " +  prmPartNum +  " does not exist in the Vendor Management Item File. " +  
                 "Please add the Customers Part Number to the Vendor Management Item File." .
         RETURN .
        END.

    END.
    
      IF prmFgItmNum <> "" THEN DO:
        FIND FIRST itemfg WHERE itemfg.i-no = prmFgItmNum AND itemfg.company = prmComp  NO-LOCK NO-ERROR.
       IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN cError  = "Invalid Item #.".
        RETURN.
       END.
       FIND FIRST vend-whse-item WHERE TRIM(vend-whse-item.company) = TRIM(cocode)
                                         AND TRIM(vend-whse-item.fg-item-no) = TRIM(prmFgItmNum) NO-LOCK NO-ERROR.
       IF NOT AVAIL vend-whse-item THEN DO:
         ASSIGN cError  = " FG Item " +  prmFgItmNum + " does not exist in the Vendor Management Item File." +  
                 "Please add the FG Item to the Vendor Management Item File." . 
        RETURN .
      END.

      END.
        
      IF prmCustPlantId <> "" THEN DO:
        FIND FIRST vend-plant WHERE vend-plant.plant-id = prmCustPlantId 
        AND vend-plant.company = prmComp  AND vend-plant.vendor-code = prmCustVnCode  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vend-plant THEN DO:
          ASSIGN cError  = "Invalid Customers Plant ID.   ".
          RETURN.
      END.
      END.
          
     IF prmCustLineNum <> 0 THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.line = prmCustLineNum AND oe-ordl.company = prmComp AND oe-ordl.ord-no = prmCustOrdNum 
        AND oe-ordl.i-no    = prmFgItmNum  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE oe-ordl THEN DO:
        ASSIGN cError  = "Invalid Suppliers Order Line Number .".
        RETURN.
     END. 
    END.
       
       IF prmCustPoNum <> "" THEN DO:
            FOR EACH b-oe-rel NO-LOCK WHERE b-oe-rel.company = prmComp
                AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                AND b-oe-rel.po-no   = prmCustPoNum:
                
                v-cnt = v-cnt + 1.
            END.
            
            IF v-cnt <= 1 THEN DO:
                FIND FIRST b-oe-rel WHERE b-oe-rel.company = PrmComp
                    AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                    AND b-oe-rel.po-no   = prmCustPoNum NO-LOCK NO-ERROR.
                IF NOT AVAILABLE(b-oe-rel) THEN DO:
                    ASSIGN 
                        cError  = "Invalid PO #.".
                    RETURN.
                    END.
            END.
         END.

      IF prmCustJobNum <> "" THEN  DO:
    FIND FIRST oe-ordl WHERE oe-ordl.job-no = prmCustJobNum AND oe-ordl.company = prmComp 
        AND oe-ordl.ord-no   =  prmCustOrdNum  AND oe-ordl.line = prmCustLineNum   NO-LOCK NO-ERROR.
       IF NOT AVAILABLE oe-ordl THEN DO:
           ASSIGN cError  = "Invalid Suppliers Job Number.     ".
           RETURN.
      END.
      END.
                                
      IF prmCustJob2Num <> 0 THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.job-no2 = prmCustJob2Num AND oe-ordl.company = prmComp 
        AND oe-ordl.job-no = prmCustJobNum  AND oe-ordl.ord-no   =  prmCustOrdNum  AND oe-ordl.line = prmCustLineNum  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE oe-ordl THEN DO:
             ASSIGN cError  = "Invalid Supliers Job 2 Number    .".
             RETURN.
         END.
      END.
  
    IF prmCustOrdNum <> 0 THEN DO:
     FIND FIRST oe-ordl WHERE oe-ordl.ord-no = prmCustOrdNum AND oe-ordl.company = prmComp 
            AND oe-ordl.i-no    = prmFgItmNum NO-LOCK NO-ERROR.
        IF NOT AVAILABLE oe-ordl THEN DO:
            ASSIGN cError  = "Invalid Order #.".
            RETURN.
         END.
    END.
    IF  prmQtyUsed  < 0 THEN DO:
        ASSIGN
            cError = "Usage Quantity must be greater than 0     ".
        RETURN.
    END.

    IF prmCustDptCode <> "" THEN DO:
        FIND FIRST vend-plant WHERE vend-plant.company  = prmComp AND vend-plant.vendor-code  = prmCustVnCode
            AND vend-plant.plant-id  = prmCustPlantId
            AND vend-plant.vendor-dept-code = prmCustDptCode  NO-LOCK NO-ERROR.         
            IF NOT AVAIL vend-plant THEN DO:
                ASSIGN
                    cError = "Invalid Customers Plant Dept Code.      " .
                RETURN .
            END.
      END.


    FIND  FIRST  b-vend-whse-trans WHERE b-vend-whse-trans.company  = prmComp  AND b-vend-whse-trans.trans-type  = "U"
        AND b-vend-whse-trans.cust-part-no  = prmPartNum  AND b-vend-whse-trans.fg-item-no  = prmFgItmNum
        AND b-vend-whse-trans.vendor-code   = prmCustVnCode  AND b-vend-whse-trans.vendor-dept-code  = prmCustDptCode
        AND b-vend-whse-trans.vendor-plant-code = prmCustPlantId  AND b-vend-whse-trans.r-no <> INT(prmSeqNum) NO-LOCK NO-ERROR.
     IF AVAIL b-vend-whse-trans  THEN DO:
         ASSIGN
            cError = "Usage transaction already exists for: " .
        RETURN.
    END.

     END.  /* validation of update*/



IF prmAction = "Add" THEN DO:

  if prmCustVnCode <> "" then DO:
    FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.vendor-code = prmCustVnCode AND vend-code-cust-xref.company = prmComp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vend-code-cust-xref THEN DO:
        ASSIGN cError  = "Invalid Customers A/P Code ".
        RETURN.
      END.
    END.
     IF prmPartNum <> "" THEN DO:
    FIND FIRST itemfg WHERE itemfg.part-no = prmPartNum AND itemfg.company = prmComp  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE itemfg THEN DO:
            ASSIGN cError  = "Invalid Customer Part #.".
            RETURN.
       END.
       FIND FIRST vend-whse-item WHERE vend-whse-item.company = cocode
                                               AND TRIM(vend-whse-item.cust-part-no) = TRIM(prmPartNum) NO-LOCK NO-ERROR.
        IF NOT AVAIL vend-whse-item THEN DO:
         ASSIGN cError  =  "Customers Part Number " +  prmPartNum +  " does not exist in the Vendor Management Item File. " +
                 "Please add the Customers Part Number to the Vendor Management Item File." .
         RETURN .
        END.

    END.
      IF prmFgItmNum <> "" THEN DO:
        FIND FIRST itemfg WHERE itemfg.i-no = prmFgItmNum AND itemfg.company = prmComp  NO-LOCK NO-ERROR.
       IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN cError  = "Invalid Item #.".
        RETURN.
       END.
       FIND FIRST vend-whse-item WHERE TRIM(vend-whse-item.company) = TRIM(cocode)
                                         AND TRIM(vend-whse-item.fg-item-no) = TRIM(prmFgItmNum) NO-LOCK NO-ERROR.
       IF NOT AVAIL vend-whse-item THEN DO:
         ASSIGN cError  = " FG Item " +  prmFgItmNum + " does not exist in the Vendor Management Item File. " +  
                 "Please add the FG Item to the Vendor Management Item File." . 
        RETURN .
      END.

      END.
        
      IF prmCustPlantId <> "" THEN DO:
        FIND FIRST vend-plant WHERE vend-plant.plant-id = prmCustPlantId 
        AND vend-plant.company = prmComp  AND vend-plant.vendor-code = prmCustVnCode  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vend-plant THEN DO:
          ASSIGN cError  = "Invalid Customers Plant ID.   ".
          RETURN.
      END.
      END.
   
     IF prmCustLineNum <> 0 THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.line = prmCustLineNum AND oe-ordl.company = prmComp AND oe-ordl.ord-no = prmCustOrdNum 
        AND oe-ordl.i-no    = prmFgItmNum  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE oe-ordl THEN DO:
        ASSIGN cError  = "Invalid Suppliers Order Line Number .".
        RETURN.
     END. 
    END.
       
       IF prmCustPoNum <> "" THEN DO:
            FOR EACH b-oe-rel NO-LOCK WHERE b-oe-rel.company = prmComp
                AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                AND b-oe-rel.po-no   = prmCustPoNum:
                
                v-cnt = v-cnt + 1.
            END.
            
            IF v-cnt <= 1 THEN DO:
                FIND FIRST b-oe-rel WHERE b-oe-rel.company = PrmComp
                    AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                    AND b-oe-rel.po-no   = prmCustPoNum NO-LOCK NO-ERROR.
                IF NOT AVAILABLE(b-oe-rel) THEN DO:
                    ASSIGN 
                        cError  = "Invalid PO #.".
                    RETURN.
                    END.
            END.
         END.

      IF prmCustJobNum <> "" THEN  DO:
    FIND FIRST oe-ordl WHERE oe-ordl.job-no = prmCustJobNum AND oe-ordl.company = prmComp 
        AND oe-ordl.ord-no   =  prmCustOrdNum  AND oe-ordl.line = prmCustLineNum   NO-LOCK NO-ERROR.
       IF NOT AVAILABLE oe-ordl THEN DO:
           ASSIGN cError  = "Invalid Suppliers Job Number.     ".
           RETURN.
      END.
      END.
                                
      IF prmCustJob2Num <> 0 THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.job-no2 = prmCustJob2Num AND oe-ordl.company = prmComp 
        AND oe-ordl.job-no = prmCustJobNum  AND oe-ordl.ord-no   =  prmCustOrdNum  AND oe-ordl.line = prmCustLineNum  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE oe-ordl THEN DO:
             ASSIGN cError  = "Invalid Supliers Job 2 Number    .".
             RETURN.
         END.
      END.
  
    IF prmCustOrdNum <> 0 THEN DO:
     FIND FIRST oe-ordl WHERE oe-ordl.ord-no = prmCustOrdNum AND oe-ordl.company = prmComp 
            AND oe-ordl.i-no    = prmFgItmNum NO-LOCK NO-ERROR.
        IF NOT AVAILABLE oe-ordl THEN DO:
            ASSIGN cError  = "Invalid Order #.".
            RETURN.
         END.
    END.
    IF  prmQtyUsed  < 0 THEN DO:
        ASSIGN
            cError = "Usage Quantity must be greater than 0     ".
        RETURN.
    END.

    IF prmCustDptCode <> "" THEN DO:
        FIND FIRST vend-plant WHERE vend-plant.company  = prmComp AND vend-plant.vendor-code  = prmCustVnCode
            AND vend-plant.plant-id  = prmCustPlantId
            AND vend-plant.vendor-dept-code = prmCustDptCode  NO-LOCK NO-ERROR.         
            IF NOT AVAIL vend-plant THEN DO:
                ASSIGN
                    cError = "Invalid Customers Plant Dept Code.      " .
                RETURN .
            END.
      END.


  END. 
/*************Add******************************/

IF prmAction = "Add" THEN DO:
  FIND LAST vend-whse-trans WHERE  vend-whse-trans.company = prmComp   NO-LOCK NO-ERROR.
  
    IF AVAIL vend-whse-trans THEN
        
     ASSIGN    v-r-no = vend-whse-trans.r-no + 1.
    ELSE
     ASSIGN    v-r-no = 1.

     FIND  FIRST  b-vend-whse-trans WHERE b-vend-whse-trans.company  = prmComp  AND b-vend-whse-trans.trans-type  = "U"
        AND b-vend-whse-trans.cust-part-no  = prmPartNum  AND b-vend-whse-trans.fg-item-no  = prmFgItmNum
        AND b-vend-whse-trans.vendor-code   = prmCustVnCode  AND b-vend-whse-trans.vendor-dept-code  = prmCustDptCode
        AND b-vend-whse-trans.vendor-plant-code = prmCustPlantId  AND b-vend-whse-trans.r-no <> INT(v-r-no) NO-LOCK NO-ERROR.
     IF AVAIL b-vend-whse-trans  THEN DO:
         ASSIGN
            cError = "Usage transaction already exists for: " .
        RETURN.
     END.


       CREATE vend-whse-trans.
            ASSIGN
               vend-whse-trans.r-no               = v-r-no
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
               vend-whse-trans.trans-type         = "U"
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


      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = prmComp 
                                 AND b-vend-whse-item.vendor-code = prmCustVnCode 
                                 AND b-vend-whse-item.vendor-plant-code = prmCustPlantId
                                 AND b-vend-whse-item.fg-item-no = prmFgItmNum
                                 AND b-vend-whse-item.cust-part-no = prmPartNum 
                                 AND b-vend-whse-item.vendor-dept-code = prmCustDptCode  NO-ERROR.
      IF AVAILABLE(b-vend-whse-item) THEN DO:
          ASSIGN
              vend-whse-trans.plant-tot-oh-qty = b-vend-whse-item.plant-tot-oh-qty - DECI(prmQtyUsed)
              vend-whse-trans.cust-no          = b-vend-whse-item.cust-no.
          END.

            ASSIGN 
                prmSeqNum = v-r-no
                prmAction = "Select".

               
END. /*IF prmAction = "Add" THEN DO:*/

/**********************Update**********************************/


IF prmAction = "Update" THEN DO:
   
    FIND FIRST vend-whse-trans WHERE vend-whse-trans.r-no = prmSeqNum AND  vend-whse-trans.trans-type = "U" EXCLUSIVE-LOCK NO-ERROR.
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
               vend-whse-trans.fg-item-no         = prmFgItmNum  .

      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = prmComp 
                                 AND b-vend-whse-item.vendor-code = prmCustVnCode 
                                 AND b-vend-whse-item.vendor-plant-code = prmCustPlantId
                                 AND b-vend-whse-item.fg-item-no = prmFgItmNum
                                 AND b-vend-whse-item.cust-part-no = prmPartNum 
                                 AND b-vend-whse-item.vendor-dept-code = prmCustDptCode  NO-ERROR.
      IF AVAILABLE(b-vend-whse-item) THEN DO:
          ASSIGN
              vend-whse-trans.plant-tot-oh-qty = b-vend-whse-item.plant-tot-oh-qty - DECI(prmQtyUsed)
              vend-whse-trans.cust-no          = b-vend-whse-item.cust-no.
          END.
          
                ASSIGN 
                    prmAction = "Select".
            
END.

/**********************delete*******************************************************/

IF prmAction = "Delete"  THEN DO:
    FIND FIRST vend-whse-trans WHERE vend-whse-trans.r-no = prmSeqNum AND vend-whse-trans.trans-type = "U" AND vend-whse-trans.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
      
    IF AVAIL vend-whse-trans THEN DO:
      DELETE vend-whse-trans.
    END.
    FIND LAST vend-whse-trans WHERE vend-whse-trans.trans-type = "U" AND vend-whse-trans.company = prmComp  NO-LOCK NO-ERROR.
     IF AVAIL vend-whse-trans  THEN
         ASSIGN 
         prmSeqNum = vend-whse-trans.r-no
         prmAction = "Select" .
END. /*IF prmAction = "delete"*/

/*****************Select********************/

IF prmAction = "Select" THEN DO:
 FIND FIRST vend-whse-trans WHERE vend-whse-trans.trans-type = "U" AND vend-whse-trans.r-no = prmSeqNum AND vend-whse-trans.company = prmComp NO-LOCK NO-ERROR.
     create ttUpdateUsage.
            assign
                ttUpdateUsage.vSeqNum            = vend-whse-trans.r-no
                ttUpdateUsage.vUsgDate           = vend-whse-trans.trans-date
                ttUpdateUsage.vQtyUsed           = vend-whse-trans.trans-qty
                ttUpdateUsage.vCustPoNum         = vend-whse-trans.item-po-no
                ttUpdateUsage.vCustPoLineNum     = vend-whse-trans.item-line-no
                ttUpdateUsage.vCustPartNum       = vend-whse-trans.cust-part-no
                ttUpdateUsage.vFgItmNum          = vend-whse-trans.fg-item-no
                ttUpdateUsage.vCustVenCode       = vend-whse-trans.vendor-code
                ttUpdateUsage.vCustPlantId       = vend-whse-trans.vendor-plant-code
                ttUpdateUsage.vCustDptCod        = vend-whse-trans.vendor-dept-code
                ttUpdateUsage.vVenOrdNum         = vend-whse-trans.vend-ord-no
                ttUpdateUsage.vVenJobNum         = vend-whse-trans.vend-job-no
                ttUpdateUsage.vVenJob2Num        = vend-whse-trans.vend-job-no2
                ttUpdateUsage.vItmSelPrice       = vend-whse-trans.sell-price
                ttUpdateUsage.vCustHandQty       = vend-whse-trans.plant-tot-oh-qty
                ttUpdateUsage.vTransType         = vend-whse-trans.trans-type
              .
       END. /*FOR EACH vend-whse-trans*/



