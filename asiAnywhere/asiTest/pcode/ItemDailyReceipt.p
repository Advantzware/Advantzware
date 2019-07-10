
/*------------------------------------------------------------------------
    File        : ItemDailyReceipt.p
    Purpose     : Item Daily Receipt
    Syntax      :

    Description : Return a Dataset of Item Daily Receipt
    Author(s)   : 
    Created     : 7 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
MESSAGE "test1" .
DEFINE TEMP-TABLE ttItemReceipt NO-UNDO
        FIELD vSeqNum            AS INT 
        FIELD vUsgDate           AS DATE FORMAT "99/99/99"  
        FIELD vQtyUsed           AS DEC   
        FIELD vCustPoNum         AS CHAR    
        FIELD vCustPoLineNum     AS INT    
        FIELD vCustPartNum       AS CHAR    
        FIELD vItemFgNum         AS CHAR 
        FIELD vCustVenCode       AS CHAR 
        FIELD vCustPlantId       AS CHAR  
        FIELD vDeptCode          AS CHAR   
        FIELD vVenOrdNum         AS INT 
        FIELD vVenJobNum         AS CHAR 
        FIELD vVenJob2Num        AS INT    
        FIELD vSellPrice         AS DEC    
        FIELD vOnHandQty         AS DEC    
        FIELD vTransType         AS CHAR 
        FIELD vBolno             AS INT 

        .
DEFINE DATASET dsItemReceipt FOR ttItemReceipt .

DEFINE INPUT PARAMETER prmUser        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqNum      AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRcptDate    AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER prmCustPoNum   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustPartNum AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItemNum   AS CHAR        NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemReceipt.

IF prmUser        = ? THEN ASSIGN prmUser = "".
IF prmAction      = ? THEN ASSIGN prmAction = "Select".
IF prmSeqNum      = ? THEN ASSIGN prmSeqNum     = 0.   
IF prmComp        = ? THEN ASSIGN prmComp     = "".
IF prmCustPoNum   = ? THEN ASSIGN prmCustPoNum = "".
IF prmCustPartNum = ? THEN ASSIGN prmCustPartNum = "".
IF prmFgItemNum   = ? THEN ASSIGN prmFgItemNum = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

MESSAGE "test2" prmAction prmSeqNum prmUser prmComp.
IF prmAction = "Select" THEN DO:
    FOR EACH vend-whse-trans WHERE  vend-whse-trans.trans-type = "R" NO-LOCK:
            create ttItemReceipt.
            assign
                ttItemReceipt.vSeqNum            = vend-whse-trans.r-no
                ttItemReceipt.vBolno             = vend-whse-trans.vend-bol-no
                ttItemReceipt.vUsgDate           = vend-whse-trans.trans-date
                ttItemReceipt.vQtyUsed           = vend-whse-trans.trans-qty
                ttItemReceipt.vCustPoNum         = vend-whse-trans.item-po-no
                ttItemReceipt.vCustPoLineNum     = vend-whse-trans.item-line-no
                ttItemReceipt.vCustPartNum       = vend-whse-trans.cust-part-no
                ttItemReceipt.vItemFgNum         = vend-whse-trans.fg-item-no
                ttItemReceipt.vCustVenCode       = vend-whse-trans.vendor-code
                ttItemReceipt.vCustPlantId       = vend-whse-trans.vendor-plant-code
                ttItemReceipt.vDeptCode          = vend-whse-trans.vendor-dept-code
                ttItemReceipt.vVenOrdNum         = vend-whse-trans.vend-ord-no
                ttItemReceipt.vVenJobNum         = vend-whse-trans.vend-job-no
                ttItemReceipt.vVenJob2Num        = vend-whse-trans.vend-job-no2
                ttItemReceipt.vSellPrice         = vend-whse-trans.sell-price
                ttItemReceipt.vOnHandQty         = vend-whse-trans.plant-tot-oh-qty
                ttItemReceipt.vTransType         = vend-whse-trans.trans-type 
              .
       END. /*FOR EACH vend-whse-trans*/
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/

    
IF prmAction = "Search" THEN DO:
    FOR EACH vend-whse-trans WHERE (vend-whse-trans.r-no = prmSeqNum or prmSeqNum = 0 )
                           AND (vend-whse-trans.item-po-no BEGINS prmCustPoNum or prmCustPoNum = "" )
                           AND (vend-whse-trans.trans-date = prmRcptDate or prmRcptDate = ? OR prmRcptDate = 01/13/001 )
                           AND (vend-whse-trans.cust-part-no BEGINS prmCustPartNum OR prmCustPartNum = "" )
                           AND (vend-whse-trans.fg-item-no  BEGINS prmFgItemNum OR prmFgItemNum = "" )
                           AND  vend-whse-trans.trans-type = "R"   NO-LOCK:
          CREATE ttItemReceipt.
            assign
                ttItemReceipt.vSeqNum            = vend-whse-trans.r-no
                ttItemReceipt.vBolno             = vend-whse-trans.vend-bol-no
                ttItemReceipt.vUsgDate           = vend-whse-trans.trans-date
                ttItemReceipt.vQtyUsed           = vend-whse-trans.trans-qty
                ttItemReceipt.vCustPoNum         = vend-whse-trans.item-po-no
                ttItemReceipt.vCustPoLineNum     = vend-whse-trans.item-line-no
                ttItemReceipt.vCustPartNum       = vend-whse-trans.cust-part-no
                ttItemReceipt.vItemFgNum         = vend-whse-trans.fg-item-no
                ttItemReceipt.vCustVenCode       = vend-whse-trans.vendor-code
                ttItemReceipt.vCustPlantId       = vend-whse-trans.vendor-plant-code
                ttItemReceipt.vDeptCode          = vend-whse-trans.vendor-dept-code
                ttItemReceipt.vVenOrdNum         = vend-whse-trans.vend-ord-no
                ttItemReceipt.vVenJobNum         = vend-whse-trans.vend-job-no
                ttItemReceipt.vVenJob2Num        = vend-whse-trans.vend-job-no2
                ttItemReceipt.vSellPrice         = vend-whse-trans.sell-price
                ttItemReceipt.vOnHandQty         = vend-whse-trans.plant-tot-oh-qty
                ttItemReceipt.vTransType         = vend-whse-trans.trans-type
              .
    END. /*FOR EACH vend-whse-trans*/
END. /* IF prmAction = "Search" THEN DO: */

/*************************************************/
                   



