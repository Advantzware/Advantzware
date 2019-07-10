/*------------------------------------------------------------------------
    File        : ItemDailyUsage.p
    Purpose     : Item Daily Usage
    Syntax      :

    Description : Return a Dataset of Item Daily Usage
    Author(s)   : 
    Created     : 25 Dec 2008 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttItemUsage NO-UNDO
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
        .
DEFINE DATASET dsItemUsage FOR ttItemUsage .

DEFINE INPUT PARAMETER prmUser        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqNum      AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRcptDate    AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER prmCustPoNum   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustPartNum AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItemNum   AS CHAR        NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemUsage.

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


IF prmAction = "Select" THEN DO:
    FOR EACH vend-whse-trans WHERE  vend-whse-trans.trans-type = "U" NO-LOCK:
            create ttItemUsage.
            assign
                ttItemUsage.vSeqNum            = vend-whse-trans.r-no
                ttItemUsage.vUsgDate           = vend-whse-trans.trans-date
                ttItemUsage.vQtyUsed           = vend-whse-trans.trans-qty
                ttItemUsage.vCustPoNum         = vend-whse-trans.item-po-no
                ttItemUsage.vCustPoLineNum     = vend-whse-trans.item-line-no
                ttItemUsage.vCustPartNum       = vend-whse-trans.cust-part-no
                ttItemUsage.vFgItmNum          = vend-whse-trans.fg-item-no
                ttItemUsage.vCustVenCode       = vend-whse-trans.vendor-code
                ttItemUsage.vCustPlantId       = vend-whse-trans.vendor-plant-code
                ttItemUsage.vCustDptCod        = vend-whse-trans.vendor-dept-code
                ttItemUsage.vVenOrdNum         = vend-whse-trans.vend-ord-no
                ttItemUsage.vVenJobNum         = vend-whse-trans.vend-job-no
                ttItemUsage.vVenJob2Num        = vend-whse-trans.vend-job-no2
                ttItemUsage.vItmSelPrice       = vend-whse-trans.sell-price
                ttItemUsage.vCustHandQty       = vend-whse-trans.plant-tot-oh-qty
                ttItemUsage.vTransType         = vend-whse-trans.trans-type
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
                           AND  vend-whse-trans.trans-type = "U"   NO-LOCK:
          CREATE ttItemUsage.
            assign
                ttItemUsage.vSeqNum            = vend-whse-trans.r-no
                ttItemUsage.vUsgDate           = vend-whse-trans.trans-date
                ttItemUsage.vQtyUsed           = vend-whse-trans.trans-qty
                ttItemUsage.vCustPoNum         = vend-whse-trans.item-po-no
                ttItemUsage.vCustPoLineNum     = vend-whse-trans.item-line-no
                ttItemUsage.vCustPartNum       = vend-whse-trans.cust-part-no
                ttItemUsage.vFgItmNum          = vend-whse-trans.fg-item-no
                ttItemUsage.vCustVenCode       = vend-whse-trans.vendor-code
                ttItemUsage.vCustPlantId       = vend-whse-trans.vendor-plant-code
                ttItemUsage.vCustDptCod        = vend-whse-trans.vendor-dept-code
                ttItemUsage.vVenOrdNum         = vend-whse-trans.vend-ord-no
                ttItemUsage.vVenJobNum         = vend-whse-trans.vend-job-no
                ttItemUsage.vVenJob2Num        = vend-whse-trans.vend-job-no2
                ttItemUsage.vItmSelPrice       = vend-whse-trans.sell-price
                ttItemUsage.vCustHandQty       = vend-whse-trans.plant-tot-oh-qty
                ttItemUsage.vTransType         = vend-whse-trans.trans-type
              .
    END. /*FOR EACH vend-whse-trans*/
END. /* IF prmAction = "Search" THEN DO: */

/*************************************************/
                   


