/*------------------------------------------------------------------------
    File        : WarehouseHistory.p
    Purpose     : Warehouse History
    Syntax      :

    Description : Return a Dataset of Warehouse History
    Author(s)   : 
    Created     : 25 Dec 2008 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttWarehouseHistory NO-UNDO
        FIELD vRNo                  AS INT 
        FIELD vTransType            AS CHAR
        FIELD vTransDate            AS CHAR
        FIELD vTransQty             AS INT
        FIELD vItemPoNo             AS CHAR
        FIELD vItemLineNo           AS INT
        FIELD vCustPartNo           AS CHAR
        FIELD vFgItemNo             AS CHAR
        FIELD vVendorCode           AS CHAR
        FIELD vVendorPlantCode      AS CHAR
        FIELD vVendorDeptCode       AS CHAR
        FIELD vVendOrdNo            AS INT
        FIELD vVendJobNo            AS CHAR
        FIELD vVendJobNo2           AS INT
        FIELD vSellPrice            AS INT
        FIELD vPlantTotOhQty        AS INT
        FIELD vCreateDate        AS DATE
        FIELD vCreatedBy        AS CHAR
        FIELD vRec_key          AS CHAR 
        .
/*DEF BUFFER b-vend-code-cust-xref FOR vend-code-cust-xref.*/
DEFINE DATASET dsWarehouseHistory FOR ttWarehouseHistory .

DEFINE INPUT PARAMETER prmUser              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqNo             AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmTransDate         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustomersPoNo     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustPart          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmReckey            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp              AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError              AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsWarehouseHistory.

IF prmUser              = ? THEN ASSIGN prmUser = "".
IF prmAction            = ? THEN ASSIGN prmAction = "Select".
IF prmSeqNo             = ? THEN ASSIGN prmSeqNo     = 0.
IF prmTransDate         = ? THEN ASSIGN prmTransDate = "".
IF prmCustomersPoNo     = ? THEN ASSIGN prmCustomersPoNo = "".
IF prmCustPart          = ? THEN ASSIGN prmCustPart = "".
IF prmFgItem            = ? THEN ASSIGN prmFgItem = "".
IF prmComp              = ? THEN ASSIGN prmComp = "".
IF prmReckey            = ? THEN ASSIGN prmReckey = "".

DEF VAR v-usercust AS LOG NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

IF prmAction = "Select" THEN DO:
      /*MAIN-LOOP:*/
    FOR EACH vend-whse-trans-hist WHERE  vend-whse-trans-hist.company = prmComp NO-LOCK:
         /*IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.*/
            create ttWarehouseHistory.
            assign
                ttWarehouseHistory.vRNo                 = vend-whse-trans-hist.r-no                
                ttWarehouseHistory.vTransType           = vend-whse-trans-hist.trans-type   
                ttWarehouseHistory.vTransDate           = STRING(vend-whse-trans-hist.trans-date)
                ttWarehouseHistory.vTransQty            = vend-whse-trans-hist.trans-qty
                ttWarehouseHistory.vItemPoNo            = vend-whse-trans-hist.item-po-no
                ttWarehouseHistory.vItemLineNo          = vend-whse-trans-hist.item-line-no                
                ttWarehouseHistory.vCustPartNo          = vend-whse-trans-hist.cust-part-no
                ttWarehouseHistory.vFgItemNo            = vend-whse-trans-hist.fg-item-no
                ttWarehouseHistory.vVendorCode          = vend-whse-trans-hist.vendor-code
                ttWarehouseHistory.vVendorPlantCode     = vend-whse-trans-hist.vendor-plant-code
                ttWarehouseHistory.vVendorDeptCode      = vend-whse-trans-hist.vendor-dept-code
                ttWarehouseHistory.vVendOrdNo           = vend-whse-trans-hist.vend-ord-no
                ttWarehouseHistory.vVendJobNo           = vend-whse-trans-hist.vend-job-no
                ttWarehouseHistory.vVendJobNo2          = vend-whse-trans-hist.vend-job-no2
                ttWarehouseHistory.vSellPrice           = vend-whse-trans-hist.sell-price
                ttWarehouseHistory.vPlantTotOhQty       = vend-whse-trans-hist.plant-tot-oh-qty  
                ttWarehouseHistory.vRec_key            = vend-whse-trans-hist.rec_key 
              .
       END. /*FOR EACH vend-whse-trans-hist*/
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/


IF prmAction = "Search" THEN DO:
     /*MAIN-LOOP:*/
    FOR EACH vend-whse-trans-hist WHERE (vend-whse-trans-hist.r-no = prmSeqNo OR prmSeqNo = 0 )   
                           AND (vend-whse-trans-hist.trans-date = DATE(prmTransDate) OR prmTransDate = "" )
                           AND (vend-whse-trans-hist.item-po-no  BEGINS prmCustomersPoNo OR prmCustomersPoNo = "" )
                           AND (vend-whse-trans-hist.cust-part-no  BEGINS prmCustPart OR prmCustPart = "" )        
                           AND (vend-whse-trans-hist.fg-item-no  BEGINS prmFgItem OR prmFgItem = "" )              
                           AND  vend-whse-trans-hist.company = prmComp   NO-LOCK:
        /*IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.*/
          CREATE ttWarehouseHistory.
            assign
                ttWarehouseHistory.vRNo                 = vend-whse-trans-hist.r-no                
                ttWarehouseHistory.vTransType           = vend-whse-trans-hist.trans-type
                ttWarehouseHistory.vTransDate           = STRING(vend-whse-trans-hist.trans-date)
                ttWarehouseHistory.vTransQty            = vend-whse-trans-hist.trans-qty
                ttWarehouseHistory.vItemPoNo            = vend-whse-trans-hist.item-po-no
                ttWarehouseHistory.vItemLineNo          = vend-whse-trans-hist.item-line-no                
                ttWarehouseHistory.vCustPartNo          = vend-whse-trans-hist.cust-part-no
                ttWarehouseHistory.vFgItemNo            = vend-whse-trans-hist.fg-item-no
                ttWarehouseHistory.vVendorCode          = vend-whse-trans-hist.vendor-code
                ttWarehouseHistory.vVendorPlantCode     = vend-whse-trans-hist.vendor-plant-code
                ttWarehouseHistory.vVendorDeptCode      = vend-whse-trans-hist.vendor-dept-code
                ttWarehouseHistory.vVendOrdNo           = vend-whse-trans-hist.vend-ord-no
                ttWarehouseHistory.vVendJobNo           = vend-whse-trans-hist.vend-job-no
                ttWarehouseHistory.vVendJobNo2          = vend-whse-trans-hist.vend-job-no2
                ttWarehouseHistory.vSellPrice           = vend-whse-trans-hist.sell-price
                ttWarehouseHistory.vPlantTotOhQty       = vend-whse-trans-hist.plant-tot-oh-qty
                ttWarehouseHistory.vRec_key            = vend-whse-trans-hist.rec_key 
              .
    END. /*FOR EACH vend-whse-trans-hist*/
END. /* IF prmAction = "Search" THEN DO: */

/*************************************************/ 


IF prmAction = "View" THEN DO:
    FIND FIRST vend-whse-trans-hist WHERE vend-whse-trans-hist.r-no EQ prmSeqNo   AND vend-whse-trans-hist.rec_key = prmReckey
                           AND  vend-whse-trans-hist.company = prmComp   NO-LOCK NO-ERROR.
          CREATE ttWarehouseHistory.
            assign
                ttWarehouseHistory.vRNo                 = vend-whse-trans-hist.r-no                
                ttWarehouseHistory.vTransType           = vend-whse-trans-hist.trans-type
                ttWarehouseHistory.vTransDate           = STRING(vend-whse-trans-hist.trans-date)
                ttWarehouseHistory.vTransQty            = vend-whse-trans-hist.trans-qty
                ttWarehouseHistory.vItemPoNo            = vend-whse-trans-hist.item-po-no
                ttWarehouseHistory.vItemLineNo          = vend-whse-trans-hist.item-line-no                
                ttWarehouseHistory.vCustPartNo          = vend-whse-trans-hist.cust-part-no
                ttWarehouseHistory.vFgItemNo            = vend-whse-trans-hist.fg-item-no
                ttWarehouseHistory.vVendorCode          = vend-whse-trans-hist.vendor-code
                ttWarehouseHistory.vVendorPlantCode     = vend-whse-trans-hist.vendor-plant-code
                ttWarehouseHistory.vVendorDeptCode      = vend-whse-trans-hist.vendor-dept-code
                ttWarehouseHistory.vVendOrdNo           = vend-whse-trans-hist.vend-ord-no
                ttWarehouseHistory.vVendJobNo           = vend-whse-trans-hist.vend-job-no
                ttWarehouseHistory.vVendJobNo2          = vend-whse-trans-hist.vend-job-no2
                ttWarehouseHistory.vSellPrice           = vend-whse-trans-hist.sell-price
                ttWarehouseHistory.vPlantTotOhQty       = vend-whse-trans-hist.plant-tot-oh-qty
                ttWarehouseHistory.vCreateDate          = vend-whse-trans-hist.create-date
                ttWarehouseHistory.vCreatedBy           = vend-whse-trans-hist.create-userid

              .    


END. /* IF prmAction = "View" THEN DO: */

/*************************************************/
