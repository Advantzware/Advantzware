

/*------------------------------------------------------------------------
    File        : RfqItem.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     : mon Feb 18 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cocode LIKE oe-ordl.company.

{RfqItem.i}
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER PrmRfqNo     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER RfqSeqNo     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER RfqQty       AS INT  NO-UNDO.
DEFINE INPUT PARAMETER RfqStock     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER RfqName      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER RfqPartno    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER Rfqstyle     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER RfqProcat    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER RfqCol       AS INT  NO-UNDO.
DEFINE INPUT PARAMETER RfqCoat      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER RfqLength    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER RfqWidth     AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER RfqDepth     AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER RfqBoard     AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER RfqCal       AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER RfqQuantity  AS INT  NO-UNDO.
DEFINE INPUT PARAMETER RfqRowid     AS RECID  NO-UNDO.
DEFINE INPUT PARAMETER lv_qty2      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER lv_qty3      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER lv_qty4      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER lv_qty5      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER lv_qty6      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER lv_qty7      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER lv_qty8      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER lv_qty9      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER lv_qty10     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER lv_price_1   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER lv_price_2   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER lv_price_3   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER lv_price_4   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER lv_price_5   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER lv_price_6   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER lv_price_7   AS DECIMAL  NO-UNDO.          
DEFINE INPUT PARAMETER lv_price_8   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER lv_price_9   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER lv_price_10  AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_1     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_2     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_3     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_4     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_5     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_6     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_7     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_8     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_9     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER lv_uom_10    AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER lv_date_1    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER lv_date_2    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER lv_date_3    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER lv_date_4    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER lv_date_5    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER lv_date_6    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER lv_date_7    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER lv_date_8    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER lv_date_9    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER lv_date_10   AS DATE NO-UNDO.

DEFINE INPUT PARAMETER lv_delivery_1    AS INT  NO-UNDO.  
DEFINE INPUT PARAMETER lv_delivery_2    AS INT  NO-UNDO.  
DEFINE INPUT PARAMETER lv_delivery_3    AS INT  NO-UNDO.  
DEFINE INPUT PARAMETER lv_delivery_4    AS INT  NO-UNDO.  
DEFINE INPUT PARAMETER lv_delivery_5    AS INT  NO-UNDO.  
DEFINE INPUT PARAMETER lv_delivery_6    AS INT  NO-UNDO.  
DEFINE INPUT PARAMETER lv_delivery_7    AS INT  NO-UNDO.  
DEFINE INPUT PARAMETER lv_delivery_8    AS INT  NO-UNDO.  
DEFINE INPUT PARAMETER lv_delivery_9    AS INT  NO-UNDO.  
DEFINE INPUT PARAMETER lv_delivery_10   AS INT  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRfqItem.
DEFINE BUFFER bf-rfqitem FOR rfqitem.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ll-is-corr-style   AS LOG  NO-UNDO.
def var K_frac as dec init 6.25 no-undo.
def var li-diff as int no-undo.
def var li-seq as int no-undo.

IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF PrmRfqNo  = ?  THEN ASSIGN prmRfqNo = 0.

IF prmAction = ""  THEN ASSIGN prmAction = "Select".
.
FIND FIRST usercomp  WHERE usercomp.company_default AND usercomp.user_id = prmUser NO-LOCK  NO-ERROR.
IF AVAILABLE usercomp  THEN DO:
     ASSIGN prmComp = usercomp.company.
END.

/*************************************/
MESSAGE "action1" prmAction prmComp PrmRfqNo.
IF prmAction = "delete" THEN DO:
  FIND FIRST bf-rfqitem WHERE RECID(bf-rfqitem) = RfqRowid .
  IF AVAIL bf-rfqitem  THEN DO:
      DELETE bf-rfqitem.
  END.  /*IF AVAIL b_rfqitem */
   li-diff = 1.  
   FIND rfq WHERE rfq.rfq-no = PrmRfqNo NO-LOCK.
   for each bf-rfqitem of rfq by bf-rfqitem.rfq-no by bf-rfqitem.seq:
       bf-rfqitem.seq = bf-rfqitem.seq - bf-rfqitem.seq + li-diff.
            li-diff = li-diff + 1.
   end.

   ASSIGN prmAction = "Select".
END. /*IF prmAction = "delete"*/
/*************************************************/

MESSAGE "action2" prmAction prmComp PrmRfqNo.
IF prmAction = "Select" THEN DO:
   FOR EACH rfqitem WHERE rfqitem.company = prmComp AND rfqitem.rfq-no = PrmRfqNo AND rfqitem.seq < 999  NO-LOCK:
        
     RUN createrecordRfq.
   END. 
END. /*IF prmAction = "Select" */
IF prmAction = "SelectRow" THEN DO:
  FIND first rfqitem WHERE recid(rfqitem) = RfqRowid NO-LOCK .
        
     RUN createrecordRfq.
  
END. /*IF prmAction = "SelectRow" */

IF prmAction = "AddRfqItem" THEN DO:
    FIND FIRST rfq WHERE rfq.rfq-no = PrmRfqNo NO-LOCK.
    CREATE rfqitem.
    find last bf-rfqitem where bf-rfqitem.rfq-no = rfq.rfq-no no-lock no-error.
    IF not avail bf-rfqitem then li-seq = 0.
    else li-seq = bf-rfqitem.seq.
         li-seq = li-seq + 1.
         assign rfqitem.rfq-no = rfq.rfq-no
             rfqitem.company = rfq.company
             rfqitem.loc = rfq.loc
             rfqitem.seq = li-seq
             .
         find first shipto where shipto.company  = rfq.company
                            and shipto.cust-no  = rfq.cust-no
                            no-lock no-error.
         assign rfqitem.ship-id = if avail shipto then shipto.ship-id else ""
             rfqitem.carrier = if avail shipto then shipto.carrier else "".

             FIND FIRST rfq-ctrl WHERE rfq-ctrl.company = rfq.company NO-LOCK NO-ERROR.
             assign rfqitem.qty[1] = 0  /* display problem when add new record */
                 rfqitem.i-col = IF AVAIL rfq-ctrl AND rfq-ctrl.def-ink <> "" THEN 1 ELSE 0
                 rfqitem.i-coat = IF AVAIL rfq-ctrl AND rfq-ctrl.def-coat <> "" THEN 1 ELSE 0
                 rfqitem.qty-date = today
                 rfqitem.qty-uom = "M"
                 rfqitem.qty[99] = 1 .
             RUN createrecordRfq.
END. /*IF prmAction = "AddRfqItem"*/

PROCEDURE createrecordRfq:
    CREATE ttRfqItem.
        ASSIGN 
            ttRfqItem.RfqSeqNo  = rfqitem.seq
            ttRfqItem.RfqQty    = rfqitem.qty[1] 
            ttRfqItem.RfqStock  = rfqitem.stock-no
            ttRfqItem.RfqName   = rfqitem.i-name
            ttRfqItem.RfqPartno = rfqitem.part-no
            ttRfqItem.Rfqstyle  = rfqitem.style
            ttRfqItem.RfqProcat = rfqitem.procat
            ttRfqItem.RfqCol    = rfqitem.i-col
            ttRfqItem.RfqCoat   = rfqitem.i-coat
            ttRfqItem.RfqBoard  = rfqitem.board
            ttRfqItem.RfqCal    = rfqitem.cal
            ttRfqItem.RfqQuantity = rfqitem.qty[99]
            ttRfqItem.RfqRowid    = RECID(rfqitem)
            ttRfqItem.lv_qty2     = rfqitem.qty[2]   
            ttRfqItem.lv_qty3     = rfqitem.qty[3]   
            ttRfqItem.lv_qty4     = rfqitem.qty[4]   
            ttRfqItem.lv_qty5     = rfqitem.qty[5]   
            ttRfqItem.lv_qty6     = rfqitem.qty[6]   
            ttRfqItem.lv_qty7     = rfqitem.qty[7]   
            ttRfqItem.lv_qty8     = rfqitem.qty[8]   
            ttRfqItem.lv_qty9     = rfqitem.qty[9]   
            ttRfqItem.lv_qty10    = rfqitem.qty[10] 
            ttRfqItem.lv_price_1  = rfqitem.qty-price[1]    
            ttRfqItem.lv_price_2  = rfqitem.qty-price[2]    
            ttRfqItem.lv_price_3  = rfqitem.qty-price[3]    
            ttRfqItem.lv_price_4  = rfqitem.qty-price[4]    
            ttRfqItem.lv_price_5  = rfqitem.qty-price[5]    
            ttRfqItem.lv_price_6  = rfqitem.qty-price[6]    
            ttRfqItem.lv_price_7  = rfqitem.qty-price[7]    
            ttRfqItem.lv_price_8  = rfqitem.qty-price[8]    
            ttRfqItem.lv_price_9  = rfqitem.qty-price[9]    
            ttRfqItem.lv_price_10 = rfqitem.qty-price[10]  
            ttRfqItem.lv_uom_1    = rfqitem.qty-uom[1]     
            ttRfqItem.lv_uom_2    = rfqitem.qty-uom[2]     
            ttRfqItem.lv_uom_3    = rfqitem.qty-uom[3]     
            ttRfqItem.lv_uom_4    = rfqitem.qty-uom[4]     
            ttRfqItem.lv_uom_5    = rfqitem.qty-uom[5]     
            ttRfqItem.lv_uom_6    = rfqitem.qty-uom[6]     
            ttRfqItem.lv_uom_7    = rfqitem.qty-uom[7]     
            ttRfqItem.lv_uom_8    = rfqitem.qty-uom[8]     
            ttRfqItem.lv_uom_9    = rfqitem.qty-uom[9]     
            ttRfqItem.lv_uom_10   = rfqitem.qty-uom[10]   
            ttRfqItem.lv_date_1   = rfqitem.qty-date[1]   
            ttRfqItem.lv_date_2   = rfqitem.qty-date[2]   
            ttRfqItem.lv_date_3   = rfqitem.qty-date[3]   
            ttRfqItem.lv_date_4   = rfqitem.qty-date[4]   
            ttRfqItem.lv_date_5   = rfqitem.qty-date[5]   
            ttRfqItem.lv_date_6   = rfqitem.qty-date[6]   
            ttRfqItem.lv_date_7   = rfqitem.qty-date[7]   
            ttRfqItem.lv_date_8   = rfqitem.qty-date[8]   
            ttRfqItem.lv_date_9   = rfqitem.qty-date[9]   
            ttRfqItem.lv_date_10   = rfqitem.qty-date[10] 
            ttRfqItem.lv_delivery_1 = rfqitem.delivery[1]    
            ttRfqItem.lv_delivery_2 = rfqitem.delivery[2]    
            ttRfqItem.lv_delivery_3 = rfqitem.delivery[3]    
            ttRfqItem.lv_delivery_4 = rfqitem.delivery[4]    
            ttRfqItem.lv_delivery_5 = rfqitem.delivery[5]    
            ttRfqItem.lv_delivery_6 = rfqitem.delivery[6]    
            ttRfqItem.lv_delivery_7 = rfqitem.delivery[7]    
            ttRfqItem.lv_delivery_8 = rfqitem.delivery[8]    
            ttRfqItem.lv_delivery_9 = rfqitem.delivery[9]    
            ttRfqItem.lv_delivery_10 = rfqitem.delivery[10]  


            .
        IF prmAction = "AddRfqItem" THEN DO:
                  ASSIGN ttRfqItem.aRowid  = RECID(rfqitem).
              END.
       
        find style where style.company = rfqitem.company AND style.style = rfqitem.style
                     no-lock no-error.
        if avail style and style.industry = "2" then   ll-is-corr-style = yes.
        else ll-is-corr-style = no.

        if ll-is-corr-style then 
            ASSIGN ttRfqItem.RfqLength = round(trunc(rfqitem.len,0) + ((rfqitem.len - trunc(rfqitem.len,0)) / K_FRAC),2).
        else ASSIGN ttRfqItem.RfqLength = rfqitem.len.
        if ll-is-corr-style then 
            ASSIGN ttRfqItem.RfqWidth = round(trunc(rfqitem.wid,0) + ((rfqitem.wid - trunc(rfqitem.wid,0)) / K_FRAC),2).
        else ASSIGN ttRfqItem.RfqWidth = rfqitem.wid.
        if ll-is-corr-style then 
            ASSIGN ttRfqItem.RfqDepth = round(trunc(rfqitem.dep,0) + ((rfqitem.dep - trunc(rfqitem.dep,0)) / K_FRAC),2).
        else ASSIGN ttRfqItem.RfqDepth = rfqitem.dep.
END PROCEDURE.  /*PROCEDURE createrecordRfq:*/

