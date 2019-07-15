{ListRfqItem.i}
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER PrmRfqNo     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER RfqSeqNo     AS INT  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsListRfqitem.

IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF PrmRfqNo  = ?  THEN ASSIGN prmRfqNo = 0.
IF RfqSeqNo  = ?  THEN ASSIGN RfqSeqNo = 0.
IF prmAction = ""  THEN ASSIGN prmAction = "Select".
def NEW SHARED {1} {2} var cocode     as   char  format "x(3)"  no-undo.

{cec/descalc.i "new"}

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ll-is-corr-style   AS LOG  NO-UNDO.
define variable cNewRecord as character no-undo.
DEFINE variable cMode      as character no-undo.
DEF var source-str as cha no-undo.    
DEF new shared temp-table formule field formule as dec extent 12.
def var lv-log as log no-undo.
def var char-val as cha no-undo.
def buffer bf-rfqitem for rfqitem.
    def buffer buff-rfqitem for rfqitem.
def new shared buffer xritem  for rfqitem.  /* for k-len k-wid calc in u2estic.p */
def var li-seq as int no-undo.
def var lv-recid as recid no-undo.
def var lv-copy-record as log no-undo.
def var lv-old-rfqitem-id as recid no-undo.
def var lv-rfqitem-copied-from-est as log no-undo.
def var lv-copy-qty as int extent 10 no-undo.
def var lv-part-no-prev as cha no-undo.
def var lv-stock-no-prev as cha no-undo.
def var lv-copy-pr as dec extent 10 no-undo.
def var lv-copy-uom as cha extent 10 no-undo.
def var lv-copy-date as date extent 10 no-undo.

def var ls-prev-val as cha no-undo.   
DEF VAR prmLoc AS CHAR NO-UNDO.
DEF  var k_frac as dec init 6.25 NO-UNDO.
def var li-diff as int no-undo.
{sys/inc/f16to32.i}
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
cocode = prmComp.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

/*************************************/
 IF prmAction = "Select" THEN DO:
     FIND FIRST rfqitem WHERE
       rfqitem.company = prmComp AND
       rfqitem.loc     = prmLoc AND
       rfqitem.rfq-no = PrmRfqNo NO-LOCK NO-ERROR.
     IF AVAIL rfqitem THEN DO:
         MESSAGE "Sewa" prmAction prmComp prmLoc PrmRfqNo.
         FOR EACH rfqitem WHERE
             rfqitem.company = prmComp AND
             rfqitem.loc     = prmLoc AND
             rfqitem.rfq-no = PrmRfqNo AND
             rfqitem.seq < 999  NO-LOCK:
             
             RUN createrecordRfq.
         END. 
     END.
END. /*IF prmAction = "Select" */
PROCEDURE createrecordRfq:
    
    CREATE ttListRfqitem.
        ASSIGN 
            ttListRfqitem.RfqSeqNo  = rfqitem.seq
            ttListRfqitem.RfqQty    = rfqitem.qty[1] 
            ttListRfqitem.RfqStock  = rfqitem.stock-no
            ttListRfqitem.RfqName   = rfqitem.i-name
            ttListRfqitem.RfqPartno = rfqitem.part-no
            ttListRfqitem.Rfqstyle  = rfqitem.style
            ttListRfqitem.RfqProcat = rfqitem.procat
            ttListRfqitem.RfqCol    = rfqitem.i-col
            ttListRfqitem.RfqCoat   = rfqitem.i-coat
            ttListRfqitem.RfqBoard  = rfqitem.board
            ttListRfqitem.RfqCal    = rfqitem.cal
            ttListRfqitem.RfqQuantity = rfqitem.qty[99]
            ttListRfqitem.RfqRowid    = RECID(rfqitem)
            ttListRfqitem.lv_qty_2     = rfqitem.qty[2]   
            ttListRfqitem.lv_qty_3     = rfqitem.qty[3]   
            ttListRfqitem.lv_qty_4     = rfqitem.qty[4]   
            ttListRfqitem.lv_qty_5     = rfqitem.qty[5]   
            ttListRfqitem.lv_qty_6     = rfqitem.qty[6]   
            ttListRfqitem.lv_qty_7     = rfqitem.qty[7]   
            ttListRfqitem.lv_qty_8     = rfqitem.qty[8]   
            ttListRfqitem.lv_qty_9     = rfqitem.qty[9]   
            ttListRfqitem.lv_qty_10    = rfqitem.qty[10] 
            ttListRfqitem.lv_price_1  = rfqitem.qty-price[1]    
            ttListRfqitem.lv_price_2  = rfqitem.qty-price[2]    
            ttListRfqitem.lv_price_3  = rfqitem.qty-price[3]    
            ttListRfqitem.lv_price_4  = rfqitem.qty-price[4]    
            ttListRfqitem.lv_price_5  = rfqitem.qty-price[5]    
            ttListRfqitem.lv_price_6  = rfqitem.qty-price[6]    
            ttListRfqitem.lv_price_7  = rfqitem.qty-price[7]    
            ttListRfqitem.lv_price_8  = rfqitem.qty-price[8]    
            ttListRfqitem.lv_price_9  = rfqitem.qty-price[9]    
            ttListRfqitem.lv_price_10 = rfqitem.qty-price[10]  
            ttListRfqitem.lv_uom_1    = rfqitem.qty-uom[1]     
            ttListRfqitem.lv_uom_2    = rfqitem.qty-uom[2]     
            ttListRfqitem.lv_uom_3    = rfqitem.qty-uom[3]     
            ttListRfqitem.lv_uom_4    = rfqitem.qty-uom[4]     
            ttListRfqitem.lv_uom_5    = rfqitem.qty-uom[5]     
            ttListRfqitem.lv_uom_6    = rfqitem.qty-uom[6]     
            ttListRfqitem.lv_uom_7    = rfqitem.qty-uom[7]     
            ttListRfqitem.lv_uom_8    = rfqitem.qty-uom[8]     
            ttListRfqitem.lv_uom_9    = rfqitem.qty-uom[9]     
            ttListRfqitem.lv_uom_10   = rfqitem.qty-uom[10]   
            ttListRfqitem.lv_date_1   = rfqitem.qty-date[1]   
            ttListRfqitem.lv_date_2   = rfqitem.qty-date[2]   
            ttListRfqitem.lv_date_3   = rfqitem.qty-date[3]   
            ttListRfqitem.lv_date_4   = rfqitem.qty-date[4]   
            ttListRfqitem.lv_date_5   = rfqitem.qty-date[5]   
            ttListRfqitem.lv_date_6   = rfqitem.qty-date[6]   
            ttListRfqitem.lv_date_7   = rfqitem.qty-date[7]   
            ttListRfqitem.lv_date_8   = rfqitem.qty-date[8]   
            ttListRfqitem.lv_date_9   = rfqitem.qty-date[9]   
            ttListRfqitem.lv_date_10   = rfqitem.qty-date[10] 
            ttListRfqitem.lv_delivery_1 = rfqitem.delivery[1]    
            ttListRfqitem.lv_delivery_2 = rfqitem.delivery[2]    
            ttListRfqitem.lv_delivery_3 = rfqitem.delivery[3]    
            ttListRfqitem.lv_delivery_4 = rfqitem.delivery[4]    
            ttListRfqitem.lv_delivery_5 = rfqitem.delivery[5]    
            ttListRfqitem.lv_delivery_6 = rfqitem.delivery[6]    
            ttListRfqitem.lv_delivery_7 = rfqitem.delivery[7]    
            ttListRfqitem.lv_delivery_8 = rfqitem.delivery[8]    
            ttListRfqitem.lv_delivery_9 = rfqitem.delivery[9]    
            ttListRfqitem.lv_delivery_10 = rfqitem.delivery[10]  
            .
        
       
        find style where style.company = rfqitem.company AND style.style = rfqitem.style
                     no-lock no-error.
        if avail style and style.industry = "2" then   ll-is-corr-style = yes
            .
        else ll-is-corr-style = no.

        if ll-is-corr-style then 
            ASSIGN ttListRfqitem.RfqLength = round(trunc(rfqitem.len,0) + ((rfqitem.len - trunc(rfqitem.len,0)) / K_FRAC),2) 
            ttListRfqitem.RfqType  = "2".
        else ASSIGN ttListRfqitem.RfqLength = rfqitem.len
             ttListRfqitem.RfqType  = "1".
        if ll-is-corr-style then 
            ASSIGN ttListRfqitem.RfqWidth = round(trunc(rfqitem.wid,0) + ((rfqitem.wid - trunc(rfqitem.wid,0)) / K_FRAC),2).
        else ASSIGN ttListRfqitem.RfqWidth = rfqitem.wid.
        if ll-is-corr-style then 
            ASSIGN ttListRfqitem.RfqDepth = round(trunc(rfqitem.dep,0) + ((rfqitem.dep - trunc(rfqitem.dep,0)) / K_FRAC),2).
        else ASSIGN ttListRfqitem.RfqDepth = rfqitem.dep.
END PROCEDURE.  /*PROCEDURE createrecordRfq:*/

