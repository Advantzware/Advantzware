

/*------------------------------------------------------------------------
    File        : RfqItem.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
def NEW SHARED {1} {2} var cocode     as   char  format "x(3)"  no-undo.
{cec/descalc.i "new"}
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
DEFINE INPUT PARAMETER RfqBoard     AS CHAR  NO-UNDO.
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
DEFINE INPUT PARAMETER RfqEstNo         AS CHAR  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRfqItem.
    DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER vMailto  AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER vBody  AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER vFrom  AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER vSubject  AS CHAR NO-UNDO.

    DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
    DEF VAR ls-to-list AS cha NO-UNDO.
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
DEF VAR vTr-no AS CHAR NO-UNDO.
DEF VAR vCas-no AS CHAR NO-UNDO.
def var ls-prev-val as cha no-undo.   
DEF VAR prmLoc AS CHAR NO-UNDO.
DEF  var k_frac as dec init 6.25 NO-UNDO.
def var li-diff as int no-undo.
DEFINE VAR vCaliper AS DECIMAL  NO-UNDO.
{sys/inc/f16to32.i}

IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF PrmRfqNo  = ?  THEN ASSIGN prmRfqNo = 0.
IF RfqSeqNo  = ?  THEN ASSIGN RfqSeqNo = 0.
IF RfqQty  = ?  THEN ASSIGN RfqQty = 0.
IF RfqStock  = ?  THEN ASSIGN RfqStock = "".
IF RfqName  = ?  THEN ASSIGN RfqName = "".
IF RfqPartno  = ?  THEN ASSIGN RfqPartno = "".
IF Rfqstyle  = ?  THEN ASSIGN Rfqstyle = "".
IF RfqProcat  = ?  THEN ASSIGN RfqProcat = "".
IF RfqCol  = ?  THEN ASSIGN RfqCol = 0.
IF RfqCoat  = ?  THEN ASSIGN RfqCoat = 0.
IF RfqLength  = ?  THEN ASSIGN RfqLength = 0.
IF RfqWidth  = ?  THEN ASSIGN RfqWidth = 0.
IF RfqDepth  = ?  THEN ASSIGN RfqDepth = 0.
IF RfqBoard  = ?  THEN ASSIGN RfqBoard = "".
IF RfqCal  = ?  THEN ASSIGN RfqCal = 0.
IF RfqQuantity  = ?  THEN ASSIGN RfqQuantity = 0.
IF lv_date_1  = ? OR lv_date_1 = 01/13/001  THEN ASSIGN lv_date_1 = ?.
IF lv_date_2  = ? OR lv_date_2 = 01/13/001  THEN ASSIGN lv_date_2 = ?.
IF lv_date_3  = ? OR lv_date_3 = 01/13/001  THEN ASSIGN lv_date_3 = ?.
IF lv_date_4  = ? OR lv_date_4 = 01/13/001  THEN ASSIGN lv_date_4 = ?.
IF lv_date_5  = ? OR lv_date_5 = 01/13/001  THEN ASSIGN lv_date_5 = ?.
IF lv_date_6  = ? OR lv_date_6 = 01/13/001  THEN ASSIGN lv_date_6 = ?.
IF lv_date_7  = ? OR lv_date_7 = 01/13/001  THEN ASSIGN lv_date_7 = ?.
IF lv_date_8  = ? OR lv_date_8 = 01/13/001  THEN ASSIGN lv_date_8 = ?.
IF lv_date_9  = ? OR lv_date_9 = 01/13/001  THEN ASSIGN lv_date_9 = ?.
IF lv_date_10 = ? OR lv_date_10 = 01/13/001  THEN ASSIGN lv_date_10 = ?.


IF prmAction = ""  THEN ASSIGN prmAction = "Select".

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
FOR EACH ttRfqItem:
    DELETE ttRfqItem.
END.

IF prmAction = "delete" THEN DO:
     FIND FIRST bf-rfqitem where bf-rfqitem.rfq-no = prmRfqNo AND bf-rfqitem.seq = RfqSeqNo EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL bf-rfqitem  THEN DO:
      DELETE bf-rfqitem.        
  END.  /*IF AVAIL b_rfqitem */
   li-diff = 1.  
   FIND rfq WHERE
        rfq.company EQ prmComp AND
        rfq.loc EQ prmLoc AND
        rfq.rfq-no = PrmRfqNo NO-LOCK.

   for each bf-rfqitem of rfq by bf-rfqitem.rfq-no by bf-rfqitem.seq:
       bf-rfqitem.seq = bf-rfqitem.seq - bf-rfqitem.seq + li-diff.
       li-diff = li-diff + 1.
   end.

   FIND LAST rfqitem where rfqitem.rfq-no = prmRfqNo NO-LOCK NO-ERROR.
   IF AVAIL rfqitem THEN
       ASSIGN
        RfqSeqNo = rfqitem.seq.
   

   ASSIGN prmAction = "Select".
END. /*IF prmAction = "delete"*/
/*************************************************/


/* Validation for Rfq Item*/
IF prmAction = "UpdateRfqItem" THEN DO:
     
    FIND FIRST style WHERE style.company = prmComp AND style.style = RfqStyle  NO-LOCK NO-ERROR.
    IF NOT AVAIL style THEN DO:
        ASSIGN 
            cError = "Invalid Style. " .
        RETURN. 
    END.
    FIND FIRST fgcat WHERE fgcat.company = prmComp
        AND fgcat.procat = RfqProcat NO-LOCK NO-ERROR.
    IF NOT AVAIL fgcat THEN DO:
        ASSIGN 
            cError =  "Invalid Product Category. " .
        RETURN. 
        
    END.
    IF RfqStock <> "" THEN DO:
    find first itemfg where itemfg.company = prmComp and
        itemfg.i-no = RfqStock no-lock no-error.        
    if not avail itemfg then do:
        ASSIGN 
            cError = "Invalid FG Item#. Try Help.".
            RETURN. 
    end. /*if not avail itemfg then do:*/ 
    END.
   /*IF RfqPartno <> "" THEN DO:
    find first itemfg where itemfg.company = prmComp and
        itemfg.part-no = RfqPartno no-lock no-error.        
    if not avail itemfg then do:
        ASSIGN 
            cError = "Invalid Part No#. Try Help.".
            RETURN. 
    end. /*if not avail itemfg then do:*/ 
    END.
     */
    FIND FIRST item WHERE item.company = prmComp and item.i-no = RfqBoard  AND
        (item.mat-type = "B" or item.mat-type = "P" or item.mat-type = "1" 
         OR item.mat-type = "2" or item.mat-type = "3" or item.mat-type = "4") NO-LOCK NO-ERROR.
    if not avail item then do:
        
        ASSIGN 
            cError = "Invalid Board".
            RETURN. 
    end. /*if not avail item then do:*/ 
    ELSE DO:
        ASSIGN vCaliper = item.cal.
    END.
     

    FIND FIRST fgcat WHERE fgcat.company = prmComp
        AND fgcat.procat = RfqProcat NO-LOCK NO-ERROR .
    if not avail item then do:
        ASSIGN 
            cError = "Invalid Category".
            RETURN. 
    end. /*if not avail fgcat then do:*/ 

    IF RfqStock = "" THEN
    find first itemfg where itemfg.company = prmComp and
        itemfg.part-no = RfqPartno no-lock no-error.        
    if  avail itemfg then do:
        ASSIGN 
            RfqStock = itemfg.i-no.
    end. /*if  avail itemfg then do:*/ 

END. /*IF prmAction = "AddRfqItem" THEN*/




IF prmAction = "AddRfqItem" THEN DO:
     

  FIND FIRST style WHERE style.company = prmComp AND style.style = RfqStyle  NO-LOCK NO-ERROR.
    IF NOT AVAIL style THEN DO:
        ASSIGN 
            cError = "Invalid Style. " .
        RETURN. 
    END.
    FIND FIRST fgcat WHERE fgcat.company = prmComp
        AND fgcat.procat = RfqProcat NO-LOCK NO-ERROR.
    IF NOT AVAIL fgcat THEN DO:
        ASSIGN 
            cError =  "Invalid Product Category. " .
        RETURN. 
        
    END.
    IF RfqStock <> "" THEN DO:
    find first itemfg where itemfg.company = prmComp and
        itemfg.i-no = RfqStock no-lock no-error.        
    if not avail itemfg then do:
        ASSIGN 
            cError = "Invalid FG Item#. Try Help.".
            RETURN. 
    end. /*if not avail itemfg then do:*/ 
    END.
   /* IF RfqPartno <> "" THEN DO:
    find first itemfg where itemfg.company = prmComp and
        itemfg.part-no = RfqPartno no-lock no-error.        
    if not avail itemfg then do:
        ASSIGN 
            cError = "Invalid Part No#. Try Help.".
            RETURN. 
    end. /*if not avail itemfg then do:*/ 
    END.*/
     
    FIND FIRST item WHERE item.company = prmComp and item.i-no = RfqBoard  AND
        (item.mat-type = "B" or item.mat-type = "P" or item.mat-type = "1" 
         OR item.mat-type = "2" or item.mat-type = "3" or item.mat-type = "4") NO-LOCK NO-ERROR.
    if not avail item then do:
        
        ASSIGN 
            cError = "Invalid Board".
            RETURN. 
    end. /*if not avail item then do:*/ 
    ELSE DO: 
        ASSIGN vCaliper = item.cal.
    END.
     

    FIND FIRST fgcat WHERE fgcat.company = prmComp
        AND fgcat.procat = RfqProcat NO-LOCK NO-ERROR .
    if not avail item then do:
        ASSIGN 
            cError = "Invalid Category".
            RETURN. 
    end. /*if not avail fgcat then do:*/ 
    IF RfqStock = "" THEN
    find first itemfg where itemfg.company = prmComp and
        itemfg.part-no = RfqPartno no-lock no-error.        
    if  avail itemfg then do:
        ASSIGN 
            RfqStock = itemfg.i-no.
    end. /*if  avail itemfg then do:*/ 

END. /*IF prmAction = "AddRfqItem" THEN*/



/* Add Logic for Rfq Item*/

IF prmAction = "AddRfqItem" THEN DO:
   
    FIND FIRST rfq WHERE
        rfq.company EQ prmComp AND
        rfq.loc EQ prmLoc AND
        rfq.rfq-no = PrmRfqNo NO-LOCK.
    FIND last rfqitem where
        rfqitem.rfq-no = PrmRfqNo
        no-lock no-error.
    IF avail rfqitem then li-seq = rfqitem.seq + 1.
    ELSE
        li-seq = 1.
        CREATE bf-rfqitem .
        assign 
            bf-rfqitem.rfq-no        = PrmRfqNo
            bf-rfqitem.company       = prmComp
            bf-rfqitem.loc           = prmLoc
            bf-rfqitem.seq           = li-seq
            bf-rfqitem.stock-no      = RfqStock
            bf-rfqitem.i-name        = RfqName
            bf-rfqitem.part-no       = RfqPartno
            bf-rfqitem.style         = RfqStyle 
            bf-rfqitem.procat        = RfqProcat
            bf-rfqitem.cal           = vCaliper
            bf-rfqitem.i-col         = RfqCol                   
            bf-rfqitem.i-coat        = RfqCoat                        
            bf-rfqitem.len           = RfqLength
            bf-rfqitem.wid           = RfqWidth
            bf-rfqitem.dep           = RfqDepth
            bf-rfqitem.board         = RfqBoard
            bf-rfqitem.qty[1]        = RfqQty
            bf-rfqitem.qty[2]        = lv_qty2      
            bf-rfqitem.qty[3]        = lv_qty3 
            bf-rfqitem.qty[4]        = lv_qty4      
            bf-rfqitem.qty[5]        = lv_qty5      
            bf-rfqitem.qty[6]        = lv_qty6      
            bf-rfqitem.qty[7]        = lv_qty7      
            bf-rfqitem.qty[8]        = lv_qty8      
            bf-rfqitem.qty[9]        = lv_qty9      
            bf-rfqitem.qty[10]       = lv_qty10     
            bf-rfqitem.delivery[1]   =  lv_delivery_1
            bf-rfqitem.delivery[2]   =  lv_delivery_2
            bf-rfqitem.delivery[3]   =  lv_delivery_3
            bf-rfqitem.delivery[4]   =  lv_delivery_4
            bf-rfqitem.delivery[5]   =  lv_delivery_5
            bf-rfqitem.delivery[6]   =  lv_delivery_6
            bf-rfqitem.delivery[7]   =  lv_delivery_7
            bf-rfqitem.delivery[8]   =  lv_delivery_8
            bf-rfqitem.delivery[9]   =  lv_delivery_9
            bf-rfqitem.delivery[10]  =  lv_delivery_10
            bf-rfqitem.qty-price[1]  =  lv_price_1
            bf-rfqitem.qty-price[2]  =  lv_price_2
            bf-rfqitem.qty-price[3]  =  lv_price_3
            bf-rfqitem.qty-price[4]  =  lv_price_4
            bf-rfqitem.qty-price[5]  =  lv_price_5
            bf-rfqitem.qty-price[6]  =  lv_price_6
            bf-rfqitem.qty-price[7]  =  lv_price_7
            bf-rfqitem.qty-price[8]  =  lv_price_8
            bf-rfqitem.qty-price[9]  =  lv_price_9
            bf-rfqitem.qty-price[10] =  lv_price_10
            bf-rfqitem.qty-uom[1]    =  lv_uom_1
            bf-rfqitem.qty-uom[2]    =  lv_uom_2
            bf-rfqitem.qty-uom[3]    =  lv_uom_3
            bf-rfqitem.qty-uom[4]    =  lv_uom_4
            bf-rfqitem.qty-uom[5]    =  lv_uom_5
            bf-rfqitem.qty-uom[6]    =  lv_uom_6
            bf-rfqitem.qty-uom[7]    =  lv_uom_7
            bf-rfqitem.qty-uom[8]    =  lv_uom_8
            bf-rfqitem.qty-uom[9]    =  lv_uom_9
            bf-rfqitem.qty-uom[10]   =  lv_uom_10
            bf-rfqitem.qty-date[1]   =  lv_date_1
            bf-rfqitem.qty-date[2]   =  lv_date_2
            bf-rfqitem.qty-date[3]   =  lv_date_3
            bf-rfqitem.qty-date[4]   =  lv_date_4
            bf-rfqitem.qty-date[5]   =  lv_date_5
            bf-rfqitem.qty-date[6]   =  lv_date_6
            bf-rfqitem.qty-date[7]   =  lv_date_7
            bf-rfqitem.qty-date[8]   =  lv_date_8
            bf-rfqitem.qty-date[9]   =  lv_date_9
            bf-rfqitem.qty-date[10]  =  lv_date_10
            .

        ASSIGN
            bf-rfqitem.qty[99] = RfqQuantity
            /*bf-rfqitem.qty[99] = 1*/.
        

        find first shipto where shipto.company  = prmComp 
            and shipto.cust-no  = rfq.cust-no no-lock no-error.
        assign bf-rfqitem.ship-id = if avail shipto then shipto.ship-id else ""
            bf-rfqitem.carrier = if avail shipto then shipto.carrier else "".
                                                                       
            find cust where cust.company = prmComp
                and cust.cust-no = rfq.cust-no no-lock no-error.
                 find first rfq-ctrl no-lock no-error.
                 bf-rfqitem.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else rfq-ctrl.def-case.
                 bf-rfqitem.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else rfq-ctrl.def-pal.      
                 vCas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else rfq-ctrl.def-case.
                 vTr-no = if avail cust and cust.pallet <> "" then cust.pallet else rfq-ctrl.def-pal. 

       /*      end.  /*IF RfqStock = "" THEN DO*/  */
            find item where item.company = prmComp and
                item.i-no = vCas-no no-lock no-error.
            if avail item then
                assign
                bf-rfqitem.cas-cnt = item.box-case
                bf-rfqitem.cas-len = item.case-l
                bf-rfqitem.cas-wid = item.case-w
                bf-rfqitem.cas-dep = item.case-d
                bf-rfqitem.cas-pal = item.case-pall
                bf-rfqitem.cas-wt  = item.avg-w         
                .
             find item where item.company = prmComp and
                item.i-no = vTr-no no-lock no-error.          
            if avail item then 
                assign 
                bf-rfqitem.tr-len = item.case-l
                bf-rfqitem.tr-wid = item.case-w
                bf-rfqitem.tr-dep = item.case-d.
            find style where style.company = rfq.company and
                style.style = RfqStyle no-lock no-error.
            if avail style then 
                assign
                bf-rfqitem.adhesive = style.material[7]
                bf-rfqitem.gluelap = style.dim-gl
                bf-rfqitem.k-wid = style.dim-dkw
                bf-rfqitem.fpanel = style.dim-pan5
                bf-rfqitem.lock = style.dim-fit
                bf-rfqitem.tuck = style.dim-tk.
            IF style.dim-gl <> 0 then bf-rfqitem.lin-in = RfqDepth.
             if RfqCol = 0 THEN bf-rfqitem.i-pass = 0.
             FIND FIRST rfqitem WHERE rfqitem.rfq-no = PrmRfqNo 
                                  AND rfqitem.seq = li-seq EXCLUSIVE-LOCK no-error.

               vBody = "Rfq Number " +  STRING(PrmRfqNo)    
                      + " has been Quote." + CHR(20).
           
                vBody = vBody  + "<br>" + "<br>"
                   + "Item: " + STRING(rfqitem.stock-no) + "&nbsp;&nbsp;"
                   + " Qty: " + STRING(rfqitem.qty[1]) + "<br>"  
                   + " Date: " + STRING(rfq.req-date) + "&nbsp;&nbsp;" 
                   + " Cust Part#: "  + STRING(rfqitem.part-no) + CHR(100). 

                ASSIGN v-prgmname = "R-QuoPrt.".

                

                 FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = rfq.cust-no NO-LOCK NO-ERROR.                 
                 
                 {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}
                     
                 FIND FIRST users WHERE  users.USER_id = prmUser  NO-LOCK NO-ERROR.
                 IF AVAIL users THEN DO:
                         ASSIGN vMailto =  vMailto + "," +  users.email .
                 END.
                 

               
           run assign-rfqitem.
           run calc-pass.
          run calc-blank-size.
           /*RUN copy-from-est. */

       
  /* END.   /*if avail bf-rfqitem*/                                                                               */
   ASSIGN  RfqSeqNo = li-seq
       prmAction = "Select".                                                                                
END. /*IF prmAction = "AddRfqItem"*/



IF prmAction = "UpdateRfqItem" THEN DO:
  FIND FIRST rfq WHERE
        rfq.company EQ prmComp AND
        rfq.loc EQ prmLoc AND
        rfq.rfq-no = PrmRfqNo EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST bf-rfqitem where bf-rfqitem.rfq-no = prmRfqNo AND bf-rfqitem.seq = RfqSeqNo EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf-rfqitem THEN DO:
        assign 
            /*bf-rfqitem.rfq-no        = PrmRfqNo*/
            bf-rfqitem.stock-no      = RfqStock
            bf-rfqitem.i-name        = RfqName
            bf-rfqitem.part-no       = RfqPartno
            bf-rfqitem.style         = RfqStyle 
            bf-rfqitem.procat        = RfqProcat
            bf-rfqitem.cal           = vCaliper
            bf-rfqitem.i-col         = RfqCol                   
            bf-rfqitem.i-coat        = RfqCoat                        
            bf-rfqitem.len           = RfqLength
            bf-rfqitem.wid           = RfqWidth
            bf-rfqitem.dep           = RfqDepth
            bf-rfqitem.board         = RfqBoard
            bf-rfqitem.qty[1]        = RfqQty
            bf-rfqitem.qty[2]        = lv_qty2      
            bf-rfqitem.qty[3]        = lv_qty3 
            bf-rfqitem.qty[4]        = lv_qty4      
            bf-rfqitem.qty[5]        = lv_qty5      
            bf-rfqitem.qty[6]        = lv_qty6      
            bf-rfqitem.qty[7]        = lv_qty7      
            bf-rfqitem.qty[8]        = lv_qty8      
            bf-rfqitem.qty[9]        = lv_qty9      
            bf-rfqitem.qty[10]       = lv_qty10     
            bf-rfqitem.delivery[1] =  lv_delivery_1
            bf-rfqitem.delivery[2] =  lv_delivery_2
            bf-rfqitem.delivery[3] =  lv_delivery_3
            bf-rfqitem.delivery[4] =  lv_delivery_4
            bf-rfqitem.delivery[5] =  lv_delivery_5
            bf-rfqitem.delivery[6] =  lv_delivery_6
            bf-rfqitem.delivery[7] =  lv_delivery_7
            bf-rfqitem.delivery[8] =  lv_delivery_8
            bf-rfqitem.delivery[9] =  lv_delivery_9
            bf-rfqitem.delivery[10] =  lv_delivery_10

            bf-rfqitem.qty-price[1]  =  lv_price_1
            bf-rfqitem.qty-price[2]  =  lv_price_2
            bf-rfqitem.qty-price[3]  =  lv_price_3
            bf-rfqitem.qty-price[4]  =  lv_price_4
            bf-rfqitem.qty-price[5]  =  lv_price_5
            bf-rfqitem.qty-price[6]  =  lv_price_6
            bf-rfqitem.qty-price[7]  =  lv_price_7
            bf-rfqitem.qty-price[8]  =  lv_price_8
            bf-rfqitem.qty-price[9]  =  lv_price_9
            bf-rfqitem.qty-price[10] =  lv_price_10
            bf-rfqitem.qty-uom[1]    =  lv_uom_1
            bf-rfqitem.qty-uom[2]    =  lv_uom_2
            bf-rfqitem.qty-uom[3]    =  lv_uom_3
            bf-rfqitem.qty-uom[4]    =  lv_uom_4
            bf-rfqitem.qty-uom[5]    =  lv_uom_5
            bf-rfqitem.qty-uom[6]    =  lv_uom_6
            bf-rfqitem.qty-uom[7]    =  lv_uom_7
            bf-rfqitem.qty-uom[8]    =  lv_uom_8
            bf-rfqitem.qty-uom[9]    =  lv_uom_9
            bf-rfqitem.qty-uom[10]   =  lv_uom_10
            bf-rfqitem.qty-date[1]   =  lv_date_1
            bf-rfqitem.qty-date[2]   =  lv_date_2
            bf-rfqitem.qty-date[3]   =  lv_date_3
            bf-rfqitem.qty-date[4]   =  lv_date_4
            bf-rfqitem.qty-date[5]   =  lv_date_5
            bf-rfqitem.qty-date[6]   =  lv_date_6
            bf-rfqitem.qty-date[7]   =  lv_date_7
            bf-rfqitem.qty-date[8]   =  lv_date_8
            bf-rfqitem.qty-date[9]   =  lv_date_9
            bf-rfqitem.qty-date[10]  =  lv_date_10
            .
        ASSIGN
            bf-rfqitem.qty[99] = RfqQuantity.

          find first shipto where shipto.company  = prmComp 
            and shipto.cust-no  = rfq.cust-no no-lock no-error.
        assign bf-rfqitem.ship-id = if avail shipto then shipto.ship-id else ""
            bf-rfqitem.carrier = if avail shipto then shipto.carrier else "".
                                                                       
             
           /*  FIND FIRST rfq-ctrl WHERE rfq-ctrl.company = rfq.company NO-LOCK NO-ERROR.
             assign bf-rfqitem.i-col = IF AVAIL rfq-ctrl AND rfq-ctrl.def-ink <> "" THEN 1 ELSE 0
                     bf-rfqitem.i-coat = IF AVAIL rfq-ctrl AND rfq-ctrl.def-coat <> "" THEN 1 ELSE 0. */
           /*  IF RfqStock = "" THEN DO:  */
                 find cust where cust.company = prmComp
                     and cust.cust-no = rfq.cust-no no-lock no-error.
                 find first rfq-ctrl no-lock no-error.
                 bf-rfqitem.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else rfq-ctrl.def-case.
                 bf-rfqitem.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else rfq-ctrl.def-pal.      
                 vCas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else rfq-ctrl.def-case.
                 vTr-no = if avail cust and cust.pallet <> "" then cust.pallet else rfq-ctrl.def-pal. 

       /*      end.  /*IF RfqStock = "" THEN DO*/  */
            find item where item.company = prmComp and
                item.i-no = vCas-no no-lock no-error.
            if avail item then
                assign
                bf-rfqitem.cas-cnt = item.box-case
                bf-rfqitem.cas-len = item.case-l
                bf-rfqitem.cas-wid = item.case-w
                bf-rfqitem.cas-dep = item.case-d
                bf-rfqitem.cas-pal = item.case-pall
                bf-rfqitem.cas-wt  = item.avg-w         
                .
             
            find item where item.company = prmComp and
                item.i-no = vTr-no no-lock no-error.          
            if avail item then 
                assign 
                bf-rfqitem.tr-len = item.case-l
                bf-rfqitem.tr-wid = item.case-w
                bf-rfqitem.tr-dep = item.case-d.
            find style where style.company = rfq.company and
                style.style = RfqStyle no-lock no-error.
            
            if avail style then 
                assign
                bf-rfqitem.adhesive = style.material[7]
                bf-rfqitem.gluelap = style.dim-gl
                bf-rfqitem.k-wid = style.dim-dkw
                bf-rfqitem.fpanel = style.dim-pan5
                bf-rfqitem.lock = style.dim-fit
                bf-rfqitem.tuck = style.dim-tk.
            
            IF style.dim-gl <> 0 then bf-rfqitem.lin-in = RfqDepth.
             if RfqCol = 0 THEN bf-rfqitem.i-pass = 0.
             
             FIND FIRST rfqitem WHERE rfqitem.rfq-no = PrmRfqNo 
                                  AND rfqitem.seq = RfqSeqNo EXCLUSIVE-LOCK no-error.
             
          run assign-rfqitem.
            
       run calc-pass.
           /*run calc-blank-size.*/
           /*RUN copy-from-est. */
       
   END.   /*if avail bf-rfqitem*/                                                                               
   ASSIGN  prmAction = "Select".                                                                                
END. /*IF prmAction = "AddRfqItem"*/
                                                                             

IF prmAction = "Select" THEN DO:
    FIND FIRST rfqitem where rfqitem.company = prmComp AND 
                       rfqitem.loc     = prmLoc AND 
                       rfqitem.rfq-no = prmRfqNo AND 
                       rfqitem.seq =  RfqSeqNo   no-error.
 
     RUN createrecordRfq.
   
END. /*IF prmAction = "Select" */
PROCEDURE createrecordRfq:
    
    CREATE ttRfqItem.
        ASSIGN 
            ttRfqItem.RfqSeqNo  = rfqitem.seq
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
            ttRfqItem.RfqQty       = rfqitem.qty[1] 
            ttRfqItem.lv_qty_2     = rfqitem.qty[2]   
            ttRfqItem.lv_qty_3     = rfqitem.qty[3]   
            ttRfqItem.lv_qty_4     = rfqitem.qty[4]   
            ttRfqItem.lv_qty_5     = rfqitem.qty[5]   
            ttRfqItem.lv_qty_6     = rfqitem.qty[6]   
            ttRfqItem.lv_qty_7     = rfqitem.qty[7]   
            ttRfqItem.lv_qty_8     = rfqitem.qty[8]   
            ttRfqItem.lv_qty_9     = rfqitem.qty[9]   
            ttRfqItem.lv_qty_10    = rfqitem.qty[10] 
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
            ttRfqItem.qtyset = rfqitem.qty[99] 
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

/*****************************************PROCEDURE assign-rfqitem******************************/
PROCEDURE assign-rfqitem:
    DEF var v-dim-fit as log no-undo.
    DEF VAR i AS INT NO-UNDO.
    if lv-rfqitem-copied-from-est then do:
        lv-rfqitem-copied-from-est = no.
    end.
    FIND FIRST rfqitem WHERE rfqitem.rfq-no = PrmRfqNo 
        AND rfqitem.seq = li-seq NO-LOCK no-error.
    find style where style.company = prmComp and
        style.style = rfqitem.style no-lock no-error.   
    if avail style and style.industry = "2" then  do:
        find item where item.company = prmComp and
            item.i-no = RfqBoard no-lock no-error.                  
        if avail item then assign rfqitem.test = item.reg-no
            rfqitem.flute = item.flute
            rfqitem.brd-dscr = item.i-name.
    end.   
    /*    IF adm-new-record then do:*/
    if not avail style then find style where style.company = prmComp and
            style.style = Rfqstyle no-lock no-error.   
        if avail style and style.industry = "2" then  do:
            find item where item.company = prmComp and
                            item.i-no = RfqBoard no-lock no-error.
            if avail item then assign rfqitem.test = item.reg-no
                                     rfqitem.flute = item.flute
                                     rfqitem.k-len = style.dim-dkl
                                     rfqitem.k-wid = style.dim-dkw
                                     .
            find xritem where recid(xritem) = recid(rfqitem).
            {rfq/u2estc.i rfqitem.gluelap 1}
            {rfq/u2estc.i rfqitem.k-wid 2}
                find first item where item.company = rfqitem.company and
                                      index("GTS",item.mat-type) > 0 and
                                      item.i-no eq rfqitem.adhesive no-lock no-error.
            if avail item and rfqitem.adhesive ne "NO JOINT" then do:
                if item.mat-type eq "G" then do:
                    if rfqitem.tab-in then do:
                        {rfq/u2estc.i rfqitem.k-len 3}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 4}
                    end.
                 end.
                 else if item.mat-type eq "S" then do:
                    if rfqitem.tab-in then do:
                       {rfq/u2estc.i rfqitem.k-len 5}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 6}
                    end.
                 end.
                 else if item.mat-type eq "T" then do:
                    {rfq/u2estc.i rfqitem.k-len 7}
                 end.
           end.  /*if avail item and rfqitem.adhesive ne "NO JOINT" then do:*/
           else do:
                 {rfq/u2estc.i rfqitem.k-len 7}
           end.
           if rfqitem.len eq rfqitem.wid then do:
                 {rfq/u2estc.i rfqitem.k-wid 2 dim-fit}
           end.
           else do:
                 {rfq/u2estc.i rfqitem.k-wid 2}
           end.   
          
        end.   /* avail style */  
        IF RfqEstNo <> "" THEN DO:
            FIND FIRST eb NO-LOCK WHERE eb.company = prmComp
                                     AND eb.est-no  = RfqEstNo
                                     AND eb.stock-no = RfqStock NO-ERROR.
            
            IF AVAIL eb THEN DO:
                ASSIGN rfqitem.gluelap    = eb.gluelap
                    rfqitem.part-dscr1 = eb.part-dscr1 
                    rfqitem.part-dscr2 = eb.part-dscr2
                    rfqitem.t-wid      = eb.t-wid 
                    rfqitem.t-len      = eb.t-len
                    rfqitem.t-sqin     = eb.t-sqin              
                    rfqitem.est-no     = eb.est-no
                    rfqitem.form-no    = eb.form-no
                    rfqitem.blank-no   = eb.blank-no
                    rfqitem.i-coldscr  = eb.i-coldscr
                    rfqitem.plate-no   = eb.plate-no
                    rfqitem.spc-no     = eb.spc-no
                    rfqitem.upc-no     = eb.upc-no
                    rfqitem.weight-m   = eb.weight-m 
                    rfqitem.tr-cas     = eb.tr-cas 
                    rfqitem.tr-cnt     = eb.tr-cnt 
                    rfqitem.tr-cost    = eb.tr-cost
                  
                    .
                IF eb.est-type > 4 THEN DO i = 1 TO 10:
                    ASSIGN rfqitem.i-ps[i] = eb.i-ps[i]
                        rfqitem.i-code[i] = eb.i-code[i]
                        rfqitem.i-dscr[i] = eb.i-dscr[i]
                        rfqitem.i-%[i] = eb.i-%[i].
                END.
                else DO i = 1 TO 10:
                    ASSIGN rfqitem.i-ps[i] = eb.i-ps2[i]
                        rfqitem.i-code[i] = eb.i-code2[i]
                        rfqitem.i-dscr[i] = eb.i-dscr2[i]
                        rfqitem.i-%[i] = eb.i-%2[i].
                    END.
                    FIND FIRST ef NO-LOCK WHERE ef.company = eb.company
                                            AND ef.est-no = eb.est-no
                                            AND ef.form-no = eb.form-no NO-ERROR.
                    IF AVAIL ef THEN DO i = 1 TO 4:
                        ASSIGN rfqitem.leaf[i] = ef.leaf[i]
                            rfqitem.leaf-dscr[i] = ef.leaf-dscr[i].
                    END.
                    assign rfqitem.adder[1] = ef.adder[1]
                        rfqitem.adder[2] = ef.adder[2]
                        rfqitem.adder[3] = ef.adder[3]
                        rfqitem.adder[4] = ef.adder[4]
                        rfqitem.adder[5] = ef.adder[5]
                        rfqitem.adder[6] = ef.adder[6]
                        rfqitem.adder[7] = ef.adder[7]
                        rfqitem.adder[8] = ef.adder[8]
                        rfqitem.adder[9] = ef.adder[9]
                        rfqitem.adder[10] = ef.adder[10]
                        rfqitem.adder[11] = ef.adder[11]
                        rfqitem.adder[12] = ef.adder[12]
                        rfqitem.leaf-l[1]  = ef.leaf-l[1]
                        rfqitem.leaf-l[2]  = ef.leaf-l[2]   
                        rfqitem.leaf-l[3]  = ef.leaf-l[3]   
                        rfqitem.leaf-l[4]  = ef.leaf-l[4]   
                        rfqitem.leaf-w[1]  = ef.leaf-w[1]   
                        rfqitem.leaf-w[2]  = ef.leaf-w[2]   
                        rfqitem.leaf-w[3]  = ef.leaf-w[3]   
                        rfqitem.leaf-w[4]  = ef.leaf-w[4]   
                        rfqitem.cost-msh   = ef.cost-msh 
                        rfqitem.cost-uom   = ef.cost-uom 
                        rfqitem.die-in     = ef.die-in
                        rfqitem.fr-msh     = ef.fr-msh  
                        rfqitem.fr-uom     = ef.fr-uom  
                        rfqitem.lam-code   = ef.lam-code 
                        rfqitem.m-code     = ef.m-code    
                        rfqitem.m-dscr     = ef.m-dscr      
                        rfqitem.medium     = ef.medium  
                        
                        .


              END. /**IF AVAIL eb THEN DO:*/
        END.    /*IF rfqitem.stock-no <> ""*/
        ELSE DO:
            FIND FIRST itemfg NO-LOCK WHERE itemfg.company = prmComp
                                        AND itemfg.i-no = RfqStock NO-ERROR.
            IF AVAIL itemfg AND itemfg.est-no <> "" THEN do:
                FIND FIRST eb NO-LOCK WHERE eb.company = itemfg.company
                                        AND eb.est-no = itemfg.est-no
                                        AND eb.stock-no = rfqitem.stock-no NO-ERROR.
                IF AVAIL eb THEN DO:
                    ASSIGN rfqitem.gluelap    = eb.gluelap
                        rfqitem.part-dscr1 = eb.part-dscr1 
                        rfqitem.part-dscr2 = eb.part-dscr2
                        rfqitem.t-wid      = eb.t-wid 
                        rfqitem.t-len      = eb.t-len
                        rfqitem.t-sqin     = eb.t-sqin              
                        rfqitem.est-no     = eb.est-no
                        rfqitem.form-no    = eb.form-no
                        rfqitem.blank-no   = eb.blank-no
                        /*rfqitem.i-coat     = eb.i-coat*/
                        rfqitem.i-coldscr  = eb.i-coldscr
                        /*rfqitem.i-col      = eb.i-col
                        rfqitem.i-pass     = eb.i-pass*/
                        rfqitem.plate-no   = eb.plate-no
                        rfqitem.spc-no     = eb.spc-no
                        rfqitem.upc-no     = eb.upc-no
                        rfqitem.weight-m   = eb.weight-m 
                        rfqitem.tr-cas     = eb.tr-cas 
                        rfqitem.tr-cnt     = eb.tr-cnt 
                        rfqitem.tr-cost    = eb.tr-cost 
                       
                        .
                 IF eb.est-type > 4 THEN DO i = 1 TO 10:
                    ASSIGN rfqitem.i-ps[i] = eb.i-ps[i]
                           rfqitem.i-code[i] = eb.i-code[i]
                           rfqitem.i-dscr[i] = eb.i-dscr[i]
                           rfqitem.i-%[i] = eb.i-%[i].
                 END.
                 else DO i = 1 TO 10:
                    ASSIGN rfqitem.i-ps[i] = eb.i-ps2[i]
                           rfqitem.i-code[i] = eb.i-code2[i]
                           rfqitem.i-dscr[i] = eb.i-dscr2[i]
                           rfqitem.i-%[i] = eb.i-%2[i].
                 END.
                 FIND FIRST ef NO-LOCK WHERE ef.company = eb.company
                                         AND ef.est-no = eb.est-no
                                         AND ef.form-no = eb.form-no NO-ERROR.
                 IF AVAIL ef THEN DO i = 1 TO 4:
                    ASSIGN rfqitem.leaf[i] = ef.leaf[i]
                           rfqitem.leaf-dscr[i] = ef.leaf-dscr[i].
                 END.
                 assign rfqitem.adder[1] = ef.adder[1]
                     rfqitem.adder[2] = ef.adder[2]
                     rfqitem.adder[3] = ef.adder[3]
                     rfqitem.adder[4] = ef.adder[4]
                     rfqitem.adder[5] = ef.adder[5]
                     rfqitem.adder[6] = ef.adder[6]
                     rfqitem.adder[7] = ef.adder[7]
                     rfqitem.adder[8] = ef.adder[8]
                     rfqitem.adder[9] = ef.adder[9]
                     rfqitem.adder[10] = ef.adder[10]
                     rfqitem.adder[11] = ef.adder[11]
                     rfqitem.adder[12] = ef.adder[12]
                     rfqitem.leaf-l[1]  = ef.leaf-l[1]   
                     rfqitem.leaf-l[2]  = ef.leaf-l[2]   
                     rfqitem.leaf-l[3]  = ef.leaf-l[3]   
                     rfqitem.leaf-l[4]  = ef.leaf-l[4]   
                     rfqitem.leaf-w[1]  = ef.leaf-w[1]   
                     rfqitem.leaf-w[2]  = ef.leaf-w[2]   
                     rfqitem.leaf-w[3]  = ef.leaf-w[3]   
                     rfqitem.leaf-w[4]  = ef.leaf-w[4]   
                     rfqitem.cost-msh   = ef.cost-msh 
                     rfqitem.cost-uom   = ef.cost-uom 
                     rfqitem.die-in     = ef.die-in
                     rfqitem.fr-msh     = ef.fr-msh  
                     rfqitem.fr-uom     = ef.fr-uom  
                     rfqitem.lam-code   = ef.lam-code 
                     rfqitem.m-code     = ef.m-code    
                     rfqitem.m-dscr     = ef.m-dscr      
                     rfqitem.medium     = ef.medium     

                     .
              END.
           END.  /* ELSE DO:*/
          
        END.
  END PROCEDURE.
  /********************************************* PROCEDURE calc-pass **********************/
  PROCEDURE calc-pass :
      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.
      find first style where style.company = rfqitem.company and
                 style.style = rfqitem.style no-lock no-error.
      
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).
         find first item where item.company = rfqitem.company and
                    item.i-no = style.material[2] no-lock no-error.
         if avail item then k = integer(style.material[3]).
         find first alt-item where alt-item.company  = rfqitem.company  and
                                   alt-item.mat-type = "V"     and
                                   alt-item.i-no     = style.material[6]
                                   no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first rfq-ctrl where rfq-ctrl.company = rfqitem.company and
                                   rfq-ctrl.loc = rfqitem.loc
                                   no-lock no-error.
         if k = 0 then k = rfq-ctrl.def-inkcov.
              
         if not avail item then do:
            find first item where item.company = rfqitem.company and
                       item.i-no = rfq-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = rfqitem.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = rfq-ctrl.def-coat
                                      no-lock no-error.
         
      end.
   
      save_id = recid(item). 
      save_id2 = IF AVAIL ALT-ITEM THEN recid(alt-item) ELSE ?.
      j = (integer(rfqitem.i-col) + integer(rfqitem.i-coat)) 
          .
      
      {sys/inc/roundup.i j}
      counter = 1.
      choice = true.
      find bf-rfqitem of rfqitem exclusive-lock.    
      if choice then do i = 1 to 10:
         if i le RfqCol THEN DO :
              find item where recid(item) = save_id no-lock no-error.
              assign bf-rfqitem.i-ps[i]   = counter
                     bf-rfqitem.i-code[i] = item.i-no
                     bf-rfqitem.i-dscr[i] = item.est-dscr
                     bf-rfqitem.i-%[i]    = k.
         end.
         else if (i > integer(rfqitem.i-col)) and
                 (i <= (integer(rfqitem.i-col) + 
                       integer(rfqitem.i-coat)) )
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
              assign bf-rfqitem.i-ps[i]   = counter
                     bf-rfqitem.i-code[i] = IF AVAIL ALT-ITEM THEN alt-item.i-no ELSE ""
                     bf-rfqitem.i-dscr[i] = IF AVAIL ALT-ITEM THEN alt-item.est-dscr ELSE ""
                     bf-rfqitem.i-%[i]    = 100.
         end.
         else if (i >  (rfqitem.i-col + rfqitem.i-coat) )
         then do:
            assign bf-rfqitem.i-ps[i]   = 0  
                     bf-rfqitem.i-code[i] = ""
                     bf-rfqitem.i-dscr[i] = "" 
                     bf-rfqitem.i-%[i]    = 0.  
        
         end.
         if j <> 0 and i modulo j = 0 then counter = counter + 1.
         if counter > (rfqitem.i-pass) then counter = rfqitem.i-pass.
      end.
   
END PROCEDURE.
/**************************************** PROCEDURE calc-blank-size  ************************/
PROCEDURE calc-blank-size :
   def var lv-panels as log no-undo.
   DEF VAR i AS INT NO-UNDO.
   def var j as int no-undo.
   def var v-score-char like v-lscore-c extent 100 NO-UNDO.


   find first sys-ctrl  where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "PANELS"
        no-lock no-error.
   if not avail sys-ctrl then do:
      create sys-ctrl.
      assign  sys-ctrl.company = cocode
              sys-ctrl.name    = "PANELS"
              sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
              sys-ctrl.log-fld = yes.
   end.

   lv-panels = sys-ctrl.log-fld.

   if not avail rfq then find first rfq where rfq.company = rfqitem.company and
                                        rfq.rfq-no = PrmRfqNo no-lock.   
   
   find style where style.company = rfq.company and
                    style.style = Rfqstyle NO-LOCK NO-ERROR.
   if avail style and style.industry = "2" then do:
       if style.type <> "F" then run calc-blank-size2. 
       run rfq/u2kinc1c.p (recid(rfq), recid(bf-rfqitem)).
       run rfq/u2kinc2c.p (recid(rfq), recid(bf-rfqitem)).
       /*find bf-rfqitem of rfqitem exclusive-lock.  */
       
       find first formule no-lock.
       assign bf-rfqitem.t-wid = (formule.formule[1])
           bf-rfqitem.t-len = (formule.formule[2])
           bf-rfqitem.t-sqin = (formule.formule[7] * formule.formule[8]).

       IF not lv-panels or style.type = "F" then 
         assign bf-rfqitem.k-wid-array2[1] = bf-rfqitem.t-wid
                bf-rfqitem.k-len-array2[1] = bf-rfqitem.t-len.
      else do:
         run rfq/descalc2.p (recid(bf-rfqitem)).

         DO i = 1 TO EXTENT(bf-rfqitem.k-wid-scr-type2):
           ASSIGN
            bf-rfqitem.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
            bf-rfqitem.k-len-scr-type2[i] = lv-k-len-scr-type[i].
         END.

         if v-lscore-c begins "No" THEN
            assign bf-rfqitem.k-wid-array2[1] = bf-rfqitem.t-wid
                   bf-rfqitem.k-len-array2[1] = bf-rfqitem.t-len.
          
         else do:
           i = 0.
           for each w-box-design-line:

             ASSIGN
                i = i + 1
                bf-rfqitem.k-wid-array2[i] = w-box-design-line.wscore-d.
             {sys/inc/k16bb.i bf-rfqitem.k-wid-array2[i]}
              
             

           end.

           assign v-score-char    = ""
                  j               = 1.
           do i = 1 to 80:
             if substr(v-lscore-c,i,1) ne "" then do:
                v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
                if substr(v-lscore-c,i + 1,1) eq "" then
                   assign  v-score-char[j] = trim(v-score-char[j])
                           j = j + 1.
             end.
             if j gt /*12*/ EXTENT(bf-rfqitem.k-len-array2) then leave.
           end.
           do i = 1 to EXTENT(bf-rfqitem.k-len-array2):
              bf-rfqitem.k-len-array2[i] = dec(v-score-char[i]).
              {sys/inc/k16bb.i bf-rfqitem.k-len-array2[i]}.
           end.
         end.  /* else v-lscore */
       end. /* panels or not foam */

   end.
   else if avail style then do:  /* folding */
      run rfq/u2kinc1f.p (recid(rfq), recid(bf-rfqitem)).
      run rfq/u2kinc2f.p (recid(rfq), recid(bf-rfqitem)).
      find first formule no-lock.
      find bf-rfqitem of rfqitem exclusive-lock.    
      assign bf-rfqitem.t-wid = (formule.formule[1])
          bf-rfqitem.t-len = (formule.formule[2])
          bf-rfqitem.t-sqin = (formule.formule[7] * formule.formule[8]).
      
   end.
END PROCEDURE.

/************************************************PROCEDURE calc-blank-size2 :*************************/

PROCEDURE calc-blank-size2 :
    
    find xritem where recid(xritem) = recid(rfqitem) no-lock.
   
   {rfq/u2estc.i rfqitem.gluelap 1}
   {rfq/u2estc.i rfqitem.k-wid 2}
   find first item where item.company = rfq.company
                    and item.i-no eq rfqitem.adhesive
                  no-lock no-error.
   
   if avail item then do:
            if item.mat-type eq "G" then do:
                    if rfqitem.tab-in then do:
                       {rfq/u2estc.i rfqitem.k-len 3}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 4}
                    end.
                       
            end.
            else if item.mat-type eq "S" then do:
                    if rfqitem.tab-in then do:
                       {rfq/u2estc.i rfqitem.k-len 5}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 6}
                    end.
            end.
            else if item.mat-type eq "T" then do:
                    {rfq/u2estc.i rfqitem.k-len 7}
            end.
    end.
    else do:
                 {rfq/u2estc.i rfqitem.k-len 7}
    end.

    if rfqitem.len eq rfqitem.wid
    then do:
                 {rfq/u2estc.i rfqitem.k-wid 2 dim-fit}
    end.
    else do:
                 {rfq/u2estc.i rfqitem.k-wid 2}
    end.

END PROCEDURE.
/*****************************************************************************************************************/

PROCEDURE copy-from-est :
    def var lv-tmp-recid as recid no-undo.
    DEF VAR i AS INT NO-UNDO.
    FIND FIRST eb where eb.stock-no = RfqStock AND eb.part-no = RfqPartno no-lock NO-ERROR.
    FIND FIRST ef of eb no-lock no-error.
    assign rfqitem.adder[1] = ef.adder[1]                                        
         rfqitem.adder[2] = ef.adder[2]
         rfqitem.adder[3] = ef.adder[3]
         rfqitem.adder[4] = ef.adder[4]
         rfqitem.adder[5] = ef.adder[5]
         rfqitem.adder[6] = ef.adder[6]
         rfqitem.adder[7] = ef.adder[7]
         rfqitem.adder[8] = ef.adder[8]
         rfqitem.adder[9] = ef.adder[9]
         rfqitem.adder[10] = ef.adder[10]
         rfqitem.adder[11] = ef.adder[11]
         rfqitem.adder[12] = ef.adder[12]
         rfqitem.leaf[1]  = ef.leaf[1]   
         rfqitem.leaf[2]  = ef.leaf[2]   
         rfqitem.leaf[3]  = ef.leaf[3]   
         rfqitem.leaf[4]  = ef.leaf[4]   
         rfqitem.leaf-dscr[1]  = ef.leaf-dscr[1] 
         rfqitem.leaf-dscr[2]  = ef.leaf-dscr[2] 
         rfqitem.leaf-dscr[3]  = ef.leaf-dscr[3] 
         rfqitem.leaf-dscr[4]  = ef.leaf-dscr[4] 
         rfqitem.leaf-l[1]  = ef.leaf-l[1]   
         rfqitem.leaf-l[2]  = ef.leaf-l[2]   
         rfqitem.leaf-l[3]  = ef.leaf-l[3]   
         rfqitem.leaf-l[4]  = ef.leaf-l[4]   
         rfqitem.leaf-w[1]  = ef.leaf-w[1]   
         rfqitem.leaf-w[2]  = ef.leaf-w[2]   
         rfqitem.leaf-w[3]  = ef.leaf-w[3]   
         rfqitem.leaf-w[4]  = ef.leaf-w[4]   
         rfqitem.brd-dscr   = ef.brd-dscr
         rfqitem.cost-msh   = ef.cost-msh 
         rfqitem.cost-uom     = ef.cost-uom 
         rfqitem.die-in       = ef.die-in 
         rfqitem.flute   = ef.flute    
         rfqitem.fr-msh   = ef.fr-msh  
         rfqitem.fr-uom    = ef.fr-uom  
         rfqitem.lam-code =  ef.lam-code 
         rfqitem.m-code   = ef.m-code    
         rfqitem.m-dscr  = ef.m-dscr      
         rfqitem.medium  = ef.medium     
         rfqitem.test     =    ef.test 
         .
       
  assign  rfqitem.adhesive = eb.adhesive 
          rfqitem.cad-no   = eb.cad-no        
          rfqitem.carr-dscr = eb.carr-dscr 
          rfqitem.carrier = eb.carrier      
          rfqitem.cas-cnt = eb.cas-cnt         
          rfqitem.cas-cost = eb.cas-cost 
          rfqitem.cas-dep  = eb.cas-dep
          rfqitem.cas-len   = eb.cas-len  
          rfqitem.cas-no   = eb.cas-no    
          rfqitem.cas-pal  = eb.cas-pal 
          rfqitem.cas-wid  = eb.cas-wid 
          rfqitem.cas-wt   = eb.cas-wt 
          rfqitem.die-in   = eb.die-in
          rfqitem.die-no   = eb.die-no 
          rfqitem.dust     = eb.dust 
          rfqitem.flute    = eb.flute 
          rfqitem.fpanel   = eb.fpanel
         .

    assign 
        rfqitem.gluelap = eb.gluelap                                       
        rfqitem.i-%[1] = eb.i-%[1]    
        rfqitem.i-%[2] = eb.i-%[2]    
        rfqitem.i-%[3] = eb.i-%[3]    
        rfqitem.i-%[4] = eb.i-%[4]    
        rfqitem.i-%[5] = eb.i-%[5]    
        rfqitem.i-%[6] = eb.i-%[6]    
        rfqitem.i-%[7] = eb.i-%[7]    
        rfqitem.i-%[8] = eb.i-%[8]    
        rfqitem.i-%[9] = eb.i-%[9]    
        rfqitem.i-%[10] = eb.i-%[10]    
        rfqitem.i-code[1] = eb.i-code[1] 
        rfqitem.i-code[2] = eb.i-code[2] 
        rfqitem.i-code[3] = eb.i-code[3] 
        rfqitem.i-code[4] = eb.i-code[4] 
        rfqitem.i-code[5] = eb.i-code[5] 
        rfqitem.i-code[6] = eb.i-code[6] 
        rfqitem.i-code[7] = eb.i-code[7] 
        rfqitem.i-code[8] = eb.i-code[8] 
        rfqitem.i-code[9] = eb.i-code[9] 
        rfqitem.i-code[10] = eb.i-code[10] 
        rfqitem.i-coldscr = eb.i-coldscr 
        rfqitem.i-dscr[1] = eb.i-dscr[1] 
        rfqitem.i-dscr[2] = eb.i-dscr[2] 
        rfqitem.i-dscr[3] = eb.i-dscr[3] 
        rfqitem.i-dscr[4] = eb.i-dscr[4] 
        rfqitem.i-dscr[5] = eb.i-dscr[5] 
        rfqitem.i-dscr[6] = eb.i-dscr[6] 
        rfqitem.i-dscr[7] = eb.i-dscr[7] 
        rfqitem.i-dscr[8] = eb.i-dscr[8] 
        rfqitem.i-dscr[9] = eb.i-dscr[9] 
        rfqitem.i-dscr[10] = eb.i-dscr[10] 
        rfqitem.i-pass = eb.i-pass   
        rfqitem.i-ps[1] = eb.i-ps[1]  
        rfqitem.i-ps[2] = eb.i-ps[2]  
        rfqitem.i-ps[3] = eb.i-ps[3]  
        rfqitem.i-ps[4] = eb.i-ps[4]  
        rfqitem.i-ps[5] = eb.i-ps[5]  
        rfqitem.i-ps[6] = eb.i-ps[6]  
        rfqitem.i-ps[7] = eb.i-ps[7]  
        rfqitem.i-ps[8] = eb.i-ps[8]  
        rfqitem.i-ps[9] = eb.i-ps[9]  
        rfqitem.i-ps[10] = eb.i-ps[10]  
        .
  assign      
        rfqitem.k-len = eb.k-len     
        rfqitem.k-wid = eb.k-wid   
        rfqitem.len  = eb.len      
        rfqitem.lin-in = eb.len     
        rfqitem.lock = eb.lock
        rfqitem.part-dscr1 = eb.part-dscr1 
        rfqitem.part-dscr2 = eb.part-dscr2
        rfqitem.part-no = eb.part-no 
        rfqitem.plate-no = eb.plate-no
        rfqitem.spc-no   = eb.spc-no  
        rfqitem.tr-cas  = eb.tr-cas 
        rfqitem.tr-cnt  = eb.tr-cnt 
        rfqitem.tr-cost = eb.tr-cost 
        rfqitem.tr-dep  = eb.tr-dep
        rfqitem.tr-len  = eb.tr-len
        rfqitem.tr-no   = eb.tr-no
        rfqitem.tr-wid  = eb.tr-wid 
        rfqitem.tuck    = eb.tuck 
        rfqitem.upc-no  = eb.upc-no
        rfqitem.weight-m = eb.weight-m
        rfqitem.t-wid    = eb.t-wid 
        rfqitem.t-len    = eb.t-len
        rfqitem.t-sqin = eb.t-sqin              
        rfqitem.est-no = eb.est-no
        rfqitem.form-no = eb.form-no
        rfqitem.blank-no = eb.blank-no
        .   
   IF eb.est-type < 5 THEN DO i = 1 TO 10:
      ASSIGN rfqitem.i-ps[i] = eb.i-ps2[i]
             rfqitem.i-code[i] = eb.i-code2[i]
             rfqitem.i-dscr[i] = eb.i-dscr2[i]
             rfqitem.i-%[i] = eb.i-%2[i].
   END.
   IF eb.est-type = 4 OR eb.est-type = 8 THEN DO:
      i = 1.
      FOR EACH est-flm NO-LOCK WHERE est-flm.company = eb.company
                                 AND est-flm.est-no = eb.est-no
                                 by est-flm.snum 
                                 by est-flm.bnum
                                 BY est-flm.line.
          IF i > 10 THEN LEAVE.
          ASSIGN rfqitem.leaf[i] = est-flm.i-no
                 rfqitem.leaf-dscr[i] = est-flm.dscr
                 rfqitem.leaf-l[i]  = est-flm.len   
                 rfqitem.leaf-w[i]  = est-flm.wid 
                 i = i + 1.
     END.
   END.
END PROCEDURE.
/************************************************************************************************************************/




