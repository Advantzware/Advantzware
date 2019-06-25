
/*------------------------------------------------------------------------
    File        : rep_qtyval.p
    Purpose     :  qty on job and cust
    Syntax      :

    Description : Reports

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttReportQtyVal NO-UNDO
        FIELD vFile AS CHAR
        FIELD vhhhjkkj AS CHAR.
    DEFINE DATASET dsReportQtyVal FOR ttReportQtyVal .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmComp           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginPo          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndPo            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeSaleman        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSalesman      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmItemCode       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmZeroQty        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmInvWarehouse   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmReceipt        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmCustPart        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut            AS CHARACTER NO-UNDO.
   
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReportQtyVal.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser          = ? THEN ASSIGN prmUser          = "".
    IF prmAction        = ? THEN ASSIGN prmAction        = "".
    IF prmBeginCust     = ? THEN ASSIGN prmBeginCust     = "".
    IF prmEndCust       = ? THEN ASSIGN prmEndCust       = "".
    IF prmBeginPo       = ? THEN ASSIGN prmBeginPo       = "".
    IF prmEndPo         = ? THEN ASSIGN prmEndPo         = "".
    IF prmBeSaleman     = ? THEN ASSIGN prmBeSaleman     = "".
    IF prmEndSalesman   = ? THEN ASSIGN prmEndSalesman   = "".
    IF prmItemCode      = ? THEN ASSIGN prmItemCode      = "".
    IF prmZeroQty       = ? THEN ASSIGN prmZeroQty       = "".
    IF prmInvWarehouse  = ? THEN ASSIGN prmInvWarehouse  = "".
    IF prmReceipt       = ? THEN ASSIGN prmReceipt       = "".
    IF prmCustPart      = ? THEN ASSIGN prmCustPart      = "".
    IF prmOut           = ? THEN ASSIGN prmOut           = "No" .
    IF prmComp          = ? THEN ASSIGN prmComp          = "".
    

{sys/inc/var.i new shared}
 {custom/xprint.i}

   
/*{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}*/

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR  tmp-path AS CHAR NO-UNDO. 
DEF VAR v-VERSION AS CHAR NO-UNDO. 

DEFINE VAR vPdfFile AS CHAR NO-UNDO.

DEF STREAM excel.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "X(8)":U NO-UNDO.
DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U  NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz"  NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"   NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 48 NO-UNDO.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)"  NO-UNDO.
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11"  NO-UNDO.
DEFINE VARIABLE rd_itm-code AS CHARACTER INITIAL "All"  NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_inc-zer AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_part AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_rcpt-dat AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */ NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.

ASSIGN
    begin_cust   =     prmBeginCust    
    end_cust     =     prmEndCust      
    begin_cust-po  =   prmBeginPo      
    END_cust-po  =     prmEndPo        
    begin_slm    =     prmBeSaleman    
    end_slm      =     prmEndSalesman  

    rd_itm-code   =   prmItemCode    
    tb_inc-zer  =  IF prmZeroQty = "Yes" THEN TRUE ELSE FALSE
    tb_inc-cust =  IF prmInvWarehouse = "Yes" THEN TRUE ELSE FALSE
    tb_rcpt-dat =  IF prmReceipt = "Yes" THEN TRUE ELSE FALSE
    tb_part     =  IF prmCustPart = "Yes" THEN TRUE ELSE FALSE
    tb_excel    =  IF prmOut = "Yes" THEN TRUE ELSE FALSE
    
    v-today      = TODAY
    .

DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser 
    AND (usercust.cust-no = prmEndCust OR prmEndCust = "zzzzzzzz")  NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.


    assign
 cocode = prmComp
 locode = usercomp.loc.

    FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "X-VERSION" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "X-VERSION"
         sys-ctrl.descrip  = "Server Name"
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "Server 2003".
   END.
   IF AVAIL sys-ctrl  THEN
        v-VERSION = sys-ctrl.char-fld .
  RELEASE sys-ctrl.
 
FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "Xspool" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "Xspool"
         sys-ctrl.descrip  = "Default path To Create temp File for Web pdf "
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "c:\spool\".
   END.
   IF AVAIL sys-ctrl  THEN
        tmp-path = sys-ctrl.char-fld .
  RELEASE sys-ctrl.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
IF prmAction = "jobcust"  THEN DO:
    IF prmOut = "No" THEN DO:
        ASSIGN  
    init-dir    = v-webrootpath
    lv-pdf-file = init-dir + 'Qtyonjobcust' 
    lv-pdf-file = lv-pdf-file + prmBeginCust + STRING(TIME)
    vPdfFile   = 'Qtyonjobcust' + prmBeginCust + STRING(TIME) + '.pdf'.
   
   run run-report.
   
   IF v-VERSION = "Server 2008" THEN do:
       OS-COPY VALUE(list-name) VALUE (tmp-path).
       PAUSE 1.
   END.
   ELSE
       RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
   
      CREATE ttReportQtyVal.
      ASSIGN ttReportQtyVal.vFile = vPdfFile.
      END.


      IF prmOut = "Yes" THEN DO:
          ASSIGN
              init-dir    = v-webrootpath
              v-excel-file = init-dir + "Qtyonjobcust" +
              STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

               vPdfFile   = "Qtyonjobcust" +
                   STRING(YEAR(v-today),"9999")
                    + STRING(MONTH(v-today),"99")
                    + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
              
               run run-report.
               
               CREATE ttReportQtyVal.
               ASSIGN ttReportQtyVal.vFile = vPdfFile.

      END.
 END.


/******************************************************************************************************/


 PROCEDURE run-report :
/* ------------------------------------------------ fg/rep/fg-sstat.p 3/94 RM */
/* finished goods inventory status by customer report                         */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-ext   as dec format "->>>,>>>,>>9.99".
def var fcst as ch init " ".
def var tcst like fcst init "zzzzzzzzz".
def var fpo# as ch init " ".
def var tpo# like fpo# init "zzzzzzzzz".
def var type as ch format "!" init "A".
def var fslm like cust.sman init " ".
def var tslm like cust.sman init "zzz".
def var zbal as log format "Y/N".
def var v-rec-dat as log format "Y/N" init no.
def var v-prt-cpn like v-rec-dat.
def var v-qty-onh as dec format "->>>,>>>,>>9".
def var v-frst as log.
def var v-frst-ord as log.
def var v-tot-ord  as dec format "->>>,>>>,>>9".
def var v-tot-ship as dec format "->>,>>>,>>9".
def var v-tot-onh as dec format "->>>,>>>,>>9".
def var v-tot-ext as dec format "->>>,>>>,>>9.99".
def var v-grand-tot-ord  as dec format "->>>,>>>,>>9".
def var v-grand-tot-ship as dec format "->>,>>>,>>9".
def var v-grand-tot-onh as dec format "->>>,>>>,>>9".
def var v-grand-tot-ext as dec format "->>>,>>>,>>9.99".
def var v-custown as log format "Y/N" init "N".
def var v-frst-i-no as log.
def var v-print as log.
def var trans-date like fg-rcpts.trans-date.
def var v-job as char format "x(9)".
def var v-rec-found as log.
def var v-qty-job like v-qty-onh.
def var v-ext-job like v-ext.
def buffer xbin for fg-bin.
def buffer xbin2 for fg-bin.
def var v-qty-ord as int.
def var v-qty-ship as int.
DEF BUFFER b-rcpth FOR fg-rcpth.
DEF BUFFER b-rdtlh FOR fg-rdtlh.

form
    cust.cust-no label "CUSTOMER"
    oe-ordl.po-no label "PO #"
    cust.sman label "SREP"
    oe-ordl.i-no  label "ITEM #"
    oe-ordl.part-no label "CUST PART #" format "x(15)"
    oe-ordl.i-name label "DESCRIPTION" format "x(15)"
    fg-bin.loc label "WHSE"
    oe-ordl.qty format "->,>>>,>>9" column-label "QTY ORDERED"
    oe-ordl.ship-qty format "->,>>>,>>9" column-label "QTY SHIPPED"
    v-qty-onh  column-label "QTY ON HAND"
    oe-ordl.price format ">>,>>>,>>9.99" column-label "SELL PRICE"
    v-ext format "->>>,>>>,>>9.99"  column-label "TOTAL VALUE"
    with frame itemx1 no-box down STREAM-IO width 160.

form
    cust.cust-no label "CUSTOMER"
    itemfg.cust-po-no label "PO #"
    cust.sman label "SREP"
    itemfg.i-no  label "ITEM #"
    itemfg.part-no label "CUST PART #" format "x(15)"
    itemfg.i-name label "DESCRIPTION" format "x(15)"
    v-job column-label "  JOB"
    v-qty-job  column-label "QTY ON HAND"
    trans-date column-label "REC DATE"
    itemfg.sell-price format ">>>,>>>9.99" column-label "SELL PRICE"
    v-ext-job format "->>>,>>>,>>9.99"  column-label "TOTAL VALUE"
    with frame itemx2 no-box down STREAM-IO width 150.

form
    cust.cust-no label "CUSTOMER"
    oe-ordl.po-no label "PO #"
    cust.sman label "SREP"
    oe-ordl.i-no  label "ITEM #"
    oe-ordl.i-name label "DESCRIPTION" format "x(15)"
    fg-bin.loc label "WHSE"
    oe-ordl.qty format "->,>>>,>>9" column-label "QTY ORDERED"
    oe-ordl.ship-qty format "->,>>>,>>9" column-label "QTY SHIPPED"
    v-qty-onh  column-label "QTY ON HAND"
    oe-ordl.price format ">>,>>>,>>9.99" column-label "SELL PRICE"
    v-ext format "->>>,>>>,>>9.99"  column-label "TOTAL VALUE"
    with frame itemx3 no-box down STREAM-IO width 150.

form
    cust.cust-no label "CUSTOMER"
    itemfg.cust-po-no label "PO #"
    cust.sman label "SREP"
    itemfg.i-no  label "ITEM #"
    itemfg.i-name label "DESCRIPTION" format "x(15)"
    v-job column-label "  JOB"
    v-qty-job  column-label "QTY ON HAND"
    trans-date column-label "REC DATE"
    itemfg.sell-price format ">>>,>>>9.99" column-label "SELL PRICE"
    v-ext-job format "->>>,>>>,>>9.99"  column-label "TOTAL VALUE"
    with frame itemx4 no-box down STREAM-IO width 150.

assign
 str-tit2 = "Finished Goods Inventory Status By Customer "
 {sys/inc/ctrtext.i str-tit2 112}

 fcst       = begin_cust
 tcst       = end_cust
 fpo#       = begin_cust-po
 tpo#       = END_cust-po
 fslm       = begin_slm
 tslm       = end_slm
 TYPE       = SUBSTR(rd_itm-code,1,1)
 zbal       = tb_inc-zer
 v-custown  = tb_inc-cust
 v-rec-dat  = tb_rcpt-dat
 v-prt-cpn  = tb_part.

 {sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

 PUT "<PDF=DIRECT><OLANDSCAPE><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=1mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><cpi13.3><p9>" FORM "x(400)". 

/*if td-show-parm then run show-param.*/

display "" with frame r-top.

SESSION:SET-WAIT-STATE("general").
IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(v-excel-file).
   /*EXPORT STREAM excel DELIMITER ","
           "CUSTOMER"
           "PO #"
           "SREP"
           "ITEM #"
           "DESCRIPTION" 
           "JOB"
           "QUANTITY ON HAND"
           "RECEIPT DATE"
           "SELLING PRICE"
           "TOTAL VALUE"
       SKIP.*/

    if not v-rec-dat then do:
        if v-prt-cpn then do:
            EXPORT STREAM excel DELIMITER ","
           "CUSTOMER"
           "PO #"
           "SREP"
           "ITEM #"
           "CUST Part"
           "DESCRIPTION" 
           "WHSE"
           "QTY ORDERED"
           "QTY SHIPPED"
           "QUANTITY ON HAND"
           "SELLING PRICE"
           "TOTAL VALUE"
            SKIP.
        END.
        ELSE DO:
            EXPORT STREAM excel DELIMITER ","
            "CUSTOMER"
            "PO #"
            "SREP"
            "ITEM #"
            "CUST Part"
            "DESCRIPTION" 
            "WHSE"
            "QTY ORDERED"
            "QTY SHIPPED"
            "QUANTITY ON HAND"
            "SELLING PRICE"
            "TOTAL VALUE"
            SKIP.
        END.
    END.
    ELSE DO:
    if v-prt-cpn then do:      
          EXPORT STREAM excel DELIMITER ","
           "CUSTOMER"
           "PO #"
           "SREP"
           "ITEM #"
           "DESCRIPTION" 
           "JOB"
           "QUANTITY ON HAND"
           "RECEIPT DATE"
           "SELLING PRICE"
           "TOTAL VALUE"
       SKIP.
     END.
        ELSE DO:         
            EXPORT STREAM excel DELIMITER ","
           "CUSTOMER"
           "PO #"
           "SREP"
           "ITEM #"
           "DESCRIPTION" 
           "JOB"
           "QUANTITY ON HAND"
           "RECEIPT DATE"
           "SELLING PRICE"
           "TOTAL VALUE"
       SKIP. 
        END.
    END.
END. 

{fgrep/r-qtyval.i} 

/*RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).*/

IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
END.

end procedure.






