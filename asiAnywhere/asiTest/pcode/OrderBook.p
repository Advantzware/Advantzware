

/*------------------------------------------------------------------------
    File        : OrderBook.p
    Purpose     :  Print Order
    Main File   : oerep\r-booked.p
    Syntax      :

    Description : Return a Dataset of Request For Order Booked

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/xprint.i}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i new shared}
DEFINE TEMP-TABLE ttOrderBookRep NO-UNDO
FIELD datFile AS CHAR
FIELD abc AS CHAR.

DEFINE DATASET dsOrderBookRep FOR ttOrderBookRep .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBookAct        AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust        AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBegdate        AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate        AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmBegsman        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndsman        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegPro         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndPro         AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmSquare         AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmPsman          AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmSortOrd        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmIdesc          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmTon            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmPro            AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmMis            AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmComm           AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmAvailMargin    AS CHARACTER NO-UNDO.  
    DEFINE INPUT PARAMETER prmOut            AS CHARACTER NO-UNDO.  
   

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderBookRep.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.
   
    
    IF prmUser        = ?  THEN ASSIGN prmUser        = "".
    IF prmBookAct     = ?  THEN ASSIGN prmBookAct        = "".
    IF prmBeginCust   = ?  THEN ASSIGN prmBeginCust   = "".
    IF prmEndCust     = ?  THEN ASSIGN prmEndCust     = "".
    IF prmBegsman     = ?  THEN ASSIGN prmBegsman     = "". 
    IF prmEndsman     = ?  THEN ASSIGN prmEndsman     = "". 
    IF prmBegPro      = ?  THEN ASSIGN prmBegPro      = "". 
    IF prmEndPro      = ?  THEN ASSIGN prmEndPro      = "". 
    IF prmSquare      = ?  THEN ASSIGN prmSquare      = "". 
    IF prmPsman       = ?  THEN ASSIGN prmPsman       = "". 
    IF prmSortOrd     = ?  THEN ASSIGN prmSortOrd     = "". 
    IF prmIdesc       = ?  THEN ASSIGN prmIdesc       = "". 
    IF prmTon         = ?  THEN ASSIGN prmTon         = "". 
    IF prmPro         = ?  THEN ASSIGN prmPro         = "". 
    IF prmMis         = ?  THEN ASSIGN prmMis         = "".
    IF prmComm        = ?  THEN ASSIGN prmComm        = "".
    IF prmEndCust   = "" THEN ASSIGN prmEndCust = "zzzzzzzz" .
   
     def var list-name as cha no-undo.
     DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
     DEF VAR lv-pdf-file AS cha NO-UNDO.
     DEFINE VAR vFile AS CHAR NO-UNDO.




 DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_fg-cat AS CHARACTER FORMAT "X(5)":U     NO-UNDO.
DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX"    NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzz"  NO-UNDO.
DEFINE VARIABLE end_fg-cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz"       NO-UNDO.
DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO.
DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz"   NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /* INITIAL "C:\Inetpub\wwwroot\pdfs\orderbook.csv" */ NO-UNDO. 
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99    NO-UNDO.
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P"   NO-UNDO.
DEFINE VARIABLE rd_sqft AS CHARACTER INITIAL "Square Ft"   NO-UNDO.
DEFINE VARIABLE tb_comm AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE tb_desc AS LOGICAL INITIAL no   NO-UNDO.
DEFINE VARIABLE tb_margin AS LOGICAL INITIAL NO NO-UNDO. 
DEFINE VARIABLE tb_prepmisc AS LOGICAL INITIAL no    NO-UNDO.
DEFINE VARIABLE tb_prft AS LOGICAL INITIAL yes      NO-UNDO.
DEFINE VARIABLE tb_smn-no AS LOGICAL INITIAL no     NO-UNDO.
DEFINE VARIABLE tb_sortby AS LOGICAL INITIAL no     NO-UNDO.
DEFINE VARIABLE tb_ton AS LOGICAL INITIAL no        NO-UNDO.
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.


def TEMP-TABLE w-data no-undo
  field ord-no like oe-ord.ord-no
  field line   like oe-ordl.line
  field sman   as char format "x(3)"
  field item-n like itemfg.i-name column-label "Item Description"
        format "x(27)"
  field procat like itemfg.procat column-label "Prod!Code"
  field qty like oe-ordl.qty column-label "Quantity!Ordered/EA"
        format ">,>>>,>>>"
  field sqft like itemfg.t-sqft column-label "Sq Ft" format ">>,>>>.999"
  field t-sqft like itemfg.t-sqft column-label "Total!Sq Ft/M" format "->,>>>.999"
  field t-tons as dec column-label "Total!  Tons" format "->,>>>.9"
  field price like oe-ordl.price format ">>>,>>9.99<<<<"
  field revenue like oe-ordl.t-price column-label "Order!Amount"
  field misc as log
  field cost as dec
  field comm as dec label "Comm %"
  FIELD margin AS DEC.

def TEMP-TABLE wkrecap no-undo    /* recap by product category */
  field procat like itemfg.procat column-label "Cat"
  field t-sqft like itemfg.t-sqft  extent 2 column-label "Sq Ft" format ">>,>>>.999"
  field t-tons as dec column-label "Tons" extent 2 format "->,>>>.9"
  field revenue like oe-ordl.t-price   extent 2 column-label "Amount"
  field price-per-m  as dec column-label "$/MSF" extent 2
  field price-per-t  as dec column-label "$/TON" extent 2
  field num-of-ord as int column-label "#Orders".

DEF TEMP-TABLE tt-report LIKE report.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
def var security-flag as log no-undo.

DEF STREAM excel.

DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc AS CHAR NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .


ASSIGN
    cocode = prmComp
    g_company = prmComp
    g_loc     = locode  .



FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid begin customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid end customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

IF prmBookAct = "orderbook" THEN DO:
 ASSIGN
    v-today        = TODAY
    begin_cust-no  = prmBeginCust
    end_cust-no    = prmEndCust
    begin_ord-date = prmBegdate
    end_ord-date   = prmEndDate
    begin_slsmn    = prmBegsman
    end_slsmn      = prmEndsman
    begin_fg-cat   = prmBegPro
    end_fg-cat     = prmEndPro 
    
    rd_sqft      = IF  prmSquare = "yes" THEN "Square Ft" ELSE "Part#"
    tb_smn-no    = IF  prmPsman  = "yes" THEN TRUE ELSE FALSE 
    tb_sortby    = IF  prmSortOrd  = "yes" THEN TRUE ELSE FALSE 
    tb_desc      = IF  prmIdesc    = "yes" THEN TRUE ELSE FALSE
    tb_ton       = IF  prmTon      = "yes" THEN TRUE ELSE FALSE
    tb_prft      = IF  prmPro      = "yes" THEN TRUE ELSE FALSE
    tb_prepmisc  = IF  prmMis      = "yes" THEN TRUE ELSE FALSE
    tb_comm      = IF  prmComm     = "yes" THEN TRUE ELSE FALSE
    tb_margin    = IF  prmAvailMargin = "yes" THEN TRUE ELSE FALSE
    .
    ASSIGN
        tb_excel = IF prmOut = "yes" THEN TRUE ELSE FALSE.
  
  ASSIGN 
    init-dir    = v-webrootpath
    fi_file = init-dir + "OrderBook" +
              STRING(YEAR(v-today),"9999")
              + STRING(MONTH(v-today),"99")
              + STRING(DAY(v-today),"99") + ".csv".  
    vFile   = "OrderBook" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
        ASSIGN
            vTextFile = "OrderBook" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt". 
    
    run run-report.
    
    CREATE ttOrderBookRep.
    IF tb_excel THEN
        ASSIGN ttOrderBookRep.datFile = vFile.
    IF NOT  tb_excel THEN
        ASSIGN ttOrderBookRep.datFile = vTextFile .
    END.

  /***************************************************************************/
PROCEDURE run-report :

{sys/form/r-topw.f}

def var fdate as date format "99/99/9999" init 01/01/0001 no-undo.
def var tdate like fdate init 12/31/9999 no-undo.
def var v-break as log init no no-undo.
def var prt-sqft as log init yes format "SqFt/PartNo" no-undo.
def var p-m-chg as log init no no-undo.
def var prt-profit as log init yes no-undo.
def var item-dscr as log init no no-undo.
def var mdate as date no-undo.
def var lo_trandate like fdate no-undo.
def var v-per-days as int extent 2 no-undo init 0.
def var v-n-lines  as int no-undo.
def var fsman as char format "x(3)" no-undo.
def var tsman as char format "x(3)" init "zzz" no-undo.
def var v-sman like w-data.sman no-undo.
def var v-exclude as log no-undo.
def var v-misc as log.
def var v-amt  like oe-ord.t-revenue.
def var v-pct as dec format "99.99".
def var v-sqft like itemfg.t-sqft  format ">,>>9.999".
def var v-tons as DEC.
def var v-qty like oe-ordl.qty format "->>>,>>9.99".
def var v-price-per-m as dec column-label "$/MSF" no-undo.
def var v-price-per-t as dec column-label "$/TON" format "->>>,>>>9.99" no-undo.
def var v-msf like v-price-per-m extent 2 no-undo.
def var v-ton like v-price-per-t extent 2 no-undo.

def var v-revenue like oe-ordl.t-price format "->,>>>,>>9.99" no-undo
  column-label "Order!Amount".
def var v-profit as dec format "->>,>>9.9" no-undo
  column-label "% Profit".
DEF VAR v-margin AS DEC FORMAT "->>,>>9.9" NO-UNDO COLUMN-LABEL "% Margin".
def var v-sname like sman.sname.

def var v as int.
def var qm as dec.
def var mat as dec.
def var lab as dec.

def var ii like i no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.

find first w-data no-error.

form header "Sales Rep:"
            w-data.sman
            "-"
            v-sname
    with frame r-top1 no-box no-attr-space page-top stream-io width 180.


assign
 str-tit2 = "Orders Booked"
 {sys/inc/ctrtext.i str-tit2 112}

 fdate      = begin_ord-date
 tdate      = end_ord-date
 fsman      = begin_slsmn
 tsman      = end_slsmn
 v-break    = tb_smn-no
 prt-sqft   = rd_sqft eq "Square Ft"
 item-dscr  = tb_desc
 prt-profit = tb_prft
 p-m-chg    = tb_prepmisc.

ASSIGN  prt-profit = YES .
IF tb_margin THEN
   prt-profit = NO .

/*
if prt-profit then do:
  IF NOT security-flag THEN RUN sys/ref/d-passwd.w (3, OUTPUT security-flag).
  prt-profit = security-flag.
end.*/

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Sales Rep,Sales Name,".
      
  if item-dscr then do:
    excelheader = excelheader + "Due Date,Order#,Customer Name,Comm %,"
                + "Item Description,Quantity Ordered/EA,"
                + "Total Sq Ft/M,$/MSF,Order Amount,".

    IF tb_margin THEN
       excelheader = excelheader +  "Avail Margin,".
    ELSE
       excelheader = excelheader + "% Profit,".

    if tb_ton THEN
      excelheader = excelheader + "Total Tons,$/TON".
  END.
  ELSE
  IF prt-sqft THEN DO:

    excelheader = excelheader + "Due Date,Order#,Customer Name,Comm %,"
                + "Prod Code,Quantity Ordered/EA,Sq Ft,"
                + "Total Sq Ft/M,$/MSF,Price,Order Amount,".

    IF tb_margin THEN
       excelheader = excelheader +  "Avail Margin,".
    ELSE
       excelheader = excelheader + "% Profit,".

    if tb_ton THEN
      excelheader = excelheader + "Total Tons,$/TON".
  END.
  ELSE DO:
    excelheader = excelheader + "Due Date,Order#,Customer Name,Comm %,"
                + "Prod Code,Quantity Ordered/EA,Customer Part Number,"
                + "$/MSF,Price,Order Amount,".

    IF tb_margin THEN
       excelheader = excelheader +  "Avail Margin,".
    ELSE
       excelheader = excelheader + "% Profit,".

    if tb_ton THEN
      excelheader = excelheader + "Total Tons,$/TON".
  END.
  
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

/*if td-show-parm then run show-param.*/


FOR EACH tt-report:
  DELETE tt-report.
END.

FOR EACH w-data:
  DELETE w-data.
END.

FOR EACH wkrecap:
  DELETE wkrecap.
END.

{oerep/r-booked.i} 

   

/*RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).*/


end procedure.


