/*------------------------------------------------------------------------
    File        : SchedRelShip.p
    Purpose     :  Print Scheduled Release by Ship-to

    Syntax      :

    Description : Return a Dataset for Order Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
{custom/xprint.i}
{sys/inc/var.i new shared}

DEFINE TEMP-TABLE ttSchedRelShip NO-UNDO
FIELD shipFile AS CHAR.
DEFINE DATASET dsSchedRelShip FOR ttSchedRelShip .

    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmShipAct         AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust        AS CHARACTER  NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginItem      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginorder     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndorder       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginship      AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndship        AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmBeginsales     AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndsales       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginDate      AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate        AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmScheduled      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmActual         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmLate           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBackOrd        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmpastdate       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmPost           AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmComplete       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmInvoice        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmSort           AS CHARACTER NO-UNDO. 


    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSchedRelShip.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser         = ? THEN ASSIGN prmUser       = "".
    IF prmBeginCust    = ? THEN ASSIGN prmBeginCust  = "".
    IF prmEndCust      = ? THEN ASSIGN prmEndCust    = "".
    IF prmBeginorder   = ? THEN ASSIGN prmBeginorder = 0.
    IF prmEndorder     = ? THEN ASSIGN prmEndorder   = 0.
    IF prmBeginItem    = ? THEN ASSIGN prmBeginItem  = "".
    IF prmEndItem      = ? THEN ASSIGN prmEndItem    = "".
    IF prmBeginship    = ? THEN ASSIGN prmBeginship  = "".
    IF prmEndship      = ? THEN ASSIGN prmEndship    = "".
    IF prmScheduled    = ? THEN ASSIGN prmScheduled  = "".
    IF prmActual       = ? THEN ASSIGN prmActual     = "".
    IF prmLate         = ? THEN ASSIGN prmLate       = "".
    IF prmBackOrd      = ? THEN ASSIGN prmBackOrd    = "".
    IF prmpastdate     = ? THEN ASSIGN prmpastdate   = "".
    IF prmPost         = ? THEN ASSIGN prmPost       = "".
    IF prmComplete     = ? THEN ASSIGN prmComplete   = "".
    IF prmInvoice      = ? THEN ASSIGN prmInvoice    = "".
    IF prmSort         = ? THEN ASSIGN prmSort       = "".
    IF  prmBeginsales  = ? THEN ASSIGN prmBeginsales = "".
    IF prmEndsales     = ? THEN ASSIGN prmEndsales   = "".
    IF prmShipAct      = ? THEN ASSIGN prmShipAct    = "".

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)"   NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE begin_ship AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE end_ship AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /*INITIAL "C:\Inetpub\wwwroot\pdfs\schedrep.csv"*/ NO-UNDO.
DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)"  NO-UNDO.
DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" NO-UNDO.
DEFINE VARIABLE tb_actual AS CHAR NO-UNDO.
DEFINE VARIABLE tb_backordered AS CHAR NO-UNDO.
DEFINE VARIABLE tb_completed AS CHAR  NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_invoice AS CHAR NO-UNDO.
DEFINE VARIABLE tb_invoiceable AS CHAR NO-UNDO.
DEFINE VARIABLE tb_late AS CHAR NO-UNDO.
DEFINE VARIABLE tb_posted AS CHAR NO-UNDO.
DEFINE VARIABLE tb_scheduled AS CHAR  NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.


def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.

    
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report FIELD qty LIKE oe-rell.qty.


DEF STREAM excel.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 48.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=9 (13CPI)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)" INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5. 
DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99"  NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no .
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO. 
DEF VAR prmComp AS CHAR NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
MESSAGE "ship" prmBeginCust prmEndCust.
FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Please enter the right customer.....".
    RETURN.
END.


FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
   AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Please enter the right customer.....".
    RETURN.
END.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

ASSIGN 
    v-today        = TODAY
    cocode         = prmComp
    locode         = usercomp.loc.

MESSAGE "testdate" prmBeginDate prmEndDate prmScheduled  prmActual prmLate prmBackOrd prmpastdate
     prmPost prmComplete prmInvoice  prmSort .
IF prmShipAct = "ShipRel" THEN DO:
assign
    
    begin_cust-no  = prmBeginCust 
    end_cust-no    = prmEndCust   
    begin_ord-no   = prmBeginorder
    end_ord-no     = prmEndorder  
    begin_i-no     = prmBeginItem 
    end_i-no       = prmEndItem   
    begin_ship     = prmBeginship 
    end_ship       = prmEndship   
    begin_slsmn    = prmBeginsales                          
    end_slsmn      = prmEndsales
    rd_sort        = prmSort  
    begin_date     = prmBeginDate 
    end_date       = prmEndDate
    .
    IF prmScheduled  = "yes" THEN ASSIGN tb_scheduled   = "S" .
    IF prmActual     = "yes" THEN ASSIGN tb_actual      = "A" .
    IF prmLate       = "yes" THEN ASSIGN tb_late        = "L".
    IF prmBackOrd    = "yes" THEN ASSIGN tb_backordered = "B" .
    IF prmpastdate   = "yes" THEN ASSIGN tb_invoiceable = "I" .
    IF prmPost       = "yes" THEN ASSIGN tb_posted      = "P".
    IF prmComplete   = "yes" THEN ASSIGN tb_completed   = "C".
    IF prmInvoice    = "yes" THEN ASSIGN tb_invoice     = "Z" .
    

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
    ASSIGN
          init-dir    = v-webrootpath.


   
    ASSIGN
            init-dir    = v-webrootpath
            fi_file = init-dir + "schedrep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "schedrep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
              
 run run-report.
    CREATE ttSchedRelShip.
    ASSIGN ttSchedRelShip.shipFile = vPdfFile.
END.   /*if prmActShip = ""*/

/*************************************************************************************    */
PROCEDURE run-report  :

{sys/form/r-top3w.f}

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-itemfg FOR itemfg.

def var v-fcust like cust.cust-no extent 2 init ["","zzzzzzzz"].
def var v-fship like shipto.ship-id extent 2 init ["","zzzzzzzz"].
def var v-ford-no as int format ">>>>>>" extent 2 init [0,999999].
def var v-fitem as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-fsman as char format "xxx" extent 2 init ["","zzz"] no-undo.
def var v-fdate as date extent 2 format "99/99/9999" init [today, 12/31/9999].
def var v-sort as char format "!" init "C".
def var v-types as char format "x(7)" init "PALSBIC".

def var v-qty like oe-rel.qty.
def var v-date like oe-rel.rel-date.
def var v-po-no like oe-rel.po-no.
def var v-rel-no like oe-rel.rel-no.
def var v-ship-id like oe-rel.ship-id.
def var v-city like cust.city.
def var v-state like cust.state.
def var v-zip like cust.zip.
def var v-price like oe-ordl.price.
def var v-value like oe-ordl.t-price.
def var v-type as char.
def var v-tot-qty as int extent 2.
def var v-tot-val as dec extent 2.
DEF VAR lv-qty LIKE oe-rell.qty NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-slsmn-info AS CHAR NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

FORM HEADER
     lv-slsmn-info FORMAT "x(30)"
     SKIP(1)
    WITH NO-BOX NO-ATTR-SPACE PAGE-TOP FRAME r-top-s.

  assign
   str-tit2 = "Scheduled Releases by Ship-To" 
   {sys/inc/ctrtext.i str-tit2 112}

   v-fcust[1]   = begin_cust-no
   v-fcust[2]   = end_cust-no
   v-fship[1]   = begin_ship
   v-fship[2]   = end_ship
   v-ford-no[1] = begin_ord-no
   v-ford-no[2] = end_ord-no
   v-fitem[1]   = begin_i-no
   v-fitem[2]   = end_i-no
   v-fsman[1]   = begin_slsmn
   v-fsman[2]   = end_slsmn
   v-fdate[1]   = begin_date
   v-fdate[2]   = end_date
   v-sort       = if rd_sort eq "Customer#"    then "C" else
                  if rd_sort eq "Release Date" then "R" else
                  if rd_sort eq "Item#"        then "I" else
                  if rd_sort eq "Item Name"    then "N" else
                  if rd_sort eq "Salesman"     then "S" else "O"
   v-types      = string(tb_posted)      + string(tb_actual)      +
                  string(tb_late)        + string(tb_scheduled)   +
                  string(tb_backordered) + string(tb_invoiceable) +
                  string(tb_completed)   + string(tb_invoice)

   str-tit3 = if v-sort eq "C" then "By Customer By Ship-To By Date"  else
              if v-sort eq "R" then "By Date By Customer"  else
              if v-sort eq "I" then "By Item By Date"      else
              if v-sort eq "N" then "By Item Name By Date" else
              if v-sort eq "O" then "By Order By Date"     else
                                    "By Salesman By Date"
   {sys/inc/ctrtext.i str-tit3 132}.
   
  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  

  IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).

    IF v-sort EQ "N" THEN
      excelheader = "Customer,Ship-To,City,St,Zip,Customer PO,Order,R#,"
                  + "FG Item#,FG Item Name,Release Qty,Date,T,Unit Pr,Sales Value".
    ELSE
      excelheader = "Customer,Ship-To,City,St,Zip,Customer PO,Order,R#,"
                  + "Item,Release Qty,Date,T,Unit Pr,Sales Value".

    PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.
  EMPTY TEMP-TABLE tt-report.

  VIEW FRAME r-top.

  {oerep/r-sched2.i}

   

end procedure.

