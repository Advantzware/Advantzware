/*------------------------------------------------------------------------
    File        : CustInvReReport2.p
    Purpose     :  Customer Inventory Reorder Report

    Syntax      :

    Description : Return a Dataset of Customer Inventory Reorder Report2

    Author(s)   : Sewa
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 
  {custom/xprint.i}  
 /*{sys/inc/var.i new shared}*/
      
DEFINE TEMP-TABLE ttCustInvReReport2 NO-UNDO
      FIELD vCustRep AS CHAR
      FIELD vCustRepFile AS CHAR.
  DEFINE DATASET dsCustInvReReport2 FOR ttCustInvReReport2.

DEFINE INPUT PARAMETER prmUser         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmReAct        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmBegCust      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEndCust      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmBegShipto    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEndShitto    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmBegItem      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEndItem      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmBegCat       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEndCat       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCustInv      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmQOnHand      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTotalAlloc   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAsOf         AS DATE NO-UNDO.
DEFINE INPUT PARAMETER prmPrint        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPrintLotCtl  AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustInvReReport2.
  DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

    IF prmUser = ?         THEN ASSIGN prmUser = "".
    IF prmReAct = ?        THEN ASSIGN prmReAct = "".
    IF prmBegCust = ?      THEN ASSIGN prmBegCust = "".
    IF prmEndCust = ?      THEN ASSIGN prmEndCust = "".
    IF prmBegShipto = ?    THEN ASSIGN prmBegShipto = "".
    IF prmEndShitto = ?    THEN ASSIGN prmEndShitto = "".
    IF prmBegItem = ?      THEN ASSIGN prmBegItem = "".
    IF prmEndItem = ?      THEN ASSIGN prmEndItem = "".
    IF prmBegCat = ?       THEN ASSIGN prmBegCat = "".
    IF prmEndCat = ?       THEN ASSIGN prmEndCat = "".
    IF prmCustInv = ?      THEN ASSIGN prmCustInv = "".
    IF prmQOnHand = ?      THEN ASSIGN prmQOnHand = "".
    IF prmTotalAlloc = ?   THEN ASSIGN prmTotalAlloc = "".
    IF prmPrint = ?        THEN ASSIGN prmPrint = "".
    IF prmPrintLotCtl = ?  THEN ASSIGN prmPrintLotCtl = "".



DEFINE VARIABLE begin_cat       AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE begin_cust-no   AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE begin_i-no      AS CHARACTER FORMAT "X(15)"  NO-UNDO.
DEFINE VARIABLE begin_shipto    AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE end_cat         AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz"  NO-UNDO.
DEFINE VARIABLE end_cust-no     AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_i-no        AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_shipto      AS CHARACTER FORMAT "X(30)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE ldt-as-of       AS DATE FORMAT "99/99/9999":U  NO-UNDO.
DEFINE VARIABLE rd-print        AS CHARACTER INITIAL "A"  NO-UNDO.
DEFINE VARIABLE rd-print-lot    AS CHARACTER INITIAL "R"  NO-UNDO.
DEFINE VARIABLE rd-tot-rel      AS CHARACTER INITIAL "T"  NO-UNDO.
DEFINE VARIABLE t-include-order AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE t-use-cust-inv  AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VAR cocode AS CHAR NO-UNDO.
DEFINE VAR locode AS CHAR NO-UNDO.
DEFINE VARIABLE tmp-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"  INITIAL "C:\Inetpub\wwwroot\pdfs\CustInvReReport2.txt"  NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE vFile AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VAR v-term AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vTxtFile AS CHAR NO-UNDO.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=9 (13CPI)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)" INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5 NO-UNDO. 
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO. 
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO.

/*DEF VAR v-print-fmt AS CHARACTER NO-UNDO.*/

DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
DEF STREAM excel.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegCust  NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
   AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
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
 v-today =  TODAY
 cocode = prmComp
 locode = usercomp.loc.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

IF prmReAct = "PrintReprot" THEN DO:

    ASSIGN

        begin_cat        = prmBegCat
        begin_cust-no    = prmBegCust
        begin_i-no       = prmBegItem
        begin_shipto     = prmBegShipto
        end_cat          = prmEndCat
        end_cust-no      = prmEndCust
        end_i-no         = prmEndItem
        end_shipto       = prmEndShitto
        ldt-as-of        = prmAsOf
        rd-print         = prmPrint
        rd-print-lot     = prmPrintLotCtl
        rd-tot-rel       = prmTotalAlloc
        t-include-order  = IF prmQOnHand = "Yes" THEN TRUE ELSE FALSE
        t-use-cust-inv   = IF prmCustInv = "Yes" THEN TRUE ELSE FALSE
         .

   ASSIGN
    init-dir    = v-webrootpath
          
        vFile   = "cstitm" + STRING(TIME) + ".txt".


    run run-report.                       

    CREATE ttCustInvReReport2.
    ASSIGN ttCustInvReReport2.vCustRep = vFile.
END.


PROCEDURE run-report :

def var save_id as recid.
def var cocode as cha no-undo.
def var time_stamp as ch.
time_stamp = string(time, "hh:mmam").

def var v-cust      like itemfg.cust-no extent 2 init ["","zzzzzzzz"].
def var v-ship      like oe-rel.ship-id extent 2 init ["","zzzzzzzz"].
def var v-cat       like itemfg.procat extent 2 init ["","zzzzzz"].
def var v-item      like itemfg.i-no extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-inconh    as   log format "Y/N" init "Y".
def var v-totrel    as   log format "Tot All/Release" init "Y".
def var v-date      as   date format "99/99/9999" init today.
def var v-pur-man   as   char format "!" init "A".
def var v-lot-reo   as   char format "!" init "R".
def var v-prt-cpn   as   log format "Y/N" init no.

def var v-reord-qty as   int.
def var v-qty-avail as   int.
def var v-alloc-qty as   int.
def var v-rec-date  as   date.
def var v-coverage as dec no-undo.
def var v-mon-coverage as dec no-undo.
def var v-new-qty as cha  form "x(7)" init "_______"    no-undo. 
def var v-tot-cust-qty as int no-undo.
def var v-tot-annual-cons as int no-undo.
def var v-first-rec as log no-undo.
def var ls-selection as cha form "x(15)" no-undo.
def var ls-key-06 as cha no-undo.
if rd-tot-rel = "A" then ls-selection = "All".
else if rd-tot-rel = "T" then ls-selection = "Total Allocated".
else if rd-tot-rel = "R" then ls-selection = "Release Qty".
else ls-selection = "".

/*{sys/inc/sa-sls01.i}*/
   
FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   init-dir = users.user_program[2].
ELSE
   init-dir = "C:\Inetpub\wwwroot\pdfs\".

list-name = init-dir + "cstitm" + STRING(TIME) + ".txt".
  

  output to value(list-name) page-size 45.
  form header
     "***** Finished Goods - Reorder List *****        As of" today  
     "     Selection:" ls-selection                   
     "Page:" + string(page-num,">>9") form "x(10)" to 151 skip
     fill("=",159)  form "x(159)"   /* 126 for font9 (pitch 10) */
     with frame rpt-ttl width 159 no-box no-label stream-io.
view frame rpt-ttl.     
     
   assign cocode = prmComp
          v-cust[1] = begin_cust-no
          v-cust[2] = end_cust-no
          v-ship[1] = begin_shipto
          v-ship[2] = end_shipto
          v-item[1] = begin_i-no
          v-item[2] = end_i-no
          v-cat[1] = begin_cat
          v-cat[2] = end_cat
          v-lot-reo = rd-print-lot
          v-pur-man = rd-print
          v-inconh = t-include-order
          v-totrel = if rd-tot-rel = "T" or rd-tot-rel = "A" then yes else no
          v-date = ldt-as-of
          .
              
 if rd-tot-rel = "A" then do:
    for each itemfg
        where itemfg.company    eq cocode
          and itemfg.cust-no >= v-cust[1] 
          and itemfg.cust-no <= v-cust[2]
          and itemfg.i-no       ge v-item[1]
          and itemfg.i-no       le v-item[2]
          and itemfg.procat     ge v-cat[1]
          and itemfg.procat     le v-cat[2]
          and ((itemfg.ord-policy     and v-lot-reo eq "R") or
               (not itemfg.ord-policy and v-lot-reo eq "L") or v-lot-reo eq "A")
          and ((itemfg.pur-man        and v-pur-man eq "P") or
               (not itemfg.pur-man    and v-pur-man eq "M") or v-pur-man eq "A")
          use-index i-no no-lock:

         IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.
    
        assign  v-qty-avail = itemfg.q-onh + (if v-inconh then itemfg.q-ono else 0)
                v-alloc-qty = 0.

                

        if v-totrel then v-alloc-qty = itemfg.q-alloc.
        else
        for each oe-ordl  where oe-ordl.company eq cocode
                            and oe-ordl.i-no    eq itemfg.i-no
                 use-index item no-lock,
            each oe-rel  where oe-rel.company  eq cocode
                           and oe-rel.ord-no   eq oe-ordl.ord-no
                           and oe-rel.i-no     eq oe-ordl.i-no
                           and oe-rel.link-no  eq 0
                           and oe-rel.rel-date le v-date
                 use-index ord-item no-lock:
            v-alloc-qty = v-alloc-qty + oe-rel.qty.
        end.  
        v-qty-avail = v-qty-avail - v-alloc-qty.
        find first oe-ordl where oe-ordl.company = cocode and oe-ordl.i-no = itemfg.i-no no-lock no-error.
        if not avail oe-ordl then do:  /* no order - new item */
           assign v-tot-cust-qty = 0
                  v-tot-annual-cons = 0
                  v-first-rec = yes
                  .
           for each cust-itm where cust-itm.company = cocode
                            and cust-itm.cust-no = itemfg.cust-no
                            and cust-itm.i-no = itemfg.i-no
                            no-lock .            
               v-tot-cust-qty = v-tot-cust-qty + cust-itm.qty.
               if v-first-rec then v-tot-annual-cons = /*v-tot-annual-cons + */ cust-itm.consum.
               v-first-rec = no.
           end.
           /* ========================== */          
           create report.
           assign report.term-id = v-term
                 report.key-01  = itemfg.cust-no
                 report.key-02  = " "/*oe-rel.ship-id */
                 report.key-03  = itemfg.i-no
                 report.key-04  = string(v-alloc-qty,"-999999999999")
                 report.key-05  = string(v-qty-avail,"-999999999999")
                 report.key-06  = if t-use-cust-inv then string(v-tot-cust-qty,"-999999999999") else "0"
                 report.key-07  = string(v-tot-annual-cons,"-999999999999")
                 /*report.rec-id  = recid(oe-ordl)*/.
        
        end.
       /* =========== end  of no oe-ordl record======= */  
        else do:                   
        if itemfg.ord-level gt v-qty-avail then
        for each oe-ordl  where oe-ordl.company eq cocode
                            and oe-ordl.i-no    eq itemfg.i-no
                          no-lock,
            each oe-rel   where oe-rel.company eq cocode
                          and oe-rel.ord-no  eq oe-ordl.ord-no
                          and oe-rel.i-no    eq oe-ordl.i-no
                          and oe-rel.line    eq oe-ordl.line no-lock
            by oe-rel.rel-date desc
            by oe-rel.r-no     desc:
        
        if oe-rel.cust-no ge v-cust[1] and
           oe-rel.cust-no le v-cust[2] and
           oe-rel.ship-id ge v-ship[1] and
           oe-rel.ship-id le v-ship[2] then do:
           /* added logic for customer's inventory */

          assign v-tot-cust-qty = 0
                 v-tot-annual-cons = 0
                 v-first-rec = yes
                 .

          for each cust-itm where cust-itm.company = cocode
                            and cust-itm.cust-no = itemfg.cust-no
                         and cust-itm.i-no = itemfg.i-no
                         no-lock .            
             v-tot-cust-qty = v-tot-cust-qty + cust-itm.qty.
             if v-first-rec then v-tot-annual-cons = /*v-tot-annual-cons + */ cust-itm.consum.
             v-first-rec = no.
          end.
          /* ========================== */          
          create report.
          assign report.term-id = v-term
                 report.key-01  = itemfg.cust-no
                 report.key-02  = oe-rel.ship-id 
                 report.key-03  = itemfg.i-no
                 report.key-04  = string(v-alloc-qty,"-999999999999")
                 report.key-05  = string(v-qty-avail,"-999999999999")
                 report.key-06  = if t-use-cust-inv then string(v-tot-cust-qty,"-999999999999") else "0"
                 report.key-07  = string(v-tot-annual-cons,"-999999999999")
                 /*report.rec-id  = recid(oe-ordl)*/.
          leave. 
        end.  /* if */   
       end.   /* for each */
       end.  /* else  */
    end. /* each itemfg  for "A" */
    /* ========= display information ==========*/
    for each report where report.term-id eq v-term  ,
       
        first itemfg        where itemfg.company eq cocode
                   and itemfg.i-no    eq report.key-03   no-lock  
        break by report.key-01
              by report.key-02
              by report.key-03
        transaction:
        
        assign  v-reord-qty = itemfg.ord-level - int(report.key-05) 
                              - int(report.key-06)
                v-rec-date  = ?.
      
     
      v-coverage = ( int(report.key-05) + int(report.key-06) ) / 
                   ( int(report.key-07) / 12 ).  /* monthly , weekly : 52 */
      v-mon-coverage =  ( int(report.key-07) / 12 ).
                        
      if rd-tot-rel = "A" and v-reord-qty < 0 then v-reord-qty = 0. 
         /* display even if v-reord-qty < 0 when rd-tot-rel = "A" */
      ls-key-06 = if int(report.key-06) <> 0 then string(int(report.key-06),"->>>>>>9")  else "________". 
      if v-reord-qty >= 0 then              
      display itemfg.cust-no            column-label "Customer"
              /*oe-rel.ship-id            column-label "Ship!To"*/
              itemfg.i-no               column-label "Item"
             
              ls-key-06        column-label "Customer!Stock" /*Inventory*/
                                        format "x(8)" 
              itemfg.q-onh              column-label "On!Hand"
                                        format "->>>>>>9"

              v-mon-coverage            column-label "Monthly!Usage" form "->>>>9.99"                          
              v-coverage                column-label "Month of!Coverage"
                                        form "->>>>9.99"
              itemfg.ord-level          column-label "Reord!Level"  
                                        format ">>>>>9"
              itemfg.ord-min            column-label "Job!Qty"
                                        format ">>>>>>9"        
              v-new-qty              column-label "New!Qty"
          with down frame aaa no-box width 180 stream-io.
              delete report.  
    end.
 end. /* ===== end for all itemfg =====*/
 /* ==== reorder report ====*/
 else do:          
    for each itemfg
        where itemfg.company    eq cocode
          and itemfg.i-no       ge v-item[1]
          and itemfg.i-no       le v-item[2]
          and itemfg.procat     ge v-cat[1]
          and itemfg.procat     le v-cat[2]
          and ((itemfg.ord-policy     and v-lot-reo eq "R") or
               (not itemfg.ord-policy and v-lot-reo eq "L") or v-lot-reo eq "A")
          and ((itemfg.pur-man        and v-pur-man eq "P") or
               (not itemfg.pur-man    and v-pur-man eq "M") or v-pur-man eq "A")
        use-index i-no no-lock:
   
      assign
       v-qty-avail = itemfg.q-onh + (if v-inconh then itemfg.q-ono else 0)
       v-alloc-qty = 0.

      if v-totrel then v-alloc-qty = itemfg.q-alloc.

      else
      for each oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.i-no    eq itemfg.i-no
          use-index item no-lock,

          each oe-rel
          where oe-rel.company  eq cocode
            and oe-rel.ord-no   eq oe-ordl.ord-no
            and oe-rel.i-no     eq oe-ordl.i-no
            and oe-rel.link-no  eq 0
            and oe-rel.rel-date le v-date
          use-index ord-item no-lock:

        v-alloc-qty = v-alloc-qty + oe-rel.qty.
      end.
      
      v-qty-avail = v-qty-avail - v-alloc-qty.

      if itemfg.ord-level gt v-qty-avail then
      for each oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.i-no    eq itemfg.i-no
          no-lock,
            
          each oe-rel
          where oe-rel.company eq cocode
            and oe-rel.ord-no  eq oe-ordl.ord-no
            and oe-rel.i-no    eq oe-ordl.i-no
            and oe-rel.line    eq oe-ordl.line
          no-lock
          
          by oe-rel.rel-date desc
          by oe-rel.r-no     desc:
        
        if oe-rel.cust-no ge v-cust[1] and
           oe-rel.cust-no le v-cust[2] and
           oe-rel.ship-id ge v-ship[1] and
           oe-rel.ship-id le v-ship[2] then do:
        
           /* added logic for customer's inventory */
          assign v-tot-cust-qty = 0
                 v-tot-annual-cons = 0
                 v-first-rec = yes
                 .
          for each cust-itm where cust-itm.company = cocode
                            and cust-itm.cust-no = oe-rel.cust-no
                         and cust-itm.i-no = oe-rel.i-no
                         no-lock .            
             v-tot-cust-qty = v-tot-cust-qty + cust-itm.qty.
             if v-first-rec then 
             v-tot-annual-cons = /*v-tot-annual-cons + */ cust-itm.consum.
             v-first-rec = no.
          end.
          /* ========================== */          
          create report.
          assign report.term-id = v-term
                 report.key-01  = oe-rel.cust-no
                 report.key-02  = oe-rel.ship-id
                 report.key-03  = itemfg.i-no
                 report.key-04  = string(v-alloc-qty,"-999999999999")
                 report.key-05  = string(v-qty-avail,"-999999999999")
                 report.key-06  = if t-use-cust-inv then string(v-tot-cust-qty,"-999999999999") else "0"
                 report.key-07  = string(v-tot-annual-cons,"-999999999999")
                 report.rec-id  = recid(oe-rel).
           
          leave. 
        end.   
      end.
    end. /* each itemfg */
    
    /* ========= display information ==========*/
    for each report where report.term-id eq v-term,
        first oe-rel where recid(oe-rel) eq report.rec-id no-lock,
        first oe-ordl where oe-ordl.company eq cocode
                        and oe-ordl.ord-no  eq oe-rel.ord-no
                        and oe-ordl.i-no    eq oe-rel.i-no
                        and oe-ordl.line    eq oe-rel.line
                       no-lock,
        first itemfg        where itemfg.company eq cocode
                   and itemfg.i-no    eq oe-rel.i-no      no-lock  
        break by report.key-01
              by report.key-02
              by report.key-03
        transaction:
        
        assign  v-reord-qty = itemfg.ord-level - int(report.key-05) 
                              - int(report.key-06)
                v-rec-date  = ?.
      
      
      v-coverage = ( int(report.key-05) + int(report.key-06) ) / 
                   ( int(report.key-07) / 12 ).  /* month  weekly - 52 */
      v-mon-coverage =  ( int(report.key-07) / 12 ).             
      if rd-tot-rel = "A" and v-reord-qty < 0 then v-reord-qty = 0. 
         /* display even if v-reord-qty < 0 when rd-tot-rel = "A" */
      ls-key-06 = if int(report.key-06) <> 0 then string(int(report.key-06),"->>>>>>9")  else "________". 
      if v-reord-qty >= 0 then              
      display oe-rel.cust-no            column-label "Customer"
              oe-rel.ship-id            column-label "Ship!To"
              oe-rel.i-no               column-label "Item"
              oe-rel.po-no              column-label "Customer PO"
              oe-rel.ord-no             column-label "Order"
              oe-ordl.t-price / oe-ordl.qty
                                        column-label "Unit Pr"
                                        format ">>>9.99<<"
              ls-key-06        column-label "Customer!Stock"  /* Inventory*/   
                                        format "x(8)"
              itemfg.q-onh              column-label "On!Hand"
                                        format "->>>>>>9"
  
              v-mon-coverage            column-label "Monthly!Usage"
              v-coverage                column-label "Month of!Coverage"
                                        form "->>>>9.99"
 
              itemfg.ord-level          column-label "Reord!Level"  
                                        format ">>>>>9"
              itemfg.ord-min            column-label "Job!Qty"
                                        format ">>>>>>9"
              v-new-qty                 column-label "New!Qty" 
          with down no-box width 180 stream-io.
        
      delete report.  
    end.
  end. /* else */  
put "***** End of Report *****" at 2 .
output close.

/*  RUN custom/usrprint.p (v-prgmname,FRAME {&FRAME-NAME}:HANDLE).*/
  

END PROCEDURE.
