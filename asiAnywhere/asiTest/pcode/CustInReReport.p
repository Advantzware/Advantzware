/*------------------------------------------------------------------------
    File        : CustInReOrdRep.p
    Purpose     :  Print Customer Inventory reorder Report

    Syntax      :

    Description : Return a Dataset of Request For Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
{custom/xprint.i}
    
/*{sys/inc/var.i new shared}*/




DEFINE TEMP-TABLE ttInvCustReorder1 NO-UNDO
FIELD vCustFile AS CHAR
.
    DEFINE DATASET dsInvCustReorder1 FOR ttInvCustReorder1 .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAct            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust        AS CHARACTER  NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginItem      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginShip      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndShip        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCat       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndCat         AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmcustom         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prIncQoh          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmUse            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmAsDate         AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintPur       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintLot       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvCustReorder1.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser           = ?  THEN ASSIGN prmUser      = "".
     IF prmAct           = ?  THEN ASSIGN prmAct      = "".
    IF prmBeginCust      = ?  THEN ASSIGN prmBeginCust = "".
    IF prmEndCust        = ?  THEN ASSIGN prmEndCust   = "".
    IF prmBeginShip      = ?  THEN ASSIGN prmBeginShip = "".
    IF prmEndShip        = ?  THEN ASSIGN prmEndShip   = "".
    IF prmBeginItem      = ?  THEN ASSIGN prmBeginItem = "".
    IF prmEndItem        = ?  THEN ASSIGN prmEndItem   = "".
    IF prmBeginCat       = ?  THEN ASSIGN prmBeginCat  = "".
    IF prmEndCat         = ?  THEN ASSIGN prmEndCat    = "".


DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(5)"     NO-UNDO.
DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)"   NO-UNDO.
DEFINE VARIABLE begin_shipto AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(5)"       NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)"   NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)"     NO-UNDO.
DEFINE VARIABLE end_shipto AS CHARACTER FORMAT "X(30)"   NO-UNDO.
DEFINE VARIABLE ldt-as-of AS DATE FORMAT "99/99/9999"    NO-UNDO.
/*DEFINE VARIABLE rd-dest AS INTEGER                       NO-UNDO.*/
DEFINE VARIABLE rd-print-lot AS CHARACTER INITIAL "R"    NO-UNDO.
DEFINE VARIABLE rd-tot-rel AS CHARACTER INITIAL "T"      NO-UNDO.
DEFINE VARIABLE rd-print AS CHARACTER INITIAL "A"        NO-UNDO.
DEFINE VARIABLE t-include-order AS LOGICAL INITIAL yes   NO-UNDO.
DEFINE VARIABLE t-use-cust-inv AS LOGICAL INITIAL yes    NO-UNDO.
DEFINE VARIABLE cocode AS CHAR NO-UNDO.
DEFINE VARIABLE locode AS CHAR NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99     NO-UNDO.

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vTxtFile AS CHAR NO-UNDO.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=9 (13CPI)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)" INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5 NO-UNDO. 
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO. 
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no .
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO.
DEFINE VARIABLE v-term AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
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
    AND usercust.cust-no = prmBeginCust  NO-ERROR.
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

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

ASSIGN 
    cocode = prmComp.
    locode = usercomp.loc.

IF prmAct = "CustomerInv1" THEN DO:
assign
    cocode          = prmComp
    locode          = usercomp.loc
    begin_cust-no   = prmBeginCust 
    end_cust-no     = prmEndCust   
    begin_i-no      = prmBeginItem 
    end_i-no        = prmEndItem   
    begin_shipto    = prmBeginShip 
    end_shipto      = prmEndShip   
    begin_cat       = prmBeginCat  
    end_cat         = prmEndCat    
    t-use-cust-inv  = IF prmcustom = "Yes" THEN TRUE ELSE FALSE
    t-include-order = IF prIncQoh  = "Yes" THEN TRUE ELSE FALSE
    rd-tot-rel      = prmUse       
    ldt-as-of       = prmAsDate    
    rd-print        = prmPrintPur  
    rd-print-lot    = prmPrintLot  .


    assign
        init-dir    = v-webrootpath
        vTxtFile   = 'CustInvReod' + STRING(TIME) + '.txt' .

    
    
    run run-report.
    
    CREATE ttInvCustReorder1.
    ASSIGN ttInvCustReorder1.vCustFile = vTxtFile.
    END.   /*if prmAct="customerinv"*/
    /*****************************************************************************************************************************/
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

list-name = init-dir + "CustInvReod" +  STRING(TIME) + ".txt".

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
      /* =========== no need rec-date
      for each fg-bin
     =======================*/
     
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
              /*oe-rel.po-no              column-label "Customer PO" 
              oe-ordl.ord-no             column-label "Order"
              oe-ordl.t-price / oe-ordl.qty
                                        column-label "Unit Pr"
                                        format ">>>9.99<<"  */
              ls-key-06        column-label "Customer!Stock" /*Inventory*/
                                        format "x(8)" 
              itemfg.q-onh              column-label "On!Hand"
                                        format "->>>>>>9"
              itemfg.q-ono when v-inconh column-label "PO/Jobs!Qty Due" /*"On!Order" */
                                         format "->>>>>9"
              int(report.key-04)        column-label "OrderQty!Released"
                                        format "->>>>>>9"
              int(report.key-05) + int(report.key-06) column-label "Avail!able"
                                        format "->>>>>>9"
              v-mon-coverage            column-label "Monthly!Usage" form "->>>>9.99"                          
              v-coverage                column-label "Month of!Coverage"
                                        form "->>>>9.99"
              v-reord-qty               column-label "Qty!Short"
                                        format ">>>>>9"
              itemfg.ord-level          column-label "Reord!Level"  
                                        format ">>>>>9"
              itemfg.ord-min            column-label "Job!Qty"
                                        format ">>>>>>9"        
              v-new-qty              column-label "New!Qty"       
          with down frame aaa no-box width 180 stream-io.
              delete report.  
    end.
 end. /* ========== end for all itemfg ==========*/
 /* ========= reorder report =========*/
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
              itemfg.q-ono when v-inconh
                                        column-label "PO/Jobs!Qty Due"
                                        format "->>>>>9"
              int(report.key-04)        column-label "OrderQty!Released"
                                        format "->>>>>>9"
              int(report.key-05) + int(report.key-06) column-label "Avail!able"
                                        format "->>>>>>9"
              v-mon-coverage            column-label "Monthly!Usage"
              v-coverage                column-label "Month of!Coverage"
                                        form "->>>>9.99"
              v-reord-qty               column-label "Short!Qty"
                                        format ">>>>>9"
              itemfg.ord-level          column-label "Reord!Level"  
                                        format ">>>>>9"
              itemfg.ord-min            column-label "Job!Qty"
                                        format ">>>>>>9"
/*            v-rec-date                column-label "1st!Recpt"
                                        format "99/99/99"
              "N/A" when v-rec-date eq ? @ v-rec-date
              itemfg.ord-policy         column-label "R!L"
*/            
              v-new-qty                 column-label "New!Qty" 
          with down no-box width 180 stream-io.
        
      delete report.  
    end.
  end. /* else */  
put "***** End of Report *****" at 2 .
output close.

 
END PROCEDURE.
