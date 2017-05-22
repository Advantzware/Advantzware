/* ---------------------------------------------- oe/rep/bolcentx 10/02 YSK */
/* PRINT Century Box                                                         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)" NO-UNDO.
def var v-fob               as   char format "x(12)" NO-UNDO.
def var v-tot-cases         as   int format ">>>>9" NO-UNDO.
def var v-tot-wt            as   dec format "->,>>>,>>9" NO-UNDO.

def var v-tot-pkgs          as   int format ">>9" NO-UNDO.
def var v-ord-qty           like oe-ordl.qty NO-UNDO.
def var v-bol-qty           like oe-boll.qty NO-UNDO.
def var v-ship-qty          like oe-ordl.ship-qty NO-UNDO.
def var v-bol-wt            as   dec NO-UNDO.
def var v-part-dscr         as   char format "x(30)" NO-UNDO.
def var v-part-comp         as   char format "x" NO-UNDO.
def var v-part-qty          as   DEC NO-UNDO.
def var v-ord-no            like oe-boll.ord-no NO-UNDO.
def var v-po-no             like oe-bolh.po-no NO-UNDO.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.

def var v-ship-name  like shipto.ship-name NO-UNDO.
def var v-ship-addr  like shipto.ship-addr NO-UNDO.
def var v-ship-city  like shipto.ship-city NO-UNDO.
def var v-ship-state like shipto.ship-state NO-UNDO.
def var v-ship-zip   like shipto.ship-zip NO-UNDO.
def var v-ship-addr3 as   char format "x(30)" NO-UNDO.
def var v-comp-name  like company.name NO-UNDO.
def var v-comp-addr  like company.addr NO-UNDO.
def var v-comp-city  like company.city NO-UNDO.
def var v-comp-state like company.state NO-UNDO.
def var v-comp-zip   like company.zip NO-UNDO.
def var v-comp-addr3 as   char format "x(30)" NO-UNDO.
def var v-cust-addr3 as   char format "x(30)" NO-UNDO.
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.
DEF VAR lv-cases LIKE oe-boll.cases NO-UNDO.

def var v-terms like oe-ord.terms-d NO-UNDO.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-pal AS INT NO-UNDO.

def workfile w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def workfile w3 no-undo
    field ship-i           as   char format "x(60)".

DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(50)" NO-UNDO.
ASSIGN ls-image1 = "" /*"images\cbxsale.jpg".*/.

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR lv-comp-unit AS INT NO-UNDO.
DEF VAR lv-cases-tot AS INT NO-UNDO.
DEF VAR lv-qty-tot AS INT NO-UNDO.
DEF VAR lv-qcase-tot AS INT NO-UNDO.
DEF VAR lv-partial-tot AS INT NO-UNDO.
DEF VAR lv-pal-tot AS INT NO-UNDO.
DEF VAR v-unit-qty AS cha NO-UNDO.
DEF VAR lv-prt-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR lv-prt-time AS INT NO-UNDO.
DEF VAR lv-prt-sts AS cha NO-UNDO.
DEF VAR lv-ord-type-code AS cha INIT ["C,N,O,Q,R,T,X"] NO-UNDO.   
DEF VAR lv-ord-type-list AS cha INIT 
    ["Change,New Customer,Original,Quality/Re-work,Repeat,Transfer,Complete Re-run"] NO-UNDO. 

assign tmpstore = fill("-",80).

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
{sa/sa-sls01.i}
/*
output stream last-page to value(tmp-dir + "bolempir.txt") page-size 62.
*/
find first company where company.company eq cocode no-lock.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
ASSIGN v-comp-add1 = company.addr[1]
       v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
       v-comp-add3 = "Phone: 604.533.2545" 
       v-comp-add4 = "Fax  : 604.533.2633"
       v-printline = 0
       lv-prt-date = TODAY
       lv-prt-time = TIME
       .
       

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

    break by oe-bolh.bol-no:
      
    if first-of(oe-bolh.bol-no) then do:
    find first carrier
        where carrier.company eq oe-bolh.company
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    ASSIGN
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-phone-num    = cust.area-code + cust.phone.
     
    /*if shipto.broker then
      assign
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.
    
    else
      assign
       v-comp-name    = company.name
       v-comp-addr[1] = company.addr[1]
       v-comp-addr[2] = company.addr[2]
       v-comp-addr3   = company.city + ", " +
                        company.state + "  " +
                        company.zip.
    */                    
    assign
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.

    if trim(v-comp-addr3) eq "," then v-comp-addr3 = "".
              
    if v-comp-addr[2] eq "" then
      assign
       v-comp-addr[2] = v-comp-addr3
       v-comp-addr3   = "".
    if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    assign
     v-salesman = ""
     v-fob      = ""
     v-terms    = "".


    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      if not available carrier then
      find first carrier where carrier.company = oe-ord.company
        and carrier.carrier = oe-ord.carrier no-lock no-error.

      do i = 1 to 3:
        if oe-ord.sman[i] ne "" then
          v-salesman = trim(v-salesman) + " " + oe-ord.sman[i] + ",".
      end.

/*    if can-do("COD,CIA", oe-ord.terms) then v-terms = oe-ord.terms-d. */
      assign v-terms = oe-ord.terms-d
             v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"
                           else if cust.frt-pay eq "B" then "Bill"
                           else if cust.frt-pay eq "C" then "Collect"
                           else if cust.frt-pay eq "T" then "Third Party"
                           else ""
             v-zone = cust.del-zone.
             
      if v-terms eq "" then
      do:
        find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
        if avail terms then
          assign v-terms = terms.dscr.
      end.
      
      v-salesman = trim(v-salesman).
      v-po-no = oe-boll.po-no.
      v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).
      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
      
      LEAVE.
    end.

    for each w3:
      delete w3.
    end.
  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.
  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.i-no
     report.key-02   = string(oe-boll.ord-no,"9999999999")
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes.
  end.
  if last-of(oe-bolh.bol-no) then do:
     IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".

     /* duplicate loop for total freight */
     v-tot-wt = 0.
     lv-tot-pg = 1.
     ln-cnt = 0.
     for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id,
         first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
         first itemfg where itemfg.company eq oe-boll.company
                      and itemfg.i-no    eq oe-boll.i-no no-lock
                break by report.key-01
                by report.key-02:
         ASSIGN v-tot-wt = v-tot-wt + oe-boll.weight.
   
         if oe-boll.weight eq 0 then
             v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).

          /*========*/
          ln-cnt = ln-cnt + 2.
          find first oe-ordl where oe-ordl.company eq cocode
                          and oe-ordl.ord-no  eq oe-boll.ord-no
                          and oe-ordl.i-no    eq oe-boll.i-no no-lock no-error.

          IF oe-ordl.part-dscr1 <> "" OR oe-boll.partial > 0 THEN ln-cnt = ln-cnt + 1.
          if itemfg.isaset then
          for each fg-set where fg-set.company eq cocode
	                       and fg-set.set-no  eq itemfg.i-no   no-lock:

             find first xitemfg where xitemfg.company eq cocode
	                           and xitemfg.i-no    eq fg-set.part-no no-lock no-error.

             FIND FIRST fg-bin where fg-bin.company eq cocode
                            and fg-bin.i-no    eq xitemfg.i-no
                            and fg-bin.job-no = oe-boll.job-no
                            AND fg-bin.job-no2 = oe-boll.job-no2 NO-LOCK NO-ERROR.
             ln-cnt = ln-cnt + 1.
             IF AVAIL fg-bin AND fg-bin.partial-count <> 0 THEN ln-cnt = ln-cnt + 1.
          END.
     END.
     /* end of dup loop */
      lv-tot-pg = IF (ln-cnt MOD 18) = 0 THEN TRUNC( ln-cnt / 18,0)
                  ELSE lv-tot-pg + TRUNC( ln-cnt / 18,0) .  /* 16->33 18 detail lines */
      /*  end of getting total page per po */

     lv-prt-sts = xxreport.key-09.

     {oe/rep/bolcent1.i}

    /*
    {oe/rep/bolempir.i "stream last-page"}

    v-page-tot = page-number (last-page) - v-last-page.
    */
    {oe/rep/bolcentx.i}

    v-last-page = page-number.


    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
      delete report.
    end.
  end.

  PUT "<R39><C50><#7>CBX Intial"
      "<=7><C+10><FROM><R+2><C+20><RECT> " 
      "<R41><C50><#8><FROM><R+3><C+30><RECT> " 
      "<=8><R+1> Total Pallets      :" oe-bolh.tot-pallets /*v-tot-cases*/ FORM ">,>>>,>>9"
    /*  "<=8><R+3> Total Weight:       " v-tot-wt FORM ">>,>>9.99"*/ .
    


PUT "<FBook Antiqua><R39><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP(1)  
    oe-bolh.ship-i[1] AT 7 SKIP
    oe-bolh.ship-i[2] AT 7 SKIP
    oe-bolh.ship-i[3] AT 7 SKIP
    oe-bolh.ship-i[4] AT 7 SKIP 
    "_____________________________________________________________________________________________________________________________" SKIP
    "<B>  Signature of Receipt </B>" SKIP
    "Customer ________________________________________                       Carrier _______________________________________" AT 23 SKIP(1)
    "Date ____________________________________________                       Date __________________________________________" AT 23 SKIP   
    "<C1>" lv-prt-date "  " STRING(lv-prt-time,"HH:MM AM") "  " lv-prt-sts
    "Page " AT 202 string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" SKIP
    "<R51><C1><P6>RECEIVED, SUBJECT TO THE CLASSIFCATION AND LAWFULLY FILED TARIFFS IN EFFECT ON THE DATE OF THIS Bill of Lading. The property described above, except as noted, marked or consigned and" 
    "<R51.6><C1>destined as indicated below, which said carrier (the word carrier being understood through this contract as meaning any person or corporation in possession of the property under the contract) agrees to carry to" SKIP
    "<R52.2><C1>its usual place of delivery at said destination. Its is mutually agreed, as to each carrier of all or any property over all or any portion of said route to destination, as to each party at any time interested" SKIP
    "<R52.8><C1>in all or any of said property, that every service be performed hereunder shall be subject to all the terms and conditions of the Uniform Domestic Straight Bill of Lading set forth (1) in Uniform Freight Classification" SKIP
    "<R53.4><C1>in effect of the date hereof, if this is rail or water shipment or (2) in the applicable motor carrier classification or tariff if this is a motor shipment. Shipper/Receiver hereby certifies the he/she is familiar" SKIP
    "<R54.0><C1>with all the terms and conditions of the said bill of lading, set forth in the classification or tariff which governs the transportation of this shipment, and the said terms and conditions are herby agreed to" SKIP
    "<R54.6><C1>by the shipper/receiver and accepted for himself/herself and his assigns." 
    .



  v-printline = v-printline + 14.
  IF last-of(oe-bolh.bol-no) THEN lv-pg-num = PAGE-NUM .

  IF v-printline < 45 THEN PUT SKIP(60 - v-printline).
  v-printline = 0.
  oe-bolh.printed = yes.
  
  

end. /* for each oe-bolh */


/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

PROCEDURE get-pallets-num:

DEF OUTPUT PARAM op-pallets AS DEC NO-UNDO.

DEF VAR v-qty-pal AS dec NO-UNDO.
DEF VAR v-int AS DEC NO-UNDO.
/*
FIND FIRST loadtag
    WHERE loadtag.company EQ cocode
      AND loadtag.tag-no  EQ oe-boll.tag
    NO-LOCK NO-ERROR.  

IF AVAIL loadtag THEN
  v-qty-pal = /*(if loadtag.qty-case   eq 0 then 1 else loadtag.qty-case)   *
               (if loadtag.case-bundle  eq 0 then 1 else loadtag.case-bundle)   * */
               (IF loadtag.pallet-count EQ 0 THEN 1 ELSE loadtag.pallet-count).

ELSE DO:
*/

  FIND FIRST fg-bin
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ oe-boll.i-no
        AND fg-bin.job-no  EQ oe-boll.job-no
        AND fg-bin.job-no2 EQ oe-boll.job-no2
        AND fg-bin.loc     EQ oe-boll.loc
        AND fg-bin.loc-bin EQ oe-boll.loc-bin
        AND fg-bin.tag     EQ oe-boll.tag
      NO-LOCK NO-ERROR.  
  v-qty-pal = 1.
  IF AVAIL fg-bin THEN
    v-qty-pal = /*(IF fg-bin.case-count   EQ 0 THEN 1 ELSE fg-bin.case-count)   *
                (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   *
                (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet).
                */
            v-qty-pal                                                     *
            (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   *
            (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet).

  IF v-qty-pal LE 1 THEN DO:
    v-int = INT(oe-boll.partial NE 0).
    IF oe-boll.qty LT 0 THEN v-int = v-int * -1.
  END.
  ELSE v-int = 0.

  v-qty-pal = (oe-boll.cases + v-int) / v-qty-pal.

/* 
op-pallets = (IF AVAIL fg-bin AND fg-bin.cases-unit > 1 THEN oe-boll.qty - oe-boll.partial
              ELSE oe-boll.qty) / v-qty-pal.

IF op-pallets = ? THEN op-pallets = 1.
*/

{sys/inc/roundup.i v-qty-pal}

IF v-qty-pal LT 0 THEN v-qty-pal = v-qty-pal * -1.
op-pallets = v-qty-pal.
END.
