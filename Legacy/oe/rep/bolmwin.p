/* ---------------------------------------------- oe/rep/bolwin.p 01/05 YSK */
/* PRINT Indiana Carton                                                       */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.
                     
/*{oe/rep/oe-lad.i}*/
DEF  VAR v-print-components AS LOG NO-UNDO.
def var v-last-page         as   int.
def var v-page-tot          as   dec format ">>9".
DEFINE  SHARED VARIABLE v-print-fmt  AS CHARACTER NO-UNDO.
DEFINE  SHARED VARIABLE LvOutputSelection AS CHARACTER NO-UNDO.
DEFINE  SHARED VAR v-term-id as char no-undo.
DEF TEMP-TABLE tt-boll LIKE oe-boll
    FIELD rec_id AS CHAR.

DEF BUFFER b-tt-boll FOR tt-boll.

def var v-salesman          as   char format "x(26)" NO-UNDO.
def var v-fob               as   char format "x(12)" NO-UNDO.
def var v-tot-cases         as   int format ">>>>9" NO-UNDO.
def var v-tot-wt            as   dec format ">>,>>>,>>9" NO-UNDO.

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
DEF VAR v-tot-palls AS INT FORM ">>>9" NO-UNDO.
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
DEF VAR ll-consol-bolls AS LOG NO-UNDO.
DEF VAR v-ship-i AS cha EXTENT 4 FORM "x(60)" NO-UNDO.
def var v-terms like oe-ord.terms-d NO-UNDO.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-pal AS INT NO-UNDO.
DEF VAR v-wgt-70 AS DEC NO-UNDO.
DEF VAR v-wgt-100 AS DEC NO-UNDO.

def workfile w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def workfile w3 no-undo
    field ship-i           as   char format "x(60)".
DEF TEMP-TABLE tt-boll2 FIELD rec-id AS RECID
                        FIELD pallets AS INT.

DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEF VAR v-frt-class AS cha NO-UNDO.
DEF VAR lv-class AS cha NO-UNDO.
DEF VAR v-prepaid AS cha NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
 
find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLFMT"
      no-lock no-error.
ll-consol-bolls = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0.
ll-consol-bolls = NO .

RUN sys/ref/nk1look.p (INPUT cocode, "BOLImageFooter", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    ls-image1 = cRtnChar NO-ERROR. 
                   

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
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
DEF VAR iBolno  LIKE OE-BOLH.BOL-NO NO-UNDO.
DEF VAR dBoldate LIKE OE-BOLH.bol-date NO-UNDO.
def var iTotBolQty    like oe-boll.qty NO-UNDO.
DEFINE VARIABLE cProCat AS CHARACTER NO-UNDO .
assign tmpstore = fill("-",80).

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
{sa/sa-sls01.i}

find first company where company.company eq cocode no-lock.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
ASSIGN v-comp-add1 = company.addr[1]
       v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
       v-comp-add3 = "Phone: 604.533.2545" 
       v-comp-add4 = "Fax  : 604.533.2633".
v-printline = 0.
j = 0 .

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    NO-LOCK
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

    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-phone-num    = cust.area-code + cust.phone.
     
            
    assign
       v-comp-name    = company.name
       v-comp-addr[1] = company.addr[1]
       v-comp-addr[2] = company.addr[2]
       v-comp-addr3   = company.city + ", " +
                        company.state + "  " +
                        company.zip.

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

   
    for each w3:
      delete w3.
    end.
    FOR EACH tt-boll:
      DELETE tt-boll.
      FIND FIRST tt-boll2 WHERE tt-boll2.rec-id = RECID(tt-boll) NO-ERROR.
      IF AVAIL tt-boll2 THEN DELETE tt-boll2.
    END.

  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.
  v-frt-class  = "".
  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    /*create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.i-no
     report.key-02   = string(oe-boll.ord-no,"9999999999")
     report.rec-id   = recid(oe-boll) */
      IF v-frt-class = "" THEN do:
         FIND FIRST itemfg WHERE itemfg.company = oe-bolh.company
                             AND itemfg.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
         v-frt-class = IF AVAIL itemfg AND itemfg.frt-class <> "" THEN itemfg.frt-class 
                       ELSE "A".
                      
      END.
      IF ll-consol-bolls THEN DO:
         IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
            RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).

         IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
            RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).
      END.
      ELSE DO:
        CREATE tt-boll.
        BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
        CREATE tt-boll2.
        RUN get-pallets-num ( OUTPUT v-pal). 
        tt-boll2.rec-id = RECID(tt-boll).
        tt-boll2.pallets = v-pal.
      END.
               
      /*oe-boll.printed = yes.*/
  end.

  IF FIRST(oe-bolh.bol-no) THEN do: 
      ASSIGN iBolno = oe-bolh.bol-no
             dBoldate = oe-bolh.bol-date
             v-ship-i[1] = oe-bolh.ship-i[1]
             v-ship-i[2] = oe-bolh.ship-i[2]
             v-ship-i[3] = oe-bolh.ship-i[3]
             v-ship-i[4] = oe-bolh.ship-i[4] .

      FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord where oe-ord.company eq oe-boll.company
	                  and oe-ord.ord-no  eq oe-boll.ord-no NO-LOCK:

      if not available carrier then
      find first carrier where carrier.company = oe-ord.company
        and carrier.carrier = oe-ord.carrier no-lock no-error.

      do i = 1 to 3:
        if oe-ord.sman[i] ne "" then
          v-salesman = trim(v-salesman) + " " + oe-ord.sman[i] + ",".
      end.

/*    if can-do("COD,CIA", oe-ord.terms) then v-terms = oe-ord.terms-d. */
      assign v-terms = oe-ord.terms-d
             v-zone = cust.del-zone.
             
      if v-terms eq "" then
      do:
        find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
        if avail terms then
          assign v-terms = terms.dscr.
      end.
      
      v-salesman = trim(v-salesman).
      v-po-no = oe-boll.po-no.
      v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no ).
      v-ord-no = oe-boll.ord-no.
      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
      FIND FIRST job NO-LOCK 
          WHERE job.company EQ cocode 
            AND job.job-no  EQ v-job-no
            AND job.job-no NE "" NO-ERROR .
      IF AVAIL job THEN
          FIND FIRST eb NO-LOCK 
            WHERE eb.company EQ cocode 
              AND eb.est-no EQ job.est-no 
              AND eb.stock-no EQ oe-boll.i-no NO-ERROR .
      IF AVAIL eb THEN
          ASSIGN cProCat = eb.procat .
      
      LEAVE.
    end.

    v-frt-terms = if oe-bolh.frt-pay eq "P" then "Prepaid"
                  else if oe-bolh.frt-pay eq "B" then "Bill"
                  else if oe-bolh.frt-pay eq "C" then "Collect"
                  else if oe-bolh.frt-pay eq "T" then "Third Party"
                  else "".
    v-prepaid = IF INDEX("BP",oe-bolh.frt-pay) > 0 THEN "To be Prepaid" ELSE "".


      {oe/rep/bolmwin1.i} 
  END.

  if last-of(oe-bolh.bol-no) then do:
     IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".

     /* duplicate loop for total freight */
     /*v-tot-wt = 0.*/
     lv-tot-pg = 1.
     ln-cnt = 0.
     
     FOR EACH tt-boll,
         first xoe-bolh where xoe-bolh.b-no eq tt-boll.b-no no-lock,
         first itemfg where itemfg.company eq tt-boll.company
                      and itemfg.i-no    eq tt-boll.i-no no-lock
         BY tt-boll.i-no
         BY tt-boll.po-no
         BY tt-boll.ord-no
         BY tt-boll.line
         BY tt-boll.cases DESC:                 
         ASSIGN v-tot-wt = v-tot-wt + tt-boll.weight.
         IF itemfg.frt-class = "100" THEN v-wgt-100 = v-wgt-100 + tt-boll.weight.
         ELSE v-wgt-70 = v-wgt-70 + tt-boll.weight.

         ln-cnt = ln-cnt + 3.
         if itemfg.isaset AND v-print-components then
            for each fg-set where fg-set.company eq cocode
	                    and fg-set.set-no  eq itemfg.i-no   no-lock:

                find first xitemfg where xitemfg.company eq cocode
	                           and xitemfg.i-no    eq fg-set.part-no no-lock no-error.
                FIND FIRST fg-bin where fg-bin.company eq cocode
                            and fg-bin.i-no    eq xitemfg.i-no
                            and fg-bin.job-no = tt-boll.job-no
                            AND fg-bin.job-no2 = tt-boll.job-no2 NO-LOCK NO-ERROR.

                IF AVAIL fg-bin THEN DO:
                   ln-cnt = ln-cnt + 1.
                   IF fg-bin.partial-count <> 0 THEN ln-cnt = ln-cnt + 1.
                END.
                ELSE ln-cnt = ln-cnt + 1.
         END.
     END.
     /* end of dup loop */
      lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 17,0) .  /* 15->31 17 po detail lines */
      /*  end of getting total page per po */
     
     /*{oe/rep/bolmwin1.i}*/
     {oe/rep/bolmwin2.i}
     
     v-last-page = page-number.

   
  end.
  
  IF LAST(oe-bolh.bol-no) THEN do:


   PUT "<=6><R41.1><C5><B><p6> Grand Total <C41>" v-tot-palls "<C47>" v-tot-wt "<C53>" iTotBolQty .

 PUT "<#9><C4><R43><from><R48><C81><RECT><|3>" SKIP. 
        PUT "<R44><C4><FROM><R44><C81><LINE>" SKIP.
        PUT "<R46><C4><FROM><R46><C81><LINE>" SKIP.
        PUT "<R47><C4><FROM><R47><C81><LINE>" SKIP.

        PUT "<R44><C10><FROM><R48><C10><LINE>" SKIP.
        PUT "<R44><C16><FROM><R48><C16><LINE>" SKIP.
        PUT "<R44><C22><FROM><R48><C22><LINE>" SKIP.
        PUT "<R43><C40><FROM><R48><C40><LINE>" SKIP.
        PUT "<R44><C45><FROM><R48><C45><LINE>" SKIP.
        PUT "<R44><C49><FROM><R48><C49><LINE>" SKIP.
        PUT "<R43><C70><FROM><R48><C70><LINE>" SKIP.
        PUT "<R44><C76><FROM><R48><C76><LINE>" SKIP.

        PUT "<=9><R43.2><C10><B><p7> Handling Unit              Package" "<C73> LTL Only </B>" SKIP.
        PUT "<=9><R44.2><C6><B><p6>Qty " "<C11> Type" "<C16> Pallet Qty" "<C29> Type" "<C40> Weight" "<C45> HM(X)"
            "<C71> NMFC" "<C77> Class</B>"
            "<=9><R45><C72><B><p6>No. </B>"
            "<=9><R44><C50><p4>Commodity Description:Commodities requiring special or  " 
            "<=9><R44.5><C50><p4>additional care or attention in handling or stowing must "
            "<=9><R45><C50><p4>be so marked and packaged as to ensure safe transportation  "
            "<=9><R45.5><C50><p4>with ordinary care. See Section 2(e) of NMFC item 360  "

            "<=9><R46.2><p7>" "<C3>" iTotBolQty "<C11>" cProCat FORMAT "x(10)" "<C16>" v-tot-palls "<C23>Pallets"  "<C38>" v-tot-wt "<C50>Printed Matter "
            .
 
        PUT "<#10><R48.5><C4><P4>Where the rate is dependent on value, shippers are required to state specifically in writing the agreed or declared value of the property as follows: The agreed OR declared value " SKIP
            "<R49><C4><P4>of the property is specifically stated by the shipper to be not exceeding__________________per______________________.<P6>" SKIP
            .
        
        PUT "<R50><C4><FROM><R50><C81><LINE>" SKIP.
        PUT "<C5><R-1><P7><b>Notes: Liability limitation for loss or damage in this Shipment may be applicable. See 49 USC $ 14706(C)(1)(A) and (B).</B><P6>" SKIP .
   
        PUT "<#11><C4><R51><from><R59.7><C81><RECT><|3>" SKIP. 
            PUT "<R54><C4><FROM><R54><C81><LINE>" SKIP.
            PUT "<R57><C4><FROM><R57><C30><LINE>" SKIP.
            PUT "<R57><C53.5><FROM><R57><C81><LINE>" SKIP.

            PUT "<R51><C55><FROM><R54><C55><LINE>" SKIP. 
            PUT "<R54><C30><FROM><R59.7><C30><LINE>" SKIP.
            PUT "<R54><C42><FROM><R59.7><C42><LINE>" SKIP.
            PUT "<R54><C53.5><FROM><R59.7><C53.5><LINE>" SKIP.

            PUT "<=11><R51.3><C4><p5> Received. Subject to individually determined rates or contracts that have been agreed upon in writing between the carrier"
                "<=11><R51.9><C4><p5> and shipper, if applicable, otherwise to the rates, classifications, and rules that have been established by the carrier " SKIP 
                "<=11><R52.5><C4><p5> and are available to the shipper, on request, and to all applicable state and federal regulations."
                "<=11><R51.5><C58><B><p5>The carrier shall not make delivery of this shipment "
                "<=11><R52.2><C58><B><p5>without payment of charges and all other lawful fees. "
                "<=11><R53.2><C55><B><p7> Shipper Signature__________________________</B> " 

                "<=11><R54.5><C32><B><p7>Trailer Loaded <C43> Freight Counted "      
                "<=11><R54.6><C6><B><p7>Shipper Signature <C60> Carrier Signature/Date </B>" 
                
                "<=11><R55.6><C30><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By Shipper   "
                "<=11><R56.8><C30><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By driver/pieces   "
                "<=11><R55.6><C42><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By Shipper   "
                "<=11><R56.6><C42><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By driver/pallets" SKIP "<R57.1><C44>  said to contain   "
                "<=11><R57.8><C42><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By driver    "

                "<=11><R57.5><C4><p4> This is to certify that the above named materials are properly classified, " 
                "<=11><R58><C4><p4>     packaged, marked, and labeled are in proper condition for "
                "<=11><R58.5><C4><p4>     transportion according to the applicable regulations of the DOT. "

                "<=11><R57.5><C54><p4>Carrier acknowledges receipt of packages and required placards. Carrier certifies" 
                "<=11><R58><C54><p4>emergency response information was made available and/or carrier has the DOT "
                "<=11><R58.5><C54><p4>emergency response guidebook or equivalent documentation in the vehicle, property "
                 "<=11><R59><C54><p4>            described above is received in good order,except as noted. "
                "<R60><C18><#1><R+6><C+52><IMAGE#1=" ls-full-img1  SKIP
               .


  v-printline = v-printline + 14.
  
  PAGE.
  v-printline = 0.
  END.
  
  
  

end. /* for each oe-bolh */


/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

PROCEDURE get-pallets-num:
DEF OUTPUT PARAM op-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS dec NO-UNDO.

find first fg-bin
    where fg-bin.company eq cocode
      and fg-bin.i-no    eq oe-boll.i-no
      and fg-bin.job-no  eq oe-boll.job-no
      and fg-bin.job-no2 eq oe-boll.job-no2
      and fg-bin.loc     eq oe-boll.loc
      and fg-bin.loc-bin eq oe-boll.loc-bin
      and fg-bin.tag     eq oe-boll.tag
    no-lock no-error.  



if avail fg-bin then
  assign
   v-qty-pal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
   op-pallets = (IF AVAIL fg-bin AND fg-bin.cases-unit > 1 THEN oe-boll.qty - oe-boll.partial
                 ELSE oe-boll.qty) / v-qty-pal.

else
  assign
   op-pallets = 1.

IF op-pallets = ? THEN op-pallets = 1.

{sys/inc/roundup.i op-pallets}

if op-pallets lt 0 then op-pallets = op-pallets * -1.

END.


PROCEDURE create-tt-boll.
  DEF INPUT PARAM ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-boll.cases NO-UNDO.


  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-boll
      WHERE tt-boll.i-no     EQ oe-boll.i-no
        AND tt-boll.po-no    EQ oe-boll.po-no
        AND tt-boll.ord-no   EQ oe-boll.ord-no
        AND tt-boll.line     EQ oe-boll.line
        AND tt-boll.qty-case EQ ip-qty-case
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-boll THEN DO:
    CREATE tt-boll.
    BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
    CREATE tt-boll2.
    tt-boll2.rec-id = RECID(tt-boll).
    ASSIGN
     tt-boll.qty-case = ip-qty-case
     tt-boll.cases    = 0
     tt-boll.qty      = 0
     tt-boll.weight   = 0
     tt-boll.partial  = 0
     tt-boll2.pallets = 0.
  END.
  RUN get-pallets-num ( OUTPUT v-pal). 
  FIND FIRST tt-boll2 WHERE tt-boll2.rec-id = RECID(tt-boll).
  ASSIGN
   tt-boll.cases  = tt-boll.cases + ip-cases
   tt-boll.qty    = tt-boll.qty + (ip-qty-case * ip-cases)
   tt-boll.weight = tt-boll.weight + 
                    ((ip-qty-case * ip-cases) / oe-boll.qty * oe-boll.weight)
   tt-boll2.pallets = tt-boll2.pallets + v-pal   .

  IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.
