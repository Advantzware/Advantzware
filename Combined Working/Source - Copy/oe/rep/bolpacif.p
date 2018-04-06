/* ---------------------------------------------- oe/rep/bolpacifp 10/02 YSK */
/* PRINT Pacific Pkg BOL                                                           */
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

def workfile w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def workfile w3 no-undo
    field ship-i           as   char format "x(60)".

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image1 = "images\action.jpg"
       ls-image2 = "images\pacific2.bmp".

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
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.


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
       v-comp-add3 = "Phone: 604.857.1660" 
       v-comp-add4 = "Fax     : 604.857.1665".
/* not working ???
FORM HEADER
   "<FArial>"
   "<C1><#1><R+5><C+25><IMAGE#1=" ls-full-img1 SKIP /* pacific package */ 
   "<=1>" SKIP
   "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
   "<P10><=2><R+4>"
   v-comp-add1 AT 8 SKIP
   v-comp-add2 AT 8  SKIP
   v-comp-add3 AT 8 SKIP
   v-comp-add4 AT 8 SKIP
   "www.pacificpackaging.ca" AT 8 SKIP(1)
         "<FCourier New>"
         "Sold To:" SPACE(30) "Ship To:"  SKIP
         SPACE(5) v-comp-name v-ship-name AT 45 skip
         SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
       SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
         SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP
  "<R4><C50><#3>" SKIP
  "<FArial><P14><=#3><C-20><R-2> <B>Bill Of Lading</B> " "<P10>" SKIP
          "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
          "<=#3><R+2>Date:" oe-bolh.bol-date        SKIP
           SKIP
           SKIP     
          "<|10><R19><C1><#4><FROM><R23><C82><RECT>" SKIP
          "<R21><C1><FROM><R21><C82><LINE>" SKIP    
          "<R19><C10><FROM><R23><C10><LINE>" SKIP
          "<R19><C29><FROM><R23><C29><LINE>" SKIP      
          "<R19><C49><FROM><R23><C49><LINE>" SKIP
          "<R19><C55><FROM><R23><C55><LINE>" SKIP
          "<R19><C72><FROM><R23><C72><LINE>" SKIP
          "<FArial><=4><R+1>  Date                  PO#                                   JOB#             FOB                  Carrier                                                 Freight Terms" SKIP
          "<FCourier New><=4><R+3> " oe-bolh.bol-date oe-bolh.po-no v-job-no v-fob carrier.dscr v-frt-terms SKIP
          "<|10><R24><C1><#5><FROM><R26><C82><RECT>" SKIP    
          "<R24><C15><FROM><R26><C15><LINE>" SKIP
          "<R24><C30><FROM><R26><C30><LINE>" SKIP
          "<R24><C55><FROM><R26><C55><LINE>" SKIP 
          "<R24><C63><FROM><R26><C63><LINE>" SKIP
          "<R24><C72><FROM><R26><C72><LINE>" SKIP            
      "<FArial><=5><R+1> Part#         Finished Good#    Description                                         Unit-Quantity           Partial/Complete            Weight" SKIP(1)
      "<FCourier New>"          
      with frame bol-top page-top no-box no-underline stream-io width 200.    
*/

      v-printline = 0.



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

    assign
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

     lv-tot-pg = 1.
     ln-cnt = 0.
     for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id,
         first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock
                break by report.key-01
                by report.key-02:
               /*========*/
         ln-cnt = ln-cnt + 5.  
         FIND first itemfg where itemfg.company eq oe-boll.company
                        and itemfg.i-no    eq oe-boll.i-no NO-LOCK NO-ERROR.

         if itemfg.isaset then
         for each fg-set where fg-set.company eq cocode
	                    and fg-set.set-no  eq itemfg.i-no   no-lock:

            find first xitemfg where xitemfg.company eq cocode
	                           and xitemfg.i-no    eq fg-set.part-no no-lock no-error.

            FIND FIRST fg-bin where fg-bin.company eq cocode
                            and fg-bin.i-no    eq xitemfg.i-no
                            and fg-bin.job-no = oe-boll.job-no
                            AND fg-bin.job-no2 = oe-boll.job-no2 NO-LOCK NO-ERROR.

            IF AVAIL fg-bin THEN
               ASSIGN lv-comp-unit = trunc((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) 
                    /*lv-comp-partial = fg-bin.qty - (lv-comp-unit * fg-bin.case-count)*/
                      .
            ELSE lv-comp-unit = 0.

            v-part-dscr = string(fg-set.part-no,"x(16)") +
		                  (if avail xitemfg then xitemfg.i-name else "").

            {sys/inc/part-qty.i v-part-qty fg-set}

            IF AVAIL fg-bin THEN DO:
               ln-cnt = ln-cnt + 1.
               IF fg-bin.partial-count <> 0 THEN ln-cnt = ln-cnt + 1.
            END.
            ELSE ln-cnt = ln-cnt + 1.
         END.
      END.
     /* end of dup loop */
      
      lv-tot-pg = IF (ln-cnt MOD 24) = 0 THEN TRUNC( ln-cnt / 24,0)   /* 13->36 24  detail lines */
                  ELSE lv-tot-pg + TRUNC( ln-cnt / 24,0).

      /*  end of getting total page per po */
/*
      put 
         "<FArial>"  SKIP
          "<P14><C+40><B>Bill Of Lading</B> " SKIP
          "<C1><#1><R+5><C+25><IMAGE#1=" ls-full-img1 SKIP /* pacific package */             
        /* "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
         */
         "<P10><=1><R+4>"  
         v-comp-add1 AT 8 SKIP
         v-comp-add2 AT 8  SKIP
         v-comp-add3 AT 8 SKIP
         v-comp-add4 AT 8 SKIP
         "www.pacificpackaging.ca" AT 8 SKIP(1)
               "<FCourier New>"
               "Sold To:" SPACE(30) "Ship To:"  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B>*/ "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" 
                string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(15)" AT 65 
                SKIP(1)
                "<=#3><R+2>Date:" oe-bolh.bol-date        SKIP
                "<=#3><R+3>" /*Ship Date:" oe-bolh.ship-date        */ SKIP
                 SKIP     
                "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
                "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP
                "<R19><C46><FROM><R23><C46><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                "<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-po-no FORM "x(15)" space(2) v-job-no v-fob space(3) carrier.dscr v-frt-terms SKIP
                "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C27><FROM><R26><C27><LINE>" SKIP
                "<R24><C49><FROM><R26><C49><LINE>" SKIP 
                "<R24><C62><FROM><R26><C62><LINE>" SKIP
                "<R24><C73><FROM><R26><C73><LINE>" SKIP            
            "<FArial><=5><R+1> Part#                        Finished Good#           Description                                        Unit-Quantity           Partial/Complete    Weight" SKIP(1)
            "<FCourier New>"                                  
            .
            v-printline = v-printline + 16.
 */

    {oe/rep/bolpaci1.i "stream last-page"}
 
    {oe/rep/bolpacif.i}

    v-last-page = page-number.


    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
      delete report.
    end.
  end.

  PUT "<R54><C50><#8><FROM><R+4><C+30><RECT> " 
    "<=8><R+1> Total Pallets/Bags :" v-tot-cases 
    "<=8><R+3> Total Weight:       " v-tot-wt FORM ">>,>>9.99".
    

PUT "<FArial><R52><C1><P12><B>     Shipping Instructions: </B> <P9> "
    "<R54><C4>" oe-bolh.ship-i[1] 
    "<R55><C4>" oe-bolh.ship-i[2] 
    "<R56><C4>" oe-bolh.ship-i[3] 
    "<R57><C4>" oe-bolh.ship-i[4]
    "<R59><C1>" "__________________________________________________________________________________________________________________" 
    "<R60><C1>" "<B>  Signature of Receipt </B>" SKIP
    "<R61><C8>" "Customer ________________________________________                       Carrier _______________________________________" 
    "<R62><C8>" "Date ____________________________________________                       Date _________________________________________" 
    .

  v-printline = v-printline + 14.
  IF last-of(oe-bolh.bol-no) THEN lv-pg-num = PAGE-NUM .

  /*IF v-printline < 45 THEN PUT SKIP(60 - v-printline).*/
  

  v-printline = 0.
  oe-bolh.printed = yes.
  PAGE.

end. /* for each oe-bolh */


/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

