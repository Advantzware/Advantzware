/* ---------------------------------------------- oe/rep/bolxprt2.p 10/02 YSK */
/* PRINT Xprint BOL 2 like Dayton                                             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)".
def var v-fob               as   char format "x(12)".
def var v-tot-cases         as   int format "->,>>>,>>9".
def var v-tot-palls         as   int format "->,>>>,>>9".
def var v-tot-wt            as   dec format "->>,>>>,>>9".

def var v-tot-pkgs          as   int format ">>9".
def var v-pal-cnt           as   dec.
def var v-bol-qty           like oe-boll.qty.
def var v-ship-qty          like oe-ordl.ship-qty.
def var v-bol-wt            as   dec.
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   char format "x".
def var v-part-qty          as   dec.
def var v-ord-no            like oe-boll.ord-no.
def var v-po-no             like oe-bolh.po-no.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-comp-name  like company.name.
def var v-comp-addr  like company.addr.
def var v-comp-city  like company.city.
def var v-comp-state like company.state.
def var v-comp-zip   like company.zip.
def var v-comp-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-lines AS INT NO-UNDO.
def var v-job-po            as   CHAR NO-UNDO.
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
ASSIGN ls-image1 = "images\pacific1.bmp"
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
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.

DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.  /* display company address */
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(56)" NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-cusx-add1 AS cha NO-UNDO.
DEF VAR v-cusx-add2 AS cha NO-UNDO.
DEF VAR v-cusx-add3 AS cha NO-UNDO.
DEF VAR v-cusx-add4 AS cha NO-UNDO.
DEF VAR v-cusx-add5 AS cha NO-UNDO.
DEF VAR v-cusx-email AS cha NO-UNDO.
DEF VAR v-cusx-name AS cha NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEF VAR lv-bolfmt-int AS INT NO-UNDO.
def var v-weight LIKE oe-boll.weight no-undo.
def buffer b-itemfg     for itemfg.

form oe-ordl.i-no                         format "x(15)"
     v-job-po                       at 17 format "x(15)"
     v-part-dscr                    at 33 format "x(30)"
     w2.cases                       to 70 format "->>>9"
     w2.cas-cnt                     to 77 format "->>>>>9"
     oe-boll.qty                    to 85 format "->>>>>9"
     oe-boll.p-c                    at 88
     oe-boll.weight                 AT 91
    with frame bol-mid down no-box no-labels stream-io width 100.

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
IF AVAIL sys-ctrl THEN 
   ASSIGN lv-display-comp = sys-ctrl.log-fld 
          lv-bolfmt-int = sys-ctrl.int-fld.
ELSE ASSIGN lv-display-comp = NO
            lv-bolfmt-int = 0.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.
ASSIGN v-comp-add1 = ""
       v-comp-add2 = ""
       v-comp-add3 = ""
       v-comp-add4 = ""
       v-comp-add5 = ""
       lv-email = ""
       lv-comp-name = "".

IF lv-display-comp THEN DO:
   FIND FIRST cust WHERE cust.company = cocode AND
                         cust.active = "X" NO-LOCK NO-ERROR.
 /*  ASSIGN v-comp-add1 = company.addr[1]
          v-comp-add2 = company.addr[2]
          v-comp-add3 = company.city + ", " + company.st + "  " + company.zip
          v-comp-add4 = "Phone:  " + IF AVAIL cust THEN string(cust.area-code,"(999)") + string(cust.phone,"999-9999") ELSE "" 
          v-comp-add5 = "Fax     :  " + IF AVAIL cust THEN string(cust.fax,"(999)999-9999") ELSE ""
          lv-email    = "Email:  " + IF AVAIL cust THEN cust.email ELSE ""
          lv-comp-name = company.NAME   
          .
 */ 
  IF AVAIL cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = cust.addr[2]
            v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME   
            v-cusx-add1 = v-comp-add1
            v-cusx-add2 = v-comp-add2
            v-cusx-add3 = v-comp-add3
            v-cusx-add4 = v-comp-add4
            v-cusx-add5 = v-comp-add5
            v-cusx-email = lv-email
            v-cusx-name = lv-comp-name
            .
END.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

{sa/sa-sls01.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

v-printline = 0.

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
     
    if shipto.broker then 
       assign
       v-comp-add1 = cust.addr[1]
       v-comp-add2 = cust.addr[2]
       v-comp-add3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip
       v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
       v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
       lv-email    = "Email:  " + cust.email   
       lv-comp-name = cust.NAME .

    ELSE ASSIGN v-comp-add1 = v-cusx-add1
                v-comp-add2 = v-cusx-add2    
                v-comp-add3 = v-cusx-add3    
                v-comp-add4 = v-cusx-add4                
                v-comp-add5 = v-cusx-add5
                lv-email    = v-cusx-email
                lv-comp-name = v-cusx-name.
        
                        
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
      
      if shipto.broker then 
         assign
           v-comp-add1 = oe-ord.sold-addr[1]
           v-comp-add2 = oe-ord.sold-addr[2]
           v-comp-add3 = oe-ord.sold-city + ", " +
                         oe-ord.sold-state + "  " +
                         oe-ord.sold-zip
           v-comp-add4 = ""
           v-comp-add5 = ""
           lv-email    = ""
           lv-comp-name = oe-ord.sold-name .

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
     IF lv-bolfmt-int = 1 THEN
       for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id,
         first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
         first itemfg where itemfg.company eq oe-boll.company
                      and itemfg.i-no    eq oe-boll.i-no no-lock
                break BY oe-boll.i-no BY oe-boll.ord-no BY oe-boll.line
                      BY oe-boll.po-no BY oe-boll.job-no BY oe-boll.job-no2:  
         IF FIRST-OF(oe-boll.i-no) THEN DO:
            FOR EACH w2.
                DELETE w2.
            END.
         END.
         if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
            find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
            if not avail w2 then create w2.
            ASSIGN w2.cas-cnt = oe-boll.qty-case
                   w2.cases   = w2.cases + oe-boll.cases.
         end.

         if oe-boll.partial ne 0 then do:
            find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
            if not avail w2 then create w2.
            ASSIGN w2.cas-cnt = oe-boll.partial
                   w2.cases   = w2.cases + 1.
         end.

         IF LAST-OF(oe-boll.i-no) THEN DO:
            i = 0.
            FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt:
                ln-cnt = ln-cnt + 2.          
                find first oe-ordl where oe-ordl.company eq cocode
                               and oe-ordl.ord-no  eq oe-boll.ord-no
                               and oe-ordl.i-no    eq oe-boll.i-no
                               and oe-ordl.line    eq oe-boll.LINE no-lock no-error.
                IF AVAIL oe-ordl THEN DO:
                  /* IF oe-ordl.part-dscr1 <> "" THEN ln-cnt = ln-cnt + 1.*/
                 /*  IF oe-ordl.part-dscr2 <> "" THEN ln-cnt = ln-cnt + 1.*/
                END.
                
                IF LAST-OF(w2.cases * w2.cas-cnt) THEN ln-cnt = ln-cnt + 1.
                /*MESSAGE oe-boll.i-no oe-boll.ord-no oe-boll.LINE w2.cases w2.cas-cnt ln-cnt skip
                    oe-ordl.part-dscr1 "," oe-ordl.part-dscr2 VIEW-AS ALERT-BOX.*/
            END.
         END.
     END.  /* bol summary page calc*/
     ELSE
     for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id,
         first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
         first itemfg where itemfg.company eq oe-boll.company
                      and itemfg.i-no    eq oe-boll.i-no no-lock
                break by report.key-01
                by report.key-02:                 
          ln-cnt = ln-cnt + 3.          
          find first oe-ordl where oe-ordl.company eq cocode
                               and oe-ordl.ord-no  eq oe-boll.ord-no
                               and oe-ordl.i-no    eq oe-boll.i-no
                               and oe-ordl.line    eq oe-boll.LINE no-lock no-error.
          IF AVAIL oe-ordl THEN DO:
          /*   IF oe-ordl.part-dscr1 <> "" THEN ln-cnt = ln-cnt + 1.
             IF oe-ordl.part-dscr2 <> "" THEN ln-cnt = ln-cnt + 1.*/
          END.
     END.

     FOR EACH w2.
         DELETE w2.
     END.
     /* end of dup loop */
      lv-tot-pg = IF lv-tot-pg MOD 23 EQ 0 THEN TRUNC( ln-cnt / 23,0)
                  ELSE lv-tot-pg + TRUNC( ln-cnt / 23,0) .  /* 16->38 23 po detail lines */

      /*  end of getting total page per po */
     {oe/rep/bolsth22.i}
     {oe/rep/bolsthp2.i}

    v-last-page = page-number.

  IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.

  PUT "<R54><C50><#8><FROM><R+4><C+30><RECT> " 
    "<=8><R+1> Total Cases       :" v-tot-cases  
    "<=8><R+3> Total Weight      :" v-tot-wt /*fORM ">>,>>9.99"*/ .
    
  PUT "<FArial><R51><C1><P12><B>     Shipping Instructions: </B> <P9> " 
    "<R53><C1>      " oe-bolh.ship-i[1] 
    "<R54><C1>      " oe-bolh.ship-i[2] 
    "<R55><C1>      " oe-bolh.ship-i[3] 
    "<R56><C1>      " oe-bolh.ship-i[4] 
    "<R58><C1>"
    "__________________________________________________________________________________________________________________" 
    "<R59><C1>" "<B>  Signature of Receipt </B>" 
    "<R60><C5>" "Customer ________________________________________                       Carrier _______________________________________" 
    "<R62><C5>" "Date ____________________________________________                       Date _________________________________________" 
    .

  v-printline = v-printline + 14.
  IF last-of(oe-bolh.bol-no) THEN lv-pg-num = PAGE-NUM .

/*  IF v-printline < 45 THEN PUT SKIP(60 - v-printline). */
  PAGE.
  v-printline = 0.

  for each report where report.term-id eq v-term-id,
      first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
    delete report.
  end.

  END.  /* last-of*/

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

