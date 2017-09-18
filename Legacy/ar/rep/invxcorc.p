/* ---------------------------------------------- ar/rep/invxcorc.p  */
/* PRINT INVOICE   Xprint Form for Corrugated Concept         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{ar/rep/invoice.i}
{custom/notesdef.i}

DEF VAR v-inst AS cha FORM "x(60)" EXTENT 4 NO-UNDO.
def var v-salesman as char format "x(14)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
def var v-line as int NO-UNDO.
def var v-printline as INT NO-UNDO.
def var v-invhead as char format "x(13)" initial
  "I N V O I C E".
def var v-pitch like asi.printer.pitch NO-UNDO.
def var v-len as int NO-UNDO.
def var v-hldpitch like asi.printer.pitch NO-UNDO.
def var v-t-weight like ar-invl.t-weight NO-UNDO.
def var v-inv-no as int NO-UNDO.
def var v-tot-cas as dec format "->>>9.9999" NO-UNDO.
def var v-tot-pallets as int NO-UNDO.
def var v-tot-qty as INT NO-UNDO.
def var v-inv-date as date initial TODAY FORM "99/99/9999" NO-UNDO.
def shared var v-fr-tax as logical initial no NO-UNDO.
def var v-tax-rate as dec format "->>>.99" NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def var v-ans as logical initial no NO-UNDO.
def var v-date-ship as date initial today NO-UNDO.
def var v-del-no as int format ">>>>>>" NO-UNDO.
def var v-bol-cases LIKE oe-boll.cases NO-UNDO.
def var v-set-qty AS INT NO-UNDO.
def var v-part-qty AS DEC FORMAT "999.9999" NO-UNDO.
def var v-net like ar-inv.net NO-UNDO.
def var v-case-cnt as char format "x(80)" extent 5 NO-UNDO.
def var v-case-line as char NO-UNDO.
def var v-part-line as char NO-UNDO.
def var tmp1 as dec NO-UNDO.
def var tmp2 as date NO-UNDO.
def var net1 as dec NO-UNDO.
def var net2 as dec NO-UNDO.
def var net3 as dec NO-UNDO.
def var cnt as int NO-UNDO.
def var disp-frt as char init "Freight:" format "x(8)" NO-UNDO.
def var minus-ship as int NO-UNDO.

def buffer xar-inv for ar-inv .
def buffer ar-invl for ar-invl .

def workfile w-sman
  field sman as char format "x(4)".

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as int format "99999" no-undo.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like ar-invl.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-price-head as char format "x(5)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def workfile w-tax
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE ar-inv.freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.

FIND FIRST ar-inv NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(150)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(150)" NO-UNDO.
ASSIGN ls-image1 = "images\corrugat.jpg"
       ls-image2 = "images\cc-only.jpg".

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
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.

/* have own logo
find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.
*/

find first company where company.company eq cocode NO-LOCK. 
/*if avail company then
assign
 v-sname     = company.name
 v-saddr [1] = company.addr [1]
 v-saddr [2] = company.addr [2]
 v-scity     = company.city
 v-sstate    = company.state
 v-szip      = company.zip
 v-comp-add1 = company.addr[1]
 v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
 v-comp-add3 = "Phone: 604.533.2545" 
 v-comp-add4 = "Fax  : 604.533.2633".
 .
 */

 ASSIGN v-comp-add1 = "3870  NE  33rd  Street"
        v-comp-add2 = "Ocala,   FL   34479" 
        v-comp-add3 = "Phone:  (352)401-9000"
        v-comp-add4 = "FAX   :  (352)401-9226"
        v-comp-add5 = ""
        .

 IF lv-display-comp THEN DO:
    FIND FIRST cust WHERE cust.company = cocode AND
                       cust.active = "X" NO-LOCK NO-ERROR.
    IF AVAIL cust THEN
       ASSIGN v-comp-add1 = cust.addr[1]
           v-comp-add2 = cust.addr[2]
           v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
           v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
           v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
           lv-email    = "Email:  " + cust.email 
           lv-comp-name = cust.NAME   
           .
 END.
    

 
 find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first ar-inv where recid(ar-inv) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      
      FIND FIRST cust WHERE cust.company = ar-inv.company
                        AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.

      find first carrier where carrier.company eq cocode
                           and carrier.carrier eq ar-inv.carrier no-lock no-error.
      if avail carrier THEN ASSIGN v-shipvia = carrier.dscr.
      else assign v-shipvia = "".
      v-addr3 = cust.city + ", " + cust.state + "  " + cust.zip .

      find first shipto where shipto.company eq cocode
                          and shipto.cust-no eq ar-inv.cust-no
                          and shipto.ship-id eq ar-inv.ship-id no-lock no-error.

      IF AVAIL shipto THEN 
         assign  v-shipto-name = shipto.ship-name
           v-shipto-addr[1] = shipto.ship-addr[1]
           v-shipto-addr[2] = shipto.ship-addr[2]
           v-shipto-city = shipto.ship-city
           v-shipto-state = shipto.ship-state
           v-shipto-zip = shipto.ship-zip
           
           v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
                          "  " + v-shipto-zip .


       if ar-inv.fob-code begins "ORIG" THEN assign v-fob = "Origin".
       ELSE assign v-fob = "Destination".

       ASSIGN v-del-no = 0
              v-line = 1
              v-printline = 0.
    
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
            no-lock no-error.
        if not avail stax then
        find first stax where stax.tax-group eq ar-inv.tax-code
            no-lock no-error.
   
        if avail stax then
          assign v-tax-rate = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
                 v-tax-code[1] = stax.tax-code[1]
                 v-tax-code[2] = stax.tax-code[2]
                 v-tax-code[3] = stax.tax-code[3]
                 v-tx-rate[1]  = stax.tax-rate[1]
                 v-tx-rate[2]  = stax.tax-rate[2]
                 v-tx-rate[3]  = stax.tax-rate[3].

        assign 
         v-tot-pallets = 0
         v-date-ship   = ar-inv.inv-date.

        for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no
          break by ar-invl.i-no:
         do i = 1 to 3:
          if ar-invl.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = ar-invl.sman[i].
          end.
         end.
         assign v-tot-qty = v-tot-qty + ar-invl.ship-qty
                v-t-weight = v-t-weight + (round(ar-invl.t-weight /
                            ar-invl.qty, 2) * ar-invl.inv-qty).
         v-tot-pallets = 0.
         
         IF ar-invl.b-no NE 0 THEN
         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = ar-invl.b-no :
           /*FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
                                          oe-boll.b-no = oe-bolh.b-no AND
                                          oe-boll.i-no = ar-invl.i-no AND
                                          oe-boll.ord-no = ar-invl.ord-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases
                     v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (if oe-boll.partial gt 0 then 1 else 0).
           END. /* each oe-boll */*/
           assign v-date-ship = oe-bolh.bol-date
                 /* v-tot-pallets = v-tot-pallets + v-bol-cases +
                                  (if oe-boll.partial gt 0 then 1 else 0) */.

         END. /* each oe-bolh */
         
         if last-of(ar-invl.i-no) then do:
           if ar-invl.est-no ne "" then
           do:
             find first eb where eb.company = ar-invl.company and
                           eb.est-no = ar-invl.est-no AND             
                           eb.form-no = ar-invl.form-no and
                           eb.blank-no = ar-invl.blank-no no-lock no-error.

             IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = ar-invl.company
                               AND fg-set.set-no = ar-invl.i-no:
                               ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
               END.
               IF v-set-qty = 0 THEN ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = ar-invl.company AND
                                eb.est-no = ar-invl.est-no AND                  
                                eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = ar-invl.company AND
                             fg-set.set-no = ar-invl.i-no  AND
                             fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                 IF AVAIL fg-set AND fg-set.part-qty NE 0 THEN
                    ASSIGN v-part-qty = fg-set.part-qty / v-set-qty.
                 ELSE
                    ASSIGN v-part-qty = 1 / v-set-qty.


                IF eb.cas-cnt = 0 THEN
                   ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                 ELSE
                   ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                 if v-bol-cases ne 0 then
                   assign v-tot-cas = v-bol-cases.
                 /***
                 ASSIGN v-tot-pallets = v-tot-pallets +
                      ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
                 ***/
               END. /* each eb */
             END. /* do */
             ELSE
             IF AVAIL eb THEN
             DO:
               IF eb.cas-cnt = 0 THEN
                 ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
               ELSE
                 ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
               if v-bol-cases ne 0 then
                 assign v-tot-cas = v-bol-cases.
               /***
               ASSIGN v-tot-pallets = v-tot-pallets +
                   ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
               ***/
             END. /* do */
           end. /* est-no ne "" */
          assign
             v-t-weight = 0
             v-tot-cas = 0
             v-tot-qty = 0.
         end. /* last-of i-no */
        end. /* each ar-invl */
    
                        /** Build Salesman Id String **/
        v-salesman = "".
        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) then
            assign v-salesman = v-salesman + w-sman.sman.
          delete w-sman.
        end.
/*
        find first oe-bolh where oe-bolh.company = ar-inv.company and
                                 oe-bolh.bol-no = ar-inv.bol-no
                                 USE-INDEX bol-no no-lock no-error.
        if avail oe-bolh then
          assign v-rel-po-no = oe-bolh.po-no.
*/
        assign v-po-no = ar-inv.po-no
               v-bill-i = ar-inv.bill-i[1]
               v-ord-no = ar-inv.ord-no
               v-ord-date = ar-inv.ord-date
               v-inv-date = ar-inv.inv-date.

        find first ar-invl where ar-invl.x-no = ar-inv.x-no
                                  no-lock no-error.
        if avail ar-invl then
        do:
          assign v-price-head = ar-invl.pr-uom
                  v-po-no = IF ar-invl.po-no <> "" THEN ar-invl.po-no ELSE v-po-no                 
                  v-ord-no = ar-invl.ord-no
                  .
        end.
        /* display heder info 
         view frame invhead-comp.  /* Print headers */
       */
        {ar/rep/invxcorc.i}

        v-subtot-lines = 0.
        v-t-tax = 0.
        for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = ""
                 v-line = v-line + 1.

            find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = ar-invl.ord-no and
                                     ar-invl.ord-no NE 0 AND
                                     oe-ordl.i-no = ar-invl.i-no AND
                                     oe-ordl.LINE = ar-invl.LINE
                                     no-lock no-error.
            if avail oe-ordl then
              assign v-bo-qty = if (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty).
            else
              assign v-bo-qty = if ( ar-invl.qty - ar-invl.ship-qty ) < 0
                                  then 0 else ar-invl.qty - ar-invl.ship-qty.

            assign v-inv-qty = ar-invl.inv-qty /* ar-invl.qty : ordered qty */
                   v-ship-qty = ar-invl.ship-qty
                   v-i-no = ar-invl.i-no
                   v-i-dscr = ar-invl.i-name
                   v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
                   v-t-price = ar-invl.amt
                   v-subtot-lines = v-subtot-lines + ar-invl.amt.


                if ar-invl.tax and avail stax then
                do i = 1 to 3:
                  if stax.tax-code[i] ne "" then do:
                    create w-tax.
                    assign
                     w-dsc      = stax.tax-dscr[i]
                     w-tax      = round((if stax.company eq "yes" then v-t-price
                                                                  else ar-invl.amt) *
                                        stax.tax-rate[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
                  end.
                end.

                if v-t-price ne ar-invl.amt then do:
                  create w-tax.
                  assign
                   w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
                end.                       

            PUT space(1) v-inv-qty format "->>>>>9" SPACE(1)
                v-ship-qty  format "->>>>>9" SPACE(1)
                /*v-bo-qty  format   "->>>>>9" SPACE(1)*/
                ar-invl.ord-no FORM ">>>>>>9" SPACE(1)
                v-i-no  format "x(15)" SPACE(1)
                v-i-dscr  format "x(25)" SPACE(1)
                v-price /* format ">>>9.9999" */ FORMAT ">>,>>9.99" SPACE(2)
                v-price-head SPACE(1)
                ar-invl.amt  format "->>>,>>9.99"                     
                SKIP.
             v-printline = v-printline + 1.

      /* not display tax for lines      
         for each w-tax :
                put fill(" ",15)  w-dsc 
                      w-tax  SKIP .             
                   
                delete w-tax.
            end.
            */
            do v = 1 to 2:
              v-part-info = IF v EQ 1 THEN
                              IF AVAIL oe-ordl THEN ar-invl.part-dscr1
                                               ELSE ar-invl.i-dscr
                                      ELSE ar-invl.part-dscr2.

              if v-part-info ne "" OR ar-invl.part-no <> "" then do:
                /*put space(40) v-part-info skip.*/
                 IF v = 1 THEN PUT SPACE(25) ar-invl.part-no SPACE v-part-info SKIP.
                 ELSE   PUT SPACE(41) v-part-info SKIP.
                 v-printline = v-printline + 1.
              end.
            end.
            put skip(1).
            v-printline = v-printline + 1.

            IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
               {ar/rep/invxcorc.i}
            END.
        end. /* each ar-invl */
/*
        for each inv-misc no-lock where inv-misc.company = ar-inv.company and
          inv-misc.r-no = ar-inv.r-no and
          inv-misc.bill = "Y" break by ord-no with frame detailm:
          if first(inv-misc.ord-no) then
          do:
            IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
               {ar/rep/invxcorc.i}
            END.
            put "** Miscellaneous Items **" at 23 skip(1).
            assign v-printline = v-printline + 2.
          end.
            
          put inv-misc.charge AT 10 inv-misc.dscr inv-misc.amt SKIP.
            v-subtot-lines = v-subtot-lines + inv-misc.amt.
            v-printline = v-printline + 1.
            if inv-misc.tax and avail stax then
            do i = 1 to 3:
              if stax.tax-code[i] ne "" then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = if stax.company eq "yes" then v-t-price
                              else inv-misc.amt
                 w-tax      = round(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
                 v-t-price  = v-t-price + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
              end.
            end.

            if v-t-price ne inv-misc.amt then do:
              create w-tax.
              assign
               w-dsc     = "******ITEM TOTAL:"
               w-tax     = v-t-price
               v-lines   = v-lines + 1.
            end.
/* no need to display tax for misc
            for each w-tax :
                put fill(" ",15)  w-dsc
                      w-tax                
                      SKIP.
                delete w-tax.
            end.


            assign v-line = v-line + 1
                   v-printline = v-printline + 2.
*/         IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
               {oe/rep/invxcorc.i}
           END.

        end. /* each inv-misc */
 */

     /*   if v-prntinst then do: */

       /*  do i = 1 to 4:
          if ar-inv.bill-i[i] ne "" then do:
       /*     if v-printline gt 29 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 72.
              assign v-printline = 0.
              page.

            end.
        
             IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
               {oe/rep/invxcorc.i}
            END.

            put ar-inv.bill-i[i] at 10 skip.
            assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */   */      
======== */
     {custom/notesprtA.i ar-inv v-inst 4}
         DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:                
                 IF v-printline > 50 THEN DO:
                    PAGE.
                    v-printline = 0.
                    {ar/rep/invxcorc.i}
                 END.
                 PUT v-inst[i] SKIP.
                 v-printline = v-printline + 1.
              END.            
        end.

        IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
               {ar/rep/invxcorc.i}
        END.
/*        end. */

        /* T O T A L S */
     /*   put skip(30 - v-printline). */
   /*
        find first cust where cust.company = cocode and
                   cust.cust-no = ar-inv.cust-no no-lock no-error.
        if avail cust and cust.sort = "Y" AND ar-inv.t-inv-tax <> 0 then
          put "Total Tax: " ar-inv.t-inv-tax SKIP.

        else PUT " " skip(1) .

        if ar-inv.f-bill then
           put disp-frt
               ar-inv.t-inv-freight
               ar-inv.t-inv-rev SKIP .
        else
          put "" SKIP.
              
  */
        v-frt-tax = ar-inv.freight.
        IF ar-inv.tax-code <> "" and
           (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
           AND ar-inv.freight <> 0
           AND AVAIL stax THEN
        do i = 1 to 3:

           if stax.tax-code[i] ne "" then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = round((if stax.company eq "yes" then v-frt-tax
                                                         ELSE ar-inv.freight) *
                                        stax.tax-rate[i] / 100,2)                 
                 v-frt-tax  = v-frt-tax + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
           END.
        end.      

        

    do i = 1 to 3:
       v-bot-lab[i] = if v-t-tax[i] ne 0 then
                    /*  ((if avail stax then string(stax.tax-dscr[i],"x(5)")
                        else fill(" ",5))*/ 
                        ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                           ELSE FILL(" ",5) ) +
                       fill(" ",6) + ":" +
                       string(v-t-tax[i],"->>>>>9.99")) else "".
    end.
    v-inv-freight = if (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
                    THEN ar-inv.freight ELSE 0.    
                    /*ar-inv.t-inv-freight*/.

    PUT "<R58><C58><#8><FROM><R+5><C+22><RECT> " 
        "<=8> Sub Total    :" v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1> Freight      :" v-inv-freight
        "<=8><R+2> Sales Tax    :" /* v-bot-lab[1] */
                           ar-inv.tax-amt FORM "->>,>>9.99"
        "<=8><R+3> " /*PST        :" ar-inv.t-inv-tax FORM "->>,>>9.99"*/
                    /*v-bot-lab[2] */
        "<=8><R+4> Invoice Total:" ar-inv.due FORM "->>,>>9.99" .

   /* PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " SKIP
         "  Your business is greatly appreciated! Thank You!"SKIP
         "  Please pay by invoice - no statements are issued." SKIP
         "  24% per annum interest charge on overdue accounts. " SKIP
         "  Any credit card purchases are subject to a charge of 2%." SKIP
         "  GST # R129463212.  Check out our website at www.pacificpackaging.ca" SKIP.
    */
    v-printline = v-printline + 6.
    /*IF v-printline < 50 THEN PUT SKIP(60 - v-printline). */
    PAGE.

    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.

    END. /* DO TRANSACTION avail ar-inv */ 

    end. /* each xar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
