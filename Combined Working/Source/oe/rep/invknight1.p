/* ---------------------------------------------- oe/rep/invknight1.p */
/* PRINT INVOICE   Xprint form for Knight Pkg           */
/* -------------------------------------------------------------------------- */

 DEF INPUT PARAM ip-copy-title AS cha NO-UNDO. 

{sys/inc/var.i shared}

{oe/rep/invoice.i}
def var v-salesman as char format "x(14)" NO-UNDO.
def var v-salesname as char format "x(30)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
def var v-printline as INT NO-UNDO.
def shared var v-fr-tax as logical initial no NO-UNDO.
DEF VAR v-inv-date AS DATE initial TODAY FORMAT "99/99/9999" NO-UNDO.
def var v-tax-rate as dec format "->>>.99" NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def var v-date-ship as date initial today NO-UNDO.
def var v-case-cnt as char format "x(80)" extent 5 NO-UNDO.
def var v-case-line as char NO-UNDO.
def var v-part-line as char NO-UNDO.
DEF VAR v-pc AS cha NO-UNDO. /* partial or complete */

def buffer xinv-head for inv-head .
def buffer xinv-line for inv-line .

def TEMP-TABLE w-sman
  field sman as char format "x(4)".

def var v-inv-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like inv-line.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
DEF var v-ship-i as char format "x(25)" no-undo.

def var v-price-head as char format "x(5)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def TEMP-TABLE w-tax
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
/*ASSIGN ls-image1 = "images\knight.jpg"
       FILE-INFO:FILE-NAME = ls-image1.
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".*/

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).

ASSIGN ls-full-img1 = cRtnChar + ">" .
    
    find first company where company.company = cocode no-lock no-error.

    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      FIND FIRST cust WHERE cust.company = xinv-head.company
                        AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

      assign v-shipto-name = xinv-head.sold-name
             v-shipto-addr[1] = xinv-head.sold-addr[1]
             v-shipto-addr[2] = xinv-head.sold-addr[2]
             v-shipto-city = xinv-head.sold-city
             v-shipto-state = xinv-head.sold-state
             v-shipto-zip = xinv-head.sold-zip.

      find first oe-bolh where oe-bolh.company = xinv-head.company and
          oe-bolh.bol-no = xinv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
     
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-bolh.cust-no and
                   shipto.ship-id = oe-bolh.ship-id no-lock no-error.
        if avail shipto then
           assign v-shipto-name = shipto.ship-name
                  v-shipto-addr[1] = shipto.ship-addr[1]
                  v-shipto-addr[2] = shipto.ship-addr[2]
                  v-shipto-city = shipto.ship-city
                  v-shipto-state = shipto.ship-state
                  v-shipto-zip = shipto.ship-zip.

      end. /* avail oe-bolh */

      IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
         RUN oe/get-inv#.p (ROWID(xinv-head)).

      DO TRANSACTION:
        FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).

        if inv-head.inv-date ne ? then v-inv-date = inv-head.inv-date.

        if inv-head.fob-code begins "ORIG" then
           assign v-fob = "Origin".
        else
           assign v-fob = "Destination".

        find FIRST carrier where
             carrier.company = inv-head.company and
             carrier.carrier = inv-head.carrier
             no-lock no-error.

        if avail carrier then
          assign v-shipvia = carrier.dscr.
        else
          assign v-shipvia = "".

        assign
          v-addr3 = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
                         "  " + v-shipto-zip
          v-printline = 0.
    
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq inv-head.tax-gr
            no-lock no-error.
        if not avail stax then
        find first stax where stax.tax-group eq inv-head.tax-gr
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
        
        for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no
          break by xinv-line.i-no:
         do i = 1 to 3:
          if xinv-line.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = xinv-line.sman[i].
          end.
         end.
         assign v-pc = "C". /* complete*/

         FOR EACH oe-bolh NO-LOCK WHERE
             oe-bolh.b-no = xinv-line.b-no:

             v-pc = "P". /* partial*/ 
             FOR EACH oe-boll NO-LOCK WHERE
                 oe-boll.company = oe-bolh.company AND
                 oe-boll.b-no = oe-bolh.b-no AND
                 oe-boll.i-no = xinv-line.i-no AND
                 oe-boll.ord-no = xinv-line.ord-no:
             
                IF oe-boll.p-c THEN v-pc = "C". /*complete*/
                
             END. /* each oe-boll */
             assign v-date-ship = oe-bolh.bol-date.
         END. /* each oe-bolh */
         
        end. /* each xinv-line */
    
        /** Build Salesman Id String **/
        v-salesman = "".
        for each w-sman break by w-sman.sman:
            if first-of(w-sman.sman) then
               assign v-salesman = v-salesman + w-sman.sman.
            delete w-sman.
        end.

        find first inv-line where inv-line.r-no = inv-head.r-no
                                  no-lock no-error.
        if avail inv-line then
        do:
          assign v-price-head = inv-line.pr-uom.
          find first oe-ord where oe-ord.company = cocode and
                                  oe-ord.ord-no = inv-line.ord-no
                                  no-lock no-error.
          if avail oe-ord then
            assign v-po-no = oe-ord.po-no
                   v-bill-i = oe-ord.bill-i[1]
                   v-ord-no = oe-ord.ord-no.
          else
            assign v-price-head = inv-line.pr-uom.
        end.
        
        {oe/rep/invknight1.i}  /* xprint form */

        ASSIGN
           v-subtot-lines = 0
           v-t-tax = 0.

        for each inv-line no-lock where inv-line.r-no = inv-head.r-no:
          
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = ""
                 v-pc = "P". /* partial*/

          for each oe-boll no-lock where oe-boll.company = inv-line.company
                        and oe-boll.bol-no = inv-head.bol-no
                        and oe-boll.i-no = inv-line.i-no use-index bol-no:

            /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
            assign v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else assign v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

            IF oe-boll.p-c THEN v-pc = "C". /*complete*/

            do i = 1 to 5:
              if (80 - length(v-case-cnt[i])) > length(v-case-line) and
                v-case-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                     v-case-line = "".
              if (80 - length(v-case-cnt[i])) > length(v-part-line) and
                v-part-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                     v-part-line = "".
            end. /* 1 to 5 */
            
          end. /* each oe-boll */

          IF v-printline > 62 THEN do:           
             PAGE.
             {oe/rep/invknight1.i}
             v-printline = 21.
          END.

          find first oe-ordl where oe-ordl.company = cocode and
                                   oe-ordl.ord-no = inv-line.ord-no and
                                   oe-ordl.i-no = inv-line.i-no
                                   no-lock no-error.
          assign v-printline = v-printline + 2
                 v-inv-qty = inv-line.qty
                 v-i-no = inv-line.i-no
                 v-price = inv-line.price * (1 - (inv-line.disc / 100))
                 v-t-price = inv-line.t-price
                 v-subtot-lines = v-subtot-lines + inv-line.t-price.

          if inv-line.tax and avail stax then
          do i = 1 to 3:
            if stax.tax-code[i] ne "" then do:
              create w-tax.
              assign
               w-dsc      = stax.tax-dscr[i]
               w-tax      = round((if stax.company eq "yes" then v-t-price
                                                            else inv-line.t-price) *
                                  stax.tax-rate[i] / 100,2)
               v-t-price  = v-t-price + w-tax
               v-t-tax[i] = v-t-tax[i] + w-tax.
            end.
          end.

          if v-t-price ne inv-line.t-price then do:
             create w-tax.
             assign
                w-dsc     = "******ITEM TOTAL:"
                w-tax     = v-t-price.
          end.
          
          ASSIGN v-po-no  = inv-line.po-no
                 v-ord-no = inv-line.ord-no
                 v-price-head = inv-line.pr-uom.

          PUT space(1)
              v-po-no FORMAT "X(15)"
              inv-line.part-no FORMAT "X(15)"
              inv-line.i-name FORMAT "X(30)"
              inv-line.ship-qty format "->>,>>>,>>9"
              v-price  format ">>>,>>9.9999"                
              inv-line.t-price  format "->>>,>>9.99"             
              SKIP
              v-ord-no FORMAT ">>>>>9" SPACE(10)
              inv-line.i-no FORMAT "X(15)"
              inv-line.part-dscr1 FORMAT "x(30)" SPACE(13)
              v-pc  FORMAT "x" SPACE(6)
              v-price-head SKIP
              inv-line.part-dscr2 FORMAT "X(30)" AT 32 SKIP.

          v-printline = v-printline + 4.
           
          put skip(1).
          
        end. /* each inv-line */

        for each inv-misc no-lock where inv-misc.company = inv-head.company and
          inv-misc.r-no = inv-head.r-no and
          inv-misc.bill = "Y" break by ord-no with frame detailm:
          IF v-printline > 62 THEN do:
             PAGE.                
             {oe/rep/invknight1.i}
             v-printline = 21.
          END.

          if first(inv-misc.ord-no) then
          do:
             put "** Miscellaneous Items **" at 23 skip(1).
             assign v-printline = v-printline + 2.
          end.

          put 
            inv-misc.po-no FORMAT "x(15)"
            inv-misc.charge AT 17 
            inv-misc.dscr FORMAT "x(30)"
            inv-misc.amt AT 85 SKIP.

          ASSIGN
             v-subtot-lines = v-subtot-lines + inv-misc.amt
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
                 v-t-tax[i] = v-t-tax[i] + w-tax.
             end.
          end.

          if v-t-price ne inv-misc.amt then do:
             create w-tax.
             assign
              w-dsc     = "******ITEM TOTAL:"
              w-tax     = v-t-price.
          end.

        end. /* each inv-misc */

        if v-prntinst then do:
         do i = 1 to 4:
          if inv-head.bill-i[i] ne "" then do:
             put inv-head.bill-i[i] at 10 skip.
             assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */
        end.

        v-frt-tax = inv-head.t-inv-freight.
        IF inv-head.tax-gr <> "" and
           inv-head.f-bill AND inv-head.t-inv-freight <> 0 AND AVAIL stax THEN
        do i = 1 to 3:

           if stax.tax-code[i] ne "" then do:
              create w-tax.
              assign
               w-dsc      = stax.tax-dscr[i]
               w-tax      = round((if stax.company eq "yes" then v-frt-tax
                                                       ELSE inv-head.t-inv-freight) *
                                      stax.tax-rate[i] / 100,2)                 
               v-frt-tax  = v-frt-tax + w-tax
               v-t-tax[i] = v-t-tax[i] + w-tax.
           END.
        end.
      end. /* DO TRANSACTION */

    do i = 1 to 3:
       v-bot-lab[i] = if v-t-tax[i] ne 0 then
                       ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                          ELSE FILL(" ",5) ) +
                      fill(" ",6) + ":" +
                      string(v-t-tax[i],"->>>>>9.99")) else "".
    end.
    v-inv-freight = if inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.

    PUT "<R58><C58><#8><FROM><R+5><C+22><RECT> " 
        "<=8> Sub Total    :" v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1> Freight      :" v-inv-freight
        "<=8><R+2> Sales Tax    :" inv-head.t-inv-tax FORM "->>,>>9.99"
        "<=8><R+3>" "" 
        "<=8><R+4> Total Invoice:" inv-head.t-inv-rev FORM "->>,>>9.99"
        "<FArial><R58><C1><P12>  <P9> " SKIP.

    v-printline = v-printline + 6.
   
    IF v-printline <= 66 THEN page.
 
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
