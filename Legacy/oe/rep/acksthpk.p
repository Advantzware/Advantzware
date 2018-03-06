/* ----------------------------------------------- oe/rep/ackpacif.p 10/10 YSK */
/* ORDER ACKNOLEDGEMENT                                                       */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/acknowl.i}
{custom/notesdef.i}
def var v-salesman as char format "x(3)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-addr4 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-line as INT NO-UNDO.
def var v-printline as int NO-UNDO.
DEF VAR v-inst AS cha EXTENT 4 FORM "x(80)" NO-UNDO.

def var v-ackhead as char format "x(32)" init
  "A C K N O W L E D G E M E N T" NO-UNDO.
def var v-len as int NO-UNDO.
def var v-totord as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-totlin as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-ans as log init no NO-UNDO.
def var lcnt as int init 1 NO-UNDO.
def var pagebreak as int init 28 NO-UNDO.
def var v-cust-phone as char format "(999)999-9999" no-undo.
def var v-part like oe-ordl.part-no no-undo.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.
DEF VAR lv-first-note AS LOG NO-UNDO.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image1 = "images\southpak.jpg"
       /*ls-image2 = "images\pacific2.bmp"*/ .

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
/*FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".
*/
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
DEF VAR lv-due-date AS DATE NO-UNDO.

  ll-calc-disc-first = NO.
  FOR EACH sys-ctrl
      WHERE sys-ctrl.company  EQ cocode
        AND sys-ctrl.name     EQ "INVPRINT"
        AND sys-ctrl.char-fld EQ "Dayton"
      NO-LOCK:
    ll-calc-disc-first = YES.
    LEAVE.
  END.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
  find first company where company.company eq cocode no-lock no-error.
  ASSIGN v-comp-add1 = company.addr[1]
         v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
         v-comp-add3 = "Phone: 604.533.2545" 
         v-comp-add4 = "Fax  : 604.533.2633".

  FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
      FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id:

      if oe-ord.sman[2] eq "" and oe-ord.sman[3] eq "" then
        v-salesman = oe-ord.sman[1].
      else
        v-salesman = oe-ord.sman[1] + oe-ord.sman[2] + oe-ord.sman[3].

      if oe-ord.fob-code eq "ORIG" then
        v-fob = "Origin".
      else
        v-fob = "Destination".

      find FIRST carrier
          where carrier.company eq oe-ord.company
            and carrier.carrier eq oe-ord.carrier
          no-lock no-error.
      if avail carrier then
        v-shipvia = carrier.dscr.
      else
        v-shipvia = "".
      v-printline = 0.
      assign
       v-addr3 = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
       v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
                      "  " + oe-ord.sold-zip
       v-line = 1.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq oe-ord.cust-no
          no-lock no-error.
      if avail cust then v-cust-phone = cust.area-code + cust.phone.
      /* get q-no for first item */
      v-q-no = 0.
      FIND first oe-ordl where oe-ordl.company eq oe-ord.company
                           and oe-ordl.ord-no  eq oe-ord.ord-no
                           NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl AND oe-ordl.est-no <> "" THEN DO:
         FIND last quotehd WHERE quotehd.company = oe-ordl.company
                              AND quotehd.est-no = oe-ordl.est-no
                              NO-LOCK NO-ERROR.
         IF AVAIL quotehd THEN DO:
            FIND FIRST quoteitm OF quotehd WHERE quoteitm.part-no = oe-ordl.part-no
                     NO-LOCK NO-ERROR.
            IF AVAIL quoteitm THEN DO:
                v-q-no = quoteitm.q-no.
               FIND FIRST quoteqty WHERE quoteqty.company = oe-ordl.company
                                     AND quoteqty.loc = quoteitm.loc
                                     AND quoteqty.q-no = quoteitm.q-no
                                     AND quoteqty.LINE = quoteitm.LINE
                                     AND quoteqty.qty = oe-ordl.qty
                                     NO-LOCK NO-ERROR.
               IF AVAIL quoteqty THEN v-q-no = quoteqty.q-no.
            END.
         END.
      END.
      lv-due-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE oe-ord.due-date.
      {oe/rep/acksthpk.i }

      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock:

        put v-line FORM ">>>9" SPACE(3)
                oe-ordl.i-no  SPACE(2)             
                oe-ordl.i-name SPACE(2)
                oe-ordl.qty SPACE(2)
                oe-ordl.price  FORM "->,>>>,>>9.99<<<<" SPACE(9)
                oe-ordl.pr-uom  SKIP            
            .
        v-printline = v-printline + 1.

       if oe-ordl.i-no ne oe-ordl.part-no or
           oe-ordl.part-dscr1 ne ""        then do:
          v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                   else "".
          put v-part             at 8
              oe-ordl.part-dscr1 at 25 skip.
          v-printline = v-printline + 1.
        end. 

        if oe-ordl.part-dscr2 ne "" then do:
          put oe-ordl.part-dscr2 at 25 skip.
          v-printline = v-printline + 1.
        end.

        FOR EACH oe-rel
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND ((oe-rel.link-no EQ 0 AND v-schrel)
               OR  (oe-rel.link-no NE 0 AND v-actrel))
            NO-LOCK BREAK BY oe-rel.link-no DESC WITH FRAME sched-rel DOWN:

        if v-printline gt 50 then
        do:
          PAGE {1}.
          {oe/rep/acksthpk.i}
          assign v-printline = 20.          
        end.

          if first-of(oe-rel.link-no) then
            if oe-rel.link-no eq 0 then lcnt = 1.
            else
            if first(oe-rel.link-no) then lcnt = 1.
  /*
          if (v-printline ge pagebreak) or
             (v-printline ge pagebreak - 1 and lcnt eq 1) then do:
            v-printline = 0.
            page.
          end.
  */
          if first-of(oe-rel.link-no) then do:
            if oe-rel.link-no eq 0 then do:
              put "Scheduled Releases:" at 10 skip.
              v-printline = v-printline + 1.
            end.
            else
            if first(oe-rel.link-no) then do:
              put "Actual Releases:" at 10 skip.
              v-printline = v-printline + 1.
            end.
          end.

           {oe/rel-stat.i lv-stat}
          IF AVAIL oe-rell THEN
          FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
          ld-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
          put lcnt AT 10 (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty) space(5) ld-date  SKIP.

          assign
           v-printline = v-printline + 1
           lcnt        = lcnt + 1.
          if v-shipto then do:
            find first shipto
                where shipto.company eq cocode
                  and shipto.cust-no eq oe-rel.cust-no
                  and shipto.ship-id eq oe-rel.ship-id
                no-lock no-error.
            if avail shipto then
              v-addr4 = shipto.ship-city + ", " +
                        shipto.ship-state + "  " + shipto.ship-zip.
       /*
            if v-printline + 4 ge pagebreak then do:
              v-printline = 0.
              page.
            end.
       */   IF AVAIL shipto THEN DO:
               if v-printline gt 49 THEN do:
                 PAGE {1}.
                 {oe/rep/acksthpk.i}
                 assign v-printline = 20.          
               end.
                put shipto.ship-name AT 10 SKIP .
                PUT shipto.ship-addr[1] AT 10 SKIP.
                v-printline = v-printline + 2.
                if v-printline gt 51 THEN do:
                 PAGE {1}.
                 {oe/rep/acksthpk.i}
                 assign v-printline = 20.          
               end.
                PUT shipto.ship-addr[2] AT 10 SKIP.
                PUT v-addr4 AT 10 SKIP.

                v-printline = v-printline + 2.
            END.
          end.
        end.   /* for each oe-rel  */

      /*  put "" skip.*/
        assign
         v-line = v-line + 1
         v-printline = v-printline + 1.
/*
        if v-printline ge pagebreak then do:
          v-printline = 0.
          page.
        end.
*/
        if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
           assign v-totlin = oe-ordl.price * IF oe-ordl.qty LT 0 THEN -1 ELSE 1.
                             
        else
        if oe-ordl.pr-uom eq "CS" then
        do:
          find first itemfg {sys/look/itemfgrlW.i}
            and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

          v-totlin = oe-ordl.qty /
                     (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                      if avail itemfg and itemfg.case-count ne 0
                      then itemfg.case-count else 1) *
                     oe-ordl.price.
        end.
        else
        if oe-ordl.pr-uom eq "C" then
          v-totlin = oe-ordl.qty / 100 * oe-ordl.price.

        else
        if oe-ordl.pr-uom eq "M" then
          v-totlin = oe-ordl.qty / 1000 * oe-ordl.price.

        else /** DEFAULT TO EACH **/
          v-totlin = oe-ordl.qty * oe-ordl.price.

        v-totlin = ROUND(v-totlin,2).

        IF oe-ordl.disc NE 0 THEN
           v-totlin = IF ll-calc-disc-first THEN 
                        (v-totlin - ROUND(v-totlin * oe-ordl.disc / 100,2))
                      ELSE
                        ROUND(v-totlin * (1 - (oe-ordl.disc / 100)),2).

        v-totord = v-totord + v-totlin.
 
        if v-printline gt 50 then
        do:
          PAGE {1}.
          {oe/rep/acksthpk.i}
          assign v-printline = 20.          
        end.
     IF v-prntinst THEN DO:
            /* print spec notes */
        find first itemfg {sys/look/itemfgrlW.i}
             and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

        IF AVAIL itemfg THEN DO:
           lv-first-note = yes.
           /*
           FOR EACH notes WHERE notes.rec_key = itemfg.rec_key NO-LOCK: 
               IF lv-first-note THEN do:
                  PUT SKIP(1).
                  v-printline = v-printline + 1.
                  lv-first-note = NO.
               END.
               v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
              {SYS/INC/ROUNDUP.I v-tmp-lines}
              IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                 PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                 v-printline = v-printline + 1.
              END.           
           end.
           IF NOT lv-first-note THEN do:
              PUT SKIP(1).
              v-printline = v-printline + 1.           
           END.
           */
           {custom/notesprt.i itemfg v-inst 4}
           DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:
                 IF lv-first-note THEN do:
                    PUT SKIP(1).
                    v-printline = v-printline + 1.
                    lv-first-note = NO.
                 END.
                 PUT v-inst[i] SKIP.
                 v-printline = v-printline + 1.
              END.
           END.
           IF NOT lv-first-note THEN do:
              PUT SKIP(1).
              v-printline = v-printline + 1.           
           END.
        END.
     END. /* v-prntinst */
      end. /* each oe-ordl */

      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
          oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:
        if v-printline gt 50 then
        do:
          PAGE {1}.
          {oe/rep/acksthpk.i}
          assign v-printline = 20.          
        end.

        if first(oe-ordm.ord-no) then
        do:
          put "** Miscellaneous Items **" at 23.
          if v-print-fmt eq "HOP" then put "Taxable" at 62.
          put skip(1).
          assign v-printline = v-printline + 2.
        end.
/*
        if v-printline ge pagebreak then
        do:
          assign v-printline = 0.
          page.
        end.
        else
        down with frame detailm.
*/
         if oe-ordm.bill eq "N" then
           PUT v-line oe-ordm.charge oe-ordm.dscr "     N/C" .          
        else
         PUT v-line oe-ordm.charge oe-ordm.dscr            
             oe-ordm.amt
             .
        PUT SKIP.
        assign v-line = v-line + 1
          v-printline = v-printline + 2.
        if oe-ordm.bill ne "N" then
        assign v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */
      if v-prntinst then
      do:
        do i = 1 TO 4:
          if oe-ord.bill-i[i] ne "" then
          do:
            put oe-ord.bill-i[i] at 5 skip.
            assign v-printline = v-printline + 1.
          end.
        end. /* 1 to 4 */
      end.
      
      assign
       v-totord        = 0
       oe-ord.ack-prnt = yes.
       
      PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P10> " 
          "<R57><C1>  PLEASE  VERIFY  AND  APPROVE  EACH  DETAIL  OF  THIS  ORDER  ACKNOWLEDGEMENT." 
          "<R58><C1>  All item quantities are subject to industry standard overrun or" 
          "<R59><C1>  underrun up to 20%. Order is subject to terms and conditions as shown on" 
          "<R60><C1>  Customer Credit Application and Information Sheet" 
          "<R62><C1> I acknowledge the above order specifications are correct."        
          "<R64><C1> ______________________________________(Please sign and fax back) " 
          .
      v-printline = v-printline + 9.
      IF v-printline <= 66 THEN page. /*PUT SKIP(74 - v-printline). */
    
      /*
      PAGE.
      assign v-printline = 0. */
    end. /* each oe-ord */

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
