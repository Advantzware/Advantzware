/* ----------------------------------------------- oe/rep/ackhopx.p */
/* ORDER ACKNOLEDGEMENT                                             */
/* for ORDER STATUS = (R), (U)                                      */
/* -----------------------------------------------------------------*/

{sys/inc/var.i shared}

{oe/rep/acknowl.i}
{custom/notesdef.i}
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
def var v-salesman as char format "x(15)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-addr4 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-line as INT NO-UNDO.
def var v-printline as int NO-UNDO.
def var v-totord as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-totlin as dec format "->>,>>>,>>9.99" NO-UNDO.
def var lcnt as int init 1 NO-UNDO.
def var v-cust-phone as char format "(999)999-9999" no-undo.
def var v-part like oe-ordl.part-no no-undo.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.


/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
ASSIGN ls-image1 = "images\hopx2.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.

DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.

DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR lv-first-note AS LOG NO-UNDO.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
  
  /*ASSIGN v-comp-add1 = "13170 Temple Avenue"
         v-comp-add2 = "City of Industry, CA 91746"
         v-comp-add3 = "Phone: 626-369-3371" 
         v-comp-add4 = "Fax     : 626-333-6115".*/

  FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
      FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id:

      if oe-ord.sman[2] eq "" and oe-ord.sman[3] eq "" then
        v-salesman = oe-ord.sname[1].
      else
        v-salesman = oe-ord.sname[1] + oe-ord.sname[2] + oe-ord.sname[3].

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
      
      {oe/rep/ackhopx.i}
      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock:
      
      FIND FIRST oe-rel
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND ((oe-rel.link-no EQ 0 AND v-schrel)
               OR  (oe-rel.link-no NE 0 AND v-actrel))
            NO-LOCK NO-ERROR. 
      IF AVAIL oe-ordl AND oe-ordl.est-no <> "" THEN DO:
          FIND FIRST eb WHERE eb.company EQ cocode 
              AND eb.est-no EQ  FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no))) + TRIM(oe-ordl.est-no)
              AND eb.stock-no EQ oe-ordl.i-no
          NO-LOCK NO-ERROR.
          IF NOT AVAIL eb  THEN
              FIND FIRST eb WHERE eb.company EQ cocode 
              AND eb.est-no EQ  FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no))) + TRIM(oe-ordl.est-no)
          NO-LOCK NO-ERROR.
          FIND FIRST ef where ef.company EQ cocode
              AND ef.est-no EQ FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no) 
              AND ef.form-no EQ eb.form-no 
          NO-LOCK NO-ERROR.
      END.
        if v-printline ge 45 then
        do:
           PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P10>  "          
               "<R57><C1>  Your Order is being Manufactured as described herein.  Any changes in this sale will not be valid unless House of"
               "<R58><C1>  Packaging, Inc. confirm them in writing.  All item quantities are subject to an over/under charge of 10%."
               "<=10><R-3>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>".
           PAGE.
           {oe/rep/ackhopx.i}
           v-printline = 20.          
        end.
        put v-line FORM ">>>9" SPACE(3)
            oe-ordl.i-no  SPACE(2)             
            oe-ordl.i-name SPACE(2)
            oe-ordl.qty SPACE(2)
            oe-ordl.price  FORM "->,>>>,>>9.99<<<<" SPACE(6)
            oe-ordl.pr-uom   SKIP.

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

        /*IF AVAIL eb THEN
        if eb.len ne 0 OR  eb.wid ne 0 OR eb.dep NE 0 then do:
          put "Carton Size: " at 25 
              trim(STRING(eb.len)) + "x" +   trim(STRING(eb.wid)) + "x" + trim(STRING(eb.dep)) skip.
          v-printline = v-printline + 1.
        end.*/

        find first itemfg {sys/look/itemfgrlW.i}
            and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

        IF AVAIL itemfg THEN
        if itemfg.l-score[50] ne 0 OR  itemfg.w-score[50] ne 0 OR itemfg.d-score[50] NE 0 then do:
          put "Carton Size:" AT 25 
              trim(STRING(itemfg.l-score[50], ">>>,>>9.99<<<<")) + "x" + trim(STRING(itemfg.w-score[50], ">>>,>>9.99<<<<")) + "x" + trim(STRING(itemfg.d-score[50], ">>>,>>9.99<<<<")) FORMAT "x(150)" skip.
          v-printline = v-printline + 1.
        END.
        
        IF AVAIL oe-rel THEN
        if oe-rel.po-no ne "" then do:
            PUT "PO#: " AT 25 
                oe-rel.po-no  SKIP.
            v-printline = v-printline + 1.
        end.

        IF AVAIL eb THEN
        IF eb.i-coldscr NE ""  THEN DO:
            PUT "Colors: " AT 25
            eb.i-coldscr SKIP.
        v-printline = v-printline + 1.
        END.

        IF AVAIL ef THEN
        IF ef.brd-dscr NE " " THEN DO:
            PUT "Material Spec: " AT 25
                ef.brd-dscr SKIP.
            v-printline = v-printline + 1.
        END.

        IF AVAIL eb THEN
        IF eb.style NE "" THEN DO:
            PUT "Style: " AT 25
            eb.style SKIP.
        v-printline = v-printline + 1.
        END.
        
        if v-printline ge 48 then
        do:
           PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P10>  "          
               "<R57><C1>  Your Order is being Manufactured as described herein.  Any changes in this sale will not be valid unless House of"
               "<R58><C1>  Packaging, Inc. confirm them in writing.  All item quantities are subject to an over/under charge of 10%."
               "<=10><R-3>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>".
            PAGE.
            {oe/rep/ackhopx.i}
            assign v-printline = 20.          
        end.

        FOR EACH oe-rel
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND ((oe-rel.link-no EQ 0 AND v-schrel)
               OR  (oe-rel.link-no NE 0 AND v-actrel))
            NO-LOCK BREAK BY oe-rel.link-no DESC WITH FRAME sched-rel DOWN:

            /*if oe-rel.po-no ne "" then do:
                PUT "PO#:" AT 25 
                    oe-rel.po-no  SKIP.
                v-printline = v-printline + 1.
            end.*/
          if v-printline ge 48 then
        do:
            PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P10>  "          
                "<R57><C1>  Your Order is being Manufactured as described herein.  Any changes in this sale will not be valid unless House of"
                "<R58><C1>  Packaging, Inc. confirm them in writing.  All item quantities are subject to an over/under charge of 10%."
                "<=10><R-3>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>".
            PAGE .
            {oe/rep/ackhopx.i}
            assign v-printline = 20.          
        end.
          if first-of(oe-rel.link-no) then
            if oe-rel.link-no eq 0 then lcnt = 1.
            else
            if first(oe-rel.link-no) then lcnt = 1.
  
          if first-of(oe-rel.link-no) then do:
            if oe-rel.link-no eq 0 then do:
              put "Scheduled Releases:" at 10   /*v-printline*/  skip.
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
            IF AVAIL shipto THEN DO:
               if v-printline ge 45 then
               do:
                  PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P10>  "          
                      "<R57><C1>  Your Order is being Manufactured as described herein.  Any changes in this sale will not be valid unless House of"
                      "<R58><C1>  Packaging, Inc. confirm them in writing.  All item quantities are subject to an over/under charge of 10%."
                      "<=10><R-3>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>".
                  PAGE.
                  {oe/rep/ackhopx.i}
                  assign v-printline = 20.          
               end.
                put shipto.ship-name AT 10 SKIP 
                    shipto.ship-addr[1] AT 10 SKIP
                    shipto.ship-addr[2] AT 10 SKIP
                    v-addr4 AT 10 SKIP.
                v-printline = v-printline + 4.
            END.
          end.
        end.   /* for each oe-rel  */
      
      /*  put "" skip.*/
        assign
         v-line = v-line + 1
         v-printline = v-printline + 1.

        if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
           assign v-totlin = oe-ordl.price * IF oe-ordl.qty LT 0 THEN -1 ELSE 1.
                             
        else
        if oe-ordl.pr-uom eq "CS" THEN do:
          /*find first itemfg {sys/look/itemfgrlW.i}
            and itemfg.i-no eq oe-ordl.i-no no-lock no-error.*/

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
           v-totlin = ROUND(v-totlin * (1 - (oe-ordl.disc / 100)),2).

        v-totord = v-totord + v-totlin.
      
        /* print spec notes */
        IF v-prntinst THEN DO:
           find first itemfg {sys/look/itemfgrlW.i}
                and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
    
           IF AVAIL itemfg THEN DO:
              lv-first-note = yes.
           
              {custom/notesprt.i itemfg v-inst 4}
              DO i = 1 TO 4:
                 IF v-inst[i] <> "" THEN DO:
                    if v-printline ge 48 THEN do:
                     PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P10>  "          
                         "<R57><C1>  Your Order is being Manufactured as described herein.  Any changes in this sale will not be valid unless House of"
                         "<R58><C1>  Packaging, Inc. confirm them in writing.  All item quantities are subject to an over/under charge of 10%."
                         "<=10><R-3>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>".
                     PAGE.
                     {oe/rep/ackhopx.i}
                     assign v-printline = 20.          
                    end.
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
        END. /* IF v-prntinst */

      end. /* each oe-ordl */

      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
               oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:
        if first(oe-ordm.ord-no) then
        do:
            if v-printline ge 48 then
        do:
            PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P10>  "          
                "<R57><C1>  Your Order is being Manufactured as described herein.  Any changes in this sale will not be valid unless House of"
                "<R58><C1>  Packaging, Inc. confirm them in writing.  All item quantities are subject to an over/under charge of 10%."
                "<=10><R-3>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>".
            PAGE.
            {oe/rep/ackhopx.i}
            assign v-printline = 20.          
        end.

          put "** Miscellaneous Items **" at 23    .
          if v-print-fmt eq "HOP" then put "Taxable" at 62.
          put skip(1).
          assign v-printline = v-printline + 2.
        end.

        if oe-ordm.bill eq "N" THEN 
            PUT v-line FORM ">>>9" SPACE(3)
             oe-ordm.charge oe-ordm.dscr "     N/C" .          
        ELSE PUT v-line FORM ">>>9" SPACE(3) oe-ordm.charge oe-ordm.dscr oe-ordm.amt.

        PUT SKIP.
        assign v-line = v-line + 1
          v-printline = v-printline + 2.
        if oe-ordm.bill ne "N" then
        assign v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */
      
      assign
       v-totord        = 0
       oe-ord.ack-prnt = yes.
       
      PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P10>  "          
          "<R57><C1>  Your Order is being Manufactured as described herein.  Any changes in this sale will not be valid unless House of"
          "<R58><C1>  Packaging, Inc. confirm them in writing.  All item quantities are subject to an over/under charge of 10%."
          "<P8><R60><C1>  I acknowledge the above quantity, product and pricing are correct." 
          " <R62><C1>______________________________________(Please sign and fax back) " 
          "<=10><R-3>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>" .
      v-printline = v-printline + 6.
      
      page. 
      assign v-printline = 0.
    end. /* each oe-ord */
