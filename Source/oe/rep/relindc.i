/* ----------------------------------------------- oe/rep/relindc.i  01/05 YSK*/
/* Print OE Release/Picking tickets    for Indian Carton                      */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

def var v-units-hdr as char format "x(5)" extent 2 NO-UNDO.
def var v-zone-hdr as char format "x(10)" NO-UNDO.
def var v-zone like shipto.dest-code NO-UNDO.
def var v-part-dscr like oe-ordl.i-name NO-UNDO.
def var v-qty like oe-rell.qty NO-UNDO.
def var v-frt-pay-dscr as char format "x(11)" no-undo.
/* === with xprint ====*/
DEF VAR v-term AS cha NO-UNDO.
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF SHARED VAR s-print-pricing AS LOG NO-UNDO.

ASSIGN ls-image1 = "images\icc.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

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
DEF VAR v-ship-i AS cha FORM "x(60)" EXTENT 4 NO-UNDO.

DEF VAR ll-display-comp AS LOG NO-UNDO.  /* display company address */
DEF VAR ll-consol-rells AS LOG NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF BUFFER xitemfg FOR itemfg.
DEF VAR lv-comp-unit AS INT NO-UNDO.
DEF SHARED VAR v-print-components AS LOG NO-UNDO.
DEF SHARED VAR s-print-part-no AS LOG NO-UNDO.

DEF VAR lv-qty-case AS INT NO-UNDO.
DEF VAR lv-cases AS INT NO-UNDO.
DEF VAR lv-partial AS INT NO-UNDO.
DEF VAR lv-tot-cases LIKE v-pallets NO-UNDO.
DEF VAR v-tag AS cha NO-UNDO.
DEF VAR lv-price AS DEC DECIMALS 4 NO-UNDO.
DEF VAR lv-ext-price AS DEC NO-UNDO.
DEF VAR lv-price-string AS CHAR NO-UNDO.
DEF VAR v-total AS DEC NO-UNDO.

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "RELPRINT" no-lock no-error.
ASSIGN
 ll-display-comp = AVAIL sys-ctrl AND sys-ctrl.log-fld
 ll-consol-rells = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0.

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

IF ll-display-comp THEN DO:
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

format
  tt-rell.ord-no
  tt-rell.po-no at 9
  /*tt-rell.loc-bin  AT 23  FORM "x(5)"*/
  tt-rell.i-no at 29  oe-ordl.i-name at 44
  oe-ordl.qty format "->>>>>>>9" to 83
  tt-rell.qty format "->>>>>>>9" SKIP
  with down frame relprint no-box no-label STREAM-IO width 110.

FORMAT
  tt-rell.loc-bin  AT 9  FORM "x(9)"
  v-tag AT 29 FORM "x(15)"
  oe-ordl.part-dscr1 at 44 format "x(30)" 
  tt-rell.partial  TO 83 
  lv-partial FORM ">>>" TO 93  skip
    oe-ordl.part-no AT 29
    oe-ordl.part-dscr2 at 44 format "x(30)"
  
  with down frame relprint-2 no-box no-label STREAM-IO width 110.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
v-printline = 0.

if v-zone-p then v-zone-hdr = "Route No.:".

    {oe/rep/foreachr.i},

        first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-relh.cust-no
        no-lock

        break by {1} by oe-relh.release#:

      RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).

      assign
       v-tot-qty = 0
       v-weight  = 0
       v-pallets = 0
       v-total   = 0
       v-zone    = if v-zone-p then shipto.dest-code else "".

      find first carrier
          where carrier.company eq cocode
            and carrier.carrier eq oe-relh.carrier
          no-lock no-error.
      
      assign
       v-carrier   = if avail carrier then carrier.dscr else ""
       v-frt-terms = "".

      FOR EACH xoe-rell
          where xoe-rell.company eq oe-relh.company
            and xoe-rell.r-no    eq oe-relh.r-no
          USE-INDEX r-no NO-LOCK,
          FIRST oe-ord
          where oe-ord.company eq xoe-rell.company
            and oe-ord.ord-no  eq xoe-rell.ord-no
          no-lock:

        case oe-ord.frt-pay:
             when "P" THEN v-frt-terms = "Prepaid".
             when "C" THEN v-frt-terms = "Collect".
             when "B" THEN v-frt-terms = "Bill".
             when "T" THEN v-frt-terms = "Third Party".
        end case.

        LEAVE.
      END.

      FOR EACH tt-rell:
        DELETE tt-rell.
      END.

      /** Calculate the total weight of the released items. **/
      for each xoe-rell
          where xoe-rell.company eq cocode
            and xoe-rell.r-no    eq oe-relh.r-no:
        find first xoe-ordl
            where xoe-ordl.company eq cocode
              and xoe-ordl.ord-no  eq xoe-rell.ord-no
              and xoe-ordl.line    eq xoe-rell.line
              and xoe-ordl.i-no    eq xoe-rell.i-no
            use-index ord-no no-lock no-error.
        if avail xoe-ordl THEN DO:
           FIND itemfg WHERE itemfg.company = cocode
                         AND itemfg.i-no = xoe-ordl.i-no NO-LOCK NO-ERROR.
          assign
           v-tot-qty = v-tot-qty + xoe-rell.qty
           v-weight = v-weight + /*(if xoe-ordl.t-weight ne ? then
                                  /*(round(xoe-ordl.t-weight /
                                   xoe-ordl.qty, 2) * xoe-rell.qty) else 0)*/
                                 ((xoe-ordl.t-weight / xoe-ordl.qty) * xoe-rell.qty) else 0)*/
                                (xoe-rell.qty / 100) * itemfg.weight-100.
        END.
        if avail xoe-ordl and xoe-ordl.est-no ne "" then do:
          find first eb
              where eb.company  eq xoe-ordl.company
                and eb.est-no   eq xoe-ordl.est-no
                and eb.form-no  eq xoe-ordl.form-no
                and eb.blank-no eq xoe-ordl.blank-no
              no-lock no-error.

          if xoe-ordl.form-no eq 0                             and
             (xoe-ordl.est-type eq 2 or xoe-ordl.est-type eq 6) then do:
            for each fg-set
                where fg-set.company eq xoe-ordl.company
                  and fg-set.set-no  eq xoe-ordl.i-no
                no-lock:
              v-set-qty = v-set-qty + fg-set.part-qty.
            end.
            if v-set-qty eq 0 then v-set-qty = 1.
            for each eb
                where eb.company eq xoe-ordl.company
                  and eb.est-no  eq xoe-ordl.est-no
                  and eb.form-no ne 0
                no-lock:
              find fg-set
                  where fg-set.company eq xoe-ordl.company
                    and fg-set.set-no  eq xoe-ordl.i-no
                    and fg-set.part-no eq eb.stock-no
                  no-lock no-error.

              assign
               v-part-qty = (if avail fg-set and fg-set.part-qty ne 0 then
                             fg-set.part-qty else 1) / v-set-qty
               v-pallets = v-pallets +
                           (if xoe-rell.qty-case ne 0 then
                              round((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                            else
                            if eb.cas-cnt ne 0 then
                              round((round((v-tot-qty * v-part-qty) /
                                         eb.cas-cnt, 2) / eb.cas-pal) + .49, 0)
                            else
                              round((round((v-weight * v-part-qty) /
                                         eb.cas-wt, 2) / eb.cas-pal) + .49, 0)).
            end. /* each eb */
          end. /* do */
          else
          if avail eb then do:
            assign
             v-pallets = v-pallets +
                         (if xoe-rell.qty-case ne 0 then
                            round((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                          else
                          if eb.cas-cnt ne 0 then
                            round((round(v-tot-qty / eb.cas-cnt, 2) /
                                                       eb.cas-pal) + .49, 0)
                          else
                            round((round(v-weight / eb.cas-wt, 2) /
                                                       eb.cas-pal) + .49, 0)).
          end. /* do */
        end. /* est-no ne "" */

        IF ll-consol-rells THEN DO:
          IF (xoe-rell.qty-case * xoe-rell.cases) NE 0 THEN
            RUN create-tt-rell (xoe-rell.qty-case, xoe-rell.cases, xoe-rell.qty).

          IF xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases) NE 0 THEN
            RUN create-tt-rell (xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases), 1, 0).
        END.

        ELSE DO:
          CREATE tt-rell.
          BUFFER-COPY xoe-rell TO tt-rell.

          FIND FIRST xitemfg of tt-rell no-lock no-error.

          IF AVAIL xitemfg THEN
          DO:
            IF xitemfg.q-onh EQ 0 THEN
               tt-rell.loc-bin = "FLOOR".
            RELEASE xitemfg.
          END.
        END.

        lv-tot-cases = lv-tot-cases + xoe-rell.cases.
        IF xoe-rell.partial > 0 THEN lv-tot-cases = lv-tot-cases + 1.
        xoe-rell.printed = YES.
      end. /* each xoe-rell */

      {oe/rep/relindc2.i}
         
      for each tt-rell
          BY tt-rell.i-no
          BY tt-rell.po-no
          BY tt-rell.ord-no
          BY tt-rell.line
          BY tt-rell.cases DESC:

	    find first oe-rel
	        where oe-rel.company  eq tt-rell.company
	          and oe-rel.ord-no   eq tt-rell.ord-no
	          and oe-rel.line     eq tt-rell.line
	          and oe-rel.link-no  eq tt-rell.r-no
	          and oe-rel.ship-id  eq oe-relh.ship-id
	          and oe-rel.po-no    eq tt-rell.po-no
	          and oe-rel.i-no     eq tt-rell.i-no
	        no-lock no-error.

	    if not avail oe-rel then
	      find first oe-rel
    	      where oe-rel.company  eq tt-rell.company
	    	    and oe-rel.ord-no   eq tt-rell.ord-no
		        and oe-rel.line     eq tt-rell.line
	   	        and oe-rel.rel-date eq oe-relh.rel-date
    		    and oe-rel.ship-id  eq oe-relh.ship-id
		        and oe-rel.po-no    eq tt-rell.po-no
		        and oe-rel.i-no     eq tt-rell.i-no
	          no-lock no-error.

        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq tt-rell.ord-no
              and oe-ordl.i-no    eq tt-rell.i-no
              and oe-ordl.line    eq tt-rell.line
            no-lock no-error.
   
        if v-headers then do:
          find itemfg of tt-rell no-lock no-error.
          locbin = "".
          v-tag = oe-ordl.part-no /*tt-rell.tag */.
          if v-p-bin then do:
             if avail itemfg then do:
                xx = 0.
                for each fg-bin
                    where fg-bin.company  eq cocode
                      and fg-bin.i-no     eq itemfg.i-no
                      and fg-bin.qty      gt 0 
                    no-lock
                    break by fg-bin.loc-bin:
                   if first-of(fg-bin.loc-bin) then do:
                      xx = xx + 1.
                      IF xx LE EXTENT(locbin) THEN locbin[xx] = fg-bin.loc-bin.
                   end.
                end.
             end.
             lv-partial = IF tt-rell.partial > 0 THEN 1 ELSE 0.

             display
              tt-rell.ord-no
              tt-rell.po-no
              tt-rell.i-no
              tt-rell.qty-case @ oe-ordl.qty
              tt-rell.cases @ tt-rell.qty
              oe-ordl.i-name   when avail oe-ordl  SKIP
              with frame relprint STREAM-IO NO-BOX NO-LABELS WIDTH 120.
              down with frame relprint.

              IF s-print-pricing THEN
              DO:
                 RUN calc-ext-cost(OUTPUT lv-price, OUTPUT lv-ext-price).
                 ASSIGN
                 lv-price-string = TRIM(STRING(lv-price,"-ZZ,ZZZ,ZZ9.9999") + "/" + oe-ordl.pr-uom)
                 v-total = v-total + lv-ext-price
                 v-printline = v-printline + 1.
                 PUT lv-price-string AT 44 FORM "X(20)" lv-ext-price FORM "->>,>>>,>>9.99" TO 93.
              END.

              DISPLAY
               tt-rell.loc-bin  
               v-tag
               oe-ordl.part-no WHEN s-print-part-no
               oe-ordl.part-dscr1 when avail oe-ordl   /* CTS */
               oe-ordl.part-dscr2 when avail oe-ordl /*bsm*/    
               tt-rell.partial WHEN tt-rell.partial > 0
               lv-partial WHEN lv-partial <> 0
               with frame relprint-2 STREAM-IO NO-BOX NO-LABELS WIDTH 120.
               down with frame relprint-2.

              v-printline = v-printline + 4 + IF xx >= 4 THEN 1 ELSE 0 .
          end.
          else do:

            display
              tt-rell.ord-no
              tt-rell.po-no  AT 9
              tt-rell.i-no   AT 29
              tt-rell.qty-case @ oe-ordl.qty TO 83
              tt-rell.cases @ tt-rell.qty 
              with frame ln-s-comp STREAM-IO NO-BOX NO-LABELS WIDTH 120.
             v-printline = v-printline + 2.
            down with frame ln-s-comp.
                                           
            IF s-print-pricing THEN
               DO:
                  RUN calc-ext-cost(OUTPUT lv-price, OUTPUT lv-ext-price).
                  ASSIGN
                  lv-price-string = TRIM(STRING(lv-price,"-ZZ,ZZZ,ZZ9.9999") + "/" + oe-ordl.pr-uom)
                  v-total = v-total + lv-ext-price
                  v-printline = v-printline + 1.
                  PUT lv-price-string AT 44 FORM "X(20)" lv-ext-price FORM "->>,>>>,>>9.99" TO 93.
               END.

            if avail oe-ordl then
            do i = 1 to 3:
              lv-partial = IF i = 1 AND tt-rell.partial > 0 THEN 1 ELSE 0.
              v-part-dscr = if i eq 1 then oe-ordl.i-name
                            else if i eq 2 then oe-ordl.part-dscr1
                            else   oe-ordl.part-dscr2.
              IF i = 1 AND s-print-part-no THEN PUT oe-ordl.part-no AT 29.              
              if v-part-dscr ne "" then put v-part-dscr at 44 FORM "x(25)" .
              IF lv-partial > 0 THEN PUT tt-rell.partial TO 83 lv-partial TO 93.              
              IF s-print-part-no OR lv-partial > 0 OR v-part-dscr <> "" THEN DO: 
                 PUT SKIP.
                 v-printline = v-printline + 1.
              END.
              
            end.
          end.
        end.  /* v-header */
        else do:

          display
            tt-rell.ord-no
            tt-rell.po-no       AT 9
            /*tt-rell.loc-bin     AT 23   FORMAT "x(5)"   WHEN v-p-bin*/
            tt-rell.i-no        AT 29
            tt-rell.qty-case @ oe-ordl.qty TO 83
            tt-rell.cases @ tt-rell.qty
            with frame ln-s.
          down with frame ln-s STREAM-IO NO-BOX NO-LABELS WIDTH 120.
          v-printline = v-printline + 1.

          IF s-print-pricing AND AVAIL oe-ordl THEN
          DO:
             RUN calc-ext-cost(OUTPUT lv-price, OUTPUT lv-ext-price).
             ASSIGN
             lv-price-string = TRIM(STRING(lv-price,"-ZZ,ZZZ,ZZ9.9999") + "/" + oe-ordl.pr-uom)
             v-total = v-total + lv-ext-price
             v-printline = v-printline + 1.
             PUT lv-price-string AT 44 FORM "X(20)" lv-ext-price FORM "->>,>>>,>>9.99" TO 93.
          END.

          if avail oe-ordl then
          DO:
             do i = 1 to 3:
                 lv-partial = IF i = 1 AND tt-rell.partial > 0 THEN 1 ELSE 0.
                 v-part-dscr = if i eq 1 then oe-ordl.i-name
                               else if i eq 2 then oe-ordl.part-dscr1
                               else   oe-ordl.part-dscr2.
                 IF i = 1 AND v-p-bin THEN PUT tt-rell.loc-bin     AT 9   FORMAT "x(9)" .
                 IF i = 1 AND s-print-part-no THEN PUT oe-ordl.part-no AT 29.              
                 if v-part-dscr ne "" then put v-part-dscr at 44 FORM "x(25)" .
                 IF lv-partial > 0 THEN PUT tt-rell.partial TO 83 lv-partial TO 93.              
                 IF s-print-part-no OR lv-partial > 0 OR v-part-dscr <> "" THEN DO: 
                    PUT SKIP.
                    v-printline = v-printline + 1.
                 END.
             end.
          END.
        end.

        /*put skip(1).
        v-printline = v-printline + 1.*/
        IF LINE-COUNTER > 55 THEN DO:
               PAGE .
               v-printline = 0.
               {oe/rep/relindc2.i}.
        END.

        IF v-print-components THEN DO: /* display componets of set */

          IF NOT AVAIL itemfg THEN find itemfg of tt-rell no-lock no-error.
          if AVAIL itemfg AND itemfg.isaset then
          for each fg-set where fg-set.company eq cocode
	                    and fg-set.set-no  eq itemfg.i-no   no-lock:

            find first xitemfg where xitemfg.company eq cocode
	                           and xitemfg.i-no    eq fg-set.part-no no-lock no-error.
            FIND FIRST fg-bin where fg-bin.company eq cocode
                              and fg-bin.i-no    eq xitemfg.i-no
                              and fg-bin.job-no = tt-rell.job-no
                              AND fg-bin.job-no2 = tt-rell.job-no2 NO-LOCK NO-ERROR.
            IF AVAIL fg-bin THEN
               ASSIGN lv-comp-unit = trunc((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) 
                 /*lv-comp-partial = fg-bin.qty - (lv-comp-unit * fg-bin.case-count)*/
                 .
            ELSE lv-comp-unit = 0.
            v-part-dscr = string(fg-set.part-no,"x(16)") +
		                  (if avail xitemfg then xitemfg.i-name else "").

            {sys/inc/part-qty.i v-part-qty fg-set}
            IF AVAIL fg-bin THEN DO:
               put lv-comp-unit AT 15  FORM "->>9" " "
                   fg-bin.case-count FORM ">>>>9"
                   v-part-dscr              at 40 format "x(40)"
                   /*lv-relqty /*tt-rell.qty*/ * v-part-qty*/
                   fg-bin.qty TO 94 format "->>>>>>>9"
	               skip.
               v-printline = v-printline + 1.
               IF fg-bin.partial-count <> 0 THEN DO:
                  PUT "  1" AT 16 "@" fg-bin.partial-count FORM ">>>>9" SKIP.          
                  v-printline = v-printline + 1.
               END.
            END.
            ELSE do:
                PUT v-part-dscr AT 40 FORM "x(40)" SKIP.
                v-printline = v-printline + 1.
            END.
            IF LINE-COUNTER > 55 THEN DO:
               PAGE .
               v-printline = 0.
               {oe/rep/relindc2.i}.
            END.
          end. /* for each fg-set */
       END.  /* end of components display */

        tt-rell.printed = true.        
      end. /* for each tt-rell */

IF s-print-pricing THEN
   PUT "Total:" AT 73 v-total FORM "->>,>>>,>>9.99" TO 93.

ASSIGN v-ship-i[1] = IF AVAIL oe-rel THEN oe-rel.ship-i[1] ELSE ""
       v-ship-i[2] = IF AVAIL oe-rel THEN oe-rel.ship-i[2] ELSE ""
       v-ship-i[3] = IF AVAIL oe-rel THEN oe-rel.ship-i[3] ELSE ""
       v-ship-i[4] = IF AVAIL oe-rel THEN oe-rel.ship-i[4] ELSE "".

PUT "<FArial><R50><C1><P12><B>     Shipping Instructions: </B> <P9> "SKIP(1)
    "<R51><C1>" v-ship-i[1] AT 7 
    "<R52><C1>" v-ship-i[2] AT 7 
    "<R53><C1>" v-ship-i[3] AT 7 
    "<R54><C1>" v-ship-i[4] AT 7
    "<R56><C1>"
    "__________________________________________________________________________________________________________________"  SKIP 

    "<|10><C1><R58><#8><FROM><C80><R60><RECT> " 
    "<=8> Pulled By                                         Checked By                                        # of Units                                         Total Weight/Cube" SKIP
    "<R58><C20><FROM><R60><C20><Line>" 
    "<R58><C40><FROM><R60><C40><Line>" 
    "<R58><C60><FROM><R60><C60><Line>" 
    .
             
  v-printline = v-printline + 14.
 
 /* IF v-printline < 45 THEN*/  PAGE. /* PUT SKIP(60 - v-printline). */
      v-printline = 0.
      RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
    
    end. /* for each oe-relh */

RETURN.

PROCEDURE create-tt-rell.
  DEF INPUT PARAM ip-qty-case LIKE oe-rell.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-rell.cases NO-UNDO.
  DEF INPUT PARAM ip-qty      LIKE oe-rell.qty NO-UNDO.

  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1
     ip-qty      = ip-qty * -1.

  FIND FIRST tt-rell
      WHERE tt-rell.i-no     EQ xoe-rell.i-no
        AND tt-rell.po-no    EQ xoe-rell.po-no
        AND tt-rell.ord-no   EQ xoe-rell.ord-no
        AND tt-rell.line     EQ xoe-rell.line
        AND tt-rell.qty-case EQ ip-qty-case
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-rell THEN DO:
    
    CREATE tt-rell.
    BUFFER-COPY xoe-rell TO tt-rell
    ASSIGN
     tt-rell.qty-case = ip-qty-case
     tt-rell.cases    = 0
     tt-rell.qty      = 0
     tt-rell.partial  = 0.

    FIND FIRST xitemfg of tt-rell no-lock no-error.

    IF AVAIL xitemfg THEN
    DO:
       IF xitemfg.q-onh EQ 0 THEN
          tt-rell.loc-bin = "FLOOR".
       RELEASE xitemfg.
    END.
  END.

  ASSIGN
   tt-rell.cases = tt-rell.cases + ip-cases
   tt-rell.qty   = tt-rell.qty + ip-qty.

  IF xoe-rell.p-c THEN tt-rell.p-c = YES.

END PROCEDURE.

PROCEDURE calc-ext-cost:

   DEF OUTPUT PARAMETER op-tmp-price AS DEC DECIMALS 4 NO-UNDO.
   DEF OUTPUT PARAMETER op-t-price AS DEC NO-UNDO.

   DEF VAR lv-tmp-price AS DEC DECIMALS 4 NO-UNDO.
   DEF VAR lv-price LIKE oe-ordl.price NO-UNDO.

   if (tt-rell.qty ne 0 and oe-ordl.pr-uom ne "") then
   do:
      find first itemfg
           {sys/look/itemfgrl.w} AND
           itemfg.i-no eq oe-ordl.i-no
           no-lock no-error.
           
      FIND FIRST reftable WHERE
           reftable.reftable EQ "oe-rell.sell-price" AND
           reftable.rec_key  EQ tt-rell.rec_key
           USE-INDEX rec_key
           NO-LOCK NO-ERROR.

      IF AVAIL reftable AND reftable.val[1] NE 0 THEN
         lv-price = reftable.val[1].
      ELSE
         lv-price = oe-ordl.price.

      RELEASE reftable.

      assign
       lv-tmp-price = if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
                       if tt-rell.qty lt 0 then -1 else 1
                       else
                       if oe-ordl.pr-uom eq "CS" then
                          tt-rell.qty / (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                                         if avail itemfg and itemfg.case-count ne 0
                                                         then itemfg.case-count else
                                                              1)
                     else
                     if oe-ordl.pr-uom eq "C" then
                       tt-rell.qty / 100
                     else
                     if oe-ordl.pr-uom eq "M" then
                       tt-rell.qty / 1000
                     else
                       tt-rell.qty
                                
        op-t-price = lv-tmp-price * lv-price
        op-tmp-price = lv-price
        op-t-price = ROUND(op-t-price * (1 - (oe-ordl.disc / 100)),2).

   end.
END PROCEDURE.
