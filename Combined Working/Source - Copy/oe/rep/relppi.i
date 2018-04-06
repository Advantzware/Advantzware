/* ----------------------------------------------- oe/rep/relppi.i  */
/* Print OE Release/Picking tickets    Preferred Packaging                    */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i} 
{sys/FORM/r-top.i}

def var v-units-hdr as char format "x(5)" extent 2.
def var v-zone-hdr as char format "x(10)".
def var v-zone like shipto.dest-code.
def var v-part-dscr like oe-ordl.i-name.
def var v-qty like oe-rell.qty.
def var v-frt-pay-dscr as char format "x(11)" no-undo.
/* === with xprint ====*/
DEF VAR v-term AS cha NO-UNDO.
DEF VAR ls-image1 AS cha NO-UNDO.

DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

ASSIGN ls-image1 = "images\ppitop.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-ship-i AS cha FORM "x(60)" EXTENT 4 NO-UNDO.

DEF VAR ll-consol-rells AS LOG NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF BUFFER xitemfg FOR itemfg.
DEF VAR lv-comp-unit AS INT NO-UNDO.
DEF SHARED VAR v-print-components AS LOG NO-UNDO.
DEF SHARED VAR s-print-part-no AS LOG NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "RELPRINT" no-lock no-error.
ASSIGN
 ll-consol-rells = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0
 tmpstore = fill("-",130).

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.

format
  tt-rell.ord-no
  tt-rell.po-no at 8
  locbin[1]  AT 23
  tt-rell.i-no at 29  oe-ordl.i-name at 44
  oe-ordl.qty format "->>>>>>>9" to 83
  tt-rell.qty format "->>>>>>>9" SKIP
  locbin[2] at 23     oe-ordl.part-no
  locbin[3] at 23
  oe-ordl.part-dscr1 at 44 format "x(30)" skip
  locbin[4] at 23
  oe-ordl.part-dscr2 at 44 format "x(30)"
  with down frame relprint no-box no-label STREAM-IO width 110.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
v-printline = 0.

if lookup(v-relprint,"Argrov,Sonoco") gt 0 then
  assign
   v-units-hdr[1] = "Units"
   v-units-hdr[2] = "-----".

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
       v-cases = 0
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
        if avail xoe-ordl then
          assign
           v-tot-qty = v-tot-qty + xoe-rell.qty
           v-weight = v-weight + (if xoe-ordl.t-weight ne ? then
                                  (round(xoe-ordl.t-weight /
                                   xoe-ordl.qty, 2) * xoe-rell.qty) else 0).

        IF ll-consol-rells THEN DO:
          IF (xoe-rell.qty-case * xoe-rell.cases) NE 0 THEN
            RUN create-tt-rell (xoe-rell.qty-case, xoe-rell.cases).

          IF xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases) NE 0 THEN
            RUN create-tt-rell (xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases), 1).
        END.

        ELSE DO:
          CREATE tt-rell.
          BUFFER-COPY xoe-rell EXCEPT rec_key TO tt-rell.
        END.

        ASSIGN
          xoe-rell.printed = YES
          v-partial  = if xoe-rell.partial eq 0 then
		               xoe-rell.qty - (xoe-rell.cases * xoe-rell.qty-case)
		               else xoe-rell.partial
          v-cases  = v-cases + xoe-rell.cases + (if v-partial gt 0 then 1 else 0).
      end. /* each xoe-rell */

      {oe/rep/relppi2.i}
         
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

             display
              tt-rell.ord-no
              tt-rell.po-no
              locbin[1]   
              locbin[2]     oe-ordl.part-no
              locbin[3]
              locbin[4]  when xx ge 4
              tt-rell.i-no
              oe-ordl.qty      when avail oe-ordl
              tt-rell.qty
              oe-ordl.i-name   when avail oe-ordl  SKIP
              oe-ordl.part-dscr1 when avail oe-ordl   /* CTS */
              oe-ordl.part-dscr2 when avail oe-ordl /*bsm*/
              with frame relprint STREAM-IO NO-BOX NO-LABELS WIDTH 120.
              down with frame relprint.
              v-printline = v-printline + 4 + IF xx >= 4 THEN 1 ELSE 0 .
              
          end.
          else do:
            display
              tt-rell.ord-no
              tt-rell.po-no  AT 8
              tt-rell.i-no   AT 29
              /*tt-rell.cases    when v-units-hdr[1] ne "" */
              oe-ordl.qty    TO 83  when avail oe-ordl
              tt-rell.qty
              /*tt-rell.qty-case */
              with frame ln-s-comp STREAM-IO NO-BOX NO-LABELS WIDTH 120.
             v-printline = v-printline + 2.
            down with frame ln-s-comp.
            
            if avail oe-ordl then
            do i = 1 to 3:
              v-part-dscr = if i eq 1 then oe-ordl.i-name
                            else
                            if i eq 2 then oe-ordl.part-dscr1
                            else           oe-ordl.part-dscr2.

              IF i = 1 THEN PUT oe-ordl.part-no at 29.
              if v-part-dscr ne "" then do:
                  put v-part-dscr at 46 skip.
                  v-printline = v-printline + 1.
              END.
              ELSE IF s-print-part-no THEN PUT SKIP.
            end.
          end.
        end.
        else do:
          display
            tt-rell.ord-no
            tt-rell.po-no       AT 8
            tt-rell.loc-bin     AT 23   FORMAT "x(5)"   WHEN v-p-bin
            tt-rell.i-no        AT 29
            oe-ordl.qty      TO 83 when avail oe-ordl
            tt-rell.qty
            with frame ln-s.
          down with frame ln-s STREAM-IO NO-BOX NO-LABELS WIDTH 120.
          v-printline = v-printline + 1.

          if avail oe-ordl then
          do i = 1 to 3:
            v-part-dscr = if i eq 1 then oe-ordl.i-name
                          else
                          if i eq 2 then oe-ordl.part-dscr1
                          else           oe-ordl.part-dscr2.
            
            IF i = 1 THEN PUT oe-ordl.part-no AT 29.
            if v-part-dscr ne "" then do:
               put v-part-dscr at 46 skip.
               v-printline = v-printline + 1.
            END.
            ELSE IF s-print-part-no THEN PUT SKIP.
          end.
        end.

        put skip(1).
        v-printline = v-printline + 1.
        IF LINE-COUNTER > 63 THEN DO:
               PAGE .
               v-printline = 0.
               {oe/rep/relppi2.i}.
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
               ASSIGN lv-comp-unit = trunc((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0).
            ELSE lv-comp-unit = 0.
            v-part-dscr = string(fg-set.part-no,"x(16)") +
		                  (if avail xitemfg then xitemfg.i-name else "").

            IF AVAIL fg-bin THEN DO:
               put lv-comp-unit AT 15  FORM "->>9" " "
                   fg-bin.case-count FORM ">>>>9"
                   v-part-dscr              at 40 format "x(40)"
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
            IF LINE-COUNTER > 63 THEN DO:
               PAGE .
               v-printline = 0.
               {oe/rep/relppi2.i}.
            END.
          end. /* for each fg-set */
       END.  /* end of components display */

        tt-rell.printed = true.        
      end. /* for each tt-rell */

ASSIGN v-ship-i[1] = IF AVAIL oe-relh THEN oe-relh.ship-i[1] ELSE ""
       v-ship-i[2] = IF AVAIL oe-relh THEN oe-relh.ship-i[2] ELSE ""
       v-ship-i[3] = IF AVAIL oe-relh THEN oe-relh.ship-i[3] ELSE ""
       v-ship-i[4] = IF AVAIL oe-relh THEN oe-relh.ship-i[4] ELSE "".

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
  ASSIGN
     v-printline = 0.
  RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
    
    end. /* for each oe-relh */

RETURN.

PROCEDURE create-tt-rell.
  DEF INPUT PARAM ip-qty-case LIKE oe-rell.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-rell.cases NO-UNDO.


  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-rell
      WHERE tt-rell.i-no     EQ xoe-rell.i-no
        AND tt-rell.po-no    EQ xoe-rell.po-no
        AND tt-rell.ord-no   EQ xoe-rell.ord-no
        AND tt-rell.line     EQ xoe-rell.line
        /*AND tt-rell.qty-case EQ ip-qty-case*/
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-rell THEN DO:
    CREATE tt-rell.
    BUFFER-COPY xoe-rell EXCEPT rec_key TO tt-rell
    ASSIGN
     tt-rell.qty-case = ip-qty-case
     tt-rell.cases    = 0
     tt-rell.qty      = 0
     tt-rell.partial  = 0.
  END.

  ASSIGN
   tt-rell.cases = tt-rell.cases + ip-cases
   tt-rell.qty   = tt-rell.qty + (ip-qty-case * ip-cases).

  IF xoe-rell.p-c THEN tt-rell.p-c = YES.

END PROCEDURE.
