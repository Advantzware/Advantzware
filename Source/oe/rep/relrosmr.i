/* ------------------------------------------ oe/rep/relrosmr.i 10080912 GDM */
/* RELEASE PRINT  Program for N-K-1 RELPRINT = Rosmar                        */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.             */
/* ------------------------------------------------------------------------- */

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
DEF VAR v-ship-i AS cha FORM "x(60)" EXTENT 4 NO-UNDO.

DEF VAR ll-display-comp AS LOG NO-UNDO.  /* display company address */
DEF VAR ll-consol-rells AS LOG NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(56)" NO-UNDO.

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
DEF VAR v-char-1 AS CHAR NO-UNDO.
DEF VAR v-char-2 AS CHAR NO-UNDO.
DEF VAR v-is-num-1 AS LOG NO-UNDO.
DEF VAR v-is-num-2 AS LOG NO-UNDO.

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
  tt-rell.po-no AT 10
  tt-rell.loc-bin  AT 25  FORM "x(5)"
  tt-rell.i-no AT 31  oe-ordl.i-name at 46
  oe-ordl.qty format "->>>>>>>9" to 83
  tt-rell.qty format "->>>>>>>9" SKIP
  tt-rell.tag AT 31 FORM "x(15)"
  oe-ordl.part-dscr1 at 46 format "x(30)" 
  tt-rell.partial  TO 83 
  lv-partial FORM ">>>" TO 93  skip
  oe-ordl.part-no AT 31
  oe-ordl.part-dscr2 at 46 format "x(30)"
  
  with down frame relprint no-box no-label STREAM-IO width 110.

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
        v-frt-terms = IF xoe-rell.frt-pay NE "" THEN xoe-rell.frt-pay ELSE oe-ord.frt-pay.
        case v-frt-terms:
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
              v-set-qty = v-set-qty + fg-set.QtyPerSet.
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
               v-part-qty = (if avail fg-set and fg-set.QtyPerSet ne 0 then
                             fg-set.QtyPerSet else 1) / v-set-qty
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
            RUN create-tt-rell (xoe-rell.qty-case, xoe-rell.cases).

          IF xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases) NE 0 THEN
            RUN create-tt-rell (xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases), 1).
        END.

        ELSE DO:
          CREATE tt-rell.
          BUFFER-COPY xoe-rell EXCEPT rec_key TO tt-rell.

          ASSIGN
             v-char-1 = SUBSTRING(tt-rell.loc-bin,1,1)
             v-char-2 = SUBSTRING(tt-rell.loc-bin,2,1).

          RUN isNumber(v-char-1, OUTPUT v-is-num-1).
          RUN isNumber(v-char-2, OUTPUT v-is-num-2).

          IF v-is-num-1 AND NOT v-is-num-2 THEN
             tt-rell.sort-loc-bin = "0" + tt-rell.loc-bin.
          ELSE
             tt-rell.sort-loc-bin = tt-rell.loc-bin.
        END.
        lv-tot-cases = lv-tot-cases + xoe-rell.cases.
        IF xoe-rell.partial > 0 THEN lv-tot-cases = lv-tot-cases + 1.
        xoe-rell.printed = YES.
      end. /* each xoe-rell */

      {oe/rep/relrosm1.i}
         
      IF NOT v-sort-loc-bin THEN
         for each tt-rell
             BY tt-rell.i-no
             BY tt-rell.po-no
             BY tt-rell.ord-no
             BY tt-rell.line
             BY tt-rell.cases DESC:
        
	          RUN inner-loop-proc.        
         end. /* for each tt-rell */
     ELSE /*sort by loc-bin*/
        for each tt-rell
             BY tt-rell.sort-loc-bin
             BY tt-rell.i-no
             BY tt-rell.po-no
             BY tt-rell.ord-no
             BY tt-rell.line
             BY tt-rell.cases DESC:
        
	          RUN inner-loop-proc.        
         end. /* for each tt-rell */
     

ASSIGN
 v-ship-i[1] = oe-relh.ship-i[1]
 v-ship-i[2] = oe-relh.ship-i[2]
 v-ship-i[3] = oe-relh.ship-i[3]
 v-ship-i[4] = oe-relh.ship-i[4].

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
    "<R58><C60><FROM><R60><C60><Line>".
             
  ASSIGN
    v-printline = v-printline + 14
    v-printline = 0.

  RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
    
  PAGE.

end. /* for each oe-relh */

RETURN.

/**************************************************************************/
PROCEDURE inner-loop-proc:

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
         lv-partial = IF tt-rell.partial > 0 THEN 1 ELSE 0.         
         display
          tt-rell.ord-no
          tt-rell.po-no
          tt-rell.loc-bin /*locbin[1]*/   
         /* locbin[2]
          locbin[3]
          locbin[4]  when xx ge 4 */
          tt-rell.i-no
          /*tt-rell.cases    when v-units-hdr[1] ne ""*/
          tt-rell.qty-case @ oe-ordl.qty  TO 83
          tt-rell.cases @ tt-rell.qty      /*when avail oe-ordl*/
          
          oe-ordl.i-name   when avail oe-ordl  SKIP
          IF trim(tt-rell.tag) NE "" 
            THEN SUBSTR(TRIM(tt-rell.tag),LENGTH(TRIM(tt-rell.tag)) - 5) 
            ELSE tt-rell.tag @ tt-rell.tag
          oe-ordl.part-no WHEN s-print-part-no
          oe-ordl.part-dscr1 when avail oe-ordl   /* CTS */
          oe-ordl.part-dscr2 when avail oe-ordl /*bsm*/    
          tt-rell.partial WHEN tt-rell.partial > 0
          lv-partial WHEN lv-partial <> 0  
          with frame relprint STREAM-IO NO-BOX NO-LABELS WIDTH 120.
          down with frame relprint.
          v-printline = v-printline + 4 + IF xx >= 4 THEN 1 ELSE 0 .
          
      end.
      else do:
        display
          tt-rell.ord-no
          tt-rell.po-no  AT 10
          tt-rell.i-no   AT 31
          /*tt-rell.cases    when v-units-hdr[1] ne "" */
          tt-rell.qty-case @ oe-ordl.qty  TO 83
          tt-rell.cases @ tt-rell.qty SKIP
          /*tt-rell.qty-case */
          with frame ln-s-comp STREAM-IO NO-BOX NO-LABELS WIDTH 120.
         v-printline = v-printline + 2.
        down with frame ln-s-comp.
                                       
        if avail oe-ordl then
        do i = 1 to 3:
          lv-partial = IF i = 1 AND tt-rell.partial > 0 THEN 1 ELSE 0.
          v-part-dscr = if i eq 1 then oe-ordl.i-name
                        else if i eq 2 then oe-ordl.part-dscr1
                        else   oe-ordl.part-dscr2.
          IF i = 1 THEN 
            IF trim(tt-rell.tag) NE ""
              THEN PUT SUBSTR(TRIM(tt-rell.tag),LENGTH(TRIM(tt-rell.tag)) - 5) AT 31.
              ELSE IF s-print-part-no THEN PUT oe-ordl.part-no AT 31.
          if v-part-dscr ne "" then put v-part-dscr at 46 FORM "x(25)" .
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
        tt-rell.po-no       AT 10
        tt-rell.loc-bin     AT 25   FORMAT "x(5)"   WHEN v-p-bin
        tt-rell.i-no        AT 31
        tt-rell.qty-case @ oe-ordl.qty   TO 83
        tt-rell.cases @ tt-rell.qty   SKIP        
        with frame ln-s.
      down with frame ln-s STREAM-IO NO-BOX NO-LABELS WIDTH 120.
      v-printline = v-printline + 1.
   
      if avail oe-ordl then
      do i = 1 to 3:
          lv-partial = IF i = 1 AND tt-rell.partial > 0 THEN 1 ELSE 0.
          v-part-dscr = if i eq 1 then oe-ordl.i-name
                        else if i eq 2 then oe-ordl.part-dscr1
                        else   oe-ordl.part-dscr2.
          IF i = 1 THEN 
           IF trim(tt-rell.tag) NE ""
            THEN PUT SUBSTR(TRIM(tt-rell.tag),LENGTH(TRIM(tt-rell.tag)) - 5) AT 31.
            ELSE IF s-print-part-no THEN PUT oe-ordl.part-no AT 31.
          if v-part-dscr ne "" then put v-part-dscr at 46 FORM "x(25)" .
          IF lv-partial > 0 THEN PUT tt-rell.partial TO 83 lv-partial TO 93.              
          IF s-print-part-no OR lv-partial > 0 OR v-part-dscr <> "" THEN DO: 
             PUT SKIP.
             v-printline = v-printline + 1.
          END.
        end.
    end.
   
    put skip(1).
    v-printline = v-printline + 1.
    IF LINE-COUNTER > 63 THEN DO:
           PAGE .
           v-printline = 0.
           {oe/rep/relrosm1.i}.
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
           put lv-comp-unit AT 17  FORM "->>9" " "
               fg-bin.case-count FORM ">>>>9"
               v-part-dscr              at 42 format "x(40)"
               /*lv-relqty /*tt-rell.qty*/ * v-part-qty*/
               fg-bin.qty TO 94 format "->>>>>>>9"
	           skip.
           v-printline = v-printline + 1.
           IF fg-bin.partial-count <> 0 THEN DO:
              PUT "  1" AT 18 "@" fg-bin.partial-count FORM ">>>>9" SKIP.          
              v-printline = v-printline + 1.
           END.
        END.
        ELSE do:
            PUT v-part-dscr AT 42 FORM "x(40)" SKIP.
            v-printline = v-printline + 1.
        END.
        IF LINE-COUNTER > 63 THEN DO:
           PAGE .
           v-printline = 0.
           {oe/rep/relrosm1.i}.
        END.
      end. /* for each fg-set */
   END.  /* end of components display */
   
   tt-rell.printed = true.
   
END PROCEDURE.

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
        AND tt-rell.qty-case EQ ip-qty-case
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-rell THEN DO:
    CREATE tt-rell.
    BUFFER-COPY xoe-rell EXCEPT rec_key TO tt-rell
    ASSIGN
     tt-rell.qty-case = ip-qty-case
     tt-rell.cases    = 0
     tt-rell.qty      = 0
     tt-rell.partial  = 0.

    ASSIGN
       v-char-1 = SUBSTRING(tt-rell.loc-bin,1,1)
       v-char-2 = SUBSTRING(tt-rell.loc-bin,2,1).

    RUN isNumber(v-char-1, OUTPUT v-is-num-1).
    RUN isNumber(v-char-2, OUTPUT v-is-num-2).

    IF v-is-num-1 AND NOT v-is-num-2 THEN
       tt-rell.sort-loc-bin = "0" + tt-rell.loc-bin.
    ELSE
       tt-rell.sort-loc-bin = tt-rell.loc-bin.
  END.

  ASSIGN
   tt-rell.cases = tt-rell.cases + ip-cases
   tt-rell.qty   = tt-rell.qty + (ip-qty-case * ip-cases).

  IF xoe-rell.p-c THEN tt-rell.p-c = YES.

END PROCEDURE.

PROCEDURE isNumber:
   DEF INPUT PARAMETER ip-string AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER op-IsNum AS LOG NO-UNDO.

   IF ip-string GE CHR(48) AND
      ip-string LE CHR(57) THEN
      op-IsNum = TRUE.

END PROCEDURE.
