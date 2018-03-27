/* ----------------------------------------------- oe/rep/oe-pick.i 03/98 JLF */
/* Print OE Release/Picking tickets                                           */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

def var v-units-hdr as char format "x(5)" extent 2.
def var v-zone-hdr as char format "x(10)".
def var v-zone like shipto.dest-code.
def var v-part-dscr like oe-ordl.i-name.
def var lv-relqty like oe-rell.qty.
def var v-frt-pay-dscr as char format "x(11)" no-undo.
/* === with xprint ====*/
DEF VAR v-term AS cha NO-UNDO.
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
DEF VAR v-ship-i AS cha FORM "x(60)" EXTENT 4 NO-UNDO.
DEF VAR lv-job-no AS cha NO-UNDO.
DEF VAR locunit AS INT  FORM ">>9" EXTENT 10 NO-UNDO.
DEF VAR loccount AS INT FORM ">>>>9" EXTENT 10 NO-UNDO.
DEF VAR locjob AS cha EXTENT 10 NO-UNDO.
DEF VAR locpartial AS INT FORM ">>>9" EXTENT 10 NO-UNDO.
DEF VAR locpartial-s AS cha FORM "x(30)" EXTENT 10 NO-UNDO.
DEF BUFFER xitemfg FOR itemfg.
DEF VAR lv-comp-unit AS INT NO-UNDO.
DEF SHARED VAR v-print-components AS LOG NO-UNDO.

/*
format
  oe-rell.ord-no
  correct-po at 12
  oe-rell.i-no at 28
  oe-rell.cases format ">>>>>9" to 48
  oe-ordl.qty format "->>>>>>>9" to 58
  oe-rell.qty format "->>>>>>>9" to 68
  oe-rell.qty-case to 77 skip
  with down frame ln-s-comp no-box no-labels STREAM-IO width 85.
*/

format
  oe-rell.ord-no
 /* oe-rell.po-no at 8 */
  /*oe-rell.cases FORM ">>>>9"  AT 10
  oe-rell.qty-case FORM ">>>>>>9" AT 15 */
  locunit[1] AT 16 loccount[1] AT 19 locbin[1]  AT 25  FORM "x(6)"
  locjob[1] FORM "x(9)" AT 31
  oe-rell.i-no at 40  oe-ordl.i-name at 55 FORM "x(20)"
  /*oe-rell.cases format ">>>>>9" to 73                    */
  oe-ordl.qty format "->>>>>>>9" to 84
  oe-rell.qty format "->>>>>>>9" SKIP
  locpartial-s[1] AT 7 SKIP
  oe-rell.po-no
  locunit[2] AT 16 loccount[2] AT 19 locbin[2] at 25 locjob[2] FORM "x(9)"
  oe-ordl.part-dscr1 at 55 format "x(20)" SKIP
  locpartial-s[2] AT 7 SKIP
  locunit[3] AT 16 loccount[3] AT 19 locbin[3] at 25 locjob[3] FORM "x(9)" 
  oe-ordl.part-dscr2 at 55 format "x(20)" SKIP
  locpartial-s[3] AT 7 SKIP
  locunit[4] AT 16 loccount[4] AT 19 locbin[4] at 25 locjob[4] FORM "x(9)"
  locpartial-s[4] AT 7 SKIP
  with down frame relprint no-box no-label STREAM-IO width 110.
/*
format
  oe-rell.ord-no
  oe-rell.po-no at 12
  oe-rell.i-no at 28
  oe-ordl.qty format "->>>>>>>9" to 58
  oe-rell.qty format "->>>>>>>9" to 68
  with down frame ln-s no-box no-labels STREAM-IO width 85.
*/

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
ASSIGN v-comp-add1 = company.addr[1]
       v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
       v-comp-add3 = "Phone: 604.857.1660" 
       v-comp-add4 = "Fax     : 604.857.1665".
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

        case oe-ord.frt-pay:
             when "P" THEN v-frt-terms = "Prepaid".
             when "C" THEN v-frt-terms = "Collect".
             when "B" THEN v-frt-terms = "Bill".
             when "T" THEN v-frt-terms = "Third Party".
        end case.

        LEAVE.
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

          if /*xoe-ordl.form-no eq 0                             and */
             (xoe-ordl.est-type eq 2 or xoe-ordl.est-type eq 6) then do:
            for each fg-set where fg-set.company eq xoe-ordl.company
                              and fg-set.set-no  eq xoe-ordl.i-no no-lock:
                v-set-qty = v-set-qty + fg-set.part-qty.
            end.
            if v-set-qty eq 0 then v-set-qty = 1.
            for each eb where eb.company eq xoe-ordl.company
                          and eb.est-no  eq xoe-ordl.est-no
                          and eb.form-no ne 0 NO-LOCK:
              find fg-set where fg-set.company eq xoe-ordl.company
                            and fg-set.set-no  eq xoe-ordl.i-no
                            and fg-set.part-no eq eb.stock-no no-lock no-error.

              assign
               v-part-qty = (if avail fg-set and fg-set.part-qty ne 0 then
                            fg-set.part-qty else 1) / v-set-qty.
              
              FIND FIRST fg-bin where fg-bin.company eq cocode
                                  and fg-bin.i-no    eq eb.stock-no
                                  and fg-bin.job-no = xoe-rell.job-no
                                  AND fg-bin.job-no2 = xoe-rell.job-no2 NO-LOCK NO-ERROR.
              IF AVAIL fg-bin THEN
                 v-pallets = v-pallets + trunc((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)  +
                             IF fg-bin.partial-count gt 0 then 1 else 0.


              ELSE ASSIGN
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
          ELSE if avail eb then do:
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
      end. /* each xoe-rell */

      {oe/rep/relpaci2.i}
    
   for each oe-rell
          where oe-rell.company eq cocode
            and oe-rell.r-no    eq oe-relh.r-no BREAK BY oe-rell.i-no:

      IF FIRST-OF(oe-rell.i-no) THEN lv-relqty = 0.
      lv-relqty = + lv-relqty + oe-rell.qty.
     
     ASSIGN v-ship-i = "".

     IF last-OF(oe-rell.i-no) THEN DO:     
        find first oe-rel
            where oe-rel.company  eq cocode
              and oe-rel.ord-no   eq oe-rell.ord-no
              and oe-rel.line     eq oe-rell.line
              and oe-rel.link-no  eq oe-rell.r-no
              and oe-rel.ship-no  eq oe-relh.ship-no
              and
              if v-relprint = "sonoco" then true else
                oe-rel.po-no eq oe-relh.po-no
              and oe-rel.i-no     eq oe-rell.i-no
            no-lock no-error.

        if not avail oe-rel then
        find first oe-rel
            where oe-rel.company  eq cocode
              and oe-rel.ord-no   eq oe-rell.ord-no
              and oe-rel.line     eq oe-rell.line
              and oe-rel.rel-date eq oe-relh.rel-date
              and oe-rel.ship-no  eq oe-relh.ship-no
              and
              if v-relprint = "sonoco" then true else
                oe-rel.po-no    eq oe-relh.po-no
              and oe-rel.i-no     eq oe-rell.i-no
            no-lock no-error.

         

        correct-po = if avail oe-rel and oe-rel.po-no ne "" then oe-rel.po-no
                     else oe-relh.po-no.

        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq oe-rell.ord-no
              and oe-ordl.i-no    eq oe-rell.i-no
              and oe-ordl.line    eq oe-rell.line
            no-lock no-error.

        lv-job-no = IF oe-rell.job-no <> "" THEN (trim(oe-rell.job-no) + "-" + STRING(oe-rell.job-no2,"99"))
                    ELSE "" .

        if v-headers then do:
          find itemfg of oe-rell no-lock no-error.
          do xx = 1 to 10:
             ASSIGN locbin[xx] = ""
                    locunit[xx] = 0
                    loccount[xx] = 0
                    locjob[xx] = ""
                    locpartial[xx] = 0
                    locpartial-s[xx] = "".
          end.
          
          if v-p-bin eq yes and v-relprint ne "Sonoco" then do:
             if avail itemfg then do:
                xx = 0.
                for each fg-bin
                    where fg-bin.company eq cocode
                      and fg-bin.i-no    eq itemfg.i-no
                      and fg-bin.qty > 0
                    no-lock,

                    first fg-rcpth
                    where fg-rcpth.company eq cocode
                      and fg-rcpth.i-no    eq fg-bin.i-no
                      and fg-rcpth.job-no  eq fg-bin.job-no
                      and fg-rcpth.job-no2 eq fg-bin.job-no2
                      and can-find(first fg-rdtlh
                                   where fg-rdtlh.r-no    eq fg-rcpth.r-no
                                     and fg-rdtlh.loc     eq fg-bin.loc
                                     and fg-rdtlh.loc-bin eq fg-bin.loc-bin
                                     and fg-rdtlh.tag     eq fg-bin.tag
                                     AND fg-rdtlh.cust-no EQ fg-bin.cust-no
                                   use-index rm-rdtl)
                    use-index i-no no-lock

                    break by fg-rcpth.trans-date
                          by fg-rcpth.r-no
                          by fg-bin.loc
                          by fg-bin.loc-bin
                          by fg-bin.tag:
               
                    if first-of(fg-bin.tag) then do:
                       xx = xx + 1.
                       IF xx > 4  THEN LEAVE.
                       ASSIGN locbin[xx] = fg-bin.loc-bin
                              locunit[xx] = trunc((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) 
                              loccount[xx] = fg-bin.case-count
                              locjob[xx] = IF fg-bin.job-no <> "" THEN (trim(fg-bin.job-no) + "-" + STRING(fg-bin.job-no2,">>"))
                                           ELSE ""
                              locpartial[xx] = fg-bin.qty - (locunit[xx] * loccount[xx])
                                           .
                      
                    end.
                end.
             end.
             lv-job-no = TRIM(lv-job-no).
             ASSIGN locpartial-s[1] = IF locpartial[1] <> 0 THEN "           1@" + string(locpartial[1],">>>9") ELSE ""
                    locpartial-s[2] = IF locpartial[2] <> 0 THEN "           1@" + string(locpartial[2],">>>9") ELSE ""
                    locpartial-s[3] = IF locpartial[3] <> 0 THEN "           1@" + string(locpartial[3],">>>9") ELSE ""
                    locpartial-s[4] = IF locpartial[4] <> 0 THEN "           1@" + string(locpartial[4],">>>9") ELSE ""
                    .

             DISPLAY oe-rell.ord-no 
                     oe-rell.po-no 
                    /* oe-rell.cases
                     oe-rell.qty-case */
                     locunit[1] WHEN locunit[1] <> 0 loccount[1] WHEN loccount[1] <> 0 locbin[1] locjob[1] 
                     locunit[2] WHEN locunit[2] <> 0 loccount[2] WHEN loccount[2] <> 0 locbin[2] locjob[2]
                     locunit[3] WHEN locunit[3] <> 0 loccount[3] WHEN loccount[3] <> 0 locbin[3] locjob[3]
                     locunit[4] when xx ge 4 loccount[4] when xx ge 4
                     locbin[4] when xx ge 4 locjob[4]  when xx ge 4             
              oe-rell.i-no
              /*oe-rell.cases    when v-units-hdr[1] ne ""*/
              oe-ordl.qty      when avail oe-ordl
              lv-relqty @ oe-rell.qty
              oe-ordl.i-name   when avail oe-ordl  SKIP
              oe-ordl.part-dscr1 when avail oe-ordl   /* CTS */
              oe-ordl.part-dscr2 when avail oe-ordl /*bsm*/
              locpartial-s[1] locpartial-s[2] locpartial-s[3]
              locpartial-s[4] WHEN xx GE 4
              with frame relprint STREAM-IO NO-BOX NO-LABELS WIDTH 120.
              down with frame relprint. 
              v-printline = v-printline + 7.
          end.
          else do:
              
            display
              oe-rell.ord-no
              /*oe-rell.po-no */
              oe-rell.cases
              oe-rell.i-no
              /*oe-rell.cases    when v-units-hdr[1] ne "" */
              oe-ordl.qty      when avail oe-ordl
              lv-relqty @ oe-rell.qty
              oe-rell.qty-case
              with frame ln-s-comp STREAM-IO NO-BOX NO-LABELS WIDTH 120.
            down with frame ln-s-comp.
            v-printline = v-printline + 1.

            if avail oe-ordl then
            do i = 1 to 3:
              v-part-dscr = if i eq 1 then oe-ordl.i-name
                            else
                            if i eq 2 then oe-ordl.part-dscr1
                            else           oe-ordl.part-dscr2.
              if v-part-dscr ne "" then do:
                  PUT v-part-dscr at 28 skip.
                  v-printline = v-printline + 1.
              END.
            end.
          end.
        end.
        else do:
          display
            oe-rell.ord-no
            oe-rell.po-no
            oe-rell.i-no
            oe-ordl.qty      when avail oe-ordl
            lv-relqty @ oe-rell.qty
            with frame ln-s.
          down with frame ln-s STREAM-IO NO-BOX NO-LABELS WIDTH 120.
          v-printline = v-printline + 1.
          if avail oe-ordl then
          do i = 1 to 3:
            v-part-dscr = if i eq 1 then oe-ordl.i-name
                          else
                          if i eq 2 then oe-ordl.part-dscr1
                          else           oe-ordl.part-dscr2.
            if v-part-dscr ne "" then do:
                PUT v-part-dscr at 28 skip.
                v-printline = v-printline + 1.
            END.
          end.
        end.
        /*
        put skip(1).
        v-printline = v-printline + 1.
        */
        IF v-print-components THEN DO: /* display componets of set */

          IF NOT AVAIL itemfg THEN find itemfg of oe-rell no-lock no-error.
          if AVAIL itemfg AND itemfg.isaset then
          for each fg-set where fg-set.company eq cocode
	                        and fg-set.set-no  eq itemfg.i-no   no-lock:

            find first xitemfg where xitemfg.company eq cocode
	                           and xitemfg.i-no    eq fg-set.part-no no-lock no-error.
            FIND FIRST fg-bin where fg-bin.company eq cocode
                              and fg-bin.i-no    eq xitemfg.i-no
                              and fg-bin.job-no = oe-rell.job-no
                              AND fg-bin.job-no2 = oe-rell.job-no2 NO-LOCK NO-ERROR.
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
                   /*lv-relqty /*oe-rell.qty*/ * v-part-qty*/
                   fg-bin.qty TO 94 format "->>>>>>>9"
	               skip.
               v-printline = v-printline + 1.
               IF fg-bin.partial-count <> 0 THEN DO:
                  PUT "  1" AT 16 "@" fg-bin.partial-count FORM ">>>>9" SKIP.          
                  v-printline = v-printline + 1.
               END.
            END.
            ELSE PUT v-part-dscr AT 40 FORM "x(40)".
                
            IF LINE-COUNTER > 63 THEN DO:
               PAGE .
               v-printline = 0.
               {oe/rep/relpaci2.i}.
            END.
          end. /* for each fg-set */
       END.  /* end of components display */

       IF LINE-COUNTER > 63 THEN DO:
          PAGE.
          {oe/rep/relpaci2.i}.
       END.
        oe-rell.printed = true.

     END. /* last-of(i-no) */

   end. /* for each oe-rell */

  ASSIGN v-ship-i[1] = oe-relh.ship-i[1] 
         v-ship-i[2] = oe-relh.ship-i[2]
                v-ship-i[3] = oe-relh.ship-i[3]
                v-ship-i[4] = oe-relh.ship-i[4].
                      


PUT "<FArial><R50><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP(1)
    v-ship-i[1] AT 7 SKIP
    v-ship-i[2] AT 7 SKIP
    v-ship-i[3] AT 7 SKIP
    v-ship-i[4] AT 7 SKIP(1)
    "__________________________________________________________________________________________________________________" SKIP
    "<|10><C1><R59><#8><FROM><C80><R61><RECT> " skip
    "<=8> Pulled By                                         Checked By                                        # of Units                                         Total Weight/Cube" SKIP
    "<R59><C20><FROM><R61><C20><Line>" SKIP
    "<R59><C40><FROM><R61><C40><Line>" SKIP
    "<R59><C60><FROM><R61><C60><Line>" SKIP
    .
             
  v-printline = v-printline + 14.
 /*
  IF v-printline < 45 THEN PUT SKIP(60 - v-printline).
  v-printline = 0.
  */
  PAGE.
/*
      if v-relprint eq "Sonoco" and avail oe-rel then do:
        if oe-rel.ship-i[1] ne "" then
          put oe-rel.ship-i[1] format "x(60)" at /* 28 */ 2 skip.
        if oe-rel.ship-i[2] ne "" then
          put oe-rel.ship-i[2] format "x(60)" at /* 28 */ 2 skip.
        if oe-rel.ship-i[3] ne "" then
          put oe-rel.ship-i[3] format "x(60)" at /* 28 */ 2 skip.
        if oe-rel.ship-i[4] ne "" then
          put oe-rel.ship-i[4] format "x(60)" at /* 28 */ 2 skip.
        put skip(1).
      end.
  */
      RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).    
    end. /* for each oe-relh */
