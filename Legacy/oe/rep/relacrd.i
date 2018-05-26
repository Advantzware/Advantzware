/* ---------------------------------------------- oe/rep/relacrd.i  03/05 YSK*/
/* Print OE Release/Picking tickets    for Fibre Xprint                         */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/form/r-top.i}
{custom/formtext.i NEW}     
{custom/notesdef.i}

def TEMP-TABLE w-oe-rell NO-UNDO like oe-rell
   field seq    as   int
   field set-no like fg-set.set-no.

def TEMP-TABLE w-bin NO-UNDO
   field w-loc like fg-bin.loc
   field w-bin like fg-bin.loc-bin
   field w-tag like fg-bin.tag
   field w-qty like fg-bin.qty extent 2
   field w-c-c like fg-bin.case-count
   field w-cas as   dec
   field w-pal as   dec
   field w-par like oe-ordl.part-dscr1
   FIELD w-descr AS CHAR FORMAT "X(110)"
   FIELD w-i-no AS cha
   FIELD w-po-no AS cha
   /* gdm - 07070908 */
   FIELD w-subq like fg-bin.qty
    .

def buffer b-cust  for cust.
def buffer b-ship  for shipto.
def buffer b-w-bin for w-bin.
DEF BUFFER ref-lot-no FOR reftable.
DEF BUFFER ref-frt-pay FOR reftable.
DEF BUFFER bf-reftable FOR reftable.

def var v-frt-pay-dscr as char format "x(11)" no-undo.
def var v-bin as char no-undo.
def var v-part-info like itemfg.i-name no-undo.
def var v-qty like oe-rell.qty no-undo.
def var v-rel-qty like v-qty NO-UNDO.

def var v-units-hdr as char format "x(5)" extent 2.
def var v-zone-hdr as char format "x(10)".
def var v-zone like shipto.dest-code.
def var v-part-dscr like oe-ordl.i-name.
/* === with xprint ====*/
DEF VAR lv-pg-num AS INT NO-UNDO.

DEF VAR v-term AS cha NO-UNDO.

DEF SHARED VAR s-print-what-item AS cha NO-UNDO.
DEF SHARED VAR s-print-loc-from AS cha NO-UNDO.
DEF SHARED VAR s-print-loc-to AS cha NO-UNDO.
DEF SHARED VAR s-print-bin-from AS cha NO-UNDO.
DEF SHARED VAR s-print-bin-to AS cha NO-UNDO.
DEF VAR v-transfer AS cha FORM "x(50)" NO-UNDO.
DEF VAR tmp-pal AS DEC NO-UNDO.
DEF VAR ship-flag AS LOG NO-UNDO.
DEF VAR lv-spec-text AS CHAR FORMAT "X(2000)" NO-UNDO.
DEF VAR v-spec-text AS cha FORMAT "X(80)" EXTENT 20 NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.
DEF VAR v-lot-no AS CHAR NO-UNDO.
DEF VAR v-frt-pay AS CHAR NO-UNDO.
DEF VAR v-fob-code AS CHAR NO-UNDO FORM "x(12)".

/* gdm - 07070908 */
DEF VAR v-rel-qty1 LIKE v-rel-qty NO-UNDO.

format w-oe-rell.ord-no                 to 6
       w-par                            at 8    format "x(26)"
       v-bin                            at 36   format "x(20)"
       /*w-x                              at 60   format "X/"*/
       w-pal                            to 61   format "->>>"
       w-cas                            to 68   format "->>>>>"
       w-c-c                            to 78   format "->>>>>>>>"
       w-qty[1]                         to 88   FORMAT "->>>>>>>>"
    with down frame rel-mid no-box no-label STREAM-IO width 97.

format w-oe-rell.ord-no                 to 6
       w-par                            at 8    format "x(26)"
       v-bin                            at 36   format "x(20)"
       w-pal                            to 61   format "->>>>"
       w-cas                            to 67   format "->>>>>"
       w-c-c                            to 76   format "->>>>>>>>"
       w-qty[1]                         to 86   FORMAT "->>>>>>>>"
       w-descr                          at 8    format "x(110)"
    with down frame consol no-box no-label STREAM-IO width 118.

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
DEF VAR v-csr AS cha NO-UNDO.
DEF VAR tmp-cas AS DEC NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR spec-note-count AS INT NO-UNDO.

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
            lv-comp-name = cust.NAME.
 END.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
v-printline = 0.

if v-zone-p then v-zone-hdr = "Route No.:".

    {oe/rep/foreachr.i},
        first cust where cust.company eq cocode
                     and cust.cust-no eq oe-relh.cust-no NO-LOCK
        break by {1} by oe-relh.release#
        TRANSACTION:

      RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).

      assign
       v-tot-qty = 0
       v-weight  = 0
       v-pallets = 0
       v-zone    = shipto.dest-code.

      find first carrier
          where carrier.company eq cocode
            and carrier.carrier eq oe-relh.carrier
          no-lock no-error.
      
      assign
       v-carrier   = if avail carrier then carrier.dscr else ""
       v-frt-terms = ""
       v-frt-pay   = ""
       v-fob-code  = "".

      FOR EACH xoe-rell
          where xoe-rell.company eq oe-relh.company
            and xoe-rell.r-no    eq oe-relh.r-no
          USE-INDEX r-no NO-LOCK,
          FIRST oe-ord
          where oe-ord.company eq xoe-rell.company
            and oe-ord.ord-no  eq xoe-rell.ord-no
          no-lock:
          
          IF xoe-rell.lot-no <> "" THEN
             ASSIGN v-frt-pay = xoe-rell.frt-pay
                    v-fob-code = xoe-rell.fob-code. 
          
        IF v-frt-pay = "" THEN
           ASSIGN v-frt-pay = oe-ord.frt-pay.
        IF v-fob-code = "" THEN
           ASSIGN v-fob-code = oe-ord.fob-code.

        case v-frt-pay:
             when "P" THEN v-frt-terms = "Prepaid".
             when "C" THEN v-frt-terms = "Collect".
             when "B" THEN v-frt-terms = "Bill".
             when "T" THEN v-frt-terms = "Third Party".
        end case.

        IF v-fob-code BEGINS "D" THEN
            ASSIGN v-fob-code = "Destination".
        ELSE IF v-fob-code BEGINS "O" THEN
            ASSIGN v-fob-code = "Origin".

        FIND FIRST oe-rel
        WHERE oe-rel.company EQ xoe-rell.company
          AND oe-rel.ord-no  EQ xoe-rell.ord-no      
          AND oe-rel.i-no    EQ xoe-rell.i-no
          AND oe-rel.line    EQ xoe-rell.line
          AND oe-rel.r-no EQ xoe-rell.link-no
        NO-LOCK NO-ERROR.
 
    IF AVAIL oe-rel THEN do:
       IF oe-rel.s-code <> "" THEN do:
            IF oe-rel.s-code = "T" OR xoe-rell.s-code = "T" THEN
                v-transfer = "<P14>TRANSFER<FGCOLOR=BLACK><P10>".
            ELSE IF oe-rel.s-code EQ "S" THEN
                v-transfer = "<P14>SHIP-ONLY<FGCOLOR=BLACK><P10>".
            ELSE v-transfer = "".
                 LEAVE.
       
        END. 
    END.      
  END.

      /* from relcntbx.p */
      FOR EACH w-oe-rell:
        DELETE w-oe-rell.
      END.

      for each oe-rell NO-LOCK
          where oe-rell.company eq cocode
            and oe-rell.r-no    eq oe-relh.r-no,
          first itemfg of oe-rell no-lock
          break by oe-rell.i-no
                BY oe-rell.ord-no
                by oe-rell.po-no:
          
        create w-oe-rell.
        buffer-copy oe-rell to w-oe-rell.
        
        assign
         i                = 0 
         w-oe-rell.seq    = i
         w-oe-rell.set-no = oe-rell.i-no.
        IF NOT oe-rell.printed THEN DO:
          FIND xoe-rell WHERE ROWID(xoe-rell) EQ ROWID(oe-rell) EXCLUSIVE NO-ERROR NO-WAIT.
          IF AVAIL xoe-rell THEN xoe-rell.printed  = YES.
        END.
        
        if  v-print-components AND itemfg.isaset and itemfg.alloc EQ YES then
        for each fg-set
            where fg-set.company eq cocode
              and fg-set.set-no  eq oe-rell.i-no
            no-lock:
            
          {sys/inc/part-qty.i v-part-qty fg-set}
          
          create w-oe-rell.
          buffer-copy oe-rell to w-oe-rell.
          
          assign
           i                = i + 1
           w-oe-rell.seq    = i
           w-oe-rell.set-no = oe-rell.i-no
           w-oe-rell.i-no   = fg-set.part-no
           w-oe-rell.qty    = w-oe-rell.qty * v-part-qty.
        end.

        if last-of(oe-rell.po-no) then do:
           for each fg-bin where fg-bin.company  eq cocode
               and fg-bin.i-no     eq w-oe-rell.i-no
               AND (s-print-what-item = "R"  
                   OR (s-print-what-item = "I" AND fg-bin.loc >= s-print-loc-from AND fg-bin.loc <= s-print-loc-to
                       AND fg-bin.loc-bin >= s-print-bin-from AND fg-bin.loc-bin <= s-print-bin-to)
                  )
               and fg-bin.qty      gt 0 no-lock:

                 IF s-print-what-item = "R" AND
                    NOT CAN-FIND(FIRST oe-rell
                                   WHERE oe-rell.company  EQ w-oe-rell.company
                                     AND oe-rell.r-no     EQ w-oe-rell.r-no
                                     AND oe-rell.ord-no   EQ w-oe-rell.ord-no
                                     AND oe-rell.i-no     EQ w-oe-rell.i-no
                                     AND oe-rell.line     EQ w-oe-rell.line
                                     AND oe-rell.rel-no   EQ w-oe-rell.rel-no
                                     AND oe-rell.b-ord-no EQ w-oe-rell.b-ord-no
                                     AND oe-rell.po-no    EQ w-oe-rell.po-no
                                     AND oe-rell.loc      EQ fg-bin.loc
                                     AND oe-rell.loc-bin  EQ fg-bin.loc-bin
                                     AND oe-rell.tag      EQ fg-bin.tag)
                 THEN NEXT.

                 IF (w-oe-rell.job-no <> "" and
                    trim(fg-bin.job-no) <> trim(string(w-oe-rell.job-no))) THEN NEXT.

                 /*ln-cnt = ln-cnt + 1. */
           END.
        END.
        v-weight = v-weight + (oe-rell.qty * itemfg.weight-100 / 100).
      end.  /* for each oe-rell*/

      ASSIGN v-csr = oe-relh.user-id
             v-ticket-date = oe-relh.upd-date.

      {oe/rep/relacrd2.i}

      EMPTY TEMP-TABLE w-bin.
      
      for each w-oe-rell,
          first oe-ordl where oe-ordl.company eq cocode
                and oe-ordl.ord-no  eq w-oe-rell.ord-no
                and oe-ordl.i-no    eq w-oe-rell.set-no
                and oe-ordl.line    eq w-oe-rell.LINE no-lock,
            first itemfg of w-oe-rell no-lock
          break by w-oe-rell.set-no
                by w-oe-rell.seq
                by w-oe-rell.i-no
                BY w-oe-rell.ord-no
                by w-oe-rell.po-no:

        ASSIGN v-rel-qty = v-rel-qty + w-oe-rell.qty
               v-rel-qty1 = v-rel-qty1 + w-oe-rell.qty.
        
        if last-of(w-oe-rell.po-no) then do:
            
          EMPTY TEMP-TABLE w-bin.
        
          i = 0.
          for each fg-bin
              where fg-bin.company  eq cocode
                and fg-bin.i-no     eq w-oe-rell.i-no
                AND (s-print-what-item = "R"  
                     OR (s-print-what-item = "I" AND fg-bin.loc >= s-print-loc-from AND fg-bin.loc <= s-print-loc-to
                         AND fg-bin.loc-bin >= s-print-bin-from AND fg-bin.loc-bin <= s-print-bin-to)
                    )
                and fg-bin.qty      gt 0
              no-lock: 
            
            IF s-print-what-item = "R" AND
               NOT CAN-FIND(FIRST oe-rell
                                 WHERE oe-rell.company  EQ w-oe-rell.company
                                   AND oe-rell.r-no     EQ w-oe-rell.r-no
                                   AND oe-rell.ord-no   EQ w-oe-rell.ord-no
                                   AND oe-rell.i-no     EQ w-oe-rell.i-no
                                   AND oe-rell.line     EQ w-oe-rell.line
                                   AND oe-rell.rel-no   EQ w-oe-rell.rel-no
                                   AND oe-rell.b-ord-no EQ w-oe-rell.b-ord-no
                                   AND oe-rell.po-no    EQ w-oe-rell.po-no
                                   AND oe-rell.loc      EQ fg-bin.loc
                                   AND oe-rell.loc-bin  EQ fg-bin.loc-bin
                                   AND oe-rell.tag      EQ fg-bin.tag)
            THEN NEXT.                            

            IF (w-oe-rell.job-no <> "" and
                trim(fg-bin.job-no) <> trim(string(w-oe-rell.job-no))) THEN NEXT.

            IF ll-consol-rells THEN DO:                

              IF s-print-what-item = "I" 
                THEN 
                  FIND w-bin 
                    WHERE w-loc   EQ fg-bin.loc
                    AND   w-bin   EQ fg-bin.loc-bin
                    AND   w-tag   EQ fg-bin.tag
                    AND   w-po-no EQ fg-bin.po-no NO-ERROR.
                 ELSE
                  /*FIND FIRST w-bin NO-ERROR.*/
                  FIND FIRST w-bin
                    WHERE w-loc     EQ fg-bin.loc
                      AND   w-bin   EQ fg-bin.loc-bin
                      /*AND   w-tag   EQ fg-bin.tag
                      AND   w-po-no EQ fg-bin.po-no */ NO-ERROR.

              IF NOT AVAILABLE w-bin THEN DO:

                CREATE w-bin.

                v-lot-no = "".

                 IF oe-rell.lot-no <> "" THEN
                 ASSIGN v-lot-no = w-oe-rell.lot-no.

                ASSIGN w-bin = fg-bin.loc-bin
                       w-loc = fg-bin.loc  /* gdm - 06220906 */
                       i     = i + 1
                       w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-no
                                                     else itemfg.part-no
                       w-descr = if w-oe-rell.seq eq 0 then oe-ordl.i-name + (IF oe-ordl.i-name NE "" THEN CHR(10) ELSE "")
                                                       else itemfg.i-name + (IF itemfg.i-name NE "" THEN CHR(10) ELSE "")
                       w-descr = if w-oe-rell.seq eq 0 then w-descr 
                                                 + (IF oe-ordl.part-dscr1 NE "" THEN FILL(" ",7) ELSE "")
                                                 + oe-ordl.part-dscr1 + (IF oe-ordl.part-dscr1 NE "" THEN CHR(10) ELSE "")
                                                else w-descr
                                                + (IF itemfg.part-dscr1 NE "" THEN FILL(" ",7) ELSE "")
                                                + itemfg.part-dscr1 + (IF itemfg.part-dscr1 NE "" THEN CHR(10) ELSE "")
                       w-descr = w-descr + FILL(" ",7) + w-oe-rell.i-no + CHR(10)
                       w-descr = if w-oe-rell.po-no ne "" then w-descr + "       PO#: " + w-oe-rell.po-no + CHR(10)
                                                          ELSE w-descr
                       w-descr = IF v-lot-no NE "" THEN w-descr + "       LOT #:" + v-lot-no
                                                   ELSE w-descr
                       w-descr = TRIM(w-descr,CHR(10)).
              END.

            END. /* ll-consol-rells*/
            ELSE DO:               
              create w-bin.
              ASSIGN w-tag    = fg-bin.tag
                     w-loc    = fg-bin.loc
                     w-bin    = fg-bin.loc-bin
                     i        = i + 1.
            END.
              
            assign
               w-qty[1] = w-qty[1] + fg-bin.qty
               w-qty[2] = w-qty[2] + fg-bin.qty 
               w-c-c    = w-c-c    + fg-bin.case-count
               w-i-no = fg-bin.i-no
               w-po-no = w-oe-rell.po-no
               tmp-pal = (if fg-bin.case-count eq 0 then 1 else fg-bin.case-count) *
                       (if fg-bin.cases-unit eq 0 then 1 else fg-bin.cases-unit)   *
                       (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
               tmp-pal = fg-bin.qty / tmp-pal
               /* gdm - 07070908 */
               w-subq  = w-subq + w-oe-rell.qty.

            {sys/inc/roundup.i tmp-pal}

            IF ll-consol-rells THEN DO:
               tmp-cas = fg-bin.qty / fg-bin.case-count.
               if tmp-cas eq ? then tmp-cas = 0.
               w-cas = w-cas + tmp-cas.
             END.

            w-pal = w-pal + tmp-pal.
          end. /* for each fg-bin */

          IF NOT ll-consol-rells THEN
            for each w-bin break by w-loc by w-bin:
               v-qty = v-qty + w-qty[1].
               
               if last-of(w-bin) then do:
                 for each b-w-bin
                     where b-w-bin.w-loc eq w-bin.w-loc
                       and b-w-bin.w-bin eq w-bin.w-bin:
                   b-w-bin.w-qty[2] = v-qty.
                 end.
                 
                 v-qty = 0.
               end.
            end.
          
          ship-flag = NO.

          if i eq 0 then do:
            find first b-cust where b-cust.company eq cocode
                       and b-cust.active  eq "X" no-lock no-error.
            if avail b-cust then do:
               find first b-ship
                  where b-ship.company eq cocode
                    and b-ship.cust-no eq b-cust.cust-no
                  no-lock no-error.
              if avail b-ship then do:
                 create w-bin.
                 assign w-loc = b-ship.loc
                        w-bin = b-ship.loc-bin
                        i     = i + 1
                        ship-flag = YES.
              end.   
            end.
          end.
         
          IF NOT ll-consol-rells OR ship-flag THEN DO:
          
            do i = i to 6:
              create w-bin.
            end.
            
            for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
              w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-no
                                            else itemfg.part-no.
              leave.
            end.  
            
            for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
              w-par = if w-oe-rell.seq eq 0 then oe-ordl.i-name
                                            else itemfg.i-name.
              leave.
            end.
            
            for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
              w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-dscr1
                                            else itemfg.part-dscr1.
              leave.
            end.
            
            for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
              w-par = w-oe-rell.i-no.
              leave.
            end.
            
            for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
              if w-oe-rell.po-no ne "" then w-par = "PO#: " + w-oe-rell.po-no.
              leave.
            end.

            v-lot-no = "".

              IF oe-rell.lot-no <> "" THEN
              ASSIGN v-lot-no = w-oe-rell.lot-no.

            for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
              if v-lot-no ne "" then w-par = "Lot #: " + v-lot-no.
              leave.
            end.
          END.

          for each w-bin break by w-qty[2] desc by w-qty[1] desc:            
            if w-par eq "" and w-loc eq "" and w-bin eq "" then delete w-bin.
          end.
          
          IF LINE-COUNTER {2} > 65 THEN DO:
               PAGE .
               v-printline = 0.
               {oe/rep/relacrd2.i}.
          END.
          
          for each w-bin break by w-qty[2] desc by w-qty[1] desc:
              
              IF NOT ll-consol-rells THEN DO:
               w-cas = w-qty[1] / w-c-c.
               if w-cas eq ? then w-cas = 0.
             END.
             
             v-bin = IF NOT ll-consol-rells THEN
                        trim(substr(w-tag,16,5)) + "/" +
                        trim(w-loc)              + "/" +
                        trim(w-bin)
                      ELSE
                        (FILL(" ",9) + "/" + TRIM(w-bin)).

             if trim(v-bin) eq "//" then v-bin = "".

             {sys/inc/roundup.i w-cas}

            IF LINE-COUNTER {2} > 73 THEN DO:
               PAGE .
               v-printline = 0.
               {oe/rep/relacrd2.i}
            END.            
            
            IF NOT ll-consol-rells THEN
              display {2}
                       w-oe-rell.ord-no    when first(w-qty[2])
                       w-par
                       v-bin 
                       /*w-x*/
                       w-pal 
                       w-cas 
                       w-c-c 
                       /* gdm - 07070908 */
                       IF s-print-what-item = "I" 
                         THEN w-qty[1]
                         ELSE w-subq @ w-qty[1]
                       with frame rel-mid.
            ELSE
              display {2}
                      w-oe-rell.ord-no
                      w-par
                      v-bin
                      w-pal 
                      w-cas 
                      w-c-c 
                      /* gdm - 07070908 */
                      IF s-print-what-item = "I" 
                         THEN w-qty[1]
                         ELSE w-subq @ w-qty[1]
                      w-descr FORMAT "X(110)"
                      with frame consol.

            if last(w-qty[2]) then do:
              if w-loc ne "" or w-bin ne "" then DO:
                  IF NOT ll-consol-rells THEN
                    down {2} with frame rel-mid.
                  ELSE
                    down {2} with frame consol.
              END.
                  
              PUT {2} "<B>" "Rel Qty"  at 71 
                      v-rel-qty     AT 79  "</B>" SKIP.
              
              lv-spec-text = "".

              FOR EACH tt-formtext:
                  DELETE tt-formtext.
              END.

              FOR EACH notes FIELDS(note_text) WHERE
                  notes.rec_key EQ itemfg.rec_key AND
                  notes.note_type EQ "S" AND
                  notes.note_code EQ "RN"
                  NO-LOCK:

                  lv-spec-text = lv-spec-text + "<B>" + notes.note_text + "</B>" + CHR(10).
              END.
             
              IF lv-spec-text NE "" THEN
              DO:
                 DO li = 1 TO 20:
                    CREATE tt-formtext.
                    ASSIGN tt-line-no = li
                           tt-length  = 80. 
                 END.
                 RUN custom/formtext.p (lv-spec-text).
                 
                 i = 0.

                 DO v-index = 1 TO 20:
                    IF v-spec-text[v-index] NE "" THEN
                       i = v-index.
                 END.

                 FOR EACH tt-formtext:
                     i = i + 1.
                     IF i <= 20 THEN
                        v-spec-text[i] = tt-formtext.tt-text.      
                 END.
              END.

              IF LINE-COUNTER {2} > 65 THEN DO:
                 PAGE .
                 v-printline = 0.
                 {oe/rep/relacrd2.i}
                 PUT " " SKIP.
              END.

              {oe/rep/relacrd3.i }
              PUT {2} "<#10><From><C+80><LINE><||6><P11>" SKIP. 
            end.            

            IF NOT ll-consol-rells THEN
              down {2} with frame rel-mid.
            ELSE
              down {2} with frame consol.

          end.  /* for eacn w-bin*/

          v-rel-qty = 0.
        end.  /* last-of(w-oe-rell.po-no) */
      end. /* for each w-oe-rell */
  
  spec-note-count = 0.

  DO i = 1 TO 20:
     IF v-spec-text[i] NE "" THEN
        spec-note-count = spec-note-count + 1.
  END.
  
  IF LINE-COUNTER {2} + spec-note-count > 73 THEN DO:
     PAGE .
     v-printline = 0.
     {oe/rep/relacrd2.i}.
  END.

  DO i = 1 TO 20:
     IF v-spec-text[i] NE "" THEN
     DO:
        PUT "<FArial><R" + STRING(55 - spec-note-count) + ">     " + v-spec-text[i] FORM "x(100)" SKIP.
        spec-note-count = spec-note-count - 1.
     END.
  END.

  /* gdm - 06100902 - TO AVOID REPEATING SPEC NOTES PER RELEASE */
  ASSIGN v-spec-text = "".

  lv-pg-num = PAGE-NUM .  

  /* IF v-printline < 45 THEN*/  PAGE. /* PUT SKIP(60 - v-printline). */
  ASSIGN
     v-printline = 0.
  RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
  
end. /* for each oe-relh */

RETURN.
