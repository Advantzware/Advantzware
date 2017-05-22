/* ---------------------------------------------- ar/rep/invnstok.p */
/* PRINT INVOICE   Xprint form for Hughes                           */
/* ---------------------------------------------------------------- */

{sys/inc/var.i shared}

{ar/rep/invoice.i}

DEFINE VARIABLE v-salesman           AS CHARACTER              FORMAT "x(14)" NO-UNDO.
DEFINE VARIABLE v-fob                AS CHARACTER              FORMAT "x(27)" NO-UNDO.
DEFINE VARIABLE v-shipvia            LIKE carrier.dscr           NO-UNDO.
DEFINE VARIABLE v-addr3              AS CHARACTER              FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-sold-addr3         AS CHARACTER              FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-shipto-name        AS CHARACTER              FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-shipto-addr        AS CHARACTER              FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-shipto-city        AS CHARACTER              FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-shipto-state       AS CHARACTER              FORMAT "x(2)" NO-UNDO.
DEFINE VARIABLE v-shipto-zip         AS CHARACTER              FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-line               AS INTEGER                NO-UNDO.
DEFINE VARIABLE v-printline          AS INTEGER                NO-UNDO.
DEFINE VARIABLE v-t-weight           LIKE inv-line.t-weight      NO-UNDO.
DEFINE VARIABLE v-inv-no             AS INTEGER                NO-UNDO.
DEFINE VARIABLE v-tot-cas            AS DECIMAL                FORMAT "->>>9.9999" NO-UNDO.
DEFINE VARIABLE v-tot-pallets        AS INTEGER                NO-UNDO.
DEFINE VARIABLE v-tot-qty            AS INTEGER                NO-UNDO.
DEFINE VARIABLE v-inv-date           AS DATE                   INITIAL TODAY FORM "99/99/9999" NO-UNDO.
DEFINE SHARED VARIABLE v-fr-tax             AS LOGICAL                INITIAL NO NO-UNDO.
DEFINE VARIABLE v-tax-rate           AS DECIMAL                FORMAT "->>>.99" NO-UNDO.
DEFINE VARIABLE v-tax-code           LIKE stax.tax-code          NO-UNDO.
DEFINE VARIABLE v-tx-rate            LIKE stax.tax-rate          NO-UNDO.
DEFINE VARIABLE v-ans                AS LOGICAL                INITIAL NO NO-UNDO.
DEFINE VARIABLE v-date-ship          AS DATE                   INITIAL TODAY NO-UNDO.
DEFINE VARIABLE v-del-no             AS INTEGER                FORMAT ">>>>>>" NO-UNDO.
DEFINE VARIABLE v-bol-cases          LIKE oe-boll.cases          NO-UNDO.
DEFINE VARIABLE v-set-qty            AS INTEGER                NO-UNDO.
DEFINE VARIABLE v-part-qty           AS DECIMAL                FORMAT "999.9999" NO-UNDO.
DEFINE VARIABLE v-net                LIKE inv-head.t-inv-rev     NO-UNDO.
DEFINE VARIABLE v-case-cnt           AS CHARACTER              FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-case-line          AS CHARACTER              NO-UNDO.
DEFINE VARIABLE v-part-line          AS CHARACTER              NO-UNDO.
DEFINE VARIABLE tmp1                 AS DECIMAL                NO-UNDO.
DEFINE VARIABLE tmp2                 AS DATE                   NO-UNDO.
DEFINE VARIABLE net1                 AS DECIMAL                NO-UNDO.
DEFINE VARIABLE net2                 AS DECIMAL                NO-UNDO.
DEFINE VARIABLE net3                 AS DECIMAL                NO-UNDO.
DEFINE VARIABLE cnt                  AS INTEGER                NO-UNDO.
DEFINE VARIABLE disp-frt             AS CHARACTER              INIT "Freight:" FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE minus-ship           AS INTEGER                NO-UNDO.
DEFINE VARIABLE decr3                AS CHARACTER              FORMAT "x(30)" NO-UNDO.

DEF BUFFER xar-inv FOR ar-inv.
DEF BUFFER xitemfg FOR itemfg.
DEFINE VARIABLE v-part-dscr          AS cha                    NO-UNDO.
DEFINE VARIABLE v-enum               LIKE inv-line.e-num         NO-UNDO.

DEF WORKFILE w-sman
FIELD sman                 AS CHARACTER              FORMAT "x(4)".

DEFINE VARIABLE v-ord-del-hdr        AS CHARACTER              FORMAT "x(3)" INIT "Del".
DEFINE VARIABLE v-beeler-lines       AS INTEGER.
DEFINE VARIABLE v-part-info          AS CHARACTER              FORMAT "x(30)".
DEFINE VARIABLE v                    AS INTEGER.
DEFINE VARIABLE v-bo-qty             AS INTEGER                FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-inv-qty            AS INTEGER                FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-ship-qty           AS INTEGER                FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-i-no               AS CHARACTER              FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-i-dscr             AS CHARACTER              FORMAT "x(18)" NO-UNDO.
DEFINE VARIABLE v-price              AS DECIMAL                FORMAT ">>>>9.9999" NO-UNDO.
DEFINE VARIABLE v-t-price            AS DECIMAL                FORMAT ">>>>>>9.99" NO-UNDO.
DEFINE VARIABLE v-po-no              LIKE inv-line.po-no         NO-UNDO.
DEFINE VARIABLE v-bill-i             AS CHARACTER              FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ord-no             LIKE oe-ord.ord-no          NO-UNDO.
DEFINE VARIABLE v-ord-date           LIKE oe-ord.ord-date        NO-UNDO.
DEFINE VARIABLE v-ship-i             AS CHARACTER              FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-rel-po-no          LIKE oe-rel.po-no           NO-UNDO.
DEFINE VARIABLE v-price-head         AS CHARACTER              FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE v-subtot-lines       AS DECIMAL                NO-UNDO.
DEF WORKFILE w-tax
FIELD w-dsc                AS CHARACTER
FIELD w-tax                AS DECIMAL.
DEFINE VARIABLE v-t-tax              AS DECIMAL                EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bot-lab            AS CHARACTER              FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-lines              AS INTEGER                NO-UNDO.
DEFINE VARIABLE v-inv-freight        LIKE inv-head.t-inv-freight NO-UNDO.
DEFINE VARIABLE v-frt-tax            AS DECIMAL                NO-UNDO.
DEFINE VARIABLE v-ordered-qty        AS INTEGER                FORM ">>>>9" NO-UNDO.

FIND FIRST ar-inv NO-LOCK NO-ERROR.

DEFINE VARIABLE v-tel                AS cha                    FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax                AS cha                    FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact            AS cha                    FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1          AS cha                    FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2          AS cha                    FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3          AS cha                    FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4          AS cha                    FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-invoice-with-order AS LOGICAL                NO-UNDO.
DEFINE VARIABLE v-show-parts         AS LOGICAL                NO-UNDO.

{fg/fullset.i NEW}


find first company where company.company = cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

for each report where report.term-id eq v-term-id no-lock,
  first ar-inv where recid(ar-inv) eq report.rec-id no-lock,
  FIRST cust WHERE cust.company = ar-inv.company
  AND cust.cust-no = ar-inv.cust-no NO-LOCK
  
  
  break by ar-inv.cust-no
  by ar-inv.inv-no:
  
  FIND FIRST reftable WHERE
  reftable.reftable EQ "cust.show-set" AND
  reftable.company  EQ cust.company AND
  reftable.loc      EQ "" AND
  reftable.code     EQ cust.cust-no
  NO-LOCK NO-ERROR.
  
  IF NOT AVAIL reftable OR
  (AVAIL reftable AND reftable.val[1] = 1) THEN
  v-show-parts = YES.
  ELSE
  v-show-parts = NO.
  
  IF ar-inv.ord-no <> 0 THEN v-invoice-with-order = YES.
  
  find first carrier where carrier.company eq cocode
  and carrier.carrier eq ar-inv.carrier no-lock no-error.
  if avail carrier THEN ASSIGN v-shipvia = carrier.dscr.
  else assign v-shipvia = "".
  
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
  v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
  v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
  "  " + v-shipto-zip.
  
  if ar-inv.fob-code begins "ORIG" THEN assign v-fob = "Origin".
  ELSE assign v-fob = "Destination".
  
  ASSIGN v-line = 1
  v-printline = 0
  v-del-no = 0.
  
  find first stax {sys/ref/stax1W.i}
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
  
  assign v-tot-pallets = 0.
  for each ar-invl NO-LOCK where ar-invl.x-no  eq ar-inv.x-no
    break by ar-invl.i-no:
    do i = 1 to 3:
      if ar-invl.sman[i] ne "" then do:
        create w-sman.
        assign w-sman.sman = ar-invl.sman[i].
      end.
    end.
    assign v-tot-qty = v-tot-qty + ar-invl.ship-qty
    v-t-weight = v-t-weight + (round(ar-invl.t-weight /
    ar-invl.qty, 2) * ar-invl.inv-qty)
    v-tot-pallets = 0.
    
    IF ar-invl.ord-no <> 0 THEN v-invoice-with-order = YES.
    if last-of(ar-invl.i-no) then do:
      if ar-invl.est-no ne "" then
      do:
        find first eb where eb.company = ar-invl.company and
        eb.est-no = ar-invl.est-no and
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
            ELSE ASSIGN v-part-qty = 1 / v-set-qty.
            
            IF eb.cas-cnt = 0 THEN
            ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) / eb.cas-wt, 2).
            ELSE ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
            eb.cas-cnt, 2).
            if v-bol-cases ne 0 then
            assign v-tot-cas = v-bol-cases.
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
        END. /* do */
      end. /* est-no ne "" */
      ASSIGN v-t-weight = 0
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
  
  assign v-rel-po-no = ar-inv.po-no
  v-bill-i = ar-inv.bill-i[1]
  v-ord-no = ar-inv.ord-no
  v-ord-date = ar-inv.ord-date
  v-inv-date = ar-inv.inv-date.
  
  find first ar-invl where ar-invl.x-no = ar-inv.x-no no-lock no-error.
  if avail ar-invl then
  do:
    assign v-price-head = ar-invl.pr-uom
    v-po-no = ar-invl.po-no
    v-ord-no = ar-invl.ord-no.
    
    FIND FIRST oe-bolh WHERE oe-bolh.b-no = ar-invl.b-no AND
    oe-bolh.ord-no = ar-invl.ord-no NO-LOCK NO-ERROR.
    v-date-ship = IF AVAIL oe-bolh THEN oe-bolh.bol-date ELSE ar-inv.inv-date.
  end.
  
  find first ar-invl
  where ar-invl.x-no  eq ar-inv.x-no
  and ar-invl.po-no ne ""
  no-lock no-error.
  if avail ar-invl then v-rel-po-no = ar-invl.po-no.
  
  IF v-salesman = "" THEN v-salesman = cust.sman.
  {ar/rep/invnstok.i}
  
  v-subtot-lines = 0.
  v-t-tax = 0.
  for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no:
    
    assign v-case-line = ""
    v-part-line = ""
    v-case-cnt = ""
    v-price-head = ar-invl.pr-uom.
    
    v-beeler-lines = 0.
    
    IF v-printline > 48 THEN DO:
      PAGE.
      v-printline = 0.
      {ar/rep/invnstok.i}
    END.
    
    find first oe-ordl where oe-ordl.company = cocode and
    oe-ordl.ord-no = ar-invl.ord-no and
    oe-ordl.i-no = ar-invl.i-no AND
    oe-ordl.LINE = ar-invl.LINE
    no-lock no-error.
    ASSIGN v-ordered-qty = 0 .
    FOR EACH oe-rel WHERE oe-rel.company EQ oe-ordl.company AND
      oe-rel.ord-no  EQ oe-ordl.ord-no AND
      oe-rel.i-no    EQ oe-ordl.i-no   AND
      oe-rel.line    EQ oe-ordl.line NO-LOCK:
      ASSIGN v-ordered-qty = v-ordered-qty + oe-rel.qty .
    END.
    
    if avail oe-ordl then
    assign v-bo-qty = if (ar-invl.qty - ar-invl.ship-qty -
    oe-ordl.t-ship-qty) < 0 then 0 else
    (ar-invl.qty - ar-invl.ship-qty -
    oe-ordl.t-ship-qty)
    /*v-ordered-qty = oe-ordl.qty */.
    else
    assign v-bo-qty = if ( ar-invl.qty - ar-invl.ship-qty ) < 0
    then 0 else ar-invl.qty - ar-invl.ship-qty
    /*v-ordered-qty = ar-invl.qty */.
    
    assign v-inv-qty = ar-invl.inv-qty
    v-ship-qty = ar-invl.ship-qty
    v-i-no = ar-invl.i-no
    v-i-dscr = ar-invl.i-name
    v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
    v-t-price = ar-invl.amt
    v-subtot-lines = v-subtot-lines + ar-invl.amt     .
    
    
    if ar-invl.tax and avail stax then
    do i = 1 to 3:
      if stax.tax-code[i] ne ""  then do:
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
    
    if v-t-price ne ar-invl.amt      then do:
      create w-tax.
      assign
      w-dsc     = "******ITEM TOTAL:"
      w-tax     = v-t-price
      v-lines   = v-lines + 1.
    end.
    
    IF NOT (NOT v-show-parts AND AVAIL oe-ordl AND
    oe-ordl.is-a-component) THEN
    DO:
      PUT space(1) v-ordered-qty format "->>>>>9" SPACE(3)
      v-ship-qty  format "->>>>>9" SPACE(1)
      v-inv-qty FORM "->>>>>9" SPACE(3)
      v-i-dscr  format "x(30)" SPACE(3)
      v-enum FORM ">>9" SPACE(1)
      v-price  format ">>,>>9.99" SPACE(2)
      v-price-head SPACE(1)
      ar-invl.amt  format "->>>,>>9.99"
      SKIP.
      
      v-printline = v-printline + 1.
      
      IF v-printline > 48 THEN DO:
        PAGE.
        v-printline = 1.
        {ar/rep/invnstok.i}
      END.
      ASSIGN decr3 = "".
      FIND first itemfg where itemfg.company eq ar-invl.company
      and itemfg.i-no    eq ar-invl.i-no NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN DO:
        ASSIGN decr3 = itemfg.part-dscr3.
      END.
      
      do v = 1 to 3:
        v-part-info = if v eq 1 then (IF ar-invl.part-dscr1 <> "" THEN ar-invl.part-dscr1 ELSE ar-invl.i-dscr)
        ELSE if v EQ 2 then ar-invl.part-dscr2
        ELSE  decr3 .
        
        if v-part-info ne "" then do:
          put space(29) v-part-info skip.
          v-printline = v-printline + 1.
        end.
      end.
      put skip(1).
      v-printline = v-printline + 2.
      IF v-printline > 48 THEN DO:
        PAGE.
        v-printline = 0.
        {ar/rep/invnstok.i}
      END.
    END.
    
    /* display componets of set */
    
    IF v-show-parts THEN
    DO:
      
      if AVAIL itemfg AND itemfg.isaset THEN DO:
        RUN fg/fullset.p (ROWID(itemfg)).
        
        FOR EACH tt-fg-set:
          find first xitemfg where xitemfg.company eq cocode
          and xitemfg.i-no    eq tt-fg-set.part-no no-lock no-error.
          v-part-dscr = string(tt-fg-set.part-no,"x(16)") +
          (if avail xitemfg then xitemfg.i-name else "").
          
          PUT v-part-dscr                        at 11 format "x(46)"
          v-inv-qty * tt-fg-set.part-qty-dec to 73 format ">>>>9"
          skip.
          v-printline = v-printline + 1.
          IF v-printline > 44 THEN DO:
            PAGE.
            v-printline = 1.
            {ar/rep/invnstok.i}
            PUT SKIP.
          END.
        END.
      END.
    END.
  end. /* each inv-line */
/* wfk - this was duplicated at the bottom       */
/*   if v-prntinst then do:                      */
/*     do i = 1 to 4:                            */
/*       if ar-inv.bill-i[i] ne "" then do:      */
/*                                               */
/*         IF v-printline > 48 THEN DO:          */
/*           PAGE.                               */
/*           v-printline = 0.                    */
/*           {ar/rep/invnstok.i}                 */
/*         END.                                  */
/*         put ar-inv.bill-i[i] at 10 skip.      */
/*         assign v-printline = v-printline + 1. */
/*       end.                                    */
/*     end. /* 1 to 4 */                         */
/*   end.                                        */
  
  v-frt-tax = ar-inv.freight.
  IF ar-inv.tax-code <> "" and
  (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
  AND ar-inv.freight <> 0
  AND AVAIL stax THEN
  do i = 1 to 3:
    
    if stax.tax-frt[i]  AND stax.tax-frt1[i] then do:
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
    
    ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)")
    ELSE FILL(" ",5) ) +
    fill(" ",6) + ":" +
    string(v-t-tax[i],"->>>>>9.99")) else "".
  end.
  
  v-inv-freight = if (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
  THEN ar-inv.freight ELSE 0.
  PUT "<R57><C60><#8><FROM><R+5><C+20><RECT> "
  "<=8> Sub Total  :" v-subtot-lines FORM "->>,>>9.99"
  "<=8><R+1> Freight    :" v-inv-freight
  "<=8><R+2> " v-bot-lab[1]
  "<=8><R+3> " v-bot-lab[2]
  "<=8><R+4> Grand Total:" ar-inv.gross FORM "->>,>>9.99" .
  
  PUT "<FArial><R57><C1><P12><B> Comments </B> <P9> " SKIP
  ar-inv.bill-i[1] SKIP
  ar-inv.bill-i[2] SKIP
  ar-inv.bill-i[3] SKIP
  ar-inv.bill-i[4].
  
  v-printline = v-printline + 6.
  PAGE.
  
  DO TRANSACTION:
    FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
    ASSIGN xar-inv.printed = yes.
  END. /* DO TRANSACTION avail ar-inv */
  
end. /* each ar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
