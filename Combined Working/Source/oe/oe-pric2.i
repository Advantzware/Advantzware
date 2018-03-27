/* -------------------------------------------------- oe/oe-pric2.i 11/96 JLF */
/*                                                                            */
/* order entry - ITEM PRICING FROM PRICE MATRIX                               */
/*                   FOR STOCK BOXES ONLY                                     */
/* -------------------------------------------------------------------------- */

find first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq v-i-item
    no-lock no-error.

if avail itemfg and itemfg.i-code eq "S" then do:
  assign
   {1}.i-no   = itemfg.i-no
   {1}.pr-uom = if {1}.pr-uom eq "" then itemfg.sell-uom else {1}.pr-uom.
   
  IF NOT price-ent THEN DO:
    ASSIGN
     {1}.price  = itemfg.sell-price
     {1}.pr-uom = itemfg.sell-uom.

    IF sellpric-cha EQ "LastPric" THEN DO:
      ldt = 01/01/0001.

      FOR EACH b-oe-ordl
          WHERE b-oe-ordl.company EQ {1}.company
            AND b-oe-ordl.i-no    EQ {1}.i-no
            AND ROWID(b-oe-ordl)  NE ROWID({1})
          NO-LOCK,
          FIRST b-oe-ord
          WHERE b-oe-ord.company  EQ b-oe-ordl.company
            AND b-oe-ord.ord-no   EQ b-oe-ordl.ord-no
            AND b-oe-ord.cust-no  EQ x{2}.cust-no
            AND b-oe-ord.ord-date GT ldt
          NO-LOCK
          BY b-oe-ord.ord-date DESC:
        LEAVE.
      END.
      IF AVAIL b-oe-ordl THEN
        ASSIGN
         ldt        = b-oe-ord.ord-date
         {1}.price  = b-oe-ordl.price
         {1}.pr-uom = b-oe-ordl.pr-uom.

      FOR EACH b-ar-invl
          WHERE b-ar-invl.company EQ {1}.company
            AND b-ar-invl.i-no    EQ {1}.i-no
            AND ROWID(b-ar-invl)  NE ROWID({1})
          NO-LOCK,
          FIRST b-ar-inv
          WHERE b-ar-inv.x-no     EQ b-ar-invl.x-no
            AND b-ar-inv.cust-no  EQ x{2}.cust-no
            AND b-ar-inv.inv-date GT ldt
          NO-LOCK
          BY b-ar-inv.inv-date:
        LEAVE.
      END.
      IF AVAIL b-ar-invl THEN
        ASSIGN
         ldt        = b-ar-inv.inv-date
         {1}.price  = b-ar-invl.unit-pr
         {1}.pr-uom = b-ar-invl.pr-qty-uom.

      FOR EACH b-inv-line
          WHERE b-inv-line.company EQ {1}.company
            AND b-inv-line.i-no    EQ {1}.i-no
            AND ROWID(inv-line)    NE ROWID({1})
          NO-LOCK,
          FIRST b-inv-head
          WHERE b-inv-head.r-no     EQ b-inv-line.r-no
            AND b-inv-head.cust-no  EQ x{2}.cust-no
            AND b-inv-head.inv-date GT ldt
          NO-LOCK
          BY b-inv-head.inv-date:
        LEAVE.
      END.
      IF AVAIL b-inv-line THEN
        ASSIGN
         ldt        = b-inv-head.inv-date
         {1}.price  = b-inv-line.price
         {1}.pr-uom = b-inv-line.pr-uom.
    END.
  END.

  if itemfg.part-no ne "" and {1}.est-no eq "" then
    {1}.part-no = itemfg.part-no.

  if itemfg.i-name ne "" then
    {1}.i-name = itemfg.i-name.

  if itemfg.i-dscr ne "" then
    {1}.i-dscr = itemfg.i-dscr.

  if itemfg.part-dscr1 ne "" then
    {1}.part-dscr1 = itemfg.part-dscr1.

  if itemfg.part-dscr2 ne "" then
    {1}.part-dscr2 = itemfg.part-dscr2.

  if {1}.est-no eq "" then do:
    {1}.cost = itemfg.total-std-cost.
    
    if itemfg.prod-uom ne "M" then
      run sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                             {1}.cost, output {1}.cost).
  end.
                         
  RELEASE oe-prmtx.

  FOR EACH oe-prmtx
      {oe/oe-prmtxW.i}
        AND oe-prmtx.custype             EQ cust.type
        AND oe-prmtx.cust-no             EQ x{2}.cust-no
        AND oe-prmtx.procat              EQ itemfg.procat
        AND oe-prmtx.i-no                BEGINS v-i-item
        AND SUBSTR(oe-prmtx.i-no,01,100) EQ v-i-item
        AND oe-prmtx.eff-date  LE        TODAY
        AND (oe-prmtx.exp-date GE        TODAY OR oe-prmtx.exp-date EQ ?
              OR oe-prmtx.exp-date EQ 01/01/0001 )
      BY oe-prmtx.eff-date DESC:
/*         AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date  */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:         */
    LEAVE.
  END.

  IF sellpric-cha NE "LastPric" THEN DO:
    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype             EQ cust.type
          AND oe-prmtx.cust-no             EQ ""
          AND oe-prmtx.procat              EQ itemfg.procat
          AND oe-prmtx.i-no                BEGINS v-i-item
          AND SUBSTR(oe-prmtx.i-no,01,100) EQ v-i-item
          AND oe-prmtx.eff-date LE        TODAY
        AND (oe-prmtx.exp-date  GE        TODAY OR oe-prmtx.exp-date EQ ? 
             OR oe-prmtx.exp-date EQ 01/01/0001)
      BY oe-prmtx.eff-date DESC:
/*         AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date  */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:         */
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype             EQ cust.type
          AND oe-prmtx.cust-no             EQ ""
          AND oe-prmtx.procat              EQ itemfg.procat
          AND SUBSTR(oe-prmtx.i-no,01,100) EQ ""
          AND oe-prmtx.eff-date  LE        TODAY
          AND (oe-prmtx.exp-date GE        TODAY OR oe-prmtx.exp-date EQ ? 
               OR oe-prmtx.exp-date EQ 01/01/0001)
      BY oe-prmtx.eff-date DESC:
/*         AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date  */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:         */
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype             EQ ""
          AND oe-prmtx.cust-no             EQ ""
          AND oe-prmtx.procat              EQ itemfg.procat
          AND oe-prmtx.i-no                BEGINS v-i-item
          AND SUBSTR(oe-prmtx.i-no,01,100) EQ v-i-item
          AND oe-prmtx.eff-date  LE        TODAY
          AND (oe-prmtx.exp-date GE        TODAY OR oe-prmtx.exp-date EQ ? 
               OR oe-prmtx.exp-date EQ 01/01/0001)
      BY oe-prmtx.eff-date DESC:
/*         AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date  */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:         */
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype             EQ ""
          AND oe-prmtx.cust-no             EQ ""
          AND oe-prmtx.procat              EQ ""
          AND oe-prmtx.i-no                BEGINS v-i-item
          AND SUBSTR(oe-prmtx.i-no,01,100) EQ v-i-item
          AND oe-prmtx.eff-date  LE        TODAY
          AND (oe-prmtx.exp-date GE        TODAY OR oe-prmtx.exp-date EQ ?
                OR oe-prmtx.exp-date EQ 01/01/0001)
      BY oe-prmtx.eff-date DESC:
/*         AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date  */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:         */
      LEAVE.
    END.
  END.

  IF NOT AVAIL oe-prmtx THEN
  ASSIGN
    i = 0
    matrixExists = NO.
  else do:
    matrixExists = YES.
    if cust.auto-reprice then do:
      v-i-qty = class-qty[index("123456789XYZ",trim(itemfg.class)) + 1].
      if itemfg.class eq "Z"     and
         class-qty[1] gt v-i-qty then v-i-qty = class-qty[1].
    end.

    do i = (IF cust.cust-level EQ 0 THEN 1 ELSE cust.cust-level) to 10:
      if v-i-qty le oe-prmtx.qty[i] then do:
        if oe-prmtx.meth then
          assign
           {1}.price = oe-prmtx.price[i]
           {1}.pr-uom = oe-prmtx.uom[i].
        else
          assign
           {1}.price = itemfg.sell-price -
                      round((itemfg.sell-price * oe-prmtx.discount[i]) / 100,2).
        i = 99.
        leave.
      end.
    end.
  end.
  
  /*if i ne 99 and not price-ent then {1}.price = itemfg.sell-price.*/

  if {1}.pr-uom begins "L" AND {1}.pr-uom NE "LB" then
    {1}.t-price = {1}.price * if {1}.{3} lt 0 then -1 else 1.

  else
  if {1}.pr-uom eq "CS" and avail itemfg and itemfg.case-count ne 0 then
    {1}.t-price = {1}.{3} / itemfg.case-count * {1}.price.

  else
  if {1}.pr-uom eq "C" then {1}.t-price = {1}.{3} / 100 * {1}.price.

  else
  if {1}.pr-uom eq "M" then {1}.t-price = {1}.{3} / 1000 * {1}.price.

  else {1}.t-price = {1}.{3} * {1}.price.

  {1}.t-price = round({1}.t-price - ({1}.t-price * {1}.disc / 100),2).
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
