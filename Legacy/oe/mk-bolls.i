
DEF VAR lv-bol-line LIKE oe-boll.bol-line NO-UNDO.
DEF BUFFER b-reftable FOR reftable.

DEF VAR g_company LIKE oe-bolh.company NO-UNDO.

ASSIGN g_company = oe-bolh.company.

FOR EACH oe-rell
    WHERE oe-rell.company EQ oe-relh.company
      AND oe-rell.r-no    EQ oe-relh.r-no
    USE-INDEX r-no NO-LOCK
    BREAK 
    BY oe-rell.ord-no
    BY oe-rell.i-no
    BY SUBSTR(oe-rell.rec_key,5,4)
    BY SUBSTR(oe-rell.rec_key,1,4)
    BY SUBSTR(oe-rell.rec_key,10,100):

    FIND FIRST oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no
          AND oe-boll.i-no    EQ oe-rell.i-no
          AND oe-boll.ord-no  EQ oe-rell.ord-no
        USE-INDEX b-no NO-ERROR.

    IF AVAIL oe-boll THEN lv-bol-line = oe-boll.bol-line.

    ELSE DO:
      FIND LAST oe-boll NO-LOCK
          WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
          USE-INDEX bol-line NO-ERROR.
      lv-bol-line = (IF AVAIL oe-boll THEN oe-boll.bol-line ELSE 0) + 1.
    END.

    create oe-boll.
    assign
     oe-boll.company  = oe-bolh.company
     oe-boll.b-no     = oe-bolh.b-no
     oe-boll.bol-no   = oe-bolh.bol-no
     oe-boll.bol-line = lv-bol-line
     oe-boll.ord-no   = oe-rell.ord-no /*oe-bolh.ord-no*/
     oe-boll.rel-no   = oe-rell.rel-no
     oe-boll.b-ord-no = oe-rell.b-ord-no
     oe-boll.po-no    = oe-rell.po-no
     oe-boll.loc-bin  = oe-rell.loc-bin
     oe-boll.loc      = oe-rell.loc
     oe-boll.r-no     = oe-rell.r-no
     oe-boll.i-no     = oe-rell.i-no
     oe-boll.line     = oe-rell.line
     oe-boll.tag      = oe-rell.tag
     oe-boll.job-no   = oe-rell.job-no
     oe-boll.job-no2  = oe-rell.job-no2
     oe-boll.cust-no  = oe-rell.cust-no
     oe-boll.cases    = oe-rell.cases
     oe-boll.qty-case = oe-rell.qty-case
     oe-boll.partial  = oe-rell.partial
     oe-boll.s-code   = oe-rell.s-code
     oe-boll.qty      = oe-rell.qty
     oe-boll.lot-no   = oe-rell.lot-no
     oe-boll.sell-price = oe-rell.sell-price
     oe-boll.zeroPrice = oe-rell.zeroPrice
     oe-boll.enteredBy = oe-rell.enteredBy
     oe-boll.enteredDT = oe-rell.enteredDT
     .

    /*task 01121106 disable trigger oe-bolh preventing this from happening*/
  IF oe-boll.rec_key EQ "" THEN
  DO:
     CREATE rec_key.
     ASSIGN
        oe-boll.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey")
        rec_key.rec_key = oe-boll.rec_key
        rec_key.table_name = "oe-boll".
     RELEASE rec_key.
  END.


    if oe-boll.loc-bin eq "" then do:
      find first sys-ctrl
          where sys-ctrl.company eq cocode
            and sys-ctrl.name    eq "BOLPRINT"
          no-lock no-error.
      if avail sys-ctrl then oe-boll.loc-bin = sys-ctrl.char-fld.
    end.

    v-bol-qty = v-bol-qty + oe-boll.qty.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-rell.i-no
        no-lock no-error.

    if oe-boll.qty-case  eq 0 and
       avail itemfg           and
       itemfg.case-count ne 0 then do:
      oe-boll.qty-case = itemfg.case-count.

      IF oe-boll.qty-case NE 0 THEN
        oe-boll.cases = TRUNC((oe-boll.qty - oe-boll.partial) / oe-boll.qty-case,0).
    END.
/*
    IF oe-boll.cases EQ 0 THEN
      IF oe-boll.qty LT oe-boll.qty-case THEN
        ASSIGN
         oe-boll.cases    = 1
         oe-boll.qty-case = oe-boll.qty.
      ELSE
        ASSIGN
         oe-boll.cases    = oe-boll.qty
         oe-boll.qty-case = 1.

    IF oe-boll.partial EQ 0 THEN
      oe-boll.partial = oe-rell.qty - (oe-boll.cases * oe-boll.qty-case).
*/
  
    IF LAST-OF(oe-rell.i-no) THEN DO: 
      {oe/oe-bolpc.i ALL}
    END.
    
    if avail itemfg then
      assign
       oe-boll.weight = ((((oe-boll.cases * oe-boll.qty-case) +
                           oe-boll.partial) / 100) * itemfg.weight-100)
       oe-bolh.tot-wt = oe-bolh.tot-wt + oe-boll.weight.

    /* gdm - 07170905 */
    {sys\inc\BOLWeight.i}
    IF BOLWt-log AND
       TRIM(oe-rell.tag) NE "" 
      THEN 
       FIND FIRST loadtag NO-LOCK 
        WHERE loadtag.company = g_company
          AND loadtag.item-type EQ NO
          AND loadtag.tag-no  = oe-rell.tag NO-ERROR.
       IF AVAIL loadtag THEN
          ASSIGN oe-boll.weight = ((oe-rell.cases * loadtag.misc-dec[1]) +
                                   loadtag.misc-dec[3]).
            
    /* gdm - 07170905 end */

    IF oe-boll.qty LT 0 THEN oe-boll.tag = "".

/*     RUN oe/getBolFrt.p (ROWID(oe-boll),         */
/*                        oe-bolh.cust-no,         */
/*                        oe-bolh.ship-id,         */
/*                        oe-bolh.carrier,         */
/*                        OUTPUT oe-boll.freight). */
end. /* each oe-rell */


RUN oe/calcBolFrt.p (INPUT ROWID(oe-bolh), OUTPUT oe-bolh.freight).
