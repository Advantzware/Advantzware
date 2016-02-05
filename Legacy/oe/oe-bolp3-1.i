/* ----------------------------------------------- oe/oe-bolp3-1.i 3/7/12 WFK */
/* BILL OF LADING POSTING                                                     */
/* extracted from oe/oe-bolp3.i                                               */
/* -------------------------------------------------------------------------- */

/** update release **/
assign
 oe-relh.ship-no   = oe-bolh.ship-no
 oe-relh.ship-id   = oe-bolh.ship-id
 oe-relh.ship-i[1] = oe-bolh.ship-i[1]
 oe-relh.ship-i[2] = oe-bolh.ship-i[2]
 oe-relh.ship-i[3] = oe-bolh.ship-i[3]
 oe-relh.ship-i[4] = oe-bolh.ship-i[4].

/* if oe-rell.link-no eq 0 then do:               */
/*   find first oe-rel                            */
/*       where oe-rel.company  eq oe-rell.company */
/*         and oe-rel.ord-no   eq oe-rell.ord-no  */
/*         and oe-rel.line     eq oe-rell.line    */
/*         and oe-rel.i-no     eq oe-rell.i-no    */
/*         and oe-rel.ship-id  eq oe-relh.ship-id */
/*         and oe-rel.link-no  eq 0               */
/*       no-error.                                */
/*                                                */
/*   if not avail oe-rel then                     */
/*   find first oe-rel                            */
/*       where oe-rel.company  eq oe-rell.company */
/*         and oe-rel.ord-no   eq oe-rell.ord-no  */
/*         and oe-rel.line     eq oe-rell.line    */
/*         and oe-rel.i-no     eq oe-rell.i-no    */
/*         and oe-rel.link-no  eq 0               */
/*       no-error.                                */
/* end.                                           */
/*                                                */
/* else                                           */
/* find first oe-rel                              */
/*     where oe-rel.r-no eq oe-rell.link-no       */
/*     use-index seq-no no-error.                 */
/*                                                */
/* if avail oe-rel THEN DO:                       */
FOR EACH oe-rel
      where oe-rel.company  eq oe-rell.company
        and oe-rel.ord-no   eq oe-rell.ord-no
        and oe-rel.line     eq oe-rell.line
        and oe-rel.i-no     eq oe-rell.i-no
        EXCLUSIVE-LOCK:
  IF oe-rel.link-no = 0 OR oe-rel.r-no = oe-rell.link-no THEN DO:
      assign
       oe-rel.ship-no   = oe-relh.ship-no
       oe-rel.ship-id   = oe-relh.ship-id
       oe-rel.ship-i[1] = oe-relh.ship-i[1]
       oe-rel.ship-i[2] = oe-relh.ship-i[2]
       oe-rel.ship-i[3] = oe-relh.ship-i[3]
       oe-rel.ship-i[4] = oe-relh.ship-i[4]
       oe-rel.po-no     = report.key-07.
    
    /* update back all release with same frt pay as oe-bolh.frt-pay/fob code AH 03/26/10 */
    /*
      FIND FIRST b-reftable 
          WHERE b-reftable.reftable EQ "oe-rel.lot-no"
            AND b-reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
          EXCLUSIVE-LOCK NO-ERROR.
    
      IF AVAIL b-reftable THEN DO:
        ASSIGN b-reftable.code2    = oe-bolh.frt-pay 
               b-reftable.dscr     = (IF v-fob-code <> "" THEN v-fob-code ELSE b-reftable.dscr).
      END.
      RELEASE b-reftable.
    */
       
    /** Use ship-no to find customer shipto because ship-no is the
        primary index. **/
    find first shipto
        where shipto.company eq oe-relh.company
          and shipto.cust-no eq oe-relh.cust-no
          and shipto.ship-no eq oe-relh.ship-no
          no-lock no-error.
    if avail shipto and avail oe-rel then
      assign
       oe-rel.ship-addr[1] = shipto.ship-addr[1]
       oe-rel.ship-addr[2] = shipto.ship-addr[2]
       oe-rel.ship-city    = shipto.ship-city
       oe-rel.ship-state   = shipto.ship-state
       oe-rel.ship-zip     = shipto.ship-zip.
  END.
END. /* each oe-rel */
