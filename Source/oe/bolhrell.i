/* oe/bolhrell.i  copied from oe/bollrell_sav.i and changed vars name */

DEF BUFFER bx-relh FOR oe-relh.
DEF BUFFER bx-rell FOR oe-rell.
DEF BUFFER bx-rel  FOR oe-rel.

DEF VAR lvv-ord-no LIKE oe-rell.ord-no NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF oe-ordl.

find first oe-ctrl where oe-ctrl.company eq oe-bolh.company no-lock.

find sys-ctrl
    where sys-ctrl.company eq oe-bolh.company
      and sys-ctrl.name    eq "ADDRELSE"
    no-lock no-error.
v-do-bol = avail sys-ctrl and sys-ctrl.log-fld and oe-ctrl.ship-from.

for each oe-relh
    where oe-relh.company  eq oe-bolh.company
      and oe-relh.release# eq oe-bolh.release#
    no-lock:
   
  for each oe-rell
      where oe-rell.company eq oe-relh.company
        and oe-rell.r-no    eq oe-relh.r-no
      use-index r-no no-lock:

    find first oe-boll
        where oe-boll.company eq oe-bolh.company
          and oe-boll.b-no    eq oe-bolh.b-no
          and oe-boll.i-no    eq oe-rell.i-no
          and oe-boll.line    eq oe-rell.line
        use-index b-no no-lock no-error.
   
    if not avail oe-boll then do:
       find first oe-ordl
          where oe-ordl.company eq oe-rell.company
            and oe-ordl.ord-no  eq oe-rell.ord-no
            and oe-ordl.i-no    eq oe-rell.i-no
            and oe-ordl.line    eq oe-rell.line
          use-index ord-no no-error.

       if avail oe-ordl THEN do:
         IF oe-rell.s-code ne "I" then 
           oe-ordl.t-rel-qty = oe-ordl.t-rel-qty - oe-rell.qty.
         if oe-ordl.t-rel-qty lt 0 then oe-ordl.t-rel-qty = 0.
       END.

       lvv-ord-no = oe-rell.ord-no.

       FIND bx-rell WHERE ROWID(bx-rell) EQ ROWID(oe-rell) NO-ERROR.
       IF AVAIL bx-rell THEN DO:
         IF v-do-bol THEN DELETE bx-rell.      
         ELSE bx-rell.posted = NO.
       END.
       RELEASE bx-rell.
      
       for each oe-rel
           where oe-rel.company eq oe-rell.company
             and oe-rel.ord-no  eq lvv-ord-no
             and oe-rel.link-no ne 0
           use-index ord-item no-lock:

         FOR EACH bx-rell
             WHERE bx-rell.company  EQ oe-rel.company
               AND bx-rell.r-no     EQ oe-rel.link-no
               AND bx-rell.ord-no   EQ oe-rel.ord-no
               AND bx-rell.i-no     EQ oe-rel.i-no
               AND bx-rell.line     EQ oe-rel.line
               AND bx-rell.rel-no   EQ oe-rel.rel-no
               AND bx-rell.b-ord-no EQ oe-rel.b-ord-no
               AND bx-rell.po-no    EQ oe-rel.po-no
               AND bx-rell.posted   EQ YES
             USE-INDEX r-no
             BREAK BY bx-rell.r-no:
           bx-rell.link-no = oe-rel.r-no.
           IF LAST(bx-rell.r-no) THEN LEAVE.
         END.
         IF NOT AVAIL bx-rell THEN DO:
           FIND bx-rel WHERE ROWID(bx-rel) EQ ROWID(oe-rel) NO-ERROR.
           IF AVAIL bx-rel THEN bx-rel.link-no = 0.
         END.
       END.
    END. /* avail oe-boll */
    
    RUN oe/rel-stat-upd.p (ROWID(oe-rell)).  
  END. /* for each oe-rell */

  FIND FIRST oe-rell
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
      USE-INDEX r-no NO-LOCK NO-ERROR.
  IF NOT AVAIL oe-rell THEN DO:
    FIND bx-relh WHERE ROWID(bx-relh) EQ ROWID(oe-relh) NO-ERROR.
    IF AVAIL bx-relh THEN DO:
      bx-relh.posted = NO.
      DELETE bx-relh.
    END.
  END.
end.  /* FOR EACH OE-RELH */
