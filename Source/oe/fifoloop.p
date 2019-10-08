
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-no-tag# AS LOG NO-UNDO.
DEF OUTPUT PARAM op-none AS LOG NO-UNDO.

{sys/inc/var.i SHARED}

DEF SHARED VAR out-recid AS RECID NO-UNDO.

DEF VAR v-csc AS LOG NO-UNDO.

FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ cocode AND
     sys-ctrl.NAME EQ "POPRINT"
     NO-LOCK NO-ERROR.

IF AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "CSC-GA" THEN
   v-csc = YES.

IF v-csc THEN
DO:
   FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.NAME EQ "QUOPRINT"
        NO-LOCK NO-ERROR.

   IF AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "CSC-Excel" THEN
      v-csc = YES.
   ELSE
      v-csc = NO.
END.

/*CSC GA code 09090801 use better index*/ 
IF v-csc THEN
DO:
    RUN oe\fifoloopcsc.p(INPUT ip-rowid,
                         INPUT ip-no-tag#,
                         OUTPUT op-none).
    RETURN.
END.

DEF VAR v-rel-qty LIKE oe-rell.qty NO-UNDO.
DEF VAR v-sum-rel-qty AS INT NO-UNDO.
DEF VAR v-fifo-loop AS INT NO-UNDO.
DEF VAR v-fifo-loop2 AS INT NO-UNDO.
DEF VAR v-i-no LIKE oe-rell.i-no NO-UNDO.
DEF VAR v-po-no LIKE oe-rell.po-no NO-UNDO.
DEF VAR v-ord-no LIKE oe-rell.ord-no NO-UNDO.
DEF VAR v-line LIKE oe-rell.line NO-UNDO.
DEF VAR v-r-no LIKE oe-rell.link-no NO-UNDO.

DEF BUFFER b-oe-ord FOR oe-ord.
DEF BUFFER b-reftable FOR reftable.

DO TRANSACTION:
  {sys/inc/relmerge.i}
  {sys/inc/addrelse.i}
END.

/** If Shipping From Bill Of Lading Then Set Ship Code = B
    Or If Shipping From Finiished Goods Then Set Ship Code = I **/
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

FIND oe-relh WHERE RECID(oe-relh) EQ out-recid NO-LOCK NO-ERROR.

FIND FIRST oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

IF NOT AVAIL oe-rel THEN
FIND FIRST inv-line WHERE ROWID(inv-line) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-rel THEN
  ASSIGN
   v-i-no    = oe-rel.i-no
   v-po-no   = oe-rel.po-no
   v-ord-no  = oe-rel.ord-no
   v-line    = oe-rel.line
   v-r-no    = oe-rel.r-no
   v-rel-qty = oe-rel.tot-qty.
ELSE
  ASSIGN
   v-i-no    = inv-line.i-no
   v-po-no   = inv-line.po-no
   v-ord-no  = 0
   v-line    = inv-line.line
   v-r-no    = 0
   v-rel-qty = inv-line.ship-qty.

ASSIGN
 op-none       = YES
 v-sum-rel-qty = 0.

/* === rel-no logic moved to line (oe-rell) ========*/
DEF BUFFER bf-rell FOR oe-rell .
DEF VAR li-nxt-rel-no AS INT NO-UNDO.
for each bf-rell
    where bf-rell.company eq cocode
      and bf-rell.ord-no  eq oe-rel.ord-no no-lock 
      by bf-rell.rel-no desc:
    
      li-nxt-rel-no =  bf-rell.rel-no.
      leave.  
end.
li-nxt-rel-no = li-nxt-rel-no + 1.
/*========== */

FIND FIRST oe-ordl
    WHERE oe-ordl.company EQ cocode
      AND oe-ordl.ord-no  EQ v-ord-no
      AND oe-ordl.i-no    EQ v-i-no
      AND oe-ordl.line    EQ v-line
    NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN
FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

FIND FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ v-i-no
    NO-LOCK NO-ERROR.

IF NOT ip-no-tag# AND AVAIL oe-relh AND (AVAIL oe-rel OR AVAIL inv-line) THEN fifo-loop:
DO v-fifo-loop = 1 TO 2.
  for each fg-bin
      where fg-bin.company    eq cocode
        and fg-bin.i-no       eq v-i-no
        AND (fg-bin.cust-no   EQ "" OR fg-bin.cust-no EQ oe-relh.cust-no)
        and (not avail oe-ordl                          or
             ((itemfg.cust-no eq oe-ord.cust-no or
               can-find(first cust
                        where cust.company eq itemfg.company
                          and cust.cust-no eq itemfg.cust-no
                          and cust.active  eq "X")) and
              (oe-ordl.job-no eq ""             or
               relmerge-int   eq 1))                     or    
             (oe-ordl.job-no  ne ""                 and
              fg-bin.job-no   eq oe-ordl.job-no     and
              fg-bin.job-no2  eq oe-ordl.job-no2))
        and ((fg-bin.qty      gt 0 and v-fifo-loop eq 2) or
             (fg-bin.qty      ge v-rel-qty and v-fifo-loop eq 1))
      use-index i-no,

      first fg-rcpth
      where fg-rcpth.company eq cocode
        and fg-rcpth.i-no    eq fg-bin.i-no
        and fg-rcpth.job-no  eq fg-bin.job-no
        and fg-rcpth.job-no2 eq fg-bin.job-no2
        and can-find(first fg-rdtlh where fg-rdtlh.r-no    eq fg-rcpth.r-no
                                      and fg-rdtlh.loc     eq fg-bin.loc
                                      and fg-rdtlh.loc-bin eq fg-bin.loc-bin
                                      and fg-rdtlh.tag     eq fg-bin.tag
                                      AND fg-rdtlh.cust-no EQ fg-bin.cust-no
                                    use-index rm-rdtl)
      use-index i-no no-lock

      by fg-rcpth.trans-date
      by fg-rcpth.r-no
      by fg-bin.job-no
      by fg-bin.job-no2
      by fg-bin.qty:

    /*RELEASE b-oe-ord.
    IF addrelse-cha          EQ "Bin/Tag"           AND
       AVAIL oe-ord                                 AND
       TRIM(oe-ordl.job-no)  NE ""                  AND
       (fg-bin.job-no        NE oe-ordl.job-no OR
        fg-bin.job-no2       NE oe-ordl.job-no2)    THEN
    FOR EACH job-hdr
        WHERE job-hdr.company EQ fg-bin.company
          AND job-hdr.job-no  EQ fg-bin.job-no
          AND job-hdr.job-no2 EQ fg-bin.job-no2
          AND job-hdr.i-no    EQ fg-bin.i-no
          AND job-hdr.ord-no  NE 0
          AND job-hdr.ord-no  NE oe-ord.ord-no
        NO-LOCK,
        FIRST b-oe-ord
        WHERE b-oe-ord.company  EQ job-hdr.company
          AND b-oe-ord.ord-no   EQ job-hdr.ord-no
          AND b-oe-ord.ord-date GT oe-ord.ord-date
        NO-LOCK:
      LEAVE.
    END.
    IF AVAIL b-oe-ord THEN NEXT.*/

          
    create oe-rell.
    ASSIGN out-recid       = RECID(oe-rell)
       oe-rell.company = cocode
       oe-rell.r-no    = oe-relh.r-no
       oe-rell.rel-no  = li-nxt-rel-no
       oe-rell.loc     = fg-bin.loc
       oe-rell.loc-bin = fg-bin.loc-bin
       oe-rell.tag     = IF ip-no-tag# THEN "" ELSE fg-bin.tag
       oe-rell.job-no  = fg-bin.job-no
       oe-rell.job-no2 = fg-bin.job-no2
       oe-rell.cust-no = fg-bin.cust-no
       oe-rell.i-no    = v-i-no
       oe-rell.po-no   = v-po-no
       oe-rell.ord-no  = v-ord-no      
       oe-rell.line    = v-line
       oe-rell.printed = no
       oe-rell.posted  = no
       oe-rell.deleted = no
       /** Set link to the planned releases **/
       oe-rell.link-no = v-r-no
       
     oe-rell.qty-case = if fg-bin.case-count gt 0 then fg-bin.case-count
                        else
                        if avail itemfg           and
                           itemfg.case-count gt 0 then itemfg.case-count
                        else
                        if avail oe-ordl          and
                           oe-ordl.cas-cnt   gt 0 then oe-ordl.cas-cnt
                        else 1.

    IF AVAILABLE oe-rel THEN 
        ASSIGN 
            oe-rell.lot-no  = oe-rel.lot-no
            oe-rell.frt-pay = oe-rel.frt-pay
            oe-rell.fob-code = oe-rel.fob-code
            oe-rell.sell-price = oe-rel.sell-price
            oe-rell.zeroPrice = oe-rel.zeroPrice
            oe-rell.s-code  = oe-rel.s-code
            .
    IF oe-rell.s-code EQ "" THEN 
        oe-rell.s-code  =  if fg-bin.cust-no gt ""                then "S"
                                                                else
                           if avail oe-ctrl and oe-ctrl.ship-from then "B" 
                                                                  else "I"
       .

    IF fg-bin.qty LE v-rel-qty THEN
      ASSIGN
       oe-rell.qty     = fg-bin.qty
       oe-rell.partial = fg-bin.partial-count.

    ELSE 
      ASSIGN
       oe-rell.qty     = v-rel-qty
       oe-rell.partial = 0.

    ASSIGN
     oe-rell.cases    = TRUNC((oe-rell.qty - oe-rell.partial) /
                              oe-rell.qty-case,0)
     oe-rell.partial  = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case).

    assign
     op-none         = NO
     v-rel-qty       = v-rel-qty - oe-rell.qty
     v-sum-rel-qty   = v-sum-rel-qty + oe-rell.qty.

    IF v-rel-qty LE 0 THEN LEAVE fifo-loop.
  end. /* end for each fg-bin */
end. /* do v-fifo-loop */ 

if v-rel-qty gt 0 AND AVAIL oe-rel then do:
  if avail oe-rell then do:
    oe-rell.qty = oe-rell.qty + v-rel-qty.
    
    assign
     oe-rell.cases    = trunc((oe-rell.qty - oe-rell.partial) /
                              oe-rell.qty-case,0)
     oe-rell.partial  = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case).
  end.
end.
  
/*else v-fifo-loop = 99.*/
