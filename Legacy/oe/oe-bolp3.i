/* -------------------------------------------------- oe/oe-bolp3.i 06/98 JLF */
/* BILL OF LADING POSTING                                                     */
/* -------------------------------------------------------------------------- */

RELEASE inv-head.

IF rCurrentInvHeadRow NE ? THEN 
    FIND FIRST inv-head WHERE ROWID(inv-head) EQ rCurrentInvHeadRow NO-ERROR.
fLogMsg("Top of oe-bolp3.i: " + "  BOL#: " + STRING(oe-bolh.bol-no) + " ino: " + oe-boll.i-no + " Avail Invhead? " + STRING(avail(inv-head))).
IF FIRST-OF({1}.{2})                   OR
    NOT AVAIL inv-head                  OR
    inv-head.cust-no NE oe-bolh.cust-no THEN 
DO:
    fLogMsg("Start create inv-head block in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " first:" + string(FIRST-OF({1}.{2}))).
    /* Original Code */
    
    v-ref-no = next-value(inv_r_no_seq). 
    fLogMsg("Obtain v-ref-no in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " v-ref-no: " + STRING(v-ref-no)).
  
  RUN oe/custxship.p (oe-bolh.company,
      oe-bolh.cust-no,
      oe-bolh.ship-id,
      BUFFER shipto).
        
  IF NOT AVAIL shipto THEN
  FIND FIRST shipto NO-LOCK
      WHERE shipto.company EQ oe-bolh.company
        AND shipto.cust-no EQ oe-bolh.cust-no
      USE-INDEX ship-no NO-ERROR.

  FIND FIRST reftable WHERE
       reftable.reftable EQ "oe-bolh.lot-no" AND
       reftable.rec_key  EQ oe-bolh.rec_key
       USE-INDEX rec_key
       NO-LOCK NO-ERROR.

  IF AVAIL reftable THEN
      ASSIGN v-fob-code = IF reftable.CODE EQ "O" THEN "ORIG"
                          ELSE IF reftable.CODE EQ "D" THEN "DEST"
                          ELSE reftable.CODE.
  ELSE
      ASSIGN v-fob-code = "".
  RELEASE reftable.
  fLogMsg("Create inv-head in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " v-ref-no: " + STRING(v-ref-no)).
  CREATE inv-head.
  ASSIGN
   inv-head.sold-no      = shipto.ship-id
   inv-head.sold-name    = shipto.ship-name
   inv-head.sold-addr[1] = shipto.ship-addr[1]
   inv-head.sold-addr[2] = shipto.ship-addr[2]
   inv-head.sold-state   = shipto.ship-state
   inv-head.sold-city    = shipto.ship-city
   inv-head.sold-zip     = shipto.ship-zip
   inv-head.r-no         = v-ref-no
   inv-head.company      = oe-bolh.company
   inv-head.bol-no       = oe-bolh.bol-no
   inv-head.bill-to      = oe-bolh.cust-no
   inv-head.cust-no      = oe-bolh.cust-no
   inv-head.frt-pay      = oe-bolh.frt-pay
   inv-head.carrier      = oe-bolh.carrier
   inv-head.ship-i[1]    = oe-bolh.ship-i[1]
   inv-head.ship-i[2]    = oe-bolh.ship-i[2]
   inv-head.ship-i[3]    = oe-bolh.ship-i[3]
   inv-head.ship-i[4]    = oe-bolh.ship-i[4]
   inv-head.fob-code     = (IF v-fob-code <> "" THEN v-fob-code ELSE oe-ord.fob-code)
   inv-head.contact      = oe-ord.contact
   inv-head.terms        = oe-ord.terms
   inv-head.terms-d      = oe-ord.terms-d
   inv-head.sman[1]      = oe-ord.sman[1]
   inv-head.sman[2]      = oe-ord.sman[2]
   inv-head.sman[3]      = oe-ord.sman[3]
   inv-head.s-pct[1]     = oe-ord.s-pct[1]
   inv-head.s-pct[2]     = oe-ord.s-pct[2]
   inv-head.s-pct[3]     = oe-ord.s-pct[3]
   inv-head.s-comm[1]    = oe-ord.s-comm[1]
   inv-head.s-comm[2]    = oe-ord.s-comm[2]
   inv-head.s-comm[3]    = oe-ord.s-comm[3]
   inv-head.f-bill       = NO
   inv-head.tax-gr       = IF AVAIL shipto AND shipto.tax-code NE ""
                           THEN shipto.tax-code ELSE oe-ord.tax-gr
   inv-head.tot-ord      = 0
   inv-head.inv-no       = 0
   inv-head.stat         = ""
   inv-head.deleted      = NO
   inv-head.posted       = NO
   inv-head.inv-date     = IF invdate-chr EQ "Current" THEN TODAY
                           ELSE oe-bolh.bol-date
   inv-head.cust-name    = cust.name
   inv-head.addr[1]      = cust.addr[1]
   inv-head.addr[2]      = cust.addr[2]
   inv-head.city         = cust.city
   inv-head.state        = cust.state
   inv-head.zip          = cust.zip
   inv-head.curr-code[1] = cust.curr-code
   rCurrentInvHeadRow = ROWID(inv-head)
   .
   
  fLogMsg("Done Create inv-head in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " v-ref-no: " + STRING(v-ref-no)).
  IF invStatus-log THEN
      inv-head.stat = "W".
  
  FIND FIRST usergrps WHERE
       usergrps.usergrps = "IN"
       NO-LOCK NO-ERROR.

  IF AVAIL usergrps AND TRIM(usergrps.users) NE "" THEN
  DO:
     ASSIGN
      v-line-count = 0
      v-start-pos  = 1.

     DO li = 1 TO LENGTH(usergrps.users):
        ls = SUBSTR(usergrps.users,li,1).
       
        IF v-line-count < 5 AND ls EQ CHR(10) OR ls EQ CHR(13) THEN
           ASSIGN
              v-line-count = v-line-count + 1
              inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos)
              v-start-pos = li + 1.
     
        IF v-line-count < 5 AND li = LENGTH(usergrps.users) AND
           NOT(ls EQ CHR(10) OR ls EQ CHR(13)) THEN
           ASSIGN
              v-line-count = v-line-count + 1
              inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos + 1).
     END.
     
     RELEASE usergrps.
  END.

  DO li = 1 TO 4:
     IF inv-head.bill-i[li] = "" THEN
        inv-head.bill-i[li] = oe-ord.bill-i[li].
  END.

  CREATE w-inv.
  w-rowid = ROWID(inv-head).
END. /* first {1}.{2} */

IF oe-bolh.freight NE 0 AND inv-head.frt-pay EQ "B" THEN inv-head.f-bill = YES.

for each oe-ordm
    where oe-ordm.company eq oe-boll.company
      and oe-ordm.ord-no  eq oe-boll.ord-no
      and oe-ordm.bill    eq "Y":
  fLogMsg("Create inv-misc in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " v-ref-no: " + STRING(v-ref-no)).          
  create inv-misc.
  BUFFER-COPY oe-ordm EXCEPT rec_key TO inv-misc
  assign
   inv-misc.r-no           = inv-head.r-no
   inv-misc.posted         = no
   inv-misc.deleted        = no
   inv-misc.inv-i-no       = oe-ordm.ord-i-no
   inv-misc.inv-line       = oe-ordm.ord-line
   inv-misc.s-commbasis[1] = oe-ordm.commbasis[1]
   oe-ordm.bill = "I".   /** Set billing flag to (I)nvoiced **/
end.

find first job-hdr
    where job-hdr.company eq oe-boll.company
      and job-hdr.loc     eq oe-boll.loc
      and job-hdr.i-no    eq oe-boll.i-no
      and job-hdr.ord-no  eq oe-boll.ord-no
      and job-hdr.job-no  eq oe-ordl.job-no
      and job-hdr.job-no2 eq oe-ordl.job-no2
    no-lock no-error.

/** update release **/
fLogMsg("Update Release in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " v-ref-no: " + STRING(v-ref-no)).
assign
 oe-relh.ship-no   = oe-bolh.ship-no
 oe-relh.ship-id   = oe-bolh.ship-id
 oe-relh.ship-i[1] = oe-bolh.ship-i[1]
 oe-relh.ship-i[2] = oe-bolh.ship-i[2]
 oe-relh.ship-i[3] = oe-bolh.ship-i[3]
 oe-relh.ship-i[4] = oe-bolh.ship-i[4].

if oe-rell.link-no eq 0 then do:
  find first oe-rel
      where oe-rel.company  eq oe-rell.company
        and oe-rel.ord-no   eq oe-rell.ord-no
        and oe-rel.line     eq oe-rell.line
        and oe-rel.i-no     eq oe-rell.i-no
        and oe-rel.ship-id  eq oe-relh.ship-id
        and oe-rel.link-no  eq 0
      no-error.

  if not avail oe-rel then
  find first oe-rel
      where oe-rel.company  eq oe-rell.company
        and oe-rel.ord-no   eq oe-rell.ord-no
        and oe-rel.line     eq oe-rell.line
        and oe-rel.i-no     eq oe-rell.i-no
        and oe-rel.link-no  eq 0
      no-error.
end.

else
find first oe-rel
    where oe-rel.r-no eq oe-rell.link-no
    use-index seq-no no-error.

if avail oe-rel THEN DO:

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
END. /* avail oe-rel */
   
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
fLogMsg("Set oe-bolh,oe-boll posted in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " v-ref-no: " + STRING(v-ref-no)).
assign
 oe-bolh.posted = yes
 oe-boll.posted = yes.

IF invlotline-log EQ NO THEN
   find first inv-line
       where inv-line.r-no   eq inv-head.r-no
         and inv-line.ord-no eq oe-boll.ord-no
         and inv-line.b-no   eq oe-bolh.b-no
         and inv-line.i-no   eq oe-boll.i-no
         and inv-line.line   eq oe-boll.line
         and inv-line.po-no  eq oe-boll.po-no
       use-index r-no no-error.
ELSE
DO:
/*    FIND FIRST b-reftable3 WHERE                        */
/*         b-reftable3.reftable EQ "oe-boll.lot-no" AND   */
/*         b-reftable3.rec_key  EQ STRING(RECID(oe-boll)) */
/*         USE-INDEX rec_key                              */
/*         NO-LOCK NO-ERROR.                              */
/*                                                        */
/*    IF AVAIL b-reftable3 THEN                           */
   find first inv-line
       where inv-line.r-no   eq inv-head.r-no
         and inv-line.ord-no eq oe-boll.ord-no
         and inv-line.b-no   eq oe-bolh.b-no
         and inv-line.i-no   eq oe-boll.i-no
         and inv-line.line   eq oe-boll.line
         and inv-line.po-no  eq oe-boll.po-no
         AND inv-line.lot-no EQ oe-boll.lot-no
/*          AND CAN-FIND(FIRST reftable WHERE              */
/*              reftable.reftable = "inv-line.lot-no" AND  */
/*              reftable.rec_key  = inv-line.rec_key AND   */
/*              reftable.CODE     = oe-boll.lot-no         */
/* /*              reftable.CODE     = b-reftable3.CODE */ */
/*              USE-INDEX rec_key)                         */
       use-index r-no no-error.
/*    ELSE                                        */
/*        find first inv-line                     */
/*        where inv-line.r-no   eq inv-head.r-no  */
/*          and inv-line.ord-no eq oe-boll.ord-no */
/*          and inv-line.b-no   eq oe-bolh.b-no   */
/*          and inv-line.i-no   eq oe-boll.i-no   */
/*          and inv-line.line   eq oe-boll.line   */
/*          and inv-line.po-no  eq oe-boll.po-no  */
/*        use-index r-no no-error.                */
END.
fLogMsg("Check inv-line avail in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " avail inv-line: " + STRING(AVAILABLE(inv-line))).
IF NOT AVAIL inv-line THEN DO:
fLogMsg("Begin create inv-line in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " v-ref-no: " + STRING(v-ref-no)).    
  CREATE inv-line.

  ASSIGN
   inv-line.r-no       = inv-head.r-no
   inv-line.company    = oe-bolh.company
   inv-line.ord-no     = oe-boll.ord-no
   inv-line.b-no       = oe-bolh.b-no
   inv-line.line       = oe-boll.line
   inv-line.i-no       = oe-boll.i-no
   inv-line.stat       = oe-boll.s-code
   inv-line.est-no     = oe-ordl.est-no
   inv-line.est-type   = oe-ord.est-type
   inv-line.ord-date   = oe-ord.ord-date
   inv-line.part-no    = oe-ordl.part-no
   inv-line.i-name     = oe-ordl.i-name
   inv-line.i-dscr     = oe-ordl.i-dscr
   inv-line.pr-uom     = oe-ordl.pr-uom
   inv-line.price      = (IF AVAIL oe-rel AND oe-rel.price GT 0 THEN oe-rel.price ELSE oe-ordl.price)
   inv-line.cas-cnt    = IF oe-ordl.pr-uom EQ "CS" THEN oe-ordl.cas-cnt
                                                   ELSE oe-boll.qty-case
   inv-line.req-code   = oe-ordl.req-code
   inv-line.req-date   = oe-ordl.req-date
   inv-line.prom-code  = oe-ordl.prom-code
   inv-line.prom-date  = oe-ordl.prom-date
   inv-line.part-dscr1 = oe-ordl.part-dscr1
   inv-line.part-dscr2 = oe-ordl.part-dscr2
   inv-line.po-no-po   = oe-ordl.po-no-po
   inv-line.e-num      = oe-ordl.e-num
   inv-line.form-no    = oe-ordl.form-no
   inv-line.blank-no   = oe-ordl.blank-no
   inv-line.j-no       = oe-ordl.j-no
   inv-line.job-no     = oe-ordl.job-no
   inv-line.job-no2    = oe-ordl.job-no2
   inv-line.tax        = oe-ordl.tax
   inv-line.disc       = oe-ordl.disc
   inv-line.qty        = oe-ordl.qty
   inv-line.p-c        = oe-boll.p-c
   inv-line.po-no      = oe-boll.po-no
   inv-line.lot-no     = oe-boll.lot-no.
   fLogMsg("Done create inv-line in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " avail inv-line: " + STRING(AVAILABLE(inv-line)) 
              + " inv-line.r-no " + STRING(inv-line.r-no) + " inv-line.b-no " + STRING(inv-line.b-no) + " inv-line.line " + STRING(inv-line.line)).
   
      IF oe-boll.zeroPrice EQ 1 THEN
         inv-line.price = 0.
      ELSE IF oe-boll.sell-price NE 0 THEN
         inv-line.price = oe-boll.sell-price.
      


   fLogMsg("Done create inv-line avail in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " avail inv-line: " + STRING(AVAILABLE(inv-line))).        
END. /* Create inv-line */
fLogMsg("Begin inv-line calculated values in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " avail inv-line: " + STRING(AVAILABLE(inv-line))).
ASSIGN
 inv-line.t-weight      = inv-line.t-weight + oe-boll.weight
 inv-head.t-inv-weight  = inv-head.t-inv-weight + oe-boll.weight
 inv-line.t-freight     = inv-line.t-freight + oe-boll.freight
 inv-head.t-inv-freight = inv-head.t-inv-freight + oe-boll.freight.
/*v-bol-qty = oe-boll.qty.
run oe/oe-bolp4.p (recid(oe-boll), recid(oe-ordl)).*/



/* Moved to before extended price calc for inv-qty */
/** Increase invoice Qty when invoice or invoice & ship **/
IF oe-boll.s-code ne "S" and not oe-ordl.is-a-component then
  inv-line.inv-qty = inv-line.inv-qty + oe-boll.qty.
  
/** Increase ship Qty when ship or invoice & ship **/
if oe-boll.s-code ne "I"                                            or
   can-find(first b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) then
  inv-line.ship-qty = inv-line.ship-qty + oe-boll.qty.

IF AVAIL oe-rel THEN
  RUN getOeRelSCode (INPUT ROWID(oe-rel), OUTPUT cRelScode).

/** 12301405 - If ship only, price must be zero **/

oe-ordl.stat = "".

RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT oe-ordl.inv-qty, OUTPUT oe-ordl.ship-qty).
IF cRelScode EQ "S"  then
  ASSIGN inv-line.qty = 0 inv-line.price = 0 inv-line.inv-qty = 0.

inv-line.t-price = inv-line.inv-qty / 1000 * inv-line.price.

IF inv-line.pr-uom BEGINS "L" AND inv-line.pr-uom NE "LB" THEN
         inv-line.t-price = inv-line.price *
                           IF inv-line.inv-qty LT 0 THEN -1 ELSE IF inv-line.inv-qty EQ 0 THEN 0 ELSE 1.
ELSE IF inv-line.pr-uom EQ "CS" THEN
    inv-line.t-price = inv-line.inv-qty /
                           (IF inv-line.cas-cnt NE 0 THEN
                             inv-line.cas-cnt
                            ELSE
                            IF itemfg.case-count NE 0 THEN
                              itemfg.case-count ELSE 1) *
                           inv-line.price.
ELSE IF LOOKUP(inv-line.pr-uom,fg-uom-list) GT 0 THEN
       inv-line.t-price = inv-line.inv-qty * inv-line.price.
ELSE
  FOR EACH uom
      WHERE uom.uom  EQ inv-line.pr-uom
        AND uom.mult NE 0
      NO-LOCK:
    inv-line.t-price = inv-line.inv-qty / uom.mult * inv-line.price.
    LEAVE.
  END.
inv-line.t-price = ROUND(inv-line.t-price,2).

IF inv-line.disc NE 0 THEN
   inv-line.t-price = 
        IF ll-calc-disc-first THEN 
          (inv-line.t-price - ROUND(inv-line.t-price * inv-line.disc / 100,2))
        ELSE
          ROUND(inv-line.t-price * (1 - (inv-line.disc / 100)),2).

RUN oe/invlcost.p (ROWID(inv-line),
                     OUTPUT v-cost[1], OUTPUT v-cost[2],
                     OUTPUT v-cost[3], OUTPUT v-cost[4],
                     OUTPUT inv-line.cost, OUTPUT inv-line.t-cost).

do i = 1 to 3:          /** Calculate Commission Amount **/
  assign
   inv-line.sname[i]   = oe-ord.sname[i]
   inv-line.s-comm[i]  = oe-ordl.s-comm[i]
   inv-line.s-pct[i]   = oe-ordl.s-pct[i]
   inv-line.sman[i]    = oe-ordl.s-man[i].
end.

DO i = 1 TO EXTENT(inv-line.sman):    /** Calculate Commission Amount **/
    RUN custom/combasis.p (oe-boll.company, inv-line.sman[i], cust.type, itemfg.procat, 0,
                           cust.cust-no,
                           OUTPUT v-basis).

    IF v-basis EQ "G" THEN
      inv-line.comm-amt[i] = ROUND(((inv-line.t-price - inv-line.t-cost)
                                       * inv-line.s-comm[i]) / 100,2).

    ELSE
      inv-line.comm-amt[i] = ROUND((((inv-line.t-price
                                       * inv-line.s-pct[i]) / 100)
                                       * inv-line.s-comm[i]) / 100,2).        
      
END.

IF AVAIL oe-rel THEN
   RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
fLogMsg("END inv-line calculated values in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " avail inv-line: " + STRING(AVAILABLE(inv-line))).
fLogMsg("Test v-u-in in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " v-u-inv " + STRING(v-u-inv)).
if v-u-inv then do:
  {oe/oe-bolp.i "oe-ordl"}
end.
fLogMsg("After oe-bolp.i in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " avail inv-line: " + STRING(AVAILABLE(inv-line))).
/** update order status **/
assign
 oe-ord.stat          = "P"
 oe-ord.inv-no        = 0
 oe-ord.inv-date      = ?
 oe-ord.t-inv-weight  = 0
 oe-ord.t-inv-tax     = 0
 oe-ord.t-inv-freight = 0
 oe-ord.t-inv-rev     = 0
 oe-ord.t-inv-cost    = 0.

if not v-u-inv then oe-bolh.w-ord = yes.

IF LAST-OF({1}.{2}) AND AVAIL inv-head THEN DO:
  fLogMsg("Begin last-of key-03 in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " avail inv-line: " + STRING(AVAILABLE(inv-line))).    
  FIND xinv-head WHERE ROWID(xinv-head) EQ ROWID(inv-head) NO-LOCK NO-ERROR.

  IF AVAIL xinv-head THEN
  FOR EACH b-invl WHERE b-invl.r-no EQ inv-head.r-no NO-LOCK,
      FIRST xoe-ord
      WHERE xoe-ord.company EQ inv-line.company
        AND xoe-ord.ord-no  EQ inv-line.ord-no
      NO-LOCK
      BREAK BY b-invl.b-no:
      
    /* Lookup Price Matrix for Billable Shipto */
    IF inv-head.cust-no EQ inv-head.sold-no                          AND
       inv-head.cust-no NE xoe-ord.cust-no                           AND
       CAN-FIND(FIRST oe-prmtx
                {oe/oe-prmtxW.i}
                  AND oe-prmtx.cust-no            EQ inv-head.cust-no
                  AND oe-prmtx.i-no               BEGINS b-invl.i-no
                  AND SUBSTR(oe-prmtx.i-no,1,100) EQ b-invl.i-no)    THEN DO:
      ASSIGN
       fil_id    = RECID(b-invl)
       save_id   = fil_id
       price-ent = YES
       v-i-item  = b-invl.i-no
       v-i-qty   = b-invl.inv-qty.

      RUN oe/oe-ipric.p.
    END.

    IF v-u-inv THEN RUN oe/invpost4.p (RECID(b-invl), 1).
    
    IF oe-bolh.trailer EQ 'UPS' THEN
    RUN createUPS (inv-head.company,inv-head.sold-no,inv-line.ord-no,
                   inv-head.bol-no,xoe-ord.terms,ROWID(inv-head)).

  END. /* each b-invl */
    fLogMsg("End last-of key-03 in oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " avail inv-line: " + STRING(AVAILABLE(inv-line))).
END. /* if last-of */
fLogMsg("Done oe-bolp3.i oe-bolp3.i: " + " BOL#: " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no + " oe-boll.b-no " + STRING(oe-boll.b-no)).
/* end --------------------------------- copyright 1998 Advanced Software Inc.*/

