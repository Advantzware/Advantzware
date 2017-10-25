/* -------------------------------------------------- fg/updsetdm.p 12/00 JLF */
/* Update Set Dimensions                                                      */
/* 09/10/01 YSK Copied and changed to use est-no instead e-num                */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid.

{sys/inc/var.i shared}

def buffer b-itemfg for itemfg.
def buffer b-eb     for eb.

def var v-part-qty      as   dec.
def var v-yld-qty       as   dec.

{ce/msfcalc.i}

{fg/fullset.i NEW}


find eb where recid(eb) eq v-recid no-lock no-error.

if avail eb then
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq eb.stock-no
      no-error.
else
  find itemfg where recid(itemfg) eq v-recid no-error.

if avail itemfg and itemfg.isaset then do:
  if avail eb then
  for each b-eb
      where b-eb.company EQ eb.company
        and  b-eb.est-no  eq eb.est-no
        and b-eb.form-no ne 0
      no-lock
      
      break by b-eb.est-no:
      
    if first(b-eb.est-no) then
      assign
       itemfg.l-score    = eb.len
       itemfg.w-score    = eb.wid
       itemfg.d-score    = eb.dep
       itemfg.t-wid      = 0
       itemfg.t-len      = 0
       itemfg.t-sqin     = 0
       itemfg.t-sqft     = 0.

    IF itemfg.spare-int-1 NE 1 THEN
       itemfg.weight-100 = 0.
       
    ASSIGN
     v-yld-qty     = IF b-eb.cust-% NE 0 THEN b-eb.cust-% ELSE b-eb.yld-qty
     v-yld-qty     = IF v-yld-qty LT 0 THEN -1 / v-yld-qty ELSE v-yld-qty
     itemfg.t-wid  = itemfg.t-wid  + (b-eb.t-wid  * v-yld-qty)
     itemfg.t-len  = itemfg.t-len  + (b-eb.t-len  * v-yld-qty)
     itemfg.t-sqin = itemfg.t-sqin + (b-eb.t-sqin * v-yld-qty)
     itemfg.t-sqft = itemfg.t-sqft +
                     ((IF v-corr THEN (b-eb.t-sqin * .007) ELSE
                                      (b-eb.t-sqin / 144)) * v-yld-qty).
                                      
    for first ef
        where ef.est-no  eq b-eb.est-no
          and ef.form-no eq b-eb.form-no
        no-lock,
        
        first item
        where item.company eq cocode
          and item.i-no    eq ef.board
        no-lock:

      IF NOT itemfg.spare-int-1 EQ 1 THEN   /* freeze-weight flag */
      itemfg.weight-100 = itemfg.weight-100 +
                          (((if v-corr then
                              (b-eb.t-sqin * .007) else
                              (b-eb.t-sqin / 144)) * v-yld-qty) * 100 /
                          1000 * item.basis-w).
    end.
  end.

  ELSE DO:
    RUN fg/fullset.p (ROWID(itemfg)).

    FOR EACH tt-fg-set,
        FIRST b-itemfg
        WHERE b-itemfg.company EQ cocode
          AND b-itemfg.i-no    EQ tt-fg-set.part-no
        NO-LOCK
    
        break by tt-fg-set.set-no:
      
      if first(tt-fg-set.set-no) then
        assign
         itemfg.t-wid      = 0
         itemfg.t-len      = 0
         itemfg.t-sqin     = 0
         itemfg.t-sqft     = 0
         itemfg.weight-100 = 0.
    
      assign
       itemfg.t-wid      = itemfg.t-wid      +
                           (b-itemfg.t-wid      * tt-fg-set.part-qty-dec)
       itemfg.t-len      = itemfg.t-len      +
                           (b-itemfg.t-len      * tt-fg-set.part-qty-dec)
       itemfg.t-sqin     = itemfg.t-sqin     +
                           (b-itemfg.t-sqin     * tt-fg-set.part-qty-dec)
       itemfg.t-sqft     = itemfg.t-sqft     +
                           (b-itemfg.t-sqft     * tt-fg-set.part-qty-dec).
       itemfg.weight-100 = itemfg.weight-100 +
                           (b-itemfg.weight-100 * tt-fg-set.part-qty-dec).
    end.
  END.
end.
/* end ---------------------------------- copr. 2000  advanced software, inc. */
