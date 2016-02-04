/* --------------------------------------------- sys/inc/fg-reset.i 08/99 JLF */
/* itemfg reset                                                               */
/* -------------------------------------------------------------------------- */

def input parameter rec-id as recid.

{sys/inc/var.i shared}

def buffer b-itemfg for itemfg.
DEF BUFFER b-itemfg-loc FOR itemfg-loc.

def var v-hld-qty as dec.
def var v-part-qty as dec.
def var v-fin-qty as dec.
def var v-fstat like oe-ord.stat init "".
def var v-tstat like v-fstat.


{sys/inc/oereordr.i}

find itemfg where recid(itemfg) eq rec-id.
    
find first oe-ctrl where oe-ctrl.company eq itemfg.company no-lock.

assign
 {1}q-onh   = 0
 {1}q-ono   = 0
 {1}q-alloc = 0
 {1}q-back  = 0.

IF itemfg.est-no GT "" THEN
    FIND FIRST eb WHERE eb.company EQ itemfg.company
                    AND eb.est-no  EQ itemfg.est-no
                    AND eb.stock-no EQ itemfg.i-no
                  NO-LOCK NO-ERROR.

FOR EACH itemfg-loc 
  WHERE itemfg-loc.company EQ itemfg.company
    AND itemfg-loc.i-no    EQ itemfg.i-no
  EXCLUSIVE-LOCK:
    assign
     itemfg-loc.q-onh   = 0
     itemfg-loc.q-ono   = 0
     itemfg-loc.q-alloc = 0
     itemfg-loc.q-back  = 0.                   

    /* Set component FGs with the same loc as the current itemfg-loc */
    for each fg-set
        where fg-set.company eq itemfg.company
          and fg-set.part-no eq itemfg.i-no
        no-lock,
        first b-itemfg
        where b-itemfg.company eq itemfg.company
          and b-itemfg.i-no    eq fg-set.set-no
          and b-itemfg.isaset  eq YES
        NO-LOCK,
        first b-itemfg-loc
        where b-itemfg-loc.company eq itemfg.company
          and b-itemfg-loc.i-no    eq fg-set.set-no
          AND b-itemfg-loc.loc     EQ itemfg-loc.loc          
        no-lock:

      /* 06111209 */
      {sys/inc/part-qty.i v-part-qty fg-set}
      v-part-qty = 
        (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE v-part-qty).

      assign
       itemfg-loc.q-ono   = itemfg-loc.q-ono   + (b-itemfg-loc.q-ono   * v-part-qty)
       itemfg-loc.q-alloc = itemfg-loc.q-alloc + (b-itemfg-loc.q-alloc * v-part-qty).

    end.

END.

for each fg-set
    where fg-set.company eq itemfg.company
      and fg-set.part-no eq itemfg.i-no
    no-lock,

    first b-itemfg
    where b-itemfg.company eq itemfg.company
      and b-itemfg.i-no    eq fg-set.set-no
      and b-itemfg.isaset  eq YES
    no-lock:
  
  /* 06111209 */
  {sys/inc/part-qty.i v-part-qty fg-set}
  v-part-qty = 
    (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE v-part-qty).

  assign
   {1}q-ono   = {1}q-ono   + (b-itemfg.q-ono   * v-part-qty)
   {1}q-alloc = {1}q-alloc + (b-itemfg.q-alloc * v-part-qty).

end.


/*** itemfg.q-onh ***/
for each fg-bin
    where fg-bin.company eq itemfg.company
      and fg-bin.i-no    eq itemfg.i-no
    no-lock:

  {1}q-onh = {1}q-onh + fg-bin.qty.
end.

/*** itemfg.q-ono from jobs and purchase orders***/
RUN fg/calcqono.p (ROWID(itemfg), OUTPUT {1}q-ono).

/*** itemfg.q-alloc & itemfg.q-back from customer orders ***/
RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT {1}q-alloc, OUTPUT {1}q-back).

{1}q-avail = {1}q-onh +
             (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE {1}q-ono) -
             {1}q-alloc.

FOR EACH itemfg-loc WHERE itemfg-loc.company = itemfg.company
     AND itemfg-loc.i-no EQ itemfg.i-no
    EXCLUSIVE-LOCK:
    /*** itemfg.q-onh ***/
    for each fg-bin
        where fg-bin.company eq itemfg-loc.company
          and fg-bin.i-no    eq itemfg-loc.i-no
          AND fg-bin.loc     EQ itemfg-loc.loc
        no-lock:
    
      itemfg-loc.q-onh = itemfg-loc.q-onh + fg-bin.qty.
    end.
    
    /*** itemfg.q-ono from jobs and purchase orders***/
    RUN fg/calcqool.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-ono).
    
    /*** itemfg.q-alloc & itemfg.q-back from customer orders ***/
    RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-alloc, OUTPUT itemfg-loc.q-back).
    
    itemfg-loc.q-avail = itemfg-loc.q-onh +
                 (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE itemfg-loc.q-ono) -
                 itemfg-loc.q-alloc.
END.

/* This section needed because q-alloc was not being calculated */
for each fg-set
    where fg-set.company eq itemfg.company
      and fg-set.part-no eq itemfg.i-no
    no-lock,

    first b-itemfg
    where b-itemfg.company eq itemfg.company
      and b-itemfg.i-no    eq fg-set.set-no
      and b-itemfg.isaset  eq YES
    no-lock,
    EACH b-itemfg-loc 
      WHERE b-itemfg-loc.company EQ b-itemfg.company
        AND b-itemfg-loc.i-no    EQ b-itemfg.i-no
      NO-LOCK,
    FIRST itemfg-loc 
      WHERE itemfg-loc.company EQ b-itemfg.company
        AND itemfg-loc.i-no    EQ itemfg.i-no
        AND itemfg-loc.loc     EQ b-itemfg-loc.loc
                       :
    ASSIGN
      /* itemfg-loc.q-ono = 0 */
      itemfg-loc.q-alloc = 0.
  /* 06111209 */
  {sys/inc/part-qty.i v-part-qty fg-set}

  v-part-qty = 
    (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE v-part-qty).
  
  assign
   /* itemfg-loc.q-ono   = itemfg-loc.q-ono   + (b-itemfg-loc.q-ono   * v-part-qty) */
   itemfg-loc.q-alloc = itemfg-loc.q-alloc + (b-itemfg-loc.q-alloc * v-part-qty).

end.
/* end ---------------------------------- copr. 1999  advanced software, inc. */
