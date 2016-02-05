/* -------------------------------------------------- fg/comp-upd.p 05/00 JLF */
/* Finished Goods - Update inventory qty for components of a set              */
/* -------------------------------------------------------------------------- */
/* v-e-num replaced with v-est-no in GUI */

def input parameter v-recid as   recid.
def input parameter v-qty   like itemfg.q-onh.
def input parameter v-field as   char.
def input parameter v-est-no like eb.est-no.

{sys/inc/var.i shared}

def buffer b-itemfg for itemfg.

{fg/fullset.i NEW}

def var v-part-qty as dec.


find itemfg where recid(itemfg) eq v-recid no-lock no-error.

if avail itemfg and itemfg.isaset THEN RUN fg/fullset.p (ROWID(itemfg)).

FOR EACH tt-fg-set,

    first b-itemfg
    where b-itemfg.company eq cocode
      and b-itemfg.i-no    eq tt-fg-set.part-no:
    find first eb
        where eb.company eq cocode 
          and eb.est-no  eq v-est-no
          and eb.part-no eq b-itemfg.part-no
        no-lock no-error.
    IF AVAIL(eb) THEN
      RUN fg/chkfgloc.p (INPUT b-itemfg.i-no, INPUT eb.loc).
    IF AVAIL(eb) THEN
    FIND FIRST itemfg-loc 
         WHERE itemfg-loc.company EQ b-itemfg.company
           AND itemfg-loc.i-no    EQ b-itemfg.i-no
           AND itemfg-loc.loc     EQ eb.loc
         EXCLUSIVE-LOCK NO-ERROR.

  v-part-qty = v-qty * 
      (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE tt-fg-set.part-qty-dec).
  
  if v-field eq "q-onh" then
    b-itemfg.q-onh = b-itemfg.q-onh + v-part-qty.
    
  if v-field eq "q-ono" and not b-itemfg.pur-man then do:

        
    if avail eb or v-est-no eq "" THEN DO:
        b-itemfg.q-ono = b-itemfg.q-ono + v-part-qty.       
        IF AVAIL itemfg-loc THEN
          itemfg-loc.q-ono = itemfg-loc.q-ono + v-part-qty.
    END.
      
  end.
    
  if v-field eq "q-alloc" THEN DO:
    b-itemfg.q-alloc = b-itemfg.q-alloc + v-part-qty.
    IF AVAIL(itemfg-loc) THEN
      itemfg-loc.q-alloc = itemfg-loc.q-alloc + v-part-qty.
  END.    
  if v-field eq "q-back" THEN DO:
  
    b-itemfg.q-back = b-itemfg.q-back + v-part-qty.
    IF AVAIL(itemfg-loc) THEN
      itemfg-loc.q-back = itemfg-loc.q-back + v-part-qty.
  END.  
  if not program-name(2) begins "jc/jc-calc." then do:
    if b-itemfg.q-ono lt 0 then DO:
        b-itemfg.q-ono = 0.
        IF AVAIL(itemfg-loc) THEN
          itemfg-loc.q-ono = 0.
    END.
  
    if b-itemfg.q-alloc lt 0 THEN DO: 
        b-itemfg.q-alloc = 0.
        IF AVAIL(itemfg-loc) THEN
          itemfg-loc.q-alloc = 0.
    END.
  
    if b-itemfg.q-back lt 0 then DO: 
        b-itemfg.q-back = 0.
        IF AVAIL(itemfg-loc) THEN
          itemfg-loc.q-back = 0.
    END.
  end.
    
  b-itemfg.q-avail = b-itemfg.q-onh + b-itemfg.q-ono - b-itemfg.q-alloc.
  IF AVAIL(itemfg-loc) THEN DO:
      itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
      FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
  END.
   
end.
  
