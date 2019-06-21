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

  v-part-qty = v-qty * tt-fg-set.part-qty-dec.
  
  if v-field eq "q-onh" then
    b-itemfg.q-onh = b-itemfg.q-onh + v-part-qty.
    
  if v-field eq "q-ono" and not b-itemfg.pur-man then do:
    find first eb
        where eb.company eq cocode 
          and eb.est-no  eq v-est-no
          and eb.part-no eq b-itemfg.part-no
        no-lock no-error.
        
    if avail eb or v-est-no eq "" then
      b-itemfg.q-ono = b-itemfg.q-ono + v-part-qty.
  end.
    
  if v-field eq "q-alloc" then
    b-itemfg.q-alloc = b-itemfg.q-alloc + v-part-qty.
      
  if v-field eq "q-back" then
    b-itemfg.q-back = b-itemfg.q-back + v-part-qty.
    
  if not program-name(2) begins "jc/jc-calc." then do:
    if b-itemfg.q-ono lt 0 then b-itemfg.q-ono = 0.
  
    if b-itemfg.q-alloc lt 0 then b-itemfg.q-alloc = 0.
  
    if b-itemfg.q-back lt 0 then b-itemfg.q-back = 0.
  end.
    
  b-itemfg.q-avail = b-itemfg.q-onh + b-itemfg.q-ono - b-itemfg.q-alloc.
end.
  
