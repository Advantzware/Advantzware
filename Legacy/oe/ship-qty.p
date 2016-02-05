/* -------------------------------------------------- oe/ship-qty.p 02/01 JLF */
/* Reset Ship Qty on Order Line                                               */
/* -------------------------------------------------------------------------- */

def input  parameter v-rowid as rowid.
def output parameter v-qty   as int.

{sys/inc/VAR.i SHARED}


v-qty = 0.

find oe-ordl where rowid(oe-ordl) eq v-rowid no-lock no-error.

if avail oe-ordl then do:
  for each oe-boll
      where oe-boll.company eq cocode
        and oe-boll.ord-no  eq oe-ordl.ord-no
        and oe-boll.i-no    eq oe-ordl.i-no
        and oe-boll.line    eq oe-ordl.line
        and oe-boll.s-code  ne "I"
        and oe-boll.s-code  ne "T"
        and can-find(first oe-bolh where oe-bolh.b-no   eq oe-boll.b-no
                                     and oe-bolh.posted eq yes)
      use-index ord-no no-lock:
    
    v-qty = v-qty + oe-boll.qty.
  end.

  for each ar-invl
      where ar-invl.company eq cocode
        and ar-invl.ord-no  eq oe-ordl.ord-no
        and ar-invl.i-no    eq oe-ordl.i-no
      use-index ord-no no-lock,
      
      first ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock,
      
      each oe-reth
      where oe-reth.company eq cocode
        and oe-reth.posted  eq yes
        and oe-reth.applied eq yes
        and oe-reth.cust-no eq ar-inv.cust-no
        and oe-reth.inv-no  eq ar-inv.inv-no
      use-index posted no-lock,
      
      each oe-retl
      where oe-retl.company eq cocode
        and oe-retl.r-no    eq oe-reth.r-no
        and oe-retl.i-no    eq ar-invl.i-no
      no-lock:
        
    v-qty = v-qty - oe-retl.tot-qty-return.  
  end.
end.
