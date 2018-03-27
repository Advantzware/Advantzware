/* --------------------------------------------------- oe/inv-qty.p 01/05 JLF */
/* Reset Invoice Qty on Order Line                                            */
/* -------------------------------------------------------------------------- */

def input  parameter v-rowid as rowid.
def output parameter v-qty   as int.

{sys/inc/VAR.i SHARED}


v-qty = 0.

find oe-ordl where rowid(oe-ordl) eq v-rowid no-lock no-error.

if avail oe-ordl and not oe-ordl.is-a-component then
for each ar-invl
    where ar-invl.company eq cocode
      and ar-invl.ord-no  eq oe-ordl.ord-no
      and ar-invl.i-no    eq oe-ordl.i-no
    no-lock:
    
  v-qty = v-qty + ar-invl.inv-qty.
end.
