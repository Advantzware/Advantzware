DEF BUFFER bf-rell FOR oe-rell.
DEF VAR lv-ord-no LIKE oe-rell.ord-no NO-UNDO.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

find sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ADDRELSE"
    no-lock no-error.
v-do-bol = avail sys-ctrl and sys-ctrl.log-fld and oe-ctrl.ship-from.

for each oe-relh
    where oe-relh.company  eq cocode
      AND oe-relh.release# = oe-bolh.release#
     /*
      and oe-relh.ord-no   eq oe-bolh.ord-no
      and oe-relh.rel-no   eq oe-bolh.rel-no
      and oe-relh.b-ord-no eq oe-bolh.b-ord-no
    use-index order*/ :
   
  for each oe-rell
      where oe-rell.company eq cocode
        and oe-rell.r-no    eq oe-relh.r-no
      use-index r-no:

    find first oe-boll
        where oe-boll.company eq cocode
          and oe-boll.b-no    eq oe-bolh.b-no
          and oe-boll.i-no    eq oe-rell.i-no
          and oe-boll.line    eq oe-boll.line
        use-index b-no no-lock no-error.
   
    if not avail oe-boll then do:
       find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-rell.ord-no
            and oe-ordl.i-no    eq oe-rell.i-no
            and oe-ordl.line    eq oe-rell.line
          use-index ord-no exclusive-lock no-error.

       if avail oe-ordl then oe-ordl.t-rel-qty = oe-ordl.t-rel-qty - oe-rell.qty.
       lv-ord-no = oe-rell.ord-no.
       if v-do-bol then delete oe-rell.      
       else oe-rell.posted = no.
      
       for each oe-rel
           where oe-rel.company eq cocode
             and oe-rel.ord-no  eq lv-ord-no
             and oe-rel.link-no ne 0
           use-index ord-item exclusive-lock:

         find first bf-rell
             where bf-rell.company eq oe-rel.company
               and bf-rell.r-no    eq oe-rel.link-no
               and bf-rell.link-no eq oe-rel.r-no
               and bf-rell.i-no    eq oe-rel.i-no
               and bf-rell.line    eq oe-rel.line
               and bf-rell.posted  eq yes
             use-index r-no no-lock no-error.
         if not avail bf-rell then oe-rel.link-no = 0.    
      
       end.

           end. /* avail oe-boll */  
  end. /* for each oe-rell */

  find first oe-rell where oe-rell.company eq cocode
                       and oe-rell.r-no    eq oe-relh.r-no
                       use-index r-no no-lock no-error.
  if not avail oe-rell then delete oe-relh.
end.  /* FOR EACH OE-RELH */


