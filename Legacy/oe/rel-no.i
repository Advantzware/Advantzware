/* oe/rel-no.i */
  /** Find last actual release for this order number and add 1 to
     the get the next release. **/
  /* === rel-no logic moved to line (oe-rell) ========*/
  DEF BUFFER bf-rell FOR oe-rell .
  DEF VAR li-nxt-rel-no AS INT NO-UNDO.
  for each bf-rell where bf-rell.company eq g_company
      and bf-rell.ord-no  eq oe-rell.ord-no no-lock 
      by bf-rell.rel-no desc:
      li-nxt-rel-no =  bf-rell.rel-no.
      leave.  
  end.
  li-nxt-rel-no = li-nxt-rel-no + 1.

