/* -------------------------------------------------- oe/invpost3.i 04/99 JLF */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */

  if inv-line.ord-no ne 0 then
    assign
     oe-bolh.inv-no = inv-head.inv-no
     oe-boll.inv-no = inv-head.inv-no.

  if not oe-ctrl.u-inv or inv-line.ord-no eq 0 then do:
    {oe/oe-bolp.i "inv-line"}
      
    oe-bolh.w-ord = no.
  end.
    
  if inv-line.ord-no eq 0 then do:
    delete oe-bolh.
    delete oe-boll.
  end.
  
/* END ---------------------------------- copr. 1999  Advanced Software, Inc. */
