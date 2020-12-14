/* ------------------------------------------------ util/rm-mkbin.i 06/00 JLF */
/* Raw Materials bin rebuild program                                          */
/* -------------------------------------------------------------------------- */

for each item
    where item.company eq cocode
      and item.i-no    ge fitm
      and item.i-no    le titm
      and item.i-no    ne ""
     no-lock by item.i-no:

  status default " Processing...    Item: " + item.i-no.

  run rm/rm-mkbin.p (recid(item)).

  if vzer then
  for each rm-bin
      where rm-bin.company eq cocode
        and rm-bin.i-no    eq item.i-no
        and rm-bin.qty     eq 0:
    delete rm-bin.
  end.

  if vneg then
  for each rm-bin
      where rm-bin.company eq cocode
        and rm-bin.i-no    eq item.i-no
        and rm-bin.qty     lt 0:
        
    x = 1.
    RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT X) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

    create rm-rcpth.
    assign
     rm-rcpth.r-no       = x
     rm-rcpth.trans-date = today
     rm-rcpth.company    = cocode
     rm-rcpth.loc        = locode
     rm-rcpth.rita-code  = "C"
     rm-rcpth.i-no       = item.i-no
     rm-rcpth.post-date  = today
     rm-rcpth.i-name     = item.i-name
     rm-rcpth.pur-uom    = item.pur-uom.

    create rm-rdtlh.
    assign
     rm-rdtlh.r-no      = rm-rcpth.r-no
     rm-rdtlh.company   = cocode
     rm-rdtlh.loc       = rm-bin.loc
     rm-rdtlh.rita-code = "C"
     rm-rdtlh.loc-bin   = rm-bin.loc-bin
     rm-rdtlh.tag       = rm-bin.tag
     rm-rdtlh.qty       = 0.

    delete rm-bin.
  end.

  run rm/rm-reset.p (recid(item)).
  
  /*reset average cost */
  FOR EACH b-rm-bin FIELDS(qty cost) NO-LOCK
      WHERE b-rm-bin.company EQ item.company
      AND b-rm-bin.i-no    EQ item.i-no
      AND b-rm-bin.cost    NE ?:
      ASSIGN
          dCost = dCost + (b-rm-bin.cost *
                   (b-rm-bin.qty * IF b-rm-bin.qty LT 0 THEN -1 ELSE 1))
          dQty = dQty +
                   (b-rm-bin.qty * IF b-rm-bin.qty LT 0 THEN -1 ELSE 1).
  END.
  FIND CURRENT ITEM EXCLUSIVE-LOCK.
  item.avg-cost = IF dQty EQ 0 THEN 0 ELSE (dCost / dQty) .
  FIND CURRENT item NO-LOCK.
  
end. /* each item */

/* end ---------------------------------- copr. 1997  advanced software, inc. */
