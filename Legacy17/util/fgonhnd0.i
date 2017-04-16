
for each itemfg 
    where itemfg.company eq cocode
      and itemfg.i-no    ge fitm
      and itemfg.i-no    le titm
      and itemfg.cust-no ge fcus
      and itemfg.cust-no le tcus:

  IF tg_exclude-set-parts AND
     CAN-FIND(FIRST fg-set WHERE
     fg-set.company EQ cocode AND
     fg-set.part-no EQ itemfg.i-no AND
     fg-set.part-qty GT 0) THEN
     NEXT.

  status default " Processing...    Cust: " + itemfg.cust-no +
                             "      Item: " + itemfg.i-no.

  for each fg-bin
      where fg-bin.company eq itemfg.company
        and fg-bin.i-no    eq itemfg.i-no
        and fg-bin.loc     ge begin_whse
        and fg-bin.loc     le end_whse
        AND fg-bin.tag     GE begin_tag
        AND fg-bin.tag     LE END_tag
      use-index co-ino:

    RUN fg/cre-pchr.p (ROWID(fg-bin), "C", 0, 0).

    assign
     itemfg.q-onh = itemfg.q-onh - fg-bin.qty
     fg-bin.qty   = 0.
    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT fg-bin.loc).
    FIND FIRST itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
                            AND itemfg-loc.i-no    EQ itemfg.i-no
                            AND itemfg-loc.loc     EQ fg-bin.loc
                          EXCLUSIVE-LOCK NO-ERROR.
    IF avail itemfg-loc THEN
      itemfg-loc.q-onh = itemfg-loc.q-onh - fg-bin.qty.
  end.

  itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
  IF AVAIL itemfg-loc THEN
    itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
end.

status default "".
