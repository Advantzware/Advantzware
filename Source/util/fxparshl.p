
SESSION:SET-WAIT-STATE ("general").

FOR EACH fg-bin,
    FIRST itemfg
    WHERE itemfg.company EQ fg-bin.company
      AND itemfg.i-no    EQ fg-bin.i-no
    NO-LOCK:

  IF fg-bin.case-count   LE 0 THEN fg-bin.case-count   = itemfg.case-count.
  IF fg-bin.case-count   LE 0 THEN fg-bin.case-count   = 1.
  IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
  IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.

  fg-bin.partial-count = fg-bin.qty - (TRUNC(fg-bin.qty / fg-bin.case-count,0) * fg-bin.case-count).
END.

SESSION:SET-WAIT-STATE ("").
