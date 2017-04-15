
SESSION:SET-WAIT-STATE ("general").

FOR EACH itemfg,
    FIRST reftable
    WHERE reftable.reftable EQ "FGTAXABLE"
      AND reftable.company  EQ itemfg.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ itemfg.i-no:

  itemfg.taxable = reftable.val[1] EQ 1.
  DELETE reftable.
END.

SESSION:SET-WAIT-STATE ("").
