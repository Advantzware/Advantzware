
FIND FIRST reftable
    WHERE reftable.reftable EQ "itemfg.exempt-disc"
      AND reftable.company  EQ {1}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ {1}.i-no
    NO-LOCK NO-ERROR.
IF {1}.class EQ "*" OR AVAIL reftable AND reftable.val[1] EQ 1 THEN {2} = "0".
