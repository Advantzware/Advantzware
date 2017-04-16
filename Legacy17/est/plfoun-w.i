
{1}
WHERE {1}.reftable EQ "PLATE/FOUNTAIN"
  AND {1}.company  EQ {2}.company
  AND {1}.loc      EQ {2}.est-no
  AND {1}.code     EQ STRING({2}.eqty,"9999999999")
  AND {1}.code2    EQ STRING({2}.form-no,"9999999999") +
                      STRING({2}.blank-no,"9999999999")
