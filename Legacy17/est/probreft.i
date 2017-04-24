
WHERE {1}.reftable EQ "est/probeset.i"
  AND {1}.company  EQ {2}.company
  AND {1}.loc      EQ ""
  AND {1}.code     EQ {2}.est-no
  AND {1}.code2    EQ STRING(probe.probe-date,"99/99/9999")
  AND {1}.val[1]   EQ {2}.est-qty
