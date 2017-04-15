
WHERE (reftable.reftable EQ "AP-INVL"
  AND  reftable.company  EQ ""
  AND  reftable.loc      EQ ""
  AND  reftable.code     EQ string({1},"9999999999"))
  AND (reftable.spare-char-1 EQ "" OR reftable.spare-char-1 = po-ordl.company )
