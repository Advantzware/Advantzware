
CREATE reftable.
ASSIGN
 reftable.reftable = "AP-INVL"
 reftable.company  = ""
 reftable.loc      = ""
 reftable.code     = TRIM(STRING({1},"9999999999"))
 reftable.code2    = STRING(ap-invl.i-no,"9999999999")
 reftable.spare-char-1 = ap-invl.company.
