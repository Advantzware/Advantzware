
create reftable.
assign
 reftable.reftable = "AP-INVL"
 reftable.company  = ""
 reftable.loc      = ""
 reftable.code     = trim(string({1},"9999999999"))
 reftable.code2    = string(ap-invl.i-no,"9999999999").
