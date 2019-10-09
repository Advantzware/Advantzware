
where reftable.reftable eq "EST-MISC"
  and reftable.company  eq {1}.company
  and reftable.loc      eq {1}.loc
  and reftable.code     eq trim({1}.est-no) + string({1}.form-no,"/99")
