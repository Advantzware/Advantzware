
{1}
where {1}.reftable eq "MachinePosition"
  and {1}.company  eq string({2}.company,"x(10)") +
                      string({2}.est-no,"x(10)")
  and {1}.loc      eq string({2}.line - if {2}.line gt 500 then 500 else 0,"9999999999")
  and {1}.code     eq ""
  and {1}.code2    eq ""
