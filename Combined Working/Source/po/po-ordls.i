
find first b-ref1
    where b-ref1.reftable eq "POLSCORE"
      and b-ref1.company  eq {1}po-ordl.company
      and b-ref1.loc      eq "1"
      and b-ref1.code     eq string({1}po-ordl.po-no,"9999999999")
      and b-ref1.code2    eq string({1}po-ordl.line, "9999999999")
    no-error.
    
find first b-ref2
    where b-ref2.reftable eq "POLSCORE"
      and b-ref2.company  eq {1}po-ordl.company
      and b-ref2.loc      eq "2"
      and b-ref2.code     eq string({1}po-ordl.po-no,"9999999999")
      and b-ref2.code2    eq string({1}po-ordl.line, "9999999999")
    no-error.
