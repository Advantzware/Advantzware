
find first {1}b-ref1
    where {1}b-ref1.reftable eq "POLSCORE"
      and {1}b-ref1.company  eq {1}job-mat.company
      and {1}b-ref1.loc      eq "1"
      and {1}b-ref1.code     eq string(recid({1}job-mat),"9999999999")
      and {1}b-ref1.code2    eq ""
    no-error.
      
find first {1}b-ref2
    where {1}b-ref2.reftable eq "POLSCORE"
      and {1}b-ref2.company  eq po-ordl.company
      and {1}b-ref2.loc      eq "2"
      and {1}b-ref2.code     eq string(recid({1}job-mat),"9999999999")
      and {1}b-ref2.code2    eq ""
    no-error.
