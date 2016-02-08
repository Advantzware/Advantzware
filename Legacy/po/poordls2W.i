
find first job-mat
    where job-mat.company    eq cocode
      and job-mat.rm-i-no    eq po-ordl.i-no
      and job-mat.job-no     eq string(fill(" ",6 -
                                            length(trim(po-ordl.job-no)))) +
                                trim(po-ordl.job-no)
      and job-mat.job-no2    eq po-ordl.job-no2
      and job-mat.i-no       eq po-ordl.i-no
      and ((job-mat.frm      eq po-ordl.s-num and po-ordl.s-num ne 0) or
           po-ordl.s-num     eq 0 OR po-ordl.s-num EQ ?)
      and ((job-mat.blank-no eq po-ordl.b-num and po-ordl.b-num ne 0) or
           po-ordl.b-num     eq 0)
    use-index i-no no-lock no-error.
