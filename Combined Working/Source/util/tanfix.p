
def temp-table tt-est like est.


input from d:\asigui\v8data\est.d no-echo.


repeat:
  insert tt-est.
  
  if est-type ne 3 then delete tt-est.
end.

for each tt-est,
    first est
    where est.company eq tt-est.company
      and est.loc     eq tt-est.loc
      and est.est-no  eq tt-est.est-no
    no-lock:
  for each est-prep
      where est-prep.company  eq est.company
        and est-prep.est-no   eq est.est-no
        and est-prep.mat-type eq "R"
        and est-prep.s-num    ne 1:
     delete est-prep.
  end.
  for each est-op
      where est-op.company  eq est.company
        and est-op.est-no   eq est.est-no
        and (est-op.dept    eq "DM" or est-op.dept eq "PD")
        and est-op.s-num    ne 1:
    delete est-op.
  end.
end.

