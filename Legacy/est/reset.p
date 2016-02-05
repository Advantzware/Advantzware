/*
for each ef .
delete ef.
end.
for each eb.
delete eb.
end.

for each est.
  delete est.
  end.
  for each est-qty.
    delete est-qty.
    end.
    for each est-prep.
       delete est-prep.
       end.
       for each est-op.
          delete est-op.
          end.

*/


  for each est no-lock:
     find first est-qty where est-qty.company = est.company and
                              est-qty.est-no = est.est-no
                              .
     find first eb of est.
     find first ef of est.
     disp eb.est-no label "B-est"
          ef.est-no label "F-est"
  
/*
  for each ef where est-no = "".
  disp est-no recid(ef) .        
  delete ef.
  */
