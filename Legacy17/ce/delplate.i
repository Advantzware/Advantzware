
for each est-prep
    where est-prep.company  eq ef.company
      AND est-prep.est-no   EQ ef.est-no
      and est-prep.s-num    eq ef.form-no
      and est-prep.mat-type eq "P",
      
    first prep
    where prep.company eq est-prep.company
      and prep.code    eq est-prep.code
      and prep.dfault  eq yes
    no-lock:
    
  delete est-prep.  
end.
