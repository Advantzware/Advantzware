/*
for each est where trim(est-no )= "4830"  no-lock,
    each ef where ef.est-no =  est.est-no no-lock,
    each eb where eb.est-no = est.est-no no-lock:
    
      disp est.est-type
           ef.est-no ef.form-no eb.form-no label "EB-Form" blank-no
           eb.part-no
      .   
      
      end.
*/
    
    find first est where trim(est-no )= "5036"  no-lock.
    for each ef where ef.est-no =  est.est-no no-lock:
        disp ef.est-no est.est-type ef.form-no 
            with title "EF".
    end.
    for each eb where eb.est-no = est.est-no no-lock by eb.form-no:
        disp eb.form-no eb.blank-no eb.part-no
            with title "EB" col 40.
    end.
      
      
  
  /*
  for each est no-lock by est-no descending :
     disp est-no est-type.
  end.    
    */
      pause.
