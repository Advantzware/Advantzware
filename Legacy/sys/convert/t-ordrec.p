/*  f-ordrec.p  Update oe-ordl.rec_key */
/*
for each oe-ord where ord-no = 41822.
disp ord-no rec_key.
for each oe-ordl of oe-ord.
  disp i-no oe-ordl.rec_key.

  find first itemfg where itemfg.company = oe-ordl.company
     and itemfg.i-no = oe-ordl.i-no no-lock no-error.
     if avail itemfg then disp itemfg.i-no itemfg.rec_key.
     
  if oe-ordl.rec_key = "" and itemfg.rec_key <> "" then
  oe-ordl.rec_key = itemfg.rec_key.
  
  */   
  
  for each oe-ordl where rec_key = "".
      disp oe-ordl.ord-no oe-ordl.i-no .
      
  find first itemfg where itemfg.company = oe-ordl.company
        and itemfg.i-no = oe-ordl.i-no no-lock no-error.
     
     
     if avail itemfg then disp itemfg.rec_key.
     
       if oe-ordl.rec_key = "" and 
         avail itemfg and itemfg.rec_key <> "" then
              oe-ordl.rec_key = itemfg.rec_key.
  
  pause 0.
