  
  IF TRIM(itemfg.i-no) NE "" THEN DO:
    choice = no.
    message "Do you wish to delete bins with zero qty?"
            view-as question button yes-no
            update choice.
    
    if choice then
    for each fg-bin
        where fg-bin.company eq itemfg.company
          and fg-bin.i-no    eq itemfg.i-no
          and fg-bin.qty     eq 0:
      delete fg-bin.
    end.
    
    choice = no.
    message "Do you wish to delete bins with negative qty?"
            view-as question button yes-no
            update choice.
    
    if choice then
    for each fg-bin
        where fg-bin.company eq itemfg.company
          and fg-bin.i-no    eq itemfg.i-no
          and fg-bin.qty     lt 0:
      
      RUN fg/cre-pchr.p (ROWID(fg-bin), "C", 0, 0).
       
      delete fg-bin.
    end.
    
    run fg/fg-reset.p (recid(itemfg)).
  END.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
