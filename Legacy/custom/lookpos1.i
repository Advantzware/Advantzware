/* custom/lookpos1.i  &lookup-file using rowid */

  do with frame {&frame-name}:
    {&browse-name}:set-repositioned-row(int({&browse-name}:down / 2),"always").
          
    find first {&lookup-file}
        where {&KEY-PHRASE}
          and ROWID({&lookup-file}) EQ ip-cur-val
        no-lock no-error.
    if avail {&lookup-file} then 
       reposition {&browse-name} to rowid rowid({&lookup-file}) no-error.
       
    {&more-query}    
  end.
