/* custom/lookpos.i  &lookup-file, &lookup-field  for descending order */

  do with frame {&frame-name}:
    {&browse-name}:set-repositioned-row(int({&browse-name}:down / 2),"always").
          
    find first {&lookup-file}
        where {&KEY-PHRASE}
          and {&field-type}({&lookup-file}.{&lookup-field}) BEGINS ip-cur-val
        no-lock no-error.
    if avail {&lookup-file} then 
       reposition {&browse-name} to rowid rowid({&lookup-file}) no-error.
       
    {&more-query}    
  end.
