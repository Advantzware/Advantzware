/* custom/lookpos3.i  &lookup-file, &lookup-field */

  do with frame {&frame-name}:
    {&browse-name}:set-repositioned-row(int({&browse-name}:down / 2),"always"). 

    find first {&lookup-file}
        where {&KEY-PHRASE}
          AND {&lookup-file}.company = ip-company
          and {&field-type}({&lookup-file}.{&lookup-field}) ge ip-cur-val
        no-lock no-error.

    if avail {&lookup-file} then 
       reposition {&browse-name} to rowid rowid({&lookup-file}) no-error.

    {&more-query}    
  end.
