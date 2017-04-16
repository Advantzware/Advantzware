/* custom/lookposL.i  &lookup-file, &lookup-field 
   find last record for descending order*/

  do with frame {&frame-name}:
    {&browse-name}:set-repositioned-row(int({&browse-name}:down / 2),"always").
          
    find LAST {&lookup-file}
        where {&KEY-PHRASE}
          and {&where-phrase}
        {&field-type}({&lookup-file}.{&lookup-field}) ge ip-cur-val
        no-lock no-error.
    if avail {&lookup-file} then 
       reposition {&browse-name} to rowid rowid({&lookup-file}) no-error.
       
    {&more-query}    
  end.
