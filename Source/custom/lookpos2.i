/* custom/lookpos3.i  &lookup-file, &lookup-field 
   find last record for ascending order*/

  do with frame {&frame-name}:
    {&browse-name}:set-repositioned-row(int({&browse-name}:down / 2),"always").
          
    find LAST {&lookup-file}
        where {&KEY-PHRASE}
          and {&field-type}({&lookup-field}) le ip-cur-val
             
        {&use-index} no-lock no-error.
    if avail {&lookup-file} then 
       reposition {&browse-name} to rowid rowid({&lookup-file}) no-error.
       
    {&more-query}    
  end.
