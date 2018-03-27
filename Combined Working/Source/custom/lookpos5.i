/* custom/lookpos5.i  &lookup-file, &lookup-field 
   find last record for ascending order*/
SESSION:SET-WAIT-STAT("general").
  do with frame {&frame-name}:
    {&browse-name}:set-repositioned-row(int({&browse-name}:down / 2),"always").
    /*      
    MESSAGE "repostion , {&fld-name-{1}} " ip-cur-val SKIP
         "Key - {&key-phrase}"  skip
        "Where - {&where-phrase}" VIEW-AS ALERT-BOX.
      */
    find first {&lookup-file}
        where {&KEY-PHRASE}
          and {&field-type}({&lookup-field}) begins ip-cur-val
         {&where-phrase}
        {&use-index} no-lock no-error.
    if avail {&lookup-file} then 
       reposition {&browse-name} to rowid rowid({&lookup-file}) no-error.

    {&more-query}    
  end.
SESSION:SET-WAIT-STAT("").
