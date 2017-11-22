    if {1} = ? then '000000' 
    else substring(string({1},'999999'),5,2) 
       + substring(string({1},'999999'),1,2)
       + substring(string({1},'999999'),3,2)
