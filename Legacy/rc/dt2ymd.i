    if {1} = ? then '00000000' 
    else substring(string({1},'99999999'),5,4) 
       + substring(string({1},'99999999'),1,2)
       + substring(string({1},'99999999'),3,2)
