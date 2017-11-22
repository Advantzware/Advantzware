substring(str_buffa,{2},{3}) = fill(' ',{3})
str_bigint = {1} * exp(10, {4})
str_bigchar = string(str_bigint,'-9999999999999999999')
substring(str_buffa, {2}, {3}) = string({4},'9')
/* substring(str_buffa, {2} + 1, {3} - 1) =  */
    + substring(str_bigchar, 20 - ({3} - 2), {3} - 1)

