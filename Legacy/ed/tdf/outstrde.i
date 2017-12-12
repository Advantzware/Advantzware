substring(str_buffa,{2},{3}) = fill(' ',{3})
str_bigint = {1} * exp(10, {4})
str_bigchar = string(str_bigint,'-9999999999999999999')
substring(str_buffa, {2}, {3}) = string({4},'9')
/* substring(str_buffa, {2} + 1, {3} - 1) =  */
    + substring(str_bigchar, 20 - ({3} - 2), {3} - 1)

SUBSTRING(str_buffa, 300, 300) = (IF ws_filetype EQ 'EDI' THEN TRIM(SUBSTRING(str_buffa, 300, 300)) + ws_elem_delim + 
  string({4},'9') + substring(str_bigchar, 20 - ({3} - 2), {3} - 1)
  ELSE "")