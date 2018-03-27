
SUBSTRING(str_buffa, {2}, {3}) = (if ({1}) = ? then ' ' else ({1}) ) 

SUBSTRING(str_buffa, 300, 300) = (IF ws_filetype EQ 'EDI' THEN TRIM(SUBSTRING(str_buffa, 300, 300)) + ws_elem_delim + 
  (if ({1}) = ? then ' ' else ({1}) )
  ELSE "")
