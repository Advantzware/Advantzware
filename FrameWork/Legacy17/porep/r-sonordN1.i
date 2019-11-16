/* porep/r-sonordN1.i */

WHEN "vFGItem#" THEN cVarValue = IF AVAIL job-hdr THEN job-hdr.i-no ELSE "".
WHEN "ld-oqty" THEN cVarValue = string(ld-oqty,"->>>,>>>,>>9.9<<<<<").
WHEN "ld-rqty" THEN cVarValue = string(ld-rqty,"->>>,>>>,>>9.9<<<<<").
WHEN "v-cst-rem" THEN cVarValue = string(v-cst-rem,">>,>>>,>>9").
WHEN "v-msf-rem" THEN cVarValue = string(v-msf-rem,">>,>>9.999").
WHEN "v-ord-no" THEN cVarValue = STRING(po-ordl.ord-no,">>>>>>").
WHEN "v-cust-name" THEN cVarValue = v-prt-name.
WHEN "v-vend-name" THEN cVarValue = substring(v-Vend-name,1,30).
WHEN "lv-uom" THEN cVarValue = lv-uom.
WHEN "v-wid" THEN cVarValue = string(po-ordl.s-wid,">>,>>9.99<<<").
WHEN "v-len" THEN cVarValue = string(po-ordl.s-len,">>,>>9.99<<<").
WHEN "v-job-no" THEN cVarValue = IF po-ordl.job-no <> "" then po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99") ELSE "".
WHEN "v-jobDueDate" THEN cVarValue = IF v-JobDueDate <> ? THEN string(v-jobDueDate) ELSE "".
