/* t-sysctrl.p   need to assign rec_key */

for each sys-ctrl where rec_key = "" :

  disp name rec_key.
  PAUSE 0.
  sys-ctrl.rec_key = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999").

  CREATE rec_key.
  ASSIGN rec_key.rec_key = sys-ctrl.rec_key
         rec_key.table_name = "sys-ctrl".
         
