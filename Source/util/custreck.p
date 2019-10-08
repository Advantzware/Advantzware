/* custreck.p  from util/reckey.i */
def var ls-key as cha form "x(20)" no-undo.

for each cust where cust.rec_key = "".


ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey").

cust.rec_key = ls-key.               
create rec_key.
assign rec_key.rec_key = cust.rec_key
      rec_key.table_name = "cust".

DISP cust.cust-no cust.rec_key.
PAUSE 0.

END.
