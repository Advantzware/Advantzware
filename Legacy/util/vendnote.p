/* util/reckey.i */
def var ls-key as cha form "x(20)" no-undo.

SESSION:SET-WAIT-STATE("general").

for each vend where vend.rec_key = "".


ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey").

vend.rec_key = ls-key.               
create rec_key.
assign rec_key.rec_key = vend.rec_key
       rec_key.table_name = "Vend".

DISP vend.vend-no vend.rec_key.
PAUSE 0.

END.

SESSION:SET-WAIT-STATE("").
