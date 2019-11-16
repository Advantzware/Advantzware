/* util/reckey.i */
def var ls-key as cha form "x(20)" no-undo.

for each {1} where {1}.rec_key = "".


ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey").

{1}.rec_key = ls-key.               
create rec_key.
assign rec_key.rec_key = item.rec_key
      rec_key.table_name = "item".

DISP {1}.rec_key.
PAUSE 0.

END.
