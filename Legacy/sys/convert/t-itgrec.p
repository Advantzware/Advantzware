/* t-cstrec.p  from util/reckey.i */
def var ls-key as cha form "x(20)" no-undo.
DISABLE TRIGGERS FOR LOAD OF itemfg.

for each itemfg where itemfg.rec_key = "".


ls-key = string(today,"99999999") +
               string(next-value(rec_key_seq,nosweat),"99999999").

itemfg.rec_key = ls-key.               
create rec_key.
assign rec_key.rec_key = itemfg.rec_key
      rec_key.table_name = "itemfg".

DISP itemfg.rec_key.
PAUSE 0.

END.
