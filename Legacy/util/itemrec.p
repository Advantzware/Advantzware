/* itemrec.p  from util/reckey.i */
def var ls-key as cha form "x(20)" no-undo.

for each item where item.rec_key = "".


ls-key = string(today,"99999999") +
               string(next-value(rec_key_seq,nosweat),"99999999").

item.rec_key = ls-key.               
create rec_key.
assign rec_key.rec_key = item.rec_key
      rec_key.table_name = "item".

DISP item.rec_key.
PAUSE 0.

END.

DEF BUFFER bf-item FOR ITEM.
FOR EACH ITEM:
   FIND FIRST bf-item WHERE bf-item.rec_key = ITEM.rec_key AND
                            RECID(bf-item) <> RECID(ITEM) NO-LOCK NO-ERROR.
   IF AVAIL bf-item THEN DO:
       ls-key = string(today,"99999999") +
                string(next-value(rec_key_seq,nosweat),"99999999").
        FIND FIRST bf-item WHERE bf-item.rec_key = ls-key NO-LOCK NO-ERROR.

        item.rec_key = ls-key.               
        create rec_key.
        assign rec_key.rec_key = item.rec_key
               rec_key.table_name = "item".

   END.
END.
