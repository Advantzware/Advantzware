/* util/reckey3.p */
DEF INPUT PARAM ip-table AS cha.
DEF OUTPUT PARAM op-rec_key AS cha .

def var ls-key as cha form "x(20)" no-undo.

ls-key = string(today,"99999999") +
               string(next-value(rec_key_seq,nosweat),"99999999").

op-rec_key = ls-key.               
create rec_key.
assign rec_key.rec_key = ls-key
       rec_key.table_name = ip-table.

