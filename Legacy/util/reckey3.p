/* util/reckey3.p */
DEF INPUT PARAM ip-table AS cha.
DEF OUTPUT PARAM op-rec_key AS cha .

def var ls-key as cha form "x(20)" no-undo.

ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey").

op-rec_key = ls-key.               
create rec_key.
assign rec_key.rec_key = ls-key
       rec_key.table_name = ip-table.

