
{1}.rec_key = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999").

CREATE rec_key.
ASSIGN
    rec_key.rec_key    = {1}.rec_key
    rec_key.table_name = "{1}"
    .
