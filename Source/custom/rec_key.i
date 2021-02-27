ASSIGN 
    {1}.rec_key =   STRING(YEAR(TODAY),"9999") + 
                    STRING(MONTH(TODAY),"99") + 
                    STRING(DAY(TODAY),"99") + 
                    STRING(TIME,"99999") + 
                    STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999").
