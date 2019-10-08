
CREATE {1}.
ASSIGN
 {1}.reftable = "PLATE/FOUNTAIN"
 {1}.company  = {2}.company
 {1}.loc      = {2}.est-no
 {1}.code     = STRING({2}.eqty,"9999999999")
 {1}.code2    = STRING({2}.form-no,"9999999999") +
                STRING({2}.blank-no,"9999999999")
 {1}.val[01]  = 1
 {1}.val[02]  = 1.
