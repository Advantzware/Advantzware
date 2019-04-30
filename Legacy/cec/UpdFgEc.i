/* cec/UpdFgEC.i Update fgitem on EC */


 FIND FIRST itemfg EXCLUSIVE-LOCK 
           WHERE itemfg.company EQ eb.company
           AND itemfg.i-no    EQ eb.stock-no NO-ERROR .
    IF AVAIL itemfg THEN
        ASSIGN 
            itemfg.trno = eb.tr-no 
            itemfg.spare-char-4 = eb.dest-code .
    RELEASE itemfg. 