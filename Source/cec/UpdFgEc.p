/* cec/UpdFgEC.p Update fgitem on EC */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER ipcRowid AS ROWID NO-UNDO .

DEFINE BUFFER bf-eb FOR eb .

FIND FIRST bf-eb NO-LOCK
    WHERE bf-eb.company EQ ipcCompany 
      AND ROWID(bf-eb) EQ ipcRowid NO-ERROR .

IF AVAIL bf-eb THEN do:
    
    FIND FIRST itemfg EXCLUSIVE-LOCK 
        WHERE itemfg.company EQ ipcCompany
          AND itemfg.i-no    EQ bf-eb.stock NO-ERROR .
    
    IF AVAIL itemfg THEN
        ASSIGN 
        itemfg.case-count = bf-eb.cas-cnt
        itemfg.case-pall = bf-eb.cas-pal
        itemfg.trno = bf-eb.tr-no 
        itemfg.spare-char-4 = bf-eb.dest-code .

    RELEASE itemfg. 
END.
