USING system.SharedConfig.
DEFINE VARIABLE scInstance AS CLASS system.SharedConfig NO-UNDO.
DEFINE VARIABLE cRowid AS CHARACTER NO-UNDO.

ASSIGN 
     scInstance = SharedConfig:instance
     cRowid     =  STRING(scInstance:GetValue("ItemFGPo")) NO-ERROR.

 RUN poinq/b-poinfo.w (TO-ROWID(cRowid), "*ALL"). 
 
 scInstance = SharedConfig:instance.
 scInstance:DeleteValue(INPUT "ItemFGPo").  
