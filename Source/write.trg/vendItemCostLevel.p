&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME vendItemCostLevel

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEFINE BUFFER bf-vendItemCost FOR vendItemCost.

DISABLE TRIGGERS FOR LOAD OF bf-vendItemCost.

ASSIGN 
    {&TABLENAME}.updatedID = USERID('ASI')
    {&TABLENAME}.updatedDate = DATE(TODAY)
    .
    
FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK
    WHERE bf-vendItemCost.vendItemCostID EQ {&TABLENAME}.vendItemCostID
    NO-ERROR.
IF AVAILABLE bf-vendItemCost THEN 
    ASSIGN 
        bf-vendItemCost.updatedID = {&TABLENAME}.updatedID
        bf-vendItemCost.updatedDate = {&TABLENAME}.updatedDate
        .
    
