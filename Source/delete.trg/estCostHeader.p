&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME estCostHeader

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH estCostMisc EXCLUSIVE-LOCK
    WHERE estCostMisc.estCostHeaderID EQ {&TABLENAME}.estCostHeaderID:
    DELETE estCostMisc.
END. 
FOR EACH estCostOperation EXCLUSIVE-LOCK
    WHERE estCostOperation.estCostHeaderID EQ {&TABLENAME}.estCostHeaderID:
    DELETE estCostOperation.
END. 
FOR EACH estCostMaterial EXCLUSIVE-LOCK
    WHERE estCostMaterial.estCostHeaderID EQ {&TABLENAME}.estCostHeaderID:
    DELETE estCostMaterial.
END.
FOR EACH estCostForm EXCLUSIVE-LOCK 
    WHERE estCostForm.estCostHeaderID EQ {&TABLENAME}.estCostHeaderID:
    DELETE estCostForm.
END.    
FOR EACH estCostBlank EXCLUSIVE-LOCK 
    WHERE estCostBlank.estCostHeaderID EQ {&TABLENAME}.estCostHeaderID:
    DELETE estCostBlank.
END.
FOR EACH estCostItem EXCLUSIVE-LOCK 
    WHERE estCostItem.estCostHeaderID EQ {&TABLENAME}.estCostHeaderID:
    DELETE estCostItem.
END.
FOR EACH estCostDetail EXCLUSIVE-LOCK 
    WHERE estCostDetail.estCostHeaderID EQ {&TABLENAME}.estCostHeaderID:
    DELETE estCostDetail.
END.
FOR EACH estCostSummary EXCLUSIVE-LOCK 
    WHERE estCostSummary.estCostHeaderID EQ {&TABLENAME}.estCostHeaderID:
    DELETE estCostSummary.
END. 
