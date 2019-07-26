&Scoped-define TABLENAME estCostSummary

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.estCostSummaryID = NEXT-VALUE(estCostSummaryID_seq,ASI).

{methods/triggers/create.i}
