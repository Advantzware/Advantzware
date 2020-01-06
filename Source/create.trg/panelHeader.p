&Scoped-define TABLENAME panelHeader

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

panelHeader.panelHeaderID = NEXT-VALUE(panelHeaderID_seq).
