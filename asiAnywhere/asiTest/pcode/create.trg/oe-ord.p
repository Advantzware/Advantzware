&Scoped-define TABLENAME oe-ord

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}


RUN oe/ordupdat.p ({&TABLENAME}.company, {&TABLENAME}.ord-no, YES).

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
