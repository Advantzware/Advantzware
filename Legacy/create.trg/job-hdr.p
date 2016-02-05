&Scoped-define TABLENAME job-hdr

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.


FIND LAST b-{&TABLENAME}
    WHERE ROWID(b-{&TABLENAME}) NE ROWID({&TABLENAME})
    USE-INDEX j-no NO-LOCK NO-ERROR.
{&TABLENAME}.j-no = (IF AVAIL b-{&TABLENAME} THEN b-{&TABLENAME}.j-no ELSE 0) + 1.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
