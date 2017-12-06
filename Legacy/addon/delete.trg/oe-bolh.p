&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-bolh

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
/* =======
    {custom/globdefs.i}
DEF VAR cocode AS cha NO-UNDO.
DEF VAR v-do-bol AS LOG NO-UNDO.

cocode = g_company.

for each oe-boll of {&TABLENAME}:
    
    if oe-bolh.posted then oe-boll.deleted = yes.
    else delete oe-boll.

end.
MESSAGE "boll don" VIEW-AS ALERT-BOX.
{oe/bollrell.i}
    
 MESSAGE "rel done" VIEW-AS ALERT-BOX.

find first oe-relh where oe-relh.company eq cocode
                     and oe-relh.release# eq oe-bolh.release#
                     exclusive-lock no-error.
if avail oe-relh THEN if v-do-bol then delete oe-relh.
                      else oe-relh.posted = no.

if oe-bolh.posted then do:
    oe-bolh.deleted = yes.
    RETURN ERROR.
END.

MESSAGE "done" VIEW-AS ALERT-BOX.
========*/
