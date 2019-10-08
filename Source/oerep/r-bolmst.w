/* oerep/r-comms.w */

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.

assign
 cocode = gcompany
 locode = gloc.

RUN sys/ref/nk1look.p (INPUT cocode, "BOLMaster", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
/*
DO TRANSACTION:
   {sys/inc/selrptcol.i "OR6" }
END.*/
IF cRtnChar EQ "" THEN RUN oerep/r-bolmstA.w PERSISTENT.
ELSE  IF cRtnChar EQ "Trailer#" THEN RUN oerep/r-bolmstA.w PERSISTENT.
ELSE RUN oerep/r-bolmstN.w PERSISTENT.
