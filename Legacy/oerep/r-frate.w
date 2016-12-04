/* oerep/r-frate.w */

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
   {sys/inc/selrptcol.i "OR15" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-frateN.w PERSISTENT.
ELSE RUN oerep/r-frateA.w PERSISTENT.