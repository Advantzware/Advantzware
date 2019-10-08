/* oerep/r-ontime.w */

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
   {sys/inc/selrptcol.i "OZ7" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-opordN.w PERSISTENT.
ELSE RUN oerep/r-opordA.w PERSISTENT.