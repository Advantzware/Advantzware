/* oerep/r-ontim2.w */

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
   {sys/inc/selrptcol.i "DR15" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-ontim2N.w PERSISTENT.
ELSE RUN oerep/r-ontim2A.w PERSISTENT.