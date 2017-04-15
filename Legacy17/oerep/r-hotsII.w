/* oerep/r-hotsII.w */

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
   {sys/inc/selrptcol.i "OZ6" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-hotIIN.w PERSISTENT.
ELSE RUN oerep/r-hotIIA.w PERSISTENT.