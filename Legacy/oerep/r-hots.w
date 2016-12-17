/* oerep/r-hots.w  OZ1 */

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
   {sys/inc/selrptcol.i "OZ1" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-hotsN.w PERSISTENT.
ELSE RUN oerep/r-hotsA.w PERSISTENT.