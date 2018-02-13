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
   {sys/inc/runAOAVer.i "OZ1" }
END.

cAOAFile = SEARCH("AOA/r-hots.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-hots.p.
ELSE RUN oerep/r-hotsN.w PERSISTENT.
