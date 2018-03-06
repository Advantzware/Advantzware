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
   {sys/inc/runAOAVer.i "OZ6" }
END.

cAOAFile = SEARCH("AOA/r-hotsII.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-hotsII.p.
ELSE RUN oerep/r-hotIIN.w PERSISTENT.
