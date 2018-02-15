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
   {sys/inc/runAOAVer.i "OZ7" }
END.

cAOAFile = SEARCH("AOA/r-Opnord.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-Opnord.p.
ELSE RUN oerep/r-opordN.w PERSISTENT.
