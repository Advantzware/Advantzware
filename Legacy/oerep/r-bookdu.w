/* oerep/r-bookdu.w */

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
   {sys/inc/runAOAVer.i "OR10" }
END.

cAOAFile = SEARCH("AOA/r-bookdu.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-bookdu.p.
ELSE RUN oerep/r-bokduN.w PERSISTENT.
