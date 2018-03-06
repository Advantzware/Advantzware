/* jcrep/r-jobprp.w */

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
   {sys/inc/runAOAVer.i "JL5" }
END.

cAOAFile = SEARCH("AOA/r-jobprp.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-jobprp.p.
ELSE RUN jcrep/r-jobprpN.w PERSISTENT.
