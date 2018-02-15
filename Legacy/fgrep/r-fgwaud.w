/* fgrep/r-fgwaud.w */

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
   {sys/inc/runAOAVer.i "IL14" }
END.

cAOAFile = SEARCH("AOA/r-fgwaud.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-fgwaud.p.
ELSE RUN fgrep/r-fgwaudN.w PERSISTENT.
