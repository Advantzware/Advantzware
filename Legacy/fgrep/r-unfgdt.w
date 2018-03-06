/* fgrep/r-unfgdt.w */

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
   {sys/inc/runAOAVer.i "IL3" }
END.

cAOAFile = SEARCH("AOA/r-unfgdt.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-unfgdt.p.
ELSE RUN fgrep/r-unfgdtN.w PERSISTENT.
