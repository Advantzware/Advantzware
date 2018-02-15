/* jcrep/r-fibaud.w */

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
   {sys/inc/runAOAVer.i "JR14" }
END.

cAOAFile = SEARCH("AOA/r-fibaud.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-fibaud.p.
ELSE RUN jcrep/r-fibaudN.w PERSISTENT.
