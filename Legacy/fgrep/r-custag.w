/* fgrep/r-custag.w */

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
   {sys/inc/runAOAVer.i "IR7" }
END.

cAOAFile = SEARCH("AOA/r-custag.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-custag.p.
ELSE RUN fgrep/r-custgN.w PERSISTENT.
