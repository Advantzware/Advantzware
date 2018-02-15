/* fgrep/r-fgpstr.w */

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
   {sys/inc/runAOAVer.i "IL6" }
END.

cAOAFile = SEARCH("AOA/r-fgpstr.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-fgpstr.p.
ELSE RUN fgrep/r-fgpstrN.w PERSISTENT.
