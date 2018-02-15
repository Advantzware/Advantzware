/* cerep/r-style.w */

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
   {sys/inc/runAOAVer.i "ER3" }
END.

cAOAFile = SEARCH("AOA/r-style.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-style.p.
ELSE RUN cerep/r-styleN.w PERSISTENT.
