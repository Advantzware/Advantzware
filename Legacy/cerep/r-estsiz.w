/* cerep/r-estsiz.w */

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
   {sys/inc/runAOAVer.i "ER8" }
END.

cAOAFile = SEARCH("AOA/r-estsiz.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-estsiz.p.
ELSE RUN cerep/r-estsizN.w PERSISTENT.
