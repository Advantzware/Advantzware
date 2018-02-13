/* cerep/r-estmar.w */

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
   {sys/inc/runAOAVer.i "ER10" }
END.

cAOAFile = SEARCH("AOA/r-estmar.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-estmar.p.
ELSE RUN cerep/r-estmarN.w PERSISTENT.
