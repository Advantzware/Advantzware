/* cerep/r-prpmat.w */

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
   {sys/inc/runAOAVer.i "ER7" }
END.

cAOAFile = SEARCH("AOA/r-prpmat.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-prpmat.p.
ELSE RUN cerep/r-prpmatN.w PERSISTENT.
