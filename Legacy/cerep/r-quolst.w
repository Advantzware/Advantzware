/* cerep/r-quolst.w */

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
   {sys/inc/runAOAVer.i "ER9" }
END.

cAOAFile = SEARCH("AOA/r-quolst.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-quolst.p.
ELSE RUN cerep/r-quolstN.w PERSISTENT.
