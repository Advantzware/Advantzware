/* salrep/r-slmprd.w */

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
   {sys/inc/runAOAVer.i "HW" }
END.

cAOAFile = SEARCH("AOA/r-slmprd.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-slmprd.p.
ELSE RUN salrep/r-slmprdN.w PERSISTENT.
