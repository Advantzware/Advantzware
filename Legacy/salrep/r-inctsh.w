/* salrep/r-inctsh.w */

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
   {sys/inc/runAOAVer.i "HR13" }
END.

cAOAFile = SEARCH("AOA/r-inctsh.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-inctsh.p.
ELSE RUN salrep/r-inctshN.w PERSISTENT.
