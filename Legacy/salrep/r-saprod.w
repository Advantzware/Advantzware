/* salrep/r-saprod.w */

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
   {sys/inc/runAOAVer.i "HF" }
END.

cAOAFile = SEARCH("AOA/r-saprod.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-saprod.p.
ELSE RUN salrep/r-saprodN.w PERSISTENT.
