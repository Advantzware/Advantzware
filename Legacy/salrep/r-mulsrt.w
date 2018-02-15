/* salrep/r-mulsrt.w */

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
   {sys/inc/runAOAVer.i "HR10" }
END.

cAOAFile = SEARCH("AOA/r-mulsrt.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-mulsrt.p.
ELSE RUN salrep/r-mulsrtN.w PERSISTENT.
