/* salrep/r-yrtoyr.w */

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
   {sys/inc/runAOAVer.i "HR14" }
END.

cAOAFile = SEARCH("AOA/r-yrtoyr.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-yrtoyr.p.
ELSE RUN salrep/r-yrtoyrN.w PERSISTENT.
