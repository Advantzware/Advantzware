/* salrep/r-prffrt.w */

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
   {sys/inc/runAOAVer.i "HR12" }
END.

cAOAFile = SEARCH("AOA/r-prffrt.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-prffrt.p.
ELSE RUN salrep/r-prffrtN.w PERSISTENT.
