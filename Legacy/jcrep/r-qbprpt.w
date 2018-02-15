/* jcrep/r-qbprpt.w */

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
   {sys/inc/runAOAVer.i "PR13" }
END.

cAOAFile = SEARCH("AOA/r-qbprpt.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-qbprpt.p.
ELSE RUN jcrep/r-qbprptN.w PERSISTENT.
