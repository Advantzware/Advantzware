/* porep/r-matjob.w */

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
   {sys/inc/runAOAVer.i "PR4" }
END.

cAOAFile = SEARCH("AOA/r-matjob.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-matjob.p.
ELSE RUN porep/r-matjobN.w PERSISTENT.
