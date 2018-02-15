/* porep/r-schrpt.w */

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
   {sys/inc/runAOAVer.i "PR2" }
END.

cAOAFile = SEARCH("AOA/r-schrpt.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-schrpt.p.
ELSE RUN porep/r-shrptN.w PERSISTENT.
