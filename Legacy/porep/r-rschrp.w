/* porep/r-rschrp.w */

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
   {sys/inc/runAOAVer.i "PR14" }
END.

cAOAFile = SEARCH("AOA/r-rschrp.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-rschrp.p.
ELSE RUN porep/r-rschrpN.w PERSISTENT.
