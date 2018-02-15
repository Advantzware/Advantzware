/* porep/r-pofgh1.w */

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
   {sys/inc/runAOAVer.i "PR9" }
END.

cAOAFile = SEARCH("AOA/r-pofgh1.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-pofgh1.p.
ELSE RUN porep/r-pofgh1N.w PERSISTENT.
