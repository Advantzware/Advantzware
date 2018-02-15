/* porep/r-rctcrd.w */

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
   {sys/inc/runAOAVer.i "PR10" }
END.

cAOAFile = SEARCH("AOA/r-rctcrd.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-rctcrd.p.
ELSE RUN porep/r-rctcrdN.w PERSISTENT.
