/* fgrep/r-trnord.w */

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
   {sys/inc/runAOAVer.i "IR11" }
END.

cAOAFile = SEARCH("AOA/r-trnord.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-trnord.p.
ELSE RUN fgrep/r-tnordN.w PERSISTENT.
