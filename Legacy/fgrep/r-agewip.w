/* fgrep/r-agewip.w */

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
   {sys/inc/runAOAVer.i "IL15" }
END.

cAOAFile = SEARCH("AOA/r-agewip.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-agewip.p.
ELSE RUN fgrep/r-agewipN.w.
