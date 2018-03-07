/* fgrep/r-qtytot.w */

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
   {sys/inc/runAOAVer.i "IL1" }
END.

cAOAFile = SEARCH("AOA/r-qtytot.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-qtytot.p.
ELSE RUN fgrep/r-qtytotN.w PERSISTENT.
