/* fgrep/r-cycnt.w */

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
   {sys/inc/runAOAVer.i "IC1" }
END.

cAOAFile = SEARCH("AOA/r-cycnt.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-cycnt.p.
ELSE RUN fgrep/r-cycntN.w PERSISTENT.
