/* fgrep/r-stajob.w */

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
   {sys/inc/runAOAVer.i "AR12" }
END.

cAOAFile = SEARCH("AOA/r-custt.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-custt.p.
ELSE RUN arrep/r-custtN.w PERSISTENT.
