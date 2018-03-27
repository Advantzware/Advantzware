/* oerep/r-sched2.w */

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
   {sys/inc/runAOAVer.i "OR12" }
END.

cAOAFile = SEARCH("AOA/r-sched2.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-sched2.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN oerep/r-shed2N.w PERSISTENT.
ELSE RUN oerep/r-shed2A.w PERSISTENT.
