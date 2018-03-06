/* oerep/r-sched.w */

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
   {sys/inc/runAOAVer.i "OR2" }
END.

cAOAFile = SEARCH("AOA/r-sched.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-sched.p.
ELSE RUN oerep/r-schedN.w PERSISTENT.
