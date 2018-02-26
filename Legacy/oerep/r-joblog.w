/* oerep/r-joblog.w */

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
   {sys/inc/runAOAVer.i "OR3" }
END.

cAOAFile = SEARCH("AOA/r-joblog.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-joblog.p.
ELSE RUN oerep/r-joblgN.w PERSISTENT.
