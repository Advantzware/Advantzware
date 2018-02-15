/* oerep/r-backl.w */

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
   {sys/inc/runAOAVer.i "OR1" }
END.

cAOAFile = SEARCH("AOA/r-backl.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-backl.p.
ELSE RUN oerep/r-backlN.w PERSISTENT.
