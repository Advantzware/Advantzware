/* oerep/r-backl1.w */

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
   {sys/inc/runAOAVer.i "OR13" }
END.

cAOAFile = SEARCH("AOA/r-backl1.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-backl1.p.
ELSE RUN oerep/r-back1N.w PERSISTENT.
