/* oerep/r-ontim2.w */

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
   {sys/inc/runAOAVer.i "DR15" }
END.

cAOAFile = SEARCH("AOA/r-ontim2.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-ontim2.p.
ELSE RUN oerep/r-ontim2N.w PERSISTENT.
