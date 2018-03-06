/* salrep/r-prfinv.w */

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
   {sys/inc/runAOAVer.i "GR5" }
END.

cAOAFile = SEARCH("AOA/r-glhist.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-glhist.p.
ELSE RUN glrep/r-glhstN.w PERSISTENT.
