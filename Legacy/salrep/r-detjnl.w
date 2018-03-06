/* salrep/r-detjnl.w */

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
   {sys/inc/runAOAVer.i "HB" }
END.

cAOAFile = SEARCH("AOA/r-detjnl.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-detjnl.p.
ELSE RUN salrep/r-detjnlN.w PERSISTENT.
