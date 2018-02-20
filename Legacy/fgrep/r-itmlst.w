/* fgrep/r-itmlst.w */

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
   {sys/inc/runAOAVer.i "IR9" }
END.

cAOAFile = SEARCH("AOA/r-itmlst.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-itmlst.p.
ELSE RUN fgrep/r-itmltN.w PERSISTENT.
