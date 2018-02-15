/* salrep/r-itcshp.w */
DEF VAR lvhRun-Proc AS HANDLE NO-UNDO.

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
   {sys/inc/runAOAVer.i "HR5" }
END.

cAOAFile = SEARCH("AOA/r-itcshp.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-itcshp.p.
ELSE RUN salrep/r-itcshN.w PERSISTENT.
