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

cAOAFile = SEARCH("AOA/r-itcshp.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-itcshp.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN salrep/r-itcshN.w PERSISTENT.
ELSE RUN salrep/r-itcshA.w PERSISTENT.
