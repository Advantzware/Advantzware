/* jcrep/r-wipjob.w */

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN 
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
   {sys/inc/runAOAVer.i "JL7" }
END.

cAOAFile = SEARCH("AOA/r-wipjob.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-wipjob.p.
/*ELSE - Only new selectable column  report exists */
