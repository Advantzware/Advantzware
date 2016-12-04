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
   {sys/inc/selrptcol.i "HR5" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-itcshN.w PERSISTENT.
ELSE RUN salrep/r-itcshA.w PERSISTENT.

/*
APPLY 'close-window' TO THIS-PROCEDURE.
APPLY 'close' TO THIS-PROCEDURE.
lvhRun-Proc = THIS-PROCEDURE.
/* Without this, if running persistent, don't close */
DELETE OBJECT lvhRun-Proc NO-ERROR.
*/
