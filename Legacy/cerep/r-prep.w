/* cerep/r-prep.w */

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
   {sys/inc/selrptcol.i "ER4" }
END.

IF SelectRptColumn-log THEN RUN cerep/r-prepN.w PERSISTENT .
ELSE RUN cerep/r-prepA.w PERSISTENT .





PROCEDURE dispatch :
    DEFINE INPUT  PARAMETER pcDummy AS CHARACTER NO-UNDO.


    IF THIS-PROCEDURE:PERSISTENT THEN 
        DELETE OBJECT THIS-PROCEDURE . 

END PROCEDURE . 
