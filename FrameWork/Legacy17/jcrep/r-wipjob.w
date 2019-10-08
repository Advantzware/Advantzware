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
   {sys/inc/selrptcol.i "JL7" }
END.

/*IF SelectRptColumn-log THEN - Only new selectable column  report exists */
    RUN jcrep/r-wipjobN.w PERSISTENT.

        
