/* jcrep/r-jcost.w */

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
   {sys/inc/selrptcol.i "JL6" }
END.

/*IF SelectRptColumn-log THEN - Only new selectable column  report exists */
    RUN jcrep/r-jcostN.w PERSISTENT.

        
