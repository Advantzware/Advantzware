/* u-fntclr.p */

{methods/defines/hndldefs.i}

RUN Get_Procedure IN Persistent-Handle ("enhance.",OUTPUT run-proc,no).
IF run-proc NE "" THEN
{methods/smartrun.i "('')"}
