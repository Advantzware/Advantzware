/* helpcont.p */

{methods/defines/hndldefs.i}

RUN Get_Procedure IN Persistent-Handle ("help.",OUTPUT run-proc,no).
IF run-proc NE "" THEN
RUN VALUE(run-proc) ("mainmenu.",-1).
