/* jasper.i - rstark - 2.12.2019 */

&IF DEFINED(user-ID) EQ 0 &THEN
&Scoped-define user-ID USERID("ASI")
&ENDIF

&IF DEFINED(paramValueID) EQ 0 &THEN
&Scoped-define paramValueID 0
&ENDIF

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

RUN AOA/Jasper.p ({&subjectID}, {&user-ID}, "{&prgmName}", {&paramValueID}).
