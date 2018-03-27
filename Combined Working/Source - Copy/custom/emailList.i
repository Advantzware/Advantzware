/* emailList.i - rstark 7.15.2005 */

&IF DEFINED(recKey) EQ 0 &THEN
DEFINE VARIABLE recKey AS CHARACTER NO-UNDO.
&SCOPED-DEFINE recKey cust.rec_key
&ENDIF

&IF DEFINED(prgmName) EQ 0 &THEN
&SCOPED-DEFINE prgmName v-prgmname
&ENDIF

&IF DEFINED(emailList) EQ 0 &THEN
DEFINE VARIABLE emailList AS CHARACTER NO-UNDO.
&SCOPED-DEFINE emailList emailList
&ENDIF

RUN custom/emailList.p ({&recKey},{&prgmName},OUTPUT {&emailList}).
