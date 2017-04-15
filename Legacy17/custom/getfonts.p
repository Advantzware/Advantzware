/* custom/getfonts.p   06/04/04 YSK */

&IF DEFINED(uib_is_running) = 0 &THEN
DEF VAR /*input PARAM*/ ip-font AS INT NO-UNDO.
DEF VAR /*OUTPUT PARAM*/  op-font AS cha NO-UNDO.
DEF var /*OUTPUT PARAM*/ op-pitch AS INT NO-UNDO.
DEF var /*OUTPUT PARAM*/ op-bold AS cha NO-UNDO.
ip-font = 11.
&else
DEF input PARAM ip-font AS INT NO-UNDO.
DEF OUTPUT PARAM  op-font AS cha NO-UNDO.
DEF OUTPUT PARAM op-pitch AS INT NO-UNDO.
DEF OUTPUT PARAM op-bold AS cha NO-UNDO.
&ENDIF

DEF VAR myfont AS cha FORM "x(170)".
DEF VAR myfont-list AS cha .
DEF VAR v-font AS cha NO-UNDO.
DEF VAR v-pitch AS cha NO-UNDO.
DEF VAR v-bold AS cha NO-UNDO.

DEF VAR i AS INT.



GET-KEY-VALUE SECTION "fonts" KEY "" VALUE myfont.
DISP
    NUM-ENTRIES(myfont)
    myfont VIEW-AS EDITOR SIZE 50 BY 10.


/*
DO i = 1 TO NUM-ENTRIES(myfont):

   GET-KEY-VALUE SECTION "fonts" KEY ENTRY(i,myfont) VALUE myfont-list.
   v-font = ENTRY(1,myfont-list).
   v-pitch = ENTRY(2,myfont-list).
   IF num-entries(myfont-list) > 2 THEN v-bold = ENTRY(3,myfont-list).
   DISP num-entries(myfont-list)
       v-font
       v-pitch
       v-bold
        .
   PAUSE.
END.
*/


GET-KEY-VALUE SECTION "fonts" KEY ENTRY(ip-font + 1,myfont) VALUE myfont-list.
v-font = ENTRY(1,myfont-list).
v-pitch = ENTRY(2,myfont-list).
IF num-entries(myfont-list) > 2 THEN v-bold = ENTRY(3,myfont-list).
v-pitch = SUBSTRING(v-pitch,7).

ASSIGN op-font = v-font
       op-pitch = int(v-pitch)
       op-bold = v-bold  
        .


DISP op-font FORM "x(30)"
     op-pitch op-bold.

