/* prgrms.i */

&Scoped-define ITEM1 Security
&Scoped-define LABEL1 Security Access
&Scoped-define PROC1 ~
RUN Get_Procedure IN Persistent-Handle ('access.',OUTPUT run-proc,yes).
&Scoped-define ITEM2 Font_Color
&Scoped-define LABEL2 Set Fonts and Colors
&Scoped-define PROC2 ~
RUN Get_Procedure IN Persistent-Handle ('enhance.',OUTPUT run-proc,no). ~
IF run-proc NE '' THEN ~
~{methods/smartrun.i ({&FIRST-EXTERNAL-TABLE}.prgmname)}
&Scoped-define ITEM3 Menu_Bar
&Scoped-define LABEL3 Menu Bar
&Scoped-define PROC3 ~
RUN Get_Procedure IN Persistent-Handle ('usermenu.',OUTPUT run-proc,yes).
&Scoped-define ITEM4 MF_Design
&Scoped-define LABEL4 Misc. Fields Design
&Scoped-define PROC4 ~
RUN Get_Procedure IN Persistent-Handle ('miscflds.',OUTPUT run-proc,yes).
&Scoped-define ITEM5
&Scoped-define LABEL5
&Scoped-define PROC5
&Scoped-define ITEM6
&Scoped-define LABEL6
&Scoped-define PROC6
&Scoped-define ITEM7
&Scoped-define LABEL7
&Scoped-define PROC7
&Scoped-define ITEM8
&Scoped-define LABEL8
&Scoped-define PROC8

{methods/menus/options.i}
