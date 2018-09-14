/* wndwmenu.i */

&IF "{&NOMENUS}" NE "yes" &THEN
&Scoped-define OPTIONS prgrms {custom/optsmenu.i}

&Scoped-define ITEM1 Search
&Scoped-define LABEL1 Search
&Scoped-define PROC1 RUN Run-Search IN THIS-PROCEDURE.
&Scoped-define ITEM2 List
&Scoped-define LABEL2 List
&Scoped-define PROC2 ~
DEFINE VARIABLE listname AS CHARACTER NO-UNDO. ~
listname = SUBSTR('{&FIRST-EXTERNAL-TABLE}',1,7) + '_.'. ~
RUN Get_Procedure IN Persistent-Handle ('listrqst.',OUTPUT run-proc,no). ~
IF run-proc NE '' THEN ~
~{methods/smartrun.i (listname)}
&Scoped-define ITEM3 Notes
&Scoped-define LABEL3 Notes
&Scoped-define PROC3 ~
RUN Get_Procedure IN Persistent-Handle ('notes.',OUTPUT run-proc,no). ~
IF run-proc NE '' THEN ~
~{methods/smartrun.i (rec_key_value,header_value)}
&Scoped-define ITEM4 Misc_Fields
&Scoped-define LABEL4 Misc Fields
&Scoped-define PROC4 ~
RUN Get_Procedure IN Persistent-Handle ('mfvalues.',OUTPUT run-proc,no). ~
IF run-proc NE '' THEN ~
~{methods/smartrun.i (b-prgrms.mfgroup,rec_key_value,header_value)}
/* ============ NOT in use 
&Scoped-define ITEM5 Browser
&Scoped-define LABEL5 Browser
&Scoped-define PROC5 RUN SELECT-PAGE (1).
&Scoped-define ITEM6 Viewer
&Scoped-define LABEL6 Viewer
&Scoped-define PROC6 RUN SELECT-PAGE (2).
=========================*/
&Scoped-define ITEM5 spec_note
&Scoped-define LABEL5 Spec Note
&Scoped-define PROC5 run Select_spec.
&Scoped-define ITEM6 Util_Appl
&Scoped-define LABEL6 Run App
&Scoped-define PROC6 RUN Select_appl.
&Scoped-define ITEM7 Exit
&Scoped-define LABEL7 E&xit
&Scoped-define PROC7 ~{methods/template/exit.i}
&Scoped-define ITEM8 Help_Topics
&Scoped-define LABEL8 Help Topics
&Scoped-define PROC8 ~
RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U). ~
help-page = INTEGER(RETURN-VALUE). ~
RUN Get_Procedure IN Persistent-Handle ('help.',OUTPUT run-proc,no). ~
IF run-proc NE '' THEN ~
RUN VALUE(run-proc) (b-prgrms.prgmname,help-page).
&Scoped-define ITEM9 Help_Index
&Scoped-define LABEL9 Help Index
&Scoped-define PROC9 ~
RUN Get_Procedure IN Persistent-Handle ('help.',OUTPUT run-proc,no). ~
IF run-proc NE '' THEN ~
RUN VALUE(run-proc) ('index.',0).
&Scoped-define ITEM10 About
&Scoped-define LABEL10 About
&Scoped-define PROC10 ~
RUN Get_Procedure IN Persistent-Handle ('about.',OUTPUT run-proc,yes).
&Scoped-define ITEM11 Util_frac
&Scoped-define LABEL11 Util_frac
&Scoped-define PROC11 RUN Select_frac.
DEFINE VARIABLE hCallAudit AS HANDLE NO-UNDO.
&Scoped-define ITEM12 Audit_Hist
&Scoped-define LABEL12 Audit History
&Scoped-define PROC12 ~
RUN system/CallAudit.p PERSISTENT SET hCallAudit ('{&FIRST-EXTERNAL-TABLE}',hTable,'Window',PROGRAM-NAME(1)).
&Scoped-define ITEM13 SysCtrlUsage
&Scoped-define LABEL13 Sys Ctrl Usage
&Scoped-define PROC13 ~
RUN Get_Procedure IN Persistent-Handle ('sysCtrlU.',OUTPUT run-proc,yes).

DEFINE SUB-MENU m_File
       {methods/menus/menuitem.i 1 m}
       {methods/menus/menuitem.i 2 m}
       {methods/menus/menuitem.i 3 m}
       {methods/menus/menuitem.i 4 m}
       {methods/menus/menuitem.i 5 m}
       {methods/menus/menuitem.i 6 m}
       {methods/menus/menuitem.i 11 m}
       RULE
       {methods/menus/menuitem.i 7 m}
       .

&IF INDEX("{&OPTIONS}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
&Scoped-define ITEMS yes
{methods/menus/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF

DEFINE SUB-MENU m_Help
       {methods/menus/menuitem.i 8 m}  
       {methods/menus/menuitem.i 9 m}
       RULE
       {methods/menus/menuitem.i 12 m}
       {methods/menus/menuitem.i 13 m}  
       RULE
       {methods/menus/menuitem.i 10 m}
       .

DEFINE SUB-MENU p_Help
       {methods/menus/menuitem.i 8 p} 
       {methods/menus/menuitem.i 9 p}
       RULE
       {methods/menus/menuitem.i 12 p} 
       {methods/menus/menuitem.i 13 p} 
       RULE
       {methods/menus/menuitem.i 10 p}
       .

DEFINE MENU POPUP-MENU-W-Win
       {methods/menus/menuitem.i 1 p}
       {methods/menus/menuitem.i 2 p}
       {methods/menus/menuitem.i 3 p}
       {methods/menus/menuitem.i 4 p}
       {methods/menus/menuitem.i 5 p}
       {methods/menus/menuitem.i 6 p}
       {methods/menus/menuitem.i 11 p}
       &IF INDEX("{&OPTIONS}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
       RULE
       SUB-MENU  p_Options      LABEL "Options"
       &ENDIF
       RULE
       SUB-MENU  p_Help         LABEL "Help"
       RULE
       {methods/menus/menuitem.i 7 p}
       .

DEFINE MENU MENU-BAR-W-Win MENUBAR
       SUB-MENU  m_File         LABEL "File"
       &IF INDEX("{&OPTIONS}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
       SUB-MENU  m_Options      LABEL "Options"
       &ENDIF
       SUB-MENU  m_Help         LABEL "Help".

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-W-Win:HANDLE
       {&WINDOW-NAME}:POPUP-MENU = MENU POPUP-MENU-W-Win:HANDLE.

{methods/menus/menutrig.i 1}
{methods/menus/menutrig.i 2}
{methods/menus/menutrig.i 3}
{methods/menus/menutrig.i 4}
{methods/menus/menutrig.i 5}
{methods/menus/menutrig.i 6}
{methods/menus/menutrig.i 7}
{methods/menus/menutrig.i 8}
{methods/menus/menutrig.i 9}
{methods/menus/menutrig.i 10}
{methods/menus/menutrig.i 11}
{methods/menus/menutrig.i 12}
{methods/menus/menutrig.i 13}

&IF INDEX("{&OPTIONS}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
&Scoped-define ITEMS no
{methods/menus/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF
&ENDIF
