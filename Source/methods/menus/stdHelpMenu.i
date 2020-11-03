/* stdHelpMenu.i - rstark - 10.16.2020 */

/* Menu Definitions */
DEFINE SUB-MENU m_File 
       {&mFileItems}
       MENU-ITEM m_Exit           LABEL "E&xit"
       .   
{&mOptionsItems}
DEFINE SUB-MENU m_Help 
       MENU-ITEM m_Help_Topics    LABEL "Help &Topics"   
       RULE
       &IF DEFINED(ExcludeAuditHistory) EQ 0 &THEN
       MENU-ITEM m_Audit_History  LABEL "Audit &History"
       &ENDIF
       MENU-ITEM m_Sys_Ctrl_Usage LABEL "&Sys Ctrl Usage"
       RULE
       MENU-ITEM m_About          LABEL "&About"
       .
DEFINE MENU MENU-BAR-{&WINDOW-NAME} MENUBAR
       SUB-MENU  m_File           LABEL "&File"
       &IF DEFINED(mOptionsItems) NE 0 &THEN
       SUB-MENU  m_options        LABEL "&Options"
       &ENDIF
       SUB-MENU  m_Help           LABEL "&Help"
       .
ON CHOOSE OF MENU-ITEM m_Exit
DO:
    {methods/template/exit.i}
END.

ON CHOOSE OF MENU-ITEM m_Help_Topics
DO:
    RUN get-attribute IN THIS-PROCEDURE ("Current-Page":U).
    help-page = INTEGER(RETURN-VALUE).
    RUN Get_Procedure IN Persistent-Handle ("help.",OUTPUT run-proc,NO).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (b-prgrms.prgmname,help-page).
END.

&IF DEFINED(ExcludeAuditHistory) EQ 0 &THEN
ON CHOOSE OF MENU-ITEM m_Audit_History
DO:
    RUN pCallAudit.
END.
&ENDIF

ON CHOOSE OF MENU-ITEM m_Sys_Ctrl_Usage
DO:
    RUN Get_Procedure IN Persistent-Handle ("sysCtrlU.",OUTPUT run-proc,YES).
END.

ON CHOOSE OF MENU-ITEM m_About
DO:
    RUN Get_Procedure IN Persistent-Handle ("about.",OUTPUT run-proc,YES).
END.

{&mOptionsTriggers}

ASSIGN {&WINDOW-NAME}:MENUBAR = MENU MENU-BAR-{&WINDOW-NAME}:HANDLE.
