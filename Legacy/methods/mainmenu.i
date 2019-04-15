/* mainmenu.i */

/* create dynamic menu bar start */

DISPLAY
    SKIP(1)
    " Initializing Menus, One Moment Please ... "
    SKIP(1)
    WITH FRAME f-message COLUMN 16 ROW 5.

CREATE MENU popup-ptr
    ASSIGN 
    POPUP-ONLY = TRUE.
CREATE wk-ptrs.
ASSIGN
    wk-ptrs.menu-name = "popup"
    wk-ptrs.smenu-ptr = popup-ptr.

FIND users NO-LOCK WHERE users.user_id EQ USERID(LDBNAME(1)).
j = IF users.developer OR USER(LDBNAME(1)) EQ "ASI" THEN 2 ELSE 1.

ASSIGN
    m_est-only    = SEARCH("menuest.r") NE ?
    m_menu-lst[1] = "{&addon}menu"
    m_menu-lst[2] = "lst".

RUN sys/ref/nk1look.p (
    g_company,"CEMenu","C",NO,NO,"","",
    OUTPUT cCEMenu,OUTPUT lFound
    ).
IF lFound THEN
    IF cCEMenu EQ "CorrWare" THEN m_menu-lst[2] = "cor".
    ELSE
        IF cCEMenu EQ "Foldware" THEN m_menu-lst[2] = "fol".

m_menu-lst[1] = TRIM(m_menu-lst[1]) + "." + TRIM(m_menu-lst[2]).

DO i = 1 TO j:
    IF i = 1 THEN
        &IF DEFINED(addon) EQ 0 &THEN
        IF SEARCH("usermenu/" + USERID(LDBNAME(1)) + "/" + m_menu-lst[1]) NE ? THEN
            INPUT FROM VALUE(SEARCH("usermenu/" + USERID(LDBNAME(1)) + "/" + m_menu-lst[1])) NO-ECHO.
        ELSE
        &ENDIF
            INPUT FROM VALUE(SEARCH(m_menu-lst[1])) NO-ECHO.
    ELSE
        INPUT FROM VALUE(SEARCH ("{&addon}popup.lst")) NO-ECHO.
    REPEAT:
        ASSIGN
            m_item1 = ""
            m_item2 = ""
            m_item3 = "".
        IMPORT m_item1 m_item2 m_item3.
        IF CAN-DO("RULE,SKIP",m_item1)       OR
         INDEX(m_item1,".") NE 0           OR
            (m_est-only AND m_item3 NE "est") THEN NEXT.
        CREATE wk-ptrs.
        wk-ptrs.menu-name = m_item1.
        IF m_item1 NE m_item2 THEN
            NEXT.
        CREATE MENU menu-bar-ptr.
        wk-ptrs.smenu-ptr = menu-bar-ptr.
    END.
END.

DO i = 1 TO j:
    IF i = 1 THEN
        &IF DEFINED(addon) EQ 0 &THEN
        IF SEARCH("usermenu/" + USERID(LDBNAME(1)) + "/" + m_menu-lst[1]) NE ? THEN
            INPUT FROM VALUE(SEARCH("usermenu/" + USERID(LDBNAME(1)) + "/" + m_menu-lst[1])) NO-ECHO.
        ELSE
        &ENDIF
            INPUT FROM VALUE(SEARCH(m_menu-lst[1])) NO-ECHO.
    ELSE
        INPUT FROM VALUE(SEARCH ("{&addon}popup.lst")) NO-ECHO.
    REPEAT:
        ASSIGN
            m_item1 = ""
            m_item2 = ""
            m_item3 = "".
        IMPORT m_item1 m_item2 m_item3.
        IF m_item1 EQ m_item2 OR
            (m_est-only AND m_item3 NE "est") THEN NEXT.
        FIND FIRST wk-ptrs WHERE wk-ptrs.menu-name EQ m_item2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE wk-ptrs THEN DO:
            IF USERID("ldbname(1)") EQ "ASI" THEN
                MESSAGE "Missing menu item: " m_item2 SKIP
                    "menu file:" m_menu-lst[1] SKIP
                    VIEW-AS ALERT-BOX.          
            NEXT.
        END.
    
        IF CAN-DO("RULE,SKIP",m_item1) THEN DO:
            CREATE MENU-ITEM menu-item-ptr
                ASSIGN 
                    PARENT = wk-ptrs.smenu-ptr
                    NAME = m_item1
                    SUBTYPE = m_item1
                    PRIVATE-DATA = m_item1
                    .
            NEXT.
        END.
        FIND prgrms WHERE prgrms.prgmname = m_item1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE prgrms THEN
            NEXT.
        IF INDEX(m_item1,".") NE 0 THEN DO:
            CREATE MENU-ITEM menu-item-ptr
                ASSIGN 
                    PARENT = wk-ptrs.smenu-ptr
                    NAME = m_item1
                    LABEL = prgrms.prgtitle
                    PRIVATE-DATA = prgrms.prgtitle
                    TRIGGERS:
                        ON CHOOSE
                            DO:
                                RUN Get_Procedure IN Persistent-Handle (SELF:NAME,OUTPUT run-proc,YES).
                                IF init_menu THEN
                                    APPLY "CLOSE":U TO THIS-PROCEDURE.
                            END.
                    END TRIGGERS.
            CREATE ttblMenuBar.
            ASSIGN
                ttblMenuBar.menuBarName = m_item1
                ttblMenuBar.menuBarPtr  = menu-item-ptr
                .
        END.
        ELSE DO:
            CREATE SUB-MENU sub-menu-ptr[1]
                ASSIGN 
                    PARENT = wk-ptrs.smenu-ptr
                    NAME = m_item1
                    LABEL = prgrms.prgtitle
                    PRIVATE-DATA = prgrms.prgtitle
                    .
            FIND FIRST wk-ptrs WHERE wk-ptrs.menu-name = m_item1.
            wk-ptrs.smenu-ptr = sub-menu-ptr[1].
        END.
    END.
    INPUT CLOSE.
END.

CREATE MENU-ITEM exit-item-ptr
    ASSIGN 
        PARENT = menu-bar-ptr:FIRST-CHILD
        NAME = "Exit"
        LABEL = "E&xit"
        PRIVATE-DATA = "Exit"
        TRIGGERS:
            ON CHOOSE
                DO:
                    APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
                END.
        END TRIGGERS.
CREATE ttblMenuBar.
ASSIGN
    ttblMenuBar.menuBarName = exit-item-ptr:NAME
    ttblMenuBar.menuBarPtr  = exit-item-ptr
    CURRENT-WINDOW:MENUBAR    = menu-bar-ptr
    CURRENT-WINDOW:POPUP-MENU = popup-ptr
    .
HIDE FRAME f-message NO-PAUSE.

/* create dynamic menu bar end */

RUN enable_UI.
{methods/enhance.i}
users_user_id = USERID(LDBNAME(1)).
DISPLAY users_user_id
    WITH FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
VIEW FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
{methods/nowait.i}
