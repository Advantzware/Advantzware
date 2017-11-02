/* mainmenu.i */

  /* create dynamic menu bar start */

  DISPLAY
    SKIP(1)
    " Initializing Menus, One Moment Please ... "
    SKIP(1)
      WITH FRAME f-message COLUMN 16 ROW 5.

  CREATE MENU popup-ptr
    ASSIGN POPUP-ONLY = TRUE.
  CREATE wk-ptrs.
  ASSIGN
    wk-ptrs.menu-name = "popup"
    wk-ptrs.smenu-ptr = popup-ptr.

  FIND users WHERE users.user_id = USERID(ldbname(1)) NO-LOCK.
  j = IF users.developer THEN 2 ELSE 1.

  ASSIGN
   m_est-only    = SEARCH("menuest.r") NE ?
   m_menu-lst[1] = "menu"
   m_menu-lst[2] = "lst".

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ g_company
        AND sys-ctrl.name    EQ "cemenu"
      NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN
    IF sys-ctrl.char-fld EQ "Corrware" THEN m_menu-lst[2] = "cor".
    ELSE
    IF sys-ctrl.char-fld EQ "Foldware" THEN m_menu-lst[2] = "fol".

  m_menu-lst[1] = TRIM(m_menu-lst[1]) + "." + TRIM(m_menu-lst[2]).


  DO i = 1 TO j:
    IF i = 1 THEN
      IF SEARCH("usermenu/" + USERID(ldbname(1)) + "/" + m_menu-lst[1]) NE ? THEN
      INPUT FROM VALUE(search("usermenu/" + USERID(ldbname(1)) + "/" + m_menu-lst[1])) NO-ECHO.
      ELSE
      INPUT FROM VALUE(search(m_menu-lst[1])) NO-ECHO.
    ELSE
    INPUT FROM VALUE(SEARCH ("popup.lst")) NO-ECHO.
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
      IF SEARCH("usermenu/" + USERID(ldbname(1)) + "/" + m_menu-lst[1]) NE ? THEN
      INPUT FROM VALUE(search("usermenu/" + USERID(ldbname(1)) + "/" + m_menu-lst[1])) NO-ECHO.
      ELSE
      INPUT FROM VALUE(search(m_menu-lst[1])) NO-ECHO.
    ELSE
    INPUT FROM VALUE(SEARCH ("popup.lst")) NO-ECHO.
    REPEAT:
      ASSIGN
       m_item1 = ""
       m_item2 = ""
       m_item3 = "".
      IMPORT m_item1 m_item2 m_item3.
      IF m_item1 EQ m_item2                OR
         (m_est-only AND m_item3 NE "est") THEN NEXT.
      FIND FIRST wk-ptrs WHERE wk-ptrs.menu-name = m_item2 NO-LOCK NO-ERROR.
      IF NOT AVAIL wk-ptrs THEN DO:
          IF USERID("ldbname(1)") EQ "ASI" THEN
            MESSAGE "Missing menu item: " m_item2 SKIP
            "menu file:" m_menu-lst[1] SKIP
            VIEW-AS ALERT-BOX.          
          NEXT.
      END.
    
      IF CAN-DO("RULE,SKIP",m_item1) THEN
      DO:
        CREATE MENU-ITEM menu-item-ptr
          ASSIGN PARENT = wk-ptrs.smenu-ptr
                 SUBTYPE = m_item1
                 PRIVATE-DATA = m_item1.
        NEXT.
      END.
      FIND prgrms WHERE prgrms.prgmname = m_item1 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE prgrms THEN
      NEXT.
      IF INDEX(m_item1,".") NE 0 THEN
      DO:
        CREATE MENU-ITEM menu-item-ptr
          ASSIGN PARENT = wk-ptrs.smenu-ptr
                 LABEL = prgrms.prgtitle
                 PRIVATE-DATA = m_item1
          TRIGGERS:
            ON CHOOSE
              DO:
                RUN Get_Procedure IN Persistent-Handle (SELF:PRIVATE-DATA,OUTPUT run-proc,yes).
                IF init_menu THEN
                APPLY "CLOSE":U TO THIS-PROCEDURE.
              END.
          END TRIGGERS.
      END.
      ELSE
      DO:
        CREATE SUB-MENU sub-menu-ptr[1]
          ASSIGN PARENT = wk-ptrs.smenu-ptr
                 LABEL = prgrms.prgtitle
                 PRIVATE-DATA = m_item1.
        FIND FIRST wk-ptrs WHERE wk-ptrs.menu-name = m_item1.
        wk-ptrs.smenu-ptr = sub-menu-ptr[1].
      END.
    END.
    INPUT CLOSE.
  END.

  CREATE MENU-ITEM exit-item-ptr
    ASSIGN PARENT = menu-bar-ptr:FIRST-CHILD
           LABEL = "E&xit"
    TRIGGERS:
    ON CHOOSE
      DO:
        /*HIDE FRAME frame-user NO-PAUSE.*/
        APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
      END.
    END TRIGGERS.

  ASSIGN
    CURRENT-WINDOW:MENUBAR = menu-bar-ptr
    CURRENT-WINDOW:POPUP-MENU = popup-ptr.

  HIDE FRAME f-message NO-PAUSE.

  /* create dynamic menu bar end */

  RUN enable_UI.
  {methods/enhance.i}
  users_user_id = USERID(ldbname(1)) /*+ " - " + users.user_name */ .
  DISPLAY users_user_id
      WITH FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
  /*
  /* Users.image_filename repurposed to email address, so this never executes */
  v_image_filename = users.image_filename.
  IF v_image_filename = "" THEN
  DO:
    FIND FIRST config NO-LOCK NO-ERROR.
    IF NOT AVAILABLE config THEN
    DO TRANSACTION:
      CREATE config.
      FIND FIRST config NO-LOCK NO-ERROR.
    END.
    v_image_filename = config.image_filename.
  END.
  IF v_image_filename NE "" THEN
  ldummy = menu-image:LOAD-IMAGE(v_image_filename) NO-ERROR.
  */
  VIEW FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
  {methods/nowait.i}
