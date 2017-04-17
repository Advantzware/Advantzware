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
  DO i = 1 TO j:
    IF i = 1 THEN
      IF SEARCH("users/" + USERID(ldbname(1)) + "/menu.lst") NE ? THEN
      INPUT FROM VALUE("users/" + USERID(ldbname(1)) + "/menu.lst") NO-ECHO.
      ELSE
      INPUT FROM value(search("addon\menu.lst")) NO-ECHO.
    ELSE
    INPUT FROM value(search("addon\popup.lst")) NO-ECHO.
    REPEAT:                          
      IMPORT m_item1 m_item2.
      IF CAN-DO("RULE,SKIP",m_item1) OR INDEX(m_item1,".") NE 0 THEN
      NEXT.
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
      IF SEARCH("users/" + USERID(ldbname(1)) + "/menu.lst") NE ? THEN
      INPUT FROM VALUE("users/" + USERID(ldbname(1)) + "/menu.lst") NO-ECHO.
      ELSE
      INPUT FROM value(search("addon\menu.lst")) NO-ECHO.
    ELSE
    INPUT FROM value(search("addon\popup.lst")) NO-ECHO.
    REPEAT:
      IMPORT m_item1 m_item2.
      IF m_item1 = m_item2 THEN
      NEXT.
      FIND FIRST wk-ptrs WHERE wk-ptrs.menu-name = m_item2.
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
    
          /* This code is for TRIALS 
          if (m_item2 begins "touch" or m_item2 begins "cadcam" or
              m_item2 begins "sharpsht" or m_item2 begins "cont") and 
              today ge 1/1/2001 then
            ASSIGN menu-item-ptr:SENSITIVE = no.
          */
          
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

  CREATE MENU-ITEM capture1-item-ptr
    ASSIGN PARENT = menu-bar-ptr:FIRST-CHILD
           LABEL = "1 Capture Screen"
    TRIGGERS:
    ON CHOOSE
      DO:
        RUN Get_Procedure IN Persistent-Handle ('scrimage.',OUTPUT run-proc,no).
        IF run-proc NE '' THEN
        RUN VALUE(run-proc) ({&WINDOW-NAME}:HANDLE).
      END.
    END TRIGGERS.

  CREATE MENU-ITEM capture2-item-ptr
    ASSIGN PARENT = menu-bar-ptr:FIRST-CHILD
           LABEL = "2 Capture Screen Selection"
    TRIGGERS:
    ON CHOOSE
      DO:
        RUN Get_Procedure IN Persistent-Handle ('scrncap.',OUTPUT run-proc,yes).
      END.
    END TRIGGERS.

 CREATE MENU-ITEM menu-item-ptr
   ASSIGN PARENT = menu-bar-ptr:FIRST-CHILD
          SUBTYPE = "RULE".

  CREATE MENU-ITEM exit-item-ptr
    ASSIGN PARENT = menu-bar-ptr:FIRST-CHILD
           LABEL = "E&xit"
    TRIGGERS:
    ON CHOOSE
      DO:
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
  users_user_id = USERID(ldbname(1)) /*+ " - " + users.user_name*/ .
  DISPLAY users_user_id
      WITH FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
  v_image_filename = users.image_filename.
  IF v_image_filename = "" THEN
  DO:
    FIND FIRST config NO-LOCK NO-ERROR.
    IF NOT AVAILABLE config THEN
    DO TRANSACTION:
      CREATE config.
      FIND CURRENT config NO-LOCK.
    END.
    v_image_filename = config.image_filename.
  END.
  IF v_image_filename NE "" THEN
  ldummy = menu-image:LOAD-IMAGE(v_image_filename) NO-ERROR.
  VIEW FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
  {methods/nowait.i}
