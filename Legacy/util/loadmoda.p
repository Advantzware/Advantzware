/* util/loadmoda.p    For Addon, build modules for liscense */
{custom/globdefs.i}
SESSION:SET-WAIT-STATE("general").

DEFINE TEMP-TABLE ttbl NO-UNDO
  FIELD menu-order AS INTEGER
  FIELD menu1 AS CHARACTER
  FIELD menu2 AS CHARACTER
    INDEX ttbl IS PRIMARY UNIQUE menu-order
    INDEX menu2 menu2 menu-order.

DEFINE TEMP-TABLE ttbl-menu NO-UNDO
  FIELD menu-name AS CHARACTER
  FIELD menu-count AS INTEGER
    INDEX ttbl-menu IS PRIMARY UNIQUE menu-name
    INDEX menu-count menu-count.

  DEF VAR m AS CHAR EXTENT 3 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR ls-menu-lst AS cha EXTENT 2 NO-UNDO.
  DEF VAR ll-est-only AS LOG NO-UNDO.
  
  
  FOR EACH ttbl EXCLUSIVE-LOCK:
    DELETE ttbl.
  END.
  FOR EACH ttbl-menu EXCLUSIVE-LOCK:
    DELETE ttbl-menu.
  END.

  /* ============= dynamic menu for foldware/corrware ============*/
  ASSIGN
   ll-est-only    = SEARCH("addon/menuest.r") NE ?
   ls-menu-lst[1] = "addon/menu"
   ls-menu-lst[2] = "lst".

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ g_company
        AND sys-ctrl.name    EQ "cemenu"
      NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN
    IF sys-ctrl.char-fld EQ "Corrware" THEN ls-menu-lst[2] = "cor".
    ELSE
    IF sys-ctrl.char-fld EQ "Foldware" THEN ls-menu-lst[2] = "fol".

  ls-menu-lst[1] = TRIM(ls-menu-lst[1]) + "." + TRIM(ls-menu-lst[2]).
  /*
  /* ========== end of mods =========================*/
  IF SEARCH("usermenu\" + USERID("nosweat") + "\" + ls-menu-lst[1]) <> ? THEN
      ls-menu-lst[1] = "usermenu\" + USERID("nosweat") + "\" + ls-menu-lst[1].
  */
  INPUT FROM VALUE(ls-menu-lst[1]) NO-ECHO.
  REPEAT:
    m = "".
    IMPORT m[1] m[2] m[3].
    IF CAN-DO('RULE,SKIP',m[1]) OR
       (ll-est-only AND m[3] NE "est") THEN NEXT.
    CREATE ttbl.
    ASSIGN
      i = i + 1
      ttbl.menu-order = i
      ttbl.menu1 = m[1]
      ttbl.menu2 = m[2].
    FIND ttbl-menu WHERE ttbl-menu.menu-name = m[2] EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ttbl-menu THEN
    DO:
      CREATE ttbl-menu.
      ttbl-menu.menu-name = m[2].
      IF m[2] = 'file' THEN
      ttbl-menu.menu-count = 1.      
    END.
    ttbl-menu.menu-count = ttbl-menu.menu-count + 1.
    /* build modules for liscense */
    FIND FIRST asi.module WHERE asi.module.module = ttbl.menu1 NO-LOCK NO-ERROR.
    IF NOT AVAIL asi.module THEN DO:
       FIND prgrms WHERE prgrms.prgmname = ttbl.menu1 NO-LOCK NO-ERROR.
       CREATE asi.module.
       ASSIGN asi.module.db-name = "ADDON"
              asi.module.module = ttbl.menu1
              asi.module.dscr = IF AVAIL prgrms THEN prgrms.prgtitle ELSE ""
              asi.module.is-used = YES.
    END.
  END.
  INPUT CLOSE.
  SESSION:SET-WAIT-STATE("").
  MESSAGE "Module loading procedure is completed." VIEW-AS ALERT-BOX.


