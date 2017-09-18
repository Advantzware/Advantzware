/* loadtag/disptagr3.i  get tag# and display tag information for RM item */

IF "{1}" = "RMItem" THEN DO:
  FIND FIRST loadtag NO-LOCK WHERE loadtag.company EQ g_company
                               AND loadtag.item-type EQ YES
                               AND loadtag.tag-no EQ {2} NO-ERROR.
  IF NOT AVAIL loadtag THEN DO:
    MESSAGE "Invalid Loadtag#." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.
  /* blank loadtag record should not exist, if it does, remove it */
  IF CAN-FIND (FIRST bLoadTag WHERE bLoadTag.company EQ "") THEN DO:
      FIND FIRST bLoadTag EXCLUSIVE-LOCK 
           WHERE bLoadTag.company EQ ""
           NO-ERROR.
      IF AVAILABLE bLoadTag THEN 
      DELETE bLoadTag.
  END. /* if blank loadtag */
  &IF DEFINED(checkNewRecord) &THEN
  IF adm-new-record THEN DO:
  &ENDIF
    ASSIGN
      rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(loadtag.po-no)
      rm-rctd.job-no:SCREEN-VALUE = loadtag.job-no
      rm-rctd.job-no2:SCREEN-VALUE = STRING(loadtag.job-no2)
      rm-rctd.i-no:SCREEN-VALUE = loadtag.i-no
      rm-rctd.i-name:SCREEN-VALUE = loadtag.i-name
      rm-rctd.qty:SCREEN-VALUE = STRING(ip-rec-qty,"->,>>>,>>9.9<<")
      rm-rctd.loc:SCREEN-VALUE = loadtag.loc
      rm-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin.
    FIND FIRST item NO-LOCK
        WHERE item.company EQ loadtag.company
          AND item.i-no    EQ loadtag.i-no NO-ERROR.
    IF AVAIL item THEN rm-rctd.pur-uom:SCREEN-VALUE = item.cons-uom.
  &IF DEFINED(checkNewRecord) &THEN
  END. /* if adm-new-record */
  &ENDIF
END.
