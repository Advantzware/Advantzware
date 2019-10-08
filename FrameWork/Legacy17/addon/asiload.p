/* asiload.p  load prgrms and lookups data */
DEF VAR ll-ans AS LOG NO-UNDO.
DEF VAR i AS INT NO-UNDO.

def temp-table tt-prg like prgrms.
def temp-table tt-ref like prgmxref.
DEF TEMP-TABLE tt-emailcod LIKE emailcod.
DEF TEMP-TABLE tt-reftable LIKE reftable.
DEF TEMP-TABLE tt-notes LIKE notes.

DISABLE TRIGGERS FOR LOAD OF reftable.

ll-ans = YES.

IF SEARCH (".\prgrms.d") <> ? AND
   (userid(ldbname(1)) = "ASI" OR userid(ldbname(1)) = "Nosweat") THEN
   DO:

   IF ll-ans THEN DO:
      SESSION:SET-WAIT-STATE("general").
      OUTPUT TO prgmxref.sav.
      FOR EACH prgmxref:
          EXPORT prgmxref.
      END.
      OUTPUT TO prgrms.sav.
      FOR EACH prgrms:
          EXPORT prgrms.
      END.
      OUTPUT CLOSE.
      /* update  prgrms */
      
      INPUT FROM prgrms.d NO-ECHO.
      
      REPEAT :
         create tt-prg.
         IMPORT tt-prg.prgmname
             tt-prg.prgtitle
             tt-prg.run_persistent
             tt-prg.can_run
             tt-prg.can_create
             tt-prg.can_update
             tt-prg.can_delete
             tt-prg.dir_group
             tt-prg.use_colors
             tt-prg.use_fonts
             tt-prg.widget_bgc
             tt-prg.widget_fgc
             tt-prg.widget_font
             tt-prg.track_usage
             tt-prg.popup
             tt-prg.prgm_ver
             tt-prg.menu_item
             tt-prg.mfgroup 
             tt-prg.rec_key.

         FIND FIRST prgrms WHERE prgrms.prgmname = tt-prg.prgmname NO-ERROR.
         IF NOT AVAIL prgrms THEN DO:
            CREATE prgrms.
            BUFFER-COPY tt-prg TO prgrms
              ASSIGN prgrms.can_run = '*'
                     prgrms.can_create = '*'
                     prgrms.can_update = '*'
                     prgrms.can_delete = '*'.
         END.
         ELSE DO:
            ASSIGN prgrms.prgtitle  = tt-prg.prgtitle
                prgrms.run_persistent = tt-prg.RUN_persistent
                prgrms.dir_group = tt-prg.DIR_group
                prgrms.use_colors = tt-prg.USE_colors
                prgrms.use_fonts = tt-prg.USE_fonts
                prgrms.track_usage = tt-prg.track_usage
                prgrms.popup = tt-prg.popup
                prgrms.prgm_ver = tt-prg.prgm_ver
                prgrms.menu_item = tt-prg.MENU_item
                prgrms.mfgroup = tt-prg.mfgroup.

             DO i = 1 TO 13:
                ASSIGN prgrms.widget_bgc[i] = tt-prg.WIDGET_bgc[i]
                      prgrms.widget_fgc[i] = tt-prg.WIDGET_fgc[i]
                      prgrms.widget_font[i] = tt-prg.WIDGET_font[i]
                      .
             END.
         END.
      END. /*end REPEAT*/

      /* delete records no longer used */
      
      DISABLE TRIGGERS FOR LOAD OF prgrms.
      FOR EACH prgrms WHERE NOT CAN-FIND(FIRST tt-prg WHERE tt-prg.prgmname = prgrms.prgmname ):

         FOR EACH prgmxref WHERE
              prgmxref.prgmname = prgrms.prgmname
              EXCLUSIVE-LOCK:

              DELETE prgmxref.
         END.

         DELETE prgrms.
      END.

      /* update prgmxref */
      INPUT FROM prgmxref.d NO-ECHO.
      REPEAT:
          CREATE tt-ref.
          SET tt-ref.table_name
              tt-ref.prgmname
              tt-ref.pageno.

          FIND FIRST prgmxref WHERE prgmxref.TABLE_name = tt-ref.TABLE_name NO-ERROR.
          IF NOT AVAIL prgmxref THEN DO:
             CREATE prgmxref.
             BUFFER-COPY tt-ref TO prgmxref.
          END.
          ELSE DO:
             ASSIGN prgmxref.prgmname = tt-ref.prgmname
                    prgmxref.pageno = tt-ref.pageno
                    .
          END.
      END.
      OS-COPY prgrms.d prgrms.OLD.
      OS-COPY prgmxref.d prgmxref.OLD.
      OS-DELETE prgrms.d.
      OS-DELETE prgmxref.d.
   END. /* SEARCH prgrms.d*/
END.

IF SEARCH (".\lookups.d") <> ? AND
   (userid(ldbname(1)) = "ASI" OR userid(ldbname(1)) = "Nosweat")
THEN DO:
  
     DEF VAR v1 AS cha FORM "x(15)" NO-UNDO.
     DEF VAR v2 AS cha FORM "x(15)" NO-UNDO.
     DEF VAR v3 LIKE lookups.FRAME_field NO-UNDO.
     DEF VAR v4 LIKE lookups.prgmname NO-UNDO.
     DEF VAR v5 LIKE lookups.rec_key NO-UNDO.

  IF NOT ll-ans THEN    MESSAGE "Load Lookup Data?" view-as alert-box warning
         BUTTON YES-NO UPDATE ll-ans .
   IF ll-ans THEN DO:
      SESSION:SET-WAIT-STATE("general").
      OUTPUT TO lookups.sav.
      FOR EACH lookups:
          EXPORT lookups.
          DELETE lookups.
      END.
      OUTPUT CLOSE.
      INPUT FROM lookups.d NO-ECHO.
      REPEAT:
          SET v1 v2 v3 v4 v5.
          CREATE lookups.
          assign lookups.FRAME_db = v1
                 lookups.FRAME_file = v2
                 lookups.FRAME_field = v3
                 lookups.prgmname = v4
                 lookups.rec_key = v5.
      END.
      INPUT CLOSE.
      
      OS-COPY lookups.d lookups.OLD.
      OS-DELETE lookups.d. 
   END.
END.

IF SEARCH(".\emailcod.d") NE ? AND CAN-DO('ASI,NoSweat',USERID('NoSweat')) THEN DO:
  CREATE tt-emailcod.
  IF NOT ll-ans THEN
  MESSAGE "Load Email Codes Data?" VIEW-AS ALERT-BOX WARNING
         BUTTON YES-NO UPDATE ll-ans.
  IF ll-ans THEN DO:
    SESSION:SET-WAIT-STATE("general").
    OUTPUT TO emailcod.sav.
    FOR EACH emailcod NO-LOCK:
      EXPORT emailcod.
    END.
    OUTPUT CLOSE.
    INPUT FROM emailcod.d NO-ECHO.
    REPEAT:
      IMPORT tt-emailcod.
      IF CAN-FIND(emailcod WHERE emailcod.emailcod EQ tt-emailcod.emailcod) THEN
      NEXT.
      CREATE emailcod.
      BUFFER-COPY tt-emailcod EXCEPT rec_key TO emailcod.
    END.
    INPUT CLOSE.
    OS-COPY emailcod.d emailcod.old.
    OS-DELETE emailcod.d. 
  END.
END.

/* reftable: utility notes */
IF SEARCH(".\notes.d") NE ? AND CAN-DO('ASI,NoSweat',USERID('NoSweat')) THEN DO:
  CREATE tt-notes.
  IF NOT ll-ans THEN
  MESSAGE "Load Utility Code Notes Data?" VIEW-AS ALERT-BOX WARNING
         BUTTON YES-NO UPDATE ll-ans.
  IF ll-ans THEN DO:
    SESSION:SET-WAIT-STATE("general").
    OUTPUT TO notes.sav.
    FOR EACH reftable NO-LOCK WHERE reftable.reftable EQ 'Utilities':
      FOR EACH notes NO-LOCK WHERE notes.rec_key EQ reftable.rec_key:
        EXPORT notes.
        DELETE notes.
      END. /* each notes */
    END. /* each reftable */
    OUTPUT CLOSE.
    INPUT FROM notes.d NO-ECHO.
    REPEAT:
      IMPORT tt-notes.
      FIND FIRST notes EXCLUSIVE-LOCK
           WHERE notes.rec_key EQ tt-notes.rec_key
             AND notes.note_date EQ tt-notes.note_date
             AND notes.note_time EQ tt-notes.note_time NO-ERROR.
      IF NOT AVAILABLE notes THEN
      CREATE notes.
      BUFFER-COPY tt-notes TO notes.
    END. /* repeat */
    INPUT CLOSE.
    OS-COPY notes.d notes.old.
    OS-DELETE notes.d. 
  END. /* if ll-ans */
END. /* if search */

IF SEARCH(".\reftable.d") NE ? AND CAN-DO('ASI,NoSweat',USERID('NoSweat')) THEN DO:
  CREATE tt-reftable.
  IF NOT ll-ans THEN
  MESSAGE "Load Utility Codes Data?" VIEW-AS ALERT-BOX WARNING
         BUTTON YES-NO UPDATE ll-ans.

  IF ll-ans THEN DO:
    SESSION:SET-WAIT-STATE("general").
    OUTPUT TO reftable.sav.
    FOR EACH reftable EXCLUSIVE-LOCK WHERE reftable.reftable EQ 'Utilities':
      EXPORT reftable.
      DELETE reftable.
    END. /* each reftable */
    OUTPUT CLOSE.
    INPUT FROM reftable.d NO-ECHO.
    REPEAT:
      IMPORT tt-reftable.
      FIND FIRST reftable EXCLUSIVE-LOCK
           WHERE reftable.reftable EQ tt-reftable.reftable
             AND reftable.company EQ tt-reftable.company
             AND reftable.loc EQ tt-reftable.loc NO-ERROR.
      IF NOT AVAILABLE reftable THEN
      CREATE reftable.
      BUFFER-COPY tt-reftable TO reftable.
    END. /* repeat */
    INPUT CLOSE.
    OS-COPY reftable.d reftable.old.
    OS-DELETE reftable.d. 
  END. /* if ll-ans */

END. /* if search */

SESSION:SET-WAIT-STATE("").
