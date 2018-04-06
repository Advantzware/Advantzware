
  SESSION:SET-WAIT-STATE ("general").

  DO TRANSACTION ON ERROR UNDO, RETURN.

      CREATE prgrms.

      ASSIGN prgrms.prgmname  = "p-pennote."
             prgrms.prgtitle  = "Pen Notes View Tab"
             prgrms.dir_group = "panels"
             prgrms.prgm_ver  = "10.10"
             prgrms.menu_item = NO
             prgrms.popup     = NO
             prgrms.run_persistent = YES
             prgrms.track_usage    = NO
             prgrms.can_run        = "*"
             prgrms.can_create     = "*"
             prgrms.can_update     = "*"
             prgrms.can_delete     = "*"
             prgrms.mfgroup        = "specnote.".

  END.

  SESSION:SET-WAIT-STATE ("").

  IF NOT ERROR-STATUS:ERROR THEN
      MESSAGE prgrms.prgmname " was added."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

