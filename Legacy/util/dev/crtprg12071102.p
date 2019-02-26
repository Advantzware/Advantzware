/* 12/19/11  S.Brooks 
   Task# 12071102 - Posting Count FG to work same as Receive FG's
   Created new business logic program to process physical count posting.
   This utility program adds the program master record so the new program
   will run.
 */
 
  SESSION:SET-WAIT-STATE ("general").

  DO TRANSACTION ON ERROR UNDO, RETURN.

      CREATE prgrms.

      ASSIGN prgrms.prgmname  = "phyctpst."
             prgrms.prgtitle  = "Physical Count Posting"
             prgrms.dir_group = "fg"
             prgrms.prgm_ver  = "10.10"
             prgrms.menu_item = YES
             prgrms.popup     = NO
             prgrms.run_persistent = NO
             prgrms.track_usage    = NO
             prgrms.can_run        = "*"
             prgrms.can_create     = "*"
             prgrms.can_update     = "*"
             prgrms.can_delete     = "*"
             prgrms.mfgroup        = "".

  END.

  SESSION:SET-WAIT-STATE ("").

  IF NOT ERROR-STATUS:ERROR THEN
      MESSAGE prgrms.prgmname " was added."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

