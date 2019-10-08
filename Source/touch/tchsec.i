/* addon/touch/tchsec.i*/

IF NOT v-can-update AND tstimeb-log = NO THEN do:
   
    ASSIGN btn_hour:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           btn_minute:SENSITIVE = NO
           btn_ampm:SENSITIVE = NO
           btn_reset_time:SENSITIVE = NO
           btn_set_time:SENSITIVE = NO
           .
          
END.
