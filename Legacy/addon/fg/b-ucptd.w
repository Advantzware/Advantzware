      fg-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(13)":U WIDTH 7
               MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
          


       IF NOT AVAIL fg-bin THEN DO: 
               MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
  END.
  IF NOT AVAIL fg-bin THEN DO: 
      MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
