/*util/updldtag.p  */

message "Are you sure you want to update Loadtag's warehouse and bin information from Inventory?"
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO update v-ans AS LOG.

IF v-ans THEN do:
  SESSION:SET-WAIT-STATE("general").
  for each loadtag WHERE item-type = NO AND
                         is-case-tag = NO:
    FIND FIRST fg-bin WHERE fg-bin.company = loadtag.company
                       AND fg-bin.job-no = loadtag.job-no
                       AND fg-bin.job-no2 = loadtag.job-no2
                       AND fg-bin.i-no = loadtag.i-no 
                       AND fg-bin.tag = loadtag.tag-no NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN DO:
      IF fg-bin.loc <> loadtag.loc THEN loadtag.loc = fg-bin.loc.
      IF fg-bin.loc-bin <> loadtag.loc-bin THEN loadtag.loc-bin = fg-bin.loc-bin.

    END.
  END.
  SESSION:SET-WAIT-STATE("").
  MESSAGE "Completed..." VIEW-AS ALERT-BOX.
end.
      
