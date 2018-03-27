DEF VAR st-time AS INT NO-UNDO.
DEF VAR end-time AS INT NO-UNDO.
DEF VAR lChoice AS LOG NO-UNDO.
DEF VAR i AS INT NO-UNDO.
MESSAGE "Create Job Farm Records?" UPDATE lChoice
    view-as alert-box button yes-no .
IF NOT lChoice THEN
    RETURN.

st-time = TIME.
FOR EACH job WHERE opened EQ YES NO-LOCK,
    EACH job-hdr OF job NO-LOCK,
    FIRST itemfg WHERE itemfg.company EQ job-hdr.company
      AND itemfg.i-no EQ job-hdr.i-no 
      AND itemfg.pur-man EQ YES NO-LOCK.
    i = i + 1.
    RUN jc/addJobFarm.p (INPUT job.job).              

END.
end-time = TIME.
MESSAGE "Done! Time:" end-time - st-time "Records: " i
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
