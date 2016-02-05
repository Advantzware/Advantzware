      
DISABLE TRIGGERS FOR LOAD OF loadtag.

 FOR EACH LOADTAG WHERE item-type = NO AND
                is-case-tag = NO.
    FIND fg-bin WHERE fg-bin.company = loadtag.company
           AND fg-bin.i-no = loadtag.i-no
        AND fg-bin.job-no = loadtag.job-no 
        AND fg-bin.tag = loadtag.tag-no NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (fg-bin.loc <> loadtag.loc  OR fg-bin.loc-bin <> loadtag.loc-bin)
    THEN  
    /*  ASSIGN loadtag.loc = fg-bin.loc
             loadtag.loc-bin = fg-bin.loc-bin.*/
        DISP loadtag.loc loadtag.loc-bin loadtag.tag-no.

END.
