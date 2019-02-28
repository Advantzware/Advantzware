/* util/updfgbin.p  update fg-bin's location from loadtag */
{custom/globdefs.i}
DISABLE TRIGGERS FOR LOAD OF loadtag.
OUTPUT TO c:\tmp\fgbin.txt.
PUT "Location Error" SKIP.

FOR EACH loadtag WHERE loadtag.company = g_company
                   AND loadtag.item-type = NO BY loadtag.tag-no.
    /*FIND FIRST*/
    FOR EACH fg-bin NO-LOCK WHERE fg-bin.company = loadtag.company
                                AND fg-bin.job-no = loadtag.job-no
                                AND fg-bin.job-no2 = loadtag.job-no2
                                AND fg-bin.i-no = loadtag.i-no
                                AND fg-bin.tag = loadtag.tag-no                                
                                AND fg-bin.qty > 0.
    /*IF AVAIL fg-bin THEN DO:*/
    /*   IF fg-bin.loc <> loadtag.loc THEN loadtag.loc = fg-bin.loc.
       IF fg-bin.loc-bin <> loadtag.loc-bin THEN loadtag.loc-bin = fg-bin.loc-bin.
    */
        IF fg-bin.loc <> loadtag.loc OR fg-bin.loc-bin <> loadtag.loc-bin THEN 
           DISPLAY loadtag.tag-no loadtag.loc LABEL "Tag_Loc"
                   loadtag.loc-bin LABEL "Tab_Bin"
                   fg-bin.loc LABEL "FG_Loc"
                   fg-bin.loc-bin LABEL "FG_Bin"
                   WITH STREAM-IO DOWN.
    END.

                                 
END.

