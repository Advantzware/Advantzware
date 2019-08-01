
DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

DEFINE BUFFER b-fg-bin   FOR fg-bin. 
DEFINE BUFFER bf-loadtag FOR loadtag.    

FIND fg-bin WHERE ROWID(fg-bin) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAILABLE fg-bin THEN 
DO:
    FIND FIRST loadtag NO-LOCK
        WHERE loadtag.company      EQ fg-bin.company
        AND loadtag.item-type    EQ NO
        AND loadtag.tag-no       EQ fg-bin.tag
        AND loadtag.i-no         EQ fg-bin.i-no
        AND loadtag.is-case-tag  EQ NO                    /* loadtags's values*/
        USE-INDEX tag
        NO-ERROR. 
    IF AVAIL loadtag THEN 
    DO:
        IF  loadtag.job-no          NE fg-bin.job-no
            OR loadtag.job-no2      NE fg-bin.job-no2
            OR loadtag.loc          NE fg-bin.loc
            OR loadtag.loc-bin      NE fg-bin.loc-bin
            OR loadtag.qty          NE fg-bin.qty
            OR loadtag.pallet-count NE fg-bin.qty
            OR loadtag.partial      NE fg-bin.partial-count THEN 
        DO:
                
            FIND FIRST bf-loadtag EXCLUSIVE-LOCK
                WHERE ROWID(bf-loadtag) EQ ROWID(loadtag)
                NO-ERROR.
            IF AVAILABLE bf-loadtag THEN 
                ASSIGN
                    bf-loadtag.job-no       = fg-bin.job-no
                    bf-loadtag.job-no2      = fg-bin.job-no2
                    bf-loadtag.loc          = fg-bin.loc
                    bf-loadtag.loc-bin      = fg-bin.loc-bin
                    bf-loadtag.qty          = fg-bin.qty
                    bf-loadtag.pallet-count = fg-bin.qty
                    bf-loadtag.partial      = fg-bin.partial-count
                    bf-loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case
                    .
                
        END.
        
        FIND CURRENT bf-loadtag NO-LOCK  NO-ERROR.
        
        IF TRIM(loadtag.job-no) NE "" THEN
            FOR EACH job NO-LOCK
                WHERE job.company EQ loadtag.company
                AND job.job-no  EQ loadtag.job-no
                AND job.job-no2 EQ loadtag.job-no2,
                EACH job-hdr NO-LOCK
                WHERE job-hdr.company EQ job.company
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                AND job-hdr.i-no    EQ loadtag.i-no
                AND job-hdr.ord-no  NE 0:
                    
                FIND FIRST bf-loadtag EXCLUSIVE-LOCK
                    WHERE ROWID(bf-loadtag) EQ ROWID(loadtag)
                    NO-ERROR.
                    
                IF AVAILABLE bf-loadtag THEN                    
                     bf-loadtag.ord-no = job-hdr.ord-no.
                     
                FIND CURRENT bf-loadtag NO-LOCK  NO-ERROR.
                LEAVE.
            END.
    END. /* If avail loadtag */
END.  /* If avail fg-bin */
