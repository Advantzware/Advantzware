
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF BUFFER b-fg-bin FOR fg-bin. 
    

FIND fg-bin WHERE ROWID(fg-bin) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL fg-bin THEN
FOR EACH loc NO-LOCK WHERE loc.company EQ fg-bin.company,
    EACH fg-rdtlh NO-LOCK
    WHERE fg-rdtlh.company EQ loc.company
      AND fg-rdtlh.loc     EQ loc.loc
      AND fg-rdtlh.tag     EQ fg-bin.tag
    USE-INDEX tag,

    FIRST fg-rcpth
    WHERE fg-rcpth.r-no      EQ fg-rdtlh.r-no
      AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code
      AND fg-rcpth.i-no      EQ fg-bin.i-no
      AND CAN-FIND(FIRST b-fg-bin
                   WHERE b-fg-bin.company EQ fg-rcpth.company
                     AND b-fg-bin.i-no    EQ fg-rcpth.i-no
                     AND b-fg-bin.job-no  EQ fg-rcpth.job-no
                     AND b-fg-bin.job-no2 EQ fg-rcpth.job-no2
                     AND b-fg-bin.loc     EQ fg-rdtlh.loc
                     AND b-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                     AND b-fg-bin.tag     EQ fg-rdtlh.tag
                     AND b-fg-bin.cust-no EQ fg-rdtlh.cust-no
                     AND b-fg-bin.qty     GT 0)
    BY fg-rcpth.trans-date DESC
    BY fg-rdtlh.trans-time DESC
    BY fg-rdtlh.r-no       DESC
    BY fg-rdtlh.qty        DESC:

  IF fg-rcpth.job-no  EQ fg-bin.job-no  AND
     fg-rcpth.job-no2 EQ fg-bin.job-no2 AND
     fg-rdtlh.loc     EQ fg-bin.loc     AND
     fg-rdtlh.loc-bin EQ fg-bin.loc-bin AND
     fg-rdtlh.cust-no EQ fg-bin.cust-no THEN
  FOR EACH loadtag
      WHERE loadtag.company      EQ fg-bin.company
        AND loadtag.item-type    EQ NO
        AND loadtag.tag-no       EQ fg-bin.tag
        AND loadtag.i-no         EQ fg-bin.i-no
        /* Removed for task# 06210614 
        AND loadtag.loc          EQ fg-bin.loc      /* task# 06130519 */
        AND loadtag.loc-bin      EQ fg-bin.loc-bin  /* Fg Transfer changes */*/
        AND loadtag.is-case-tag  EQ NO                    /* loadtags's values*/
      USE-INDEX tag:
    ASSIGN
     loadtag.job-no       = fg-bin.job-no
     loadtag.job-no2      = fg-bin.job-no2
     loadtag.loc          = fg-bin.loc
     loadtag.loc-bin      = fg-bin.loc-bin
     loadtag.qty          = fg-bin.qty
     loadtag.pallet-count = fg-bin.qty
     loadtag.partial      = fg-bin.partial-count
     loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.

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
      loadtag.ord-no = job-hdr.ord-no.
      LEAVE.
    END.
  END.

  LEAVE.
END.
