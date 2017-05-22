
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


DEF VAR li AS INT NO-UNDO.

FIND fg-bin WHERE ROWID(fg-bin) EQ ip-rowid NO-ERROR.

IF AVAIL fg-bin THEN DO:
  ASSIGN
   fg-bin.qty           = 0
   fg-bin.partial-count = 0.

  IF TRIM(fg-bin.tag) EQ "" THEN
  FOR EACH fg-rcpth NO-LOCK
      WHERE fg-rcpth.company      EQ fg-bin.company
        AND fg-rcpth.i-no         EQ fg-bin.i-no
        AND fg-rcpth.job-no       EQ fg-bin.job-no
        AND fg-rcpth.job-no2      EQ fg-bin.job-no2
      USE-INDEX tran,

      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
        AND fg-rdtlh.loc          EQ fg-bin.loc
        AND fg-rdtlh.loc-bin      EQ fg-bin.loc-bin
        AND fg-rdtlh.tag          EQ fg-bin.tag
        AND fg-rdtlh.cust-no      EQ fg-bin.cust-no
        AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code
      USE-INDEX rm-rdtl

      BREAK BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:

    {fg/fgmkbin2.i}    
  END.

  ELSE
  FOR EACH fg-rdtlh
      WHERE fg-rdtlh.company      EQ fg-bin.company
        AND fg-rdtlh.tag          EQ fg-bin.tag
        AND fg-rdtlh.loc          EQ fg-bin.loc
        AND fg-rdtlh.loc-bin      EQ fg-bin.loc-bin
        AND fg-rdtlh.cust-no      EQ fg-bin.cust-no
      USE-INDEX tag,

      FIRST fg-rcpth NO-LOCK
      WHERE fg-rcpth.r-no         EQ fg-rdtlh.r-no
        AND fg-rcpth.i-no         EQ fg-bin.i-no
        AND fg-rcpth.job-no       EQ fg-bin.job-no
        AND fg-rcpth.job-no2      EQ fg-bin.job-no2
        AND fg-rcpth.rita-code    EQ fg-rdtlh.rita-code
      USE-INDEX r-no

      BREAK BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:

    {fg/fgmkbin2.i}
  END.
END.

