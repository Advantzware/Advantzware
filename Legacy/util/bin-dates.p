
DISABLE TRIGGERS FOR LOAD OF fg-bin.
DISABLE TRIGGERS FOR LOAD OF rm-bin.
                    
PAUSE 0 BEFORE-HIDE.

FOR EACH company NO-LOCK:

  FOR EACH fg-bin WHERE fg-bin.company EQ company.company TRANSACTION:

    DISPLAY "Processing Company/FG Item#: " +
            TRIM(fg-bin.company) + "/"      +
            TRIM(fg-bin.i-no)               FORMAT "x(70)"
        WITH FRAME f1 1 DOWN.

    IF fg-bin.tag EQ "" THEN
    FOR EACH fg-rcpth
        WHERE fg-rcpth.company   EQ fg-bin.company
          AND fg-rcpth.i-no      EQ fg-bin.i-no
          AND fg-rcpth.job-no    EQ fg-bin.job-no
          AND fg-rcpth.job-no2   EQ fg-bin.job-no2
        USE-INDEX tran NO-LOCK,

        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
          AND fg-rdtlh.loc       EQ fg-bin.loc    
          AND fg-rdtlh.loc-bin   EQ fg-bin.loc-bin
          AND fg-rdtlh.tag       EQ fg-bin.tag
          AND fg-rdtlh.cust-no   EQ fg-bin.cust-no
        USE-INDEX rm-rdtl NO-LOCK

        BREAK BY fg-rcpth.trans-date
              BY fg-rdtlh.trans-time
              BY fg-rcpth.r-no:

      IF fg-bin.aging-date EQ ? THEN
        fg-bin.aging-date = fg-rcpth.trans-date.

      IF fg-rcpth.rita-code EQ "R" THEN
        fg-bin.last-rct-date = fg-rcpth.trans-date.
    END.

    ELSE
    FOR EACH fg-rdtlh
        WHERE fg-rdtlh.company EQ fg-bin.company
          AND fg-rdtlh.tag     EQ fg-bin.tag
          AND fg-rdtlh.loc     EQ fg-bin.loc
          AND fg-rdtlh.loc-bin EQ fg-bin.loc-bin
          AND fg-rdtlh.cust-no EQ fg-bin.cust-no
        USE-INDEX tag NO-LOCK,
          
        EACH fg-rcpth
        WHERE fg-rcpth.r-no      EQ fg-rdtlh.r-no
          AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code
          AND fg-rcpth.i-no      EQ fg-bin.i-no
          AND fg-rcpth.job-no    EQ fg-bin.job-no
          AND fg-rcpth.job-no2   EQ fg-bin.job-no2
        USE-INDEX r-no NO-LOCK

        BREAK BY fg-rcpth.trans-date
              BY fg-rdtlh.trans-time
              BY fg-rcpth.r-no:

      IF fg-bin.aging-date EQ ? THEN
        fg-bin.aging-date = fg-rcpth.trans-date.

      IF fg-rcpth.rita-code EQ "R" THEN
        fg-bin.last-rct-date = fg-rcpth.trans-date.
    END.
  END.

  HIDE FRAME f1 NO-PAUSE.

  FOR EACH rm-bin WHERE rm-bin.company EQ company.company TRANSACTION:

    DISPLAY "Processing Company/RM Item#: " +
            TRIM(rm-bin.company) + "/"      +
            TRIM(rm-bin.i-no)               FORMAT "x(70)"
        WITH FRAME f2 1 DOWN.

    IF rm-bin.tag EQ "" THEN
    FOR EACH rm-rcpth
        WHERE rm-rcpth.company EQ rm-bin.company
          AND rm-rcpth.i-no    EQ rm-bin.i-no
        NO-LOCK,

        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND rm-rdtlh.loc       EQ rm-bin.loc    
          AND rm-rdtlh.loc-bin   EQ rm-bin.loc-bin
          AND rm-rdtlh.tag       EQ rm-bin.tag
        USE-INDEX rm-rdtl NO-LOCK

        BREAK BY rm-rcpth.trans-date
              BY rm-rcpth.r-no:

      IF rm-bin.aging-date EQ ? THEN
        rm-bin.aging-date = rm-rcpth.trans-date.

      IF rm-rcpth.rita-code EQ "R" THEN
        rm-bin.last-rct-date = rm-rcpth.trans-date.
    END.

    ELSE
    FOR EACH rm-rdtlh
        WHERE rm-rdtlh.company EQ rm-bin.company
          AND rm-rdtlh.tag     EQ rm-bin.tag
          AND rm-rdtlh.loc     EQ rm-bin.loc
          AND rm-rdtlh.loc-bin EQ rm-bin.loc-bin
        USE-INDEX tag NO-LOCK,
          
        EACH rm-rcpth
        WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
          AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code
          AND rm-rcpth.i-no      EQ rm-bin.i-no
        USE-INDEX r-no NO-LOCK

        BREAK BY rm-rcpth.trans-date
              BY rm-rcpth.r-no:

      IF rm-bin.aging-date EQ ? THEN
        rm-bin.aging-date = rm-rcpth.trans-date.

      IF rm-rcpth.rita-code EQ "R" THEN
        rm-bin.last-rct-date = rm-rcpth.trans-date.
    END.
  END.  

  HIDE FRAME f2 NO-PAUSE.
END.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
