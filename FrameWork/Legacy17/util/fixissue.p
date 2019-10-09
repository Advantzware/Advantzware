
SESSION:SET-WAIT-STATE ("general").

FOR EACH company NO-LOCK,

    EACH rm-rcpth
    WHERE rm-rcpth.company   EQ company.company
      AND rm-rcpth.rita-code EQ "I"
    NO-LOCK,

    EACH rm-rdtlh
    WHERE rm-rdtlh.r-no EQ rm-rcpth.r-no
      AND rm-rdtlh.tag  GT ""
      AND rm-rdtlh.qty  EQ 0,

    FIRST rm-bin
    WHERE rm-bin.company EQ rm-rcpth.company
      AND rm-bin.i-no    EQ rm-rcpth.i-no
      AND rm-bin.loc     EQ rm-rdtlh.loc
      AND rm-bin.loc-bin EQ rm-rdtlh.loc-bin
      AND rm-bin.tag     EQ rm-rdtlh.tag
    NO-LOCK

    BY rm-rcpth.company
    BY rm-rcpth.r-no
    BY rm-rcpth.i-no
    BY rm-rdtlh.tag:

  DISPLAY rm-rcpth.company LABEL "Company"
          rm-rcpth.r-no    LABEL "Seq#"
          rm-rcpth.i-no    LABEL "RM Item#"
          rm-rdtlh.tag     LABEL "Tag#"
                           FORMAT "x(20)"
      WITH DOWN.

  rm-rdtlh.qty = rm-bin.qty.
END.

HIDE ALL NO-PAUSE.

SESSION:SET-WAIT-STATE ("").

