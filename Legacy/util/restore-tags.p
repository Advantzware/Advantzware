
{sys/inc/var.i NEW SHARED}


PAUSE 0 BEFORE-HIDE.

FOR EACH company,
    EACH itemfg WHERE itemfg.company EQ company.company:

  DISPLAY "Processing Company/FG Item#:" +
          TRIM(itemfg.company)           + "/" +
          TRIM(itemfg.i-no)              FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  FOR EACH fg-bin NO-LOCK
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        AND fg-bin.tag     GT ""
        AND SUBSTR(fg-bin.tag,1,15)
                           EQ itemfg.i-no
        AND fg-bin.qty     NE 0
        AND NOT CAN-FIND(FIRST loadtag
                         WHERE loadtag.company     EQ fg-bin.company
                           AND loadtag.item-type   EQ NO
                           AND loadtag.tag-no      EQ fg-bin.tag
                           AND loadtag.is-case-tag EQ NO):

    CREATE loadtag.
    ASSIGN
     loadtag.company      = fg-bin.company
     loadtag.item-type    = NO
     loadtag.is-case-tag  = NO
     loadtag.tag-no       = fg-bin.tag
     loadtag.i-no         = fg-bin.i-no
     loadtag.i-name       = itemfg.i-name
     loadtag.job-no       = fg-bin.job-no     
     loadtag.job-no2      = fg-bin.job-no2
     loadtag.qty          = fg-bin.qty
     loadtag.pallet-count = fg-bin.qty
     loadtag.qty-case     = fg-bin.case-count
     loadtag.partial      = fg-bin.partial-count
     loadtag.case-bundle  = fg-bin.cases-unit
     loadtag.tot-cases    = TRUNC((loadtag.qty - loadtag.partial) / loadtag.qty-case,0)
     loadtag.loc          = fg-bin.loc        
     loadtag.loc-bin      = fg-bin.loc-bin
     loadtag.sts          = "Printed"
     loadtag.tag-date     = TODAY
     loadtag.tag-time     = TIME.
  END.

  FOR EACH fg-rdtlh NO-LOCK
      WHERE fg-rdtlh.company          EQ itemfg.company
        AND fg-rdtlh.tag              BEGINS itemfg.i-no
        AND SUBSTR(fg-rdtlh.tag,1,15) EQ itemfg.i-no
        AND fg-rdtlh.rita-code        EQ "R"
      USE-INDEX tag,
      FIRST fg-rcpth NO-LOCK
      WHERE fg-rcpth.r-no      EQ fg-rdtlh.r-no
        AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code
      BY fg-rdtlh.tag DESC:
        
    IF NOT CAN-FIND(FIRST loadtag
                    WHERE loadtag.company     EQ fg-rdtlh.company
                      AND loadtag.item-type   EQ NO
                      AND loadtag.tag-no      EQ fg-rdtlh.tag
                      AND loadtag.is-case-tag EQ NO) THEN DO:
      CREATE loadtag.
      ASSIGN
       loadtag.company      = fg-rdtlh.company
       loadtag.item-type    = NO
       loadtag.is-case-tag  = NO
       loadtag.tag-no       = fg-rdtlh.tag
       loadtag.i-no         = fg-rcpth.i-no
       loadtag.i-name       = itemfg.i-name
       loadtag.job-no       = fg-rcpth.job-no     
       loadtag.job-no2      = fg-rcpth.job-no2
       loadtag.qty          = fg-rdtlh.qty
       loadtag.pallet-count = fg-rdtlh.qty
       loadtag.qty-case     = fg-rdtlh.qty-case
       loadtag.partial      = fg-rdtlh.partial
       loadtag.case-bundle  = fg-rdtlh.stacks-unit
       loadtag.tot-cases    = TRUNC((loadtag.qty - loadtag.partial) / loadtag.qty-case,0)
       loadtag.loc          = fg-rdtlh.loc        
       loadtag.loc-bin      = fg-rdtlh.loc-bin
       loadtag.sts          = "Printed"
       loadtag.tag-date     = TODAY
       loadtag.tag-time     = TIME.
    END.

    LEAVE.
  END.
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
