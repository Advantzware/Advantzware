DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-fg-bin FOR fg-bin.


FOR EACH company NO-LOCK,
    EACH oe-ordl NO-LOCK WHERE oe-ordl.company EQ company.company
    BY oe-ordl.ord-no
    BY oe-ordl.i-no:

  DISPLAY oe-ordl.company LABEL "Company"
          oe-ordl.ord-no  LABEL "Ord#"
          oe-ordl.i-no    LABEL "FG Item#"
                          FORMAT "x(20)".

  IF NOT oe-ordl.is-a-component                               AND
     CAN-FIND(FIRST b-oe-ordl
              WHERE b-oe-ordl.company        EQ oe-ordl.company
                AND b-oe-ordl.ord-no         EQ oe-ordl.ord-no
                AND b-oe-ordl.is-a-component EQ YES
                AND b-oe-ordl.set-hdr-line   EQ oe-ordl.line) THEN
  FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company EQ oe-ordl.company
        AND oe-boll.ord-no  EQ oe-ordl.ord-no
        AND oe-boll.i-no    EQ oe-ordl.i-no
        AND oe-boll.line    EQ oe-ordl.line,
      EACH fg-rcpth NO-LOCK
      WHERE fg-rcpth.company   EQ oe-boll.company
        AND fg-rcpth.b-no      EQ oe-boll.b-no
        AND fg-rcpth.i-no      EQ oe-boll.i-no
        AND fg-rcpth.rita-code EQ "T"
      USE-INDEX b-no,
      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND fg-rdtlh.cust-no   NE "",
      FIRST fg-bin
      WHERE fg-bin.company EQ fg-rcpth.company
        AND fg-bin.i-no    EQ fg-rcpth.i-no
        AND fg-bin.job-no  EQ fg-rcpth.job-no
        AND fg-bin.job-no2 EQ fg-rcpth.job-no2
        AND fg-bin.loc     EQ fg-rdtlh.loc
        AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
        AND fg-bin.tag     EQ fg-rdtlh.tag
        AND fg-bin.cust-no EQ fg-rdtlh.cust-no
        AND fg-bin.qty     GT 0,
      FIRST b-fg-bin
      WHERE b-fg-bin.company EQ fg-bin.company
        AND b-fg-bin.i-no    EQ fg-bin.i-no
        AND b-fg-bin.job-no  EQ fg-bin.job-no
        AND b-fg-bin.job-no2 EQ fg-bin.job-no2
        AND b-fg-bin.loc     EQ fg-bin.loc
        AND b-fg-bin.loc-bin EQ fg-bin.loc-bin
        AND b-fg-bin.tag     EQ fg-bin.tag
        AND b-fg-bin.cust-no EQ ""
        AND b-fg-bin.qty     EQ fg-bin.qty * -1:   

    ASSIGN
     fg-rdtlh.cust-no = ""
     b-fg-bin.qty     = 0.

    DELETE fg-bin.
  END.
END.

MESSAGE "Procedure complete..." VIEW-AS ALERT-BOX.
