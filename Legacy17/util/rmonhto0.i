 
DISABLE TRIGGERS FOR LOAD OF item. 
 
FOR EACH item 
    WHERE item.company EQ cocode
      AND item.i-no    GE fitm
      AND item.i-no    LE titm
    USE-INDEX i-no
    BY item.i-no:

  EMPTY TEMP-TABLE tt-rm-bin.

  FOR EACH rm-bin
      WHERE rm-bin.company EQ item.company
        AND rm-bin.i-no    EQ item.i-no
        AND rm-bin.qty     NE 0
      USE-INDEX i-no
      TRANSACTION:

    rm-bin.qty = 0.

    CREATE tt-rm-bin.
    BUFFER-COPY rm-bin TO tt-rm-bin.
  END.
  
  DISPLAY item.i-no FORMAT "x(15)" LABEL "RM Item#" WITH DOWN.

  FOR EACH b-rm-rcpth NO-LOCK
      WHERE b-rm-rcpth.company EQ item.company
        AND b-rm-rcpth.i-no    EQ item.i-no
      USE-INDEX i-no,

      EACH b-rm-rdtlh NO-LOCK
      WHERE b-rm-rdtlh.r-no EQ b-rm-rcpth.r-no
        AND b-rm-rdtlh.qty  NE 0
      USE-INDEX rm-rdtl

      BREAK BY b-rm-rdtlh.loc
            BY b-rm-rdtlh.loc-bin
            BY b-rm-rdtlh.tag
            BY b-rm-rdtlh.r-no desc:

    IF NOT CAN-FIND(FIRST tt-rm-bin
                    WHERE tt-rm-bin.company EQ item.company
                      AND tt-rm-bin.i-no    EQ item.i-no
                      AND tt-rm-bin.loc     EQ b-rm-rdtlh.loc
                      AND tt-rm-bin.loc-bin EQ b-rm-rdtlh.loc-bin
                      AND tt-rm-bin.tag     EQ b-rm-rdtlh.tag
                    USE-INDEX loc-bin) THEN DO:
      CREATE tt-rm-bin.
      BUFFER-COPY b-rm-rcpth EXCEPT po-no TO tt-rm-bin.
      BUFFER-COPY b-rm-rdtlh TO tt-rm-bin.
    END.  
  END.

  FOR EACH tt-rm-bin TRANSACTION:
    x = 0.

    RUN sys/ref/asiseq.p (INPUT item.company, INPUT "rm_rcpt_seq", OUTPUT X) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
       MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.


    CREATE rm-rcpth.
    ASSIGN
     rm-rcpth.r-no       = X
     rm-rcpth.trans-date = TODAY
     rm-rcpth.company    = item.company
     rm-rcpth.loc        = locode
     rm-rcpth.rita-code  = "C"
     rm-rcpth.i-no       = item.i-no
     rm-rcpth.post-date  = today
     rm-rcpth.i-name     = item.i-name
     rm-rcpth.pur-uom    = item.pur-uom.

    CREATE rm-rdtlh.
    ASSIGN
     rm-rdtlh.r-no      = rm-rcpth.r-no
     rm-rdtlh.company   = item.company
     rm-rdtlh.loc       = tt-rm-bin.loc
     rm-rdtlh.rita-code = "C"
     rm-rdtlh.loc-bin   = tt-rm-bin.loc-bin
     rm-rdtlh.tag       = tt-rm-bin.tag
     rm-rdtlh.qty       = 0.
  END.

  DO TRANSACTION:
    ASSIGN
     item.q-onh   = 0
     item.q-avail = item.q-onh + item.q-ono - item.q-comm.
  END.
END.
