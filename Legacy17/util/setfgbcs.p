
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR li-unit AS INT NO-UNDO.


FOR EACH company NO-LOCK:
  FOR EACH fg-bin
      WHERE fg-bin.company EQ company.company
        AND fg-bin.tag     GT ""
        AND SUBSTR(fg-bin.tag,31,10) EQ "CUSTOMER  "
      USE-INDEX tag
      BY fg-bin.tag
      BY fg-bin.i-no:

    DISPLAY fg-bin.company LABEL "Company"
            SUBSTR(fg-bin.tag,41,30)
                           LABEL "Cust#"
                           FORMAT "x(12)"
            fg-bin.i-no    LABEL "FG Item#"
                           FORMAT "x(20)"
        WITH FRAME f1 TITLE "  FG Bins  ".
        
    SUBSTR(fg-bin.tag,31,100) = "".
  END.

  FOR EACH loc NO-LOCK WHERE loc.company EQ company.company,
      EACH fg-rdtlh
      WHERE fg-rdtlh.company EQ loc.company
        AND fg-rdtlh.loc     EQ loc.loc
        AND fg-rdtlh.tag     GT ""
        AND SUBSTR(fg-rdtlh.tag,31,10) EQ "CUSTOMER  "
      USE-INDEX tag,

      FIRST fg-rcpth WHERE fg-rcpth.r-no EQ fg-rdtlh.r-no NO-LOCK

      BY fg-rdtlh.tag
      BY fg-rcpth.i-no:

    DISPLAY fg-rdtlh.company LABEL "Company"
            SUBSTR(fg-rdtlh.tag,41,30)
                             LABEL "Cust#"
                             FORMAT "x(12)"
            fg-rcpth.i-no    LABEL "FG Item#"
                             FORMAT "x(20)"
        WITH FRAME f2 TITLE "  FG History  ".
        
    ASSIGN
     fg-rdtlh.cust-no            = SUBSTR(fg-rdtlh.tag,41,30)
     SUBSTR(fg-rdtlh.tag,31,100) = "".
  END.
END.

MESSAGE "Procedure Complete..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

HIDE FRAME f1 NO-PAUSE.
HIDE FRAME f2 NO-PAUSE.

