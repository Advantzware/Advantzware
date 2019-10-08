
{sys/inc/var.i SHARED}


SESSION:SET-WAIT-STATE ("general").

FOR EACH rm-rctd
    WHERE rm-rctd.company    EQ cocode
      AND rm-rctd.rita-code  EQ "I"
      AND rm-rctd.s-num      EQ 0
      AND rm-rctd.job-no     NE ""
      AND INT(rm-rctd.po-no) NE 0,

    EACH rm-rcpth
    WHERE rm-rcpth.company    EQ rm-rctd.company
      AND rm-rcpth.i-no       EQ rm-rctd.i-no
      AND rm-rcpth.po-no      EQ rm-rctd.po-no
      AND rm-rcpth.trans-date EQ rm-rctd.rct-date
      AND rm-rcpth.job-no     EQ rm-rctd.job-no
      AND rm-rcpth.job-no2    EQ rm-rctd.job-no2
      AND rm-rcpth.r-no       LT rm-rctd.r-no
      AND rm-rcpth.rita-code  EQ "R"
    NO-LOCK,

    FIRST rm-rdtlh
    WHERE rm-rdtlh.r-no  EQ rm-rcpth.r-no
      AND rm-rdtlh.s-num EQ 0:

  FIND po-ordl
      WHERE po-ordl.company   EQ rm-rctd.company
        AND po-ordl.po-no     EQ INT(rm-rctd.po-no)
        AND po-ordl.i-no      EQ rm-rctd.i-no
        AND po-ordl.job-no    EQ rm-rctd.job-no
        AND po-ordl.job-no2   EQ rm-rctd.job-no2
        AND po-ordl.item-type EQ YES
      NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN
    ASSIGN
     rm-rdtlh.s-num = po-ordl.s-num
     rm-rdtlh.b-num = po-ordl.b-num
     rm-rctd.s-num  = po-ordl.s-num
     rm-rctd.b-num  = po-ordl.b-num.
END.

SESSION:SET-WAIT-STATE ("").
