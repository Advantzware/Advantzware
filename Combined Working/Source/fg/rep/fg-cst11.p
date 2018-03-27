
{sys/inc/var.i SHARED}

{fgrep/r-fgcst1.i}

FOR EACH tt-itemfg USE-INDEX cust-no NO-LOCK,
  {fg/rep/fg-cst1.i tt-itemfg.cust-no tt-itemfg.i-no}
END.


