
{sys/inc/var.i SHARED}

{fgrep/r-fgcst1.i}

FOR EACH tt-itemfg USE-INDEX part-no NO-LOCK,
  {fg/rep/fg-cst1.i tt-itemfg.part-cust tt-itemfg.i-no}
END.

