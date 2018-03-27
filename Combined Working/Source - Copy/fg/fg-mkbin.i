
IF INDEX("RATCE",fg-rcpth.rita-code) NE 0 THEN
  {1}fg-bin.qty = fg-rdtlh.qty +
                  IF fg-rcpth.rita-code EQ "C" THEN 0 ELSE {1}fg-bin.qty.

ELSE
  {1}fg-bin.qty = {1}fg-bin.qty - fg-rdtlh.qty.

IF fg-rcpth.rita-code EQ "C" THEN {1}fg-bin.partial-count = fg-rdtlh.partial.

ELSE DO:
  FIND FIRST {2}fg-rctd WHERE {2}fg-rctd.r-no EQ fg-rcpth.r-no USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL {2}fg-rctd AND {2}fg-rctd.partial NE 0 THEN
    {1}fg-bin.partial-count = {1}fg-bin.partial-count +
                              ({2}fg-rctd.partial *
                               IF INDEX("TS",fg-rcpth.rita-code) GT 0 THEN -1 ELSE 1).
  ELSE
  IF fg-rdtlh.qty-case NE 0 THEN
    {1}fg-bin.partial-count = {1}fg-bin.partial-count +
                              ((fg-rdtlh.qty - (fg-rdtlh.cases * fg-rdtlh.qty-case)) *
                               IF INDEX("TS",fg-rcpth.rita-code) GT 0 THEN -1 ELSE 1).
END.
