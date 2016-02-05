for each cust no-lock,
    EACH ar-inv no-lock WHERE ar-inv.posted  = NO AND
	 ar-inv.company = cocode and
	 ar-inv.cust-no = cust.cust-no and
	 ar-inv.company = cocode AND
	 ar-inv.loc     <> "Zqw"
      BREAK BY cust.{&sort-by}
	    BY ar-inv.inv-no
      WITH FRAME a1{&frame}:
    PUT SCREEN ROW lorow COLUMNS 70 STRING(ar-inv.inv-no,">>>>>9") .
    IF FIRST-OF(cust.{&sort-by}) THEN
      PUT cust.cust-no SPACE(1) cust.name.
    PUT ar-inv.inv-no   TO 47
      ar-inv.inv-date AT 49
      ar-inv.net      AT 58.
    v2 = v2 + net.
    v1 = v1 + ar-inv.disc-taken.
    FOR EACH ar-invl WHERE ar-invl.x-no = ar-inv.x-no
    no-lock BREAK BY ar-invl.line
	WITH FRAME a2{&frame} NO-BOX NO-LABELS STREAM-IO WIDTH 132:
      PUT SCREEN ROW lorow COLUMNS 77 STRING(ar-invl.line,">9") .
      PUT
	ar-invl.line FORMAT ">>9" AT 75
	ar-invl.i-name AT 79 SPACE(1)
	ar-invl.amt SKIP.
      IF ar-invl.i-dscr NE "" THEN
      PUT ar-invl.i-dscr AT 79 SKIP.
    END. /* each ar-invl */
    IF LAST-OF(cust.{&sort-by}) THEN
    DO:
      DISPLAY  "*  CUSTOMER TOTALS" TO 56 v2 AT 58 " *" SKIP(1)
	WITH FRAME vtot{&frame} NO-BOX NO-LABELS STREAM-IO WIDTH 132.
      g1 = g1 + v1.
      g2 = g2 + v2.

      v1 = 0.
      v2 = 0.
    END.
    g3 = g3 + ar-inv.tax-amt.
    g4 = g4 + ar-inv.freight.
  END. /* each invoice */
