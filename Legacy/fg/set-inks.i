
DO li = 1 TO EXTENT(eb.i-code{1}):
  IF eb.i-code{1}[li] NE "" THEN DO:
    FIND FIRST itemfg-ink
        WHERE itemfg-ink.company EQ itemfg.company
          AND itemfg-ink.i-no    EQ itemfg.i-no
          AND itemfg-ink.rm-i-no EQ eb.i-code{1}[li]
          AND itemfg-ink.pass    EQ eb.i-ps{1}[li]
          AND itemfg-ink.in-out  EQ NO
        NO-ERROR.

    IF NOT AVAIL itemfg-ink THEN DO:
      CREATE itemfg-ink.
      {custom/rec_key.i itemfg-ink}
    END.

    ASSIGN
     itemfg-ink.company = itemfg.company
     itemfg-ink.i-no    = itemfg.i-no
     itemfg-ink.rm-i-no = eb.i-code{1}[li]
     itemfg-ink.dscr    = eb.i-dscr{1}[li]
     itemfg-ink.pass    = eb.i-ps{1}[li]
     itemfg-ink.cover%  = eb.i-%{1}[li] NO-ERROR.

    

    IF NOT CAN-DO(rec-list,itemfg-ink.rec_key) THEN
      ASSIGN
       rec-list        = TRIM(rec-list)                       +
                         (IF rec-list EQ "" THEN "" ELSE ",") +
                         itemfg-ink.rec_key
       itemfg-ink.occurs = 0.

    itemfg-ink.occurs = itemfg-ink.occurs + 1.
  END.
END.
