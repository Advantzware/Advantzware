DEF PARAM BUFFER io-bolh FOR oe-bolh.
DEF INPUT PARAM new-freight LIKE oe-bolh.freight NO-UNDO.
DEF INPUT PARAM old-freight LIKE oe-bolh.freight NO-UNDO.

DEF VAR ld AS DEC NO-UNDO.

     
      IF CAN-FIND(FIRST oe-boll
                  WHERE oe-boll.company   EQ io-bolh.company
                    AND oe-boll.b-no      EQ io-bolh.b-no
                    AND ((oe-boll.freight EQ 0 AND oe-boll.weight NE 0) OR
                         oe-boll.freight  EQ ?)) THEN
      FOR EACH oe-boll
          WHERE oe-boll.company EQ io-bolh.company
            AND oe-boll.b-no    EQ io-bolh.b-no
          BREAK BY oe-boll.b-no:

        ASSIGN
         oe-boll.freight = new-freight * (oe-boll.weight / io-bolh.tot-wt)
         ld              = ld + oe-boll.freight.

        IF LAST(oe-boll.b-no) AND ld NE new-freight THEN
          oe-boll.freight = oe-boll.freight + (new-freight - ld).
      END.

      ELSE
      FOR EACH oe-boll
          WHERE oe-boll.company EQ io-bolh.company
            AND oe-boll.b-no    EQ io-bolh.b-no
          BREAK BY oe-boll.b-no:

        IF old-freight EQ 0 THEN oe-boll.freight = 0.
        ELSE
          ASSIGN
           oe-boll.freight = new-freight * (oe-boll.freight / old-freight)
           ld              = ld + oe-boll.freight.

        IF LAST(oe-boll.b-no) AND ld NE new-freight THEN
          oe-boll.freight = oe-boll.freight + (new-freight - ld).
      END.
