/* ---------------------------------------------- oe/rep/bolhamil.p 05/07 JLF */

{sys/inc/var.i SHARED}
{sys/form/r-top.i}

DEF BUFFER xoe-boll FOR oe-boll.

{oe/rep/oe-lad.i}

DEF VAR v-ship-id          LIKE shipto.ship-id NO-UNDO.
DEF VAR v-ship-name        LIKE shipto.ship-name NO-UNDO.
DEF VAR v-ship-addr        LIKE shipto.ship-addr NO-UNDO.
DEF VAR v-ship-city        LIKE shipto.ship-city NO-UNDO.
DEF VAR v-ship-state       LIKE shipto.ship-state NO-UNDO.
DEF VAR v-ship-zip         LIKE shipto.ship-zip NO-UNDO.
DEF VAR v-ship-addr3       AS   CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-terms            LIKE oe-ord.terms-d NO-UNDO.
DEF VAR v-printline        AS   INT NO-UNDO.
DEF VAR v-lines            AS   INT NO-UNDO.
DEF VAR v-i-dscr           AS   CHAR EXTENT 4 NO-UNDO.
DEF VAR v-units            AS   DEC INIT 0 NO-UNDO.
DEF VAR v-pallets          AS   DEC INIT 0 NO-UNDO.
DEF VAR lv-date            AS   CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-text            AS   CHAR NO-UNDO.
DEF VAR li                 AS   INT NO-UNDO.
DEF VAR v-cases-unit       AS   DEC NO-UNDO.

{custom/formtext.i NEW}

FORM oe-bolh.bol-no        TO 78
     SKIP(4)
     "SHIP TO:"            AT 6
	 v-ship-id
     SKIP
     "------------------------------" AT 5
	 v-ship-name           AT 5
	 v-ship-addr[1]        AT 5
	 v-ship-addr[2]        AT 5
	 v-ship-addr3          AT 5
     SKIP(2)
     lv-date               AT 2
	 carrier.dscr          AT 48  FORMAT "x(17)"
	 v-terms               AT 66  FORMAT "x(19)"
	 SKIP(1)
     oe-bolh.release#      TO 43  FORMAT ">>>>>>>>>>"
     SKIP(2)
    WITH FRAME bolhead NO-LABELS NO-BOX NO-UNDERLINE STREAM-IO WIDTH 200.

FORM oe-boll.po-no     AT 2
     itemfg.part-no    AT 18   FORMAT "x(20)"
     oe-boll.i-no      AT 39   FORMAT "x(20)"
     v-pallets         TO 68   FORMAT ">>>,>>>"
     oe-boll.qty       TO 80   FORMAT ">>>,>>>,>>>"
     oe-boll.ord-no    TO 16   FORMAT ">>>>>>>>>>"
     v-i-dscr[1]       AT 18   FORMAT "X(40)"
     v-i-dscr[2]       AT 18   FORMAT "X(40)"
  WITH FRAME detail NO-ATTR-SPACE NO-LABELS NO-BOX NO-UNDERLINE DOWN STREAM-IO WIDTH 200.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

FOR EACH report   WHERE report.term-id EQ v-term-id,
    FIRST oe-bolh WHERE RECID(oe-bolh) EQ report.rec-id
    BREAK by oe-bolh.bol-no:

  IF FIRST-OF(oe-bolh.bol-no) THEN DO:
    RUN custom/eurodate.p (oe-bolh.bol-date, OUTPUT lv-date).

    FIND FIRST cust NO-LOCK
	    WHERE cust.company EQ oe-bolh.company
	      AND cust.cust-no EQ oe-bolh.cust-no
	    NO-ERROR.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    FIND FIRST carrier NO-LOCK
	    WHERE carrier.company EQ oe-bolh.company
	      AND carrier.carrier EQ oe-bolh.carrier
	    NO-ERROR.

	ASSIGN
	 v-ship-id      = shipto.ship-id
     v-ship-name    = shipto.ship-name
	 v-ship-addr[1] = shipto.ship-addr[1]
	 v-ship-addr[2] = shipto.ship-addr[2]
	 v-ship-city    = shipto.ship-city
	 v-ship-state   = shipto.ship-state
	 v-ship-zip     = shipto.ship-zip
     v-ship-addr3   = v-ship-city + ", " +
		              v-ship-state + "  " +
		              v-ship-zip
     v-terms        = "".
      
    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no,

        FIRST oe-ord NO-LOCK
	    WHERE oe-ord.company EQ oe-boll.company
	      AND oe-ord.ord-no  EQ oe-boll.ord-no:

      v-terms = oe-ord.terms-d.   

      LEAVE.
    END.

    IF TRIM(v-ship-addr3) EQ "," THEN v-ship-addr3 = "".

    RUN top-of-page.
  END. /* first-of(oe-bolh.bol-no) */

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no,

      FIRST itemfg NO-LOCK
	  WHERE itemfg.company EQ cocode
	    AND itemfg.i-no    EQ oe-boll.i-no
      BREAK BY oe-boll.i-no:

    FOR EACH tt-formtext:
      DELETE tt-formtext.
    END.

    lv-text = "".
    FOR EACH notes NO-LOCK
        WHERE notes.rec_key   EQ itemfg.rec_key
          AND notes.note_type EQ "S"
          AND notes.note_code EQ "SN":

      lv-text = lv-text                + " " +
                TRIM(notes.note_title) + " " +
                TRIM(notes.note_text)  + CHR(10).
    END.

    DO li = 1 TO 15:
      CREATE tt-formtext.
      ASSIGN
       tt-line-no = li
       tt-length  = 40.
    END.

    RUN custom/formtext.p (lv-text).

    FIND FIRST oe-ordl NO-LOCK
	    WHERE oe-ordl.company EQ oe-boll.company
	      and oe-ordl.ord-no  EQ oe-boll.ord-no
	      and oe-ordl.i-no    EQ oe-boll.i-no
	      and oe-ordl.line    EQ oe-boll.line
	    NO-ERROR.

    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ oe-boll.company
          AND fg-bin.job-no  EQ oe-boll.job-no
          AND fg-bin.job-no2 EQ oe-boll.job-no2
          AND fg-bin.i-no    EQ oe-boll.i-no
          AND fg-bin.loc     EQ oe-boll.loc
          AND fg-bin.loc-bin EQ oe-boll.loc-bin
          AND fg-bin.tag     EQ oe-boll.tag
          AND fg-bin.cust-no EQ oe-boll.cust-no
        NO-ERROR.

    ASSIGN
     v-i-dscr     = ""
     v-i-dscr[1]  = IF AVAIL oe-ordl THEN oe-ordl.i-name ELSE itemfg.i-name
     v-cases-unit = IF AVAIL fg-bin AND fg-bin.cases-unit NE 0 THEN
                      fg-bin.cases-unit ELSE 1
     v-pallets    = oe-boll.qty / oe-boll.qty-case / v-cases-unit
     v-units      = TRUNC(oe-boll.cases / v-cases-unit,0).

    {sys/inc/roundup.i v-pallets}

    IF v-units NE 0 THEN DO:
      v-cases-unit = MIN((oe-boll.qty - oe-boll.partial) / oe-boll.qty-case,
                         v-cases-unit).

      {sys/inc/roundup.i v-cases-unit}

      v-i-dscr[2] = TRIM(STRING(v-units,"->>>>>>>>>>"))+ "x" +
                    (IF v-cases-unit EQ 1 THEN "" ELSE
                     TRIM(STRING(v-cases-unit,">>>>>>>>>>")) + "x") +
                    TRIM(STRING(oe-boll.qty-case,">>>>>>>>>>")).
    END.

    IF oe-boll.partial NE 0 THEN DO:
      v-cases-unit = TRUNC(oe-boll.partial / oe-boll.qty-case,0).

      IF v-cases-unit NE 0 THEN
        v-i-dscr[2] = TRIM(v-i-dscr[2]) +
                      (IF v-i-dscr[2] EQ "" THEN "" ELSE "+") +
                      (IF oe-boll.partial LT 0 THEN "-" ELSE "") + "1x" +
                      (IF v-cases-unit EQ 1 THEN "" ELSE
                       TRIM(STRING(v-cases-unit,">>>>>>>>>>")) + "x") +
                      TRIM(STRING(oe-boll.qty-case,">,>>>,>>>")).

      v-cases-unit = oe-boll.partial MODULO oe-boll.qty-case.

      IF v-cases-unit NE 0 THEN
        v-i-dscr[2] = v-i-dscr[2] +
                      (IF v-i-dscr[2] EQ "" THEN "" ELSE "+") +
                      (IF v-cases-unit LT 0 THEN "-" ELSE "") + "1x" +
                      TRIM(STRING(v-cases-unit,">>>>>>>>>>")).
    END.

    IF v-i-dscr[1] EQ "" THEN
      ASSIGN
       v-i-dscr[1] = v-i-dscr[2]
       v-i-dscr[2] = "".

    v-lines = 3.

    IF v-i-dscr[2] NE "" THEN v-lines = v-lines + 1. 
    
    FOR EACH tt-formtext WHERE tt-text NE "":
      v-lines = v-lines + 1.
    END.

    IF v-printline + v-lines GT 38 THEN RUN page-break (LAST(oe-bolh.bol-no)).

    v-printline = v-printline + v-lines.

    DISPLAY oe-boll.po-no
            itemfg.part-no
            oe-boll.i-no
            v-pallets
            oe-boll.qty
            oe-boll.ord-no
            v-i-dscr[1]
            v-i-dscr[2]
	    WITH FRAME detail.

    FOR EACH tt-formtext WHERE tt-text NE "":
      PUT tt-text AT 18 FORMAT "X(40)" SKIP.
    END.

    PUT SKIP(1).

    oe-boll.printed = TRUE.
  END. /* for each oe-boll */

  RUN page-break (LAST(oe-bolh.bol-no)).

  oe-bolh.printed = TRUE.
END. /* for each oe-bolh */

RETURN.

PROCEDURE top-of-page.

  DISPLAY oe-bolh.bol-no
          v-ship-id
          v-ship-name
	      v-ship-addr[1]
	      v-ship-addr[2]
	      v-ship-addr3
          lv-date
	      carrier.dscr WHEN AVAIL carrier
	      v-terms
          oe-bolh.release#
      WITH FRAME bolhead.

  v-printline = 18.

END PROCEDURE.

PROCEDURE page-break.
  DEF INPUT PARAM ip-last AS LOG NO-UNDO.


  PUT SKIP(38 - v-printline).

  IF NOT ip-last THEN RUN top-of-page.

END PROCEDURE.
