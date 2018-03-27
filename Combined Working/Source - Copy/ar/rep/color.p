/* ------------------------------------------------ ar/rep/color.p  05/97 FWK */
/* Color Carton A/R Invoice Print Program - A/R Module                        */
/* -------------------------------------------------------------------------- */
{sys/inc/var.i shared}
{sys/form/s-top.f}

{ar/rep/invoice.i}

def var save_id         AS RECID NO-UNDO.
def var v-print-align   AS LOGICAL FORMAT "Y/N" NO-UNDO.
def var v-align-ok      AS LOGICAL FORMAT "Y/N" NO-UNDO.
def var v-line-number   AS int NO-UNDO.
def var v-subtot        LIKE ar-inv.t-sales NO-UNDO COLUMN-LABEL "Subtotal".
def var v-invhead AS CHARACTER FORMAT "x(13)" INITIAL
  "I N V O I C E".
def var letterhead AS CHARACTER NO-UNDO FORMAT 'x(50)' EXTENT 5.
def var big_ul AS CHARACTER NO-UNDO FORMAT 'x(80)'.
def var big_ul2 LIKE big_ul NO-UNDO.
def var v-disc-avail like ar-inv.disc-taken no-undo
    label "Disc Avail"
    column-label "Disc Avail".
def var v-disc-date like ar-inv.due-date no-undo
    label "Disc Date"
    column-label "Disc Date".
DEF VAR lv-zero LIKE ar-inv.t-weight INIT 0 NO-UNDO.
def var lv-ship-date as date initial today.
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.
def var v-case-cnt as char format "x(70)" extent 5.
def var v-case-line as char.
def var v-part-line as char.
def var disp-frt as char init "Freight:".

big_ul = FILL('=',80).
big_ul2 = big_ul.

{custom/notesdef.i}
DEF VAR v-inst AS cha FORM "x(60)" EXTENT 4 NO-UNDO.

DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.

{custom/formtext.i NEW}

FORMAT /*SKIP(2)*/
  v-invhead AT 24
  ar-inv.inv-date        AT 70 FORMAT "99/99/99" SKIP
  company.name AT 11 SKIP
  company.addr[1] AT 11 SKIP
  company.addr[2] AT 11
  ar-inv.inv-no          AT 70 FORMAT "zzzzz9" SKIP
  company.city AT 11
  company.state company.zip SKIP
  sman.sman         AT 70
  SKIP (3)
  ar-inv.fob-code        AT 4
  ar-inv.terms-d         AT 32
  SKIP (2)
  ar-inv.cust-no         AT 11
  ar-inv.ship-id         AT 55
  cust.name              AT 11
  shipto.ship-name       AT 55 FORMAT "x(27)"
  cust.addr [1]          AT 11
  shipto.ship-addr [1]   AT 55 FORMAT "x(27)"
  cust.addr [2]          AT 11
  shipto.ship-addr [2]   AT 55 FORMAT "x(27)"
  cust.city              AT 11
  cust.state             AT 28
  cust.zip               AT 31
  shipto.ship-city       AT 55 FORMAT "x(12)"
  shipto.ship-state      AT 69
  shipto.ship-zip        AT 72
  SKIP (5)
  lv-ship-date           to 8
  carrier.dscr           AT 11 FORMAT "x(25)"
  lv-zero                TO 44 FORMAT ">>>>9.99"
  lv-bol-no              to 65 format ">>>>>>>>"
  SKIP (3)
  WITH FRAME invoice-header NO-BOX NO-LABELS stream-io width 90.

FORMAT 
  ar-invl.po-no
  ar-invl.i-no           AT 19 FORMAT "x(23)"
  ar-invl.inv-qty        TO 52 FORMAT "->,>>>,>>9"
  ar-invl.unit-pr        TO 64 FORMAT "->>>,>>9.99"
  ar-invl.pr-qty-uom     TO 68
  ar-invl.amt            TO 80 FORMAT "->>>,>>9.99" SKIP
  ar-invl.i-name         AT 19 FORMAT "x(23)"
  ar-invl.ship-qty       TO 52 format "->,>>>,>>9" SKIP
  WITH FRAME invoice-line NO-BOX NO-LABELS stream-io width 90 DOWN.

FORMAT SKIP (1)
  disp-frt               AT 1
  ar-inv.freight         AT 11 FORMAT "->,>>9.99"
  ar-inv.tax-amt         AT 21 FORMAT "->,>>9.99"
  ar-inv.net             TO 43 FORMAT "->,>>>,>>9.99"
  ar-inv.disc-taken      TO 56 FORMAT ">>>,>>9.99"
  ar-inv.due-date        AT 60 FORMAT "99/99/99" 
  ar-inv.due             TO 80 FORMAT "->,>>>,>>9.99"
  WITH FRAME invoice-total NO-BOX NO-LABELS stream-io width 90.

/* Following set of forms are when sys-ctrl says to draw form ...  9507 CAH */
FORMAT SKIP (2)
  letterhead[1] AT 5
  "     Date:" TO 70 ar-inv.inv-date FORMAT "99/99/99" SKIP
  letterhead[2] AT 5
  letterhead[3] AT 5
  "Invoice #:" TO 70 ar-inv.inv-no SKIP
  letterhead[4] AT 5
  letterhead[5] AT 5
  SKIP (4)
  "SOLD TO:" AT 11
  ar-inv.cust-no
  "SHIP TO:" AT 54
  ar-inv.ship-id
  cust.name              AT 11
  shipto.ship-name       AT 54 FORMAT "x(27)"
  cust.addr [1]          AT 11
  shipto.ship-addr [1]   AT 54 FORMAT "x(27)"
  cust.addr [2]          AT 11
  shipto.ship-addr [2]   AT 54 FORMAT "x(27)"
  cust.city              AT 11
  cust.state
  cust.zip
  shipto.ship-city       AT 54 FORMAT "x(12)"
  shipto.ship-state
  shipto.ship-zip
  SKIP(5)
  big_ul AT 1
  WITH FRAME invoice-header-labels NO-BOX NO-LABELS stream-io width 80.

FORMAT
  ar-inv.po-no   AT 6
  /* ar-inv.terms 9507 CAH */
  ar-inv.terms-d    COLUMN-LABEL "Terms"
  ar-inv.fob        COLUMN-LABEL "FOB"
  ar-inv.carrier    COLUMN-LABEL "Via"
  /* carrier.dscr      column-label "Carrier" */
  /*ar-inv.t-weight   COLUMN-LABEL "Weight" FORMAT "->>,>>9"*/
  lv-zero           COLUMN-LABEL "Weight" FORMAT "->>,>>9"
  sman.sman    COLUMN-LABEL "Rep" SKIP
  WITH FRAME INVOICE-RIBBON-LABELS NO-BOX NO-UNDERLINE stream-io width 80 1 DOWN.

FORMAT
  ar-invl.i-name  COLUMN-LABEL "Item / Description" AT 3
  ar-invl.inv-qty format "->>>,>>9.99" COLUMN-LABEL "Quantity"
  ar-invl.unit-pr format "->,>>>,>>9.99<<" COLUMN-LABEL "Price"
  ar-invl.pr-qty-uom  NO-LABEL /* gdm - 06190905*/
  ar-invl.amt     COLUMN-LABEL "Amount"
  WITH FRAME INVOICE-LINE-LABELS NO-BOX stream-io width 80 DOWN.

FORMAT
    space(21)   /* to align with line item amount field */
  v-subtot FORMAT "$->>,>>>,>>9.99"
  ar-inv.freight  FORMAT "->>>,>>9.99"
  COLUMN-LABEL "Freight"
  ar-inv.tax-amt  FORMAT "->>>,>>9.99"
  COLUMN-LABEL "Sales Tax"
  /*
  COLUMN-LABEL "Net Amount"
  ar-inv.disc-taken       FORMAT "->>,>>9.99"
  COLUMN-LABEL "Cash Disc"
  ar-inv.due-date
  COLUMN-LABEL "If Paid By"
  ar-inv.due              FORMAT "->,>>>,>>>.99"
  */
  ar-inv.net              FORMAT "$->>,>>>,>>9.99"
  COLUMN-LABEL "Invoice Amt"
  WITH FRAME invoice-total-labels NO-BOX NO-UNDERLINE stream-io width 85.


FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

IF v-print-head THEN
DO:
  IF oe-ctrl.prcom THEN
  ASSIGN
    letterhead[1] = "=== " + v-invhead + " ==="
    letterhead[2] =  company.name
    letterhead[3] =  company.addr[1]
    letterhead[4] =  company.addr[2]
    letterhead[5] =  company.city + ', ' + company.state + '  ' + company.zip.

  DO i = 5 TO 2 BY -1:
    IF letterhead[i - 1] <= ' '
      AND letterhead[i] >= ''
      THEN
    ASSIGN letterhead[i - 1] = letterhead[i]
      letterhead[i] = ''.
  END.
  DO i = 1 TO 5:
    {sys/inc/ctrtext.i letterhead[i] 50}.
  END.
END.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id:
    
  CLEAR FRAME invoice-header ALL NO-PAUSE.
  assign
    v-disc-avail = 0
    v-disc-date = ?
    v-subtot = 0.

  FIND FIRST cust WHERE cust.company = cocode AND
    cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.

  FIND FIRST shipto WHERE shipto.company = cocode AND
    shipto.cust-no = ar-inv.cust-no AND  /* DAR */
    shipto.ship-id = ar-inv.ship-id NO-LOCK NO-ERROR.

  FIND FIRST carrier WHERE carrier.company = cocode AND
    carrier.carrier = ar-inv.carrier NO-LOCK NO-ERROR.

  FIND FIRST sman WHERE sman.company = cocode AND
    sman.sman = cust.sman NO-LOCK NO-ERROR.

  ASSIGN
   lv-bol-no    = 0
   lv-ship-date = ar-inv.inv-date.

  FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no NO-LOCK:
    lv-bol-no = ar-invl.bol-no.
    FIND FIRST oe-bolh
        WHERE oe-bolh.company EQ ar-inv.company
          AND oe-bolh.bol-no  EQ ar-invl.bol-no
        NO-LOCK NO-ERROR.
    IF AVAIL oe-bolh THEN lv-ship-date = oe-bolh.bol-date.
  end.

  IF v-print-head THEN
  DO:                     /* Print Headers */
    DISPLAY
      /* v-invhead when oe-ctrl.prcom 9507 in letterhead[1] */
      ar-inv.inv-date
      letterhead[1]
      letterhead[2]
      letterhead[3]
      letterhead[4]
      letterhead[5]
      /*
      company.name when oe-ctrl.prcom
      company.addr[1] when oe-ctrl.prcom
      company.addr[2] when oe-ctrl.prcom
      company.city when oe-ctrl.prcom
      company.state when oe-ctrl.prcom
      company.zip when oe-ctrl.prcom
      */
      ar-inv.inv-no
      ar-inv.cust-no
      ar-inv.ship-id
      cust.name            WHEN AVAILABLE cust
      shipto.ship-name     WHEN AVAILABLE shipto
      cust.addr [1]        WHEN AVAILABLE cust
      shipto.ship-addr [1] WHEN AVAILABLE shipto
      cust.addr [2]        WHEN AVAILABLE cust
      shipto.ship-addr [2] WHEN AVAILABLE shipto
      cust.city            WHEN AVAILABLE cust
      cust.state           WHEN AVAILABLE cust
      cust.zip             WHEN AVAILABLE cust
      shipto.ship-city     WHEN AVAILABLE shipto
      shipto.ship-state    WHEN AVAILABLE shipto
      shipto.ship-zip      WHEN AVAILABLE shipto
      big_ul
      WITH FRAME invoice-header-labels.
    DISPLAY
      ar-inv.po-no
      /* ar-inv.terms 9507 CAH */
      ar-inv.terms-d
      ar-inv.carrier
      ar-inv.fob-code
      /* carrier.dscr         WHEN AVAILABLE carrier 9507 CAH */
      /*ar-inv.t-weight WHEN ar-inv.t-weight <> 0*/
      lv-zero WHEN lv-zero <> 0
      sman.sman
      WITH FRAME invoice-ribbon-labels.
    PUT SKIP BIG_UL2 SKIP(1).
  END.
  ELSE
  DISPLAY
    v-invhead WHEN oe-ctrl.prcom
    ar-inv.inv-date
    company.name WHEN oe-ctrl.prcom
    company.addr[1] WHEN oe-ctrl.prcom
    ar-inv.inv-no
    company.addr[2] WHEN oe-ctrl.prcom
    company.city WHEN oe-ctrl.prcom
    company.state WHEN oe-ctrl.prcom
    company.zip WHEN oe-ctrl.prcom
    sman.sman
    ar-inv.fob-code
    ar-inv.terms-d
    ar-inv.cust-no
    ar-inv.ship-id
    cust.name            WHEN AVAILABLE cust
    shipto.ship-name     WHEN AVAILABLE shipto
    cust.addr [1]        WHEN AVAILABLE cust
    shipto.ship-addr [1] WHEN AVAILABLE shipto
    cust.addr [2]        WHEN AVAILABLE cust
    shipto.ship-addr [2] WHEN AVAILABLE shipto
    cust.city            WHEN AVAILABLE cust
    cust.state           WHEN AVAILABLE cust
    cust.zip             WHEN AVAILABLE cust
    shipto.ship-city     WHEN AVAILABLE shipto
    shipto.ship-state    WHEN AVAILABLE shipto
    shipto.ship-zip      WHEN AVAILABLE shipto
    lv-ship-date
    carrier.dscr         WHEN AVAILABLE carrier
    lv-zero
    lv-bol-no
    WITH FRAME invoice-header.

  ASSIGN v-line-number = 22.

  FOR EACH ar-invl WHERE ar-invl.x-no = ar-inv.x-no BREAK BY ar-invl.line:
    ASSIGN
     v-case-line = ""
     v-part-line = ""
     v-case-cnt  = "".
        
    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ ar-invl.company
          AND oe-boll.b-no    EQ ar-invl.b-no
          AND oe-boll.i-no    EQ ar-invl.i-no
          AND oe-boll.po-no   EQ ar-invl.po-no
        USE-INDEX b-no:

                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
            assign v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else assign v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

            do i = 1 to 5:
              if (80 - length(v-case-cnt[i])) > length(v-case-line) and
                v-case-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                     v-case-line = "".
              if (80 - length(v-case-cnt[i])) > length(v-part-line) and
                v-part-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                     v-part-line = "".
            end. /* 1 to 5 */
    END. /* each oe-boll */

    IF v-print-head THEN
    DO
        WITH FRAME invoice-line-labels:       

      DISPLAY
        ar-invl.i-name
        ar-invl.inv-qty
          ar-invl.qty when ar-invl.inv-qty eq 0 @ ar-invl.inv-qty
        ar-invl.unit-pr
        ar-invl.pr-qty-uom /* gdm - 06190905*/
        ar-invl.amt.
      DOWN 1.
      IF ar-invl.i-dscr > ' ' THEN
      DO:
        DISPLAY ar-invl.i-dscr @ ar-invl.i-name.
        DOWN 1.
      END.
      DOWN 1.
    END.
    ELSE
    DO:
        
      DISPLAY ar-invl.po-no
              ar-invl.i-no
                ar-invl.i-name WHEN ar-invl.i-no EQ "" @ ar-invl.i-no
              ar-invl.inv-qty
                ar-invl.qty WHEN ar-invl.inv-qty EQ 0 @ ar-invl.inv-qty
              ar-invl.unit-pr
              ar-invl.pr-qty-uom
              ar-invl.amt
              ar-invl.i-name
                ar-invl.i-dscr WHEN ar-invl.i-no EQ "" @ ar-invl.i-name
                ar-invl.part-dscr1 WHEN ar-invl.i-no EQ "" AND ar-invl.i-dscr EQ "" @ ar-invl.i-name
              ar-invl.ship-qty
        WITH FRAME invoice-line DOWN.
      DOWN 1 WITH FRAME invoice-line.
    END.
                                          /** Display Case Count Lines **/
    DO i = 1 TO 5:
      IF v-case-cnt[i] NE "" THEN DO:
        PUT v-case-cnt[i] AT 5.
        v-line-number = v-line-number + 1.
      END.
    END. /* 1 to 5 */

    put skip(1).

    v-subtot = v-subtot + ar-invl.amt.

    IF v-print-head THEN
    DO:    /* page break logic for drawn form */
      IF LINE-COUNTER >= (PAGE-SIZE - 8) THEN
      DO:
        PUT SKIP(1)
          space(29) "- - - CONTINUED - - -".
          PAGE.
        VIEW FRAME invoice-header-labels.
        VIEW FRAME invoice-ribbon-labels.
        PUT SKIP BIG_UL2 SKIP(1).
        VIEW FRAME invoice-line-labels.
      END.
    END.
    ELSE
    DO:        /* page break logic for manual form */
      ASSIGN v-line-number = v-line-number + 3.
      IF v-line-number >= 46 THEN
      DO:
        DISPLAY SKIP (1)
          "- - - CONTINUED - - -" AT 30
          /*SKIP (7)*/
          WITH FRAME page-break NO-BOX NO-LABELS NO-ATTR-SPACE.
        PAGE.
        DISPLAY WITH FRAME invoice-header.
        v-line-number = 22.
        
      END.
    END.    /* preprinted form */
  END. /* for each ar-invl record */

  ASSIGN
   v-disc-avail = ROUND(ar-inv.net * (ar-inv.disc-% / 100), 2)
   v-disc-date  = ar-inv.inv-date + ar-inv.disc-days.

  /*
   {custom/notesprt.i inv-head v-inst 4}
           DO i = 1 TO 4:
               IF v-inst[i] <> "" THEN DO: 
                  PUT v-inst[i] SKIP.
                  v-printline = v-printline + 1.
               END.
           END.

  */
  lv-text = "".     
  FOR EACH notes WHERE notes.rec_key EQ ar-inv.rec_key 
      AND notes.note_type <> "I" NO-LOCK:
    lv-text = lv-text + " " + TRIM(notes.note_text).
  END.

  FOR EACH tt-formtext:
    DELETE tt-formtext.
  END.

  CREATE tt-formtext.
  ASSIGN
   tt-line-no = 1
   tt-length  = 63.
    
  DO li = 2 TO 4:
    CREATE tt-formtext.
    ASSIGN
     tt-line-no = li
     tt-length  = 73.
  END.

  RUN custom/formtext.p (lv-text).

  li = 0.
  FOR EACH tt-formtext:
    li = li + 1.
    IF li = 1 THEN PUT "            " AT 5 tt-text FORM "x(65)" SKIP.
    ELSE PUT tt-text AT 5 FORM "x(73)" SKIP.
    v-line-number = v-line-number + 1.
    IF li GE 4 THEN LEAVE.
  END.

  IF v-print-head THEN
  DO:
    def var adv AS int NO-UNDO.
    adv = 56 - LINE-COUNTER.
    PUT SKIP(adv).

    DISPLAY
      v-subtot
      ar-inv.freight
      ar-inv.tax-amt
      ar-inv.net
      WITH FRAME invoice-total-labels.
    if v-disc-avail > 0 then
        put skip(1)
            space(30)
            "If paid by " v-disc-date " you may deduct:"
             v-disc-avail format "$>>>,>>>.99".
  END.
  ELSE
  DO:
    DO i = v-line-number TO 50:
      DISPLAY SKIP (1) WITH FRAME skip-to-bottom NO-BOX NO-LABELS DOWN.
      DOWN 1 WITH FRAME skip-to-bottom.
    END.
    DISPLAY disp-frt
            ar-inv.freight
            ar-inv.tax-amt
            ar-inv.net
            v-disc-avail @ ar-inv.disc-taken
            v-disc-date  @ ar-inv.due-date
            ar-inv.due
      WITH FRAME invoice-total.
  END.

  ASSIGN ar-inv.printed = YES.

END. /* for each ar-inv record */

/* End ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */
