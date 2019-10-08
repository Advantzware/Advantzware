/******************************************************************************** 
 ORIGINAL FORM =  ar/rep/crdbmemo.p
 GDM - 04210922 
 AlWstmemo.i
********************************************************************************/

{sys/inc/var.i shared}
{custom/notesdef.i}
{custom/formtext.i NEW} 
DEF SHARED var v-lo-cust like cust.cust-no init "" NO-UNDO.
DEF SHARED var v-hi-cust like cust.cust-no init "zzzzzzzz" NO-UNDO.
DEF SHARED VAR v-lo-memo like ar-cash.check-no init 0 NO-UNDO.
DEF SHARED VAR v-hi-memo like ar-cash.check-no init 99999999 NO-UNDO.
DEF SHARED VAR v-begdt   LIKE ar-cash.check-date NO-UNDO.
DEF SHARED VAR v-enddt   LIKE ar-cash.check-date NO-UNDO.
DEF SHARED VAR v-tbpst   AS LOG                  NO-UNDO.
DEF SHARED VAR v-reprint AS LOG                  NO-UNDO.
DEF SHARED VAR v-term-id AS CHAR                 NO-UNDO.

/* gdm - 03120909 */
DEF VAR lv-display-comp AS LOG  INIT YES       NO-UNDO.

DEF VAR lv-comp-color   AS CHAR INIT "BLACK"   NO-UNDO.
DEF VAR lv-other-color  AS CHAR INIT "BLACK"   NO-UNDO.
DEF VAR lv-comp-name    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR lv-email        AS CHAR FORMAT "x(48)" NO-UNDO.
DEF VAR v-comp-add1     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add5     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-bol-no        AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-po-no         AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-ord-no        AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-s-man         AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-fob           AS CHAR FORMAT "x(12)" NO-UNDO.
DEF VAR v-date-ship     AS DATE INIT TODAY     NO-UNDO.

DEF VAR v-page-num      AS INT                 NO-UNDO.
DEF VAR v-printline     AS INT                 NO-UNDO.

DEF VAR v-creamt        AS DEC  FORMAT "->>,>>9.99"    NO-UNDO.
DEF VAR v-debamt        AS DEC  FORMAT "->>,>>9.99"    NO-UNDO.
DEF VAR v-tcreamt       AS DEC  FORMAT "->>,>>9.99"    NO-UNDO.
DEF VAR v-tdebamt       AS DEC  FORMAT "->>,>>9.99"    NO-UNDO.
DEF VAR v2              AS DEC  FORMAT "->,>>>,>>9.99" NO-UNDO.
DEF VAR g1              AS DEC  FORMAT "->,>>>,>>9.99" NO-UNDO.
DEF VAR v1              AS DEC  FORMAT "->,>>>,>>9.99" NO-UNDO.
DEF VAR g2              AS DEC  FORMAT "->,>>>,>>9.99" NO-UNDO.

DEF VAR v-shipvia       LIKE carrier.dscr      NO-UNDO.
DEF VAR v-terms         LIKE ar-inv.terms-d    NO-UNDO.
DEF VAR lv-desc LIKE ar-cashl.dscr NO-UNDO.

/* gdm - 02200906 */
DEF VAR v-text       AS CHAR                          NO-UNDO.
DEF VAR v-notes      AS CHAR FORMAT "x(80)" EXTENT 5  NO-UNDO.
DEF VAR v-licnt      AS INT                           NO-UNDO. 
DEF VAR note-count   AS INT                           NO-UNDO. 

/* gdm - 04210922 logo */
DEF VAR ls-cust-img  AS CHAR                         NO-UNDO.
DEF VAR ls-image1    AS CHAR                         NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(50)"          NO-UNDO.
DEF VAR v-memo-name  AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-memo-addr  AS CHAR FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEF VAR v-memo-city  AS CHAR FORMAT "x(15)"          NO-UNDO.
DEF VAR v-memo-state AS CHAR FORMAT "x(2)"           NO-UNDO.
DEF VAR v-memo-zip   AS CHAR FORMAT "x(10)"          NO-UNDO.

ASSIGN 
    v-comp-add1  = ""
    v-comp-add2  = "" 
    v-comp-add3  = ""
    v-comp-add4  = ""
    v-comp-add5  = ""
    v-memo-name  = ""
    v-memo-addr  = ""
    v-memo-city  = ""
    v-memo-state = ""
    v-memo-zip   = ""
    .

ASSIGN
    ls-image1 = "images\allwest.jpg"
    FILE-INFO:FILE-NAME = ls-image1
    ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

FIND FIRST cust NO-LOCK
    WHERE cust.company = cocode 
      AND cust.active = "X"  NO-ERROR.

IF AVAIL cust
  THEN
   ASSIGN 
    v-comp-add1 = cust.addr[1]
    v-comp-add2 = cust.addr[2]
    v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
    v-comp-add4 = "Phone:  " + 
                   STRING(cust.area-code,"(999)") + 
                   STRING(cust.phone,"999-9999") 
    v-comp-add5 = "Fax     :  " + STRING(cust.fax,"(999)999-9999") 
    lv-email    = "Email:  " + cust.email 
    lv-comp-name = cust.NAME   
    . 


FOR EACH ar-cash
    WHERE ar-cash.company    EQ cocode
      AND ar-cash.posted     EQ v-tbpst
      AND ar-cash.memo       EQ yes
      AND ar-cash.cust-no    GE v-lo-cust
      AND ar-cash.cust-no    LE v-hi-cust
      AND ar-cash.check-no   GE v-lo-memo
      AND ar-cash.check-no   LE v-hi-memo
      AND ar-cash.check-date GE v-begdt
      AND ar-cash.check-date LE v-enddt
      AND CAN-FIND(FIRST ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no)
      AND ((v-reprint AND 
            CAN-FIND(FIRST reftable
                        WHERE reftable.reftable EQ  "AR-CASH"
                          AND reftable.code     EQ 
                              STRING(ar-cash.c-no,"9999999999")
                     use-index CODE)
           ) or
           (NOT v-reprint AND
            NOT CAN-FIND(FIRST reftable
                         WHERE reftable.reftable EQ "AR-CASH"
                           AND reftable.code     EQ 
                               STRING(ar-cash.c-no,"9999999999")
                         use-index CODE)
           ))
    USE-INDEX posted        
    BREAK BY ar-cash.cust-no
          BY ar-cash.check-no:

    FIND FIRST reftable 
        WHERE reftable.reftable = "ARCASHHOLD" 
          AND reftable.rec_key = ar-cash.rec_key 
        USE-INDEX rec_key NO-LOCK NO-ERROR.
    IF AVAIL reftable AND 
        reftable.CODE EQ "H" THEN DO:

        RELEASE reftable.
        NEXT.
    END.
    RELEASE reftable NO-ERROR.

    IF CAN-FIND(FIRST ar-cashl WHERE ar-cashl.company = cocode 
                               AND ar-cashl.c-no = ar-cash.c-no 
                               AND (ar-cashl.amt-paid + ar-cashl.amt-disc) < 0)
      THEN DO:
        CREATE report.
        ASSIGN
            report.term-id = v-term-id
            report.key-01  = STRING(ar-cash.check-no,"9999999999")
            report.rec-id  = RECID(ar-cash).
    END.

    ASSIGN v-memo-name  = "" 
           v-memo-addr  = "" 
           v-memo-city  = "" 
           v-memo-state = "" 
           v-memo-zip   = "" 
           v-date-ship  = ?
           v-fob        = ""
           v-shipvia    = ""
           v-terms      = ""
           v-s-man      = ""
           v-bol-no     = "".

    FOR EACH ar-cashl
       WHERE ar-cashl.company EQ cocode
         AND ar-cashl.c-no    EQ ar-cash.c-no
        BREAK BY ar-cashl.check-no
              BY ar-cashl.line:

        FIND FIRST ar-inv NO-LOCK
            WHERE ar-inv.company EQ ar-cashl.company
              AND ar-inv.cust-no EQ ar-cashl.cust-no
              AND ar-inv.inv-no  EQ ar-cashl.inv-no NO-ERROR.
        IF AVAIL ar-inv THEN DO:
            ASSIGN 
                v-memo-name    = ar-inv.cust-name
                v-memo-addr[1] = ar-inv.addr[1] 
                v-memo-addr[2] = ar-inv.addr[2]  
                v-memo-city    = ar-inv.city
                v-memo-state   = ar-inv.state
                v-memo-zip     = ar-inv.zip
                v-fob          = IF ar-inv.fob-code BEGINS "ORIG" 
                                  THEN "Origin" 
                                  ELSE "Destination"
                v-date-ship    = ar-inv.inv-date
                v-terms        = ar-inv.terms-d.

            FIND FIRST carrier NO-LOCK
                WHERE carrier.company EQ cocode
                AND carrier.carrier EQ ar-inv.carrier NO-ERROR.
            IF AVAIL carrier 
              THEN ASSIGN v-shipvia = carrier.dscr.
              ELSE ASSIGN v-shipvia = "".       

            IF ar-cashl.dscr BEGINS "CREDIT MEMO CREATED FROM OE RETURN" THEN
            DO:
               FIND FIRST oe-reth WHERE
                    oe-reth.company EQ ar-cashl.company AND
                    oe-reth.r-no    EQ INT(SUBSTRING(ar-cashl.dscr,51))
                    NO-LOCK NO-ERROR.

               IF AVAIL oe-reth THEN
               DO:
                  FIND FIRST oe-retl WHERE
                       oe-retl.company EQ oe-reth.company AND
                       oe-retl.r-no EQ oe-reth.r-no AND
                       oe-retl.LINE EQ ar-cashl.LINE
                       NO-LOCK no-error.

                  IF AVAIL oe-retl THEN
                  DO:
                     ASSIGN
                        v-ord-no = STRING(oe-retl.ord-no)
                        v-po-no  = ""
                        v-bol-no = ""
                        v-s-man  = "".

                     FIND FIRST ar-invl WHERE
                          ar-invl.company EQ oe-reth.company AND
                          ar-invl.x-no EQ ar-inv.x-no AND
                          ar-invl.i-no EQ oe-retl.i-no
                          NO-LOCK NO-ERROR.

                     IF AVAIL ar-invl THEN
                     DO:
                        ASSIGN
                           v-po-no  = STRING(ar-invl.po-no)
                           v-bol-no = STRING(ar-invl.bol-no,">>>>>>>9")
                           v-s-man  = IF ar-invl.sman[1] NE "" THEN
                                         ar-invl.sman[1]
                                      ELSE IF ar-invl.sman[2] NE "" THEN
                                           ar-invl.sman[2]
                                      ELSE ar-invl.sman[3].

                        RELEASE ar-invl.
                     END.

                     RELEASE oe-retl.
                  END.

                  RELEASE oe-reth.
               END.
            END.
            ELSE
            FOR EACH ar-invl NO-LOCK
                WHERE ar-invl.x-no EQ ar-inv.x-no
                  AND ar-invl.LINE EQ ar-cashl.LINE:

                ASSIGN v-po-no  = STRING(ar-invl.po-no)
                     v-ord-no = STRING(ar-invl.ord-no)
                     v-bol-no = STRING(ar-invl.bol-no,">>>>>>>9")
                     v-s-man  = IF ar-invl.sman[1] NE ""
                                  THEN ar-invl.sman[1]
                                  ELSE IF ar-invl.sman[2] NE ""
                                         THEN ar-invl.sman[2]
                                         ELSE ar-invl.sman[3].

            END.
        END.


        IF FIRST-OF(ar-cashl.check-no) 
          THEN DO:

            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ cocode
                  AND cust.cust-no EQ ar-cashl.cust-no NO-ERROR.

            IF v-s-man EQ "" AND
               AVAIL cust 
              THEN v-s-man  = cust.sman.

            IF v-memo-name    EQ "" AND
               v-memo-addr[1] EQ "" AND
               v-memo-addr[2] EQ "" AND
               v-memo-city    EQ "" AND
               v-memo-state   EQ "" AND
               v-memo-zip     EQ "" AND
               AVAIL cust 
              THEN
                ASSIGN v-memo-name   = cust.NAME 
                      v-memo-addr[1] = cust.addr[1]
                      v-memo-addr[2] = cust.addr[2]
                      v-memo-city    = cust.city 
                      v-memo-state   = cust.state 
                      v-memo-zip     = cust.zip.

            {ar/rep/AlWstMem.i}
        END.

        ASSIGN
            v-creamt = 0 v-debamt = 0
            v2      = v2 + ar-cashl.amt-paid - ar-cashl.amt-disc
            lv-desc = ar-cashl.dscr.

        IF v-tbpst THEN DO:

            IF lv-desc BEGINS "Credit -" 
              THEN lv-desc = SUBSTR(lv-desc,10).
              ELSE 
              IF lv-desc BEGINS "Debit -" 
                THEN lv-desc = SUBSTR(lv-desc,9).
        END.

        IF (ar-cashl.amt-paid - ar-cashl.amt-disc) LT 0
           THEN v-creamt = (ar-cashl.amt-paid - ar-cashl.amt-disc).
           ELSE v-debamt = (ar-cashl.amt-paid - ar-cashl.amt-disc).

        PUT SPACE(1)
            STRING(ar-cashl.inv-no) FORMAT "x(9)"       SPACE(1)
            v-po-no                 FORMAT "x(11)"      SPACE(1)
            v-ord-no                FORMAT "x(8)"       SPACE(1)
            lv-desc                 FORMAT "x(50)"      SPACE(3)
            IF v-creamt NE 0 
            THEN v-creamt 
            ELSE  v-debamt          FORMAT "->>,>>9.99" 
            SKIP(1).

        v-printline = v-printline + 1.

        /* gdm - 02200906 */
        FIND FIRST  ASI.notes NO-LOCK
            WHERE notes.rec_key = ar-cashl.rec_key
             AND TRIM(notes.note_text) NE "" NO-ERROR.
        IF AVAIL notes THEN DO:

            PUT "<B>Notes: </B>" SKIP(1).

            v-printline = v-printline + 1.



            FOR EACH ASI.notes NO-LOCK
                WHERE notes.rec_key = ar-cashl.rec_key
                BY note_date BY note_time:

                FOR EACH tt-formtext: DELETE tt-formtext. END.

                ASSIGN 
                    v-text = ""
                    v-text = v-text + " " + notes.note_text.

                DO v-licnt = 1 TO 5:
                    CREATE tt-formtext.
                    ASSIGN tt-line-no = v-licnt
                        tt-length  = 100. 
                END.
                RUN custom/formtext.p (v-text).

                ASSIGN 
                    i = 0 v-notes = "" note-count = 0.
                FOR EACH tt-formtext:
                    i = i + 1.
                    IF  i <= 5 THEN
                        v-notes[i] = tt-formtext.tt-text.
                    IF v-notes[i] <> "" THEN note-count = i.
                END.

                DO i = 1 TO note-count:

                    IF v-notes[i] NE "" THEN
                        PUT  v-notes[i] FORM "x(80)" SKIP.

                    v-printline = v-printline + 1.

                    IF v-printline > 54 THEN DO:
                        PAGE.
                        v-printline = 0.
                        {ar/rep/AlWstMem.i}
/*                         {ar/rep/crdbmemo2.i} */
                    END.
                END.
            END. /* For each notes */

            IF v-printline > 54 THEN DO:
                PAGE.
                v-printline = 0.
                {ar/rep/AlWstMem.i}
/*                 {ar/rep/crdbmemo2.i} */
            END.

            PUT SKIP(2).
        END.
        /* gdm - 02200906 end */       

    END. /* each ar-cashl */

    IF LAST-OF(ar-cash.check-no) THEN DO:
        IF v2 LT 0 
            THEN v-tcreamt = v2.
            ELSE v-tdebamt = v2.

        PUT 
            "<R58><C53><#8><FROM><R+3><C80><RECT> " 
            "<=8><R+.5><P10><B> Credit Memo Amount :</B>" v-tcreamt FORMAT "->>>,>>9.99"
            "<=8><R+1.5><P10><B>  Debit Memo Amount :</B>" v-tdebamt FORMAT "->>>,>>9.99".

        PAGE.

        v-printline = 0.
        ASSIGN 
            g1 = g1 + v1
            g2 = g2 + v2
            v1 = 0 v2 = 0
            v-tcreamt = 0 v-tdebamt = 0
            v-creamt = 0 v-debamt = 0.
    END.     

    FIND FIRST reftable
        WHERE reftable.reftable EQ "AR-CASH"
          AND reftable.code     EQ STRING(ar-cash.c-no,"9999999999")
        USE-INDEX CODE NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN 
          reftable.reftable = "AR-CASH"
          reftable.code     = STRING(ar-cash.c-no,"9999999999").
    END.

    /* gdm 07010903 */
    ASSIGN ar-cash.ret-memo = YES.
END. /* each ar-cash */

ASSIGN g2 = 0.

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.
