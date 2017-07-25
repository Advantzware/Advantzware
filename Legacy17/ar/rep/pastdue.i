/* ----------------------------------------------- ar/rep/pastdue.i 11/02 JLF */
/* A/R Past Due Receivables Report Program - A/R Module                       */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ar/rep/pastdue1.i}
{sys/ref/CustList.i}
DEFINE SHARED VARIABLE det-rpt2     AS LOG       NO-UNDO.
DEFINE        VARIABLE v-cr-db-amt  AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
DEFINE        VARIABLE v-disc-amt   AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
DEFINE        VARIABLE v-type       AS CHARACTER FORMAT "x(2)".
DEFINE        VARIABLE v-first-cust AS LOGICAL.
DEFINE        VARIABLE d            AS INTEGER   LABEL "Days".
DEFINE        VARIABLE ni           AS INTEGER.
DEFINE        VARIABLE cust-t       AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99".
DEFINE        VARIABLE sman-t       AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99".
DEFINE        VARIABLE onacc        AS DECIMAL.
DEFINE        VARIABLE s            AS INTEGER.
DEFINE        VARIABLE ag           AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
DEFINE        VARIABLE amt          LIKE ag.
DEFINE        VARIABLE paid-amt     LIKE ag.
DEFINE        VARIABLE c1           AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99".
DEFINE        VARIABLE m1           AS CHARACTER FORMAT "x(20)".
DEFINE        VARIABLE m2           AS CHARACTER FORMAT "x(20)".
DEFINE        VARIABLE m3           AS CHARACTER FORMAT "x(20)".
DEFINE        VARIABLE save_id      AS RECID.
DEFINE        VARIABLE unapp        LIKE cust-t.
DEFINE        VARIABLE first-unapp  AS LOG       INIT YES.
DEFINE        VARIABLE tmp-var      AS CHARACTER FORMAT "x(20)".  /* DAR */
DEFINE        VARIABLE v-disc-type  AS CHARACTER FORMAT "x(4)".
DEFINE        VARIABLE v-sman       AS CHARACTER FORMAT "x(21)".
DEFINE        VARIABLE v-dec        AS DECIMAL   EXTENT 4 NO-UNDO.
DEFINE        VARIABLE v-cr-lim     LIKE cust.cr-lim NO-UNDO.
DEFINE SHARED VARIABLE lSelected    AS LOG       INIT YES NO-UNDO.
/* DEFINE SHARED FRAME r-top. */

{sys/form/r-top3.f}


FOR EACH cust
    WHERE cust.company EQ cocode
    AND cust.cust-no GE v-s-cust
    AND cust.cust-no LE v-e-cust
    AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
    AND ttCustList.log-fld NO-LOCK) ELSE TRUE)  /*v-s-cust*/
    /* and cust.cust-no le v-e-cust*/
    AND cust.sman    GE v-s-sman
    AND cust.sman    LE v-e-sman
    NO-LOCK
      
    BREAK BY {&sort-by}:

    {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}
      
    FIND FIRST sman
        WHERE sman.company EQ cocode
        AND sman.sman    EQ cust.sman
        NO-LOCK NO-ERROR.
    v-sman = cust.sman + "-" + (IF AVAILABLE sman THEN sman.sname
    ELSE "Slsmn not on file").
      
    IF FIRST-OF({&sort-by})        AND
        NOT first ({&sort-by})      AND
        "{&sort-by}" EQ "cust.sman" THEN PAGE.

    v-first-cust = YES.

    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company     EQ cust.company
        AND ar-inv.posted      EQ YES
        AND ar-inv.cust-no     EQ cust.cust-no
        AND ((ar-inv.inv-date  LE v-date - v-days[1] AND ll-date) OR
        (ar-inv.due-date  LE v-date - v-days[1] AND NOT ll-date))
        AND ar-inv.terms       NE "CASH"
        BY ar-inv.{&sort-by2} BY ar-inv.inv-no:

        /* task 09200521 include factored fg items*/
        IF NOT v-include-factored THEN 
        DO:
            FIND FIRST ar-invl NO-LOCK
                WHERE ar-invl.x-no EQ ar-inv.x-no
                AND CAN-FIND(FIRST reftable
                WHERE reftable.reftable EQ "FACTORED"
                AND reftable.company  EQ ar-inv.company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ ar-invl.i-no
                AND reftable.code2    EQ "YES")
                NO-ERROR.
            IF AVAILABLE ar-invl THEN NEXT.
        END.

        /* Inserted because AR stores gross wrong */
        IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
            amt = ar-inv.net.
        ELSE
            amt = ar-inv.gross.

        ASSIGN
            ag     = amt
            d      = v-date - ar-inv.{&date}
            ni     = ni + 1
            v-type = IF ar-inv.terms EQ "FCHG" THEN "FC" ELSE "IN".

        FOR EACH ar-cashl
            WHERE ar-cashl.company  EQ ar-inv.company
            AND ar-cashl.posted   EQ YES
            AND ar-cashl.cust-no  EQ ar-inv.cust-no
            AND ar-cashl.inv-no   EQ ar-inv.inv-no
            USE-INDEX inv-no NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-date
            USE-INDEX c-no NO-LOCK:

            IF ar-cashl.memo THEN
                /*
                          if ar-cashl.dscr begins "CREDIT MEMO CREATED FROM OE RETURN" then
                */
                IF ar-cashl.amt-disc NE 0 THEN
                    ag = ag - ar-cashl.amt-disc.
                ELSE
                    IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                        ag = ag + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                    ELSE
                        ag = ag + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
            ELSE
                ag = ag + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
        END.

        IF ag GT 0 THEN 
        DO:
            IF v-first-cust THEN 
            DO:
                ASSIGN 
                    paid-amt = 0  
                    m3       = ""  
                    ni       = 0.
                IF cust.area-code NE "" THEN
                    m3 = STRING(cust.area-code,"(999) ").

                ASSIGN
                    m3       = m3 + string(cust.phone,"999-9999")
                    v-cr-lim = cust.cr-lim.

                IF v-cr-lim GT 9999999.99 THEN v-cr-lim = 9999999.99.

                FIND FIRST terms WHERE terms.company = cocode AND
                    terms.t-code = cust.terms NO-LOCK NO-ERROR.

                IF det-rpt2 THEN 
                    DISPLAY cust.cust-no
                        SPACE(3)
                        cust.name
                        SPACE(3)
                        cust.area-code                           FORMAT "(xxx)"
                        cust.phone                               FORMAT "xxx-xxxx"
                        "  Fax:"
                        substr(cust.fax,1,3)                     FORMAT "(xxx)"
                        substr(cust.fax,4,7)                     FORMAT "xxx-xxxx"
                        SKIP
                        "CL:"
                        TRIM(STRING(v-cr-lim,">,>>>,>>>,>>9.99")) FORMAT "x(17)"
                        cust.contact                             FORMAT "x(20)"
                        SPACE(2)
                        v-sman
                        SPACE(2)
                        terms.dscr WHEN AVAILABLE terms              FORMAT "x(13)"
                        WITH NO-LABELS NO-BOX FRAME a1 STREAM-IO WIDTH 80.
              
                IF v-prt-add THEN RUN print-cust-add.

                v-first-cust = NO.
            END.

            IF det-rpt2 THEN
                DISPLAY d AT 4 FORMAT "-9999" WHEN v-days-old 
                    SPACE(7) "IN" SPACE(5) ar-inv.inv-no
                    SPACE(2) ar-inv.inv-date FORMAT "99/99/99"
                    amt TO 54 ag TO 77
                    WITH FRAME c NO-LABELS NO-BOX STREAM-IO WIDTH 80.

            ASSIGN
                cust-t[1] = cust-t[1] + ag
                v-dec     = 0
                v-dec[1]  = ag.
       
            IF v-export THEN
                RUN export-data ("", d, "IN", STRING(ar-inv.inv-no,">>>>>>>>>>"),
                    ar-inv.inv-date, amt, v-dec[1]).

            FOR EACH ar-cashl
                WHERE ar-cashl.company  EQ ar-inv.company
                AND ar-cashl.posted   EQ YES
                AND ar-cashl.cust-no  EQ ar-inv.cust-no
                AND ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no NO-LOCK,

                EACH ar-cash
                WHERE ar-cash.c-no       EQ ar-cashl.c-no
                AND ar-cash.check-date LE v-date
                USE-INDEX c-no NO-LOCK:

                IF ar-cashl.memo THEN

                    /* CTS CM/DM signs are reversed *****************************/
                    /*if (ar-cashl.amt-paid + ar-cashl.amt-disc) lt 0 then
                       assign v-type = "CM"
                              v-cr-db-amt = ar-cashl.amt-paid
                              v-disc-amt = ar-cashl.amt-disc.
         
                    else*/
                    IF (ar-cashl.amt-paid + ar-cashl.amt-disc) GT 0 THEN
                        ASSIGN v-type      = "DM"
                            v-cr-db-amt = ar-cashl.amt-paid
                            v-disc-amt  = ar-cashl.amt-disc.

                    ELSE
                        ASSIGN v-type      = "CM"
                            v-cr-db-amt = ar-cashl.amt-paid
                            v-disc-amt  = - (ar-cashl.amt-disc).

                ELSE
                    ASSIGN v-type      = "PY"
                        v-cr-db-amt = (ar-cashl.amt-paid) * -1
                        v-disc-amt  = ar-cashl.amt-disc * -1.

                IF v-disc-amt NE 0 THEN 
                DO:

                    v-disc-type = "DISC".
                    /*
                              if ar-cashl.dscr begins "CREDIT MEMO CREATED FROM OE RETURN" then
                    */
                    IF ar-cashl.memo THEN
                        ASSIGN
                            v-disc-type = "RETN"
                            v-disc-amt  = - v-disc-amt.

                    IF det-rpt2 THEN 
                    DO:
                        IF v-disc-type EQ "DISC" THEN 
                        DO:
                            DISPLAY ar-cashl.check-no AT 4 FORMAT "x(10)" 
                                WHEN NOT v-days-old 
                                v-type AT 16
                                ar-cashl.inv-no AT 23
                                ar-cash.check-date AT 31 FORMAT "99/99/99"
                                v-cr-db-amt TO 54 SKIP
                                WITH FRAME f-1 NO-BOX NO-LABELS STREAM-IO WIDTH 80.
                  
                            IF v-export THEN
                                RUN export-data (ar-cashl.check-no, 0, v-type,
                                    STRING(ar-cashl.inv-no,">>>>>>>>>>"),
                                    ar-cash.check-date, v-cr-db-amt, 0).
                        END.

                        DISPLAY ar-cashl.check-no AT 4 FORMAT "x(10)" 
                            WHEN NOT v-days-old 
                            v-disc-type AT 16
                            ar-cashl.inv-no AT 23
                            ar-cash.check-date AT 31 FORMAT "99/99/99"
                            v-disc-amt TO 54
                            WITH FRAME f-50{&frame} NO-BOX NO-LABELS STREAM-IO WIDTH 80.
                
                        IF v-export THEN
                            RUN export-data (ar-cashl.check-no, 0, v-disc-type,
                                STRING(ar-cashl.inv-no,">>>>>>>>>>"),
                                ar-cash.check-date, v-disc-amt, 0).
                    END.
                END.

                ELSE
                    IF det-rpt2 THEN 
                    DO:
                        DISPLAY ar-cashl.check-no AT 4 FORMAT "x(10)" 
                            WHEN NOT v-days-old 
                            v-type AT 16
                            ar-cashl.inv-no AT 23
                            ar-cash.check-date AT 31 FORMAT "99/99/99"
                            v-cr-db-amt TO 54
                            WITH FRAME f-100 NO-BOX NO-LABELS STREAM-IO WIDTH 80.
              
                        IF v-export THEN
                            RUN export-data (ar-cashl.check-no, 0, v-type,
                                STRING(ar-cashl.inv-no,">>>>>>>>>>"),
                                ar-cash.check-date, v-cr-db-amt, 0).
                    END.
            END. /* for each ar-cashl record */
        END.
    END. /* for each ar-inv record */

    ASSIGN 
        unapp[1] = 0.

    /* This loop finds all unapplied balances and totals by age */
    IF cust-t[1] NE 0 THEN
        FOR EACH ar-cash
            WHERE ar-cash.company     EQ cust.company
            AND ar-cash.cust-no     EQ cust.cust-no
            AND (ar-cash.check-date LE v-date OR
            ar-cash.check-date EQ ?)
            AND ar-cash.posted      EQ YES
            USE-INDEX ar-cash NO-LOCK,

            EACH ar-cashl
            WHERE ar-cashl.c-no       EQ ar-cash.c-no
            AND ar-cashl.posted     EQ YES
            USE-INDEX c-no NO-LOCK:

            IF ar-cashl.inv-no NE 0 THEN 
            DO:
                FIND FIRST ar-inv
                    WHERE ar-inv.company     EQ cust.company
                    AND ar-inv.inv-no      EQ ar-cashl.inv-no
                    AND ar-inv.inv-date    GT v-date
                    USE-INDEX inv-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ar-inv THEN NEXT.
            END.

            IF ar-cashl.memo THEN 
            DO:

                /* CTS CM/DM signs are reversed *****************************/
                IF (ar-cashl.amt-paid + ar-cashl.amt-disc) GT 0 THEN
                    ASSIGN v-type      = "DM"
                        v-cr-db-amt = ar-cashl.amt-paid
                        v-disc-amt  = ar-cashl.amt-disc.

                ELSE
                    ASSIGN v-type      = "CM"
                        v-cr-db-amt = ar-cashl.amt-paid
                        v-disc-amt  = ar-cashl.amt-disc.
            END.

            ELSE
                ASSIGN v-cr-db-amt = ar-cashl.amt-paid * -1
                    v-disc-amt  = ar-cashl.amt-disc * -1.

            unapp[1] = unapp[1] + v-cr-db-amt - v-disc-amt.
        END. /* for each ar-cashl record */

    first-unapp = YES.
    /* this loop displays all unapplied balances */
    
    IF unapp[1] NE 0 THEN
        FOR EACH ar-cash
            WHERE ar-cash.company     EQ cust.company
            AND ar-cash.cust-no     EQ cust.cust-no
            AND (ar-cash.check-date LE v-date OR
            ar-cash.check-date EQ ?)
            AND ar-cash.posted      EQ YES
            USE-INDEX ar-cash NO-LOCK,

            EACH ar-cashl
            WHERE ar-cashl.c-no       EQ ar-cash.c-no
            AND ar-cashl.posted     EQ YES
            USE-INDEX c-no NO-LOCK:

            IF ar-cashl.inv-no NE 0 THEN 
            DO:
                FIND FIRST ar-inv
                    WHERE ar-inv.company     EQ cust.company
                    AND ar-inv.inv-no      EQ ar-cashl.inv-no
                    AND ar-inv.inv-date    GT v-date
                    USE-INDEX inv-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ar-inv THEN NEXT.
            END.

            IF v-first-cust THEN 
            DO:
                ASSIGN 
                    paid-amt = 0  
                    cust-t   = 0  
                    m3       = ""  
                    ni       = 0.
                IF cust.area-code NE "" THEN
                    m3 = STRING(cust.area-code,"(999) ").

                m3 = m3 + string(cust.phone,"999-9999").

                FIND FIRST terms WHERE terms.company = cocode AND
                    terms.t-code = cust.terms NO-LOCK NO-ERROR.

                IF det-rpt2 THEN
                    DISPLAY cust.cust-no
                        cust.name
                        cust.area-code                           FORMAT "(xxx)"
                        cust.phone                               FORMAT "xxx-xxxx"
                        "Fax:"
                        substr(cust.fax,1,3)                     FORMAT "(xxx)"
                        substr(cust.fax,4,7)                     FORMAT "xxx-xxxx"
                        SKIP
                        "CL:"
                        TRIM(STRING(v-cr-lim,">,>>>,>>>,>>9.99")) FORMAT "x(17)"
                        cust.contact                             FORMAT "x(20)"
                        SPACE(2)
                        v-sman
                        SPACE(2)
                        terms.dscr WHEN AVAILABLE terms              FORMAT "x(13)"
                        WITH NO-LABELS NO-BOX FRAME a2 STREAM-IO WIDTH 80.
              
                IF v-prt-add THEN RUN print-cust-add.
        
                ASSIGN 
                    v-first-cust = NO.
            END.

            IF ar-cashl.memo EQ TRUE THEN 
            DO:
                IF (ar-cashl.amt-paid + ar-cashl.amt-disc) LT 0 THEN
                    ASSIGN v-type      = "CM"
                        v-cr-db-amt = ar-cashl.amt-paid
                        v-disc-amt  = ar-cashl.amt-disc.
                ELSE
                    IF (ar-cashl.amt-paid + ar-cashl.amt-disc) GT 0 THEN
                        ASSIGN v-type      = "DM"
                            v-cr-db-amt = ar-cashl.amt-paid
                            v-disc-amt  = ar-cashl.amt-disc.
            END.

            ELSE
                ASSIGN v-type      = "PY"
                    v-cr-db-amt = ar-cashl.amt-paid * -1
                    v-disc-amt  = ar-cashl.amt-disc * -1.

            IF first-unapp THEN 
            DO:

                IF det-rpt2 THEN
                    DISPLAY SKIP(1)
                        ar-cashl.check-no AT 4 FORMAT "x(10)" WHEN NOT v-days-old 
                        v-type AT 16
                        "ON ACCT" AT 23
                        ar-cash.check-date AT 31 FORMAT "99/99/99"
                   (v-cr-db-amt + v-disc-amt)
                         format "->>>,>>>,>>9.99" to 54
                   unapp[1] when unapp[1] ne 0 to 77
               with frame ab no-labels no-box STREAM-IO width 80.
               
                IF v-export THEN
                    RUN export-data (ar-cashl.check-no, 0, v-type, "ON ACCT",
                        ar-cash.check-date, v-cr-db-amt + v-disc-amt,
                        unapp[1]).

                cust-t[1] = cust-t[1] + unapp[1].
            END.

            IF first-unapp THEN first-unapp = NO.

            ELSE 
            DO:
                IF det-rpt2 THEN
                    DISPLAY ar-cashl.check-no AT 4 FORMAT "x(10)" WHEN NOT v-days-old 
                        v-type AT 16
                        "ON ACCT" AT 23
                        ar-cash.check-date AT 31 FORMAT "99/99/99"
                  (v-cr-db-amt + v-disc-amt)
                           format "->>>,>>>,>>9.99" to 54
              with frame f-2 no-box no-labels STREAM-IO width 80.
              
                IF v-export THEN
                    RUN export-data (ar-cashl.check-no, 0, v-type, "ON ACCT",
                        ar-cash.check-date, v-cr-db-amt + v-disc-amt, 0).
            END.
        END. /* for each ar-cashl record */

    c1 = cust-t[1].

    IF (NOT v-first-cust) OR c1 NE 0 THEN 
    DO:
        IF det-rpt2 THEN 
        DO:
            DISPLAY SKIP(1) "***** CUSTOMER TOTALS" AT 4 c1 TO 54 cust-t[1] TO 77
                SKIP(1)
                WITH FRAME a3 NO-LABELS NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 80.
        
            IF NOT LAST-OF({&sort-by}) OR "{&sort-by}" NE "cust.sman" THEN
                PUT SKIP(1).
        END.
      
        ELSE
            DISPLAY cust.cust-no SPACE(2) cust.name + "  " + m3 FORMAT "x(50)" SKIP
                c1        TO 54
                cust-t[1] TO 77
                SKIP(1)
                WITH FRAME a3sum NO-LABELS NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 80.
            
        ASSIGN
            sman-t[1] = sman-t[1] + cust-t[1]
            cust-t[1] = 0.
    END.
    
    IF LAST-OF({&sort-by}) THEN 
    DO:
        c1 = sman-t[1].
          
        IF "{&sort-by}" EQ "cust.sman" THEN
            DISPLAY v-sman                  AT 4    FORMAT "x(33)"
                "TOTALS: " + v-sman                  @ v-sman
                "***** SALESMAN TOTALS" WHEN det-rpt2 @ v-sman
                c1                      TO 54
                sman-t[1]               TO 77
                SKIP(2)
                WITH FRAME slsmn NO-LABELS NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 80.

        ASSIGN
            grand-t[1] = grand-t[1] + sman-t[1]
            sman-t[1]  = 0.
    END.
    
    m3 = "".
    IF ni EQ 1 THEN m3 = m2.
    ASSIGN
        v-cr-db-amt = 0
        v-disc-amt  = 0.
END.  /* for each cust record */
  
RETURN.
  
PROCEDURE print-cust-add:
    DISPLAY cust.addr[1]                                                SKIP
        cust.addr[2]                                                SKIP
        TRIM(cust.city) + ", " +
        trim(cust.state) + "  " + trim(cust.zip) FORMAT "x(50)"
            
        WITH NO-LABELS NO-BOX FRAME cust-detail STREAM-IO WIDTH 80.
END.
  
PROCEDURE export-data:
    DEFINE INPUT PARAMETER v-field-01 LIKE ar-cashl.check-no NO-UNDO.
    DEFINE INPUT PARAMETER v-field-02 LIKE d                 NO-UNDO.
    DEFINE INPUT PARAMETER v-field-03 LIKE v-type            NO-UNDO.
    DEFINE INPUT PARAMETER v-field-04 AS   CHARACTER              NO-UNDO.
    DEFINE INPUT PARAMETER v-field-05 LIKE ar-inv.inv-date   NO-UNDO.
    DEFINE INPUT PARAMETER v-field-06 LIKE amt               NO-UNDO.
    DEFINE INPUT PARAMETER v-field-07 LIKE ag                NO-UNDO.
    
    
    /*put stream s-temp unformatted
        trim(cust.cust-no)                                      + "," +
        trim(cust.name)                                         + "," +
        trim(cust.contact)                                      + "," +
        trim(v-sman)                                            + "," +
        trim(if avail terms then terms.dscr else "")            + "," +
        trim(cust.addr[1])                                      + "," +
        trim(cust.addr[2])                                      + "," +
        trim(cust.city)                                         + "," +
        trim(cust.state)                                        + "," +
        trim(cust.zip)                                          + "," +
        trim(string(cust.cr-lim,">>>>>>>>9.99"))                + "," +
        trim(string(cust.area-code,"(xxx)") + " " +
             string(cust.phone,"xxx-xxxx"))                     + "," +
        trim(string(substr(cust.fax,1,3),"(xxx)") + " " +
             string(substr(cust.fax,4,7),"xxx-xxxx"))           + "," +
        trim(v-field-01)                                        + "," +
        trim(string(v-field-02,"->>>>"))                        + "," +
        trim(v-field-03)                                        + "," +
        trim(v-field-04)                                        + "," +
        trim(string(v-field-05,"99/99/9999"))                   + "," +
        trim(string(v-field-06,"->>>>>>>>9.99"))                + "," +
        trim(string(v-field-07,"->>>>>>>>9.99"))
        skip.*/
    EXPORT STREAM s-temp DELIMITER ","
        TRIM(cust.cust-no)                               
        TRIM(cust.name)                                  
        TRIM(cust.contact)                               
        TRIM(v-sman)                                     
        TRIM(IF AVAILABLE terms THEN terms.dscr ELSE "")     
        TRIM(cust.addr[1])                               
        TRIM(cust.addr[2])                               
        TRIM(cust.city)                                  
        TRIM(cust.state)                                 
        TRIM(cust.zip)                                   
        TRIM(STRING(cust.cr-lim,">>>>>>>>9.99"))         
        TRIM(STRING(cust.area-code,"(xxx)") + " " +      
        string(cust.phone,"xxx-xxxx"))              
        TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + " " +
        string(substr(cust.fax,4,7),"xxx-xxxx"))    
        TRIM(v-field-01)                                 
        TRIM(STRING(v-field-02,"->>>>"))                 
        TRIM(v-field-03)                                 
        TRIM(v-field-04)                                 
        TRIM(STRING(v-field-05,"99/99/9999"))            
        TRIM(STRING(v-field-06,"->>>>>>>>9.99"))         
        TRIM(STRING(v-field-07,"->>>>>>>>9.99"))  
        SKIP.

END.
  
/* End ---------------------------------- Copr. 2002  Advanced Software, Inc. */
