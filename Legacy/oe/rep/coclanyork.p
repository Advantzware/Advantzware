/* ---------------------------------------------- oe/rep/coclanyork.p */
/* Print Premiere COC (Certificate of Compliance)                     */
/* -------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-bol-recid AS RECID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{oe/rep/oe-lad.i}

DEFINE VARIABLE v-cust-addr3  AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE ls-image1     AS cha       NO-UNDO.
DEFINE VARIABLE v-manuf-date  AS DATE      INIT 12/31/2999 NO-UNDO.
DEFINE VARIABLE v-bol-qty     AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-type        AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rel-date    AS DATE      INIT 12/31/2999 NO-UNDO.
DEFINE VARIABLE v-part-desc   AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE v-cad         LIKE eb.cad-no NO-UNDO.
DEFINE VARIABLE v-style       LIKE eb.style NO-UNDO.
DEFINE VARIABLE v-flute       LIKE eb.flute NO-UNDO.
DEFINE VARIABLE v-test        LIKE eb.test NO-UNDO.
DEFINE VARIABLE ls-image-s    AS cha       NO-UNDO.
DEFINE VARIABLE cRtnChar      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-ship-name   LIKE shipto.ship-name NO-UNDO.
DEFINE VARIABLE v-ship-addr   LIKE shipto.ship-addr NO-UNDO.
DEFINE VARIABLE v-ship-city   LIKE shipto.ship-city NO-UNDO.
DEFINE VARIABLE v-ship-state  LIKE shipto.ship-state NO-UNDO.
DEFINE VARIABLE v-ship-zip    LIKE shipto.ship-zip NO-UNDO.
DEFINE VARIABLE v-ship-addr3  AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE ls-full-img-s AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE ls-full-img1  AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE v-job-no      AS CHARACTER INIT "" NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).

ASSIGN 
    ls-full-img1 = cRtnChar + ">" .
{sa/sa-sls01.i}
FOR EACH report WHERE report.term-id EQ v-term-id,
    FIRST oe-bolh WHERE RECID(oe-bolh) EQ report.rec-id NO-LOCK:

    IF ip-bol-recid NE ? AND
        ip-bol-recid NE report.rec-id THEN
        NEXT.

    FOR EACH oe-boll
        WHERE oe-boll.company EQ cocode
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.qty NE 0
        NO-LOCK:
   
        CREATE xreport.
        ASSIGN
            xreport.term-id = v-term-id
            xreport.key-01  = report.key-01
            xreport.key-02  = report.key-02
            xreport.key-03  = report.key-03
            xreport.key-04  = report.key-04
            xreport.key-06  = oe-boll.i-no
            xreport.rec-id  = RECID(oe-boll).
    END.
   
    DELETE report.
END.
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK,
    FIRST itemfg
    WHERE itemfg.company EQ cocode
    AND itemfg.i-no    EQ oe-boll.i-no
    NO-LOCK,
    FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no NO-LOCK,
    FIRST cust    WHERE cust.cust-no   EQ oe-bolh.cust-no NO-LOCK

    BREAK BY report.key-01
    BY report.key-02
    BY report.key-03
    BY report.key-04
    BY report.key-06 DESCENDING:

    v-bol-qty = v-bol-qty + oe-boll.qty.
   
    IF LAST-OF(report.key-06) THEN 
    DO:
      
        FIND FIRST oe-ordl WHERE
            oe-ordl.company EQ cocode AND
            oe-ordl.ord-no  EQ oe-boll.ord-no AND
            oe-ordl.i-no    EQ oe-boll.i-no AND
            oe-ordl.line    EQ oe-boll.line
            NO-LOCK NO-ERROR.

        RUN oe/custxship.p (oe-bolh.company,
            oe-bolh.cust-no,
            oe-bolh.ship-id,
            BUFFER shipto).

        ASSIGN
            v-ship-name    = shipto.ship-name
            v-ship-addr[1] = shipto.ship-addr[1]
            v-ship-addr[2] = shipto.ship-addr[2]
            v-ship-addr3   = shipto.ship-city + ", " +
                            shipto.ship-state + "  " +
                            shipto.ship-zip.
      
        v-cust-addr3 = cust.city + ", " +
            cust.state + "  " +
            cust.zip. 
      
        IF TRIM(v-cust-addr3) EQ "," THEN
            v-cust-addr3 = "".

        IF v-ship-addr[2] EQ "" THEN
            ASSIGN
                v-ship-addr[2] = v-ship-addr3
                v-ship-addr3   = "".

        IF TRIM(v-ship-addr3) EQ "," THEN v-ship-addr3 = "".
      
        {oe\rep\coclanyork.i}

        FOR EACH fg-rctd FIELDS(rct-date) WHERE
            fg-rctd.company EQ cocode AND
            fg-rctd.i-no EQ oe-ordl.i-no AND
            fg-rctd.job-no EQ oe-ordl.job-no AND
            fg-rctd.job-no2 EQ oe-ordl.job-no2
            NO-LOCK
            USE-INDEX i-no
            BY fg-rctd.rct-date:
      
            v-manuf-date = fg-rctd.rct-date.
            LEAVE.
        END.
      
        IF v-manuf-date EQ 12/31/2999 THEN
            FOR EACH fg-rctd WHERE
                fg-rctd.company EQ cocode AND
                fg-rctd.i-no EQ oe-ordl.i-no AND
                fg-rctd.po-no EQ STRING(oe-ordl.po-no-po) AND
                fg-rctd.rita-code EQ "R"
                NO-LOCK
                USE-INDEX i-no
                BY fg-rctd.rct-date:
      
                v-manuf-date = fg-rctd.rct-date.
                LEAVE.
            END.
         
        IF AVAILABLE oe-ordl THEN
        DO:
            FIND FIRST eb WHERE
                eb.company EQ cocode AND
                eb.est-no EQ oe-ordl.est-no AND
                eb.form-no EQ oe-ordl.form-no AND
                eb.blank-no EQ oe-ordl.blank-no
                NO-LOCK NO-ERROR.
      
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company   EQ oe-ordl.company
                AND oe-rel.ord-no    EQ oe-ordl.ord-no
                AND oe-rel.i-no      EQ oe-ordl.i-no
                AND oe-rel.line      EQ oe-ordl.line:
         
                RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).
      
                IF INDEX("A,B,P",v-type) > 0 THEN
                DO:
                    IF oe-rel.rel-date LT v-rel-date THEN
                        v-rel-date = oe-rel.rel-date.
                END.
            END.
        END.

        FIND FIRST eb WHERE
            eb.company EQ cocode AND
            eb.est-no EQ oe-ordl.est-no AND
            eb.form-no EQ oe-ordl.form-no AND
            eb.blank-no EQ oe-ordl.blank-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE eb AND eb.part-dscr1 NE "" THEN
            v-part-desc = eb.part-dscr1.
        ELSE IF AVAILABLE oe-ordl THEN
                v-part-desc = oe-ordl.i-name.

        IF AVAILABLE eb AND eb.cad-no NE "" THEN
            v-cad = eb.cad-no.
        ELSE
            v-cad = itemfg.cad-no.
        IF AVAILABLE eb THEN
            ASSIGN
                v-style = eb.style
                v-flute = eb.flute
                v-test  = eb.test .
        ELSE
            ASSIGN v-style = ""
                v-flute = ""
                v-test  = "" .
      
        IF AVAILABLE oe-ordl AND oe-ordl.ord-no NE 0 THEN
            ASSIGN 
                v-job-no = STRING(oe-ordl.ord-no).
        ELSE
            ASSIGN 
                v-job-no = "" .
        PUT "<FCourier New>"
            "<c11>Customer Name:" "<C25>" cust.NAME SKIP(1)
            "<c6>Customer PO  Number:" "<C25>" oe-boll.po-no  SKIP(1)
            "<c5>Customer Part Number:" "<C25>" itemfg.part-no  SKIP(1)
            "<C14>Item Name:" "<C25>" v-part-desc SKIP(1)
            "<c11.5>Order Number:" "<C25>" v-job-no SKIP(1)
            "<C8>Quantity Shipped:" "<C25>" TRIM(STRING(v-bol-qty,"->,>>>,>>9")) FORMAT "x(10)" SKIP(1)
            "<C7>Manufactured Date:" "<C25>" (IF v-manuf-date NE 12/31/2999 THEN
            STRING(v-manuf-date,"99/99/99")
            ELSE "") SKIP(1)
            "<C13.5>Ship Date:" "<C25>" (IF AVAILABLE oe-bolh AND oe-bolh.bol-date NE 12/31/2999 THEN 
            STRING(oe-bolh.bol-date,"99/99/99")
            ELSE "") SKIP(1).
        IF v-style NE "" OR v-test NE "" THEN
            PUT "<C12.5>Style/Test:" "<C25>" v-style "  " v-test "  " v-flute .
        PUT SKIP(1).
      
        PUT "<||5><C5><FROM><C80><LINE>" SKIP(1).
        PUT  
            "<C5>Authorized Signature: ________________________________________________________" SKIP(1)
            "<C5>               Title: ________________________________________________________" SKIP(1)
            "<C5>                Date: ____________________" SKIP(1).
              
        FIND FIRST users 
            WHERE users.user_id = USERID("nosweat")
            NO-LOCK NO-ERROR.
        ASSIGN
            v-bol-qty    = 0
            v-rel-date   = 12/31/2999
            v-manuf-date = 12/31/2999
            v-part-desc  = ""
            v-cad        = "".
        PAGE.
    END.
END.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
