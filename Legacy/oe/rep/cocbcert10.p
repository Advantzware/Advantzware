/* ---------------------------------------------- oe/rep/cocprempkg.p */
/* Print Premiere COC (Certificate of Compliance)                     */
/* -------------------------------------------------------------------*/

DEFINE INPUT PARAMETER ip-bol-recid AS RECID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{oe/rep/oe-lad.i}

def var v-cust-addr3 as char format "x(30)".
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR v-manuf-date AS DATE INIT 12/31/2999 NO-UNDO.
def var v-bol-qty    AS INTEGER NO-UNDO.
DEF VAR v-type AS CHAR NO-UNDO.
DEF VAR v-rel-date AS DATE INIT 12/31/2999 NO-UNDO.
DEF VAR v-part-desc AS CHAR FORMAT "X(30)" NO-UNDO.
DEF VAR v-cad LIKE eb.cad-no NO-UNDO.
DEF VAR v-style LIKE eb.style NO-UNDO.
DEF VAR v-flute LIKE eb.flute NO-UNDO.
DEF VAR v-test LIKE eb.test NO-UNDO.
DEF VAR ls-image-s AS cha NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
def var v-ship-name  like shipto.ship-name NO-UNDO.
def var v-ship-addr  like shipto.ship-addr NO-UNDO.
def var v-ship-city  like shipto.ship-city NO-UNDO.
def var v-ship-state like shipto.ship-state NO-UNDO.
def var v-ship-zip   like shipto.ship-zip NO-UNDO.
def var v-ship-addr3 as   char format "x(30)" NO-UNDO.
DEF VAR ls-full-img-s AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEF VAR ls-full-img1 AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEF VAR v-job-no AS CHAR INIT "" NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).

ASSIGN ls-full-img1 = cRtnChar + ">" .
 {sa/sa-sls01.i}
for each report where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id no-lock:

    IF ip-bol-recid NE ? AND
       ip-bol-recid NE report.rec-id THEN
       NEXT.

    for each oe-boll
        where oe-boll.company eq cocode
          and oe-boll.b-no    eq oe-bolh.b-no
          AND oe-boll.qty NE 0
        no-lock:
   
      create xreport.
      assign
       xreport.term-id = v-term-id
       xreport.key-01  = report.key-01
       xreport.key-02  = report.key-02
       xreport.key-03  = report.key-03
       xreport.key-04  = report.key-04
       xreport.key-06  = oe-boll.i-no
       xreport.rec-id  = recid(oe-boll).
    end.
   
    delete report.
end.
for each report where report.term-id eq v-term-id NO-LOCK,
    first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    no-lock,
    first oe-bolh where oe-bolh.b-no   eq oe-boll.b-no no-lock,
    first cust    where cust.cust-no   eq oe-bolh.cust-no no-lock

    break by report.key-01
          by report.key-02
          by report.key-03
          by report.key-04
          by report.key-06 DESC:

    v-bol-qty = v-bol-qty + oe-boll.qty.
   
    if LAST-OF(report.key-06) then do:
      
       find first oe-ordl WHERE
            oe-ordl.company eq cocode AND
            oe-ordl.ord-no  eq oe-boll.ord-no AND
            oe-ordl.i-no    eq oe-boll.i-no AND
            oe-ordl.line    eq oe-boll.line
            no-lock no-error.

       RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

       assign
           v-ship-name    = shipto.ship-name
           v-ship-addr[1] = shipto.ship-addr[1]
           v-ship-addr[2] = shipto.ship-addr[2]
           v-ship-addr3   = shipto.ship-city + ", " +
                            shipto.ship-state + "  " +
                            shipto.ship-zip.
      
       v-cust-addr3 = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip. 
      
       if trim(v-cust-addr3) eq "," then
          v-cust-addr3 = "".

       if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
      
       {oe\rep\cocbcert10.i}

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
         
       IF AVAIL oe-ordl THEN
       DO:
          FIND FIRST eb WHERE
               eb.company EQ cocode AND
               eb.est-no EQ oe-ordl.est-no AND
               eb.form-no EQ oe-ordl.form-no AND
               eb.blank-no EQ oe-ordl.blank-no
               NO-LOCK NO-ERROR.
      
          for each oe-rel no-lock
              where oe-rel.company   eq oe-ordl.company
                and oe-rel.ord-no    eq oe-ordl.ord-no
                and oe-rel.i-no      eq oe-ordl.i-no
                and oe-rel.line      eq oe-ordl.line:
         
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).
      
              IF index("A,B,P",v-type) > 0 THEN
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

       IF AVAIL eb AND eb.part-dscr1 NE "" THEN
          v-part-desc = eb.part-dscr1.
       ELSE IF AVAIL oe-ordl THEN
          v-part-desc = oe-ordl.i-name.

       IF AVAIL eb AND eb.cad-no NE "" THEN
          v-cad = eb.cad-no.
       ELSE
          v-cad = itemfg.cad-no.
      IF AVAIL eb THEN
          ASSIGN
          v-style = eb.style
          v-flute = eb.flute
          v-test  = eb.test .
      ELSE
          ASSIGN v-style = ""
                 v-flute = ""
                 v-test  = "" .
      
          IF AVAIL oe-ordl AND oe-ordl.ord-no NE 0 THEN
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
           "<C8>Quantity Shipped:" "<C25>"  v-bol-qty FORMAT "->>,>>>,>>9" SKIP(1)
           "<C7>Manufactured Date:" "<C25>" (IF v-manuf-date NE 12/31/2999 THEN
                                                     STRING(v-manuf-date,"99/99/99")
                                                  ELSE "") SKIP(1)
           "<C13.5>Ship Date:" "<C25>" (IF AVAIL oe-bolh AND oe-bolh.bol-date NE 12/31/2999 THEN
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
          v-bol-qty = 0
          v-rel-date = 12/31/2999
          v-manuf-date = 12/31/2999
          v-part-desc = ""
          v-cad = "".
       PAGE.
    end.
END.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
