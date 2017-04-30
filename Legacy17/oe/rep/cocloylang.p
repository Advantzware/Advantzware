/* ---------------------------------------------- oe/rep/cocprempkg.p */
/* Print Premiere COC (Certificate of Compliance)                     */
/* -------------------------------------------------------------------*/

DEFINE INPUT PARAMETER ip-bol-recid AS RECID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{oe/rep/oe-lad.i}

def var v-cust-addr3 as char format "x(30)".
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR v-manuf-date AS DATE INIT 12/31/2999 NO-UNDO.
def var v-bol-qty    like oe-boll.qty NO-UNDO.
DEF VAR v-type AS CHAR NO-UNDO.
DEF VAR v-rel-date AS DATE INIT 12/31/2999 NO-UNDO.
DEF VAR v-part-desc AS CHAR FORMAT "X(30)" NO-UNDO.
DEF VAR v-cad LIKE eb.cad-no NO-UNDO.
DEF VAR v-style LIKE eb.style NO-UNDO.
DEF VAR v-flute LIKE eb.flute NO-UNDO.
DEF VAR v-test LIKE eb.test NO-UNDO.
DEF VAR ls-image-s AS cha NO-UNDO.
DEF VAR ls-full-img-s AS cha FORM "x(100)" NO-UNDO.
DEF VAR v-job-no AS CHAR INIT "" NO-UNDO.


ASSIGN
   ls-image1 = "images\Loylang.jpg"
   FILE-INFO:FILE-NAME = ls-image1
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
  
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
      
       v-cust-addr3 = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip. 
      
       if trim(v-cust-addr3) eq "," then
          v-cust-addr3 = "".
      
       {oe\rep\cocloylang.i}

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
      
          IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
              ASSIGN 
              v-job-no = (STRING(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99")) .
          ELSE
              ASSIGN 
              v-job-no = "" .

       PUT UNFORMATTED
           "<c5>Customer Name :           " "<C20>" cust.NAME SKIP(1)
           "<c1>Customer PO  Number :           " "<C20>" oe-boll.po-no  SKIP(1)
           "Customer Part Number :          " "<C20>" itemfg.part-no /*(IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE "")*/ SKIP(1)
           "<C8> Item Name :" "<C20>" v-part-desc SKIP(1)
          /* "Drawing Number:                    " v-cad SKIP(1)*/
          /* "Customer Specification/Drawing No./" SKIP*/
          /* "Revision No:"                        SKIP(1)*/
           " Factory Order Number :            " "<C20>" v-job-no SKIP(1)
           /*"Vendor Manufacturing Batch Number:" SKIP(1)*/
           "<C4>  Quantity Shipped :            " "<C20>"  STRING(v-bol-qty) SKIP(1)
           "<C2>    Manufactured Date :              " "<C20>" (IF v-manuf-date NE 12/31/2999 THEN
                                                     STRING(v-manuf-date,"99/99/9999")
                                                  ELSE "") SKIP(1)
        /*   "<C9> Ship Date :            " "<C20>" (IF v-rel-date NE 12/31/2999 THEN
                                                     STRING(v-rel-date,"99/99/9999")
                                                  ELSE "") SKIP(1) */
           "<C9> Ship Date :            " "<C20>" (IF AVAIL oe-bolh AND oe-bolh.bol-date NE 12/31/2999 THEN
                                                     STRING(oe-bolh.bol-date,"99/99/9999")
                                                  ELSE "") SKIP(1)
           "<C9> Style/Test :            " "<C20>"    v-style "  " v-test "  " v-flute   SKIP(3).

    
              ASSIGN
                  ls-image-s = "signature\" + USERID("nosweat") + ".jpg" 
                  FILE-INFO:FILE-NAME = ls-image-s
                  ls-full-img-s = FILE-INFO:FULL-PATHNAME + ">".

      PUT UNFORMATTED  
           "Approved By: <R-2> "  "<C10><#1><R+4><C+30><IMAGE#1=" ls-full-img-s  SKIP
           "<||5><C10><R-1><FROM><C30><LINE>" SKIP.
      FIND FIRST users 
          WHERE users.user_id = USERID("nosweat")
          NO-LOCK NO-ERROR.
      IF AVAIL users THEN 
          PUT UNFORMATTED
           "<C11><R-.5>" users.user_name SKIP.
      PUT UNFORMATTED
           "<C45><R-3.5>Date: " STRING(TODAY,"99/99/9999") .

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
