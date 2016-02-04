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
DEF VAR v-jobs AS CHAR NO-UNDO EXTENT 10.
DEF VAR vi-jobcnt AS INT NO-UNDO.
DEF VAR v-spc LIKE eb.spc-no NO-UNDO.

ASSIGN
   ls-image1 = "images\Premiercx.jpg"
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
vi-jobcnt = 0.
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
    
    vi-jobcnt = vi-jobcnt + 1.
    IF vi-jobcnt LE 10 THEN DO:
        IF oe-boll.job-no NE "" THEN
            ASSIGN 
                v-jobs[vi-jobcnt] = trim(oe-boll.job-no + "-" + string(oe-boll.job-no2,"99")).
        FIND FIRST fg-rdtlh WHERE
               fg-rdtlh.company EQ "001" AND
               fg-rdtlh.tag EQ oe-boll.tag AND
               fg-rdtlh.rita-code EQ "R" NO-LOCK NO-ERROR.
        IF AVAIL fg-rdtlh THEN 
            FIND FIRST fg-rcpth WHERE fg-rcpth.r-no = fg-rdtlh.r-no
                NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth THEN 
            v-jobs[vi-jobcnt] = v-jobs[vi-jobcnt] + "<C30>" + STRING(fg-rcpth.trans-date,"99/99/9999").
    END.
    
    v-bol-qty = v-bol-qty + oe-boll.qty.
   
    if LAST-OF(report.key-06) then do:
       vi-jobcnt = 0. 
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
      
       {oe\rep\cocprempkg.i}

/*        FOR EACH fg-rctd FIELDS(rct-date) WHERE  */
/*            fg-rctd.company EQ cocode AND        */
/*            fg-rctd.i-no EQ oe-ordl.i-no AND     */
/*            fg-rctd.job-no EQ oe-ordl.job-no AND */
/*            fg-rctd.job-no2 EQ oe-ordl.job-no2   */
/*            NO-LOCK                              */
/*            USE-INDEX i-no                       */
/*            BY fg-rctd.rct-date:                 */
/*                                                             */
/*            v-manuf-date = fg-rctd.rct-date.                 */
/*            LEAVE.                                           */
/*        END.                                                 */
/*                                                             */
/*        IF v-manuf-date EQ 12/31/2999 THEN                   */
/*           FOR EACH fg-rctd WHERE                            */
/*               fg-rctd.company EQ cocode AND                 */
/*               fg-rctd.i-no EQ oe-ordl.i-no AND              */
/*               fg-rctd.po-no EQ STRING(oe-ordl.po-no-po) AND */
/*               fg-rctd.rita-code EQ "R"                      */
/*               NO-LOCK                                       */
/*               USE-INDEX i-no                                */
/*               BY fg-rctd.rct-date:                          */
/*                                                             */
/*               v-manuf-date = fg-rctd.rct-date.              */
/*               LEAVE.                                        */
/*           END.                                              */
         
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

/*        IF AVAIL eb AND eb.part-dscr1 NE "" THEN */
/*           v-part-desc = eb.part-dscr1.          */
/*        ELSE IF AVAIL oe-ordl THEN               */
/*           v-part-desc = oe-ordl.i-name.         */
       IF AVAIL itemfg THEN v-part-desc = itemfg.part-dscr1.
       IF AVAIL eb AND eb.cad-no NE "" THEN
          v-cad = eb.cad-no.
       ELSE
          v-cad = itemfg.cad-no.
       IF AVAIL eb AND eb.spc-no NE "" THEN
          v-spc = eb.spc-no.
       ELSE
          v-spc = itemfg.spc-no.

       PUT UNFORMATTED
           "Customer Name:<C20>" cust.NAME SKIP(1)
           "Customer Order Number:<C20>"  oe-boll.po-no SKIP(1)
           "Customer Part Number:<C20>" (IF AVAIL itemfg THEN itemfg.i-name ELSE "") SKIP(1)
           "Customer Part Description:<C20>" v-part-desc SKIP(1)
           "Drawing Number:<C20>" v-cad SKIP(1)
           "Customer Specification/Drawing No./" SKIP
           "Revision No:<C20>" v-spc SKIP(1)
           "Vendor Order Number:<C20>" (IF AVAIL oe-ordl THEN STRING(oe-ordl.ord-no)
                                                  ELSE "") SKIP(1)
           "Quantity Shipped:<C20>"  STRING(v-bol-qty,"ZZ,ZZZ,ZZ9") SKIP(1)
/*            "Manufactured Date:<C20>" (IF v-manuf-date NE 12/31/2999 THEN               */
/*                                                      STRING(v-manuf-date,"99/99/9999") */
/*                                                   ELSE "") SKIP(1)                     */
           "Release Date:<C20>" (IF v-rel-date NE 12/31/2999 THEN
                                                     STRING(v-rel-date,"99/99/9999")
                                                  ELSE "") SKIP(1)
/*            "Vendor Manufacturing Batch Numbers and Dates:" SKIP */
/*            "<C20>" v-jobs[1] SKIP                               */
/*            "<C20>" v-jobs[2] SKIP                               */
/*            "<C20>" v-jobs[3] SKIP                               */
/*            "<C20>" v-jobs[4] SKIP                               */
/*            "<C20>" v-jobs[5] SKIP                               */
/*            "<C20>" v-jobs[6] SKIP                               */
/*            "<C20>" v-jobs[7] SKIP                               */
/*            "<C20>" v-jobs[8] SKIP                               */
/*            "<C20>" v-jobs[9] SKIP                               */
/*            "<C20>" v-jobs[10] SKIP                              */
                                                .
        
    PUT UNFORMATTED
        "<C1><R63>Signature: ___________________________________________".
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
