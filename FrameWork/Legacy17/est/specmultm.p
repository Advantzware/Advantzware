/* ---------------------------------------------- est/specmult */
/* PRINT multi spec sheet                                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xxreport     for report. 

{oe/rep/oe-lad.i} 

DEF VAR v-printline AS INT.
def var v-tot-wt            as   dec format ">>,>>>,>>9" NO-UNDO.
def var v-tot-cases         as   int format ">>>>9" NO-UNDO.

def var v-ship-name  like shipto.ship-name NO-UNDO.
def var v-ship-addr  like shipto.ship-addr NO-UNDO.
def var v-ship-city  like shipto.ship-city NO-UNDO.
def var v-ship-state like shipto.ship-state NO-UNDO.
def var v-ship-zip   like shipto.ship-zip NO-UNDO.
def var v-ship-addr3 as   char format "x(30)" NO-UNDO.
def var v-comp-name  like company.NAME NO-UNDO.
def var v-comp-addr  like company.addr NO-UNDO.
def var v-comp-city  like company.city NO-UNDO.
def var v-comp-state like company.state NO-UNDO.
def var v-comp-zip   like company.zip NO-UNDO.
def var v-comp-addr3 as   char format "x(30)" NO-UNDO.
def var v-cust-addr3 as   char format "x(30)" NO-UNDO.
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.

DEF VAR v-sample-no AS CHAR NO-UNDO.
DEF VAR v-sample-sent AS CHAR NO-UNDO.
DEF VAR v-date-rec    AS CHAR NO-UNDO.
DEF VAR v-date-due    AS CHAR NO-UNDO.
DEF VAR v-no-samples  AS CHAR NO-UNDO.
DEF VAR v-no-cells    AS CHAR NO-UNDO.

DEFINE VARIABLE scr-end-cell-l1 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.
DEFINE VARIABLE scr-end-cell-l2 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 VIEW-AS FILL-IN SIZE 17 BY 1.
DEFINE VARIABLE scr-end-cell-w1 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 VIEW-AS FILL-IN SIZE 17 BY 1.
DEFINE VARIABLE scr-end-cell-w2 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 VIEW-AS FILL-IN SIZE 17 BY 1.
DEFINE VARIABLE scr-in-cell-length AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 VIEW-AS FILL-IN SIZE 17 BY 1.
DEFINE VARIABLE scr-in-cell-width AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 VIEW-AS FILL-IN SIZE 17 BY 1.
DEFINE VARIABLE scr-style-1 AS CHARACTER FORMAT "X(6)":U VIEW-AS FILL-IN SIZE 10 BY 1 NO-UNDO.
DEFINE VARIABLE scr-style-2 AS CHARACTER FORMAT "X(6)":U VIEW-AS FILL-IN SIZE 10 BY 1 NO-UNDO.

DEF VAR v-eb-len LIKE eb.len NO-UNDO.
DEF VAR v-eb-wid LIKE eb.len NO-UNDO.
DEF VAR v-eb-dep LIKE eb.len NO-UNDO.
DEF VAR v-eb-len1 LIKE eb.len NO-UNDO.
DEF VAR v-eb-wid1 LIKE eb.len NO-UNDO.
DEF VAR v-eb-dep1 LIKE eb.len NO-UNDO.
DEF VAR v-eb-len2 LIKE eb.len NO-UNDO.
DEF VAR v-eb-wid2 LIKE eb.len NO-UNDO.
DEF VAR v-eb-dep2 LIKE eb.len NO-UNDO.

DEF VAR v-slot1  AS INT FORMAT ">>,>>9" NO-UNDO.
DEF VAR v-slot2  AS INT FORMAT ">>,>>9" NO-UNDO.
DEF VAR v-strips1 LIKE eb.yld-qty  NO-UNDO.
DEF VAR v-strips2 LIKE eb.yld-qty NO-UNDO.
DEF VAR v-caliper1 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-caliper2 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-part     AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-customer LIKE company.NAME FORMAT "x(25)" NO-UNDO.
DEF VAR v-attn     AS CHAR FORMAT "x(25)" NO-UNDO.
find first cust no-lock no-error.
DEF VAR r2 AS INT INIT[1].

find first company where company.company eq cocode no-lock.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
ASSIGN 
       v-comp-add1 = company.addr[1]
       v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
       v-comp-add3 = "Phone: 604.533.2545" 
       v-comp-add4 = "Fax  : 604.533.2633"
      .

for each xxreport where xxreport.term-id eq v-term-id:

    ASSIGN
      v-sample-no                 = xxreport.key-03
      v-sample-sent               = xxreport.key-04
      v-date-rec                  = xxreport.key-05
      v-date-due                  = xxreport.key-06
      v-no-samples                = xxreport.key-07
      v-no-cells                  = TRIM(xxreport.key-08).
    IF v-no-cells = "0" THEN
      v-no-cells = "".

    FIND FIRST eb WHERE recid(eb) = xxreport.REC-ID
                  NO-LOCK NO-ERROR.
    IF AVAIL eb THEN
      FIND FIRST est WHERE est.company = eb.company 
                       AND est.est-no  = eb.est-no
                     NO-LOCK NO-ERROR.
    IF NOT AVAIL eb OR NOT AVAIL est THEN
      NEXT.

    RUN calc-values (INPUT ROWID(est), ROWID(eb)).
    FIND first cust where cust.company eq cocode
                      and cust.cust-no eq eb.cust-no 
                    NO-LOCK.
    v-customer = cust.NAME.
    
        
    RUN oe/custxship.p (eb.company ,
                        eb.cust-no ,
                        eb.ship-id ,
                        BUFFER shipto).
    v-attn     = shipto.contact.
          
    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     /*v-phone-num    = cust.area-code + cust.phone */
     v-comp-name    = cust.name
     v-comp-addr[1] = cust.addr[1]
     v-comp-addr[2] = cust.addr[2]
     v-comp-addr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip.

    if trim(v-comp-addr3) eq "," then v-comp-addr3 = "".
              
    if v-comp-addr[2] eq "" then
      assign
       v-comp-addr[2] = v-comp-addr3
       v-comp-addr3   = "".
    if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

     IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
     IF v-ship-addr[r2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".
     {est/specmult1.i}
   /* {est/specmult.i} */

    v-last-page = page-number.

  v-printline = v-printline + 14.
  
  PAGE.
  v-printline = 0.
 end. /* for each oe-bolh */

 for each report where report.term-id eq v-term-id /*,
     first oe-boll where recid(oe-boll) eq report.rec-id no-lock */:
  delete report.
end.

PROCEDURE calc-values:

DEF INPUT PARAMETER ip-est-rowid AS ROWID NO-UNDO.
DEF INPUT PARAMETER ip-eb-rowid AS ROWID NO-UNDO.
DEF VAR v-int AS INT NO-UNDO.
DEF VAR ll-crt-itemfg AS LOG INIT NO NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO. 
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR iCnt AS INT NO-UNDO.

DEF BUFFER b-eb1 FOR eb.
DEF BUFFER b-eb2 FOR eb.
DEF BUFFER bf-est FOR est.
DEF BUFFER bf-set FOR eb.


{sys/inc/f16to32.i}
{cec/msfcalc.i}
{sys/inc/setprint.i}

find est where rowid(est) = ip-est-rowid no-lock NO-ERROR.
FIND eb WHERE ROWID(eb) = ip-eb-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL eb THEN
  RETURN.

FIND FIRST bf-set WHERE
     bf-set.company = eb.company AND
     bf-set.est-no = eb.est-no AND
     bf-set.form-no = 0
     NO-LOCK NO-ERROR.

/* Just to get part number */
FIND FIRST b-eb1 WHERE 
     b-eb1.company EQ eb.company AND
     b-eb1.est-no  EQ eb.est-no AND
     b-eb1.form-no EQ 0
     USE-INDEX est-qty
     NO-LOCK NO-ERROR.
v-part = "".
v-part = eb.part-dscr1.
IF v-part = "" THEN
  v-part = eb.part-dscr1.

FIND FIRST b-eb1 WHERE 
     b-eb1.company EQ eb.company AND
     b-eb1.est-no  EQ eb.est-no AND
     b-eb1.form-no NE 0 AND
     b-eb1.blank-no NE 0
     USE-INDEX est-qty
     NO-LOCK NO-ERROR.

IF AVAIL b-eb1 THEN
   FIND FIRST b-eb2 WHERE 
        b-eb2.company EQ eb.company AND
        b-eb2.est-no  EQ eb.est-no AND
        b-eb2.form-no NE 0 AND
        b-eb2.blank-no NE 0 AND
        ROWID(b-eb2) NE ROWID(b-eb1)
        USE-INDEX est-qty
        NO-LOCK NO-ERROR.

IF AVAIL b-eb1 THEN
DO:

   FIND FIRST style WHERE
        style.company EQ b-eb1.company AND
        style.style = b-eb1.style
        NO-LOCK.
   IF AVAIL(style) THEN
     v-slot1 = style.dim-df.
   iCnt = 0.
   FOR EACH ef WHERE ef.company = b-eb1.company
                 AND ef.est-no  = b-eb1.est-no
                 AND ef.eqty    = b-eb1.eqty
                 AND ef.form-no = b-eb1.form-no
               NO-LOCK.

     FIND FIRST ITEM WHERE ITEM.company = ef.company
                       AND ITEM.i-no = ef.board 
                     NO-LOCK NO-ERROR.
     IF AVAIL ITEM THEN DO:

       iCnt = iCnt + 1.
       IF icnt = 1 THEN
         v-caliper1 = STRING(ITEM.i-name).
       ELSE
         v-caliper2 = string(ITEM.i-name).
     END.

   END.
   ASSIGN
      v-int = style.dim-df + 1
      scr-style-1 = b-eb1.style
      scr-end-cell-l1 = {sys/inc/k16.i b-eb1.k-len-array2[1]}
      scr-end-cell-l2 = {sys/inc/k16.i b-eb1.k-len-array2[v-int]}
      scr-in-cell-length = {sys/inc/k16.i b-eb1.k-len-array2[2]}
      v-strips1 = b-eb1.yld-qty.

   ASSIGN
   v-eb-len1 = {sys/inc/k16.i b-eb1.len}
   v-eb-wid1 = {sys/inc/k16.i b-eb1.wid}
   v-eb-dep1 = {sys/inc/k16.i b-eb1.dep}.
END.

IF AVAIL b-eb2 THEN
DO:
   FIND FIRST style WHERE
        style.company EQ b-eb2.company AND
        style.style = b-eb2.style
        NO-LOCK.
   IF AVAIL(style) THEN
     v-slot2 = style.dim-df.
   ASSIGN
      v-int = style.dim-df + 1
      scr-style-2 = b-eb2.style
      scr-end-cell-w1 = {sys/inc/k16.i b-eb2.k-len-array2[1]}
      scr-end-cell-w2 = {sys/inc/k16.i b-eb2.k-len-array2[v-int]}
      scr-in-cell-width = {sys/inc/k16.i b-eb2.k-len-array2[2]}
      v-strips2 = b-eb2.yld-qty       .
   iCnt = 0.
   FOR EACH ef WHERE ef.company = b-eb2.company
                 AND ef.est-no  = b-eb2.est-no
                 AND ef.eqty    = b-eb2.eqty
                 AND ef.form-no = b-eb2.form-no
               NO-LOCK.

     FIND FIRST ITEM WHERE ITEM.company = ef.company
                       AND ITEM.i-no = ef.board 
                     NO-LOCK NO-ERROR.
     IF AVAIL ITEM THEN DO:

       iCnt = iCnt + 1.
       IF v-caliper2 = "" THEN
         v-caliper2 = string(ITEM.i-name).
     END.

   END.
   ASSIGN
     v-eb-len2 = {sys/inc/k16.i b-eb2.len}
     v-eb-wid2 = {sys/inc/k16.i b-eb2.wid}
     v-eb-dep2 = {sys/inc/k16.i b-eb2.dep}.


END.

ASSIGN
   v-eb-len = {sys/inc/k16.i eb.len}
   v-eb-wid = {sys/inc/k16.i eb.wid}
   v-eb-dep = {sys/inc/k16.i eb.dep}.

RELEASE itemfg.

IF avail(bf-set) AND bf-set.stock-no NE "" THEN
   FIND FIRST itemfg WHERE
        itemfg.company EQ bf-set.company AND
        itemfg.i-no    EQ bf-set.stock-no
        NO-LOCK NO-ERROR.

END.
/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */


