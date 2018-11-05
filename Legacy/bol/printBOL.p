/******  Addon/bol/printBOL.p *******/  

DEFINE INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-loc AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-bol-no AS INT NO-UNDO.
DEFINE INPUT PARAMETER ip-printed AS LOG NO-UNDO.
DEFINE INPUT PARAMETER ip-posted AS LOG NO-UNDO.
DEFINE INPUT PARAMETER ip-post-bol AS LOG NO-UNDO.

DEF VAR v-format-str AS CHAR INIT "BOLFMT" NO-UNDO.
def var v-print-fmt AS CHAR NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR lines-per-page AS INT INIT 99 NO-UNDO.
def var v-headers as log  no-undo.
def var list-name as char no-undo.
def var init-dir as char no-undo.
DEF VAR lv-run-bol AS char no-undo.
DEF VAR lv-run-commercial AS char no-undo.
DEF VAR lv-prt-bypass AS LOG NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
def var v-check-qty as log no-undo.

DEF BUFFER b-oe-bolh FOR oe-bolh.
DEFINE BUFFER bf-oe-boll FOR oe-boll.
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.



{sys/inc/var.i new shared}
{oe/rep/oe-lad.i NEW}
{custom/xprint.i}
{oe/bolcheck.i NEW}
{oe/closchk.i NEW}

DEF TEMP-TABLE tt-post NO-UNDO 
    FIELD row-id AS ROWID.

ASSIGN
   v-print-bol = YES
   cocode = ip-company
   locode = ip-loc.

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLBOL &Company=cocode}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

find first sys-ctrl WHERE
     sys-ctrl.company eq ip-company AND
     sys-ctrl.name    eq "BOLFMT"
     NO-LOCK no-error.

if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = ip-company
   sys-ctrl.name     = "BOLFMT"
   sys-ctrl.descrip  = "Bill of lading format"
   sys-ctrl.char-fld = "ASI".
  message "System control record not found. Update BOL Print format"
  update sys-ctrl.char-fld.
end.
assign
 v-print-fmt = sys-ctrl.char-fld
 v-headers   = sys-ctrl.log-fld.

find first sys-ctrl WHERE
     sys-ctrl.company eq cocode AND
     sys-ctrl.name    eq "BOLPOST"
     no-lock no-error.

if not avail sys-ctrl then do transaction:
   create sys-ctrl.
   assign
      sys-ctrl.company = cocode
      sys-ctrl.name    = "BOLPOST"
      sys-ctrl.descrip = "Post BOL if BOL Qty > Bin Qty".
end.

v-check-qty = sys-ctrl.char-fld eq "Bin>Qty".

RUN SetBolForm(INPUT v-print-fmt).
vcDefaultForm = v-print-fmt.

IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
   sys-ctrl-shipto.company = ip-company AND
   sys-ctrl-shipto.NAME = v-format-str) THEN
   DO:
      IF CAN-FIND(FIRST b-oe-bolh WHERE
         b-oe-bolh.company EQ ip-company AND
         b-oe-bolh.bol-no  EQ ip-bol-no AND
         b-oe-bolh.cust-no EQ ip-cust-no AND
         b-oe-bolh.printed EQ ip-printed AND
         b-oe-bolh.posted  EQ ip-posted AND
         CAN-FIND (FIRST oe-boll
                   WHERE oe-boll.company EQ b-oe-bolh.company
                     AND oe-boll.b-no    EQ b-oe-bolh.b-no)) THEN
         FOR EACH b-oe-bolh WHERE
             b-oe-bolh.company EQ ip-company AND
             b-oe-bolh.bol-no  EQ ip-bol-no AND
             b-oe-bolh.cust-no EQ ip-cust-no AND
             b-oe-bolh.printed EQ ip-printed AND
             b-oe-bolh.posted  EQ ip-posted AND
             CAN-FIND (FIRST oe-boll
                       WHERE oe-boll.company EQ b-oe-bolh.company
                         AND oe-boll.b-no    EQ b-oe-bolh.b-no)
             NO-LOCK
             BREAK BY b-oe-bolh.company
                   BY b-oe-bolh.cust-no:
   
             IF FIRST-OF(b-oe-bolh.cust-no) THEN
             DO:
                FIND FIRST sys-ctrl-shipto WHERE
                     sys-ctrl-shipto.company = ip-company AND
                     sys-ctrl-shipto.NAME = v-format-str AND
                     sys-ctrl-shipto.cust-vend = YES AND
                     sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                     sys-ctrl-shipto.ship-id      = b-oe-bolh.ship-id AND
                     sys-ctrl-shipto.char-fld > ''
                     NO-LOCK NO-ERROR.
   
                IF NOT AVAIL sys-ctrl-shipto THEN
                   FIND FIRST sys-ctrl-shipto WHERE
                     sys-ctrl-shipto.company = ip-company AND
                     sys-ctrl-shipto.NAME = v-format-str AND
                     sys-ctrl-shipto.cust-vend = YES AND
                     sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                     sys-ctrl-shipto.char-fld > ''
                     NO-LOCK NO-ERROR.
   
                IF AVAIL sys-ctrl-shipto THEN
                DO:
                   RUN SetBolForm(sys-ctrl-shipto.char-fld).
                   v-print-fmt = sys-ctrl-shipto.char-fld.
                END.
                ELSE
                DO:
                   RUN SetBolForm (vcDefaultForm).
                   v-print-fmt = vcDefaultForm.
                END.
   
                RUN run-report(b-oe-bolh.cust-no,YES).
                RUN GenerateReport(b-oe-bolh.cust-no,YES).
             END. /*first-of*/
         END. /*each b-oe-bolh*/
   END.
ELSE /*not find first sys-ctrl-shipto*/
DO:
   v-print-fmt = vcDefaultForm.
   
   RUN SetBolForm(v-print-fmt).
   RUN run-report("",NO).
   RUN GenerateReport(ip-cust-no,NO).
END.

ll = ip-post-bol AND NOT ip-posted.

IF ll AND oe-ctrl.u-inv AND v-check-qty THEN
   FOR EACH tt-post,
       FIRST oe-bolh NO-LOCK 
       WHERE ROWID(oe-bolh) EQ tt-post.row-id:
   
       RUN oe/bolcheck.p (ROWID(oe-bolh)).
      
       IF CAN-FIND (FIRST w-except 
                    WHERE w-except.bol-no EQ oe-bolh.bol-no) THEN DO:
      
          MESSAGE "BOL has insufficient inventory and cannot be posted..."
              VIEW-AS ALERT-BOX ERROR.
          ll = NO.
          LEAVE.
       END.
   END.

IF ll THEN
   RUN post-bol.

/*********************************************************************/
PROCEDURE SetBolForm:

   DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.

   {sys/inc/bolform.i}
END.
/*********************************************************************/
PROCEDURE run-report:

  DEFINE INPUT PARAMETER ip-cust-no-2 AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.
    
  {sys/form/r-top.i}
  
  assign
    v-s-bol             = ip-bol-no
    v-printed           = ip-printed
    v-print-pal         = YES
    v-print-components  = YES
    v-print-shipnotes   = YES
    v-print-dept        = NO.

  IF ip-sys-ctrl-ship-to THEN
     ASSIGN
        v-s-cust = ip-cust-no-2.
  ELSE
     ASSIGN 
        v-s-cust = ip-cust-no.
  
  {sys/inc/print1.i}
  
  {sys/inc/outprint.i value(lines-per-page)}
  
  {sa/sa-sls01.i}
  
  v-term-id = v-term.

  run build-work ('').
  
  IF lv-run-bol = "YES" THEN
     RUN value(v-program).
  
  for each report where report.term-id eq v-term-id:
      delete report.
  end.
  
  OUTPUT CLOSE.
END.
/*********************************************************************/
PROCEDURE build-work:

   DEF INPUT PARAM ic2ndKey  AS CHAR NO-UNDO.

  build-work:
  FOR EACH oe-bolh
     WHERE oe-bolh.company EQ cocode
       AND oe-bolh.bol-no  EQ v-s-bol
       AND oe-bolh.cust-no EQ v-s-cust
       AND oe-bolh.printed EQ v-printed
       AND oe-bolh.posted  EQ ip-posted
       AND CAN-FIND (FIRST oe-boll
                     WHERE oe-boll.company EQ oe-bolh.company
                       AND oe-boll.b-no    EQ oe-bolh.b-no)
      USE-INDEX post:
  
    IF NOT oe-ctrl.p-bol THEN
    FOR EACH oe-boll
       WHERE oe-boll.company EQ oe-bolh.company
         AND oe-boll.bol-no  EQ oe-bolh.bol-no
         AND CAN-FIND(FIRST oe-ord
                      WHERE oe-ord.company EQ oe-boll.company
                        AND oe-ord.ord-no  EQ oe-boll.ord-no
                        AND (oe-ord.stat    EQ "H" OR oe-ord.priceHold))
        NO-LOCK:

      MESSAGE "Order on BOL is on " + (IF oe-ord.stat = "H" THEN "hold" ELSE "Price hold") + ", and BOL will not print."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

      NEXT build-work.
    END.
  
    /* update loadtag status - Bill of lading task#: 10190414 */
    IF NOT oe-bolh.printed THEN
       FOR EACH bf-oe-boll NO-LOCK
          WHERE bf-oe-boll.company EQ oe-bolh.company 
            AND bf-oe-boll.b-no    EQ oe-bolh.b-no
            AND bf-oe-boll.tag     NE "",
          FIRST loadtag
          WHERE loadtag.company   EQ bf-oe-boll.company
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ bf-oe-boll.tag
          USE-INDEX tag:
          loadtag.sts = "Bill of Lading".
       END.
  
    IF ic2ndKey NE ? AND ic2ndKey NE '' THEN DO:
       FIND FIRST shipto WHERE
            shipto.rec_key = ic2ndKey AND
            shipto.ship-id = oe-bolh.ship-id
            NO-LOCK NO-ERROR.
       IF NOT AVAIL shipto THEN NEXT build-work.
    END.

    FIND FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company      EQ oe-bolh.company AND
         sys-ctrl-shipto.name         EQ "BOLFMT" AND
         sys-ctrl-shipto.cust-vend    EQ YES AND
         sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no AND
         sys-ctrl-shipto.ship-id      EQ oe-bolh.ship-id
         NO-LOCK NO-ERROR.

    CREATE report.
    
    ASSIGN 
        report.term-id  = v-term-id
        report.key-01   = oe-bolh.cust-no
        report.key-02   = oe-bolh.ship-id
        report.rec-id   = RECID(oe-bolh)
        report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
        oe-bolh.printed = YES
        report.key-03   = IF AVAIL sys-ctrl-shipto AND  NOT sys-ctrl-shipto.log-fld THEN "C" /*commercial invoice only*/
                          ELSE IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN "B" /*commercial invoice and bol both*/
                          ELSE "N" /*BOL only*/ 
        report.key-04   = IF AVAIL sys-ctrl-shipto THEN sys-ctrl-shipto.char-fld ELSE "".

    IF lv-run-bol        = "" AND report.key-03 <> "C" THEN lv-run-bol = "YES" .
    IF lv-run-commercial = "" AND report.key-03 <> "N" THEN lv-run-commercial = "YES".
    
    CREATE tt-post.
    tt-post.row-id = ROWID(oe-bolh).
  END.
  
  v-lines-per-page = lines-per-page.
END.
/*********************************************************************/
PROCEDURE GenerateReport:

   DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

   IF v-print-bol AND v-print-fmt <> "SouthPak-XL" THEN
      run output-to-printer(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
END PROCEDURE.
/*********************************************************************/
PROCEDURE output-to-printer:
   DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

   IF is-xprint-form THEN DO:
      FILE-INFO:FILE-NAME = list-name.
      RUN printfile (FILE-INFO:FILE-NAME).
   END.
   ELSE
      RUN custom/prntproc.p (list-name,15,"P").
END.
/*********************************************************************/  
PROCEDURE post-bol:
   {sa/sa-sls01.i}

  FOR EACH w-ord.
      DELETE w-ord.
  END.

  FOR EACH tt-post TRANSACTION:
    RELEASE oe-bolh.
    DO WHILE NOT AVAIL oe-bolh:
      FIND FIRST oe-bolh EXCLUSIVE WHERE ROWID(oe-bolh) EQ tt-post.row-id
          NO-WAIT NO-ERROR.

      IF AVAIL oe-bolh AND oe-bolh.posted EQ NO THEN
      FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:
          RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term).
      END.
    END.
  END.

  RUN oe/oe-bolp3.p (v-term).

  /* close transfer order here */
  RUN oe/closchk.p (0).

  FOR EACH w-ord:
      RUN oe/close.p (w-ord.rec-id, YES).  
  END.
END.
/*********************************************************************/
