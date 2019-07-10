

/*------------------------------------------------------------------------
    File        : BookingsHighReport.p
    Purpose     :  Bookings Highlights

    Syntax      :

    Description : Return a Dataset of Request Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
       

    DEFINE TEMP-TABLE ttBookingReport NO-UNDO
          FIELD vBookingFile AS CHAR              
    .

    DEFINE DATASET dsBookingReport FOR ttBookingReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDate            AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmComp            AS CHAR  NO-UNDO.
    
    DEFINE INPUT PARAMETER prmOutexcel        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBookingReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF  prmUser        = ?        THEN ASSIGN     prmUser       = "".
    IF  prmAction      = ?        THEN ASSIGN     prmAction     = "".
    IF  prmDate        = ?        THEN ASSIGN     prmDate       = "".
    IF  prmComp        = ?        THEN ASSIGN     prmComp       = "".     

    

    {sys/inc/var.i new shared}
    {salrep/dashbook.i NEW}

    DEF TEMP-TABLE tt-report NO-UNDO LIKE report
        FIELD DATE AS DATE
        FIELD row-id AS ROWID
        FIELD qty AS DEC
        FIELD amt       LIKE ar-invl.amt        FORMAT "->>>>>>>9.99"
        FIELD cash-date LIKE ar-inv.inv-date
        FIELD misc AS LOG
        FIELD cost AS DEC
        FIELD msf AS DEC.

    def TEMP-TABLE w-data no-undo
        field w-sman-no   AS CHAR
        field w-sqft      LIKE itemfg.t-sqft format "->>>9.999"    extent 4
        field w-amt       like ar-inv.gross  format "->>>,>>9.99"  extent 4
        field w-cost      like ar-inv.t-cost format "->>,>>9.99"   extent 3
        FIELD w-msf       AS DEC EXTENT 3.
    
    def TEMP-TABLE w-ord NO-UNDO
        field cost like oe-ordl.cost
    field price like oe-ordl.price
    field t-price like oe-ordl.t-price format "->>,>>>,>>9"
    field rel-qty like oe-rel.qty
    field rel-date as DATE
    field msf as dec format "->>9.999"
    FIELD tons AS DEC.


    DEF BUFFER b-oe-ordl FOR oe-ordl.
  
   DEFINE VARIABLE fi_as-of-date AS DATE FORMAT "99/99/9999"   NO-UNDO.                                                            
   DEFINE VARIABLE fi_company AS CHARACTER FORMAT "X(3)"    NO-UNDO.
   DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
   

    
    DEFINE NEW SHARED VAR v-webrootpath AS CHAR NO-UNDO.
    DEFINE  NEW SHARED VARIABLE vFileName AS CHAR NO-UNDO.
    DEFINE VARIABLE init-dir AS CHAR NO-UNDO.
    DEFINE VARIABLE  v-excel-file    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE v-VERSION AS CHARACTER NO-UNDO.

   
    FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
    NO-LOCK NO-ERROR.

   /* prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".*/

    assign
        cocode = prmComp
        locode = usercomp.loc
        v-today = TODAY . 

    
        
  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.



IF prmAction = "RunReport" THEN DO:  

    ASSIGN
        fi_as-of-date  = Date(prmDate)
        fi_company     = prmComp  .

        EMPTY TEMP-TABLE tt-raw-op.
        EMPTY TEMP-TABLE tt-report.
        EMPTY TEMP-TABLE w-data.

        
        RUN run-report.        

        CREATE ttBookingReport.
        ASSIGN ttBookingReport.vBookingFile = vFileName .
        
 
 
 END.
  
 /*****************************************PROCEDURE run-report :*****************************************************/
 

 PROCEDURE run-report :


    RUN raw-op-proc. /*Raw OP*/   
   
    RUN salrep\dashboard-book.p(INPUT fi_company,
                               INPUT fi_as-of-date).

    OS-DELETE VALUE("d:\webapps\invhigh.csv") . 
   
                           
 end procedure.




  /*****************************************PROCEDURE raw-op-proc :*****************************************************/

 PROCEDURE raw-op-proc :

   DEF VAR v-date AS DATE NO-UNDO.
   DEF VAR v-price AS DEC NO-UNDO.
   DEF VAR v-oe-gp AS DEC NO-UNDO.
   DEF VAR v-start-last-year AS DATE NO-UNDO.
   DEF VAR v-end-this-year AS DATE NO-UNDO.

   ASSIGN
      v-start-last-year = DATE(1,1,YEAR(fi_as-of-date) - 1)
      v-end-this-year   = DATE(12,31,YEAR(fi_as-of-date)).
   
   EMPTY TEMP-TABLE tt-raw-op.
   
   DO v-date = DATE(1,1,YEAR(fi_as-of-date) - 1) TO
      DATE(12,31,YEAR(fi_as-of-date)):
      
      CREATE tt-raw-op.
      tt-raw-op.DATE = v-date.
      RELEASE tt-raw-op.
   END.
   
   FOR each oe-ord FIELDS(company ord-no ord-date TYPE) WHERE
       oe-ord.company  eq fi_company AND
       oe-ord.ord-date ge v-start-last-year AND
       oe-ord.ord-date le v-end-this-year AND
       oe-ord.type     ne "T"
       no-lock,
       each oe-ordl FIELDS(t-price qty company ord-no cost) WHERE
            oe-ordl.company eq oe-ord.company AND
            oe-ordl.ord-no  eq oe-ord.ord-no
            no-lock,
       first itemfg FIELDS(company i-no t-sqft weight-100) WHERE
             itemfg.company eq oe-ord.company AND
             itemfg.i-no    eq oe-ordl.i-no
             no-lock,
       FIRST tt-raw-op WHERE
             tt-raw-op.DATE EQ oe-ord.ord-date:

      ASSIGN
        tt-raw-op.oe-dollars = tt-raw-op.oe-dollars + oe-ordl.t-price
        tt-raw-op.oe-qty = tt-raw-op.oe-qty 
                         + oe-ordl.qty
        tt-raw-op.oe-qty-msf = tt-raw-op.oe-qty-msf 
                             + (itemfg.t-sqft * oe-ordl.qty / 1000)
        tt-raw-op.oe-qty-tons = tt-raw-op.oe-qty-tons
                              +( itemfg.weight-100 * oe-ordl.qty / 100 / 2000)
        v-oe-gp = (IF oe-ordl.t-price NE 0 THEN
                   ((oe-ordl.t-price - (oe-ordl.cost * (oe-ordl.qty / 1000) ) )  
                     / oe-ordl.t-price * 100)
                   ELSE 0)
        v-oe-gp = IF v-oe-gp EQ ? THEN 0 ELSE v-oe-gp
        tt-raw-op.oe-gp = tt-raw-op.oe-gp 
                        + v-oe-gp.
       
   END.

   RUN raw-op-rel-proc.

END PROCEDURE.



  /*****************************************PROCEDURE raw-op-rel-proc :*****************************************************/


PROCEDURE raw-op-rel-proc :

   DEF VAR v-types AS CHAR INIT "PALSBICZ" NO-UNDO.
   DEF VAR v-type AS CHAR NO-UNDO.
   DEF VAR v-start-date AS DATE NO-UNDO.
   DEF VAR v-end-date AS DATE NO-UNDO.
   DEF VAR lv-qty AS DEC NO-UNDO.
   DEF VAR v-qty AS DEC NO-UNDO.
   DEF VAR v-date AS DATE NO-UNDO.
   DEF VAR v-rel-gp AS DEC NO-UNDO.

   ASSIGN
     v-start-date = DATE(1,1,YEAR(fi_as-of-date) - 1)
     v-end-date = DATE(12,31,YEAR(fi_as-of-date)). 

   EMPTY TEMP-TABLE w-ord.
   EMPTY TEMP-TABLE tt-report.

   FOR EACH oe-ordl FIELDS(company opened ord-no LINE i-no)
      WHERE oe-ordl.company EQ fi_company
        AND oe-ordl.opened  EQ YES
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
                         USE-INDEX ord-no)
      USE-INDEX opened NO-LOCK,
      FIRST oe-ord FIELDS(company ord-no)
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
      NO-LOCK:

      RUN oe/cleanrel.p (ROWID(oe-ordl)).

      for each oe-rel FIELDS(company cust-no ord-no i-no LINE rel-date) no-lock
        where oe-rel.company   eq oe-ordl.company
          and oe-rel.ord-no    eq oe-ordl.ord-no
          and oe-rel.i-no      eq oe-ordl.i-no
          and oe-rel.line      eq oe-ordl.line
          and oe-rel.rel-date  ge v-start-date
          and oe-rel.rel-date  le v-end-date
        use-index ord-item:
      
        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).
        
        if index("AB",v-type) gt 0 then next.
        
        if index(v-types,v-type) gt 0 then do:
          create tt-report.
          assign
           tt-report.key-06  = v-type
           tt-report.rec-id  = recid(oe-rel).
        end.
      end.
   
      FOR EACH oe-rell FIELDS(r-no company ord-no i-no LINE b-ord-no po-no
          qty rel-no) NO-LOCK
        WHERE oe-rell.company EQ oe-ordl.company
          AND oe-rell.ord-no  EQ oe-ordl.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line
          AND ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
               (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
        USE-INDEX ord-no,

        FIRST oe-relh fields(cust-no r-no posted deleted rel-date) NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND oe-relh.rel-date GE v-start-date
          AND oe-relh.rel-date LE v-end-date
          
        USE-INDEX r-no
      
      BREAK BY oe-rell.r-no
            BY oe-rell.ord-no
            BY oe-rell.i-no
            BY oe-rell.line
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no:

       IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.
       
       lv-qty = lv-qty + oe-rell.qty.
       
       IF LAST-OF(oe-rell.po-no) THEN DO:
         create tt-report.
         assign
          tt-report.key-06  = if oe-rell.b-ord-no eq 0 then "A" else "B"
          tt-report.qty     = lv-qty
          tt-report.rec-id  = recid(oe-rell).
       END.
      END.
   END.

   IF NOT CAN-FIND(FIRST tt-report) THEN DO:
      CREATE tt-report.
   END.

   RELEASE tt-report.

   for each tt-report:

       release oe-rel.
       release oe-rell.
       release oe-relh.
       release oe-ord.
       release oe-ordl.
      
       find first oe-rel 
           where recid(oe-rel) eq tt-report.rec-id 
           no-lock no-error.
      
       if avail oe-rel then do:
         FOR EACH oe-rell FIELDS(company ord-no rel-no b-ord-no i-no LINE
             r-no) NO-LOCK
             WHERE oe-rell.company  EQ fi_company
               AND oe-rell.ord-no   EQ oe-rel.ord-no
               AND oe-rell.rel-no   EQ oe-rel.rel-no
               AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
               AND oe-rell.i-no     EQ oe-rel.i-no
               AND oe-rell.line     EQ oe-rel.line
             USE-INDEX ord-no,
             FIRST oe-relh FIELDS(cust-no r-no posted deleted) WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK:
      
           IF oe-relh.posted EQ NO AND oe-relh.deleted EQ NO THEN
             tt-report.rec-id = recid(oe-rell).
           ELSE RELEASE oe-relh.
      
           LEAVE.
         END.
       
         find first oe-ordl
             where oe-ordl.company eq fi_company
               and oe-ordl.ord-no  eq oe-rel.ord-no
               and oe-ordl.i-no    eq oe-rel.i-no
               and oe-ordl.line    eq oe-rel.line
             no-lock.
       end.
      
       find oe-rell where recid(oe-rell) eq tt-report.rec-id no-lock no-error.
       if avail oe-rell then do:    
         if index("SLI",tt-report.key-06) gt 0 then
           tt-report.key-06 = if oe-rell.b-ord-no eq 0 then "A" else "B" .
      
         find first oe-relh
             where oe-relh.company eq fi_company
               and oe-relh.r-no    eq oe-rell.r-no
             use-index r-no no-lock.
         
         find first oe-ordl
             where oe-ordl.company eq fi_company
               and oe-ordl.ord-no  eq oe-rell.ord-no
               and oe-ordl.i-no    eq oe-rell.i-no
               and oe-ordl.line    eq oe-rell.line
             no-lock.
       end.

       find first oe-ord of oe-ordl no-lock no-error.
    
       if avail oe-ord then
        find first cust
            where cust.company eq fi_company
              and cust.cust-no eq oe-ord.cust-no
            no-lock no-error.
       
        if avail oe-relh then
          assign
           v-qty     = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty
           v-date    = oe-relh.rel-date.
        else
        if avail oe-rel then
          assign
           v-qty     = oe-rel.qty 
           v-date    = oe-rel.rel-date.

    if avail oe-ordl then do:
      find first itemfg
          where itemfg.company eq fi_company
            and itemfg.i-no    eq oe-ordl.i-no
          NO-LOCK NO-ERROR.

      IF AVAIL itemfg THEN
      DO:
         create w-ord.
         assign
          w-ord.cost      = oe-ordl.cost
          w-ord.price     = oe-ordl.t-price / oe-ordl.qty
          w-ord.rel-qty   = v-qty
          w-ord.t-price   = w-ord.price * w-ord.rel-qty
          w-ord.rel-date  = v-date
          w-ord.msf       = w-ord.rel-qty * itemfg.t-sqft / 1000
          w-ord.tons      = itemfg.weight-100 * oe-ordl.qty / 100 / 2000.
      END.
    END.
   END. /*each tt-report*/

   FOR EACH w-ord,
       FIRST tt-raw-op WHERE
             tt-raw-op.DATE EQ w-ord.rel-date:

       ASSIGN
        tt-raw-op.rel-dollars = tt-raw-op.rel-dollars + w-ord.t-price
        tt-raw-op.rel-qty = tt-raw-op.rel-qty 
                          + w-ord.rel-qty
        tt-raw-op.rel-qty-msf = tt-raw-op.rel-qty-msf 
                              + w-ord.msf
        tt-raw-op.rel-qty-tons = tt-raw-op.rel-qty-tons
                               + w-ord.tons
        v-rel-gp = (IF w-ord.t-price NE 0 THEN
                   ((w-ord.t-price - (w-ord.cost * (w-ord.rel-qty / 1000) ) )  
                     / w-ord.t-price * 100)
                   ELSE 0)
        v-rel-gp = IF v-rel-gp EQ ? THEN 0 ELSE v-rel-gp
        tt-raw-op.rel-gp = tt-raw-op.rel-gp 
                         + v-rel-gp.
   END.
   
END PROCEDURE.
