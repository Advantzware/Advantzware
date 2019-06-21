

/*------------------------------------------------------------------------
    File        : SalesRepHighReport.p
    Purpose     :  Sales Highlights

    Syntax      :

    Description : Return a Dataset of Request Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
       

    DEFINE TEMP-TABLE ttSalesRepReport NO-UNDO
          FIELD vSalesRepFile AS CHAR              
    .

    DEFINE DATASET dsSalesRepReport FOR ttSalesRepReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDate            AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmComp            AS CHAR  NO-UNDO.
    
    DEFINE INPUT PARAMETER prmOutexcel        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSalesRepReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF  prmUser        = ?        THEN ASSIGN     prmUser       = "".
    IF  prmAction      = ?        THEN ASSIGN     prmAction     = "".
    IF  prmDate        = ?        THEN ASSIGN     prmDate       = "".
    IF  prmComp        = ?        THEN ASSIGN     prmComp       = "".     

    

    {sys/inc/var.i new shared}
    {salrep/dashrep.i NEW}

    DEF TEMP-TABLE tt-report LIKE report
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

     DEF BUFFER b-tt-report FOR tt-report.
     DEF BUFFER b-ar-invl FOR ar-invl.   

    DEF VAR v-sman-no AS CHAR NO-UNDO.
    DEF VAR fsman AS CHAR NO-UNDO.
    DEF VAR tsman AS CHAR INIT "zzz" NO-UNDO.


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

    /* prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001". */

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

        EMPTY TEMP-TABLE tt-report.
        EMPTY TEMP-TABLE tt-raw-salesmen.

        
        FOR EACH sman FIELDS(sman sname) WHERE
            sman.company EQ fi_company
            NO-LOCK:

            CREATE tt-raw-salesmen.
            ASSIGN tt-raw-salesmen.sman = sman.sman
               tt-raw-salesmen.sname = sman.sname
               tt-raw-salesmen.DATE = fi_as-of-date.
            RELEASE tt-raw-salesmen.
        END.

        RUN run-report.

        CREATE ttSalesRepReport.
        ASSIGN ttSalesRepReport.vSalesRepFile = vFileName .
        
 
 
 END.
  
 /*****************************************PROCEDURE run-report :*****************************************************/
 

 PROCEDURE run-report :
    RUN raw-salesmen-proc. /*Raw Salesmen*/ 

   
    RUN salrep\dashrep.p(INPUT prmComp,
                        INPUT prmDate).
    
    OS-DELETE VALUE("d:\webapps\invhigh.csv") . 

 end procedure.


/*****************************************PROCEDURE raw-salesmen-proc :*****************************************************/

 PROCEDURE raw-salesmen-proc :

  def var v-pct       as   dec format "99.99"                             no-undo.
  def var v-amt       LIKE ar-inv.gross  format "->,>>>,>>9.99"           no-undo.    
  def var v-cost      LIKE itemfg.t-sqft  format "->,>>9.999"             no-undo.
  def var v-sqft      like ar-inv.t-cost format "->,>>>,>>9.99"           no-undo.
  DEF VAR v-month AS INT NO-UNDO.
  DEF VAR ld-inv-pct AS DEC NO-UNDO.
  DEF VAR v-start-of-year AS DATE NO-UNDO.
  DEF VAR v-end-of-year AS DATE NO-UNDO.
  DEF VAR v-this-month AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-report.
  EMPTY TEMP-TABLE w-data.
  
  ASSIGN
     v-start-of-year = DATE(1,1,YEAR(fi_as-of-date))
     v-end-of-year   = DATE(12,31,YEAR(fi_as-of-date))
     v-this-month    = MONTH(fi_as-of-date).

  /*from HT*/
  for each ar-inv FIELDS(company cust-no x-no inv-date)
      where ar-inv.company  eq fi_company
        and ar-inv.posted   eq yes
        and ar-inv.inv-date ge v-start-of-year
        and ar-inv.inv-date le v-end-of-year
        and ar-inv.type    ne "FC"
        no-lock,
      first cust FIELDS(sman)
      where cust.company eq ar-inv.company
        and cust.cust-no eq ar-inv.cust-no
      no-lock,

      each ar-invl FIELDS(sman s-pct i-no actnum)
      where ar-invl.x-no eq ar-inv.x-no
        and (ar-invl.billable or not ar-invl.misc)
      no-lock:

      {sa/sa-sman6.i ar-inv.inv-date "ar-invl" }
  end. /*each ar-inv*/

  for each cust FIELDS(cust-no sman)
      where cust.company eq fi_company
      no-lock,
      each ar-cash FIELDS(c-no cust-no check-date)
      where ar-cash.company    eq fi_company
        and ar-cash.cust-no    eq cust.cust-no
        and ar-cash.check-date ge v-start-of-year
        and ar-cash.check-date le v-end-of-year
        and ar-cash.posted     eq yes
      no-lock,

      EACH ar-cashl FIELDS(company actnum inv-no dscr c-no amt-paid amt-disc)
      WHERE ar-cashl.c-no    EQ ar-cash.c-no
        AND ar-cashl.posted  EQ YES
        AND ar-cashl.memo    EQ YES
        AND CAN-FIND(FIRST account
                     WHERE account.company EQ ar-cashl.company
                       AND account.actnum  EQ ar-cashl.actnum
                       AND account.type    EQ "R")
      NO-LOCK:

    RELEASE ar-invl.

    RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

    IF AVAIL oe-retl THEN
    find first ar-invl
        where ar-invl.company eq fi_company
          and ar-invl.cust-no eq ar-cash.cust-no
          and ar-invl.inv-no  eq ar-cashl.inv-no
          and ar-invl.i-no    eq oe-retl.i-no
          and (ar-invl.billable or not ar-invl.misc)
        no-lock no-error.

    IF ar-cashl.inv-no NE 0                                                       AND
           (AVAIL ar-invl                             OR
            (NOT AVAIL reftable AND
             NOT ar-cashl.dscr MATCHES "*oe return*") OR
            SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
        FOR EACH b-ar-invl FIELDS(billable misc sman s-pct i-no actnum)
            WHERE b-ar-invl.company EQ ar-cashl.company
              AND b-ar-invl.cust-no EQ cust.cust-no
              AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
              AND (b-ar-invl.billable OR NOT b-ar-invl.misc)
              AND (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))
            NO-LOCK:
          {sa/sa-sman6.i ar-cash.check-date "ar-cashl" "b-"}
        end.

        else
        do:
          create tt-report.
          assign
           tt-report.key-02  = cust.sman
           tt-report.rec-id  = recid(ar-cashl)
           tt-report.DATE    = ar-cash.check-date.
        end.
  end. /*each cust*/

  FOR EACH tt-report,
      FIRST tt-raw-salesmen WHERE
            tt-raw-salesmen.sman EQ tt-report.key-02
      break by tt-report.key-02:

      find first w-data
          where w-data.w-sman-no eq tt-report.key-02
          no-lock no-error.

      if not avail w-data then do:
        create w-data.
        w-data.w-sman-no = tt-report.key-02.
      end.

      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

      if avail ar-invl then do:
         find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

         find first itemfg
              where itemfg.company eq fi_company
              and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.

         assign
           v-pct  = 1
           v-amt  = ar-invl.amt
           v-cost = ar-invl.t-cost
           v-sqft = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                    else
                    if avail itemfg then
                      (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0.

         if v-amt  eq ? then v-amt  = 0.
         if v-cost eq ? then v-cost = 0.
         if v-sqft eq ? then v-sqft = 0.
        
         do i = 1 to 3:
           if ar-invl.sman[i] eq tt-report.key-02 then
             assign
              v-pct = ar-invl.s-pct[i] / 100
              i     = 3.
         end.
        
         if v-pct eq 0 then
         do i = 1 to 3:
           if i eq 1 then j = 0.
           if ar-invl.sman[i] ne "" then j = j + 1.
           if i eq 3 then v-pct = 1 / j.
         end.
        
         if v-pct le 0 or v-pct eq ? then v-pct = 1.
        
         IF ar-inv.inv-date EQ tt-raw-salesmen.DATE THEN
            assign
              w-data.w-sqft[1] = w-data.w-sqft[1] + (v-sqft * v-pct)
              w-data.w-amt[1]  = w-data.w-amt[1]  + (v-amt  * v-pct)
              w-data.w-cost[1] = w-data.w-cost[1] + (v-cost * v-pct).

         IF ar-inv.inv-date LE fi_as-of-date THEN
         DO:
            IF MONTH(ar-inv.inv-date) EQ v-this-month THEN
               assign
                 w-data.w-sqft[2] = w-data.w-sqft[2] + (v-sqft * v-pct)
                 w-data.w-amt[2]  = w-data.w-amt[2]  + (v-amt  * v-pct)
                 w-data.w-cost[2] = w-data.w-cost[2] + (v-cost * v-pct).
            
            assign
              w-data.w-sqft[3] = w-data.w-sqft[3] + (v-sqft * v-pct)
              w-data.w-amt[3]  = w-data.w-amt[3]  + (v-amt  * v-pct)
              w-data.w-cost[3] = w-data.w-cost[3] + (v-cost * v-pct).
         END.

         assign
            w-data.w-sqft[4] = w-data.w-sqft[4] + (v-sqft * v-pct)
            w-data.w-amt[4]  = w-data.w-amt[4]  + (v-amt  * v-pct).
         
      END.
      ELSE DO:
         find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

         if avail ar-cashl then do:
           find ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.
        
           assign
            v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
            v-cost = 0
            v-sqft = 0
            v-pct  = 1.
        
           RELEASE itemfg.
           RELEASE ar-invl.
           RELEASE oe-retl.

           FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

           IF NOT AVAIL ar-invl THEN
              RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

           IF AVAIL oe-retl THEN DO:
             find first itemfg
                 where itemfg.company eq fi_company
                   and itemfg.i-no    eq oe-retl.i-no
                 no-lock no-error.
        
             v-sqft = if avail itemfg then
                        oe-retl.tot-qty-return * itemfg.t-sqft / 1000
                      else 0.

             if v-sqft eq ? then v-sqft = 0.

             RUN salrep/salecost.p(3,
                                   ROWID(ar-invl),
                                   oe-retl.job-no,
                                   oe-retl.job-no,
                                   oe-retl.tot-qty-return,
                                   OUTPUT v-cost).
           END.
           ELSE
           IF AVAIL ar-invl THEN DO:
              ld-inv-pct = 0.
              FOR EACH b-ar-invl FIELDS(amt) WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
                ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
                ACCUMULATE 1 (TOTAL).
              END.
              ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                              (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                           ELSE (ACCUM TOTAL 1))
                           ELSE (ar-invl.amt / ld-inv-pct).
            
              IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.
            
              v-amt = v-amt * ld-inv-pct.
            
              if v-sqft eq ? then v-sqft = 0.
            
              do i = 1 to 3:
                if ar-invl.sman[i] eq tt-report.key-02 then
                  assign
                   v-pct = ar-invl.s-pct[i] / 100
                   i     = 3.
              end.
            
              if v-pct eq 0 then
              do i = 1 to 3:
                if i eq 1 then j = 0.
                if ar-invl.sman[i] ne "" then j = j + 1.
                if i eq 3 then v-pct = 1 / j.
              end.
            
              if v-pct le 0 or v-pct eq ? then v-pct = 1.
           end.
           
           IF ar-cash.check-date EQ tt-raw-salesmen.DATE THEN
            assign
              w-data.w-sqft[1] = w-data.w-sqft[1] - (v-sqft * v-pct)
              w-data.w-amt[1]  = w-data.w-amt[1]  + (v-amt  * v-pct)
              w-data.w-cost[1] = w-data.w-cost[1] - (v-cost * v-pct).

           IF ar-cash.check-date LE fi_as-of-date THEN
           DO:
              IF MONTH(ar-cash.check-date) EQ v-this-month THEN
               assign
                 w-data.w-sqft[2] = w-data.w-sqft[2] - (v-sqft * v-pct)
                 w-data.w-amt[2]  = w-data.w-amt[2]  + (v-amt  * v-pct)
                 w-data.w-cost[2] = w-data.w-cost[2] - (v-cost * v-pct).

               assign
                 w-data.w-sqft[3] = w-data.w-sqft[3] - (v-sqft * v-pct)
                 w-data.w-amt[3]  = w-data.w-amt[3]  + (v-amt  * v-pct)
                 w-data.w-cost[3] = w-data.w-cost[3] - (v-cost * v-pct).
           END.

           assign
            w-data.w-sqft[4] = w-data.w-sqft[4] - (v-sqft * v-pct)
            w-data.w-amt[4]  = w-data.w-amt[4]  + (v-amt  * v-pct).

         END.
      END.

      IF LAST-OF(tt-report.key-02) THEN
      DO:
         ASSIGN
            v-month = MONTH(tt-report.DATE)
            tt-raw-salesmen.date-msf = tt-raw-salesmen.date-msf + w-data.w-sqft[1]
            tt-raw-salesmen.date-amt = tt-raw-salesmen.date-amt + w-data.w-amt[1]
            tt-raw-salesmen.date-sf = tt-raw-salesmen.date-sf + (w-data.w-sqft[1] * 1000)
            tt-raw-salesmen.date-cost = tt-raw-salesmen.date-cost + w-data.w-cost[1]
            tt-raw-salesmen.mtd-msf = tt-raw-salesmen.mtd-msf + w-data.w-sqft[2] 
            tt-raw-salesmen.mtd-amt = tt-raw-salesmen.mtd-amt + w-data.w-amt[2]
            tt-raw-salesmen.mtd-sf = tt-raw-salesmen.mtd-sf + (w-data.w-sqft[2] * 1000)
            tt-raw-salesmen.mtd-cost = tt-raw-salesmen.mtd-cost + w-data.w-cost[2]
            tt-raw-salesmen.ytd-msf = tt-raw-salesmen.ytd-msf + w-data.w-sqft[3]
            tt-raw-salesmen.ytd-amt = tt-raw-salesmen.ytd-amt + w-data.w-amt[3]
            tt-raw-salesmen.ytd-sf = tt-raw-salesmen.ytd-sf + (w-data.w-sqft[3] * 1000)
            tt-raw-salesmen.ytd-cost = tt-raw-salesmen.ytd-cost + w-data.w-cost[3]
            tt-raw-salesmen.amt[v-month] = tt-raw-salesmen.amt[v-month] + w-data.w-amt[4]
            tt-raw-salesmen.msf[v-month] = tt-raw-salesmen.msf[v-month] + w-data.w-sqft[4].
        
         DELETE w-data.
      END.
  END.

  FOR EACH tt-raw-salesmen:

      ASSIGN
         tt-raw-salesmen.date-profit = IF tt-raw-salesmen.date-amt NE 0 THEN
                                          (tt-raw-salesmen.date-amt - tt-raw-salesmen.date-cost) /
                                          tt-raw-salesmen.date-amt * 100
                                       ELSE 0
         tt-raw-salesmen.mtd-profit = IF tt-raw-salesmen.mtd-amt NE 0 THEN
                                         (tt-raw-salesmen.mtd-amt - tt-raw-salesmen.mtd-cost) /
                                         tt-raw-salesmen.mtd-amt * 100
                                      ELSE 0
         tt-raw-salesmen.ytd-profit = IF tt-raw-salesmen.ytd-amt NE 0 THEN
                                         (tt-raw-salesmen.ytd-amt - tt-raw-salesmen.ytd-cost) /
                                         tt-raw-salesmen.ytd-amt * 100
                                      ELSE 0.
  END.  

END PROCEDURE.
