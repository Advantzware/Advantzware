/*------------------------------------------------------------------------
    File        : InvoiceHighReport.p
    Purpose     :  Invoice Highlights

    Syntax      :

    Description : Return a Dataset of Request Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  /* File connect to data base asi - 3800 nosweat - 3802 rfq - 3811 emptrack - 3808 */
{custom/xprint.i}
       

    DEFINE TEMP-TABLE ttInvoiceReport NO-UNDO
          FIELD vInvoiceFile AS CHAR              
    .

    DEFINE DATASET dsInvoiceReport FOR ttInvoiceReport .

    DEFINE VAR  prmUser            AS CHARACTER  NO-UNDO.
    DEFINE VAR  prmAction          AS CHARACTER NO-UNDO.
    DEFINE VAR  prmDate            AS CHARACTER NO-UNDO. 
    DEFINE VAR  prmComp            AS CHAR  NO-UNDO.
    
    DEFINE VAR  prmOutexcel        AS CHARACTER NO-UNDO.
    /*DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvoiceReport.*/
    DEFINE VAR cError  AS CHAR NO-UNDO.
    ASSIGN  
        prmUser    = "Admin"
        prmAction   = "RunReport"
        prmDate     = "08/20/2011"
        prmComp     = "001" .



    IF  prmUser        = ?        THEN ASSIGN     prmUser       = "".
    IF  prmAction      = ?        THEN ASSIGN     prmAction     = "".
    IF  prmDate        = ?        THEN ASSIGN     prmDate       = "".
    IF  prmComp        = ?        THEN ASSIGN     prmComp       = "".     

    

    {sys/inc/var.i new shared}
    {salrep/dashinv.i NEW}

    DEF TEMP-TABLE tt-report NO-UNDO LIKE report
        FIELD DATE AS DATE
        FIELD row-id AS ROWID
        FIELD qty AS DEC
        FIELD amt       LIKE ar-invl.amt        FORMAT "->>>>>>>9.99"
        FIELD cash-date LIKE ar-inv.inv-date
        FIELD misc AS LOG
        FIELD cost AS DEC
        FIELD msf AS DEC.


   DEFINE VARIABLE fi_as-of-date AS DATE FORMAT "99/99/9999"   NO-UNDO.                                                            
   DEFINE VARIABLE fi_company AS CHARACTER FORMAT "X(3)"    NO-UNDO.
   DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
   

    
    DEFINE NEW SHARED VAR v-webrootpath AS CHAR NO-UNDO.
    DEFINE  NEW SHARED VARIABLE vFileName AS CHAR NO-UNDO.
    DEFINE VARIABLE init-dir AS CHAR NO-UNDO.
    DEFINE VARIABLE  v-excel-file    AS CHARACTER   NO-UNDO.

   
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
        EMPTY TEMP-TABLE tt-raw-sales.

        IF NOT CAN-FIND(FIRST company WHERE
            company.company EQ fi_company) THEN
        DO:
            cError =  "Invalid Company." .
            RETURN.          
        END.           

        RUN run-report.        

        CREATE ttInvoiceReport.
        ASSIGN ttInvoiceReport.vInvoiceFile = vFileName .
        
 
 
 END.
  
 /*****************************************PROCEDURE run-report :*****************************************************/
 

 PROCEDURE run-report :

    RUN raw-sales-proc. /*Raw Sales*/
   
    RUN salrep\dashinv.p(INPUT fi_company,
                        INPUT fi_as-of-date).                                 
                           
 end procedure.




  /*****************************************PROCEDURE raw-sales-proc :*****************************************************/

 
PROCEDURE raw-sales-proc :

   DEF VAR from-date AS DATE NO-UNDO.
   DEF VAR to-date AS DATE NO-UNDO.
   DEF VAR ld-cost AS DEC NO-UNDO.
   DEF VAR date-index AS DATE NO-UNDO.

   EMPTY TEMP-TABLE tt-report.
   EMPTY TEMP-TABLE tt-raw-sales.

   ASSIGN from-date = DATE(1,1,YEAR(fi_as-of-date) - 1)
          to-date = DATE(12,31,YEAR(fi_as-of-date)).

   DO date-index = from-date TO to-date:

      CREATE tt-raw-sales.
      ASSIGN tt-raw-sales.DATE = date-index.
      RELEASE tt-raw-sales.
   END.

   for each cust WHERE
       cust.company eq fi_company
       no-lock:

       /*{sa/sa-sls03.i from-date to-date}*/
       for each ar-inv FIELDS() WHERE
           ar-inv.company  eq fi_company AND
           ar-inv.posted   eq yes AND
           ar-inv.cust-no  eq cust.cust-no AND
           ar-inv.inv-date ge from-date AND
           ar-inv.inv-date le to-date AND
           ar-inv.type    ne "FC" 
           no-lock:
      
           create tt-report.
           assign
             tt-report.key-09  = cust.cust-no
             tt-report.key-10  = "ar-inv"
             tt-report.rec-id  = recid(ar-inv).
       end.

       for each ar-cash FIELDS(c-no) WHERE 
           ar-cash.company    eq fi_company AND
           ar-cash.cust-no    eq cust.cust-no AND
           ar-cash.check-date ge from-date AND
           ar-cash.check-date le to-date AND
           ar-cash.posted     eq yes
           no-lock,
           EACH ar-cashl FIELDS(company actnum) WHERE
                ar-cashl.c-no    EQ ar-cash.c-no AND
                ar-cashl.posted  EQ YES AND
                ar-cashl.memo    EQ YES AND
                CAN-FIND(FIRST account WHERE
                         account.company EQ ar-cashl.company AND
                         account.actnum  EQ ar-cashl.actnum AND
                         account.type    EQ "R")
           NO-LOCK:

           create tt-report.
           assign
              tt-report.key-09  = cust.cust-no
              tt-report.key-10  = "ar-cashl"
              tt-report.rec-id  = recid(ar-cashl).
       end.
   end.

   FOR each tt-report:

    if tt-report.key-10 eq "ar-inv" then do:
      find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

      FIND FIRST tt-raw-sales WHERE
           tt-raw-sales.DATE EQ ar-inv.inv-date.

      for each ar-invl
          where ar-invl.x-no    eq ar-inv.x-no
          no-lock:

        find first itemfg
            where itemfg.company eq fi_company
              and itemfg.i-no    eq ar-invl.i-no
            no-lock no-error.
        
        RUN salrep/salecost.p (3, /*Invoice Cost*/
                               ROWID(ar-invl),
                               ar-invl.job-no,
                               ar-invl.job-no2,
                               ar-invl.ship-qty,
                               OUTPUT ld-cost).

        assign
         tt-raw-sales.date-qty = tt-raw-sales.date-qty +
                                 ar-invl.ship-qty
         tt-raw-sales.date-msf = tt-raw-sales.date-msf +
                                 (if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                                  else
                                  if avail itemfg then
                                    (ar-invl.ship-qty * itemfg.t-sqft / 1000)
                                  else 0)
         tt-raw-sales.date-cost  = tt-raw-sales.date-cost + ld-cost
         tt-raw-sales.date-tons = tt-raw-sales.date-tons +
                                  ((if ar-invl.t-weight ne 0 then ar-invl.t-weight
                                    else
                                    if avail itemfg then
                                      (ar-invl.ship-qty * itemfg.weight-100 / 100)
                                    else 0) / 2000)
        tt-raw-sales.date-amt = tt-raw-sales.date-amt + ar-invl.amt.
      end.
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
      find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
      find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

      FIND FIRST tt-raw-sales WHERE
           tt-raw-sales.DATE EQ ar-cash.check-date.

      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

      IF AVAIL oe-retl THEN DO:
        find first ar-invl
            where ar-invl.company eq fi_company
              and ar-invl.cust-no eq ar-cash.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
              and (ar-invl.billable or not ar-invl.misc)
            no-lock no-error.

        if avail ar-invl then do:
         
          find first itemfg
              where itemfg.company eq fi_company
                and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.
          
          RUN salrep/salecost.p (3, /*Invoice Cost*/
                                 ROWID(ar-invl),
                                 oe-retl.job-no,
                                 oe-retl.job-no2,
                                 oe-retl.tot-qty-return,
                                 OUTPUT ld-cost).

          assign
           tt-raw-sales.date-qty = tt-raw-sales.date-qty -
                                   oe-retl.tot-qty-return
           tt-raw-sales.date-msf = tt-raw-sales.date-msf -
                                  (if avail itemfg then
                                  (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                                  else 0)
           tt-raw-sales.date-tons = tt-raw-sales.date-tons +
                                    ((if avail itemfg then
                                     (oe-retl.tot-qty-return * itemfg.weight-100 / 100)
                                     else 0) / 2000)
           tt-raw-sales.date-cost = tt-raw-sales.date-cost + ld-cost.
          
        end.
      end.

      tt-raw-sales.date-amt = tt-raw-sales.date-amt + (ar-cashl.amt-paid - ar-cashl.amt-disc).
    end.
   end.

    FOR EACH tt-raw-sales:
        tt-raw-sales.date-net-profit = IF tt-raw-sales.date-amt NE 0 THEN
                                          (tt-raw-sales.date-amt - tt-raw-sales.date-cost) /
                                           tt-raw-sales.date-amt
                                       ELSE 0.
    END.

END PROCEDURE.


  
























/*DEFINE TEMP-TABLE ttbrwsWarehouseCust  NO-UNDO
    FIELD  vCompany     AS CHAR       FORMAT "x(3)"     
    FIELD  vCust        AS CHAR       FORMAT "x(8)"     
    FIELD  vShipid      AS CHAR       FORMAT "x(8)"    
    FIELD  vSName       AS CHAR       FORMAT "x(30)"  
    FIELD  vShipAddr1   AS CHAR       FORMAT "x(30)"  
    FIELD  vShipAddr2   AS CHAR       FORMAT "x(30)" 
    FIELD  vShipCity    AS CHAR       FORMAT "x(15)" 
    FIELD  vShipState   AS CHAR       FORMAT "x(2)"
    FIELD  vShipZip     AS CHAR       FORMAT "x(10)"
    FIELD  vShiploc     AS CHAR       FORMAT "x(5)"
    FIELD  vShipCarr    AS CHAR       FORMAT "x(5)"
    FIELD  vShipPhone   AS CHAR      
    FIELD  vShipFax     AS CHAR      
    FIELD  vShipCont    AS CHAR       FORMAT "x(25)"
    .
DEFINE DATASET dsbrwsWarehouseCust FOR ttbrwsWarehouseCust.


    run FgViewAdjustment.p(INPUT "admin", INPUT "DeleteCustPlant", INPUT "ATT1000" ,INPUT "ATT1000", input-output dataset dsbrwsWarehouseCust).

for each ttbrwsWarehouseCust no-lock:
display ttbrwsWarehouseCust.vShipid.
    
    .
end.*/
      /*
DEFINE TEMP-TABLE ttSalesmanRep1 NO-UNDO
    FIELD vsalesman AS CHARACTER .
DEFINE DATASET dsSalesmanRep1 FOR ttSalesmanRep1.
 
    DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

    run SalesPerRep.p(INPUT "admin", INPUT "Salesman", INPUT date(05/12/2011) ,INPUT "",INPUT "zzz", INPUT "Yes", INPUT-OUTPUT dataset dsSalesmanRep1,OUTPUT cError).

    /*for each ttSalesmanRep no-lock:
display ttSalesmanRep.vsalesman.
END.*/ */

    /*DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
    DEFINE STREAM excel.
    OUTPUT STREAM excel TO VALUE("W:/pdfs/testcsv.csv").

     excelheader = 
    "No,Sales Rep Name,Daily Sq Ft/M,Amount,PTD Sq Ft/M,Amount,YTD Sq Ft/M,Amount".
     PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.*/
