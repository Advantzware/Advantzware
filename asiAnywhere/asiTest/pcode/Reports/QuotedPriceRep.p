

/*------------------------------------------------------------------------
    File        : QuotedPriceRep.p
    Purpose     : Quoted Price
    Main File   : cerep\r-quolst.w
    Syntax      :

    Description : Return a Dataset of Request For Quoted Price

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttQuotedPriceReport NO-UNDO
        FIELD qPrcFile AS CHAR     
        .

    DEFINE DATASET dsQuotedPriceReport FOR ttQuotedPriceReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegSalrep       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSalrep       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuotedPriceReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


    IF  prmUser         = ?        THEN ASSIGN     prmUser         = "".
    IF  prmAction       = ?        THEN ASSIGN     prmAction       = "".
    IF  prmBegCustomer  = ?        THEN ASSIGN     prmBegCustomer  = "".
    IF  prmEndCustomer  = ?        THEN ASSIGN     prmEndCustomer  = "".
    IF  prmBeginDate    = ?        THEN ASSIGN     prmBeginDate    = "".
    IF  prmEndDate      = ?        THEN ASSIGN     prmEndDate      = "".
    IF  prmBegSalrep    = ?        THEN ASSIGN     prmBegSalrep    = "".
    IF  prmEndSalrep    = ?        THEN ASSIGN     prmEndSalrep    = "".
    IF  prmOut          = ?        THEN ASSIGN     prmOut          = "".

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.
    

    DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
    DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" NO-UNDO.
    DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
    DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.    
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
    DEF STREAM excel.
    DEFINE VAR custcount AS CHAR NO-UNDO.



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

    assign
 cocode = prmComp.


 FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
 
 ASSIGN    
 v-today = TODAY . 

 FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegCustomer NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCustomer  OR prmEndCustomer = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
     usercust.company = prmComp  NO-LOCK:
    ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


 IF prmAction = "QuotedPriceRep" THEN DO:
    ASSIGN 
        begin_cust-no  = prmBegCustomer
        begin_date     = datetime(prmBeginDate) 
        begin_slsmn    = prmBegSalrep 
        end_cust-no    = prmEndCustomer 
        end_date       = datetime(prmEndDate)
        end_slsmn      = prmEndSalrep .

    ASSIGN
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE     
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "qtdprice" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "qtdprice" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "qtdprice" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttQuotedPriceReport.
        ASSIGN ttQuotedPriceReport.qPrcFile = excel-file.
    END.
    ELSE DO:
        CREATE ttQuotedPriceReport.
        ASSIGN ttQuotedPriceReport.qPrcFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
PROCEDURE run-report :
{sys/form/r-top3w.f}

str-tit = coname + " - " + loname.
{sys/inc/ctrtext.i str-tit 112}.

def var fcust like quote.cust-no NO-UNDO.
def var tcust like fcust init "zzzzzzzz" NO-UNDO.
def var fsman like quote.sman NO-UNDO.
def var tsman like fsman init "zzz" NO-UNDO.
def var fdate as date format "99/99/9999" init 01/01/01 NO-UNDO.
def var tdate like fdate init TODAY NO-UNDO.

def var v-cst as log format "yes/no" init NO NO-UNDO.

def var v-cst-hdr as char format "x(60)" NO-UNDO.
def var v-lines as INT NO-UNDO.
def var v-price as DEC NO-UNDO.
def var v-rels as int format ">>>" no-undo.
def var v-sname like sman.sname no-undo.

find first quote no-lock no-error.
find first cust no-lock no-error.
find first sman no-lock no-error.

/* gdm - 10130808 */
DEF VAR v_exclhdr1 AS CHAR NO-UNDO.
DEF VAR v_exclhdr2 AS CHAR NO-UNDO.
DEF VAR v_exclhdr3 AS CHAR NO-UNDO.

/* gdm - 10130808 */
DEF VAR v_billto AS CHAR             NO-UNDO.
DEF VAR v_addr1  AS CHAR             NO-UNDO.
DEF VAR v_addr2  AS CHAR             NO-UNDO.  
DEF VAR v_city   LIKE cust.city      NO-UNDO.
DEF VAR v_st     LIKE cust.state     NO-UNDO.
DEF VAR v_zip    LIKE cust.zip       NO-UNDO.
DEF VAR v_style  LIKE eb.style       NO-UNDO.
DEF VAR v_quo-dt AS CHAR             NO-UNDO.


assign
 /*str-tit2 = trim(c-win:title) 
 {sys/inc/ctrtext.i str-tit2 112}*/

 fcust    = begin_cust-no
 tcust    = end_cust-no
 fsman    = begin_slsmn
 tsman    = end_slsmn
 fdate    = begin_date
 tdate    = end_date.

/* gdm - 10130808 */
ASSIGN
    v_exclhdr1 = 
    "Company Name,Address1,City,State,Zip,Rep," +
    "Qty,Price/M,Rel,Cust,Part#,Item Name,Size,Style,Board,Colors,Quote#,Est#,Date".

form header
     skip(1)
     fill("-",132)                          format "x(132)"
     quotehd.billto[1]                      format "x(25)"
     cust.addr[1]   when avail cust         format "x(25)"
     cust.addr[2]   when avail cust         format "x(25)"
     cust.city      when avail cust         format "x(15)"
     cust.state     when avail cust         format "x(02)"
     cust.zip       when avail cust         format "x(10)"
     v-sname                                format "x(24)"
     fill("-",132)                          format "x(132)"
     skip(1)     
   with frame r-top STREAM-IO.

assign
     str-tit2 = "Quoted Price List - by Customer by Quote#"
     {sys/inc/ctrtext.i str-tit2 112}
     str-tit3 = "FROM  " +
                string(fdate,"99/99/99") + "  TO  " +
                string(tdate,"99/99/99")
     str-tit3 = trim(str-tit3)
     {sys/inc/ctrtext.i str-tit3 130}.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

{sys/inc/outprint.i  value(lines-per-page) }

/*if td-show-parm then run show-param.

display str-tit with frame r-top.
*/
{sa/sa-sls01.i}

/* gdm - 10130808 */
IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    PUT STREAM excel UNFORMATTED
        v_exclhdr1
    SKIP.

END.
    

   for each quotehd
        where quotehd.company  eq cocode
          and quotehd.loc      eq locode
          and quotehd.cust-no  ge fcust
          and quotehd.cust-no  le tcust
          and quotehd.sman     ge fsman
          and quotehd.sman     le tsman
          and quotehd.quo-date ge fdate
          and quotehd.quo-date le tdate
        no-lock:
      create report.
      assign
       report.term-id = v-term
       report.key-01  = quotehd.cust-no
       report.key-02  = string(quotehd.q-no,"9999999999")
       report.rec-id  = recid(quotehd).
    end.

    for each report where report.term-id eq v-term,
        first quotehd where recid(quotehd)    EQ report.rec-id no-lock,
        FIRST est   WHERE est.company    EQ quotehd.company                     
                      AND est.est-no     EQ quotehd.est-no NO-LOCK
        break by report.key-01: 
  
      find first cust
          where cust.company eq quotehd.company
            and cust.cust-no eq quotehd.cust-no
          no-lock no-error.     
          
      find first sman
          where sman.company eq quotehd.company
            and sman.sman    eq quotehd.sman
         no-lock no-error.
         
      if first-of(report.key-01) then do:
        v-sname = if avail sman then sman.sname else quotehd.sman.
        if first(report.key-01) then VIEW frame r-top.

        ASSIGN
            v_billto = quotehd.billto[1]
            v_addr1  = cust.addr[1]   
            v_addr2  = cust.addr[2]    
            v_city   = cust.city     
            v_st     = cust.state    
            v_zip    = cust.zip.
        
        page.
      end.
      
      for each quoteitm OF quotehd no-lock:
          
        find first eb where eb.company = quoteitm.company
                        AND eb.est-no = est.est-no                     
                        and eb.part-no eq quoteitm.part-no
                        and eb.form-no ne 0
                        no-lock no-error.
        if not avail eb then
           find first eb where eb.company = quoteitm.company
                        AND eb.est-no = est.est-no                     
                        and eb.form-no ne 0
                        no-lock no-error.
        
        if avail eb then
           find first ef where ef.company = quoteitm.company
                           AND ef.est-no   eq est.est-no
                           and ef.form-no eq eb.form-no
                           no-lock no-error.
          
        v-rels = 1.

        ASSIGN v_style  = IF AVAIL eb THEN eb.style ELSE "".              

      
      FOR EACH quoteqty WHERE quoteqty.company = quoteitm.company
                          AND quoteqty.loc = quoteitm.loc
                          AND quoteqty.q-no = quoteitm.q-no
                          AND quoteqty.LINE = quoteitm.LINE NO-LOCK:

        IF quoteqty.uom EQ "M" THEN
          v-price = quoteqty.price.
        ELSE
          RUN sys/ref/convcuom.p (quoteqty.uom, "M", 0, 0, 0, 0,
                                  quoteqty.price, OUTPUT v-price).
        
        ASSIGN v_quo-dt = STRING(quotehd.quo-date,"99/99/99").

        display quoteqty.qty        label "Qty"
                                    format ">>,>>>,>>9"
                v-price             label "Price/M"
                                    format ">>,>>9.99"
                quoteqty.rels       label "Rel"         FORMAT ">>>"
                quoteitm.part-no    label "Cust Part#"
                                    format "x(15)"
                quoteitm.part-dscr1 label "Item Name"
                                    format "x(20)"
                quoteitm.size       label "Size"
                                    format "x(10)"
                quoteitm.style      label "Style"
                                    format "x(5)"
                eb.style when avail eb @ quoteitm.style
                quoteitm.i-dscr     label "Board"
                                    format "x(10)"
                quoteitm.i-coldscr  label "Colors"
                                    format "x(20)"
                quotehd.q-no        label "Quote#"
                quotehd.est-no      label "    Est#"    FORMAT "x(8)"
                quotehd.quo-date    label "Date"        FORMAT "99/99/99"
                
            with frame detail down no-attr-space no-box STREAM-IO width 150.
            
        down with frame detail.

        /* gdm - 10130808 */
        IF tb_excel THEN
            PUT STREAM excel UNFORMATTED               
                '"' v_billto '",'            
                '"' v_addr1 + v_addr2  '",'  
                '"' v_city   '",'            
                '"' v_st     '",'            
                '"' v_zip    '",'            
                '"' v-sname  '",'           
                '"' quoteqty.qty        '",'
                '"' v-price             '",'
                '"' quoteqty.rels       '",'
                '"' quoteitm.part-no    '",'
                '"' quoteitm.part-dscr1 '",'
                '"' quoteitm.size       '",'
                '"' quoteitm.style      '",'
                '"' v_style             '",'
                '"' REPLACE(quoteitm.i-dscr,'"',' ')   '",'
                '"' quoteitm.i-coldscr  '",'
                '"' quotehd.q-no        '",'
                '"' quotehd.est-no      '",'
                '"' v_quo-dt            '"'
               SKIP.

      end.     

      END.
      put skip(1).
      
      delete report.
    end.

end procedure.
