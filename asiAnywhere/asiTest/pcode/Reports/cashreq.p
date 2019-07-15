

/*------------------------------------------------------------------------
    File        : cashreq.p
    Purpose     : Cash Requirements
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttCashRequirements NO-UNDO
        FIELD cash AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsCashRequirements FOR ttCashRequirements.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmcash          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegcom        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendcom        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt1        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt2        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt3        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmshrtby        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmdiscdt        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmcompny        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCashRequirements.

     IF prmUser     = ? THEN ASSIGN    prmUser      = "".   
     IF prmcash     = ? THEN ASSIGN    prmcash      = "". 
     IF prmbegcom   = ? THEN ASSIGN    prmbegcom    = "". 
     IF prmendcom   = ? THEN ASSIGN    prmendcom    = "". 
     IF prmbegdt1   = ? THEN ASSIGN    prmbegdt1    = "".
     IF prmbegdt2   = ? THEN ASSIGN    prmbegdt2    = "". 
     IF prmbegdt3   = ? THEN ASSIGN    prmbegdt3    = "". 
     IF prmshrtby   = ? THEN ASSIGN    prmshrtby    = "".
     IF prmdiscdt   = ? THEN ASSIGN    prmdiscdt    = "". 
     IF prmcompny   = ? THEN ASSIGN    prmcompny    = "".
     IF prmOut      = ? THEN ASSIGN    prmOut       = "".

DEFINE VARIABLE begin_company   AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE begin_date-1    AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_date-2    AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_date-3    AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE end_company     AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" no-UNDO.
DEFINE VARIABLE tb_disc-date    AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_show-company AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_sort         AS LOGICAL INITIAL NO NO-UNDO.

DEF VAR g_company AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
def var list-name as cha no-undo.
def var list-name2 as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.
DEF VAR t-setup AS LOG INITIAL NO NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE tb_excel      AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEF VAR lv-list-name LIKE list-name EXTENT 2 NO-UNDO.
DEFINE VARIABLE tb_runExcel   AS LOGICAL INITIAL no NO-UNDO.
DEF VAR lv-txt-file AS cha NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)" NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .


assign
 cocode = prmComp 
 g_company = prmComp
 vuser     = prmUser .

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
     NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    NO-ERROR.
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


DEF TEMP-TABLE tt-report NO-UNDO LIKE report.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF STREAM excel.

 assign
   begin_date-1 = today
   begin_date-2 = begin_date-1 + 7
   begin_date-3 = begin_date-2 + 7.



  IF prmcash = "cash" THEN DO:
     MESSAGE "hello ". 
        ASSIGN
        v-today          = TODAY
        begin_company    = prmbegcom 
        begin_date-1     = date(prmbegdt1)
        begin_date-2     = DATE(prmbegdt2)
        begin_date-3     = date(prmbegdt3) 
        end_company      = prmendcom
        tb_disc-date     = IF prmdiscdt = "Yes" THEN TRUE ELSE FALSE
        tb_show-company  = IF prmcompny = "Yes" THEN TRUE ELSE FALSE  
        tb_sort          = IF prmshrtby = "Yes" THEN TRUE ELSE FALSE . 
    
       assign
        init-dir    = v-webrootpath
        lv-txt-file =  'CashReq' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME)  + ".txt" .
       
        
        v-excel-file =  'CashReq' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME)  + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .


        
        run run-report. 

        CREATE ttCashRequirements.

        IF prmOut = "Yes" THEN
        ASSIGN ttCashRequirements.cash = v-excel-file.
        ELSE
            ASSIGN ttCashRequirements.cash = lv-txt-file .

 


  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* --------------------------------------------------- ap/ap-flow.p 12/92 cd  */
/*                                                                            */
/* a/p - cash requirements report                                             */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var ws_gross like ap-inv.net no-undo.
def var b-comp as CHAR format "x(3)" no-undo.
def var e-comp as CHAR format "x(3)" no-undo.
def var d1 as date extent 3 format "99/99/9999" no-undo.
def var ni as int no-undo.
def var vend-t as dec extent 4 format "->>>>>.99" no-undo.
def var vend-d as dec extent 4 format "->>>>.99" no-undo.
def var inv-t as dec format "->>>>>>9.99" extent 4 no-undo.
def var inv-d like vend-d no-undo.
def var grand-t like vend-t no-undo.
def var grand-d like vend-d no-undo.
def var s as int no-undo.
def var ag as dec no-undo.
def var amt like ag no-undo.
def var t1 as dec format "$->>>,>>>.99" no-undo.
def var c1 as dec format "$->>>,>>>.99" no-undo.
def var m1 as char format "x(20)" no-undo.
def var m2 as char format "x(20)" no-undo.
def var m3 as char format "x(20)" no-undo.
def var first-time as log init yes no-undo.
def var ws_disc-avail as dec no-undo column-label "Disc"
    format '->>>>.99'.
def var v-sort as log init no no-undo.
def var v-disc as log init no no-undo.
def var v-disc-date as date no-undo.
def var v-company as log init no no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.

form header
     d1[1] to 50 d1[2] to 72 d1[3] to 94 "Beyond" to 114 "Total" to 137 skip
     "Invoice#     Inv Date/Due Date       Gross     Disc       Gross    "
     "Disc       Gross     Disc       Gross     Disc         Gross     Disc company"
     skip
     fill("_",145) format "x(145)"
     
    with page-top frame b-top stream-io width 145 no-box.
    
form ap-inv.inv-no
     ap-inv.inv-date format "99/99/99"
     ap-inv.due-date format "99/99/99"
     inv-t[1]
     inv-d[1] format "->>>>.99" 
     inv-t[2]
     inv-d[2] format "->>>>.99" 
     inv-t[3]
     inv-d[3] format "->>>>.99" 
     inv-t[4]
     inv-d[4] format "->>>>.99" 
     ws_gross
     ws_disc-avail
     ap-inv.company FORMAT "x(8)"
     
    with stream-io width 150 frame b no-labels down no-box.

form header
     d1[1] to 50 d1[2] to 72 d1[3] to 94 "Beyond" to 114 "Total" to 137 skip
     "Invoice#     Inv Date/Due Date       Gross     Disc       Gross    "
     "Disc       Gross     Disc       Gross     Disc         Gross     Disc"
     skip
     fill("_",137) format "x(137)"
     
    with page-top frame f-top stream-io width 137 no-box.
    
form ap-inv.inv-no
     ap-inv.inv-date format "99/99/99"
     ap-inv.due-date format "99/99/99"
     inv-t[1]
     inv-d[1] format "->>>>.99" 
     inv-t[2]
     inv-d[2] format "->>>>.99" 
     inv-t[3]
     inv-d[3] format "->>>>.99" 
     inv-t[4]
     inv-d[4] format "->>>>.99" 
     ws_gross
     ws_disc-avail
     
    with stream-io width 137 frame a no-labels down no-box.

EMPTY TEMP-TABLE tt-report.

assign
 str-tit2 = "AP Cash Requirements Report" 
 {sys/inc/ctrtext.i str-tit2 112}
 
 b-comp = begin_company
 e-comp = end_company
 d1[1]  = begin_date-1
 d1[2]  = begin_date-2
 d1[3]  = begin_date-3
 v-sort = tb_sort
 v-disc = tb_disc-date
 v-company = tb_show-company . 

/*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.


{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  IF NOT v-company THEN DO:
  excelheader = "Vendor#,Vendor Name,Invoice#,Inv Date,Due Date,"
              + STRING(d1[1]) + " Gross," + STRING(d1[1]) + " Disc,"
              + STRING(d1[2]) + " Gross," + STRING(d1[2]) + " Disc,"
              + STRING(d1[3]) + " Gross," + STRING(d1[3]) + " Disc,"
              + "Beyond Gross,Beyond Disc,Gross,Total Disc".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.
  ELSE DO:
       excelheader = "Vendor#,Vendor Name,Invoice#,Inv Date,Due Date,"
              + STRING(d1[1]) + " Gross," + STRING(d1[1]) + " Disc,"
              + STRING(d1[2]) + " Gross," + STRING(d1[2]) + " Disc,"
              + STRING(d1[3]) + " Gross," + STRING(d1[3]) + " Disc,"
              + "Beyond Gross,Beyond Disc,Gross,Total Disc,Company".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.
END.



   FOR EACH company WHERE
       company.company GE b-comp AND
       company.company LE e-comp
       NO-LOCK,
       each ap-inv      
       where ap-inv.company EQ company.company
        and ap-inv.posted  eq yes
        and ap-inv.due     ne 0
      no-lock,
      
      first vend
      where vend.company eq ap-inv.company 
        and vend.vend-no eq ap-inv.vend-no
      no-lock:

    create tt-report.
    assign
     tt-report.key-01  = if v-sort then vend.name else ""
     tt-report.key-02  = vend.vend-no
     tt-report.key-03  = ap-inv.inv-no
     tt-report.rec-id  = recid(ap-inv).
  end.
  display "" with frame r-top.
  IF v-company THEN
      display "" with frame b-top.
  ELSE
      display "" with frame f-top.

  for each tt-report,
      first ap-inv where recid(ap-inv) eq tt-report.rec-id no-lock
      
      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03:
            
    find first terms where terms.t-code eq ap-inv.terms no-lock no-error.

    find first vend
        where vend.company eq ap-inv.company
          and vend.vend-no eq ap-inv.vend-no
        no-lock no-error.
    
    if first-of (tt-report.key-02) then do:
      put ap-inv.vend-no.
      if avail vend then put vend.name.
      put skip.
    end.
    
    assign
     ws_gross      = ap-inv.due
     ws_disc-avail = if ap-inv.net ne 0 then
                       (ap-inv.net * (ap-inv.disc-% / 100) - ap-inv.disc-taken)
                     else 0.
                     
    do i = 1 to 4:
      assign
       inv-t[i] = 0
       inv-d[i] = 0.
    end.
                     
    if ap-inv.due-date gt d1[3] then
      assign
       vend-t[4] = vend-t[4] + ws_gross
       inv-t[4]  = ws_gross.
       
    else
    if ap-inv.due-date gt d1[2] then
      assign
       vend-t[3] = vend-t[3] + ws_gross
       inv-t[3]  = ws_gross.
    
    else
    if ap-inv.due-date gt d1[1] then
      assign
       vend-t[2] = vend-t[2] + ws_gross
       inv-t[2]  = ws_gross.
    
    else
      assign
       vend-t[1] = vend-t[1] + ws_gross
       inv-t[1]  = ws_gross.
       
    v-disc-date = if avail terms then
                    (ap-inv.inv-date + terms.disc-days) else ap-inv.due-date.
       
    if v-disc-date gt d1[3] then
      assign
       vend-d[4] = vend-d[4] + ws_disc-avail
       inv-d[4]  = ws_disc-avail.
              
    else
    if v-disc-date gt d1[2] then
      assign
       vend-d[3] = vend-d[3] + ws_disc-avail
       inv-d[3]  = ws_disc-avail.

    else
    if v-disc-date gt d1[1] then
      assign
       vend-d[2] = vend-d[2] + ws_disc-avail
       inv-d[2]  = ws_disc-avail.
    
    else
      assign
       vend-d[1] = vend-d[1] + ws_disc-avail
       inv-d[1]  = ws_disc-avail.
  IF NOT v-company THEN do:
    display ap-inv.inv-no
            ap-inv.inv-date
            ap-inv.due-date
              ap-inv.inv-date + ap-inv.disc-days
                when ws_disc-avail ne 0 and v-disc @ ap-inv.due-date
            inv-t[1] when inv-t[1] ne 0
            inv-d[1] when inv-d[1] ne 0
            inv-t[2] when inv-t[2] ne 0
            inv-d[2] when inv-d[2] ne 0
            inv-t[3] when inv-t[3] ne 0
            inv-d[3] when inv-d[3] ne 0
            inv-t[4] when inv-t[4] ne 0
            inv-d[4] when inv-d[4] ne 0
            ws_gross
            ws_disc-avail
           
        with frame a.
    down with frame a.
  END. 
   ELSE do:
    display ap-inv.inv-no
            ap-inv.inv-date
            ap-inv.due-date
              ap-inv.inv-date + ap-inv.disc-days
                when ws_disc-avail ne 0 and v-disc @ ap-inv.due-date
            inv-t[1] when inv-t[1] ne 0
            inv-d[1] when inv-d[1] ne 0
            inv-t[2] when inv-t[2] ne 0
            inv-d[2] when inv-d[2] ne 0
            inv-t[3] when inv-t[3] ne 0
            inv-d[3] when inv-d[3] ne 0
            inv-t[4] when inv-t[4] ne 0
            inv-d[4] when inv-d[4] ne 0
            ws_gross
            ws_disc-avail
            ap-inv.company
            
        with frame b.
    down with frame b.
   END.
    
    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
          '"' IF first-of(tt-report.key-02) THEN
                 ap-inv.vend-no ELSE ""                                    '",'
          '"' IF FIRST-OF(tt-report.key-02) AND    
                 avail vend then vend.NAME ELSE ""                         '",'
          '"' ap-inv.inv-no                                                '",'
          '"' IF ap-inv.inv-date NE ? THEN
                 STRING(ap-inv.inv-date) ELSE ""                           '",'
          '"' IF ws_disc-avail ne 0 and v-disc THEN
                 STRING(ap-inv.inv-date + ap-inv.disc-days)               
              ELSE IF ap-inv.due-date NE ? THEN
                   STRING(ap-inv.due-date) ELSE ""                         '",'
          '"' IF inv-t[1] ne 0 THEN STRING(inv-t[1],"->>>>>>9.99") ELSE "" '",'
          '"' IF inv-d[1] ne 0 THEN STRING(inv-d[1],"->>>>.99") ELSE ""    '",'
          '"' IF inv-t[2] ne 0 THEN STRING(inv-t[2],"->>>>>>9.99") ELSE "" '",'
          '"' IF inv-d[2] ne 0 THEN STRING(inv-d[2],"->>>>.99") ELSE ""    '",'
          '"' IF inv-t[3] ne 0 THEN STRING(inv-t[3],"->>>>>>9.99") ELSE "" '",'
          '"' IF inv-d[3] ne 0 THEN STRING(inv-d[3],"->>>>.99") ELSE ""    '",'
          '"' IF inv-t[4] ne 0 THEN STRING(inv-t[4],"->>>>>>9.99") ELSE "" '",'
          '"' IF inv-d[4] ne 0 THEN STRING(inv-d[4],"->>>>.99") ELSE ""    '",'
          '"' STRING(ws_gross,"->,>>>,>>9.99")                             '",'
          '"' STRING(ws_disc-avail,'->>>>.99')                             '",'
          '"' IF v-company THEN STRING(ap-inv.company) ELSE ""             '",'
          SKIP.

    if last-of(tt-report.key-02) then do:
        IF NOT v-company THEN do:
        display "       *" @ ap-inv.due-date
              vend-t[1]  @ inv-t[1]
              vend-d[1]  @ inv-d[1]
              vend-t[2]  @ inv-t[2]
              vend-d[2]  @ inv-d[2]
              vend-t[3]  @ inv-t[3]
              vend-d[3]  @ inv-d[3]
              vend-t[4]  @ inv-t[4]
              vend-d[4]  @ inv-d[4]
              vend-t[1] + vend-t[2] + vend-t[3] + vend-t[4] @ ws_gross
              vend-d[1] + vend-d[2] + vend-d[3] + vend-d[4] @ ws_disc-avail
              
          with frame a.
      down 2 with frame a.
        END.
      ELSE do:
      display "       *" @ ap-inv.due-date
              vend-t[1]  @ inv-t[1]
              vend-d[1]  @ inv-d[1]
              vend-t[2]  @ inv-t[2]
              vend-d[2]  @ inv-d[2]
              vend-t[3]  @ inv-t[3]
              vend-d[3]  @ inv-d[3]
              vend-t[4]  @ inv-t[4]
              vend-d[4]  @ inv-d[4]
              vend-t[1] + vend-t[2] + vend-t[3] + vend-t[4] @ ws_gross
              vend-d[1] + vend-d[2] + vend-d[3] + vend-d[4] @ ws_disc-avail
              
          with frame b.
      down 2 with frame b.
      END.
      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             SKIP(1)
             '"' ""                               '",'
             '"' ""                               '",'
             '"' ""                               '",'
             '"' ""                               '",'
             '"' "       *"                       '",'
             '"' STRING(vend-t[1],"->>>>>>9.99")  '",'
             '"' STRING(vend-d[1],"->>>>.99")     '",'
             '"' STRING(vend-t[2],"->>>>>>9.99")  '",'
             '"' STRING(vend-d[2],"->>>>.99")     '",'
             '"' STRING(vend-t[3],"->>>>>>9.99")  '",'
             '"' STRING(vend-d[3],"->>>>.99")     '",'
             '"' STRING(vend-t[4],"->>>>>>9.99")  '",'
             '"' STRING(vend-d[4],"->>>>.99")     '",'
             '"' STRING(vend-t[1] + vend-t[2] + vend-t[3] + vend-t[4],"->,>>>,>>9.99") '",'
             '"' STRING(vend-d[1] + vend-d[2] + vend-d[3] + vend-d[4],'->>>>.99') '",'
             SKIP(1).

      do i = 1 to 4:
        assign
         grand-t[i] = grand-t[i] + vend-t[i]
         grand-d[i] = grand-d[i] + vend-d[i]
         
         vend-t[i]  = 0
         vend-d[i]  = 0.
      end.
    end.  /* last-of loop */
    
    if last(tt-report.key-02) then do:
      IF NOT v-company THEN DO:
      down 1 with frame a.

      display "      **" @ ap-inv.due-date
              grand-t[1] @ inv-t[1]
              grand-d[1] @ inv-d[1]
              grand-t[2] @ inv-t[2]
              grand-d[2] @ inv-d[2]
              grand-t[3] @ inv-t[3]
              grand-d[3] @ inv-d[3]
              grand-t[4] @ inv-t[4]
              grand-d[4] @ inv-d[4]
              grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4] @ ws_gross
              grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4] @ ws_disc-avail
          
          with frame a.
      END.
      ELSE do:
      down 1 with frame b.

      display "      **" @ ap-inv.due-date
              grand-t[1] @ inv-t[1]
              grand-d[1] @ inv-d[1]
              grand-t[2] @ inv-t[2]
              grand-d[2] @ inv-d[2]
              grand-t[3] @ inv-t[3]
              grand-d[3] @ inv-d[3]
              grand-t[4] @ inv-t[4]
              grand-d[4] @ inv-d[4]
              grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4] @ ws_gross
              grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4] @ ws_disc-avail
          
          with frame b.
      END.
      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             SKIP(1)
             '"' ""                               '",'
             '"' ""                               '",'
             '"' ""                               '",'
             '"' ""                               '",'
             '"' "      **"                       '",'
             '"' STRING(grand-t[1],"->>>>>>9.99")  '",'
             '"' STRING(grand-d[1],"->>>>.99")     '",'
             '"' STRING(grand-t[2],"->>>>>>9.99")  '",'
             '"' STRING(grand-d[2],"->>>>.99")     '",'
             '"' STRING(grand-t[3],"->>>>>>9.99")  '",'
             '"' STRING(grand-d[3],"->>>>.99")     '",'
             '"' STRING(grand-t[4],"->>>>>>9.99")  '",'
             '"' STRING(grand-d[4],"->>>>.99")     '",'
             '"' STRING(grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4],"->,>>>,>>9.99") '",'
             '"' STRING(grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4],'->>>>.99')      '",'
             SKIP.
    end.
  end. /* for each */

  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
  END.

  

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
