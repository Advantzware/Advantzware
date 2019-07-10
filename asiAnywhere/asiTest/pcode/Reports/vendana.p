

/*------------------------------------------------------------------------
    File        : vendana.p
    Purpose     : AP Vendor Analysis
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttAPVendorAnalysis NO-UNDO
        FIELD venana AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsAPVendorAnalysis FOR ttAPVendorAnalysis.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmvenana        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegcom        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendcom        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAPVendorAnalysis.

     IF prmUser     = ? THEN ASSIGN    prmUser      = "".   
     IF prmvenana   = ? THEN ASSIGN    prmvenana    = "". 
     IF prmbegcom   = ? THEN ASSIGN    prmbegcom    = "". 
     IF prmendcom   = ? THEN ASSIGN    prmendcom    = "". 
     IF prmenddt    = ? THEN ASSIGN    prmenddt     = "".
     IF prmOut      = ? THEN ASSIGN    prmOut       = "". 
     

DEFINE VARIABLE begin_comp      AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE end_comp    AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE end_date    AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 NO-UNDO.


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



DEF STREAM excel.
DEF VAR lv-default-comp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO INIT 0.

FOR EACH usercomp WHERE usercomp.USER_id = USERID("nosweat") AND  usercomp.loc = "" NO-LOCK :
    v-count = v-count + 1 .
END.
FIND FIRST usercomp WHERE usercomp.USER_id = USERID("nosweat") AND
                                  usercomp.company_default NO-LOCK NO-ERROR.
ASSIGN     
lv-default-comp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


ASSIGN
     end_date = TODAY
     begin_comp = lv-default-comp
     end_comp   = lv-default-comp .



  IF prmvenana = "venana" THEN DO:
     
        ASSIGN
        v-today    = TODAY
        begin_comp = prmbegcom 
        end_comp   = prmendcom 
        end_date   = date(prmenddt)   . 
    
            

    assign
        init-dir    = v-webrootpath
        lv-txt-file =  'VendAnalysis' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .
       
        
        v-excel-file =  'VendAnalysis' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .

        
        run run-report. 

        CREATE ttAPVendorAnalysis.

        IF prmOut = "Yes" THEN
        ASSIGN ttAPVendorAnalysis.venana = v-excel-file.
        ELSE
            ASSIGN ttAPVendorAnalysis.venana = lv-txt-file .



 


  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* --------------------------------------------------- ap/ap-anal.p 12/92 cd  */
/*                                                                            */
/* a/p - vendor analysis report                                               */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

DEF VAR baldue AS DEC NO-UNDO.
DEF VAR save_id AS RECID NO-UNDO.
DEF VAR time_stamp AS ch NO-UNDO.
DEF VAR v-date AS DATE FORMAT "99/99/9999" INIT TODAY EXTENT 4 NO-UNDO.
DEF VAR v-period LIKE period.pnum NO-UNDO.
DEF VAR v-tot-ptd AS DEC NO-UNDO.
DEF VAR v-tot-ytd AS DEC NO-UNDO.
DEF VAR v-tot-lyd AS DEC NO-UNDO.
DEF VAR v-year AS INT NO-UNDO.
DEF VAR v-amt LIKE ap-ledger.amt NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.


FIND FIRST period
    WHERE period.company EQ cocode
      AND period.pst     LE end_date
      AND period.pend    GE end_date
    NO-LOCK NO-ERROR.
    
IF AVAIL period THEN
  ASSIGN 
   end_date  = period.pend
   .
   

      
ASSIGN
 str-tit2 = "Vendor Analysis"
 {sys/inc/ctrtext.i str-tit2 112}

 str-tit3 = "Company From: " + STRING(begin_comp) + " To: " + STRING(end_comp)
 {sys/inc/ctrtext.i str-tit3 132}.

 
   /*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.


{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Vend.#,Name,Due,MTD,YTD,Last Year,Variance".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.



   DISPLAY "" WITH FRAME r-top.
FOR EACH company WHERE
       company.company GE begin_comp AND
       company.company LE end_comp
       NO-LOCK:

      FIND FIRST period
    WHERE period.company EQ company.company
      AND period.pst     LE end_date
      AND period.pend    GE end_date
    NO-LOCK NO-ERROR.
    
    IF AVAIL period THEN
    ASSIGN
        v-period  = period.pnum
        v-date[1] = period.pst
        /*end_date  = period.pend*/
        v-year    = period.yr.
    ELSE
    ASSIGN
        v-period  = MONTH(TODAY)
        v-date[1] = TODAY
        v-date[2] = TODAY
        v-year    = YEAR(TODAY).

    FOR EACH period
        WHERE period.company EQ company.company
        AND period.yr      EQ v-year
        AND period.pnum    GT 0
        NO-LOCK
        BY period.pnum:
            v-date[2] = period.pst.
            LEAVE.
    END.

    ASSIGN
    v-date[3] = 01/01/0001
    v-date[4] = v-date[2] - 1.

    FOR EACH period
        WHERE period.company EQ company.company
         AND period.yr      EQ v-year - 1
         AND period.pnum    GT 0
         NO-LOCK
         BREAK BY period.pnum:
            IF FIRST(period.pnum) THEN v-date[3] = period.pst.
            IF LAST(period.pnum)  THEN v-date[4] = period.pend.
    END.

    FOR EACH vend NO-LOCK
       WHERE vend.company EQ company.company
       USE-INDEX vend
       BY vend.vend-no:

     ASSIGN
      baldue    = 0 
      v-tot-ptd = 0
      v-tot-ytd = 0
      v-tot-lyd = 0.

     FOR EACH ap-ledger
         WHERE ap-ledger.company EQ vend.company
           AND ap-ledger.tr-date GE v-date[3]
           AND ap-ledger.tr-date LE end_date
           AND ap-ledger.vend-no EQ vend.vend-no
           AND (ap-ledger.refnum BEGINS "INV#" OR
                ap-ledger.refnum BEGINS "Memo#")
         NO-LOCK:

       v-amt = ap-ledger.amt *
               (IF ap-ledger.refnum BEGINS "Memo#" THEN -1 ELSE 1).

       IF ap-ledger.tr-date GE v-date[1] THEN
         v-tot-ptd = v-tot-ptd + v-amt.

       IF ap-ledger.tr-date GE v-date[2] THEN
         v-tot-ytd = v-tot-ytd + v-amt.

       IF ap-ledger.tr-date LT v-date[2] THEN
         v-tot-lyd = v-tot-lyd + v-amt.
     END.

     FOR EACH ap-inv FIELDS(due)
         WHERE ap-inv.company  EQ vend.company
           AND ap-inv.vend-no  EQ vend.vend-no
           AND ap-inv.posted   EQ YES
           AND ap-inv.inv-date LE end_date
         NO-LOCK
         USE-INDEX ap-inv:
       baldue = baldue + ap-inv.due.
     END.

     DISPLAY vend.vend-no
             vend.name
             baldue                     LABEL "Due"
                                        FORMAT "->>,>>>,>>9.99"
             v-tot-ptd                  LABEL "MTD"
                                        FORMAT "->>,>>>,>>9.99"
             v-tot-ytd                  LABEL "YTD"
                                        FORMAT "->>,>>>,>>9.99"
             v-tot-lyd                  LABEL "Last Year"
                                        FORMAT "->>,>>>,>>9.99"
             (v-tot-ytd - v-tot-lyd)    LABEL "Variance"
                                        FORMAT "->>,>>>,>>9.99"
         WITH STREAM-IO WIDTH 132.

     IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' vend.vend-no                                   '",'
            '"' vend.NAME                                      '",'
            '"' STRING(baldue,"->>,>>>,>>9.99")                '",'
            '"' STRING(v-tot-ptd,"->>,>>>,>>9.99")             '",'
            '"' STRING(v-tot-ytd,"->>,>>>,>>9.99")             '",'
            '"' STRING(v-tot-lyd,"->>,>>>,>>9.99")             '",'
            '"' STRING(v-tot-ytd - v-tot-lyd,"->>,>>>,>>9.99") '",'
            SKIP.

     ACCUMULATE baldue    (TOTAL).
     ACCUMULATE v-tot-ptd (TOTAL).
     ACCUMULATE v-tot-ytd (TOTAL).
     ACCUMULATE v-tot-lyd (TOTAL).
   END.
   END.
   

   DISPLAY "T O T A L S" to 39
           (ACCUM TOTAL baldue)         FORMAT "->>,>>>,>>9.99"
           (ACCUM TOTAL v-tot-ptd)      FORMAT "->>,>>>,>>9.99"
           (ACCUM TOTAL v-tot-ytd)      FORMAT "->>,>>>,>>9.99"
           (ACCUM TOTAL v-tot-lyd)      FORMAT "->>,>>>,>>9.99"
           (ACCUM TOTAL v-tot-ytd) -
           (ACCUM TOTAL v-tot-lyd)      FORMAT "->>,>>>,>>9.99"
       WITH STREAM-IO WIDTH 132 NO-LABELS.

   IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
          '"' ""                                               '",'
          '"' "T O T A L S"                                    '",'
          '"' STRING((ACCUM TOTAL baldue),"->>,>>>,>>9.99")    '",'
          '"' STRING((ACCUM TOTAL v-tot-ptd),"->>,>>>,>>9.99") '",'
          '"' STRING((ACCUM TOTAL v-tot-ytd),"->>,>>>,>>9.99") '",'
          '"' STRING((ACCUM TOTAL v-tot-lyd),"->>,>>>,>>9.99") '",'
          '"' STRING((ACCUM TOTAL v-tot-ytd) - 
                     (ACCUM TOTAL v-tot-lyd),"->>,>>>,>>9.99") '",'
          SKIP.

   IF tb_excel THEN DO:
      OUTPUT STREAM excel CLOSE.
      IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
   END.
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
