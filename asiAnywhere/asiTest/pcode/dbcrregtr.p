

/*------------------------------------------------------------------------
    File        : dbcrregtr.p
    Purpose     : Vendor Invoices Post
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttDebitCreditRegister NO-UNDO
        FIELD dbreg AS CHAR.
        
       

DEFINE DATASET dsDebitCreditRegister FOR ttDebitCreditRegister.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmvendpost      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBegindate     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEnddate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPstDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDebitCreditRegister.

     IF prmUser        = ? THEN ASSIGN     prmUser        = "".   
     IF prmvendpost    = ? THEN ASSIGN     prmvendpost    = "". 
     IF prmBegindate   = ? THEN ASSIGN     prmBegindate   = "". 
     IF prmEnddate     = ? THEN ASSIGN     prmEnddate     = "". 
     IF prmPstDate     = ? THEN ASSIGN     prmPstDate     = "". 
     IF prmperiod      = ? THEN ASSIGN     prmperiod      = 0. 
     IF prmOut         = ? THEN ASSIGN     prmOut         = "". 



DEFINE VARIABLE begin_date  AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE end_date    AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.

DEF VAR lv-audit-dir AS CHAR NO-UNDO.
DEF VAR lv-post AS LOG NO-UNDO.
DEFINE BUFFER bf-chk FOR ap-chk.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR lv-comp-curr AS cha NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

def new shared var v-post as log init NO NO-UNDO.
def new shared var v-trnum as INT NO-UNDO.


DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.


DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.

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
 vuser     = prmUser
 v-today   = TODAY 
 g_company = cocode
 g_user    = prmUser .


 

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
  ASSIGN  
      init-dir    = v-webrootpath .

DEF VAR g1 as dec format "->>>,>>>,>>9.99".
DEF VAR g2 as dec format "->>>,>>>,>>9.99".
DEF VAR xtrnum as int.
DEF VAR xap-acct as char.
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

DEF TEMP-TABLE tt-rm-bin NO-UNDO LIKE rm-bin.

{sys/inc/postdate.i}
{sys/inc/apcrmemo.i}


  IF prmvendpost = "dbreg" THEN DO:
      
        ASSIGN
       begin_date    = DATE(prmBegindate)
       end_date      = DATE(prmEnddate)  
       tran-date     = date(prmPstDate)                               
       tran-period   = prmperiod. 
           
        
        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "DebitReg" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "DebitReg" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        
        RUN init-proc.

        IF postdate-log THEN DO:
            ASSIGN
                prmPstDate     = STRING(TODAY)
                tran-date      = TODAY.
            
            v-invalid = no.

            find first period                   
                where period.company eq cocode
                and period.pst     le tran-date
                and period.pend    ge tran-date
                no-lock no-error.

            if avail period then do:
                IF NOT period.pstat THEN DO:
                    ASSIGN cError = "Period Already Closed. " .
                    v-invalid = YES.
                END.
                prmperiod = period.pnum.
            END.
            ELSE DO:
                ASSIGN cError = "No Defined Period Exists for" .
                v-invalid = yes.
            end.

        END.
        ELSE
            ASSIGN
                prmPstDate   = ""
                prmperiod    = 0.

            run run-report.

            IF v-postable THEN DO:    
                lv-post =  IF prmOut = "Yes" THEN TRUE ELSE FALSE .

                IF lv-post THEN do:
                    RUN post-gl.
                    RUN copy-report-to-audit-dir.
                    cError = "Posting Complete".     
                END.
                ELSE RUN undo-trnum.  
            END.
            ELSE do:
                cError = "No Debit/Credit Memos available for posting..." .
                RUN undo-trnum.  
            END.


        

        
   
  CREATE ttDebitCreditRegister.
    ASSIGN ttDebitCreditRegister.dbreg = vTextFile .

    
  END.
/*****************************************************************************************/

  PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 find first ap-ctrl where ap-ctrl.company = cocode NO-LOCK.
 xap-acct = ap-ctrl.payables.
 release ap-ctrl.

 find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "AUDITDIR"
    no-lock no-error.
  
  if not avail sys-ctrl then DO:
     create sys-ctrl.
     assign
        sys-ctrl.company = cocode
        sys-ctrl.name    = "AUDITDIR"
        sys-ctrl.descrip = "Audit Trails directory"
        sys-ctrl.char-fld = ".\AUDIT TRAILS".
  end.

  lv-audit-dir = sys-ctrl.char-fld.

  IF INDEX(lv-audit-dir,'P', 1) > 0 THEN ASSIGN 
   lv-audit-dir =  REPLACE(lv-audit-dir,'P:',"D:").

  IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
     lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).

  RELEASE sys-ctrl.

END PROCEDURE.

PROCEDURE run-report PRIVATE :
{sys/form/r-top3w.f}

DEF VAR v1 as dec format "->>>,>>>,>>9.99".
DEF VAR v2 as dec format "->>>,>>>,>>9.99".
DEF VAR lcnt as int init 0. /* DAR */

form header
SKIP(1)
"VENDOR#  Name                          MEMO#  INVOICE#     INV.DATE      "
"     DEBIT           CREDIT     G/L DISTRIBUTION" skip fill("_",130) format "x(130)"
WITH frame r-top.

form
    ap-pay.check-no  at 35 FORMAT ">>>>>>>>>>" space(2)
    ap-payl.inv-no   space(1)
    ap-payl.due-date FORMAT "99/99/99" space(3)
    ap-payl.amt-disc format "->>,>>>,>>9.99" space(3)
    ap-payl.amt-paid space(5)
    ap-payl.actnum with frame dbcr-memo no-box no-labels STREAM-IO width 132.
 



v-postable = NO.

ASSIGN 
 /*str-tit2 = c-win:title + " " + string(xtrnum)*/
 /*{sys/inc/ctrtext.i str-tit2 112} */
 str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
 {sys/inc/ctrtext.i str-tit3 132}

 g1 = 0
 g2 = 0.
    
/*{sys/inc/print1.i}*/
if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}



DISPLAY "" WITH frame r-top.
 
   FOR EACH ap-pay
       WHERE ap-pay.company EQ cocode
         AND ap-pay.memo    EQ YES
         AND ap-pay.posted  EQ NO
         AND ap-pay.check-date GE begin_date
         AND ap-pay.check-date LE end_date,
       FIRST vend
       WHERE vend.company EQ ap-pay.company
         AND vend.vend-no EQ ap-pay.vend-no
       NO-LOCK,
       EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK

       BREAK BY ap-pay.vend-no
             BY ap-pay.check-no
             BY ap-payl.inv-no
             BY ap-payl.line
       WITH FRAME dbcr-memo:

      if first-of(ap-pay.vend-no) then do:
         ap-pay.period = tran-period.
         put vend.vend-no space(1) vend.name. 
      end.
      else 
      if first-of(ap-payl.inv-no) then put skip(1).

      display
          ap-pay.check-no  when first-of(ap-pay.check-no)
          ap-payl.inv-no
          ap-payl.due-date
          ap-payl.amt-paid 
          ap-payl.amt-disc
          ap-payl.actnum.

      down.
      ASSIGN
      lcnt = lcnt + 1
      v1 = v1 + ap-payl.amt-paid
      v2 = v2 + ap-payl.amt-disc.

      if last-of(ap-pay.vend-no) then do:
         display  "*  VENDOR TOTALS" to 62 v2 to 84 v1 to 101 " *" skip(1)
         with frame vtot no-box no-labels width 132 STREAM-IO.
         ASSIGN
         g1 = g1 + v1
         g2 = g2 + v2
         v1 = 0
         v2 = 0.
      end.
      v-postable = YES.
   end. /* each memo */

   RELEASE ap-pay.

   display  "** GRAND TOTAL" to 62 g2 to 84 g1 to 101 " **" skip(1)
   with no-labels no-underline width 132 frame GT STREAM-IO.



 

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.


PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR t1 as dec.
   DEF VAR ld-pct AS DEC NO-UNDO.
   DEF VAR ld-cst AS DEC NO-UNDO.
   DEF VAR ld-qty AS DEC NO-UNDO.
   DEF VAR v-r-qty AS DEC NO-UNDO.
   DEF VAR v-i-qty AS DEC NO-UNDO.
   DEF VAR v-t-qty AS DEC NO-UNDO.
   DEF VAR lv-uom AS CHAR NO-UNDO.

   DEF BUFFER b-rm-bin FOR rm-bin.

   
   

   EMPTY TEMP-TABLE tt-rm-bin.

   postit:
   do transaction on error undo:

      FOR EACH ap-pay
          WHERE ap-pay.company EQ cocode
            AND ap-pay.memo    EQ YES
            AND ap-pay.posted  EQ NO
            AND ap-pay.check-date GE begin_date
            AND ap-pay.check-date LE end_date,
          FIRST vend
          WHERE vend.company EQ ap-pay.company
            AND vend.vend-no EQ ap-pay.vend-no,
          EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no

          break by ap-pay.vend-no by ap-payl.inv-no by ap-payl.line
          on error undo postit, leave postit:

         FIND FIRST ap-inv
             WHERE ap-inv.company EQ cocode
               AND ap-inv.vend-no EQ vend.vend-no
               AND ap-inv.inv-no  EQ ap-payl.inv-no
             NO-ERROR.
         IF AVAIL ap-inv THEN DO:
           ASSIGN
            ap-inv.paid = ap-inv.paid + ap-payl.amt-paid - ap-payl.amt-disc
            ap-inv.due  = ap-inv.net - ap-inv.paid - ap-inv.disc-taken.

           IF apcrmemo-log AND (ap-payl.amt-paid - ap-payl.amt-disc) GT 0 THEN DO:
             ld-cst = 0.

             FOR EACH rm-rdtlh NO-LOCK
                 WHERE rm-rdtlh.company      EQ ap-inv.company
                   AND rm-rdtlh.receiver-no BEGINS STRING(ap-inv.i-no,"9999999999")
                   AND rm-rdtlh.rita-code   EQ "R"
                 USE-INDEX receiver-no,
                 FIRST rm-rcpth NO-LOCK
                 WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
                   AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code:
                   ld-cst = ld-cst +
                           (rm-rdtlh.qty * rm-rdtlh.cost) + rm-rdtlh.frt-cost. 
             END.

             ld-pct = 1 - ((ap-payl.amt-paid - ap-payl.amt-disc) / ld-cst).
             IF ld-pct LT 0 THEN ld-pct = 0.
             IF ld-pct EQ ? THEN ld-pct = 1.

             IF ld-pct NE 1 THEN
                FOR EACH rm-rdtlh
                    WHERE rm-rdtlh.company      EQ ap-inv.company
                      AND rm-rdtlh.receiver-no BEGINS STRING(ap-inv.i-no,"9999999999")
                      AND rm-rdtlh.rita-code   EQ "R"
                    USE-INDEX receiver-no,
                    FIRST rm-rcpth NO-LOCK
                    WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
                      AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code:
               
                  ASSIGN
                   rm-rdtlh.cost     = rm-rdtlh.cost * ld-pct
                   rm-rdtlh.frt-cost = rm-rdtlh.frt-cost * ld-pct.
               
                  IF NOT CAN-FIND(FIRST tt-rm-bin
                                  WHERE tt-rm-bin.company EQ rm-rcpth.company
                                    AND tt-rm-bin.i-no    EQ rm-rcpth.i-no
                                    AND tt-rm-bin.loc     EQ rm-rdtlh.loc
                                    AND tt-rm-bin.loc-bin EQ rm-rdtlh.loc-bin
                                    AND tt-rm-bin.tag     EQ rm-rdtlh.tag) THEN DO:
                    CREATE tt-rm-bin.
                    ASSIGN
                     tt-rm-bin.company = rm-rcpth.company
                     tt-rm-bin.i-no    = rm-rcpth.i-no
                     tt-rm-bin.loc     = rm-rdtlh.loc
                     tt-rm-bin.loc-bin = rm-rdtlh.loc-bin
                     tt-rm-bin.tag     = rm-rdtlh.tag.
                  END.
                END.
           END.
         END.

         RELEASE ap-inv.

         create gltrans.
         assign
            t1 = t1 - ap-payl.amt-paid + ap-payl.amt-disc
            gltrans.company = cocode
            gltrans.actnum  = ap-payl.actnum
            gltrans.jrnl    = "APMEM"
            gltrans.tr-dscr = vend.name  + "  " + string(ap-pay.check-date)
            gltrans.tr-date = tran-date
            gltrans.tr-amt  = - (ap-payl.amt-paid - ap-payl.amt-disc)
            gltrans.period  = tran-period
            gltrans.trnum   = xtrnum
            ap-payl.posted  = true.

         find first bank where bank.company = cocode and
                               bank.actnum = ap-payl.actnum no-error.
         if avail bank then
           assign bank.bal = bank.bal + gltrans.tr-amt.

         RELEASE gltrans.

         if last-of(ap-payl.inv-no) then do:
            assign
               vend.purch[tran-period]   = vend.purch[tran-period]   + t1
               vend.purch[13]        = vend.purch[13]   + t1
               vend.acc-bal          = vend.acc-bal     + t1.
            if vend.acc-bal >= vend.hibal or vend.hibal = 0 THEN
               ASSIGN
               vend.hibal = vend.acc-bal
               vend.hibal-date = ap-pay.check-date.
            
            create ap-ledger.
            assign
               ap-ledger.company  = cocode
               ap-ledger.vend-no  = ap-pay.vend-no
               ap-ledger.amt      = ap-payl.amt-paid - ap-payl.amt-disc
               ap-ledger.refnum   = "MEMO#" + ap-payl.inv-no
               ap-ledger.ref-date = ap-pay.check-date
               ap-ledger.trnum    = xtrnum
               ap-ledger.tr-date  = tran-date
               t1 = 0.

            RELEASE ap-ledger.
         end.
         assign ap-pay.posted = true.
      end.
      create gltrans.
      assign
      gltrans.company = cocode
      gltrans.actnum  = xap-acct
      gltrans.jrnl    = "APMEM"
      gltrans.tr-dscr = "ACCOUNTS PAYABLE MEMO"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = g1 - g2   /* DAR  -  g2 - g1  */
      gltrans.period  = tran-period
      gltrans.trnum   = xtrnum.
      RELEASE gltrans.

      /* Recalc Rm Bins whose cost has been changed by APCRMEMO sys-ctrl param */
      FOR EACH tt-rm-bin,
          FIRST rm-bin
          WHERE rm-bin.company EQ tt-rm-bin.company
            AND rm-bin.i-no    EQ tt-rm-bin.i-no
            AND rm-bin.loc     EQ tt-rm-bin.loc
            AND rm-bin.loc-bin EQ tt-rm-bin.loc-bin
            AND rm-bin.tag     EQ tt-rm-bin.tag
          BREAK BY tt-rm-bin.company
                BY tt-rm-bin.i-no:

        ASSIGN
         rm-bin.qty  = 0
         rm-bin.cost = 0.

        IF TRIM(rm-bin.tag) NE "" THEN
        FOR EACH rm-rdtlh NO-LOCK
            WHERE rm-rdtlh.company EQ fg-bin.company
              AND rm-rdtlh.tag     EQ fg-bin.tag
              AND rm-rdtlh.loc     EQ fg-bin.loc
              AND rm-rdtlh.loc-bin EQ fg-bin.loc-bin
            USE-INDEX tag,
            EACH rm-rcpth NO-LOCK
            WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
              AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code
              AND rm-rcpth.i-no      EQ fg-bin.i-no
            USE-INDEX r-no
    
            BY rm-rcpth.trans-date
            BY rm-rcpth.rec_key
            BY rm-rdtlh.rec_key
            BY rm-rcpth.r-no:
    
          {rm/rm-mkbin.i}
        END. /* each rm-rcpth */

        ELSE
        FOR EACH rm-rcpth
            WHERE rm-rcpth.company EQ fg-bin.company
              AND rm-rcpth.i-no    EQ fg-bin.i-no
            NO-LOCK USE-INDEX i-no,
            EACH rm-rdtlh
            WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
              AND rm-rdtlh.loc       EQ fg-bin.loc
              AND rm-rdtlh.loc-bin   EQ fg-bin.loc-bin
              AND rm-rdtlh.tag       EQ fg-bin.tag
            USE-INDEX rm-rdtl no-lock
    
            BY rm-rcpth.trans-date
            BY rm-rcpth.rec_key
            BY rm-rdtlh.rec_key
            BY rm-rcpth.r-no:
    
          {rm/rm-mkbin.i}
        END. /* each rm-rcpth */

        IF LAST-OF(tt-rm-bin.i-no) THEN DO:
          FIND FIRST item NO-LOCK
              WHERE item.company EQ tt-rm-bin.company
                AND item.i-no    EQ tt-rm-bin.i-no
              NO-ERROR.

          IF AVAIL item THEN DO:
            ASSIGN
             ld-qty = 0
             ld-cst = 0.

            FOR EACH b-rm-bin FIELDS(qty cost) NO-LOCK
                WHERE b-rm-bin.company EQ item.company
                  AND b-rm-bin.i-no    EQ item.i-no
                  AND b-rm-bin.cost    NE ?:
              ASSIGN
               ld-cst = ld-cst + (b-rm-bin.cost *
                        (b-rm-bin.qty * IF b-rm-bin.qty LT 0 THEN -1 ELSE 1))
               ld-qty = ld-qty +
                        (b-rm-bin.qty * IF b-rm-bin.qty LT 0 THEN -1 ELSE 1).
            END.

            FIND CURRENT item.
            item.avg-cost = IF ld-qty EQ 0 THEN 0 ELSE (ld-cst / ld-qty).
            FIND CURRENT item NO-LOCK.
          END.
        END.
      END.
    end. /* postit: transaction */

   RELEASE vend.
   RELEASE ap-pay.
   RELEASE ap-payl.

   

END PROCEDURE.

PROCEDURE copy-report-to-audit-dir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname2 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname3 AS CHAR FORMAT "X(20)" NO-UNDO.
  
  ASSIGN targetfile = lv-audit-dir + "\AP\VW2\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AP"
         dirname3 = lv-audit-dir + "\AP\VW2".

  OS-COPY VALUE(list-name) VALUE (targetfile).

  IF SEARCH(targetfile) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
    OS-CREATE-DIR VALUE(dirname2).
    OS-CREATE-DIR VALUE(dirname3).
    OS-COPY VALUE(list-name) VALUE (targetfile).
  END.
END PROCEDURE.

PROCEDURE undo-trnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        IF xtrnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  

END PROCEDURE.


