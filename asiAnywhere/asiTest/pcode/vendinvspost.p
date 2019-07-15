

/*------------------------------------------------------------------------
    File        : vendinvpost.p
    Purpose     : Vendor Invoices Post
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttVendInvPost NO-UNDO
        FIELD vendpost AS CHAR
        FIELD vpstinv AS CHAR.

DEFINE DATASET dsVendInvPost FOR ttVendInvPost.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmvendpost      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeginvend     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndvend       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeginUsr      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndUsr        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBegindate     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEnddate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPstDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmGlActNm       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmGl2           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendInvPost.

     IF prmUser        = ? THEN ASSIGN     prmUser        = "".   
     IF prmvendpost    = ? THEN ASSIGN     prmvendpost    = "". 
     IF prmBeginvend   = ? THEN ASSIGN     prmBeginvend   = "". 
     IF prmEndvend     = ? THEN ASSIGN     prmEndvend     = "". 
     IF prmBeginUsr    = ? THEN ASSIGN     prmBeginUsr    = "".  
     IF prmEndUsr      = ? THEN ASSIGN     prmEndUsr      = "".  
     IF prmBegindate   = ? THEN ASSIGN     prmBegindate   = "". 
     IF prmEnddate     = ? THEN ASSIGN     prmEnddate     = "". 
     IF prmPstDate     = ? THEN ASSIGN     prmPstDate     = "". 
     IF prmGlActNm     = ? THEN ASSIGN     prmGlActNm     = "". 
     IF prmperiod      = ? THEN ASSIGN     prmperiod      = 0. 
     IF prmGl2         = ? THEN ASSIGN     prmGl2         = "". 
     IF prmOut         = ? THEN ASSIGN     prmOut         = "". 



DEFINE VARIABLE begin_date  AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_user  AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE begin_vend  AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE end_date    AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE end_user    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_vend    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE lbl_sort    AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE tran-date   AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR lv-comp-curr AS cha NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.
DEF VAR lv-post AS LOG NO-UNDO.
DEFINE BUFFER bf-chk FOR ap-chk.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

def new shared var v-post as log init NO NO-UNDO.
def new shared var v-trnum as INT NO-UNDO.

def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---".
def var time_stamp as ch.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-invalid-inv AS LOG NO-UNDO.
DEF VAR v-frt-acct LIKE ap-ctrl.freight NO-UNDO.
DEF VAR v-cash-acct AS CHAR NO-UNDO.
DEF VAR xap-acct LIKE account.actnum NO-UNDO.
DEF VAR xap-stax LIKE account.actnum NO-UNDO.
DEF VAR lv-frt-total AS DEC NO-UNDO.  /* accum total */
def var v-postable as log init NO NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.
DEF VAR v-bank-code AS CHAR NO-UNDO.
DEF VAR lv-bank-acct AS CHAR NO-UNDO.
DEF VAR lv-period LIKE period.pnum NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD actnum LIKE account.actnum
    FIELD ex-rate LIKE currency.ex-rate INIT 1
    FIELD curr-amt LIKE ap-inv.net.

DEF VAR v-fgpostgl AS LOG NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.
DEF VAR lv-fgpost-dir AS LOG NO-UNDO.
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

{sys/inc/fgpostgl.i}
 v-fgpostgl = fgpostgl NE "None".
{sys/inc/rmpostgl.i}
{sys/inc/postdate.i}
{sys/inc/apsecure.i}
{sys/inc/apautocheck.i}
 

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

find first sys-ctrl where
        sys-ctrl.company eq cocode AND
        sys-ctrl.name    eq "AUDITDIR"
        no-lock no-error.
   
   if not avail sys-ctrl THEN DO:
      create sys-ctrl.
      assign
         sys-ctrl.company = cocode
         sys-ctrl.name    = "AUDITDIR"
         sys-ctrl.descrip = "Audit Trails directory"
         sys-ctrl.char-fld = ".\AUDIT TRAILS".
   end.
  
   lv-audit-dir = sys-ctrl.char-fld.

   IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
      lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).
  
   RELEASE sys-ctrl.


   find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "GLPOST"
    no-lock no-error.

   
   if not avail sys-ctrl then do:
      create sys-ctrl.
      assign
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "GLPOST"
         sys-ctrl.log-fld  = NO
         sys-ctrl.descrip  = "Post AP Invoices within current period only? Default to NO "
         sys-ctrl.char-fld = "User Defined Password to be entered when Logical Value = YES ".
      END.
      
      lv-fgpost-dir = sys-ctrl.log-fld .

DEF TEMP-TABLE tt-ap-invl NO-UNDO
                          FIELD row-id AS ROWID
                          FIELD actnum LIKE account.actnum
                          FIELD unit-pr LIKE ap-invl.unit-pr
                          FIELD amt LIKE ap-invl.amt
                          INDEX row-id row-id.

DEF TEMP-TABLE tt-ap-tax  NO-UNDO
                          FIELD row-id AS ROWID
                          FIELD actnum LIKE account.actnum
                          FIELD amt LIKE ap-invl.amt
                          FIELD curr-amt LIKE ap-invl.amt
                          INDEX row-id row-id.

 
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .




  IF prmvendpost = "vendpost" THEN DO:
      
        ASSIGN
       begin_date    = DATE(prmBegindate)              
       begin_user    = prmBeginUsr                                    
       begin_vend    = prmBeginvend                                   
       end_date      = DATE(prmEnddate)                               
       end_user      = prmEndUsr                                      
       end_vend      = prmEndvend                                     
       lbl_sort      = prmGlActNm     
       tran-date     = date(prmPstDate)                               
       tran-period   = prmperiod. 
           
        
        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "vendorpost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "vendorpost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        /*Init run*/
        find first ap-ctrl where ap-ctrl.company eq cocode no-lock no-error.
        if not avail ap-ctrl then return.
        assign xap-acct = ap-ctrl.payables
            xap-stax = ap-ctrl.stax
            v-frt-acct = ap-ctrl.freight
            v-cash-acct = ap-ctrl.cash-act.
        release ap-ctrl.
        
        FIND FIRST bank WHERE
            bank.company = cocode AND
            bank.actnum = v-cash-acct
            NO-LOCK NO-ERROR.
        IF AVAIL bank THEN DO:
            ASSIGN
                v-bank-code = bank.bank-code
                lv-bank-acct = bank.actnum.
            RELEASE bank.
        END.

       loop:
       REPEAT:
           FIND FIRST gl-ctrl EXCLUSIVE-LOCK
               WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
           IF AVAIL gl-ctrl THEN DO:
               ASSIGN v-trnum       = gl-ctrl.trnum + 1
                   gl-ctrl.trnum = v-trnum.
               FIND CURRENT gl-ctrl NO-LOCK.
               LEAVE loop.
           END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */


        run run-report.

         IF v-postable THEN DO: 
            lv-post =  IF prmOut = "Yes" THEN TRUE ELSE FALSE .
            
            IF lv-post THEN do:
                 RUN post-gl.
                 RUN copy-report-to-audit-dir.
                 RUN clear-ap.

                cError = "Posting Complete" .     
            END. 
         END.

         ELSE do: 
             cError = "Nothing available for posting...".
         END.

   
  CREATE ttVendInvPost.
    ASSIGN ttVendInvPost.vendpost = vTextFile .

  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* ---------------------------------------------------- ap/ap-inreg.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing TRANSACTIONs                   */
/* -------------------------------------------------------------------------- */
def var g1 as dec format "->>,>>>,>>9.99" NO-UNDO.
def var g2 like g1 NO-UNDO.
def var t1 like g1 NO-UNDO.
def var t2 like g1 NO-UNDO.
def var t3 like g1 NO-UNDO.
def var v1 like g1 NO-UNDO.
def var v2 like g1 NO-UNDO.

def var total-msf like ap-invl.amt-msf NO-UNDO.
def var v-s-date like inv-head.inv-date format "99/99/9999" init 01/01/0001 NO-UNDO.
def var v-e-date like v-s-date init today NO-UNDO.
def var v-prt-dscr as log init no no-undo.
def var v-s-vend like vend.vend-no initial "First" no-undo.
def var v-e-vend like vend.vend-no initial "Last" no-undo.
DEF BUFFER xap-inv FOR ap-inv.
def var v-loop as int init 1 no-undo.
DEF VAR v-upd AS LOG NO-UNDO.
DEF var v-po-no like fg-rcpth.po-no NO-UNDO.
def var v-dscr          like account.dscr.
def var v-disp-actnum   like account.actnum.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr".
  DEF VAR ld-gl-amt AS DEC NO-UNDO.

{sys/form/r-top3w.f}
time_stamp = string(time,"hh:mmam").

form header
     "VENDOR#  Name                              INVOICE #       INV.DATE    DUE DATE         AMOUNT " 
     "    G/L DISTRIBUTION" skip fill("_",130) format "x(130)"
    with no-labels no-box no-underline frame f-top page-top width 132 STREAM-IO.

form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     udate         label "DATE"   
     v-disp-amt    label "AMOUNT" skip

    with down STREAM-IO width 130 frame gldetail.




tmpstore = fill("_",125).

ASSIGN v-s-vend   = begin_vend
       v-e-vend   = END_vend
       v-s-date   = date(begin_date)
       v-e-date   = date(END_date)
       v-prt-dscr = IF lbl_sort = "True" THEN TRUE ELSE FALSE  .

/*{sys/inc/print1.i}*/  
        if tmp-dir = "" then tmp-dir = v-webrootpath .
        assign list-name = tmp-dir + vTextFile
        init-dir = tmp-dir.


{sys/inc/outprint.i VALUE(lines-per-page)}



assign
 g1 = 0
 g2 = 0
 t1 = 0
 t2 = 0
 t3 = 0
 v1 = 0
 v2 = 0
 total-msf = 0.

assign
 str-tit  = coname + " - " + loname
 str-tit2 = "VENDOR INVOICES  -  EDIT REGISTER " + string(v-trnum)
 str-tit3 = "Period " + string(tran-period,"99") +
            " - TRANSACTION Date Entered: " + string(tran-date)
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (114 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3.

EMPTY TEMP-TABLE tt-report.

EMPTY TEMP-TABLE tt-ap-invl.

EMPTY TEMP-TABLE tt-ap-tax.
 
display "" with frame r-top.
display "" with frame f-top.

{ap/r-apve&p.i}

END PROCEDURE.

PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR g2 AS dec NO-UNDO.
DEF VAR t1 AS DEC NO-UNDO.
DEF VAR v-upd AS LOG NO-UNDO.
DEF var v-po-no like fg-rcpth.po-no NO-UNDO.
def var total-msf like ap-invl.amt-msf NO-UNDO.
def var v-qty like ap-invl.qty.
def var v-qty1 like v-qty.
def var v-qty2 like v-qty.
def var v-qty3 like v-qty.
def var v-cost like fg-rdtlh.cost.
DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR ll-rcpth AS LOG NO-UNDO.

  /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
  postit:
do transaction on error undo postit:
  g2 = 0.
  
  for each tt-report
      where can-find(first ap-inv where recid(ap-inv) eq tt-report.rec-id
                                    and ap-inv.posted eq no)
      BREAK BY tt-report.actnum:

    find first ap-inv
        where recid(ap-inv) eq tt-report.rec-id
        exclusive-lock no-error no-wait.
     
    if not avail ap-inv then do:
      cError = "Unable to Post due to Invoice Record being Locked.  " +
              "Please Try again Later".
      /*pause.
      hide message no-pause.
      undo postit, leave postit.*/
      undo postit, leave postit.
      RETURN.
    end.

    ap-inv.period = tran-period.
    
    find first vend
        where vend.company eq cocode
          and vend.vend-no eq ap-inv.vend-no
        use-index vend no-lock.

    for each ap-invl where ap-invl.i-no eq ap-inv.i-no,

        first tt-ap-invl where tt-ap-invl.row-id eq rowid(ap-invl):

      create gltrans.
      assign
       t1              = t1 + ap-invl.amt
       g2              = g2 + ap-invl.amt
       total-msf       = total-msf + ap-invl.amt-msf
       gltrans.company = cocode
       gltrans.actnum  = tt-ap-invl.actnum
       gltrans.jrnl    = "ACPAY"
       gltrans.tr-dscr = vend.name  + "  " + string(ap-inv.inv-date)
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = tt-ap-invl.amt
       gltrans.trnum   = v-trnum
       gltrans.period  = tran-period
       ap-invl.posted  = yes.      

      RELEASE gltrans.

      find first po-ordl
          where po-ordl.company eq cocode
            and po-ordl.po-no   eq (if ap-invl.po-no eq 0 then ap-inv.po-no
                                                          else ap-invl.po-no)
            and po-ordl.line    eq {ap/invlline.i -1}
          use-index po-no no-error.

      if avail po-ordl then do:
        find first reftable
            {ap/apreftbw.i po-ordl.po-no}
              and reftable.code2 eq string(ap-invl.i-no,"9999999999")
            no-lock no-error.
        if not avail reftable then do:
          {ap/addreftb.i po-ordl.po-no}
          RELEASE reftable.
        end.
        
        po-ordl.t-inv-qty = po-ordl.t-inv-qty + ap-invl.qty.

        RELEASE item.
        IF po-ordl.item-type THEN
        FIND FIRST item
            WHERE item.company EQ po-ordl.company
              AND item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.

        IF AVAIL item           AND
           item.i-code EQ "R"   AND
           INDEX("MOXY789",ITEM.mat-type) GT 0 AND
           item.stocked EQ NO   THEN DO:

          ll-rcpth = NO.

          FOR EACH rm-rcpth NO-LOCK
              WHERE rm-rcpth.company   EQ po-ordl.company
                AND rm-rcpth.po-no     EQ STRING(po-ordl.po-no)
                AND rm-rcpth.i-no      EQ po-ordl.i-no
                AND rm-rcpth.rita-code EQ "R",
              EACH rm-rdtlh NO-LOCK
              WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
                AND rm-rdtlh.job-no  EQ po-ordl.job-no
                AND rm-rdtlh.job-no2 EQ po-ordl.job-no2
                AND rm-rdtlh.s-num   EQ po-ordl.s-num
              BREAK BY rm-rcpth.company:

            IF FIRST(rm-rcpth.company) THEN
              ASSIGN
               po-ordl.t-rec-qty = 0
               ll-rcpth          = YES.

            v-qty = rm-rdtlh.qty.

            IF rm-rcpth.pur-uom NE po-ordl.cons-uom THEN
              RUN sys/ref/convquom.p (rm-rcpth.pur-uom, po-ordl.cons-uom,
                                    item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                    v-qty, OUTPUT v-qty).

            IF po-ordl.cons-uom EQ "EA" THEN DO:
              {sys/inc/roundup.i v-qty}
            END.

            po-ordl.t-rec-qty = po-ordl.t-rec-qty + v-qty.
          END.

          IF NOT ll-rcpth THEN DO:
            v-dep = item.s-dep.          
            {po/pol-dims.i}

            v-qty = ap-invl.qty.

            IF po-ordl.pr-qty-uom NE po-ordl.cons-uom THEN
              RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, po-ordl.cons-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      v-qty, OUTPUT v-qty).

            po-ordl.t-rec-qty = po-ordl.t-rec-qty + v-qty.
          END.

          RUN rm/polclose.p (ROWID(po-ordl), ap-invl.qty, po-ordl.pr-qty-uom).
        END.

        RUN po/closechk.p (ROWID(po-ordl)).
        
        /* Ensure receipts = payables */
        if not po-ordl.item-type and v-fgpostgl then do:
          release prod.
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
              no-error.
              
          if avail itemfg then
          find first prodl
              where prodl.company eq cocode
                and prodl.procat  eq itemfg.procat
                and can-find(first prod
                             where prod.company eq cocode
                               and prod.prolin  eq prodl.prolin)
              no-lock no-error.

          if avail prodl then
          find first prod
              where prod.company eq cocode
                and prod.prolin  eq prodl.prolin
              no-lock no-error.
                
          if avail itemfg then do:
            run sys/ref/convquom.p (po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                                    ap-invl.qty, output v-qty1).
                                   
            assign
             v-po-no = trim(string(po-ordl.po-no,">>>>>>>>>>"))
             v-qty   = 0
             v-cost  = ap-invl.amt / (v-qty1 / 1000).
                                   
            for each fg-rcpth
                where fg-rcpth.company   eq cocode
                  and fg-rcpth.i-no      eq po-ordl.i-no
                  and fg-rcpth.po-no     eq v-po-no
                  and fg-rcpth.rita-code eq "R"
                  and ((fg-rcpth.b-no    eq ap-invl.i-no and v-fgpostgl) or
                       (fg-rcpth.b-no    eq 0        and not v-fgpostgl))
                use-index item-po,
                
                each fg-rdtlh where fg-rdtlh.r-no eq fg-rcpth.r-no
                
                break by fg-rcpth.trans-date
                      BY fg-rdtlh.trans-time
                      by fg-rcpth.r-no
                      by recid(fg-rdtlh):
              
              assign
               v-qty         = v-qty + fg-rdtlh.qty
               fg-rdtlh.cost = v-cost
               fg-rcpth.b-no = ap-invl.i-no.
              
              if last(fg-rcpth.trans-date) and
                 v-qty ne v-qty1           then do:
                 
                find first fg-bin
                    where fg-bin.company eq cocode
                      and fg-bin.i-no    eq fg-rcpth.i-no
                      and fg-bin.loc     eq fg-rdtlh.loc
                      and fg-bin.loc-bin eq fg-rdtlh.loc-bin
                      and fg-bin.tag     eq fg-rdtlh.tag
                      and fg-bin.job-no  eq fg-rcpth.job-no
                      and fg-bin.job-no2 eq fg-rcpth.job-no2
                    no-error.  
 
                if not avail fg-bin then do:
                  create fg-bin.
                  assign
                   fg-bin.company      = fg-rdtlh.company
                   fg-bin.job-no       = fg-rcpth.job-no
                   fg-bin.job-no2      = fg-rcpth.job-no2
                   fg-bin.loc          = fg-rdtlh.loc
                   fg-bin.loc-bin      = fg-rdtlh.loc-bin
                   fg-bin.tag          = fg-rdtlh.tag
                   fg-bin.i-no         = fg-rcpth.i-no
                   fg-bin.case-count   = itemfg.case-count
                   fg-bin.cases-unit   = 1
                   fg-bin.aging-date   = fg-rcpth.trans-date
                   fg-bin.pur-uom      = "M"
                   fg-bin.std-tot-cost = fg-rdtlh.cost
                   fg-bin.std-mat-cost = fg-bin.std-tot-cost
                   fg-bin.std-lab-cost = 0
                   fg-bin.std-var-cost = 0
                   fg-bin.std-fix-cost = 0.
                end.
                 
                assign
                 v-qty1         = v-qty1 - v-qty
                 fg-rdtlh.qty   = fg-rdtlh.qty + v-qty1
                 fg-rdtlh.cases = trunc(fg-rdtlh.qty / fg-rdtlh.qty-case,0)
                 fg-bin.qty     = fg-bin.qty + v-qty1
                 itemfg.q-onh   = itemfg.q-onh + v-qty1.
              end.
            end.
            
            for each fg-rcpth
                where fg-rcpth.company   eq cocode
                  and fg-rcpth.i-no      eq po-ordl.i-no
                  and fg-rcpth.po-no     eq v-po-no
                  and fg-rcpth.rita-code eq "R"
                use-index item-po no-lock,
                
                each fg-rdtlh where fg-rdtlh.r-no eq fg-rcpth.r-no
                    
                break by fg-rcpth.job-no
                      by fg-rcpth.job-no2
                      by fg-rdtlh.loc
                      by fg-rdtlh.loc-bin
                      by fg-rdtlh.tag:
                      
              if first-of(fg-rdtlh.tag) then
                assign
                 v-qty  = 0
                 v-cost = 0.
                      
              assign
               v-qty  = v-qty + fg-rdtlh.qty
               v-cost = v-cost + (fg-rdtlh.qty / 1000 * fg-rdtlh.cost).
              
              if last-of(fg-rdtlh.tag) then do:
                find first fg-bin
                    where fg-bin.company eq cocode
                      and fg-bin.i-no    eq fg-rcpth.i-no
                      and fg-bin.loc     eq fg-rdtlh.loc
                      and fg-bin.loc-bin eq fg-rdtlh.loc-bin
                      and fg-bin.tag     eq fg-rdtlh.tag
                      and fg-bin.job-no  eq fg-rcpth.job-no
                      and fg-bin.job-no2 eq fg-rcpth.job-no2
                    no-error.  
 
                if not avail fg-bin then do:
                  create fg-bin.
                  assign
                   fg-bin.company      = fg-rdtlh.company
                   fg-bin.job-no       = fg-rcpth.job-no
                   fg-bin.job-no2      = fg-rcpth.job-no2
                   fg-bin.loc          = fg-rdtlh.loc
                   fg-bin.loc-bin      = fg-rdtlh.loc-bin
                   fg-bin.tag          = fg-rdtlh.tag
                   fg-bin.i-no         = fg-rcpth.i-no
                   fg-bin.case-count   = itemfg.case-count
                   fg-bin.cases-unit   = 1
                   fg-bin.aging-date   = fg-rcpth.trans-date
                   fg-bin.pur-uom      = "M"
                   fg-bin.std-tot-cost = fg-rdtlh.cost
                   fg-bin.std-mat-cost = fg-bin.std-tot-cost
                   fg-bin.std-lab-cost = 0
                   fg-bin.std-var-cost = 0
                   fg-bin.std-fix-cost = 0.
                end.
                
                v-cost = v-cost / (v-qty / 1000).
                  
                if fg-bin.pur-uom eq "M" then
                  fg-bin.std-tot-cost = v-cost.
                else
                  run sys/ref/convcuom.p ("M", fg-bin.pur-uom, 0, 0, 0, 0,
                                          v-cost, output fg-bin.std-tot-cost).
                                         
                assign
                 fg-bin.std-mat-cost = fg-bin.std-tot-cost
                 fg-bin.std-lab-cost = 0
                 fg-bin.std-var-cost = 0
                 fg-bin.std-fix-cost = 0.
              end.
            end.
          end.
          
          run fg/updfgcst.p (po-ordl.i-no).
        end.
      end.

      IF ap-invl.actnum NE "" THEN
      FIND FIRST bank
          WHERE bank.company EQ cocode
            AND bank.actnum  EQ ap-invl.actnum
          EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bank THEN bank.bal = bank.bal + ap-invl.amt.
      RELEASE bank.
    end.  /* each line */

    find first vend
        where vend.company eq cocode
          and vend.vend-no eq ap-inv.vend-no
        use-index vend exclusive-lock.

    assign
     vend.purch[tran-period]   = vend.purch[tran-period] + t1
     vend.n-purch[tran-period] = vend.n-purch[tran-period] + 1
     vend.purch[13]        = vend.purch[13] + t1
     vend.n-purch[13]      = vend.n-purch[13] + 1
     vend.ptd-msf[tran-period] = vend.ptd-msf[tran-period] + total-msf
     vend.ytd-msf          = vend.ytd-msf + total-msf
     vend.acc-bal          = vend.acc-bal + t1.

    if vend.acc-bal ge vend.hibal then
      assign
       vend.hibal      = vend.acc-bal
       vend.hibal-date = ap-inv.inv-date.

    FIND CURRENT vend NO-LOCK NO-ERROR.
    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    FIND CURRENT itemfg NO-LOCK NO-ERROR.
    FIND CURRENT fg-bin NO-LOCK NO-ERROR.
    FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.
    FIND CURRENT fg-rcpth NO-LOCK NO-ERROR.

    create ap-ledger.
    assign
     ap-ledger.company  = cocode
     ap-ledger.vend-no  = ap-inv.vend-no
     ap-ledger.amt      = ap-inv.net
     ap-ledger.refnum   = "INV# " + ap-inv.inv-no
     ap-ledger.ref-date = ap-inv.inv-date
     ap-ledger.trnum    = v-trnum
     ap-ledger.period   = tran-period
     ap-ledger.tr-date  = tran-date.
    
    RELEASE ap-ledger.

    assign
     t1            = 0
     ap-inv.posted = yes.
      
    IF apautocheck-log AND ap-inv.receiver-no NE "0" THEN
       RUN create-manual-check-proc.

    ACCUM ap-inv.net (TOTAL BY tt-report.actnum).
      
    ACCUM tt-report.curr-amt - (ap-inv.net + ap-inv.freight) (TOTAL BY tt-report.actnum).

    ACCUM ap-inv.freight * tt-report.ex-rate (TOTAL).

    IF LAST-OF(tt-report.actnum) AND
       tt-report.actnum NE ""    AND
       (ACCUM TOTAL BY tt-report.actnum tt-report.curr-amt - (ap-inv.net + ap-inv.freight))
                        NE 0    THEN DO:
      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = tt-report.actnum
       gltrans.jrnl    = "ACPAY"
       gltrans.tr-dscr = "ACCOUNTS PAYABLE CURRENCY GAIN/LOSS"
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = (ACCUM TOTAL BY tt-report.actnum tt-report.curr-amt - (ap-inv.net + ap-inv.freight)) * -1
       gltrans.period  = tran-period
       gltrans.trnum   = v-trnum.
      RELEASE gltrans.
    END.
  end. /* for each ap-inv */

  g2 = g2 + lv-frt-total.

  if lv-frt-total ne 0 then do:
    create gltrans.
    assign
     gltrans.company = cocode
     gltrans.actnum  = v-frt-acct
     gltrans.jrnl    = "ACPAY"
     gltrans.tr-dscr = "ACCOUNTS PAYABLE FREIGHT"
     gltrans.tr-date = tran-date
     gltrans.tr-amt  = (ACCUM TOTAL ap-inv.freight * tt-report.ex-rate)
     gltrans.period  = tran-period
     gltrans.trnum   = v-trnum.
    RELEASE gltrans.
  end.

  FOR EACH tt-ap-tax BREAK BY tt-ap-tax.actnum:
    ACCUM tt-ap-tax.curr-amt (TOTAL BY tt-ap-tax.actnum).

    g2 = g2 + tt-ap-tax.amt.

    IF LAST-OF(tt-ap-tax.actnum) THEN DO:
      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = tt-ap-tax.actnum
       gltrans.jrnl    = "ACPAY"
       gltrans.tr-dscr = "ACCOUNTS PAYABLE TAX"
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = (ACCUM TOTAL BY tt-ap-tax.actnum tt-ap-tax.curr-amt)
       gltrans.period  = tran-period
       gltrans.trnum   = v-trnum.
      RELEASE gltrans.
    END.
  END.
  
  create gltrans.
  assign
   gltrans.company = cocode
   gltrans.actnum  = xap-acct
   gltrans.jrnl    = "ACPAY"
   gltrans.tr-dscr = "ACCOUNTS PAYABLE INVOICE"
   gltrans.tr-date = tran-date
   gltrans.tr-amt  = - g2
   gltrans.period  = tran-period
   gltrans.trnum   = v-trnum.
  RELEASE gltrans.


end. /* postit: transaction */

END PROCEDURE.

PROCEDURE copy-report-to-audit-dir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR targetfile AS CHAR FORMAT "X(150)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname2 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname3 AS CHAR FORMAT "X(20)" NO-UNDO.

 if index(lv-audit-dir,'P',1) > 0 then assign
 lv-audit-dir = "D" + SUBSTRING(lv-audit-dir,2,100).
  
  ASSIGN targetfile = lv-audit-dir + "\AP\VU3\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AP"
         dirname3 = lv-audit-dir + "\AP\VU3".

  OS-COPY VALUE(list-name) VALUE (targetfile).

  IF SEARCH(targetfile) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
    OS-CREATE-DIR VALUE(dirname2).
    OS-CREATE-DIR VALUE(dirname3).
    OS-COPY VALUE(list-name) VALUE (targetfile).
  END.
END PROCEDURE.


PROCEDURE clear-ap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  for each ap-inv
    where ap-inv.company  eq cocode
      and ap-inv.posted   eq no
      AND ap-inv.user-id  EQ prmUser
      /*
      and xap-inv.inv-date ge v-s-date
      and xap-inv.inv-date le v-e-date 
      and xap-inv.vend-no  ge v-s-vend
      and xap-inv.vend-no  le v-e-vend
      AND CAN-FIND(FIRST ap-invl where ap-invl.i-no eq xap-inv.i-no USE-INDEX i-no)
    use-index posted no-lock
    transaction:
    */   :

      IF NOT CAN-FIND(FIRST ap-invl WHERE ap-invl.i-no = ap-inv.i-no) THEN
         DELETE ap-inv.
  END.

END PROCEDURE.

PROCEDURE create-manual-check-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR X AS INT NO-UNDO.
  DEF VAR v-check-no AS INT NO-UNDO.

  v-check-no = INTEGER(ap-inv.receiver-no).

  /*IF CAN-FIND(FIRST ap-pay WHERE
     ap-pay.company = cocode AND
     ap-pay.check-no = v-check-no AND
     (ap-pay.check-act = lv-bank-acct OR 
      ap-pay.bank-code = v-bank-code) AND
      ap-pay.posted) THEN
      DO:
         cError = "Check Number has already been posted. Cannot Create Manual Check." .
           
         LEAVE.
      END.*/

  FOR EACH bf-chk NO-LOCK BY bf-chk.c-no DESCENDING:
      X = bf-chk.c-no.
      LEAVE.
  END.

  /* Code placed here will execute AFTER standard behavior.    */
  
  CREATE ap-chk.
  ASSIGN ap-chk.bank-code = v-bank-code
         ap-chk.check-no = v-check-no
         ap-chk.man-check = YES   
         ap-chk.check-date = ap-inv.inv-date
         ap-chk.c-no = X + 1
         ap-chk.company = cocode
         ap-chk.vend-no = ap-inv.vend-no
         ap-chk.check-amt = ap-inv.due.

  FIND FIRST bank WHERE
       bank.company = cocode AND
       bank.bank-code = ap-chk.bank-code
       NO-ERROR.

  IF AVAIL bank THEN DO:
     IF ap-chk.check-no > bank.last-chk THEN
        bank.last-chk = ap-chk.check-no.

     ap-chk.check-act = bank.actnum.

     RELEASE bank.
  END.

  CREATE ap-sel.
  ASSIGN ap-sel.company = cocode
         ap-sel.vend-no = ap-chk.vend-no
         ap-sel.check-no = ap-chk.check-no
         ap-sel.bank-code = ap-chk.bank-code
         ap-sel.man-check = YES
         ap-sel.pre-date = ap-chk.check-date
         ap-sel.actnum = IF lv-bank-acct <> "" THEN lv-bank-acct
                         ELSE "NO Account"
         ap-sel.inv-no = ap-inv.inv-no
         ap-sel.due-date = ap-inv.due-date
         ap-sel.inv-bal = ap-inv.due
         ap-sel.amt-paid = ap-inv.due.

  IF ap-sel.pre-date - ap-inv.inv-date LE ap-inv.disc-days THEN
     ap-sel.disc-amt = round(ap-inv.disc-% * ap-inv.net / 100,2).

  RELEASE ap-sel.
  RELEASE ap-chk.

END PROCEDURE.

PROCEDURE check-inv-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-period LIKE period.pnum NO-UNDO.
  DEFINE INPUT PARAMETER ip-date AS CHAR.
  DEF VAR lv-msg AS CHAR NO-UNDO.
  
  DEF VAR v-month AS CHAR . 
  ASSIGN  lv-msg = ""
           v-invalid-inv = NO 
            v-month = SUBSTRING(STRING(TODAY),1,2) .
  
      FIND FIRST period                   
         WHERE period.company EQ cocode
         AND period.pst     LE date(ip-date)
         AND period.pend    GE date(ip-date)
          AND period.pnum   EQ INT(v-month)
       NO-LOCK NO-ERROR.

      IF NOT AVAIL period THEN
         DO:
            lv-msg = "CAN NOT POST OUT OF PERIOD, ENTER SECURITY PASSWORD OR  ENTER TO RETURN".
         END.

         ELSE IF NOT period.pstat THEN
            lv-msg = "Perio for " + TRIM(STRING(ip-date)) + " is already closed, enter security password or enter to return".
         ELSE
            lv-msg = "".

         IF lv-msg NE "" THEN DO:
             IF NOT ll-secure THEN do:
                 cError = TRIM(lv-msg) + "...".
             END.
         END.

         IF lv-msg NE "" THEN DO:
            IF NOT ll-secure THEN do:  
               RUN sys/ref/d-passwd.w (1, OUTPUT ll-secure). 
               IF NOT ll-secure THEN v-invalid-inv = YES .
            END.
         END.

END PROCEDURE.
