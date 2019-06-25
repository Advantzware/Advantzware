

/*------------------------------------------------------------------------
    File        : fgpost.p
    Purpose     :  Finished Goods Post
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/xprint.i}
    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttTransPost NO-UNDO
        FIELD vtrnspost AS CHAR
        FIELD vpstfg AS CHAR.
DEFINE DATASET dsTransPost FOR ttTransPost.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnspost      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeginTag      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndTag        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeginUsrid    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndUsrid      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPstDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmGlActNm       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmshwinv        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTransPost.

     IF prmUser        = ? THEN ASSIGN     prmUser        = "".   
     IF prmtrnspost    = ? THEN ASSIGN     prmtrnspost    = "". 
     IF prmBeginTag    = ? THEN ASSIGN     prmBeginTag    = "". 
     IF prmEndTag      = ? THEN ASSIGN     prmEndTag      = "". 
     IF prmBeginUsrid  = ? THEN ASSIGN     prmBeginUsrid  = "".  
     IF prmEndUsrid    = ? THEN ASSIGN     prmEndUsrid    = "".  
     IF prmPstDate     = ? THEN ASSIGN     prmPstDate     = "". 
     IF prmGlActNm     = ? THEN ASSIGN     prmGlActNm     = "". 
     IF prmshwinv      = ? THEN ASSIGN     prmshwinv      = "". 
     IF prmOut         = ? THEN ASSIGN     prmOut         = "". 
     





DEFINE VARIABLE begin_userid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_userid AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE FI_beg-tag-no AS CHARACTER FORMAT "X(20)" NO-UNDO.               
DEFINE VARIABLE FI_end-tag-no AS CHARACTER FORMAT "X(20)" NO-UNDO.               
DEFINE VARIABLE post-date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO. 
DEFINE VARIABLE tg_account AS LOGICAL INITIAL no NO-UNDO.                        
DEFINE VARIABLE tg_show-inv AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.




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
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR lv-post AS LOG NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.

/*{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}*/



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
 vuser     = prmUser  .
 

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


DEF NEW SHARED VAR choice AS LOG NO-UNDO.



DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR tran-date AS DATE NO-UNDO.
DEF VAR tran-period AS INT NO-UNDO.


    

 

def buffer b-fg-rctd   for fg-rctd.
def buffer b-itemfg     for itemfg.
def buffer b-fg-bin     for fg-bin.

def var v-post-date     as   date init today.
def var v-gl            as   log init no.

def var save_id         as   recid.
def var v-qty-onh       as   dec.
def var v-temp-cost     as   dec format "->>>>>9.99".
def var time_stamp      as   char.
def var v-cum-qty       as   dec format "->>>>>>9".
def var v-tot-value     as   dec format "->>>,>>>,>>9.99".
DEF VAR v-sell-price    LIKE itemfg.sell-price .
def var v-tot-price     as   dec format "->>>,>>>,>>9.99".
def var v-item-tot      as   dec format "->>>,>>>,>>9.99".
def var v-std-cost      as   dec format ">>>,>>9.99<<".
def var v-q-adj-ytd     as   int.
def var v-adj-qty       as   int.
def var v-dscr          like account.dscr.
def var v-disp-actnum   like account.actnum.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr".
def var v-cost          like itemfg.std-tot-cost extent 4.
def var v-uom           like itemfg.prod-uom.

DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.



def new shared var v-trnum as int.

def TEMP-TABLE w-fg-rctd NO-UNDO like fg-rctd.

DEF TEMP-TABLE tt-fg-bin NO-UNDO
    FIELD rct-date AS DATE
    FIELD i-no AS CHAR    
    FIELD i-name AS CHAR
    FIELD part-no AS CHAR
    FIELD sell-price LIKE itemfg.sell-price
    FIELD job-no AS CHAR  
    FIELD job-no2 AS INT
    FIELD loc AS CHAR
    FIELD loc-bin AS CHAR
    FIELD tag AS CHAR
    FIELD on-hand-qty AS INT
    FIELD counted-qty AS INT
    FIELD pur-uom AS CHAR
    FIELD std-cost AS DEC FORMAT ">>>,>>9.99<<"
    FIELD tot-value AS DEC FORMAT "->>>,>>>,>>9.99"
    FIELD seq-no AS INT
    FIELD count-trans AS LOG
    FIELD v-cost like itemfg.std-tot-cost extent 4
    FIELD v-uom AS CHAR
    FIELD v-adj-qty AS INT
    INDEX i-no i-no job-no job-no2 loc loc-bin tag
    INDEX seq-no seq-no.

{oe/invwork.i new}
{sys/FORM/r-topw.f}

{fg/fullset.i NEW}






  IF prmtrnspost = "TransPost" THEN DO:
      
     
        ASSIGN
        v-today        = TODAY              
        begin_userid   = prmBeginUsrid                      
        end_userid     = prmEndUsrid                        
        FI_beg-tag-no  = prmBeginTag                      
        FI_end-tag-no  = prmEndTag                        
        post-date      = date(prmPstDate)
        tg_account     = IF prmGlActNm = "Yes" THEN TRUE ELSE FALSE                      
        tg_show-inv    = IF prmshwinv = "Yes" THEN TRUE ELSE FALSE.   

           
           ASSIGN post-date = tran-date
                v-post-date = tran-date.

           assign
      init-dir    =  v-webrootpath.
        
        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "transpost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .
        vTextFile2 =  "transpost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .



        IF NOT tg_show-inv THEN
            run run-report.
        ELSE
            RUN run-report-inv.


        IF v-postable THEN DO: 
            lv-post = YES.
            
            IF lv-post THEN do:
                RUN cpost.
                cError = "Posting Complete" .     
            END. 
         END.

         ELSE do: 
             cError = "Nothing available for posting...".
             RETURN. 
         END.
         
    
        

    
  CREATE ttTransPost.
    ASSIGN ttTransPost.vtrnspost = vTextFile .


  
  


  END.
/*****************************************************************************************/

  PROCEDURE run-report PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign
 str-tit  = coname + " - " + loname
 str-tit2 = "FINISHED GOODS PHYSICAL COUNT - POSTING REPORT"
 str-tit3 = ""
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (112 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3.



form fg-rctd.rct-date     column-label "TRANS.!DATE"
     fg-rctd.i-no           label "ITEM"
     fg-rctd.i-name         format "x(20)" label "DESCRIPTION"
     itemfg.part-no         format "x(15)" label "Customer Part#"
     fg-rctd.job-no         label "   JOB" space(0) "-" space(0)
     fg-rctd.job-no2        label "# " format "99"
     fg-rctd.loc             label "WHSE"
     fg-rctd.loc-bin         label "BIN"
     fg-rctd.tag             label "TAG" FORM "x(23)"
     fg-rctd.t-qty           format "->>>>>>9" label "QUANTITY"
     v-sell-price            format "->>>,>>>,>>9.99" LABEL "Selling Value"
     fg-rctd.pur-uom        label "UOM"
     v-std-cost        label "COST/UOM"
     v-tot-value             label "TOTAL COST"
     SKIP
     with frame itemx no-box down width 175 STREAM-IO.

form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     udate         label "DATE"   
     v-disp-amt    label "AMOUNT" SKIP
    with down width 130 frame gldetail STREAM-IO.


time_stamp = string(time,"hh:mmam").

     /*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.


      {sys/inc/outprint.i value(lines-per-page)}

  v-postable = NO.

  /* gdm - 12050809 */
  FOR EACH fg-rctd NO-LOCK 
      WHERE fg-rctd.company   EQ cocode
        and fg-rctd.rita-code EQ "C"
        AND fg-rctd.loc-bin   NE ""
        AND fg-rctd.tag       GE FI_beg-tag-no 
        AND fg-rctd.tag       LE FI_end-tag-no,
      EACH reftable NO-LOCK 
      WHERE reftable.reftable EQ "fg-rctd.user-id" 
        AND reftable.company  EQ fg-rctd.company 
        AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
        AND reftable.code     GE begin_userid
        AND reftable.code     LE end_userid, 
      FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
      BREAK BY fg-rctd.i-no:

      ASSIGN
        v-sell-price = itemfg.sell-price .

    if first-of(fg-rctd.i-no) then do:
      put skip(1).
      ASSIGN
         v-cum-qty = 0
         v-tot-price = 0 .
    end.

    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq fg-rctd.i-no
          and fg-bin.loc     eq fg-rctd.loc
          and fg-bin.loc-bin eq fg-rctd.loc-bin
          and fg-bin.tag     eq fg-rctd.tag
          and fg-bin.job-no  eq fg-rctd.job-no
          and fg-bin.job-no2 eq fg-rctd.job-no2
        no-lock no-error.
    if not avail fg-bin and fg-rctd.tag ne "" then
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq fg-rctd.i-no
          and fg-bin.loc     eq fg-rctd.loc
          and fg-bin.loc-bin eq fg-rctd.loc-bin
          and fg-bin.tag     eq ""
          and fg-bin.job-no  eq fg-rctd.job-no
          and fg-bin.job-no2 eq fg-rctd.job-no2
        no-lock no-error.

    IF AVAIL fg-bin THEN
    DO:
       if fg-bin.pur-uom eq "EA" then
          v-std-cost = fg-rctd.std-cost.
       else
          run sys/ref/convcuom.p(fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                                 fg-rctd.std-cost, output v-std-cost).
    END.
    ELSE
       if itemfg.prod-uom eq "EA" then
          v-std-cost = fg-rctd.std-cost.
    else
       run sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                              fg-rctd.std-cost, output v-std-cost).

       IF AVAIL fg-bin THEN do: 
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ fg-bin.company AND
                 oe-ordl.job-no EQ fg-bin.job-no AND
                 oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                 oe-ordl.i-no EQ fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  v-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100)) .
       END.

    
    assign
     v-tot-value = v-std-cost * fg-rctd.t-qty
     v-cum-qty   = v-cum-qty + fg-rctd.t-qty
     v-tot-price = v-tot-price + v-sell-price .

    display fg-rctd.rct-date when first-of(fg-rctd.i-no)
            fg-rctd.i-no       when first-of(fg-rctd.i-no)
            fg-rctd.i-name
            itemfg.part-no
            fg-rctd.tag
            fg-rctd.t-qty
            fg-rctd.pur-uom
            fg-rctd.loc
            fg-rctd.loc-bin
            v-sell-price
            v-std-cost
            v-tot-value
            fg-rctd.job-no
            fg-rctd.job-no2
        with frame itemx.
    down with frame itemx.

    if avail fg-bin then
      assign
       v-cost[1] = fg-bin.std-lab-cost
       v-cost[2] = fg-bin.std-fix-cost
       v-cost[3] = fg-bin.std-var-cost
       v-cost[4] = fg-bin.std-mat-cost
       v-uom     = fg-bin.pur-uom.
    else   
      assign
       v-cost[1] = itemfg.std-lab-cost
       v-cost[2] = itemfg.std-fix-cost
       v-cost[3] = itemfg.std-var-cost
       v-cost[4] = itemfg.std-mat-cost
       v-uom     = itemfg.prod-uom.
 
    v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) - fg-rctd.t-qty.

    /*Invoicing  - Post Invoicing Transactions - Job Costing*/
    run oe/invposty.p (0, itemfg.i-no, v-adj-qty, v-uom,
                       v-cost[1], v-cost[2], v-cost[3], v-cost[4]).

    v-item-tot = v-item-tot + v-tot-value.

    if last-of(fg-rctd.i-no) then do:
      put "--------" to 116 "--------------" to 132 "---------------" to 163 skip
          "Item Total"                       to 107
          v-cum-qty                          to 116
          v-tot-price                        TO 132
          v-item-tot                         to 163 skip.

      v-item-tot = 0.
    end.
    v-postable = YES.
  end. /* each fg-rctd */

  if v-gl then
  for each work-job break by work-job.actnum:
  
    find first account
        where account.company eq cocode
          and account.actnum  eq work-job.actnum
        no-lock no-error.
        
    assign
     v-dscr        = if avail account then account.dscr
                     else "ACCOUNT NOT FOUND - " + work-job.actnum
     v-disp-actnum = work-job.actnum.

    if work-job.fg then
      v-disp-amt = - work-job.amt.
    else
      v-disp-amt = work-job.amt.

    
  end. /* each work-job */

  

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

PROCEDURE run-report-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-seq-no AS INT INIT 1 NO-UNDO.

assign
 str-tit  = coname + " - " + loname
 str-tit2 = "FINISHED GOODS PHYSICAL COUNT - POSTING REPORT"
 str-tit3 = ""
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (112 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3.

form tt-fg-bin.rct-date FORMAT "99/99/99" column-label "TRANS.!DATE"
     tt-fg-bin.i-no     label "ITEM"
     tt-fg-bin.i-name   format "x(20)" label "DESCRIPTION"
     tt-fg-bin.part-no  format "x(15)" label "Customer Part#"
     tt-fg-bin.job-no   label "   JOB" space(0) "-" space(0)
     tt-fg-bin.job-no2  label "# " format "99"
     tt-fg-bin.loc      label "WHSE"
     tt-fg-bin.loc-bin  label "BIN"
     tt-fg-bin.tag      label "TAG" FORM "x(8)"
     tt-fg-bin.on-hand-qty format "->>>>>>9" label "O/H Qty."
     tt-fg-bin.counted-qty format "->>>>>>9" LABEL "Cnt Qty."
     tt-fg-bin.sell-price format "->>>,>>>,>>9.99" LABEL "Selling Value."
     tt-fg-bin.pur-uom  label "UOM"
     tt-fg-bin.std-cost label "COST/UOM"
     tt-fg-bin.tot-value label "TOTAL COST"
     SKIP
     with frame itemx2 no-box down width 170 STREAM-IO.

form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     udate         label "DATE"   
     v-disp-amt    label "AMOUNT" SKIP
    with down width 133 frame gldetail2 STREAM-IO.

time_stamp = string(time,"hh:mmam").

 
if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.


      {sys/inc/outprint.i value(lines-per-page)}

  

  v-postable = NO.

  EMPTY TEMP-TABLE tt-fg-bin.

  FOR EACH fg-rctd NO-LOCK 
      WHERE fg-rctd.company   EQ cocode
        and fg-rctd.rita-code EQ "C"
        AND fg-rctd.loc-bin   NE ""
        AND fg-rctd.tag       GE FI_beg-tag-no 
        AND fg-rctd.tag       LE FI_end-tag-no,
      EACH reftable NO-LOCK 
      WHERE reftable.reftable EQ "fg-rctd.user-id" 
        AND reftable.company  EQ fg-rctd.company 
        AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
        AND reftable.code     GE begin_userid
        AND reftable.code     LE end_userid, 
      FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
      BREAK BY fg-rctd.i-no:

      CREATE tt-fg-bin.
      ASSIGN tt-fg-bin.rct-date = fg-rctd.rct-date
             tt-fg-bin.i-no = fg-rctd.i-no
             tt-fg-bin.i-name = fg-rctd.i-name
             tt-fg-bin.job-no = fg-rctd.job-no  
             tt-fg-bin.job-no2 = fg-rctd.job-no2
             tt-fg-bin.loc = fg-rctd.loc
             tt-fg-bin.loc-bin = fg-rctd.loc-bin
             tt-fg-bin.tag = fg-rctd.tag
             tt-fg-bin.counted-qty = fg-rctd.t-qty
             tt-fg-bin.pur-uom = fg-rctd.pur-uom
             tt-fg-bin.seq-no = v-seq-no
             tt-fg-bin.count-trans = YES
             tt-fg-bin.part-no = itemfg.part-no
             tt-fg-bin.sell-price = itemfg.sell-price
             v-seq-no = v-seq-no + 1.

      find first fg-bin WHERE
           fg-bin.company eq cocode AND
           fg-bin.i-no    eq fg-rctd.i-no AND
           fg-bin.loc     eq fg-rctd.loc AND
           fg-bin.loc-bin eq fg-rctd.loc-bin AND
           fg-bin.tag     eq fg-rctd.tag AND
           fg-bin.job-no  eq fg-rctd.job-no AND
           fg-bin.job-no2 eq fg-rctd.job-no2
           no-lock no-error.

      if not avail fg-bin and fg-rctd.tag ne "" then
         find first fg-bin WHERE
              fg-bin.company eq cocode AND
              fg-bin.i-no    eq fg-rctd.i-no AND
              fg-bin.loc     eq fg-rctd.loc AND
              fg-bin.loc-bin eq fg-rctd.loc-bin AND
              fg-bin.tag     eq "" AND
              fg-bin.job-no  eq fg-rctd.job-no AND
              fg-bin.job-no2 eq fg-rctd.job-no2
              no-lock no-error.
      
      IF AVAIL fg-bin THEN
      DO:
         if fg-bin.pur-uom eq "EA" then
            v-std-cost = fg-rctd.std-cost.
         else
            run sys/ref/convcuom.p(fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                                   fg-rctd.std-cost, output v-std-cost).
      END.
      ELSE
         if itemfg.prod-uom eq "EA" then
            v-std-cost = fg-rctd.std-cost.
      else
         run sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                fg-rctd.std-cost, output v-std-cost).

      ASSIGN
         tt-fg-bin.std-cost = v-std-cost
         tt-fg-bin.tot-value = v-std-cost * fg-rctd.t-qty.

      if avail fg-bin then
         assign
            tt-fg-bin.v-cost[1] = fg-bin.std-lab-cost
            tt-fg-bin.v-cost[2] = fg-bin.std-fix-cost
            tt-fg-bin.v-cost[3] = fg-bin.std-var-cost
            tt-fg-bin.v-cost[4] = fg-bin.std-mat-cost
            tt-fg-bin.v-uom     = fg-bin.pur-uom
            tt-fg-bin.on-hand-qty = fg-bin.qty.
      else   
         assign
            tt-fg-bin.v-cost[1] = itemfg.std-lab-cost
            tt-fg-bin.v-cost[2] = itemfg.std-fix-cost
            tt-fg-bin.v-cost[3] = itemfg.std-var-cost
            tt-fg-bin.v-cost[4] = itemfg.std-mat-cost
            tt-fg-bin.v-uom     = itemfg.prod-uom.

       IF AVAIL fg-bin THEN do: 
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ fg-bin.company AND
                 oe-ordl.job-no EQ fg-bin.job-no AND
                 oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                 oe-ordl.i-no EQ fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  tt-fg-bin.sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100)) .
       END.
        

      tt-fg-bin.v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) - fg-rctd.t-qty.

      RELEASE fg-bin.

      v-postable = YES.

      IF LAST-OF(fg-rctd.i-no) THEN
         FOR EACH fg-bin WHERE
             fg-bin.company EQ cocode AND
             fg-bin.i-no    EQ tt-fg-bin.i-no AND
             fg-bin.qty     NE 0
             USE-INDEX co-ino
             NO-LOCK:
        
             FIND FIRST tt-fg-bin WHERE
                  tt-fg-bin.i-no    eq fg-bin.i-no AND
                  tt-fg-bin.loc     eq fg-bin.loc AND
                  tt-fg-bin.loc-bin eq fg-bin.loc-bin AND
                  tt-fg-bin.tag     eq fg-bin.tag AND
                  tt-fg-bin.job-no  eq fg-bin.job-no AND
                  tt-fg-bin.job-no2 eq fg-bin.job-no2
                  NO-ERROR.
        
             IF NOT AVAIL tt-fg-bin THEN
             DO:
                CREATE tt-fg-bin.
                ASSIGN
                  tt-fg-bin.i-no    = fg-bin.i-no
                  tt-fg-bin.loc     = fg-bin.loc
                  tt-fg-bin.loc-bin = fg-bin.loc-bin
                  tt-fg-bin.tag     = fg-bin.tag
                  tt-fg-bin.job-no  = fg-bin.job-no
                  tt-fg-bin.job-no2 = fg-bin.job-no2
                  tt-fg-bin.seq-no = v-seq-no
                  v-seq-no = v-seq-no + 1.
             END.
        
             tt-fg-bin.on-hand-qty = fg-bin.qty.
        
             RELEASE tt-fg-bin.
         END. /*each fg-bin*/

  END. /*for each fg-rctd*/

  FOR EACH tt-fg-bin NO-LOCK 
      BREAK BY tt-fg-bin.i-no
            BY tt-fg-bin.seq-no:

    if first-of(tt-fg-bin.i-no) then do:
       put skip(1).
       ASSIGN
          v-tot-price = 0
          v-cum-qty   = 0.
    end.

    display tt-fg-bin.rct-date when first-of(tt-fg-bin.i-no)
            tt-fg-bin.i-no     when first-of(tt-fg-bin.i-no)
            tt-fg-bin.i-name
            tt-fg-bin.part-no
            tt-fg-bin.tag
            SUBSTR(tt-fg-bin.tag,16,8) WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no @ tt-fg-bin.tag
            tt-fg-bin.on-hand-qty
            tt-fg-bin.counted-qty WHEN tt-fg-bin.counted-qty NE 0
            tt-fg-bin.sell-price
            tt-fg-bin.pur-uom
            tt-fg-bin.loc
            tt-fg-bin.loc-bin
            tt-fg-bin.std-cost WHEN tt-fg-bin.count-trans
            tt-fg-bin.tot-value WHEN tt-fg-bin.count-trans
            tt-fg-bin.job-no
            tt-fg-bin.job-no2
        with frame itemx2.
    down with frame itemx2.

    ASSIGN
       v-tot-price = v-tot-price + tt-fg-bin.sell-price .
        
    IF tt-fg-bin.count-trans THEN
    DO:
       ASSIGN
          v-cum-qty = v-cum-qty + tt-fg-bin.counted-qty
          v-item-tot = v-item-tot + tt-fg-bin.tot-value.

       /*Invoicing  - Post Invoicing Transactions - Job Costing*/
       run oe/invposty.p (0, tt-fg-bin.i-no, tt-fg-bin.v-adj-qty, tt-fg-bin.v-uom,
                          tt-fg-bin.v-cost[1], tt-fg-bin.v-cost[2], tt-fg-bin.v-cost[3], tt-fg-bin.v-cost[4]).
    END.

    if last-of(tt-fg-bin.i-no) then do:
       put "--------" to 112 "---------------" to 128  "---------------" to 163 skip
           "Item Count Total"                 to 102
           v-cum-qty                          to 112
           v-tot-price                        TO 128
           v-item-tot                         to 163 skip.

       v-item-tot = 0.
    end.
  end. /* each fg-rctd */

  if v-gl then
     for each work-job break by work-job.actnum:
     
       find first account
           where account.company eq cocode
             and account.actnum  eq work-job.actnum
           no-lock no-error.
           
       assign
        v-dscr        = if avail account then account.dscr
                        else "ACCOUNT NOT FOUND - " + work-job.actnum
        v-disp-actnum = work-job.actnum.
    
       if work-job.fg then
         v-disp-amt = - work-job.amt.
       else
         v-disp-amt = work-job.amt.
    
       
     end. /* each work-job */

 
END PROCEDURE.

PROCEDURE cpost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b2-fg-bin FOR fg-bin.

EMPTY TEMP-TABLE w-fg-rctd.

postit:
  do transaction on error undo postit, leave postit:
    for each fg-rctd
        where fg-rctd.company   eq cocode
          and fg-rctd.rita-code eq "C"
          AND fg-rctd.tag       GE FI_beg-tag-no
          AND fg-rctd.tag       LE FI_end-tag-no
          AND ((begin_userid    LE "" AND
                end_userid      GE "") OR
               CAN-FIND(FIRST reftable
                        WHERE reftable.reftable EQ "fg-rctd.user-id"
                          AND reftable.company  EQ fg-rctd.company
                          AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
                          AND reftable.code     GE begin_userid
                          AND reftable.code     LE end_userid))
        no-lock,
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq fg-rctd.i-no
          and itemfg.isaset
          and itemfg.alloc
        NO-LOCK:

      RUN fg/fullset.p (ROWID(itemfg)).

      FOR EACH tt-fg-set,
          FIRST b-itemfg
          WHERE b-itemfg.company EQ cocode
            AND b-itemfg.i-no    EQ tt-fg-set.part-no
          NO-LOCK:

        x = 1.
        for each w-fg-rctd by w-fg-rctd.r-no desc:
          leave.
        end.
        if avail w-fg-rctd THEN x = w-fg-rctd.r-no + 1.
        FOR each b-fg-rctd NO-LOCK BY b-fg-rctd.r-no DESCENDING:
          IF b-fg-rctd.r-no GE X THEN X = b-fg-rctd.r-no + 1.
          LEAVE.
        END.
      
        find last fg-rcpth use-index r-no no-lock no-error.
        if avail fg-rcpth and fg-rcpth.r-no ge x THEN x = fg-rcpth.r-no + 1.

        create w-fg-rctd.
        buffer-copy fg-rctd to w-fg-rctd
        assign
         w-fg-rctd.i-no   = b-itemfg.i-no
         w-fg-rctd.i-name = b-itemfg.i-name
         w-fg-rctd.r-no   = x.
          
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq itemfg.i-no
              and fg-bin.loc     eq fg-rctd.loc
              and fg-bin.loc-bin eq fg-rctd.loc-bin
              and fg-bin.tag     eq fg-rctd.tag
              and fg-bin.job-no  eq fg-rctd.job-no
              and fg-bin.job-no2 eq fg-rctd.job-no2
              and fg-bin.cust-no eq fg-rctd.cust-no
            use-index co-ino no-error.
        v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) * tt-fg-set.part-qty-dec.
        
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq b-itemfg.i-no
              and fg-bin.loc     eq fg-rctd.loc
              and fg-bin.loc-bin eq fg-rctd.loc-bin
              and fg-bin.tag     eq fg-rctd.tag
              and fg-bin.job-no  eq fg-rctd.job-no
              and fg-bin.job-no2 eq fg-rctd.job-no2
              and fg-bin.cust-no eq fg-rctd.cust-no
            use-index co-ino no-error.
        v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) - v-adj-qty.
        
        if v-adj-qty lt 0 then v-adj-qty = 0.

        ASSIGN w-fg-rctd.t-qty = (fg-rctd.t-qty * tt-fg-set.part-qty-dec) + v-adj-qty.
      END.
    END.

    {fg/fg-cpst.i w-}

    {fg/fg-cpst.i}

    if v-gl then do:
      /** GET next G/L TRANS. POSTING # **/
      /* gdm - 11050906 */
      REPEAT:
       FIND FIRST gl-ctrl EXCLUSIVE-LOCK
         WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
       IF AVAIL gl-ctrl THEN DO:

         ASSIGN v-trnum       = gl-ctrl.trnum + 1
                gl-ctrl.trnum = v-trnum.

         FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
         LEAVE.
       END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */

      for each work-job break by work-job.actnum:
        create gltrans.
        assign
         gltrans.company = cocode
         gltrans.actnum  = work-job.actnum
         gltrans.jrnl    = "OEINV"
         gltrans.tr-date = udate
         gltrans.period  = uperiod
         gltrans.trnum   = v-trnum.
    
        if work-job.fg then
          assign
           gltrans.tr-amt  = - work-job.amt
           gltrans.tr-dscr = "ORDER ENTRY INVOICE FG".
        else
          assign
           gltrans.tr-amt  = work-job.amt
           gltrans.tr-dscr = "ORDER ENTRY INVOICE COGS".
      end. /* each work-job */
    end.
  end. /* postit */



END PROCEDURE.
