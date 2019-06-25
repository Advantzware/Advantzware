

/*------------------------------------------------------------------------
    File        : BrwsCEstimate.p
    Purpose     : Corrugated Box
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttmoveViewFGrece NO-UNDO
        FIELD vRno          AS INT
        FIELD vDate           AS CHAR  
        FIELD vTransTime      AS CHAR   
        FIELD vTag            AS CHAR FORMAT "x(20)"
        FIELD vPo_no          AS CHAR FORMAT "x(9)"
        FIELD vJob_no         AS CHAR FORMAT "x(6)"  
        FIELD vJob_no2        AS INT
        FIELD vItem           AS CHAR FORMAT "x(15)"
        FIELD vItemName       AS CHAR FORMAT "x(30)"
        FIELD vLoc            AS CHAR  FORMAT "x(5)" 
        FIELD vLocBin         AS CHAR  FORMAT "x(8)"  
        FIELD vCases          AS INT 
        FIELD vQtyCas         AS INT 
        FIELD vCasUnit        AS INT    
        FIELD vPartial        AS INT    
        FIELD vStdCost        AS DEC
        FIELD vCostUom        AS CHAR FORMAT "x(3)"
        FIELD vT_Qty          AS DEC
        FIELD vFrtCost        AS DEC
        FIELD vExtCost        AS DEC
        FIELD vStackCode      AS CHAR FORMAT "x(20)"   
        FIELD vCreatedBy      AS CHAR   FORMAT "x(9)"
        FIELD vCreate2        AS CHAR  FORMAT "x(9)"
        FIELD vTot_Wt         AS DEC
        FIELD goods             AS CHAR
        FIELD rece           AS CHAR
        FIELD extra          AS CHAR
        FIELD vRecKey         AS CHAR 
        .


DEFINE DATASET dsmoveViewFGrece FOR ttmoveViewFGrece .

DEFINE INPUT PARAMETER prmUser           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJobno          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPono           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqno          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRcptDate       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTagno          AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmTransTime      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJob_no2        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmName           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLoc            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLocBin         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCases          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty_Cas        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCasUnit        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPartial        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmStdCost        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCost_Uom       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTQty           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFrtCost        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmExtCost        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmStackCode      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCreatedBy      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCreate2        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTotWt          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey         AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError            AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsmoveViewFGrece.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
/*DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.*/
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc   AS CHAR NO-UNDO.
DEF VAR v-auto-add-tag AS LOG NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR v-next-tag AS cha NO-UNDO.
{sys/inc/var.i "new shared"}



DEF VAR ll-help-run AS LOG NO-UNDO.

DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.
DEF VAR v-post-date AS DATE INITIAL TODAY.
def var fg-uom-list  as char NO-UNDO.
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
DEF VAR v-prgmname AS CHAR INIT "b-rcptd." NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.


{fg/fg-post3.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/invrecpt.i NEW}

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd 
    FIELD row-id   AS ROWID
    FIELD has-rec  AS LOG INIT NO
    FIELD invoiced AS LOG INIT NO.

DEF TEMP-TABLE tt-email FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DEF STREAM logFile.
DEF STREAM st-email.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.

{sys/inc/ssmovefg.i}


IF prmUser          = ? THEN ASSIGN prmUser        = "".
IF prmAction        = ? THEN ASSIGN prmAction      = "Select".
IF prmFgItem        = ? THEN ASSIGN prmFgItem      = "".
IF prmJobno         = ? THEN ASSIGN prmJobno       = "".  
IF prmPono          = ? THEN ASSIGN prmPono        = "".
IF prmSeqno         = ? THEN ASSIGN prmSeqno       = "".
IF prmRcptDate      = ? THEN ASSIGN prmRcptDate    = "".
IF prmTagno         = ? THEN ASSIGN prmTagno       = "".
IF prmTransTime     = ? THEN ASSIGN prmTransTime   = "".
IF prmJob_no2       = ? THEN ASSIGN prmJob_no2     = "0".
IF prmName          = ? THEN ASSIGN prmName    = "".
IF prmLoc           = ? THEN ASSIGN prmLoc         = "".
IF prmLocBin        = ? THEN ASSIGN prmLocBin      = "".
IF prmCases         = ? THEN ASSIGN prmCases       = "0".
IF prmQty_Cas        = ? THEN ASSIGN prmQty_Cas      = "0".
IF prmCasUnit       = ? THEN ASSIGN prmCasUnit     = "0".
IF prmPartial       = ? THEN ASSIGN prmPartial     = "0".
IF prmStdCost       = ? THEN ASSIGN prmStdCost     = "0".
IF prmCost_Uom       = ? THEN ASSIGN prmCost_Uom     = "".
IF prmTQty          = ? THEN ASSIGN prmTQty        = "0".
IF prmFrtCost       = ? THEN ASSIGN prmFrtCost     = "0".
IF prmExtCost       = ? THEN ASSIGN prmExtCost     = "0".
IF prmStackCode     = ? THEN ASSIGN prmStackCode   = "".
IF prmCreatedBy     = ? THEN ASSIGN prmCreatedBy   = "".
IF prmCreate2       = ? THEN ASSIGN prmCreate2     = "".
IF prmTotWt         = ? THEN ASSIGN prmTotWt       = "0".
IF prmRecKey        = ? THEN ASSIGN prmRecKey      = "".
IF cError           = ? THEN ASSIGN cError         = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp
    g_company = prmComp
    vuser     = prmUser 
    g_loc     = "Main" .

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DO TRANSACTION:
   {sys/inc/closejob.i FGPost}
   {sys/inc/fgpostgl.i}
   {sys/inc/fgemails.i}
   {sys/inc/adjustgl.i}
   
END.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGPOTAG#"
    no-lock no-error.
v-auto-add-tag = NO.
IF AVAIL sys-ctrl THEN
  v-auto-add-tag = sys-ctrl.log-fld.

DEF TEMP-TABLE tt-fg-rctd LIKE fg-rctd
    FIELD tt-rowid AS ROWID
    FIELD po-rowid AS ROWID.


   FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

 IF prmAction = "GridSelect" THEN DO:

     FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
          (fg-rctd.rita-code eq "R" ) NO-LOCK :

         create ttmoveViewFGrece.
           assign
               ttmoveViewFGrece.vRno             = fg-rctd.r-no
               ttmoveViewFGrece.vDate            = string(fg-rctd.rct-date)
               ttmoveViewFGrece.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
               ttmoveViewFGrece.vTag             = fg-rctd.tag
               ttmoveViewFGrece.vPo_no           = fg-rctd.po-no
               ttmoveViewFGrece.vJob_no          = fg-rctd.job-no
               ttmoveViewFGrece.vJob_no2         = fg-rctd.job-no2
               ttmoveViewFGrece.vItem            = fg-rctd.i-no
               ttmoveViewFGrece.vItemName        = fg-rctd.i-name
               ttmoveViewFGrece.vLoc             = fg-rctd.loc 
               ttmoveViewFGrece.vLocBin          = fg-rctd.loc-bin
               ttmoveViewFGrece.vCases           = fg-rctd.cases 
               ttmoveViewFGrece.vQtyCas          = fg-rctd.qty-case 
               ttmoveViewFGrece.vCasUnit         = fg-rctd.cases-unit  
               ttmoveViewFGrece.vPartial         = fg-rctd.partial
               ttmoveViewFGrece.vStdCost         = fg-rctd.std-cost
               ttmoveViewFGrece.vCostUom         = fg-rctd.cost-uom
               ttmoveViewFGrece.vT_Qty           = fg-rctd.t-qty
               ttmoveViewFGrece.vFrtCost         = fg-rctd.frt-cost
               ttmoveViewFGrece.vExtCost         = fg-rctd.ext-cost
               ttmoveViewFGrece.vStackCode       = fg-rctd.stack-code 
               /*ttmoveViewFGrece.vCreatedBy       = reftable.code
               ttmoveViewFGrece.vCreate2         = reftable.code2 */ 
               ttmoveViewFGrece.vTot_Wt          = fg-rctd.tot-wt 
               ttmoveViewFGrece.vRecKey          = fg-rctd.rec_key .



     END.
 
     
 END.  /* end of grid select */



 IF prmAction = "GridSearch" THEN DO:

      FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and 
            (fg-rctd.rita-code eq "R" ) AND
            (fg-rctd.tag  BEGINS prmTagno OR  prmTagno = "") NO-LOCK :

        create ttmoveViewFGrece.
          assign
              ttmoveViewFGrece.vRno             = fg-rctd.r-no
              ttmoveViewFGrece.vDate            = string(fg-rctd.rct-date)
              ttmoveViewFGrece.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
              ttmoveViewFGrece.vTag             = fg-rctd.tag
              ttmoveViewFGrece.vPo_no           = fg-rctd.po-no
              ttmoveViewFGrece.vJob_no          = fg-rctd.job-no
              ttmoveViewFGrece.vJob_no2         = fg-rctd.job-no2
              ttmoveViewFGrece.vItem            = fg-rctd.i-no
              ttmoveViewFGrece.vItemName        = fg-rctd.i-name
              ttmoveViewFGrece.vLoc             = fg-rctd.loc 
              ttmoveViewFGrece.vLocBin          = fg-rctd.loc-bin
              ttmoveViewFGrece.vCases           = fg-rctd.cases 
              ttmoveViewFGrece.vQtyCas          = fg-rctd.qty-case 
              ttmoveViewFGrece.vCasUnit         = fg-rctd.cases-unit  
              ttmoveViewFGrece.vPartial         = fg-rctd.partial
              ttmoveViewFGrece.vStdCost         = fg-rctd.std-cost
              ttmoveViewFGrece.vCostUom         = fg-rctd.cost-uom
              ttmoveViewFGrece.vT_Qty           = fg-rctd.t-qty
              ttmoveViewFGrece.vFrtCost         = fg-rctd.frt-cost
              ttmoveViewFGrece.vExtCost         = fg-rctd.ext-cost
              ttmoveViewFGrece.vStackCode       = fg-rctd.stack-code 
              /*ttmoveViewFGrece.vCreatedBy       = reftable.code
              ttmoveViewFGrece.vCreate2         = reftable.code2 */ 
              ttmoveViewFGrece.vTot_Wt          = fg-rctd.tot-wt 
              ttmoveViewFGrece.vRecKey          = fg-rctd.rec_key .



    END.


END.  /* end of grid select */







   

IF prmAction = "Update" THEN DO: 
   
  FIND FIRST fg-rctd WHERE fg-rctd.company = cocode NO-LOCK NO-ERROR.

  FIND FIRST loc WHERE loc.company = cocode
      AND loc.loc = prmLoc NO-LOCK NO-ERROR.
  IF NOT AVAIL loc THEN DO:
      cError =  "Invalid Warehouse. Try Help. " .
      RETURN .
  END.

  FIND FIRST fg-bin WHERE fg-bin.company = cocode
      AND fg-bin.i-no = ""
      AND fg-bin.loc = prmLoc
      AND fg-bin.loc-bin = prmLocBin
      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
      cError = "Invalid Bin#. Try Help. " .
      RETURN .
  END.
      
       

  
            
END.  /*** update*/  

IF prmAction = "Update" THEN DO:
  
    FIND FIRST fg-rctd WHERE fg-rctd.company eq cocode and 
           fg-rctd.r-no EQ int(prmSeqno) AND 
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") 
           use-index fg-rctd EXCLUSIVE-LOCK NO-ERROR.
      FIND FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL fg-rctd AND AVAIL reftable THEN
          ASSIGN
              
            fg-rctd.loc         = prmLoc           
            fg-rctd.loc-bin     = prmLocBin    
            reftable.code2      = prmUser      .
     
          ASSIGN
          prmAction = "Select" . 
END.  /**** update  ***/ 




IF prmAction = "PostMove" THEN DO:

    FIND FIRST fg-rctd WHERE fg-rctd.company eq cocode and 
               fg-rctd.r-no EQ int(prmSeqno) AND 
               (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") 
               use-index fg-rctd EXCLUSIVE-LOCK NO-ERROR.
          FIND FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                 reftable.company  EQ fg-rctd.company AND 
                 reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") EXCLUSIVE-LOCK NO-ERROR.
         
          RUN post-finished-goods.


END. /* end of post move */

    

 IF prmAction = "Select" THEN DO:
  
  
  FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
           fg-rctd.r-no EQ int(prmSeqno) AND
           (fg-rctd.rita-code eq "R" ) 
           use-index fg-rctd NO-LOCK:
    
            create ttmoveViewFGrece.
            assign
                ttmoveViewFGrece.vRno             = fg-rctd.r-no
                ttmoveViewFGrece.vDate            = string(fg-rctd.rct-date)
                ttmoveViewFGrece.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
                ttmoveViewFGrece.vTag             = fg-rctd.tag
                ttmoveViewFGrece.vPo_no           = fg-rctd.po-no
                ttmoveViewFGrece.vJob_no          = fg-rctd.job-no
                ttmoveViewFGrece.vJob_no2         = fg-rctd.job-no2
                ttmoveViewFGrece.vItem            = fg-rctd.i-no
                ttmoveViewFGrece.vItemName        = fg-rctd.i-name
                ttmoveViewFGrece.vLoc             = fg-rctd.loc 
                ttmoveViewFGrece.vLocBin          = fg-rctd.loc-bin
                ttmoveViewFGrece.vCases           = fg-rctd.cases 
                ttmoveViewFGrece.vQtyCas          = fg-rctd.qty-case 
                ttmoveViewFGrece.vCasUnit         = fg-rctd.cases-unit  
                ttmoveViewFGrece.vPartial         = fg-rctd.partial
                ttmoveViewFGrece.vStdCost         = fg-rctd.std-cost
                ttmoveViewFGrece.vCostUom         = fg-rctd.cost-uom
                ttmoveViewFGrece.vT_Qty           = fg-rctd.t-qty
                ttmoveViewFGrece.vFrtCost         = fg-rctd.frt-cost
                ttmoveViewFGrece.vExtCost         = fg-rctd.ext-cost
                ttmoveViewFGrece.vStackCode       = fg-rctd.stack-code 
                /*ttmoveViewFGrece.vCreatedBy       = reftable.code
                ttmoveViewFGrece.vCreate2         = reftable.code2 */ 
                ttmoveViewFGrece.vTot_Wt          = fg-rctd.tot-wt 
                ttmoveViewFGrece.vRecKey          = fg-rctd.rec_key .


               

                      /* v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.*/
                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/


   
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/



PROCEDURE post-finished-goods :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer b-fg-rcpts for fg-rcpts.
  def buffer b-fg-rdtl for fg-rdtl.
  def buffer b-fg-bin for fg-bin.
  DEF BUFFER b-itemfg FOR itemfg.
  def buffer b-itemfg1 for itemfg.
  def buffer ps-rctd for fg-rctd .
  def buffer b-po-ordl for po-ordl.
  def buffer b-oe-ordl for oe-ordl.

  def var v-one-item as log.
  def var v-dec as dec decimals 10.
  def var v-po-no like rm-rcpt.po-no no-undo.
  def var x as int no-undo.
  def var i as int no-undo.
  def var v-r-qty like fg-rctd.qty no-undo.
  def var v-i-qty like fg-rctd.qty no-undo.
  def var v-t-qty like fg-rctd.qty no-undo.
  def var v-overrun-qty like fg-rctd.qty no-undo.
  def var v-underrun-qty like fg-rctd.qty no-undo.
  DEF VAR v-reduce-qty AS INT NO-UNDO.
  DEF VAR v-est-no AS cha NO-UNDO.
  def var v-recid as recid no-undo.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-binqty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-tagcost AS DEC NO-UNDO.
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec DECIMALS 10 no-undo.
  def var v-autobin  as cha no-undo.
  def var v-newhdr as log no-undo. 
  def var v-fin-qty as dec no-undo.
  def var choice as log no-undo.
  def var v-trnum like gl-ctrl.trnum no-undo.
  def var uperiod as int no-undo.
  def var sysdate as date init today no-undo.    
  def var v-date like sysdate no-undo.
  DEF VAR v-underrun AS DEC NO-UNDO.
  DEF VAR v-qty-received AS INT NO-UNDO.
  DEF VAR v-got-fgemail AS LOG NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR li-tag-no AS INT NO-UNDO.
  DEF VAR ll-qty-changed AS LOG NO-UNDO.
  DEF VAR ll-whs-item AS LOG NO-UNDO.

  DEFINE VARIABLE fgPostLog AS LOGICAL NO-UNDO.

  fgPostLog = SEARCH('logs/fgpstall.log') NE ?.
  IF fgPostLog THEN
  OUTPUT STREAM logFile TO VALUE( v-webrootpath + '/fgpstall.' +
         STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').

  
  IF fgPostLog THEN RUN fgPostLog ('Started').
  
  FIND FIRST period NO-LOCK
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date NO-ERROR.
  

  IF NOT AVAIL period THEN DO:
      cError = "Find First/Last Failed For Table Period" .
      RETURN.
  END.
  
  find first sys-ctrl  where sys-ctrl.company eq cocode
                         and sys-ctrl.name    eq "AUTOPOST"
       no-lock no-error.
  v-autobin = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

  FOR EACH w-fg-rctd:
    DELETE w-fg-rctd.
  END.

  /* Create a single workfile record for the finished good being posted */
  CREATE w-fg-rctd.
  BUFFER-COPY fg-rctd TO w-fg-rctd
  ASSIGN w-fg-rctd.row-id  = ROWID(fg-rctd)
         w-fg-rctd.has-rec = YES.

  FOR EACH w-fg-rctd,

        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no

        BY w-fg-rctd.tag
        BY w-fg-rctd.rct-date
        BY w-fg-rctd.r-no:

      IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).
      {fg/fg-post.i w-fg-rctd w-fg-rctd}

      FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      FIND CURRENT fg-bin NO-LOCK NO-ERROR.

      IF fgPostLog THEN RUN fgPostLog ('End fg/fg-post.i - Start fg/fgemails.i').
      IF w-fg-rctd.rita-code = "R" THEN DO:
         {fg/fgemails.i}
      END.

      IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').

      FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

      IF AVAIL fg-rctd THEN DO:
        ASSIGN
         fg-rctd.rita-code = "P"  /* posted */
         fg-rctd.post-date = v-post-date
         fg-rctd.tag2      = w-fg-rctd.tag2.

        FOR EACH fg-rcpts
            WHERE fg-rcpts.company EQ cocode
              AND fg-rcpts.r-no    EQ fg-rctd.r-no:
          fg-rcpts.rita-code = fg-rctd.rita-code.
        END.
      END.

      IF fgPostLog THEN RUN fgPostLog ('End loop'). 
    END.  /* for each fg-rctd */

    FIND CURRENT itemfg NO-LOCK NO-ERROR.

    IF fgPostLog THEN RUN fgPostLog ('End fg/fgemails.i - Start loadtag').
    FOR EACH w-fg-rctd
        BREAK BY w-fg-rctd.i-no
              BY w-fg-rctd.job-no
              BY w-fg-rctd.job-no2
              BY w-fg-rctd.loc
              BY w-fg-rctd.loc-bin
              BY w-fg-rctd.tag:

      IF LAST-OF(w-fg-rctd.tag) THEN DO:
        IF TRIM(w-fg-rctd.tag) NE "" THEN 
        /* Ensure Bin/Tags Qty is correct.  Task 01270602 */
        
        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company EQ g_company
              AND fg-bin.i-no    EQ loadtag.i-no
              AND fg-bin.tag     EQ loadtag.tag-no
            USE-INDEX tag:
          RUN fg/calcbinq.p (ROWID(fg-bin)).
        END.

        /* IF w-fg-rctd.tag <> "" then*/
        FIND FIRST loadtag
            WHERE loadtag.company   EQ cocode
              AND loadtag.item-type EQ NO
              AND loadtag.tag-no    EQ w-fg-rctd.tag
              AND loadtag.i-no      EQ w-fg-rctd.i-no
              AND loadtag.job-no    EQ w-fg-rctd.job-no
            USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.
        IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').

        IF AVAIL loadtag THEN DO:
          FIND FIRST fg-bin
              WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ loadtag.i-no
                AND fg-bin.tag     EQ loadtag.tag-no
              /*AND fg-bin.job-no = loadtag.job-no
                AND fg-bin.job-no2 = loadtag.job-no2*/
                AND fg-bin.qty     GT 0
              USE-INDEX tag NO-LOCK NO-ERROR.
          IF w-fg-rctd.rita-code = "T" AND /*loadtag.tot-cases = w-fg-rctd.cases*/
             TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN  /* full qty transfer*/ 
            ASSIGN
             loadtag.loc          = w-fg-rctd.loc2   
             loadtag.loc-bin      = w-fg-rctd.loc-bin2
             loadtag.qty          = fg-bin.qty
             loadtag.pallet-count = fg-bin.qty
             loadtag.partial      = fg-bin.partial-count
             loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.
          ELSE /*partial transfer */
            ASSIGN
             loadtag.loc     = w-fg-rctd.loc
             loadtag.loc-bin = w-fg-rctd.loc-bin.

          FIND CURRENT loadtag NO-LOCK NO-ERROR.
        END.
      END.
    END.

    FOR EACH w-inv:
      DELETE w-inv.
    END.

    IF fgPostLog THEN RUN fgPostLog ('End First - Start Second For Each w-fg-rctd').
    FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK:

      CREATE w-inv.
      w-inv.row-id = w-fg-rctd.row-id.
    END.
    IF fgPostLog THEN RUN fgPostLog ('End Second For Each w-fg-rctd').

    IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/invrecpt.p').
    RUN fg/invrecpt.p (?, 2).
    IF fgPostLog THEN RUN fgPostLog ('End Run fg/invrecpt.p').

    IF fgPostLog THEN RUN fgPostLog ('End First - Start Third For Each w-fg-rctd').
    FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",
        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK
        BREAK BY w-fg-rctd.i-no:

      IF LAST-OF(w-fg-rctd.i-no) THEN DO:
        IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).
        RUN fg/updfgcs1.p (RECID(itemfg), NO).
        IF fgPostLog THEN RUN fgPostLog ('End Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).

        FOR EACH oe-ordl
            WHERE oe-ordl.company EQ cocode
              AND oe-ordl.opened  EQ YES
              AND oe-ordl.i-no    EQ w-fg-rctd.i-no
              AND oe-ordl.job-no  EQ ""
              AND oe-ordl.cost    EQ 0
            USE-INDEX opened NO-LOCK
            BREAK BY oe-ordl.ord-no
            TRANSACTION :

          DO i = 1 TO 1000:
            FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.
            IF AVAIL b-oe-ordl THEN DO:
              IF itemfg.prod-uom EQ "M" THEN
                b-oe-ordl.cost = itemfg.total-std-cost.
              ELSE
                RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0
                                        THEN "EA" ELSE itemfg.prod-uom),
                                       "M", 0, 0, 0, 0,
                                       itemfg.total-std-cost, OUTPUT b-oe-ordl.cost).
              LEAVE.
            END.
          END.
        END.
      END.
    END.
    IF fgPostLog THEN RUN fgPostLog ('End Third For Each w-fg-rctd').

    IF v-fgpostgl NE "None" THEN DO TRANSACTION:
      /* gdm - 11050905 */
      REPEAT:
       FIND FIRST gl-ctrl EXCLUSIVE-LOCK
         WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
       IF AVAIL gl-ctrl THEN DO:
         ASSIGN v-trnum       = gl-ctrl.trnum + 1
                gl-ctrl.trnum = v-trnum.

         FIND CURRENT gl-ctrl NO-LOCK.
         
         IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').         
         RUN gl-from-work (1, v-trnum).
         IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').
         RUN gl-from-work (2, v-trnum).
         IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').
         
         LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050905 */
    END.
    find first w-job no-error.
    if avail w-job THEN DO:
      IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').
      run jc/d-jclose.w.
      IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').
    END.

    if v-adjustgl then do TRANSACTION:
      /** GET next G/L TRANS. POSTING # **/
      find first gl-ctrl where gl-ctrl.company eq cocode exclusive-lock.
      assign
       v-trnum       = gl-ctrl.trnum + 1
       gl-ctrl.trnum = v-trnum.
      FIND CURRENT gl-ctrl NO-LOCK.
      IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').
      for each work-job break by work-job.actnum:
         create gltrans.
        assign
         gltrans.company = cocode
         gltrans.actnum  = work-job.actnum
         gltrans.jrnl    = "ADJUST"
         gltrans.tr-date = v-post-date
         gltrans.period  = period.pnum
         gltrans.trnum   = v-trnum.

        if work-job.fg then
          assign
           gltrans.tr-amt  = - work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT FG".
        else
          assign
           gltrans.tr-amt  = work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT COGS".

        RELEASE gltrans.
      end. /* each work-job */
      IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
    end.
    IF v-got-fgemail THEN DO:
      /*IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
      RUN send-fgemail (v-fgemail-file).*/
      IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
    END.
    IF fgPostLog THEN RUN fgPostLog ('End').
    IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
    
  
  END PROCEDURE.



PROCEDURE fgpostlog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.
        
    PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
        STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.
END PROCEDURE.
  




PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
  
  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:
      
    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "FGPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    end.
  end.
END PROCEDURE.
