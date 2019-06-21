

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
    
    DEFINE TEMP-TABLE ttfgpost NO-UNDO
        FIELD vfgpost AS CHAR
        FIELD vpstfg AS CHAR.
DEFINE DATASET dsfgpost FOR ttfgpost.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmfgpost        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeginSeq      AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndSeq        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeginUsrid    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndUsrid      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeDate        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeItem        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndItem       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeJob         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndJob        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeWare        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndWare       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPstDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmRecept        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmShipmnt       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmTrnsfr        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAdjstmnt      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmCrdRtn        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmItmcod        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmcostsell      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmitmcustp      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmNamPoVn       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmUomJob        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmGlActNm       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmTcost         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmGrndTotl      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnstype      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmsetup         AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsfgpost.

     IF prmUser        = ? THEN ASSIGN     prmUser        = "".   
     IF prmfgpost      = ? THEN ASSIGN     prmfgpost      = "". 
     IF prmBeginSeq    = ? THEN ASSIGN     prmBeginSeq    = 0. 
     IF prmEndSeq      = ? THEN ASSIGN     prmEndSeq      = 0. 
     IF prmBeginUsrid  = ? THEN ASSIGN     prmBeginUsrid  = "".  
     IF prmEndUsrid    = ? THEN ASSIGN     prmEndUsrid    = "".  
     IF prmBeDate      = ? THEN ASSIGN     prmBeDate      = "". 
     IF prmEndDate     = ? THEN ASSIGN     prmEndDate     = "". 
     IF prmBeItem      = ? THEN ASSIGN     prmBeItem      = "". 
     IF prmEndItem     = ? THEN ASSIGN     prmEndItem     = "". 
     IF prmBeJob       = ? THEN ASSIGN     prmBeJob       = "". 
     IF prmEndJob      = ? THEN ASSIGN     prmEndJob      = "". 
     IF prmBeWare      = ? THEN ASSIGN     prmBeWare      = "". 
     IF prmEndWare     = ? THEN ASSIGN     prmEndWare     = "".
     IF prmPstDate     = ? THEN ASSIGN     prmPstDate     = "". 
     IF prmRecept      = ? THEN ASSIGN     prmRecept      = "". 
     IF prmShipmnt     = ? THEN ASSIGN     prmShipmnt     = "". 
     IF prmTrnsfr      = ? THEN ASSIGN     prmTrnsfr      = "". 
     IF prmAdjstmnt    = ? THEN ASSIGN     prmAdjstmnt    = "". 
     IF prmCrdRtn      = ? THEN ASSIGN     prmCrdRtn      = "". 
     IF prmItmcod      = ? THEN ASSIGN     prmItmcod      = "". 
     IF prmcostsell    = ? THEN ASSIGN     prmcostsell    = "". 
     IF prmitmcustp    = ? THEN ASSIGN     prmitmcustp    = "". 
     IF prmNamPoVn     = ? THEN ASSIGN     prmNamPoVn     = "". 
     IF prmUomJob      = ? THEN ASSIGN     prmUomJob      = "".  
     IF prmGlActNm     = ? THEN ASSIGN     prmGlActNm     = "".  
     IF prmTcost       = ? THEN ASSIGN     prmTcost       = "". 
     IF prmGrndTotl    = ? THEN ASSIGN     prmGrndTotl    = "". 
     IF prmtrnstype    = ? THEN ASSIGN     prmtrnstype    = "".
     IF prmsetup       = ? THEN ASSIGN     prmsetup       = "".
     



DEFINE VARIABLE begin_fg-r-no AS INTEGER FORMAT ">>>>>>>>":U NO-UNDO.
DEFINE VARIABLE begin_i-no    AS CHARACTER FORMAT "X(15)":U  NO-UNDO.              
DEFINE VARIABLE begin_job-no  AS CHARACTER FORMAT "X(6)":U  NO-UNDO.             
DEFINE VARIABLE begin_userid  AS CHARACTER FORMAT "X(8)":U  NO-UNDO.             
DEFINE VARIABLE begin_whs     AS CHARACTER FORMAT "X(5)":U NO-UNDO.                 
DEFINE VARIABLE end_fg-r-no   AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE end_i-no      AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_job-no    AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" NO-UNDO.        
DEFINE VARIABLE end_userid    AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.      
DEFINE VARIABLE end_whs       AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" NO-UNDO.            
DEFINE VARIABLE ldt-from      AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.                          
DEFINE VARIABLE ldt-to        AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.                            
DEFINE VARIABLE v-post-date   AS DATE FORMAT "99/99/9999":U NO-UNDO.                       
DEFINE VARIABLE v-trans-lbl   AS CHARACTER FORMAT "X(256)":U NO-UNDO.                      
DEFINE VARIABLE lv-ornt       AS CHARACTER  NO-UNDO.                                           
DEFINE VARIABLE rd-dest       AS INTEGER NO-UNDO.
DEFINE VARIABLE rd-Itm#Cst#   AS INTEGER NO-UNDO.
DEFINE VARIABLE rd-ItmPo      AS INTEGER NO-UNDO.
DEFINE VARIABLE rd-UOMJob     AS INTEGER NO-UNDO.
DEFINE VARIABLE rd_print      AS CHARACTER NO-UNDO.
DEFINE VARIABLE t-adj         AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE t-receipt     AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE t-ret         AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE t-ship        AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE t-trans       AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_excel      AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_glnum      AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_grndtotal  AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_runExcel   AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_totCstVal  AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE td-show-parm  AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tgl-itemCD    AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"  NO-UNDO.

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


DEF NEW SHARED VAR choice AS LOG NO-UNDO.

DEF VAR v-fgpostgl AS CHAR NO-UNDO.
def var v-fg-value as dec format "->,>>>,>>9.99".
def var v-msf as dec format ">,>>9.999" extent 6.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR lv-list-name LIKE list-name EXTENT 2 NO-UNDO.
DEF VAR ip-rowid AS ROWID NO-UNDO.


DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD has-rec  AS LOG INIT NO
                                    FIELD invoiced AS LOG INIT NO
                                    FIELD old-tag AS CHAR
                                    FIELD ret-loc AS CHAR
                                    FIELD ret-loc-bin AS CHAR.

DEF TEMP-TABLE tt-email NO-UNDO FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .

{fg/fullset.i NEW}
{jc/jcgl-sh.i NEW}
{fg/fg-post3.i NEW}
{fg/invrecpt.i NEW}

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DEF STREAM logFile.
DEF STREAM before.
DEF STREAM after.

DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

DEFINE BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b2-fg-rctd FOR fg-rctd.

{sys/inc/ssfgretc.i}
{sys/inc/fgpost.i} 

{sys/inc/closejob.i FGPost}
{sys/inc/fgpostgl.i}   
{sys/inc/adjustgl.i}
{sys/inc/fgemails.i}
{sys/inc/postdate.i}


    ASSIGN
    v-fgpostgl  = fgpostgl
    tb_glnum    = v-fgpostgl NE "None" OR v-adjustgl
    tgl-itemCD  = YES   .


DEF STREAM excel.

  IF prmfgpost = "FinishGPost" THEN DO:
     
        ASSIGN
        v-today        = TODAY
        begin_fg-r-no  = prmBeginSeq
        begin_i-no     = prmBeItem
        begin_job-no   = prmBeJob
        begin_userid   = prmBeginUsrid
        begin_whs      = prmBeWare
        end_fg-r-no    = prmEndSeq
        end_i-no       = prmEndItem
        end_job-no     = prmEndJob
        end_userid     = prmEndUsrid
        end_whs        = prmEndWare
        ldt-from       = datetime(prmBeDate) 
        ldt-to         = datetime(prmEndDate)
        v-post-date    = datetime(prmPstDate)
        v-trans-lbl    = prmtrnstype
        rd-Itm#Cst#    = int(prmitmcustp)
        rd-ItmPo       = int(prmNamPoVn)
        rd-UOMJob      = int(prmUomJob)
        rd_print       = prmcostsell  
        t-adj          = IF prmAdjstmnt = "Yes" THEN TRUE ELSE FALSE
        t-receipt      = IF prmRecept = "Yes" THEN TRUE ELSE FALSE
        t-ret          = IF prmCrdRtn = "Yes" THEN TRUE ELSE FALSE
        t-ship         = IF prmShipmnt = "Yes" THEN TRUE ELSE FALSE
        t-trans        = IF prmTrnsfr = "Yes" THEN TRUE ELSE FALSE
        tb_glnum       = IF prmGlActNm = "Yes" THEN TRUE ELSE FALSE
        tb_grndtotal   = IF prmGrndTotl = "Yes" THEN TRUE ELSE FALSE
        tb_totCstVal   = IF prmTcost = "Yes" THEN TRUE ELSE FALSE
        tgl-itemCD     = IF prmItmcod = "Yes" THEN TRUE ELSE FALSE
        /*t-setup        = IF prmsetup = "SETUP" THEN TRUE ELSE FALSE*/ .
    
        ASSIGN 
            tb_excel = IF prmOut = "yes" THEN TRUE ELSE FALSE.

    assign
      init-dir    =  v-webrootpath
        fi_file = init-dir + "fgcost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile =  "fgcost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        
        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "fgcost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .
        vTextFile2 =  "afterfgcost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .
            


  FOR EACH w-fg-rctd:
    DELETE w-fg-rctd.
  END.

  FOR EACH work-gl:
    DELETE work-gl.
  END.

  FOR EACH work-job:
    DELETE work-job.
  END.

  run run-report.

  
    CREATE ttfgpost.
    IF tb_excel THEN
        ASSIGN ttfgpost.vfgpost = vPdfFile .
    IF NOT tb_excel THEN
         ASSIGN ttfgpost.vfgpost = vTextFile .


  choice = CAN-FIND(FIRST w-fg-rctd WHERE w-fg-rctd.has-rec).

  IF choice THEN
  FOR EACH w-fg-rctd
      WHERE w-fg-rctd.has-rec
        AND NOT CAN-FIND(FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id):
    choice = NO.
    LEAVE.
  END.

  IF choice THEN DO:
    choice = IF prmsetup = "Yes"  THEN TRUE ELSE FALSE. 
    /*MESSAGE "Are you ready to post to finished goods?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice.*/
  END.

  ELSE do: cError =  "Sorry, nothing is available for posting..." .
       RETURN .
  END.

  IF choice THEN DO:
    FOR EACH w-fg-rctd
        WHERE w-fg-rctd.has-rec
          AND CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ w-fg-rctd.r-no),
        FIRST fg-rctd
        WHERE ROWID(fg-rctd)    EQ w-fg-rctd.row-id
          AND fg-rctd.rita-code NE "P":
      lv-r-no = fg-rctd.r-no.
      DO TRANSACTION:
        fg-rctd.r-no = 0.
      END.
      DO TRANSACTION:
        fg-rctd.r-no = lv-r-no.
      END.
      w-fg-rctd.r-no = fg-rctd.r-no.
    END.

    FOR EACH w-fg-rctd WHERE w-fg-rctd.has-rec,
        FIRST fg-rctd NO-LOCK WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id,
        FIRST fg-rcpth NO-LOCK WHERE fg-rcpth.r-no EQ fg-rctd.r-no:

      cError =  "Sorry, these FG Transactions cannot be processed because 1 or " +
              "more have already been posted by UserID: " +
              TRIM(fg-rcpth.user-id) + "..." .
        RETURN.

    END.
  END.

  IF choice THEN DO: 
    RUN fg-post. 

    cError = "Posting completed..." .
  END.


  END.
/*****************************************************************************************/

  PROCEDURE run-report PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 112}.
    
{sys/form/r-top3w1.f "Before"}
    
{sys/form/r-top3w1.f "After"}

DEF VAR ext-cost AS DEC NO-UNDO.
def var type as ch format "X" initial "R".
def var type-prt as ch format "X(11)" init "".
def var v-fg-qty like fg-rctd.t-qty.
def var v-fg-cost as dec format "->,>>>,>>9.99<<".
def var v-tot-qty as int format "->>>,>>>,>>9".
def var v-tot-cost as dec format "->>>,>>9.99<<".
def var v-grd-tot-qty as int format "->>>,>>>,>>9".
def var v-grd-tot-cost as dec format "->>,>>>,>>9.99<<".                     
def var v-grd-tot-value as dec format "->>,>>>,>>9.99<<".                     
def var v-tot-value as dec format "->>,>>>,>>9.99".
def var v-cum-tot as de.                                   
def var v-tran-type as char format "x(1)".      
def var v-entrytype as char initial "REC ,TRAN,ADJ ,SHIP,RET ,INIT".
def var v-on like eb.num-up.
def var v-qty-pallet as decimal format "->>,>>>,>>9" no-undo.
def var v-whse like fg-rctd.loc.            
def var v-one as integer format "->>,>>9" init 1.
def var v-ftime as logical init no.
def var v-dscr          like account.dscr.
def var v-disp-actnum   like account.actnum.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr".
def var v-hdr as char format "x(12)".
def var v-postlst  as cha no-undo.
DEF VAR ll-wip AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li-loop AS INT NO-UNDO.
DEF VAR v-time AS CHAR FORMAT "X(5)" NO-UNDO.

DEF VAR v-itm-lbl  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-itm-dsh  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-desc-lbl AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-Po-lbl   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-vend-lbl AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-desc-dsh AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-Po-dsh   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-vend-dsh AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-uom-lbl  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-uom-dsh  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-cstprt   AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-pr-tots AS LOG FORMAT "Y/N" NO-UNDO.
DEF VAR v-pr-tots2 LIKE v-pr-tots         NO-UNDO.
DEF var v-cost-sell as log format "Cost/Sell Value".


/*if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
MESSAGE "list-name  " list-name init-dir .*/
/* {sys/inc/outprint.i /*value(lines-per-page)*/}*/


IF rd-Itm#Cst# EQ 1 
  THEN ASSIGN v-itm-lbl = "ITEM"
              v-itm-dsh = "---------------".
  ELSE ASSIGN v-itm-lbl = "CUSTOMER PART #"
              v-itm-dsh = "---------------".

IF rd-ItmPo EQ 1   
  THEN ASSIGN v-desc-lbl = "DESCRIPTION                           "
              v-Po-lbl   = ""
              v-vend-lbl = ""
              v-desc-dsh = "------------------------------".
                            
  ELSE ASSIGN v-desc-lbl = "DESCRIPTION"
              v-Po-lbl   = "P.O. #"
              v-vend-lbl = "VEND"
              v-desc-dsh = "-------------- --------- --------".
              
IF rd-UOMJob EQ 1 
  THEN ASSIGN v-uom-lbl = "UOM"
              v-uom-dsh = "----".
  ELSE ASSIGN v-uom-lbl = "JOB #"
              v-uom-dsh = "----------".

FORM HEADER
     SPACE(56) "PRE POST AUDIT TRAIL"
    WITH FRAME before STREAM-IO WIDTH 132 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

FORM HEADER
     SPACE(57) "POSTED AUDIT TRAIL"
    WITH FRAME after STREAM-IO WIDTH 132 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

FORM HEADER
     "WHSE:"
     v-whse
     SKIP    
     "         TOTAL"   at 128    
     "DATE"             at 1
     "TIME"             AT 10
     TRIM(v-itm-lbl)  FORMAT "x(15)"  at 16
     TRIM(v-desc-lbl) FORMAT "x(11)"  at 32
     TRIM(v-Po-lbl)     at 47
     TRIM(v-vend-lbl)  FORMAT "X(4)" at 57
     "T"                at 63
     "TAG #"            at 65
     "UNITS"            at 88  
     "COUNT"            at 97
     "TOTAL"            at 106
     "BIN"              at 112    
     TRIM(v-uom-lbl) FORMAT "x(10)" at 119
     v-hdr                  at 130
     "--------"             at 1                /*date*/
     "----"                 AT 10               /* time */                
     TRIM(v-itm-dsh)  FORMAT "x(15)" at 16       /*item*/
     TRIM(v-desc-dsh) FORMAT "x(30)" at 32      /*description p.o. # vendor*/
     "-"                    at 63               /*t*/
     "--------------------" at 65               /*tag # 8 -> 20*/
     "-------"              at 86               /*units*/
     "--------"             at 94               /*count*/
     "--------"             at 103              /*total 11->8*/
     "------"               at 112              /*bin  8 -> 6*/    
     TRIM(v-uom-dsh) FORMAT "x(10)" at 119              /*uom*/
     "------------"         at 130              /*total value 14 -> 12*/
    with frame r-top1 STREAM-IO width 170 no-labels no-box no-underline page-top.

/*form #1 Print cases / qty case for TOTAL COST*/
form w-fg-rctd.rct-date             format "99/99/99" 
     v-time                                           
     w-fg-rctd.i-no                 format "x(15)"    
     w-fg-rctd.i-name               format "x(14)"    
     w-fg-rctd.po-no                                  
     po-ord.vend-no                 FORMAT "x(5)"                                  
     v-tran-type                                      
     w-fg-rctd.tag                  FORM "x(20)"      
     w-fg-rctd.cases                format "->>,>>9"  
     w-fg-rctd.qty-case             format "->>>,>>9" 
     v-fg-qty                       format "->>>,>>9" 
     w-fg-rctd.loc-bin              FORM "x(6)"       
     w-fg-rctd.pur-uom              FORMAT "x(9)"     
     v-fg-cost
    with frame itemx no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99" AT 1  
     v-time                                           AT 10 
     w-fg-rctd.i-no                 format "x(15)"    AT 16 
     w-fg-rctd.i-name               format "x(27)"    AT 32 
     v-tran-type                                      AT 63 
     w-fg-rctd.tag                  FORM "x(20)"      AT 65 
     w-fg-rctd.cases                format "->>,>>9"  AT 86 
     w-fg-rctd.qty-case             format "->>>,>>9" AT 94 
     v-fg-qty                       format "->>>,>>9" AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"       AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"     AT 119
     v-fg-cost 
    with frame itemxA no-box down STREAM-IO width 170 no-labels.

/*form #2 Print 1 / partial for TOTAL COST*/
form w-fg-rctd.rct-date             format "99/99/99"
     v-time                                          
     w-fg-rctd.i-no                 format "x(15)"   
     w-fg-rctd.i-name               format "x(14)"   
     w-fg-rctd.po-no                                 
     po-ord.vend-no                 FORMAT "x(5)"                   
     v-tran-type                                     
     w-fg-rctd.tag                FORM "x(20)"       
     v-one                          format "->>,>>9" 
     w-fg-rctd.partial              format "->>>,>>9"
     v-fg-qty                       format "->>>,>>9"
     w-fg-rctd.loc-bin              FORM "x(6)"      
     w-fg-rctd.pur-uom              FORMAT "x(9)"    
     v-fg-cost  
    with frame itempx no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99"   AT 1  
     v-time                                             AT 10 
     w-fg-rctd.i-no                 format "x(15)"      AT 16 
     w-fg-rctd.i-name               format "x(27)"      AT 32 
     v-tran-type                                        AT 63 
     w-fg-rctd.tag                  FORM "x(20)"        AT 65 
     v-one                          format "->>,>>9"    AT 86 
     w-fg-rctd.partial              format "->>>,>>9"   AT 94 
     v-fg-qty                       format "->>>,>>9"   AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"         AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"       AT 119
     v-fg-cost                                       
    with frame itempxA no-box down STREAM-IO width 170 no-labels.

/*form #3 Print cases / qty case for TOTAL VALUE*/
form w-fg-rctd.rct-date             format "99/99/99"   
     v-time                                             
     w-fg-rctd.i-no                 format "x(15)"      
     w-fg-rctd.i-name               format "x(14)"      
     w-fg-rctd.po-no                                    
     po-ord.vend-no                 FORMAT "x(5)"                                   
     v-tran-type                                        
     w-fg-rctd.tag                  FORM "x(20)"        
     w-fg-rctd.cases                format "->>,>>9"    
     w-fg-rctd.qty-case             format "->>>,>>9"   
     v-fg-qty                       format "->>>,>>9"   
     w-fg-rctd.loc-bin              FORM "x(6)"         
     w-fg-rctd.pur-uom              FORMAT "x(9)"       
     v-fg-value 
    with frame itemy no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99"  AT 1  
     v-time                                            AT 10 
     w-fg-rctd.i-no                 format "x(15)"     AT 16 
     w-fg-rctd.i-name               format "x(27)"     AT 32 
     v-tran-type                                       AT 63 
     w-fg-rctd.tag                  FORM "x(20)"       AT 65 
     w-fg-rctd.cases                format "->>,>>9"   AT 86 
     w-fg-rctd.qty-case             format "->>>,>>9"  AT 94 
     v-fg-qty                       format "->>>,>>9"  AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"        AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"      AT 119
     v-fg-value                                      
    with frame itemyA no-box down STREAM-IO width 170 no-labels.

/*form #4 Print 1 / partial for TOTAL VALUE*/
form w-fg-rctd.rct-date             format "99/99/99"    
     v-time                                              
     w-fg-rctd.i-no                 format "x(15)"       
     w-fg-rctd.i-name               format "x(14)"       
     w-fg-rctd.po-no                                     
     po-ord.vend-no                 FORMAT "x(5)"                                  
     v-tran-type                                         
     w-fg-rctd.tag                                       
     v-one                          format "->>,>>9"     
     w-fg-rctd.partial              format "->>>,>>9"    
     v-fg-qty                       format "->>,>>>,>>9" 
     w-fg-rctd.loc-bin              FORM "x(6)"          
     w-fg-rctd.pur-uom              FORMAT "x(9)"        
     v-fg-value 
    with frame itempy no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99"    AT 1  
     v-time                                              AT 10 
     w-fg-rctd.i-no                 format "x(15)"       AT 16 
     w-fg-rctd.i-name               format "x(27)"       AT 32 
     v-tran-type                                         AT 63 
     w-fg-rctd.tag                                       AT 65 
     v-one                          format "->>,>>9"     AT 86 
     w-fg-rctd.partial              format "->>>,>>9"    AT 94 
     v-fg-qty                       format "->>,>>>,>>9" AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"          AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"        AT 119
     v-fg-value                                       
    with frame itempyA no-box down STREAM-IO width 170 no-labels.


form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     udate         label "DATE"   
     v-disp-amt    label "AMOUNT" SKIP
    with down STREAM-IO width 130 frame gldetail.    

{ce/msfcalc.i}



IF length(begin_job-no) < 6 THEN
   begin_job-no = FILL(" ",6 - LENGTH(trim(begin_job-no))) + TRIM(begin_job-no).
IF length(end_job-no) < 6 THEN
   end_job-no = FILL(" ",6 - LENGTH(trim(end_job-no))) + TRIM(end_job-no).



ASSIGN
 str-tit2 = "Finished Goods Posting"
 {sys/inc/ctrtext.i str-tit2 112}
 str-tit3 = "Period Date: " + string(v-post-date,"99/99/9999") + "             Posted by: " + USERID('nosweat') + "  As of " + string(TODAY,"99/99/9999")
 {sys/inc/ctrtext.i str-tit3  132}

 v-postlst   = (IF t-receipt THEN "R," ELSE "") +
               (IF t-setup THEN "I," ELSE "") +
               (IF t-ship THEN "S," ELSE "") +
               (IF t-trans THEN "T," ELSE "") +
               (IF t-adj THEN "A," ELSE "") +
               (IF t-ret THEN "E," ELSE "")
 v-cost-sell = rd_print EQ "C"
 v-pr-tots2  = tb_totCstVal
 v-pr-tots   = tb_grndtotal.



IF LENGTH(v-postlst) GT 0 AND
   SUBSTR(v-postlst,LENGTH(v-postlst),1) EQ "," THEN
   SUBSTR(v-postlst,LENGTH(v-postlst),1) = "".


/*DO li = 1 TO 2:*/
  {sys/inc/print1.i}
  if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       list-name2 = tmp-dir + vTextFile2
       init-dir = tmp-dir.     
 ASSIGN lv-list-name[1] = list-name
        lv-list-name[2] = list-name2 .
  /*PAUSE 1 NO-MESSAGE.
END.*/

OUTPUT STREAM before TO VALUE(lv-list-name[1]) /*PAGE-SIZE VALUE(lines-per-page)*/ .
OUTPUT STREAM after  TO VALUE(lv-list-name[2]) /*PAGE-SIZE VALUE(lines-per-page)*/ .

FIND fg-rctd WHERE ROWID(fg-rctd) EQ ip-rowid NO-ERROR.

IF AVAIL fg-rctd THEN RUN build-tables.

ELSE
DO li-loop = 1 TO NUM-ENTRIES(v-postlst):
  FOR EACH fg-rctd
      WHERE fg-rctd.company   EQ cocode
        AND fg-rctd.rita-code EQ ENTRY(li-loop,v-postlst)
        AND fg-rctd.r-no      GE begin_fg-r-no
        AND fg-rctd.r-no      LE end_fg-r-no
        AND fg-rctd.i-no      GE begin_i-no
        AND fg-rctd.i-no      LE end_i-no
        AND fg-rctd.rct-date  GE ldt-from
        AND fg-rctd.rct-date  LE ldt-to
        AND fg-rctd.job-no    GE begin_job-no
        AND fg-rctd.job-no    LE end_job-no
        AND fg-rctd.loc-bin   NE ""
        AND fg-rctd.loc       GE begin_whs
        AND fg-rctd.loc       LE end_whs
        AND ((begin_userid    LE "" AND
              end_userid      GE "" AND
              NOT CAN-FIND(FIRST reftable
                           WHERE reftable.reftable EQ "fg-rctd.user-id"
                             AND reftable.company  EQ fg-rctd.company
                             AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999"))) OR
             CAN-FIND(FIRST reftable
                      WHERE reftable.reftable EQ "fg-rctd.user-id"
                        AND reftable.company  EQ fg-rctd.company
                        AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
                        AND reftable.code     GE begin_userid
                        AND reftable.code     LE end_userid))
      USE-INDEX rita-code:

    RUN build-tables.
  END.
END.

if v-cost-sell then do:
  v-hdr = "        COST".

  IF tb_excel THEN 
  DO:
      
    OUTPUT STREAM excel TO VALUE(fi_file).

    IF rd-ItmPo EQ 1
      THEN ASSIGN excelheader = "Date,Time,Item,Description,".
      ELSE ASSIGN excelheader = "Date,Time,Item,Description,Po No,Vendor,".

    ASSIGN excelheader = excelheader + 
                         "T,Tag No,Units,Count,Total,Bin,".
    IF rd-UOMJob EQ 1 
      THEN ASSIGN excelheader = excelheader + "UOM,Total Cost".
      ELSE ASSIGN excelheader = excelheader + "Job #,Total Cost".

    PUT STREAM excel UNFORMATTED excelheader SKIP.

  END.

   IF rd-ItmPo EQ 1 THEN DO:
     {fg/rep/fg-post.i "itemxA" "v-fg-cost" "itempxA" "v-tot-cost"}
   END.
   ELSE DO:
     {fg/rep/fg-post.i "itemx" "v-fg-cost" "itempx" "v-tot-cost"}
   END.
end.
else do:
  v-hdr = "       VALUE".

  IF tb_excel THEN 
  DO:
      
    OUTPUT STREAM excel TO VALUE(fi_file).


    IF rd-ItmPo EQ 1
      THEN ASSIGN excelheader = "Date,Time,Item,Description,".
      ELSE ASSIGN excelheader = "Date,Time,Item,Description,Po No,Vendor,".

    ASSIGN excelheader = excelheader + 
                         "T,Tag No,Units,Count,Total,Bin,".
          
    IF rd-UOMJob EQ 1 
      THEN ASSIGN excelheader = excelheader + "UOM,Total Value".
      ELSE ASSIGN excelheader = excelheader + "Job #,Total Value".

    PUT STREAM excel UNFORMATTED excelheader SKIP.
  END.

  IF rd-ItmPo EQ 1 THEN DO:
   {fg/rep/fg-post.i "itemyA" "v-fg-value" "itempyA" "v-tot-value"}
  END.
  ELSE DO:
   {fg/rep/fg-post.i "itemy" "v-fg-value" "itempy" "v-tot-value"}
  END.
end.
  
if v-pr-tots then do:
  if v-cost-sell then DO:                   
    PUT STREAM before
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],">>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],">>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],">>,>>9.9<<"))
                    format "x(59)" at 15
        "GRAND TOTALS:" to 97
        v-grd-tot-qty to 110 v-grd-tot-cost to 141 skip. 

    PUT STREAM after
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],">>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],">>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],">>,>>9.9<<"))
                    format "x(59)" at 15 
        "GRAND TOTALS:" to 97
        v-grd-tot-qty to 110 v-grd-tot-cost to 141 skip.     
  END.
  ELSE DO:
    PUT STREAM before
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],">>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],">>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],">>,>>9.9<<"))
                    format "x(59)" at 15 
        "GRAND TOTALS:" to 100
        v-grd-tot-qty to 113 v-grd-tot-value to 144 skip.

    PUT STREAM after
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],">>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],">>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],">>,>>9.9<<"))
                    format "x(59)" at 15 
        "GRAND TOTALS:" to 97
        v-grd-tot-qty to 110 v-grd-tot-value to 141 skip.
  END.
end. /* if v-pr-tots */

HIDE FRAME r-top1.
  
if tb_glnum THEN DO:
  PAGE STREAM before.
  PAGE STREAM after.

   
  for each work-gl break by work-gl.actnum:
  
    find first account
        where account.company eq cocode
          and account.actnum  eq work-gl.actnum
        no-lock no-error.
        
    assign
     v-dscr        = if avail account then account.dscr
                     else "ACCOUNT NOT FOUND - " + work-gl.actnum
     v-disp-actnum = work-gl.actnum
     v-disp-amt    = work-gl.debits - work-gl.credits.

    display STREAM before
            v-disp-actnum v-dscr udate v-disp-amt
          with frame gldetail.
    down STREAM before with frame gldetail.

    display STREAM after
            v-disp-actnum v-dscr udate v-disp-amt
          with frame gldetail.
    down STREAM after with frame gldetail.
  end. /* each work-job */
  
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

    display STREAM before
            v-disp-actnum v-dscr udate v-disp-amt
          with frame gldetail.
    down STREAM before with frame gldetail.

    display STREAM after
            v-disp-actnum v-dscr udate v-disp-amt
          with frame gldetail.
    down STREAM after with frame gldetail.
  end. /* each work-job */
END.

OUTPUT STREAM before CLOSE.
OUTPUT STREAM after  CLOSE.

IF tb_excel THEN 
  DO:
      OUTPUT STREAM excel CLOSE.
      /*IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).*/
END.



END PROCEDURE.

PROCEDURE build-tables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
DEF VAR li-max-qty AS INT NO-UNDO.
def var v-part-qty as dec no-undo.
def var v-set-qty as dec no-undo.
DEF VAR v-cost AS DEC NO-UNDO.

DEF BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b-itemfg FOR itemfg.
/*DEF BUFFER use-job FOR reftable.*/

FIND FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ fg-rctd.i-no
    NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO TRANSACTION:
  li-max-qty = fg-rctd.t-qty.

  IF li-max-qty GE fg-rctd.t-qty THEN DO:
    CREATE w-fg-rctd.
    BUFFER-COPY fg-rctd TO w-fg-rctd
    ASSIGN
     w-fg-rctd.row-id  = ROWID(fg-rctd)
     w-fg-rctd.has-rec = YES.

    IF prmsetup EQ "SETUP" THEN
       ASSIGN
       w-fg-rctd.old-tag = fg-rctd.tag
       w-fg-rctd.ret-loc = fg-rctd.loc
       w-fg-rctd.ret-loc-bin = fg-rctd.loc-bin.
  END.
END.

END PROCEDURE.


PROCEDURE fg-post :
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
        AND period.pend    GE v-post-date.

  find first sys-ctrl  where sys-ctrl.company eq cocode
                         and sys-ctrl.name    eq "AUTOPOST"
       no-lock no-error.
  v-autobin = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

  FOR EACH w-fg-rctd
      BY w-fg-rctd.tag
      BY w-fg-rctd.rct-date
      BY w-fg-rctd.trans-time
      BY w-fg-rctd.r-no:

    IF NOT CAN-FIND(FIRST itemfg WHERE
       itemfg.company EQ cocode AND
       itemfg.i-no    EQ w-fg-rctd.i-no) THEN
       NEXT.

    loop1:
    REPEAT:
   
       FIND FIRST itemfg WHERE
            itemfg.company EQ cocode AND
            itemfg.i-no    EQ w-fg-rctd.i-no
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
       
       IF AVAIL itemfg THEN
       DO:
          IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).
          {fg/fg-post.i w-fg-rctd w-fg-rctd}

          FIND CURRENT po-ordl NO-LOCK NO-ERROR.
          FIND CURRENT fg-bin NO-LOCK NO-ERROR.

          FIND CURRENT itemfg NO-LOCK NO-ERROR.
          LEAVE loop1.
       END.
    END.
    
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
       fg-rctd.trans-time = TIME
       fg-rctd.tag2      = w-fg-rctd.tag2.

      FOR EACH fg-rcpts
          WHERE fg-rcpts.company EQ fg-rctd.company
            AND fg-rcpts.r-no    EQ fg-rctd.r-no:
        fg-rcpts.rita-code = fg-rctd.rita-code.
      END.
    END.

    IF fgPostLog THEN RUN fgPostLog ('End loop'). 
  END.  /* for each w-fg-rctd */

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
            AND fg-bin.i-no    EQ w-fg-rctd.i-no
            AND fg-bin.tag     EQ w-fg-rctd.tag
          USE-INDEX tag:
        RUN fg/calcbinq.p (ROWID(fg-bin)).
      END.

      /* IF w-fg-rctd.tag <> "" then*/
      FIND FIRST loadtag
          WHERE loadtag.company   EQ g_company
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ w-fg-rctd.tag
            AND loadtag.i-no      EQ w-fg-rctd.i-no
            AND loadtag.job-no    EQ w-fg-rctd.job-no
          USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.
      IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').

      IF AVAIL loadtag THEN DO:
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ g_company
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
      END.
    END.

    IF prmsetup EQ "SETUP" AND ssfgretc-log AND
       ( (w-fg-rctd.rita-code EQ "T" AND w-fg-rctd.inv-no NE 0) OR
          w-fg-rctd.rita-code EQ "I" ) THEN
       RUN create-phy-count-proc.
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
          TRANSACTION:
           
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
    /* gdm - 11050906 */
    loop2:
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
       LEAVE loop2.
     END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.
  find first w-job no-error.
  if avail w-job THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').
    /*run jc/d-jclose.w.*/
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
    end. /* each work-job */
    IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
  end.
  /*IF v-got-fgemail THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
    RUN send-fgemail (v-fgemail-file).
    IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
  END.*/
  IF fgPostLog THEN RUN fgPostLog ('End').
  IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
 
END PROCEDURE.

PROCEDURE create-phy-count-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rno AS INT NO-UNDO.

   DEF BUFFER b-fg-bin FOR fg-bin.

   CREATE b2-fg-rctd.

   FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
   IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN
      lv-rno = b-fg-rctd.r-no.

   FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
   IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN
      lv-rno = fg-rcpth.r-no.

   DO WHILE TRUE:
      lv-rno = lv-rno + 1.
      IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no) OR
         CAN-FIND(FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd) THEN
         NEXT.
      LEAVE.
   END.

   /*task 06101005*/
   IF w-fg-rctd.rita-code EQ "I" THEN
      ASSIGN
         w-fg-rctd.job-no = ""
         w-fg-rctd.job-no2 = 0
         w-fg-rctd.cost = 0
         w-fg-rctd.std-cost = 0
         w-fg-rctd.ext-cost = 0.

   assign
      b2-fg-rctd.company = cocode
      b2-fg-rctd.r-no    = lv-rno
      b2-fg-rctd.rita-code = "C"
      b2-fg-rctd.s-num = 0
      b2-fg-rctd.rct-date = today
      b2-fg-rctd.trans-time = TIME 
      b2-fg-rctd.tag = w-fg-rctd.old-tag
      b2-fg-rctd.loc = w-fg-rctd.ret-loc
      b2-fg-rctd.loc-bin = w-fg-rctd.ret-loc-bin
      b2-fg-rctd.i-no = w-fg-rctd.i-no
      b2-fg-rctd.i-name = w-fg-rctd.i-name
      b2-fg-rctd.job-no = w-fg-rctd.job-no
      b2-fg-rctd.job-no2 = w-fg-rctd.job-no2
      b2-fg-rctd.t-qty = w-fg-rctd.inv-no
      b2-fg-rctd.cases = w-fg-rctd.cases
      b2-fg-rctd.cases-unit = w-fg-rctd.cases-unit
      b2-fg-rctd.qty-case = w-fg-rctd.qty-case
      b2-fg-rctd.std-cost = w-fg-rctd.std-cost
      b2-fg-rctd.cost     = w-fg-rctd.cost
      b2-fg-rctd.cost-uom = w-fg-rctd.cost-uom
      b2-fg-rctd.ext-cost = w-fg-rctd.ext-cost
      b2-fg-rctd.cust-no  = w-fg-rctd.cust-no.

   IF b2-fg-rctd.t-qty NE w-fg-rctd.t-qty AND
      b2-fg-rctd.qty-case NE 0 THEN
      ASSIGN
         b2-fg-rctd.cases = TRUNC(b2-fg-rctd.t-qty / b2-fg-rctd.qty-case,0)
         b2-fg-rctd.partial = b2-fg-rctd.t-qty - (b2-fg-rctd.cases * b2-fg-rctd.qty-case).

   FIND FIRST b-fg-bin 
      WHERE b-fg-bin.company EQ b2-fg-rctd.company
        AND b-fg-bin.i-no    EQ b2-fg-rctd.i-no
        AND b-fg-bin.job-no  EQ b2-fg-rctd.job-no
        AND b-fg-bin.job-no2 EQ b2-fg-rctd.job-no2
        AND b-fg-bin.loc     EQ b2-fg-rctd.loc
        AND b-fg-bin.loc-bin EQ b2-fg-rctd.loc-bin
        AND b-fg-bin.tag     EQ b2-fg-rctd.tag
        AND b-fg-bin.cust-no EQ b2-fg-rctd.cust-no
      NO-LOCK NO-ERROR.

  IF AVAIL b-fg-bin THEN
     ASSIGN
        b2-fg-rctd.ext-cost = b2-fg-rctd.t-qty /
                           (IF b-fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) *
                           b-fg-bin.std-tot-cost
        b2-fg-rctd.cost     = b2-fg-rctd.ext-cost / b2-fg-rctd.t-qty
        b2-fg-rctd.cost-uom = b-fg-bin.pur-uom.
  
  IF b2-fg-rctd.ext-cost EQ ? THEN b2-fg-rctd.ext-cost = 0.
  IF b2-fg-rctd.cost     EQ ? THEN b2-fg-rctd.cost = 0.

   FIND FIRST loadtag WHERE
        loadtag.company = g_company AND
        loadtag.item-type = NO AND
        loadtag.tag-no = b2-fg-rctd.tag
        NO-LOCK NO-ERROR.

   IF AVAIL loadtag AND
      CAN-FIND(FIRST fg-bin WHERE
      fg-bin.company EQ cocode AND
      fg-bin.i-no    EQ b2-fg-rctd.i-no AND
      fg-bin.tag     EQ b2-fg-rctd.tag AND
      fg-bin.job-no  EQ b2-fg-rctd.job-no AND
      fg-bin.job-no2 EQ b2-fg-rctd.job-no2 AND
      (fg-bin.loc    NE b2-fg-rctd.loc OR
       fg-bin.loc-bin NE b2-fg-rctd.loc-bin)
       USE-INDEX tag) AND
       (loadtag.loc <> b2-fg-rctd.loc OR 
        loadtag.loc-bin <> b2-fg-rctd.loc-bin) THEN 
        RUN crt-transfer.

   RELEASE b2-fg-rctd.
END PROCEDURE.


PROCEDURE crt-transfer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rno AS INT NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF VAR lv-rctd-rowid AS ROWID NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no) OR
       CAN-FIND(FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd) THEN
       NEXT.
    LEAVE.
  END.

  /*FOR EACH b-fg-rctd WHERE
      recid(b-fg-rctd) <> RECID(b2-fg-rctd) AND
      b-fg-rctd.i-no = b2-fg-rctd.i-no AND
      b-fg-rctd.tag = b2-fg-rctd.tag:
      DELETE b-fg-rctd.
  END.*/

  FOR EACH fg-bin WHERE
      fg-bin.company EQ cocode AND
      fg-bin.i-no    EQ b2-fg-rctd.i-no AND
      fg-bin.job-no  EQ b2-fg-rctd.job-no AND
      fg-bin.job-no2 EQ b2-fg-rctd.job-no2 AND
      fg-bin.tag     EQ b2-fg-rctd.tag
      NO-LOCK:

     IF fg-bin.loc NE b2-fg-rctd.loc OR
        fg-bin.loc-bin NE b2-fg-rctd.loc-bin THEN DO:
        CREATE b-fg-rctd.
        BUFFER-COPY b2-fg-rctd EXCEPT b2-fg-rctd.r-no TO b-fg-rctd
        ASSIGN b-fg-rctd.r-no = lv-rno
               b-fg-rctd.loc = fg-bin.loc
               b-fg-rctd.loc-bin = fg-bin.loc-bin
               b-fg-rctd.cases = 0
               b-fg-rctd.qty-case = 0
               b-fg-rctd.cases-unit = 0
               b-fg-rctd.partial = 0
               b-fg-rctd.t-qty = 0
               lv-rno = lv-rno + 1.
        RELEASE b-fg-rctd.
     END.
  END.  /* for each fg-bin*/
END PROCEDURE.

PROCEDURE fgPostLog :
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
       gltrans.trnum   = ip-trnum.

      assign
       debits  = 0
       credits = 0.
    end.
  end.

END PROCEDURE.



PROCEDURE calc-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    /*find first item finished goods based on the item number*/
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        use-index i-no no-lock no-error.
        
    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.

      if itemfg.sell-uom begins "L" then
        v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

      else
      if itemfg.sell-uom eq "CS" then
        v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

      else
      if avail uom then
        v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / uom.mult).

      else
        v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / 1000).

      if w-fg-rctd.rita-code eq "R" then do:
        if v-msf[1] gt w-fg-rctd.t-qty * itemfg.t-sqft then
          v-msf[2] = v-msf[2] + (v-msf[1] - ((w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft)).

        v-msf[1] = (w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft.
      end.
    end. /* avail itemfg */

END PROCEDURE.



PROCEDURE calc-partial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    /*find first item finished goods based on the item number*/
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        use-index i-no no-lock no-error.
        
    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.
          
      if itemfg.sell-uom begins "L" then
        v-fg-value = 0.

      else
      if itemfg.sell-uom eq "CS" then
        v-fg-value = 0.

      else
      if avail uom then
        v-fg-value = itemfg.sell-price * w-fg-rctd.partial / uom.mult.

      else
        v-fg-value = itemfg.sell-price * w-fg-rctd.partial / 1000.

      if w-fg-rctd.rita-code eq "R" then do:
        if v-msf[1] gt w-fg-rctd.partial * itemfg.t-sqft then
          v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.partial * itemfg.t-sqft)).

        v-msf[1] = w-fg-rctd.partial * itemfg.t-sqft.
      end.
    end. /* avail */

END PROCEDURE.

PROCEDURE orig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
    /*find first item finished goods based on the item number*/
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        use-index i-no no-lock no-error.
        
    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.
          
      if itemfg.sell-uom begins "L" then
        v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

      else
      if itemfg.sell-uom eq "CS" then
        v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

      else
      if avail uom then
        v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / uom.mult.

      else
        v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / 1000.

      if w-fg-rctd.rita-code eq "R" then do:
        if v-msf[1] gt w-fg-rctd.t-qty * itemfg.t-sqft then
          v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.t-qty * itemfg.t-sqft)).

        v-msf[1] = w-fg-rctd.t-qty * itemfg.t-sqft.
      end.
    end. /* avail itemfg */

    assign
     v-msf[1] = v-msf[1] / 1000
     v-msf[2] = v-msf[2] / 1000.

END PROCEDURE.
