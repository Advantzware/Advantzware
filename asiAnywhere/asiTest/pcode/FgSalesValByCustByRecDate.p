/*------------------------------------------------------------------------
    File        : FgSalesValByCustByRecDate.p
    Purpose     : Report for Finished Good Sales Value By Customer By Receipt Date

    Syntax      :

    Description : Report for Finished Good Sales Value By Customer By Receipt Date

    Author(s)   : Sewa
    Created     : june 29, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttFgSalesValByCustByRecDate NO-UNDO
        FIELD vFile1 AS CHAR
        FIELD aws AS CHAR.

    DEFINE DATASET dsFgSalesValByCustByRecDate FOR ttFgSalesValByCustByRecDate.

    DEFINE INPUT PARAMETER prmUser              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmDate              AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCustPo       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCustPo         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginSman         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSman           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmIncludeZeroQty    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmIncCustWareHouse  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustPart          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrint             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSort              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOlderThan         AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER cError              AS CHAR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFgSalesValByCustByRecDate.


    IF prmUser             = ? THEN ASSIGN prmUser            =  "".
    IF prmAction           = ? THEN ASSIGN prmAction          =  "".
    IF prmOut              = ? THEN ASSIGN prmOut             =  "".
    IF prmBeginCust        = ? THEN ASSIGN prmBeginCust       =  "".
    IF prmEndCust          = ? THEN ASSIGN prmEndCust         =  "".
    IF prmBeginCustPo      = ? THEN ASSIGN prmBeginCustPo     =  "".
    IF prmEndCustPo        = ? THEN ASSIGN prmEndCustPo       =  "".
    IF prmBeginSman        = ? THEN ASSIGN prmBeginSman       =  "".
    IF prmEndSman          = ? THEN ASSIGN prmEndSman         =  "".
    IF prmIncludeZeroQty   = ? THEN ASSIGN prmIncludeZeroQty  =  "".
    IF prmIncCustWareHouse = ? THEN ASSIGN prmIncCustWareHouse = "".
    IF prmCustPart         = ? THEN ASSIGN prmCustPart         = "".
    IF prmPrint            = ? THEN ASSIGN prmPrint            = "".
    IF prmSort             = ? THEN ASSIGN prmSort             = "".
    IF prmOlderThan        = ? THEN ASSIGN prmOlderThan        = 0.

    ASSIGN prmEndCust = prmBeginCust.

    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
    DEF VAR lv-pdf-file AS cha NO-UNDO.
    DEFINE VAR vPdfFile AS CHAR NO-UNDO.

{custom/xprint.i}
{sys/inc/var.i new shared}

    DEF VAR is-xprint-form AS LOG NO-UNDO.
    DEF VAR ls-fax-file AS cha NO-UNDO.

    def temp-table tt-report like report field qty as dec
                                     FIELD tt-date AS DATE
                                     FIELD rct-date AS DATE
                                     FIELD ship-date AS DATE
                                     FIELD cust-no AS cha.
    
    DEF TEMP-TABLE tt-fg-bin LIKE fg-bin.
    DEF STREAM excel.

    DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999" INITIAL 12/31/01 NO-UNDO.
    DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO .
    DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "X(15)" NO-UNDO .
    DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX" NO-UNDO.
    DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO .
    DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzz" NO-UNDO .
    DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" NO-UNDO .
    DEFINE VARIABLE fi_days-old AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 NO-UNDO .
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cusitm.csv" NO-UNDO .
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 50 NO-UNDO .
    DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" NO-UNDO .
    DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" NO-UNDO .
    DEFINE VARIABLE rd_ascdsc AS CHARACTER NO-UNDO .
    DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Order Date" NO-UNDO .
    DEFINE VARIABLE tb_cust-pt AS LOGICAL INITIAL NO NO-UNDO .
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO NO-UNDO .
    DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL NO NO-UNDO .
    DEFINE VARIABLE tb_inc-zer AS LOGICAL INITIAL NO NO-UNDO .
    
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.

    ASSIGN
        as-of-date = prmDate
        begin_cust = prmBeginCust
        end_cust   = prmEndCust
        begin_cust-po = prmBeginCustPo
        end_cust-po = prmEndCustPo
        begin_slm = prmBeginSman
        end_slm = prmEndSman
        fi_days-old = prmOlderThan
        rd_ascdsc = prmSort
        rd_sort = prmPrint
        tb_cust-pt = IF prmCustPart = "Yes" THEN TRUE ELSE FALSE
        tb_inc-cust = IF prmIncCustWareHouse = "Yes" THEN TRUE ELSE FALSE
        tb_inc-zer = IF prmIncludeZeroQty = "Yes" THEN TRUE ELSE FALSE
        .

        DEF VAR prmComp AS CHAR NO-UNDO.
        FIND FIRST usercomp WHERE
            usercomp.user_id = prmUser AND
            usercomp.loc = '' AND
            usercomp.company_default = YES
            NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

    FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust
    AND usercust.cust-no = prmEndCust  NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Cust No should be same for Begin Cust and End Cust".
    RETURN.
END.

    assign
        cocode = prmComp
        locode = usercomp.loc
        tb_excel   = IF prmOut = "Yes" THEN TRUE ELSE FALSE
        v-today = TODAY   .

        FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
            IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
            ASSIGN  
                init-dir    = v-webrootpath .
                IF prmAction = "Sales" THEN DO:
                    IF prmOut = "No" THEN DO:
                        assign
                            init-dir    = v-webrootpath
                            lv-pdf-file = init-dir + 'SALES'.
                            lv-pdf-file = lv-pdf-file + prmBeginCust + STRING(TIME).
                            vPdfFile   = 'SALES' + prmBeginCust + STRING(TIME) + '.pdf'.
    
                            run run-report.
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        CREATE ttFgSalesValByCustByRecDate.
                        ASSIGN ttFgSalesValByCustByRecDate.vFile1 = vPdfFile.
                    END.
            IF prmOut = "Yes"  THEN DO:
                assign
                init-dir    = v-webrootpath
                v-excel-file = init-dir + 'SALES' +
                        STRING(YEAR(v-today),"9999")
                        + STRING(MONTH(v-today),"99")
                        + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        
                    vPdfFile   = 'SALES' + 
                            STRING(YEAR(v-today),"9999")
                            + STRING(MONTH(v-today),"99")
                            + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
                   
    
                    run run-report.
        
                    CREATE ttFgSalesValByCustByRecDate.
                    ASSIGN ttFgSalesValByCustByRecDate.vFile1 = vPdfFile.
                END.
    END.


/*******************************Run Report*************************************/

    PROCEDURE run-report :
{sys/form/r-topw.f  "Hot Keys I-R-6"}

def var vdat        as   date init today format "99/99/9999" no-undo.
def var fcus        like itemfg.cust-no no-undo.
def var tcus        like fcus init "zzzzzzzz" no-undo.
def var fpo         like oe-ordl.po-no no-undo.
def var tpo         like fpo init "zzzzzzzzzzzzzzz" no-undo.
def var fsls        like cust.sman no-undo.
def var tsls        like fsls init "zzz" no-undo.
def var vzer        as   log format "Y/N" init NO no-undo.
def var vwhs        like vzer no-undo.
def var vpcp        like vzer no-undo.
def var vdue        like vzer format "DueDate/OrderDate" no-undo.

def var v-frst      as   log no-undo.
def var v-print     as   log no-undo.
def var v-bin       as   log no-undo.
def var v-po-no     like oe-ordl.po-no no-undo.
def var v-price     like itemfg.sell-price no-undo.
def var v-uom       like itemfg.sell-uom no-undo.
def var v-cas-cnt   like itemfg.case-count no-undo.
def var v-binqty    as   DEC no-undo.
def var v-ext       as   dec extent 3 no-undo.
def var v-qty       as   dec extent 3 no-undo.
def var v-date      as   date no-undo.
def var v-rct-date      as   date no-undo.
def var v-pallets   as   dec no-undo.
DEF VAR v-frst-date AS   DATE NO-UNDO.
DEF VAR v-ship-date AS   DATE NO-UNDO.
DEF VAR lv-stat     AS   CHAR NO-UNDO.

DEF BUFFER b-fg-rcpth FOR fg-rcpth.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

form header skip(1) with frame r-top.

form itemfg.cust-no column-label "CUST ID"
     v-po-no column-label " P.O. NUM"
     itemfg.i-no column-label " ITEM NUM"
     itemfg.part-no label "CUST PART #" format "x(15)"
     itemfg.i-name label "DESCRIPTION" format "x(20)"
     fg-bin.job-no column-label "  JOB NUM"
     space(0) "-" space(0) fg-bin.job-no2 column-label "" format ">9"
     v-qty[1] column-label "QTY ON-HAND" format "->>>,>>>,>>9"
     v-pallets column-label "PALLETS" format "->>>>>9"
     v-price column-label "SELL PRICE"  format ">>>,>>9.99"
     v-ext[1] column-label "TOTAL VALUE"  format "->>>,>>>,>>9.99"
     v-date column-label "DATE" format "99/99/99"
     v-rct-date column-label "REC DATE" format "99/99/99"
     v-ship-date column-label "L.SHIP DATE" format "99/99/99"
     with no-box frame itemx1 down STREAM-IO width 200.

form itemfg.cust-no column-label "CUST ID"
     v-po-no column-label " P.O.NUM"
     itemfg.i-no column-label " ITEM NUM"
     itemfg.i-name label "DESCRIPTION"   format "x(20)"
     fg-bin.job-no column-label "  JOB NUM"
     space(0) "-" space(0) fg-bin.job-no2 column-label "" format ">9"
     v-qty[1] column-label "QTY ON-HAND" format "->>>,>>>,>>9"
     v-pallets column-label "PALLETS" format "->>>>>9"
     v-price column-label "SELL PRICE"  format ">>>,>>9.99"
     v-ext[1] column-label "TOTAL VALUE"  format "->>>,>>>,>>9.99"
     v-date column-label "DATE" format "99/99/99"
     v-rct-date column-label "REC DATE" format "99/99/99"
     v-ship-date column-label "L.SHIP DATE" format "99/99/99"
    with no-box frame itemx2 down STREAM-IO width 200.


assign
 str-tit2 = "Finished Goods Sales Value By Customer By Receipt Date--" + "Cust#:" + prmBeginCust
 {sys/inc/ctrtext.i str-tit2 112}
 
 vdat   = as-of-date
 fcus   = begin_cust
 tcus   = end_cust
 fpo    = begin_cust-po
 tpo    = END_cust-po
 fsls   = begin_slm
 tsls   = end_slm
 vzer   = tb_inc-zer
 vwhs   = tb_inc-cust
 vpcp   = tb_cust-pt
 vdue   = rd_sort EQ "due Date".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}
    PUT "<PDF=DIRECT><OLANDSCAPE><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><CPI15.3><P8>" FORM "x(350)". 

 IF tb_excel THEN DO:
          OUTPUT STREAM excel TO VALUE(v-excel-file).
          excelheader = "CUST ID,PO#,ITEM #,CUST PART#,DESCRIPTION,JOB#," +
                        ",QTY ON-HAND,PALLETS,SELL PRICE,TOTAL VALUE,DATE," +
                        "RECEIPT DATE,LAST SHIP DATE".
      PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
END. 


for each tt-report:
    delete tt-report.
end.

for each itemfg
        where itemfg.company    eq cocode
          and itemfg.cust-no    ge fcus
          and itemfg.cust-no    le tcus
          and itemfg.cust-po-no ge fpo
          and itemfg.cust-po-no le tpo
        use-index customer no-lock,
        first cust
        where cust.company eq cocode
          and cust.cust-no eq itemfg.cust-no
          and cust.sman    ge fsls
          and cust.sman    le tsls
        no-lock
        break by itemfg.cust-no
              BY itemfg.i-no:

      STATUS DEFAULT "Processing Customer#/FG Item#: " +
                     TRIM(itemfg.cust-no) + "/" + TRIM(itemfg.i-no).

      v-bin = no.

      for each fg-bin
          where fg-bin.company eq cocode
            and fg-bin.i-no    eq itemfg.i-no
            and (vwhs or (fg-bin.loc ne "CUST" and fg-bin.cust-no eq ""))
          NO-LOCK
          use-index i-no:

       IF fg-bin.tag NE "" then
       FOR EACH fg-rdtlh NO-LOCK
          WHERE fg-rdtlh.company    EQ fg-bin.company
            AND fg-rdtlh.tag        EQ fg-bin.tag
            AND fg-rdtlh.loc        EQ fg-bin.loc
            AND fg-rdtlh.loc-bin    EQ fg-bin.loc-bin
            AND fg-rdtlh.cust-no    EQ fg-bin.cust-no
          USE-INDEX tag,
          FIRST fg-rcpth NO-LOCK
          WHERE fg-rcpth.r-no       EQ fg-rdtlh.r-no
            AND fg-rcpth.i-no       EQ fg-bin.i-no
            AND fg-rcpth.job-no     EQ fg-bin.job-no
            AND fg-rcpth.job-no2    EQ fg-bin.job-no2
            AND fg-rcpth.rita-code  EQ fg-rdtlh.rita-code
            AND fg-rcpth.trans-date LE vdat 
          USE-INDEX r-no
          break by fg-rcpth.i-no
                by fg-rcpth.job-no
                by fg-rcpth.job-no2
                by fg-rdtlh.loc
                by fg-rdtlh.loc-bin
                by fg-rdtlh.tag
                by fg-rdtlh.cust-no
                by fg-rcpth.trans-date
                by fg-rcpth.r-no:
                
        v-bin = yes.
       
        IF FIRST-OF(fg-rdtlh.tag) THEN v-frst-date = fg-rcpth.trans-date.

        if index("RATE",fg-rcpth.rita-code) ne 0 THEN v-binqty = v-binqty + fg-rdtlh.qty.
        ELSE if fg-rcpth.rita-code eq "C" THEN v-binqty = fg-rdtlh.qty.
        ELSE if fg-rcpth.rita-code eq "S" THEN v-binqty = v-binqty - fg-rdtlh.qty.

        /*if last-of(fg-bin.tag) then*/
        if last-of(fg-rdtlh.cust-no) then do:
          assign
           v-qty[1]    = v-qty[1] + v-binqty
           v-binqty    = 0
           v-ship-date = ?.

        /*if last-of(fg-bin.job-no2) then do:*/
           find last oe-ordl
              where oe-ordl.company   eq cocode
                and oe-ordl.i-no      eq fg-bin.i-no
                and (oe-ordl.ord-no   eq fg-bin.ord-no or
                     (oe-ordl.job-no  eq fg-bin.job-no and
                      oe-ordl.job-no2 eq fg-bin.job-no2))
              use-index item no-lock no-error.
          if avail oe-ordl THEN DO:
            find first oe-ord
                where oe-ord.company eq cocode
                  and oe-ord.ord-no  eq oe-ordl.ord-no
                no-lock.
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-ordl.company
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no
                  AND oe-rel.i-no    EQ oe-ordl.i-no
                  AND oe-rel.line    EQ oe-ordl.line
                BY oe-rel.rel-date DESC:
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
              IF lv-stat EQ "C" THEN DO:
                v-ship-date = oe-rel.rel-date.
                LEAVE.
              END.
            END.
          END.
              
          v-date = if avail oe-ordl then 
                     if vdue then oe-ordl.req-date
                             else oe-ord.ord-date
                   else ?.

          IF fi_days-old > 0 THEN RUN calc-qoh (OUTPUT v-qty[1]).

          RUN first-date (ROWID(fg-bin), INPUT-OUTPUT v-frst-date).

          create tt-report.
          assign
           tt-report.key-01 = if v-date eq ? then ""
                              else string(year(v-date),"9999") +
                                   string(month(v-date),"99")  +
                                   string(day(v-date),"99")
           tt-report.qty    = v-qty[1]
           v-qty[1]         = 0
           tt-report.rec-id = recid(fg-bin)
           tt-report.tt-date = v-date
           tt-report.cust-no = itemfg.cust-no
           tt-report.rct-date = v-frst-date
           tt-report.ship-date = v-ship-date.
        end.
       END.

       else
       FOR each fg-rcpth
          where fg-rcpth.company      eq fg-bin.company
            and fg-rcpth.i-no         eq fg-bin.i-no
            and fg-rcpth.job-no       eq fg-bin.job-no
            and fg-rcpth.job-no2      eq fg-bin.job-no2
            and fg-rcpth.trans-date   le vdat
          no-lock use-index tran,
          each fg-rdtlh
          where fg-rdtlh.r-no         eq fg-rcpth.r-no
            and fg-rdtlh.loc          eq fg-bin.loc
            and fg-rdtlh.loc-bin      eq fg-bin.loc-bin
            and fg-rdtlh.tag          eq fg-bin.tag
            and fg-rdtlh.cust-no      eq fg-bin.cust-no
            and fg-rdtlh.rita-code    eq fg-rcpth.rita-code
          no-lock
          break by fg-rcpth.i-no
                by fg-rcpth.job-no
                by fg-rcpth.job-no2
                by fg-rdtlh.loc
                by fg-rdtlh.loc-bin
                by fg-rdtlh.tag
                by fg-rdtlh.cust-no
                by fg-rcpth.trans-date
                by fg-rcpth.r-no:
                
        v-bin = yes.
       
        IF FIRST-OF(fg-rdtlh.tag) THEN v-frst-date = fg-rcpth.trans-date.

        if index("RATE",fg-rcpth.rita-code) ne 0 THEN v-binqty = v-binqty + fg-rdtlh.qty.
        ELSE if fg-rcpth.rita-code eq "C" THEN v-binqty = fg-rdtlh.qty.
        ELSE if fg-rcpth.rita-code eq "S" THEN v-binqty = v-binqty - fg-rdtlh.qty.

        /*if last-of(fg-bin.tag) then*/
        if last-of(fg-rdtlh.cust-no) then do:
          assign
           v-qty[1]    = v-qty[1] + v-binqty
           v-binqty    = 0
           v-ship-date = ?.

        /*if last-of(fg-bin.job-no2) then do:*/
           find last oe-ordl
              where oe-ordl.company   eq cocode
                and oe-ordl.i-no      eq fg-bin.i-no
                and (oe-ordl.ord-no   eq fg-bin.ord-no or
                     (oe-ordl.job-no  eq fg-bin.job-no and
                      oe-ordl.job-no2 eq fg-bin.job-no2))
              use-index item no-lock no-error.
          if avail oe-ordl THEN DO:
            find first oe-ord
                where oe-ord.company eq cocode
                  and oe-ord.ord-no  eq oe-ordl.ord-no
                no-lock.
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-ordl.company
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no
                  AND oe-rel.i-no    EQ oe-ordl.i-no
                  AND oe-rel.line    EQ oe-ordl.line
                BY oe-rel.rel-date DESC:
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
              IF lv-stat EQ "C" THEN DO:
                v-ship-date = oe-rel.rel-date.
                LEAVE.
              END.
            END.
          END.
              
          v-date = if avail oe-ordl then 
                     if vdue then oe-ordl.req-date
                             else oe-ord.ord-date
                   else ?.

          IF fi_days-old > 0 THEN RUN calc-qoh (OUTPUT v-qty[1]).

          RUN first-date (ROWID(fg-bin), INPUT-OUTPUT v-frst-date).

          create tt-report.
          assign
           tt-report.key-01 = if v-date eq ? then ""
                              else string(year(v-date),"9999") +
                                   string(month(v-date),"99")  +
                                   string(day(v-date),"99")
           tt-report.qty    = v-qty[1]
           v-qty[1]         = 0
           tt-report.rec-id = recid(fg-bin)
           tt-report.tt-date = v-date
           tt-report.cust-no = itemfg.cust-no
           tt-report.rct-date = v-frst-date
           tt-report.ship-date = v-ship-date.                 
        end.
       END.
      end.
END.

IF rd_ascdsc = "A" THEN {fgrep/r-custim.i}
                   ELSE {fgrep/r-custim.i DESC}



IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
    
 END.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.


/******************************End Run Report*********************************/

/**************************Calc QOH***************************************/

 PROCEDURE calc-qoh :

  DEF OUTPUT PARAM op-qty AS DEC.

  DEF VAR vdat          AS   DATE.
  DEF VAR v-curr        AS   LOG.
  DEF VAR v-q-or-v      AS   LOG.

  DEF VAR v-qohj        AS   DEC                EXTENT 6.
  DEF VAR v-qohi        LIKE v-qohj.
  DEF VAR v-qty         AS   INT.
  DEF VAR v-qty1        LIKE v-qty.
  DEF VAR v-qtyc        LIKE v-qty.
  DEF VAR v-red         LIKE v-qty.
  DEF VAR v             AS   INT.
  DEF VAR v-val         AS   DEC                EXTENT 4.
  DEF VAR v-cst         AS   DEC                EXTENT 4.
  DEF VAR v-u-val       AS   DEC.
  DEF VAR v-u-cst       AS   DEC.
  DEF VAR v-date        AS   DATE.
  DEF VAR lv-tag        LIKE fg-rdtlh.tag NO-UNDO.
  DEF VAR ld-last       AS   DATE NO-UNDO.

  DEF BUFFER b-f-rc for fg-rcpth.
  DEF BUFFER b-f-rd for fg-rdtlh.

  ASSIGN
   vdat     = as-of-date
   v-curr   = YES
   v-q-or-v = YES.
  
  /*
  FOR EACH itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ tt-report.key-06
      NO-LOCK,
      EACH fg-bin
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE tt-fg-bin.
    BUFFER-COPY fg-bin EXCEPT rec_key TO tt-fg-bin.
    */

    IF fi_days-old NE 0 THEN DO:
      /*tt-fg-bin.qty = 0.*/
      ASSIGN
           v-cst[1] = 0
           v-val[1] = 0
           v-qohi   = 0.

      FOR EACH fg-rcpth
          WHERE fg-rcpth.company    EQ fg-bin.company
            AND fg-rcpth.i-no       EQ fg-bin.i-no
            AND fg-rcpth.job-no     EQ fg-bin.job-no
            AND fg-rcpth.job-no2    EQ fg-bin.job-no2
            AND fg-rcpth.trans-date LE as-of-date 
          NO-LOCK USE-INDEX tran,

          EACH fg-rdtlh
          WHERE fg-rdtlh.r-no       EQ fg-rcpth.r-no
            AND fg-rdtlh.loc        EQ fg-bin.loc
            AND fg-rdtlh.loc-bin    EQ fg-bin.loc-bin
            AND fg-rdtlh.tag        EQ fg-bin.tag
            AND fg-rdtlh.cust-no    EQ fg-bin.cust-no
            AND fg-rdtlh.rita-code  EQ fg-rcpth.rita-code
          NO-LOCK

          BREAK BY fg-rdtlh.loc
                BY fg-rdtlh.loc-bin
                BY fg-rdtlh.tag
                BY fg-rdtlh.cust-no
                BY fg-rcpth.trans-date
                BY fg-rcpth.r-no:

        /*IF FIRST(fg-bin.i-no) THEN
          ASSIGN
           v-cst[1] = 0
           v-val[1] = 0
           v-qohi   = 0.
        */

        {fg/rep/fg-aging.i fi_days-old}

        v-qtyc = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                 v-qohj[4] + v-qohj[5] + v-qohj[6].      

        /*if v-qohj[6] lt 0 then do:
            v-qty = v-qohj[6] * -1.
          
            do v = 5 to 1 by -1:
              if v-qohj[v] gt 0 then
                assign
                 v-red     = min(v-qty,v-qohj[v])
                 v-qohj[v] = v-qohj[v] - v-red
                 v-qty     = v-qty     - v-red.
               
              if v-qty le 0 then leave.
            end.
          
            if v-qty gt 0 then v-qohi[6] = v-qohi[6] - v-qty.
        END.*/

            release oe-ordl.
            if fg-bin.job-no ne "" then
            find last oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.job-no  eq fg-bin.job-no
                and oe-ordl.job-no2 eq fg-bin.job-no2
                and oe-ordl.i-no    eq fg-rcpth.i-no
              use-index job no-lock no-error.
              
            if not v-curr then
            assign
             v-qohj[1] = 0
             v-qohj[2] = 0
             v-qohj[3] = 0.

          IF fg-rcpth.rita-code EQ "C" THEN v-qohi = 0.

          assign
           v-qty     = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                       v-qohj[4] + v-qohj[5]
           v-qohi[1] = v-qohi[1] + v-qohj[1]
           v-qohi[2] = v-qohi[2] + v-qohj[2]
           v-qohi[3] = v-qohi[3] + v-qohj[3]
           v-qohi[4] = v-qohi[4] + v-qohj[4]
           v-qohi[5] = v-qohi[5] + v-qohj[5]
           v-qohi[6] = v-qohi[6] + v-qohj[6]
           v-qohj    = 0.

          if avail oe-ordl then
            assign
             v-u-cst  = oe-ordl.t-cost / oe-ordl.qty
             v-u-val  = oe-ordl.t-price / oe-ordl.qty.
           
          else do:
            if itemfg.prod-uom eq "EA" then
              v-u-cst = itemfg.total-std-cost.
            else
              run sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                   itemfg.total-std-cost, output v-u-cst).
                                   
            if itemfg.sell-uom eq "EA" then
              v-u-val = itemfg.sell-price.
            else
              run sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                     itemfg.sell-price, output v-u-val).
          end.
        
          if v-u-cst eq ? then v-u-cst = 0.
          if v-u-val eq ? then v-u-val = 0.
        
          assign
           v-cst[1] = v-cst[1] + (v-qty * v-u-cst)
           v-val[1] = v-val[1] + (v-qty * v-u-val).
        
          if v-qohi[6] lt 0 then do:
            assign
             v-qty     = v-qohi[6] * -1
             v-qohi[6] = 0.
          
            do v = 5 to 1 by -1:
              if v-qohi[v] gt 0 then
                assign
                 v-red     = min(v-qty,v-qohi[v])
                 v-qohi[v] = v-qohi[v] - v-red
                 v-qty     = v-qty     - v-red.
               
              if v-qty le 0 then leave.
            end.
          
            if v-qty gt 0 then
              assign
               v-qohi   = 0
               v-cst[1] = 0
               v-val[1] = 0.
          end. 

          if v-cst[1] lt 0 then v-cst[1] = 0.
          if v-val[1] lt 0 then v-val[1] = 0.
        
          if not v-q-or-v then do:
            v-qty = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].
          
            do v = 1 to 5:
               v-qohi[v] = v-val[1] / v-qty * v-qohi[v].             
               if v-qohi[v] eq ? then v-qohi[v] = 0.
            end.
          end.
      END.

      IF v-qohi[1] LT 0 THEN DO:
        v-qty = v-qohi[1] * -1.

        DO v = 5 TO 2 BY -1:
          IF v-qohi[v] GT 0 THEN
            ASSIGN
             v-red     = MIN(v-qty,v-qohi[v])
             v-qohi[v] = v-qohi[v] - v-red
             v-qty     = v-qty     - v-red.
               
          IF v-qty LE 0 THEN LEAVE.
        END.
      END.

      op-qty /*tt-fg-bin.qty*/ = v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].
    END.

    /*
  END. /* for each itemfg*/
     */
END PROCEDURE.

/***************************End Calc QOH *******************************/

/******************************First Date*****************************/

 PROCEDURE first-date :

  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT-OUTPUT PARAM io-date AS DATE NO-UNDO.

  DEF BUFFER b-bin   FOR fg-bin.
  DEF BUFFER b-rcpth FOR fg-rcpth.
  DEF BUFFER b-rdtlh FOR fg-rdtlh.


  FIND b-bin WHERE ROWID(b-bin) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-bin THEN
  IF TRIM(b-bin.tag) NE "" THEN
  FOR EACH b-rdtlh NO-LOCK
      WHERE b-rdtlh.company   EQ b-bin.company
        AND b-rdtlh.tag       EQ b-bin.tag
        AND b-rdtlh.rita-code EQ "R"
      USE-INDEX tag,
      EACH b-rcpth NO-LOCK
      WHERE b-rcpth.r-no      EQ b-rdtlh.r-no
        AND b-rcpth.i-no      EQ b-bin.i-no
        AND b-rcpth.rita-code EQ b-rdtlh.rita-code
      USE-INDEX r-no
      BY b-rcpth.trans-date
      BY b-rcpth.r-no:
    LEAVE.
  END.

  ELSE
  IF TRIM(b-bin.job-no) NE "" THEN
  FOR EACH b-rcpth NO-LOCK
      WHERE b-rcpth.company   EQ b-bin.company
        AND b-rcpth.job-no    EQ b-bin.job-no
        AND b-rcpth.job-no2   EQ b-bin.job-no2
        AND b-rcpth.i-no      EQ b-bin.i-no
        AND b-rcpth.rita-code EQ "R"
      USE-INDEX job,
      EACH b-rdtlh NO-LOCK
      WHERE b-rdtlh.r-no      EQ b-rcpth.r-no
        AND b-rdtlh.rita-code EQ b-rcpth.rita-code
      BY b-rcpth.trans-date
      BY b-rcpth.r-no:
    LEAVE.
  END.

  IF AVAIL b-rcpth THEN io-date = b-rcpth.trans-date.

END PROCEDURE.

/******************************End First Date**************************/
