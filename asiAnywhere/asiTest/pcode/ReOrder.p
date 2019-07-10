


/*------------------------------------------------------------------------
    File        : ReOrder.p
    Purpose     :  Print Order

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*{methods/defines/hndldefs.i}
   
{methods/prgsecur.i} */
    
{custom/xprint.i}
    
{sys/inc/var.i new shared}




DEFINE TEMP-TABLE ttReOrder NO-UNDO
FIELD aFile AS CHAR
FIELD vFile AS CHAR
FIELD bFile AS CHAR.
DEFINE DATASET dsReOrder FOR ttReOrder .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust        AS CHARACTER  NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginItem      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginCategory  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCategory    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginWare      AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndWare        AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmQtyOnHand      AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER prIncQoh          AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER prmBelow          AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER prmPart           AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER prmHistory        AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER prmDash           AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRdqoh          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmDate           AS DATE NO-UNDO. 

    DEFINE INPUT PARAMETER prmPrintStock     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintPurch     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintLot       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintQty       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintPrice     AS CHAR NO-UNDO.

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReOrder.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser   = ?   THEN ASSIGN prmUser = "".
    IF prmBeginCust   = ?   THEN ASSIGN prmBeginCust = "".
    IF prmEndCust = ? THEN ASSIGN prmEndCust = "".
    IF prmBeginCategory  = ?  THEN ASSIGN prmBeginCategory = "".
    IF prmEndCategory  = ?  THEN ASSIGN prmEndCategory = "".
    IF prmBeginItem  = ?  THEN ASSIGN prmBeginItem = "".
    IF prmEndItem  = ?  THEN ASSIGN prmEndItem = "".
    IF prmBeginWare  = ?  THEN ASSIGN prmBeginWare = "".
    IF prmEndWare  = ?  THEN ASSIGN prmEndWare = "".
    IF prmPrintStock = ? THEN ASSIGN prmPrintStock = "All".
    IF prmPrintPurch = ? THEN ASSIGN prmPrintPurch = "All". 
    IF prmPrintLot   = ? THEN ASSIGN prmPrintLot   = "All".
    IF prmPrintQty   = ? THEN ASSIGN prmPrintQty   = "Qty Avail".
    IF prmPrintPrice = ? THEN ASSIGN prmPrintPrice = "Vendor".

DEFINE VARIABLE begin_as-of AS DATE INITIAL 01/01/02  NO-UNDO.
DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" NO-UNDO.
DEFINE VARIABLE tb_inc-qoh AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_part AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_history AS LOGICAL  NO-UNDO.
DEFINE VARIABLE tb_dash AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_below AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE begin_cat AS CHARACTER NO-UNDO.
DEFINE VARIABLE begin_cust AS CHARACTER NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE end_cat AS CHARACTER INITIAL "zzzzz" NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.


DEFINE VARIABLE rd_lot-reo AS CHARACTER INITIAL "All".
DEFINE VARIABLE rd_pri-ven-max AS CHARACTER INITIAL "Vendor".
DEFINE VARIABLE rd_pur-man AS CHARACTER INITIAL "All".
DEFINE VARIABLE rd_qav-ven AS CHARACTER INITIAL "Qty Avail".
DEFINE VARIABLE rd_stocked AS CHARACTER INITIAL "All".
DEFINE VARIABLE rd_qoh AS CHARACTER INITIAL  "Total Allocated"  .

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF VAR  tmp-path AS CHAR NO-UNDO. 
DEF VAR v-VERSION AS CHAR NO-UNDO. 
    
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD qty LIKE fg-bin.qty.

DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

DEF STREAM excel.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 47.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=9 (13CPI)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)" INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5. 
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO. 
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no .
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO.

DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.


DEF VAR prmComp AS CHAR NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
     AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

assign
    cocode       = prmComp
    locode       = usercomp.loc
    begin_whse  = prmBeginWare
    end_whse  = prmEndWare
    begin_as-of = prmDate
    begin_cat = prmBeginCategory
    begin_cust  = prmBeginCust
    begin_i-no  = prmBeginItem
    end_cat  = prmEndCategory
    end_cust  = prmEndCust
    end_i-no  = prmEndItem
    tb_inc-qoh  = prmQtyOnHand
    rd_qoh   = prmRdqoh
    tb_below   = prmBelow
    tb_part   = prmPart
    tb_inc-cust  = prIncQoh
    tb_dash   = prmDash
    tb_history   = prmHistory
    v-today    = TODAY
    tb_excel   = IF prmOut = "Yes" THEN TRUE ELSE FALSE
        
     rd_stocked =  prmPrintStock         
     rd_pur-man =  prmPrintPurch 
     rd_lot-reo =  prmPrintLot   
     rd_qav-ven =  prmPrintQty   
     rd_pri-ven-max =   prmPrintPrice 
      . 

    FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "X-VERSION" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "X-VERSION"
         sys-ctrl.descrip  = "Server Name"
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "Server 2003".
   END.
   IF AVAIL sys-ctrl  THEN
        v-VERSION = sys-ctrl.char-fld .
  RELEASE sys-ctrl.
 
FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "Xspool" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "Xspool"
         sys-ctrl.descrip  = "Default path To Create temp File for Web pdf "
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "c:\spool\".
   END.
   IF AVAIL sys-ctrl  THEN
        tmp-path = sys-ctrl.char-fld .
  RELEASE sys-ctrl.

 IF prmAction = "Reorder" THEN DO:
    IF prmOut = "No" THEN DO:
    assign
        init-dir    = v-webrootpath 
        lv-pdf-file = init-dir + 'REORDER'
        lv-pdf-file = lv-pdf-file + prmBeginCust + STRING(TIME)
        vPdfFile   = 'REORDER' + prmBeginCust + STRING(TIME) + '.pdf'.
    
    run run-report.
    /*MESSAGE "testfile " list-name.
    /*RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").*/
    RUN printFile(list-name).*/

    IF v-VERSION = "Server 2008" THEN do:
        OS-COPY VALUE(list-name) VALUE (tmp-path).
        PAUSE 1.
    END.
    ELSE
        RUN printFile(list-name).
    
    CREATE ttReOrder.
    ASSIGN ttReOrder.vFile = vPdfFile.
    END.   

    IF prmOut = "Yes" THEN DO:

         ASSIGN
              init-dir    = v-webrootpath
              v-excel-file = init-dir + "REORDER" +
              STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + ".csv".  

               vPdfFile   = "REORDER" +
                   STRING(YEAR(v-today),"9999")
                    + STRING(MONTH(v-today),"99")
                    + STRING(DAY(v-today),"99") + ".csv".  
               MESSAGE "testre" v-excel-file. 
               run run-report.
               
               CREATE ttReOrder.
               ASSIGN ttReOrder.vFile = vPdfFile.
    END.
 END.
    
   PROCEDURE run-report :
/* ------------------------------------------------ fg/rep/fg-reord.p 9/91 cd */
/* reorder report                                                             */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}


def var v-cust      like itemfg.cust-no extent 2 init ["","zzzzzzzz"].
def var v-cat       like itemfg.procat extent 2 init ["","zzzzzz"].
def var v-item      like itemfg.i-no extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-inconh    as   log format "Y/N" init "Y".
def var v-totrel    as   log format "Tot All/Release" init "Y".
def var v-date      as   date format "99/99/9999" init today.
def var v-custown   as   log format "Y/N" init "N".
def var v-prt-all   as   log format "All/Below" init no.
def var v-stocked   as   char format "!" init "A".
def var v-pur-man   as   char format "!" init "A".
def var v-lot-reo   as   char format "!" init "R".
def var v-prt-cpn   as   log format "Y/N" init no.
def var v-prt-qty   as   log format "Qty/Vendor" init no.
def var v-prt-prc   as   char format "!" init "V".

def var v-qty-onh   as   int format "->>>,>>>,>>9".
def var v-cust-qty  as   int format "->>>,>>>,>>9".
def var v-reord-qty as   int format ">>>,>>>,>>>".
def var v-qty-avail as   INT format "->>>,>>>,>>9".
def var v-alloc-qty as   INT format "->>>,>>>,>>9".
def var v-stat      as   char                                           no-undo.

DEF VAR li-hist AS INT FORMAT "->>>,>>>,>>9" EXTENT 6 NO-UNDO.
DEF VAR ls-hlbl AS CHAR FORMAT "x(48)" NO-UNDO.
DEF VAR ls-hlbl2 AS CHAR FORMAT "x(68)" NO-UNDO.
DEF VAR ld-fr AS DATE NO-UNDO.
DEF VAR ld-to AS DATE NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO. /* 02/05/07 01100718 */

IF tb_history THEN DO:

 FORMAT HEADER "               "
                  "                   "
                  "       "
                  "     "
                  "         " 
                  "         "
                  "         "
                  "         "
                  "           "
                  "            "
                  "                  "
                  "                       "
                  SPACE(1)
                  "LAST 6 MONTHS USAGE"
                  SKIP

                  "   ITEM #      "
                  "    DESCRIPTION    "
                  "CAT   "
                  "UOM  "
                  "REORDER  "
                  "ON HAND  "
                  "QT ALLOC  "
                  "ON ORD  "
                  "MIN ORD QT  "
                  " QT AVAIL    "
                  "VENDOR NUMBER     "
                  "SUGGESTED REORDER  "
                  SPACE(0)
                  ls-hlbl2
                  SKIP

                  "---------------"
                  "-------------------"
                  "-------"
                  "-----"
                  "---------"
                  "---------"
                  "---------"
                  "----------"
                  "-----------"
                  "------------"
                  "------------------"
                  "-----------------------"
                  "----------"
                  "----------"
                  "----------"
                  "----------"
                  "----------"
                  "---------"
                  SKIP
    WITH FRAME itemhist-top2 NO-BOX PAGE-TOP STREAM-IO WIDTH 500   .
END.

ELSE DO:
    /*DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO. /* 02/05/07 01100718 */*/
    FORMAT HEADER "              "
                  "               "
                  "                  "
                  "      "
                  "    "
                  "           " 
                  "           "
                  "           "
                  "         "
                  "          "
                  "           "
                  "            "
                  SKIP

                  "     ITEM #      "
                  "  Cust Part  "
                  " DESCRIPTION      "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND    "
                  "QT ALLOC     "
                  " ON ORD   "
                  "MIN ORD QT "
                  "QT AVAIL   "
                  "VENDOR NUMBER "
                  "SUGG REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------"
                  "-------------------"
                  "-----"
                  "----"
                  "-----------"
                  "------------"
                  "------------"
                  "-----------"
                  "----------"
                  "------------"
                  "--------------"
                  "-------------"
                 
                  SKIP
        WITH FRAME itemhist-top NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

END.

/******************/
 FORMAT HEADER    "              "
                  "                "
                  "      "
                  "    "
                  "           " 
                  "           "
                  "            "
                  "         "
                  "          "
                  "           "
                  "            "
                  SKIP

                  "     ITEM #      "
                 "    DESCRIPTION   "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND    "
                  "QT ALLOC     "
                  " ON ORD     "
                  "MIN ORD QT "
                  "QT AVAIL   "
                  "VENDOR NUMBER "
                  "SUGGESTED REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------------"
                  "-----"
                  "----"
                  "-----------"
                  "------------"
                  "------------"
                  "------------"
                  "-----------"
                  "------------"
                  "--------------"
                  "-------------------"
                 
                  SKIP
        WITH FRAME itemhist-top3 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

 /**************************************/

 FORMAT HEADER    "              "
                  "               "
                  "                 "
                  "      "
                  "    "
                  "           " 
                  "           "
                  "           "
                  "         "
                  "          "
                  "            "
                  "            "
                  SKIP

                  "     ITEM #      "
                  "  Cust Part  "
                  " DESCRIPTION     "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND    "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "QT AVAIL   "
                  "SELLING PRICE "
                  "SUGG REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------"
                  "------------------"
                  "-----"
                  "----"
                  "-----------"
                  "------------"
                  "------------"
                  "------------"
                  "----------"
                  "------------"
                  "--------------"
                  "-------------"
                 
                  SKIP
        WITH FRAME item-top NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .
 /**************************************/

FORMAT HEADER    "              "
                  "               "
                  "                 "
                  "      "
                  "    "
                  "           " 
                  "           "
                  "           "
                  "         "
                  "          "
                  "            "
                  "            "
                  SKIP

                  "     ITEM #      "
                  "  Cust Part  "
                  " DESCRIPTION     "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND    "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "QT AVAIL   "
                  " MAX ORDER    "
                  "SUGG REORDER  "
                  
                  SKIP

                  "---------------"
                  "--------------"
                  "------------------"
                  "-----"
                  "----"
                  "-----------"
                  "------------"
                  "------------"
                  "------------"
                  "----------"
                  "------------"
                  "--------------"
                  "-------------"
                 
                  SKIP
        WITH FRAME item-top2 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

 /********************************************/
FORMAT HEADER     "              "
                  "               "
                  "                 "
                  "      "
                  "    "
                  "           " 
                  "           "
                  "           "
                  "         "
                  "          "
                  "            "
                  "            "
                  SKIP

                  "     ITEM #      "
                  "  Cust Part  "
                  " DESCRIPTION     "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND    "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "VENDOR     "
                  "SELLING PRICE "
                  "SUGG REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------"
                  "------------------"
                  "-----"
                  "----"
                  "-----------"
                  "------------"
                  "------------"
                  "------------"
                  "----------"
                  "------------"
                  "--------------"
                  "-------------"
                 
                  SKIP
        WITH FRAME item-top3 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

/***********************************************/

FORMAT HEADER     "              "
                  "               "
                  "                 "
                  "      "
                  "    "
                  "           " 
                  "           "
                  "           "
                  "         "
                  "          "
                  "            "
                  "            "
                  SKIP

                  "     ITEM #      "
                  "  Cust Part  "
                  " DESCRIPTION     "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND    "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "VENDOR     "
                  "VENDOR NUMBER "
                  "SUGG REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------"
                  "------------------"
                  "-----"
                  "----"
                  "-----------"
                  "------------"
                  "------------"
                  "------------"
                  "----------"
                  "------------"
                  "--------------"
                  "---------------"
                 
                  SKIP
        WITH FRAME item-top4 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

/*************************************************************/


FORMAT HEADER     "              "
                  "               "
                  "                 "
                  "      "
                  "    "
                  "           " 
                  "           "
                  "           "
                  "         "
                  "          "
                  "             "
                  "            "
                  SKIP

                  "     ITEM #      "
                  "  Cust Part  "
                  " DESCRIPTION     "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND    "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "VENDOR     "
                  "MAX ORDER      "
                  "SUGG REORDER  "
                  
                  SKIP

                  "---------------"
                  "--------------"
                  "------------------"
                  "-----"
                  "----"
                  "-----------"
                  "------------"
                  "------------"
                  "------------"
                  "----------"
                  "------------"
                  "--------------"
                  "--------------"
                 
                  SKIP
        WITH FRAME item-top5 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

/**************************************************************/
 FORMAT HEADER    "              "
                  "                "
                  "      "
                  "    "
                  "           " 
                  "           "
                  "           "
                  "         "
                  "          "
                  "           "
                  "            "
                  SKIP

                  "     ITEM #      "
                 "    DESCRIPTION   "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND    "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "QT AVAIL   "
                  " SELLING PRICE "
                  "SUGG REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------------"
                  "-----"
                  "----"
                  "-----------"
                  "------------"
                  "------------"
                  "------------"
                  "-----------"
                  "------------"
                  "--------------"
                  "---------------"
                 
                  SKIP
        WITH FRAME item-top6 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

/******************************************************************/
 FORMAT HEADER    "              "
                  "                "
                  "      "
                  "    "
                  "           " 
                  "          "
                  "           "
                  "          "
                  "          "
                  "           "
                  "            "
                  SKIP

                  "     ITEM #      "
                 "    DESCRIPTION   "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND     "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "QT AVAIL   "
                  " MAX REORDER   "
                  "SUGG REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------------"
                  "-----"
                  "----"
                  "-----------"
                  "-------------"
                  "------------"
                  "------------"
                  "-----------"
                  "------------"
                  "--------------"
                  "---------------"
                 
                  SKIP
        WITH FRAME item-top7 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

 /******************************************************************/

 FORMAT HEADER    "              "
                  "                "
                  "      "
                  "    "
                  "           " 
                  "          "
                  "           "
                  "          "
                  "            "
                  "           "
                  "            "
                  SKIP

                  "     ITEM #      "
                 "    DESCRIPTION   "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND     "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "VENDOR     "
                  " SELLING PRICE  "
                  "SUGG REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------------"
                  "-----"
                  "----"
                  "-----------"
                  "-------------"
                  "------------"
                  "------------"
                  "-----------"
                  "------------"
                  "--------------"
                  "--------------"
                 
                  SKIP
        WITH FRAME item-top8 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

 /******************************************************************/
FORMAT HEADER    "              "
                  "                "
                  "      "
                  "    "
                  "           " 
                  "          "
                  "           "
                  "          "
                  "            "
                  "            "
                  "              "
                  SKIP

                  "     ITEM #      "
                 "    DESCRIPTION   "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND     "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "VENDOR     "
                  " VENDOR NUM   "
                  "  SUGG REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------------"
                  "-----"
                  "----"
                  "-----------"
                  "-------------"
                  "------------"
                  "------------"
                  "-----------"
                  "------------"
                  "--------------"
                  "----------------"
                 
                  SKIP
        WITH FRAME item-top9 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

 /******************************************************************/
FORMAT HEADER    "              "
                  "                "
                  "      "
                  "    "
                  "           " 
                  "          "
                  "           "
                  "          "
                  "            "
                  "            "
                  "              "
                  SKIP

                  "     ITEM #      "
                 "    DESCRIPTION   "
                  "CAT  "
                  "UOM "
                  "REORDER    "
                  " ON HAND     "
                  "QT ALLOC     "
                  " ON ORD    "
                  "MIN ORD QT "
                  "VENDOR     "
                  " MAX REORDER   "
                  "  SUGG REORDER "
                  
                  SKIP

                  "---------------"
                  "--------------------"
                  "-----"
                  "----"
                  "-----------"
                  "-------------"
                  "------------"
                  "------------"
                  "-----------"
                  "------------"
                  "--------------"
                  "----------------"
                 
                  SKIP
        WITH FRAME item-top10 NO-BOX PAGE-TOP STREAM-IO WIDTH 380   .

/************************************************************************/


ASSIGN
 li1   = MONTH(TODAY) + 1
 ld-to = TODAY.

DO li = 1 TO 6:
  li1 = li1 - 1.
  IF li1 LT 1 THEN li1 = li1 + 12.

  ASSIGN
   ld-fr   = DATE(li1,1,YEAR(TODAY) - IF li1 GT MONTH(TODAY) THEN 1 ELSE 0)
   ls-hlbl = ls-hlbl + "   " + STRING(MONTH(ld-fr),"99") + "/" +
                               SUBSTR(STRING(YEAR(ld-fr),"9999"),3,2) + " , " 
   ls-hlbl2 = ls-hlbl2 + "     " + STRING(MONTH(ld-fr),"99") + "/" +
                               SUBSTR(STRING(YEAR(ld-fr),"9999"),3,2) .
END.
ASSIGN
str-tit2 = "FG Reordering Advice Report"
 {sys/inc/ctrtext.i str-tit2 132}

 v-cust[1]   = begin_cust
 v-cust[2]   = end_cust
 v-cat[1]    = begin_cat
 v-cat[2]    = end_cat
 v-item[1]   = begin_i-no
 v-item[2]   = end_i-no
 v-inconh    = tb_inc-qoh
 v-totrel    = rd_qoh BEGINS "Tot"
 v-date      = begin_as-of
 v-custown   = tb_inc-cust
 v-prt-all   = NOT tb_below
 v-stocked   = SUBSTR(rd_stocked,1,1)
 v-pur-man   = SUBSTR(rd_pur-man,1,1)
 v-lot-reo   = SUBSTR(rd_lot-reo,1,1)
 v-prt-cpn   = tb_part
 v-prt-qty   = rd_qav-ven BEGINS "Qty"
 v-prt-prc   = SUBSTR(rd_pri-ven-max,1,1).
 .


list-name = init-dir + "tmp" + string(time).
{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}

IF tb_history THEN DO:
 PUT "<PDF=DIRECT><OLANDSCAPE><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><CPI20.5><P6>" FORM "x(340)". 
 END.
ELSE DO:
 PUT "<PDF=DIRECT><OLANDSCAPE><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><CPI15.1><P8>" FORM "x(320)". 
 END.


IF tb_excel THEN 
DO:
  OUTPUT STREAM excel TO VALUE(v-excel-file).

excelheader = "".
 IF tb_history THEN 
   excelheader = "ITEM#,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,QTY AVAIL,VENDOR #,SUGT REORD," + 
              TRIM(ls-hlbl).
 ELSE DO:
     if v-prt-cpn then
         if v-prt-qty then
             if v-prt-prc eq "P" then 
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," + 
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,QTY AVAIL,SELL PRC,SUGT REORD".
             else if v-prt-prc eq "V" then 
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," + 
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,QTY AVAIL,VENDOR #,SUGT REORD".
             else
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," +
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,QTY AVAIL," + 
               "MAX ORDER QTY,SUGT REORD".
         else if v-prt-prc eq "P" then
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," + 
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,VENDOR,SELL PRC,SUGGT REORD".
         else if v-prt-prc eq "V" then 
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," +
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,VENDOR,VENDOR #,SUGT REORD".
         else
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," +
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,VENDOR," + 
               "MAX ORD QTY,SUGT REORD".
         else if v-prt-qty then
           if v-prt-prc eq "P" then 
 excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
               "QTY ON ORD,MIN ORD QTY,QTY AVAIL,SELL PRC,SUGGT REORD".
           else if v-prt-prc eq "V" then 
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
               "QTY ON ORD,MIN ORD QTY,QTY AVAIL,VENDOR #,SUGT REORD".
           else
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,QTY AVAIL,MAX ORD QTY,SUGT REORD".
         else if v-prt-prc eq "P" then 
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,VENDOR,SELL PRC,SUGT REORD".
         else if v-prt-prc eq "V" then 
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "MIN ORD QTY,VENDOR,VENDOR #,SUGT REORD".
         else
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,VENDOR,MAX ORD QTY,SUGT REORD".
 END.

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


display with frame r-top.

IF tb_history THEN DO: 
    DISPLAY WITH FRAME itemhist-top2 .
END.
ELSE DO:
 /******************************************/

        if v-prt-cpn then
               if v-prt-qty then
                   if v-prt-prc eq "P" then do:
                     DISPLAY WITH FRAME item-top .  /*fram*/ 
                   end.
                   else
                     if v-prt-prc eq "V" then do:

                        DISPLAY WITH FRAME itemhist-top .   /*fram*/ 
                        
                     end.
                   else do:
                      DISPLAY WITH FRAME item-top2 .    /*fram*/ 
                   end.
               else
                 if v-prt-prc eq "P" then do:
                   DISPLAY WITH FRAME item-top3 .    /*fram*/ 
                 end.
               else
                 if v-prt-prc eq "V" then do:
                     DISPLAY WITH FRAME item-top4 .    /*fram*/  
                 end.
                 else do:
                    DISPLAY WITH FRAME item-top5 .    /*fram*/ 
                 end.
           else
             if v-prt-qty then
               if v-prt-prc eq "P" then do:
                   DISPLAY WITH FRAME item-top6 .    /*fram*/
               end.
               else
                 if v-prt-prc eq "V" then do:
                  DISPLAY WITH FRAME itemhist-top3 .   /*fram*/
                 end.
                 else do:
                     DISPLAY WITH FRAME item-top7 .    /*fram*/
                 end.
             else
               if v-prt-prc eq "P" then DO:
                   DISPLAY WITH FRAME item-top8 .    /*fram*/
               end.
               else
                 if v-prt-prc eq "V" then do:
                   DISPLAY WITH FRAME item-top9 .    /*fram*/
                 end.
                 else do:
                    DISPLAY WITH FRAME item-top10 .    /*fram*/ 
                 end.

       /**************************************/

END.   /*end of else do*/

 
{fgrep/r-fgrord.i}

    IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  
END.


/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */

end procedure.





