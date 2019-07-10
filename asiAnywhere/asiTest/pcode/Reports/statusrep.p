
/*------------------------------------------------------------------------
    File        : statusrep.p
    Purpose     : Finished Goods Status Report
    Main File   : fgrep\r-fgstat.w
    Syntax      :

    Description : Reports

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttStatusReport NO-UNDO
        FIELD vStatRepFile AS CHAR
    .
    DEFINE DATASET dsStatusReport FOR ttStatusReport .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCust        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginItem      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem        AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER prmOut            AS CHARACTER NO-UNDO.
   
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsStatusReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser          = ? THEN ASSIGN prmUser          = "".
    IF prmAction        = ? THEN ASSIGN prmAction        = "".
    IF prmBegCust       = ? THEN ASSIGN prmBegCust       = "".
    IF prmEndCust       = ? THEN ASSIGN prmEndCust       = "".
    IF prmBeginItem     = ? THEN ASSIGN prmBeginItem     = "".
    IF prmEndItem       = ? THEN ASSIGN prmEndItem       = "".    
    IF prmOut           = ? THEN ASSIGN prmOut           = "No" .
    
    {sys/inc/var.i new shared}
    {custom/xprint.i}

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.    

    DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U NO-UNDO.
    DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.    
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
    AND usercust.cust-no = prmBegCust NO-ERROR.
 IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid begin customer for the user.....".
    RETURN.
 END.

 FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
 IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid end customer for the user.....".
    RETURN.
 END.

 FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
 END.


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


 IF prmAction = "statusrep" THEN DO:
    ASSIGN 
        begin_cust    = prmBegCust                                     
        begin_i-no    = prmBeginItem                            
        end_cust      = prmEndCust                                      
        end_i-no      = prmEndItem .                                  
                                                                         
    ASSIGN                                                          
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE     
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "statrep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "statrep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "statrep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttStatusReport.
        ASSIGN ttStatusReport.vStatRepFile = excel-file.
    END.
    ELSE DO:
        CREATE ttStatusReport.
        ASSIGN ttStatusReport.vStatRepFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
PROCEDURE run-report :
{sys/form/r-topw.f}

def var v-ino like itemfg.i-no extent 2 initial [" ", "ZZZZZZZZZZZZZZZ"].
def var v-cust as char format "x(8)" extent 2 initial [" ", "ZZZZZZZZ"].
def var v-custown as logical format "Y/N" initial "N".
DEF VAR sort-opt AS CHAR NO-UNDO INITIAL "C" FORMAT "!".
def var pcat as logical initial no.
def var v-first as logical initial no no-undo.
def var v-dscr as char format "x(25)" no-undo.
def var v-qty as dec format "->>,>>>,>>9" no-undo.
def var v-inv-qty LIKE oe-ordl.inv-qty format "->>,>>>,>>9" no-undo.
def var v-ship-qty LIKE oe-ordl.ship-qty format "->>,>>>,>>9" no-undo.
def var v-q-onh as dec format "->>,>>>,>>9" no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.

form header
"Order# Item#           Description                 Order Qty Shipped Qty  OnHand Qty Date    P.O. Number      Job Number"                 
  with frame f-top STREAM-IO width 132 no-box page-top.

form
    cust.name label "Customer Name" 
with side-labels down STREAM-IO width 132 frame custname.

form
    oe-ord.ord-no 
    oe-ordl.i-no 
    v-dscr 

    v-qty
    v-ship-qty
    v-q-onh
    oe-ord.ord-date 
    oe-ord.po-no 
    oe-ord.job-no 
    oe-ord.job-no2 

with no-labels down STREAM-IO width 132 frame itemx.
 
assign
    str-tit2 = "Finished Goods Status Report"
   {sys/inc/ctrtext.i str-tit2 112}

 v-cust[1]   = begin_cust
 v-cust[2]   = end_cust
 v-ino[1]    = begin_i-no
 v-ino[2]    = end_i-no.


/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Customer Name,Order#,FG Item#,Description,Order Qty,Shipped Qty,On Hand Qty,Date,P.O. Number,Job Number,Job 2 Number".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.
 display with frame f-top.

    FOR EACH cust
        WHERE cust.company EQ cocode
          AND cust.cust-no GE v-cust[1]
          AND cust.cust-no LE v-cust[2] 
          AND lookup(cust.cust-no,custcount) <> 0
          AND CAN-FIND(FIRST oe-ord
                       WHERE oe-ord.company EQ cocode
                         AND oe-ord.cust-no EQ cust.cust-no
                       USE-INDEX cust)
        NO-LOCK USE-INDEX cust
        BREAK BY cust.cust-no:

      if first-of(cust.cust-no) then v-first = yes.

      for each oe-ord where oe-ord.company = cocode and
                            oe-ord.cust-no = cust.cust-no and
                            oe-ord.opened = yes
                            no-lock
                            use-index opened:

        for each oe-ordl of oe-ord where 
                          oe-ordl.i-no >= v-ino[1] and oe-ordl.i-no <= v-ino[2]
                          no-lock use-index ord-no:

          find first itemfg where itemfg.company = cocode and
                                  itemfg.i-no = oe-ordl.i-no no-lock no-error.
          if avail itemfg then
          do:
            if line-counter > lines-per-page /*56*/  then page.
            if v-first then
            do:
              display cust.name with frame custname.
              assign v-first = no.
            end.

            assign v-dscr = itemfg.i-name
                   v-qty = oe-ordl.qty
                   v-q-onh = itemfg.q-onh.

            RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT v-inv-qty, OUTPUT v-ship-qty).

            display oe-ord.ord-no oe-ordl.i-no v-dscr 
                    v-qty v-ship-qty v-q-onh 
                    oe-ord.ord-date oe-ord.po-no oe-ord.job-no oe-ord.job-no2
                    with frame itemx down.

/*
                    oe-ordl.qty oe-ordl.ship-qty itemfg.q-onh 
*/
            down with frame itemx.
            pause 0.

            IF tb_excel THEN 
              PUT STREAM excel UNFORMATTED
                  '"' cust.NAME                                     '",'
                  '"' oe-ord.ord-no                                 '",'
                  '"' oe-ordl.i-no                                  '",'
                  '"' v-dscr                                        '",'
                  '"' STRING(v-qty,"->>,>>>,>>9")                   '",'
                  '"' STRING(v-ship-qty,"->>,>>>,>>9")              '",'
                  '"' STRING(v-q-onh,"->>,>>>,>>9")                 '",'
                  '"' (IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date,"99/99/9999")
                       ELSE "")                                     '",'
                  '"' oe-ord.po-no                                  '",'
                  '"' oe-ord.job-no                                 '",'
                  '"' oe-ord.job-no2                                '",'
                  SKIP.
          end.
        end. /* for each oe-ordl  */
      end. /* for each oe-ord  */
    end. /* for each cust  */

end procedure.
