/*------------------------------------------------------------------------
    File        : Unfgsm.p
    Purpose     : Unshipped Finished Goods Summary
    Main File   : fgrep/r-unfgsm.w
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}   
{sys/inc/var.i new shared}

DEFINE TEMP-TABLE ttUnfgsmRep NO-UNDO
    FIELD vUnfgsmFile AS CHAR.

DEFINE DATASET dsUnfgsmRep FOR ttUnfgsmRep.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegOrdDate AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndOrdDate AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust    AS CHAR NO-UNDO.    
    DEFINE INPUT PARAMETER prmBeginItem  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginWhse  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndWhse    AS CHAR NO-UNDO.    
    DEFINE INPUT PARAMETER prmOrdStatus  AS CHAR NO-UNDO.            
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUnfgsmRep.
    IF prmUser       = ?  THEN ASSIGN prmUser       = "".
    IF prmAction     = ?  THEN ASSIGN prmAction     = "".
    IF prmBegOrdDate = ?  THEN ASSIGN prmBegOrdDate = "".
    IF prmEndOrdDate = ?  THEN ASSIGN prmEndOrdDate = "".
    IF prmBeginCust  = ?  THEN ASSIGN prmBeginCust  = "".
    IF prmEndCust    = ?  THEN ASSIGN prmEndCust    = "".    
    IF prmBeginItem  = ?  THEN ASSIGN prmBeginItem  = "".
    IF prmEndItem    = ?  THEN ASSIGN prmEndItem    = "".
    IF prmBeginWhse  = ?  THEN ASSIGN prmBeginWhse  = "".
    IF prmEndWhse    = ?  THEN ASSIGN prmEndWhse    = "".    
    IF prmOrdStatus  = ?  THEN ASSIGN prmOrdStatus  = "".         
    IF prmOut        = ?  THEN ASSIGN prmOut        = "". 
    
    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.
    DEFINE VAR custcount AS CHAR NO-UNDO.
                                                      
    DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U NO-UNDO.
    DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
    DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)" NO-UNDO.    
    DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
    DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.    
    DEFINE VARIABLE rd_ord-stat AS CHARACTER INITIAL "Open" NO-UNDO.    
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
    DEF STREAM excel.   
    DEF TEMP-TABLE tt-report LIKE report.
 

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN 
    cocode = prmComp .

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
    AND usercust.cust-no = prmBeginCust NO-ERROR.
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


IF prmAction = "UnFgSumm" THEN DO:
    ASSIGN
        begin_cust      = prmBeginCust                             
        begin_i-no      = prmBeginItem                  
        begin_ord-date  = DATE(prmBegOrdDate)                 
        begin_whse      = prmBeginWhse                    
        end_cust        = prmEndCust                  
        end_i-no        = prmEndItem                       
        end_ord-date    = DATE(prmEndOrdDate)                  
        end_whse        = prmEndWhse                                                  
        rd_ord-stat     = prmOrdStatus                       
        .        

    ASSIGN                                                                   
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE . 

    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "unfgsm" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "unfgsm" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "unfgsm" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttUnfgsmRep.
        ASSIGN ttUnfgsmRep.vUnfgsmFile = excel-file.
    END.
    ELSE DO:
        CREATE ttUnfgsmRep.
        ASSIGN ttUnfgsmRep.vUnfgsmFile = vtextfile.
    END.
END.


/*********************************************************************************************/
PROCEDURE run-report :
{sys/form/r-topw.f}

def var v-fdate        as   date format "99/99/9999" init 01/01/0001.
def var v-tdate        like v-fdate                  init 12/31/9999.
def var v-fcust        like oe-ord.cust-no           init "".
def var v-tcust        like v-fcust                  init "zzzzzzzz".
def var v-fitem        like itemfg.i-no              init "".
def var v-titem        like v-fitem                  init "zzzzzzzzzzzzzzz".
def var v-floc         like oe-rell.loc              init "".
def var v-tloc         like v-floc                   init "zzzzz".
def var v-stat         as   char format "!"          init "O".

def var v-ono          as   int format "->>>>>>>>9" extent 2.
def var v-shp          like v-ono.
def var v-bol          like v-ono.
def var v-onh          like v-ono.
def var v-opo          like v-ono.

def var v-bal          as   int format "->>>>>>>>9".
def var v-una          like v-bal.
def var v-var          like v-bal.

def var v-hld-qty      as   dec.
def var str-tit4       like str-tit3.
def var v-loc          like oe-boll.loc.
DEF VAR excelheader AS CHAR NO-UNDO.

&Scoped-define where-phrase where oe-ordl.company eq cocode  ~
                              and oe-ordl.i-no    ge v-fitem ~
                              and oe-ordl.i-no    le v-titem


assign
 str-tit2 = "Unshipped Finished Goods Summary"
 {sys/inc/ctrtext.i str-tit2 112}

 v-stat    = SUBSTR(rd_ord-stat,1,1)
 v-fdate   = begin_ord-date
 v-tdate   = end_ord-date
 v-fcust   = begin_cust
 v-tcust   = end_cust
 v-fitem   = begin_i-no
 v-titem   = end_i-no
 v-floc    = begin_whse
 v-tloc    = end_whse.
 
/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "FG Item#,Description,Ordered,Shipped,Balance To Ship,"
              + "Issued BOL,On Hand,Available,Open PO,Net Variance".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE("general").

display "" with frame r-top.
   
FOR EACH tt-report:
  DELETE tt-report.
END.

    IF v-stat EQ "O" THEN
    FOR EACH oe-ordl
        {&where-phrase}
          AND oe-ordl.opened EQ YES
        USE-INDEX opened NO-LOCK:
      RUN create-report. 
    END.

    ELSE
    IF v-stat EQ "C" THEN
    FOR EACH oe-ordl
        {&where-phrase}
          AND oe-ordl.opened EQ NO
        USE-INDEX opened NO-LOCK:
      RUN create-report. 
    END.

    ELSE
    FOR EACH oe-ordl {&where-phrase} NO-LOCK:
      RUN create-report. 
    END.
    
    for each tt-report WHERE tt-report.term-id EQ "",
    
        first oe-rel where recid(oe-rel) eq tt-report.rec-id no-lock,
        
        first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-rel.ord-no
          and oe-ordl.i-no    eq oe-rel.i-no
          and oe-ordl.line    eq oe-rel.line
        no-lock,
        
        first oe-ord of oe-ordl no-lock,

        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-rel.i-no
        no-lock,

        first shipto of oe-rel no-lock
        
        break by tt-report.key-01
              by tt-report.key-02 
              by tt-report.key-03
              
        transaction:
            
      {fg/rep/fg-unsum.i}
            
      v-ono[1] = v-ono[1] + oe-rel.qty.

      if avail ar-invl then v-shp[1] = v-shp[1] + ar-invl.ship-qty.

      else
      if avail oe-rell        and
         avail oe-bolh        and
         (not oe-bolh.posted) then v-bol[1] = v-bol[1] + oe-rell.qty.

      if last-of(tt-report.key-01) then do:
        v-onh[1] = 0.
        for each fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq oe-ordl.i-no
              and fg-bin.loc     ge v-floc
              and fg-bin.loc     le v-tloc
            no-lock:
          v-onh[1] = v-onh[1] + fg-bin.qty.
        end.

        for each po-ordl
            where po-ordl.company   eq itemfg.company
              and po-ordl.i-no      eq itemfg.i-no
              and po-ordl.item-type eq no
              and lookup(po-ordl.stat,"O,P,U") gt 0
              and po-ordl.t-rec-qty lt po-ordl.cons-qty
            no-lock,

            first po-ord WHERE
                  po-ord.company EQ po-ordl.company AND
                  po-ord.po-no   EQ po-ordl.po-no AND
             lookup(po-ord.stat,"N,O,R,U") gt 0
              and po-ord.loc                    ge v-floc
              and po-ord.loc                    le v-tloc
            no-lock:

          if po-ordl.cons-uom eq "EA" then
            v-hld-qty = po-ordl.cons-qty.
          else
            run sys/ref/convquom.p(po-ordl.cons-uom, "EA", 0, 0, 0, 0,
                                   po-ordl.cons-qty, output v-hld-qty).

          if v-hld-qty - po-ordl.t-rec-qty gt 0 then
            v-opo[1] = v-opo[1] + (v-hld-qty - po-ordl.t-rec-qty).
        end.

        assign
         v-bal = v-ono[1] - v-shp[1]
         v-una = v-onh[1] - v-bol[1].

        if v-bal lt 0 then v-bal = 0.
        if v-una lt 0 then v-una = 0.

        v-var = v-onh[1] + v-opo[1] - v-bal.

        display oe-ordl.i-no        column-label "!Item"
                itemfg.i-name       column-label "!Description"
                                    format "x(25)"
                v-ono[1]            column-label "!Ordered"
                v-shp[1]            column-label "!Shipped"
                v-bal               column-label "Balance!To Ship"
                v-bol[1]            column-label "Issued!  BOL"
                v-onh[1]            column-label "!On Hand"
                v-una               column-label "Available"
                v-opo[1]            column-label "!Open PO"
                v-var               column-label "   Net!Variance"

            with no-box no-attr-space down STREAM-IO width 132.
        down.

        IF tb_excel THEN 
          PUT STREAM excel UNFORMATTED
               '"' oe-ordl.i-no                                           '",'
               '"' itemfg.i-name                                          '",'
               '"' STRING(v-ono[1],"->>>>>>>>9")                          '",'
               '"' STRING(v-shp[1],"->>>>>>>>9")                          '",'
               '"' STRING(v-bal,"->>>>>>>>9")                             '",'
               '"' STRING(v-bol[1],"->>>>>>>>9")                          '",'
               '"' STRING(v-onh[1],"->>>>>>>>9")                          '",'
               '"' STRING(v-una,"->>>>>>>>9")                             '",'
               '"' STRING(v-opo[1],"->>>>>>>>9")                          '",'
               '"' STRING(v-var,"->>>>>>>>9")                             '",'
               SKIP.

        assign
         v-ono[2] = v-ono[2] + v-ono[1]
         v-shp[2] = v-shp[2] + v-shp[1]
         v-bol[2] = v-bol[2] + v-bol[1]
         v-onh[2] = v-onh[2] + v-onh[1]
         v-opo[2] = v-opo[2] + v-opo[1]

         v-ono[1] = 0
         v-shp[1] = 0
         v-bol[1] = 0
         v-onh[1] = 0
         v-opo[1] = 0.

        if last(tt-report.key-01) then do:
          assign
           v-bal = v-ono[2] - v-shp[2]
           v-una = v-onh[2] - v-bol[2].

          if v-bal lt 0 then v-bal = 0.
          if v-una lt 0 then v-una = 0.

          v-var = v-onh[2] + v-opo[2] - v-bal.

          clear no-pause.

          put skip(1).

          display "           tt-report Totals:"   @ itemfg.i-name
                  v-ono[2]                      @ v-ono[1]
                  v-shp[2]                      @ v-shp[1]
                  v-bal
                  v-bol[2]                      @ v-bol[1]
                  v-onh[2]                      @ v-onh[1]
                  v-una
                  v-opo[2]                      @ v-opo[1]
                  v-var.
        end.
      end.
    end.

end procedure.


/*------------------------------------------------------------------------------------------*/

PROCEDURE create-report :
def var v-loc like oe-boll.loc.


    FOR EACH oe-ord of oe-ordl
        where oe-ord.ord-date ge begin_ord-date
          and oe-ord.ord-date le end_ord-date
          and oe-ord.cust-no  ge begin_cust
          and oe-ord.cust-no  le end_cust
          and oe-ord.stat     ne "D"
          and LOOKUP(oe-ord.cust-no, custcount) <> 0
        no-lock,

        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-ordl.i-no
        no-lock,

        each oe-rel
        where oe-rel.company eq cocode
          and oe-rel.ord-no  eq oe-ord.ord-no
          and oe-rel.i-no    eq oe-ordl.i-no
          and oe-rel.line    eq oe-ordl.line
        no-lock,

        first shipto of oe-rel no-lock:
        
      {fg/rep/fg-unsum.i}
      
      if v-loc ge begin_whse and v-loc le end_whse then do:
        create tt-report.
        assign
         tt-report.key-01  = oe-ordl.i-no
         tt-report.key-02  = string(oe-ordl.ord-no,"9999999999")
         tt-report.key-03  = string(oe-ordl.line,  "9999999999")
         tt-report.rec-id  = recid(oe-rel).
      end.
    END.

END PROCEDURE.
