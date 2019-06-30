def input param ip-recid as recid no-undo.
def input param ip-eb-recid as recid no-undo.
def input param ip-qty as cha no-undo.
def output param op-char-val as cha no-undo.
def output param op-char-val2 as cha no-undo.
def output param op-date-val as cha no-undo.
def output param op-date-val2 as cha no-undo.

def var i as int no-undo.
def SHARED VAR cocode as CHAR no-undo.
DEF VAR ld-msf AS DEC NO-UNDO.


DEFINE VARIABLE lv-qty1 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0  NO-UNDO. 
DEFINE VARIABLE lv-qty10 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0  NO-UNDO.
DEFINE VARIABLE lv-qty11 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty12 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty13 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty14 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty15 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty16 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty17 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty18 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty19 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty2 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty20 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty3 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty4 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty5 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty6 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0     NO-UNDO.
DEFINE VARIABLE lv-qty7 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty8 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-qty9 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 NO-UNDO.


op-char-val = string(lv-qty1) + "," + string(lv-qty2) + "," +
                  string(lv-qty3) + "," + string(lv-qty4) + "," +
                  string(lv-qty5) + "," + string(lv-qty6) + "," +
                  string(lv-qty7) + "," + string(lv-qty8) + "," +
                  string(lv-qty9) + "," + string(lv-qty10) .

op-char-val2 = string(lv-qty11) + "," + string(lv-qty12) + "," +
                  string(lv-qty13) + "," + string(lv-qty14) + "," +
                  string(lv-qty15) + "," + string(lv-qty16) + "," +
                  string(lv-qty17) + "," + string(lv-qty18) + "," +
                  string(lv-qty19) + "," + string(lv-qty20) .


DEF VAR lv-yld AS DEC NO-UNDO.

   find est-qty where recid(est-qty) = ip-recid no-lock no-error.
   if avail est-qty then do:
      find est where est.company = est-qty.company and
                     est.est-no = est-qty.est-no
                     no-lock no-error.
                     
      ASSIGN lv-qty2 = est-qty.qty[2]
             lv-qty3 = est-qty.qty[3]
             lv-qty4 = est-qty.qty[4]
             lv-qty5 = est-qty.qty[5]
             lv-qty6 = est-qty.qty[6]
             lv-qty7 = est-qty.qty[7]
             lv-qty8 = est-qty.qty[8]
             lv-qty9 = est-qty.qty[9]
             lv-qty10 = est-qty.qty[10]
             lv-qty11 = est-qty.qty[11]
             lv-qty12 = est-qty.qty[12]
             lv-qty13 = est-qty.qty[13]
             lv-qty14 = est-qty.qty[14]
             lv-qty15 = est-qty.qty[15]
             lv-qty16 = est-qty.qty[16]
             lv-qty17 = est-qty.qty[17]
             lv-qty18 = est-qty.qty[18]
             lv-qty19 = est-qty.qty[19]
             lv-qty20 = est-qty.qty[20].
    
