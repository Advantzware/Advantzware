/* ---------------------------------------------- cec/rep/jobmcga.i */
/* factory ticket                                                             */
/* -------------------------------------------------------------------------- */

def {1} var v-lines         as   int init 39                            no-undo.
def {1} var save_id as recid.

def {1} buffer xest     for est.
def {1} buffer xef      for ef.
def {1} buffer xeb      for eb.
def {1} buffer xoe-ord  for oe-ord.
def {1} buffer xoe-ordl for oe-ordl.
def {1} buffer xoe-rel  for oe-rel.
def {1} buffer xstyle   for style.
def {1} buffer xxprep   for prep.

def {1} var v-break         as   log                                    no-undo.
def {1} var v-job-prt       as   char format "x(9)"                     no-undo.
def {1} var v-ord-no        as   char format "x(8)"                     no-undo.
def {1} var v-ord-date      as   char format "x(10)"                     no-undo.
def {1} var v-est-no        as   char format "x(6)"                     no-undo.
def {1} var v-fg            as   char format "x(37)"                    no-undo.
def {1} var v-cp            as   char format "x(37)"                    no-undo.
def {1} var v-due-date      as   char format "x(13)"                    no-undo.

def {1} var v-joint-dscr    like item.i-name                            no-undo.
def {1} var v-cus           as   char format "x(30)" extent 4           no-undo.
def {1} var v-shp           like v-cus                                  no-undo.
def {1} var v-standards     as   log                                    no-undo.
def {1} var v-adders        as   char format "x(31)"                    no-undo.
def {1} var v-adders1       as   char format "x(31)"                    no-undo.
def {1} var v-adders2       as   char format "x(31)"                    no-undo.
def {1} var v-adders3       as   char format "x(31)"                    no-undo.
def {1} var v-sht-qty       as   int  format ">,>>>,>>9"                no-undo.
def {1} var v-dc-qty        as   int  format ">,>>>,>>>,>>9"            no-undo.
def {1} var v-1st-dc        as   log                                    no-undo.
def {1} var v-out           as   int  format ">9"                       no-undo.
def {1} var v-outw          as   int  format ">9"                       no-undo.
def {1} var v-outl          as   int  format ">9"                       no-undo.
def {1} var v-upw           as   int  format ">9"                       no-undo.
def {1} var v-upl           as   int  format ">9"                       no-undo.
def {1} var v-letter        as   char format "x"                        no-undo.
def {1} var v-form-code     as   char format "x(32)"                    no-undo.
def {1} var v-form-dscr     as   char format "x(32)"                    no-undo.
def {1} var v-form-len      like job-mat.len                            no-undo.
def {1} var v-form-wid      like job-mat.wid                            no-undo.
def {1} var v-form-sqft     as   dec decimals 3 format ">>9.9<<"        no-undo.
def {1} var v-len-score     as   char format "x(27)"                    no-undo.
def {1} var v-inst          as   char                                   no-undo.
def {1} var v-ship          as   char                                   no-undo.
def {1} var v-cas-pal       like eb.cas-pal                             no-undo.
def {1} var v-tr-cnt        like eb.tr-cnt                              no-undo.
def {1} var v-numstacks     as   int                                    no-undo.
def {1} var v-stackcode     as   char                                   no-undo.
def {1} var v-error         as   log                                    no-undo.
def {1} var v-line          as   char format "x(130)" extent 8          no-undo.
def {1} var v-form-hdr      as   char format "x(16)"                    no-undo.
def {1} var v-net-shts      as   log                                    no-undo.
def {1} var v-iso           as   char format "x(14)"                    no-undo.
def {1} var v-loc           as   char format "x(14)"                    no-undo.
def {1} var v-loc-bin       as   char format "x(14)"                    no-undo.
def {1} var v-set-hdr       as   char format "x(25)"                    no-undo.
def {1} var v-board-code    as   char format "x(15)"                    no-undo.
def {1} var v-board-dscr    as   char format "x(32)"                    no-undo.
DEF {1} VAR v-die1          LIKE eb.die-no                              NO-UNDO.
DEF {1} VAR v-die2          LIKE eb.die-no                              NO-UNDO.
def {1} workfile w-m no-undo
  FIELD m-code LIKE mach.m-code
  field dseq like mach.d-seq
  field dscr like mach.m-dscr
  field s-hr like job-mch.mr-hr
  field r-sp like job-mch.speed
  field r-hr like job-mch.run-hr format ">>>9.99".

def {1} workfile w-i no-undo
  field i-code as ch format "x(10)"
  field i-dscr as ch format "x(19)"
  field i-qty  as de format ">>>9.99"
  field i-code2 as ch format "x(10)"
  field i-dscr2 as ch format "x(19)"
  field i-qty2  as de format ">>>9.99".

def {1} workfile w-ef no-undo
  field frm  like ef.form-no.
IF NOT AVAIL xeb THEN
    FIND FIRST xeb NO-LOCK.
/*def {1} frame head. */

FORM HEADER
    "<=1><C2><B>Customer Name:</B>" 
       "<C37><B>Special Requirements:</B>" 
       "<C73><B>Estimate#:</B>" v-est-no SKIP
       "<C2>Contact:" v-contact FORMAT "x(30)"
       /*"<C37>Label/Perf Part #s:"  v-perf */
       "<C73>Order Qty Assd Sets:" v-set-qty SKIP
       "<C2>Tel:"  v-tel 
       /*"<C37>Boxed:" v-cas-no */
       "<C73>Min Qty Assd Sets:  " v-qty-1 SKIP
       "<C2>Fax:" v-fax 
       "<C37>Reverse Corrugation:" v-reversed
       "<C73>Max Qty Assd Sets:  " v-qty-2 skip
       "<C2>E-mail:" v-email FORMAT "x(32)"
       /*"<C37># of Load Tags" v-num-tags  */
       "<C73>Under/Overrun:"  lv-under-run format "x(7)" "," lv-over-run format "x(7)"  SKIP
       "<C2><R+.5><B>Long Strip Dies: <C44> Short Strip Dies <C85> 3rd Strip Dies</B>" SKIP 
       "<C1>Slot Hght:" v-slot-height1
            "<C21>Length:"     v-len
            "<C36.5>Slot Hght:"  v-slot-height2 
            "<C57>Length:"     v-wid
            "<C72.5>Slot Hght:"  v-slot-height2  
            "<C93>Length:" "<C95>" v-wid3          SKIP

       "<C1.9>Material:"      v-board-type-1 
            "<C21>Height:"    v-hight1
            "<C37.4>Material:"  v-board-type-2
            "<C57>Height:"    v-hight2
            "<C73.5>Material:"  v-board-type-3  
            "<C93>Height:" "<C95>" v-hight3        SKIP

       "<C2>" v-board-type-1-1
            "<C22>Strip:"     v-qty-set
            "<C36>"           v-board-type-2-1
            "<C58>Strip:"     v-qty-set2
            "<C74>"           v-board-type-3-1
            "<C93.8>Strip:"   v-qty-set3           SKIP

       "<C2.5>Tot MSF:"         v-board-size-1
            "<C22.1>Slots:"     v-slt1
            "<C38.4>Tot MSF:"   v-board-size-2
            "<C58>Slots:"       v-slt2
            "<C74.5>Tot MSF:"     v-board-size-3 
            "<C93.8>Slots:"       v-slt3             SKIP

       "<C6>Qty:"               v-board-qty-1
            "<C19.5>Interior:"    scr-in-cell-length 
            "<C41.5>Qty:"         v-board-qty-2
            "<C55.5>Interior:"  scr-in-cell-width
            "<C77.9>Qty:"       v-board-qty-3
            "<C89.4>Interior:"  scr-in-cell-width3 SKIP

       "<C23.5>End:"            scr-end-cell-l2 
            "<C59.5>End:"       scr-end-cell-w2
            "<C93.5>End:"       scr-end-cell-w3    SKIP
/*       "<C4>" v-dc-msetup[1] */
/*       "<C41.5>" v-dc-msetup[2]  */
       /* SKIP(1) */ 
       "<C2>" v-adders1
       "<C37.4>" v-adders2
       "<C93.8>" v-adders3                          SKIP

       "<C15><B>Cell Sizes:</B>" "<C45><B>Cell Sizes:</B>" "<C85><B>Cell Sizes:</B>" SKIP

       "<C1><R18.5><p7>1:" string(v-len-array1[1]) "<C9> 8:" string(v-len-array1[8]) "<C19>15:" STRING(v-len-array1[15]) "<C28>22:" STRING(v-len-array1[22]) SKIP
       "<C1><p7>2:" string(v-len-array1[2]) "<C9> 9:" STRING(v-len-array1[9]) "<C19>16:" STRING(v-len-array1[16]) "<C28>23:" STRING(v-len-array1[23]) SKIP
       "<C1><p7>3:" string(v-len-array1[3]) "<C9>10:" STRING(v-len-array1[10]) "<C19>17:" STRING(v-len-array1[17]) "<C28>24:" STRING(v-len-array1[24]) SKIP
       "<C1><p7>4:" string(v-len-array1[4]) "<C9>11:" STRING(v-len-array1[11]) "<C19>18:" STRING(v-len-array1[18]) "<C28>25:" STRING(v-len-array1[25]) SKIP
       "<C1><p7>5:" string(v-len-array1[5]) "<C9>12:" STRING(v-len-array1[12]) "<C19>19:" STRING(v-len-array1[19]) "<C28>26:" STRING(v-len-array1[26]) SKIP
       "<C1><p7>6:" string(v-len-array1[6]) "<C9>13:" STRING(v-len-array1[13]) "<C19>20:" STRING(v-len-array1[20]) "<C28>27:" STRING(v-len-array1[27]) SKIP
       "<C1><p7>7:" string(v-len-array1[7]) "<C9>14:" STRING(v-len-array1[14]) "<C19>21:" STRING(v-len-array1[21]) "<C28>28:" STRING(v-len-array1[28]) "<P10>" SKIP

       "<C37><R18.5><p7>1:" v-len-array2[1] "<C44> 8:" v-len-array2[8] "<C54>15:" v-len-array2[15] "<C63>22:" v-len-array2[22] SKIP
       "<C37><p7>2:" v-len-array2[2] "<C44> 9:" v-len-array2[9] "<C54>16:" v-len-array2[16] "<C63>23:" v-len-array2[23] SKIP
       "<C37><p7>3:" v-len-array2[3] "<C44>10:" v-len-array2[10] "<C54>17:" v-len-array2[17] "<C63>24:" v-len-array2[24] SKIP
       "<C37><p7>4:" v-len-array2[4] "<C44>11:" v-len-array2[11] "<C54>18:" v-len-array2[18] "<C63>25:" v-len-array2[25] SKIP
       "<C37><p7>5:" v-len-array2[5] "<C44>12:" v-len-array2[12] "<C54>19:" v-len-array2[19] "<C63>26:" v-len-array2[26] SKIP
       "<C37><p7>6:" v-len-array2[6] "<C44>13:" v-len-array2[13] "<C54>20:" v-len-array2[20] "<C63>27:" v-len-array2[27] SKIP
       "<C37><p7>7:" v-len-array2[7] "<C44>14:" v-len-array2[14] "<C54>21:" v-len-array2[21] "<C63>28:" v-len-array2[28] "<P10>" SKIP

       "<C72><R18.5><p7>1:" v-len-array3[1] "<C79> 8:" v-len-array3[8] "<C89>15:" v-len-array3[15] "<C97.5>22:" v-len-array3[22] SKIP
       "<C72><p7>2:" v-len-array3[2] "<C79> 9:" v-len-array3[9] "<C89>16:" v-len-array3[16] "<C97.5>23:" v-len-array3[23] SKIP
       "<C72><p7>3:" v-len-array3[3] "<C79>10:" v-len-array3[10] "<C89>17:" v-len-array3[17] "<C97.5>24:" v-len-array3[24] SKIP
       "<C72><p7>4:" v-len-array3[4] "<C79>11:" v-len-array3[11] "<C89>18:" v-len-array3[18] "<C97.5>25:" v-len-array3[25] SKIP
       "<C72><p7>5:" v-len-array3[5] "<C79>12:" v-len-array3[12] "<C89>19:" v-len-array3[19] "<C97.5>26:" v-len-array3[26] SKIP
       "<C72><p7>6:" v-len-array3[6] "<C79>13:" v-len-array3[13] "<C89>20:" v-len-array3[20] "<C97.5>27:" v-len-array3[27] SKIP
       "<C72><p7>7:" v-len-array3[7] "<C79>14:" v-len-array3[14] "<C89>21:" v-len-array3[21] "<C97.5>28:" v-len-array3[28] "<P10>" SKIP



/*
       "<C2><B>Board: Long"  "<C36>Board: Short</B>" SKIP
       "<C2>Type:" v-board-type-1 "<C36>Type:" v-board-type-2 SKIP
       "<C2>Total MSF:" v-board-size-1 "<C36>Total MSF:" v-board-size-2 SKIP
       "<C2>Qty:"  v-board-qty-1 "<C36>Qty:"  v-board-qty-2  SKIP
       "<C2>Due Date:"  v-due-date1 space(5) v-req-code1 "<C36>Due Date:" v-due-date2 space(5) v-req-code2 SKIP */
    with no-box frame head no-labels width 170 NO-ATTR-SPACE STREAM-IO.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
