/* ---------------------------------------------- fg/rep/fg-ibtg1.i 10/99 JLF */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG".                          */
/* -------------------------------------------------------------------------- */

def {1} {2} var vdat as date init today format "99/99/9999" no-undo.
def {1} {2} var fcus like itemfg.cust-no init "" no-undo.
def {1} {2} var tcus like fcus init "zzzzzzzzzzzzzzz" no-undo.
def {1} {2} var v-loc like fg-bin.loc extent 2 init ["","zzzzz"] no-undo.
def {1} {2} var v-loc-bin like fg-bin.loc-bin extent 2 init ["","zzzzzzzz"] no-undo.
def {1} {2} var fino like fg-bin.i-no init " " no-undo.
def {1} {2} var tino like fg-bin.i-no init "zzzzzzzzzzzzzzz" no-undo.
def {1} {2} var fcat as ch init " " no-undo.
def {1} {2} var tcat like fcat init "zzzzzzzz" no-undo.
def {1} {2} var v-type as ch format "!" init "A" no-undo.
def {1} {2} var v-sort-by-cust as char format "!" init "C" no-undo.
def {1} {2} var zbal as log format "Y/N" init yes no-undo.
def {1} {2} var v-custown as log format "Y/N" init no no-undo.
def {1} {2} var v-prt-c as log format "Y/N" init yes no-undo.
def {1} {2} var v-dl-mat as log format "Y/N" init "N" no-undo.
def {1} {2} var v-prt-p as log format "Y/N" init yes no-undo.
def {1} {2} var v-prt-cpn as log format "Y/N" init no no-undo.
def {1} {2} var v-prt-po as log format "Y/N" init no no-undo.
def {1} {2} var v-prt-arqty as log format "Y/N" init no no-undo.
def {1} {2} var v-po-type as char format "!" init "L" no-undo.
def {1} {2} var v-prt-msf as log format "Y/N" init no no-undo.
def {1} {2} var v-subt as log no-undo.
def {1} {2} var v-fgprice as log no-undo.
def {1} {2} var v-sets as log format "Y/N" init no no-undo.
def {1} {2} var v-rct-date as log format "Y/N" init no no-undo.
def {1} {2} var v-file as char no-undo.
def {1} {2} var v-excel as log no-undo.
def {1} {2} var v-runexcel as log no-undo.
def {1} {2} stream excel.
def {1} {2} var v-summ-bin as log format "Y/N" init NO no-undo.

def {1} {2} var v-tot-qty as dec format "->>>,>>>,>>9"  extent 3 no-undo.
def {1} {2} var v-tot-cst as dec format "->>>>>,>>9.99" extent 3 no-undo.
def {1} {2} var v-tot-ext as dec format "->>>>>,>>9.99" extent 3 no-undo.
def {1} {2} var v-tot-gsl as dec format "->>>>>,>>9.99" extent 3 no-undo.
def {1} {2} var v-tot-gsm as dec format "->>>>>,>>9.99" extent 3 no-undo.
def {1} {2} var v-po-no   like oe-ordl.po-no no-undo.
def {1} {2} var v-qoh-f as char no-undo.
def {1} {2} var vItemCount as int no-undo.

def var v-cost as dec format "->>>,>>9.99<<" no-undo.
def var v-cost1 as dec format "->>>>9.9<<<<" no-undo.
def var v-costl like fg-bin.std-lab-cost no-undo.
def var v-costm like fg-bin.std-mat-cost no-undo.
def var v-ext as dec format  "->>>,>>9.99" no-undo.
def var ftyp like itemfg.mat-type init " " no-undo.
def var ttyp like itemfg.mat-type init ">" no-undo.
def var v-job-no as char format "x(9)" no-undo.
def var v-first as log init no extent 2 no-undo.
def var v-prnt as log init no extent 2 no-undo.
def var v-qoh like fg-bin.qty no-undo.
def var v-arq like fg-bin.qty format "->,>>>,>>9" no-undo.
def var v-bin-arq like fg-bin.qty format "->,>>>,>>9" no-undo.
def var v-qoh-s AS CHAR no-undo.
def var v-tot-sum as dec format "->>>,>>9.99<<" no-undo.
def var v-ext-sum as dec format "->>>,>>9.99<<" no-undo.
def var v-gsl-sum as dec format "->>>,>>9.99<<" no-undo.
def var v-gsm-sum as dec format "->>>,>>9.99<<" no-undo.
def var v-all-sum as dec format "->>>,>>9.99<<" no-undo.
def var v-label1 as char format "x(8)" extent 4 no-undo.
def var v-label2 as char format "x(11)" extent 6 no-undo.
def var v-label3 as char format "x(10)" extent 3 no-undo.
def var v-procat like itemfg.procat no-undo.
def var v-password as char format "x(16)" no-undo.
def var v-bin as log no-undo.
DEF VAR lv-sell-price LIKE itemfg.sell-price NO-UNDO.
DEF VAR lv-sell-uom LIKE itemfg.sell-uom NO-UNDO.
DEF VAR lv-case-count LIKE itemfg.case-count NO-UNDO.
DEF VAR lv-rct-date AS DATE FORMAT "99/99/99" NO-UNDO.

def var v-binqty as dec no-undo.
def var v-qty as dec no-undo.
DEF VAR v-found-job AS LOG NO-UNDO.

DEF VAR  v-tot-bin-sum  AS INTE NO-UNDO.
DEF VAR  v-ext-bin-sum  AS DEC NO-UNDO.
DEF VAR  v-bin-qoh      AS INTE NO-UNDO.

{fg/rep/tt-fgbin.i {1} {2}}

form itemfg.i-no
     itemfg.i-name format "x(25)"
     tt-fg-bin.loc
     tt-fg-bin.loc-bin
     tt-fg-bin.cases SPACE(3)
     tt-fg-bin.case-count
     space(8) v-qoh-s     
   with frame itemx1 no-labels no-box down stream-io width 200.


form itemfg.cust-no
     itemfg.i-no
     tt-fg-bin.tag
     lv-rct-date
     itemfg.i-name format "x(25)"
     tt-fg-bin.loc
     tt-fg-bin.loc-bin
     v-job-no
     v-qoh-s
     tt-fg-bin.pur-uom 
     v-cost1
     v-tot-sum format "->>>,>>9.99<<" space(2)
     v-ext-sum format "->>>,>>9.99<<"
     v-po-no
     v-arq
   with frame itemx3 no-labels no-box down stream-io width 200.

form itemfg.cust-no
     itemfg.i-no
     tt-fg-bin.tag
     lv-rct-date
     itemfg.i-name format "x(25)"
     tt-fg-bin.loc
     tt-fg-bin.loc-bin
     v-job-no
     v-qoh-s
     tt-fg-bin.pur-uom 
     v-cost1
     v-tot-bin-sum format "->>>,>>9.99<<" space(2)
     v-ext-bin-sum format "->>>,>>9.99<<"
     v-po-no
     v-bin-arq
   with frame itemx33 no-labels no-box down stream-io width 200.




DEF TEMP-TABLE tt-rdtlh NO-UNDO LIKE fg-rdtlh    
    INDEX tt-rdtlh job-no job-no2 loc loc-bin trans-date r-no rec_key.

&SCOPED-DEFINE itemfg-index loc loc-bin i-no job-no job-no2 tag bin-cust-no
DEF {1} {2} TEMP-TABLE tt-itemfg NO-UNDO
    FIELD row-id      AS   ROWID
    FIELD i-no        LIKE itemfg.i-no
    FIELD cust-no     LIKE itemfg.cust-no
    FIELD part-no     LIKE itemfg.part-no
    FIELD part-cust   AS   CHAR
    FIELD procat      LIKE itemfg.procat
    FIELD job-no      LIKE fg-rcpth.job-no
    FIELD job-no2     LIKE fg-rcpth.job-no2
    FIELD loc         LIKE fg-rdtlh.loc
    FIELD loc-bin     LIKE fg-rdtlh.loc-bin
    FIELD tag         LIKE fg-rdtlh.tag
    FIELD bin-cust-no LIKE fg-rdtlh.cust-no
    FIELD loc-bin-tag AS   CHAR
    FIELD cases LIKE fg-bin.cases-unit
    FIELD case-count LIKE fg-bin.case-count
    FIELD RecordCount AS INT
    INDEX i-no {&itemfg-index}
    INDEX cust-no cust-no {&itemfg-index}
    INDEX part-no part-cust {&itemfg-index}
    INDEX procat procat {&itemfg-index}
    INDEX loc-bin-tag loc-bin-tag {&itemfg-index}.

find fg-ctrl where fg-ctrl.company eq cocode no-lock.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

