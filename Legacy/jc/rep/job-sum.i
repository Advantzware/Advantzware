/* ----------------------------------------------- jc/rep/job-sum.i 05/99 JLF */
/* Job Summary Report                                                         */
/* -------------------------------------------------------------------------- */

def var save_id as recid.

def var v-stat  as   char format "!" init "O".
def var v-ind   as   char format "!" init "".
def var v-job-no like job.job-no extent 2 init [" ", "zzzzzz"] no-undo.
def var v-job-no2 like job.job-no2 extent 2 init [00, 99] no-undo.
def var v-date like job.start-date format "99/99/9999" extent 2
                                      init ["01/01/0001", "12/31/9999"] no-undo.
DEF VAR v-inv-tot-only AS LOG NO-UNDO.
DEF VAR v-incl-farmout AS LOG NO-UNDO.
def var v-mr-cost-var as dec no-undo.
def var v-run-cost-var as dec no-undo.
def var v-mr-prod-p as dec no-undo.
def var v-run-prod-p as dec no-undo.
def var v-mr-wst-var as dec no-undo.
def var v-run-wst-var as dec no-undo.
def var v-act-speed as dec no-undo.
def var v-dscr like dept.dscr no-undo.
def var v-cst-var as dec format ">,>>>,>>9-" no-undo.
def var v-prod-p as dec format ">>>>9.9-" no-undo.
def var v-sub-cst-var as dec format ">,>>>,>>9-" no-undo.
def var v-sub-prod-p as dec format ">>>>9.9-" no-undo.
def var v-prd-var as dec format ">,>>>,>>9-" no-undo.
def var v-t-est-hrs as dec format ">>>>9.99" no-undo.
def var v-t-act-hrs as dec format ">>>>9.9" no-undo.
def var v-t-est-cost as dec format ">,>>>,>>9" no-undo.
def var v-t-act-cost as dec format ">,>>>,>>9" no-undo.
def var v-sub-est-cost as dec format ">,>>>,>>9" no-undo.
def var v-sub-act-cost as dec format ">,>>>,>>9" no-undo.
def var v-act-spo as int format ">>>,>>9-" no-undo.
def var v-a-spo-p as dec format ">>9.9-" no-undo.
def var v-e-spo-p as dec format ">>9.9-" no-undo.
def var v-over-pct as dec format ">>>9.9"  no-undo.
def var v-avg-prc as dec format ">>>>>9.99" no-undo.
def var v-t-ord as int format ">>,>>>,>>9" no-undo.
def var v-t-prs as int format ">>,>>>,>>9" no-undo.
def var v-t-prod as int format ">>,>>>,>>9" no-undo.
DEF VAR v-t-inv-qty AS DEC NO-UNDO.
def var v-t-all as int format ">>,>>>,>>9" no-undo.
def var v-op-cost as dec no-undo.
def var v-ip-sc-uom like job-mat.sc-uom no-undo.

def var v-num-up as int no-undo.
def var v-t-bord as int no-undo.
def var v-t-ebspo as dec no-undo.
def var v-t-abspo as dec no-undo.
def var v-est-spo as int format ">>>,>>9-" no-undo.
def var v-t-est-spo as int format ">>>>>>>9-" no-undo.
def var v-blk-pct as dec no-undo.
def var v-cust like cust.name no-undo.
def var v-gt-est-hrs as dec no-undo.
def var v-gt-est-cst as dec no-undo.
def var v-gt-act-hrs as dec no-undo.
def var v-gt-act-cst as dec no-undo.
def var v-mr-cost as dec no-undo.
def var v-run-cost as dec no-undo.

def {1} shared var v-lab     as log                         no-undo.
def {1} shared var v-foh     as log                         no-undo.
def {1} shared var v-voh     as log                         no-undo.
def {1} shared var v-mch     as log init no                 no-undo.
def {1} shared var v-tot     as log init no                 no-undo.
def {1} shared var v-lab-mrk as dec format "->9.99"         no-undo.
def {1} shared var v-est     as log init yes                no-undo.
def {1} shared var v-sale    as dec format "->>,>>>,>>9.99" no-undo.
def {1} shared var v-mater   as dec format "->>,>>>,>>9.99" no-undo.
def {1} shared var v-prep    as dec format "->>,>>>,>>9.99" no-undo.
def {1} shared var v-labor   as dec format "->>,>>>,>>9.99" no-undo.
def {1} shared var v-lab-m   as dec format "->>,>>>,>>9.99" no-undo.
def {1} shared var v-comm    as dec format "->>,>>>,>>9.99" no-undo.
def {1} shared var v-frate   as dec format "->>,>>>,>>9.99" no-undo.
def {1} shared var v-total   as dec format "->>,>>>,>>9.99" no-undo.
def {1} shared var v-charge-prep    as log INIT NO                 no-undo.
def {1} shared var v-misc-prep      as dec format "->>,>>>,>>9.99" no-undo.
DEF {1} SHARED VAR v-mat-sort AS CHAR NO-UNDO.
DEF {1} SHARED VAR v-merge-mat AS LOG NO-UNDO.
def {1} shared var v-tot-mchg     as log                         no-undo.

{jc/rep/job-cls.i {1}}

def {1} shared workfile work-prep no-undo
   field code like misc-act.m-code
   field dscr like prep.dscr
   field est-cost as dec
   field act-cost as DEC 
   FIELD work-ml  AS CHAR.
   
def var v-qty-ord  like work-item.qty-ord no-undo.
def var v-press-1  like work-item.press-1 no-undo.
def var v-qty-prod like work-item.qty-prod no-undo.
def var v-qty-all  like work-item.qty-all no-undo.
def var v-ext-price as dec no-undo.
DEF BUFFER bf-work-mat FOR work-mat.

/* end ---------------------------------- copr. 1999  advanced software, inc. */
