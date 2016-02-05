/* ---------------------------------------------------- ce/print4.i 05/96 JLF */

def {1} var sh-wid          as dec no-undo.
def {1} var sh-len          as dec no-undo.
def {1} var fac-tot         as dec format ">>>>>>9.99" no-undo.
def {1} var fac-tot2        as dec format ">>>>>>9.99" no-undo.
def {1} var ord-cost        as dec format ">>>,>>9.99" no-undo.
def {1} var tt-tot          as dec format ">>>,>>9.99" no-undo.
def {1} var vmclean         as log no-undo.
def {1} var vmcl            as int no-undo.
def {1} var vprint          as log init no no-undo.

def {2} var qtty            as int extent 28 format ">>,>>>,>>>" no-undo.
def {2} var rels            as int extent 28 format ">>" no-undo.
def {2} var save-qty        as dec no-undo.
def {2} var xcal            as dec no-undo.
def {2} var lctr            as int no-undo.
def {2} var lin-count       as int no-undo.
def {2} var gsa-mat         as dec format ">>>,>>9.99" no-undo.
def {2} var gsa-lab         as dec format ">>>,>>9.99" no-undo.
def {2} var gsa-com         as dec format ">>>,>>9.99" no-undo.
def {2} var gsa-war         as dec format ">>>,>>9.99" no-undo.
def {2} var gsa-fm          as dec format ">>>,>>9.99" no-undo.
def {2} var outfile1        as char no-undo.
def {2} var outfile2        as char no-undo.
def {2} var outfile3        as char no-undo.
DEF {2} VAR outfile4        AS CHAR NO-UNDO.
def {2} var brd-l           like eb.len extent 4 no-undo.
def {2} var brd-w           like brd-l no-undo.
def {2} var brd-d           like brd-l no-undo.
def {2} var brd-sq          as dec format ">>>>9.9<<<<" extent 4 no-undo.
def {2} var brd-sf          as dec format ">>>>>9.9<<"  extent 4 no-undo.
def {2} var brd-wu          like brd-sq no-undo.
def {2} var b-wt            as dec no-undo.
def {2} var b-qty           as dec no-undo.
def {2} var b-cost          as dec format ">>>,>>9.99" no-undo.
def {2} var b-waste         as dec format ">>>,>>9.99" no-undo.
def {2} var b-tot           as dec format ">>>,>>9.99" no-undo.
def {2} var b-totw          as dec format ">>>,>>9.99" no-undo.
def {2} var b-msh           as dec format ">>>9.99" no-undo.
def {2} var b-uom           as char format "xxx" no-undo.
def {2} var i-qty           as dec extent 10 no-undo.
def {2} var i-cost          as dec extent 10 no-undo.
def {2} var adh-qty         as int extent 3 no-undo.
def {2} var dr-qty          as dec no-undo.
def {2} var dr-cost         as dec format ">>>,>>9.99" no-undo.
def {2} var dm-qty          as dec no-undo.
def {2} var dm-cost         as dec format ">>>,>>9.99" no-undo.
def {2} var fg-qty          as dec no-undo.
def {2} var fg-wt           as dec no-undo. /* tally fg weight MFG */
def {2} var f-qty           as dec extent 4 no-undo.
def {2} var f-cost          as dec extent 4 no-undo.
def {2} var t-qty           as dec no-undo.
def {2} var t-cost          as dec format ">>>,>>9.99" no-undo.
def {2} var c-qty           as dec no-undo.
def {2} var c-cost          as dec format ">>>,>>9.99" no-undo.
def {2} var p-qty           as dec no-undo.
def {2} var p-cost          as dec format ">>>,>>9.99" no-undo.
def {2} var s-qty           as dec extent 8 no-undo.
def {2} var s-cost          as dec extent 8 no-undo.
def {2} var dm-tot          as dec extent 5 no-undo.
def {2} var hand-tot        as dec no-undo.
def {2} var prep-lab        as dec format "->>>>>9.99" no-undo.
def {2} var prep-mat        as dec format "->>>>>9.99" no-undo.
def {2} var prep-add        as dec format "->>>>>9.99" no-undo.
def {2} var prep-atz        as dec format "->>>>>9.99" no-undo.
def {2} var prep-sim        as char no-undo.
def {2} var prep-tot        as dec format "->>>>>9.99" no-undo.
def {2} var tprep-lab       as dec format "->>>>>9.99" no-undo.
def {2} var tprep-mat       as dec format "->>>>>9.99" no-undo.
def {2} var tprep-add       as dec format "->>>>>9.99" no-undo.
def {2} var tprep-atz       as dec format "->>>>>9.99" no-undo.
def {2} var tprep-sim       as char no-undo.
def {2} var tprep-tot       as dec format "->>>>>9.99" no-undo.
def {2} var op-tot          as dec extent 7 no-undo.
def {2} var mis-tot         as dec extent 6 no-undo.
def {2} var tt-blk          as dec no-undo.           /* total blank qty all sheets */
def {2} var r-spo$          as dec extent 99 no-undo.
def {2} var r-spo           as dec extent 99 no-undo.
def {2} var spo             as dec no-undo.
def {2} var ctrl            as dec extent 20 no-undo.
def {2} var ctrl2           as dec extent 20 no-undo.
def {2} var fr-tot          as dec format "->>>>>9.99" no-undo.
def {2} var v-2             as log init no no-undo.
def {2} var t-shtfrm        as dec extent 99 no-undo.  /* total # sheets per form    */
def {2} var t-blksht        as int extent 99 no-undo. /* total # blanks per sheets  */
def {2} var t-blkqty        as dec extent 99 no-undo.  /* total blank qty per sheet  */
def {2} var vbsf            as dec no-undo.
def {2} var opsplit$        as dec extent 3 no-undo.
DEF {2} VAR v-op-qty        AS INT NO-UNDO.
DEF {2} VAR fg-rate-f       AS DEC NO-UNDO.
DEF {2} VAR rm-rate-f       AS DEC NO-UNDO.
DEF {2} VAR hand-pct-f      AS DEC NO-UNDO.
DEF {2} VAR v-do-all-forms-ink AS LOG NO-UNDO.
DEF {2} VAR v-board-cost-from-blank AS LOG NO-UNDO.
DEF {2} VAR v-print-cm      AS LOG NO-UNDO INIT TRUE.
DEF {2} VAR cm-disp         AS CHAR NO-UNDO.
DEF {2} VAR cmah-disp       AS CHAR NO-UNDO.
DEF {2} VAR cmoh-disp       AS CHAR NO-UNDO.
DEF {2} VAR cm%-disp        AS CHAR NO-UNDO.

def var save-lock           as log no-undo.
def var do-gsa              as log init no no-undo.
def var do-speed            as log init yes no-undo.
def var do-mr               as log init yes no-undo.
def var do-freight          as log init no no-undo.
def var do-split            as log init yes no-undo.
def var cust-ad             as char format "x(30)" extent 4 no-undo.
def var ship-ad             like cust-ad no-undo.
def var dsc                 as char format "x(22)" extent 2 no-undo.
def var sizcol              as char format "x(20)" extent 2 no-undo.
def var stypart             as char format "x(20)" extent 2 no-undo.
def var oprun               as dec no-undo.
def var opmr$               like oprun no-undo.
def var oprun$              like oprun no-undo.
def var optot$              as dec format ">>>,>>9.99" no-undo.
def var vhld                as int no-undo.
def var vmcl-desc           as char no-undo.
def var vmcl-cost           as dec format ">>>,>>9.99" no-undo.
def var v-cnt               as int no-undo.
def var r-spoil             as dec no-undo.
def var tr-tot              as dec format ">>>,>>9.99" no-undo.

DEF TEMP-TABLE tt-all-forms-ink NO-UNDO
    FIELD i-code AS CHAR
    FIELD i-qty AS DEC
    INDEX i-code i-code.

def {1} temp-table mclean     no-undo
        field    rec-type   as char
        field    form-no    like ef.form-no
        field    descr      as char format "x(30)"
        field    cost       as dec  format "->>,>>9.99" extent 28
        FIELD    total-field AS INT.

def {1} temp-table wprobe     no-undo
        field    line       like probe.line
        field    bsf        as dec.

{ce/print4a.i "{1}"}

def {2} temp-table kli NO-UNDO
        field    cust-no    like cust.cust-no
        field    cust-add   as char format "x(30)" extent 4
        field    ship-add   as char format "x(30)" extent 4
        field    sman       as char format "x(3)"
        field    sname      as char format "x(20)"
        field    rec-type   as char
        field    form-no    like ef.form-no
        field    descr      as char format "x(30)"
        field    cost       as dec  format "->>,>>9.99" extent 28.
 
def {2} temp-table ink NO-UNDO
        field    id         as ch
        field    snum       as int
        field    bnum       as int
        field    i-code     as char format "x(10)"
        field    i-dscr     as char format "x(19)"
        field    i-%        as int format ">>9"
        field    i-qty      as dec format ">>>9.99"
        field    i-cost     as dec format ">>>,>>9.99".

def {2} temp-table flm NO-UNDO
        field    id         as char
        field    snum       as int format "9"
        field    bnum       as int format "99"
        field    i-no       like item.i-no
        field    dscr       like item.est-dscr
        field    qty        as dec
        field    uom        as char format "x(3)"
        field    cosm       as dec format ">>>9.99"
        field    cost       as dec format ">>>,>>9.99".

def {2} temp-table cas NO-UNDO
        field    typ        as int
        field    id         as char
        field    snum       as int
        field    bnum       as int
        field    ino        like item.i-no
        field    dscr       like item.est-dscr
        field    t-qty      as int
        field    qty        as int
        field    cosm       as dec format ">>>9.99"
        field    cost       as dec format ">>>,>>9.99".

def {2} temp-table car NO-UNDO
        field    id         as char
        field    snum       as int
        field    bnum       as int
        field    carrier    like carrier.carrier
        field    dscr       like item.est-dscr
        field    qty        as int
        field    rate       as dec format ">>>9.99"
        field    cost       as dec format ">>>,>>9.99"
        FIELD    msf        AS DEC.

def {1} temp-table brd NO-UNDO
        field    form-no    like ef.form-no
        field    blank-no   like eb.blank-no
        field    i-no       like e-item.i-no
        field    qty        as dec
        field    qty-mr     like job-mat.qty-mr
        field    qty-wst    like job-mat.qty-wst
        field    qty-uom    as char format "x(3)"
        field    sc-uom     as char format "x(3)"
        field    cost       as dec
        field    cost-m     as dec format ">>>>>9.99"
        field    dscr       as char format "x(20)"
        field    basis-w    like job-mat.basis-w
        field    len        like job-mat.len
        field    wid        like job-mat.wid
        FIELD    amount AS DEC .

def {1} temp-table op NO-UNDO
        field    form-no    like ef.form-no
        field    blank-no   like eb.blank-no
        field    i-no       like e-item.i-no
        field    i-name     as char format "x(30)"
        field    dept       like job-mch.dept
        field    line       like job-mch.line
        field    m-code     like job-mch.m-code
        field    mr-fixoh   like job-mch.mr-fixoh
        field    mr-hr      like job-mch.mr-hr
        field    mr-rate    like job-mch.mr-rate
        field    mr-varoh   like job-mch.mr-varoh
        field    mr-waste   like job-mch.mr-waste
        field    pass       like est-op.op-pass
        field    run-hr     as dec
        field    speed      like est-op.op-speed
        field    wst-prct   as deci format ">>9.99"
        field    run-qty    like job-mch.run-qty
        FIELD opmr AS DEC
        FIELD oprun AS DEC
        FIELD optot AS DEC.

def {1} temp-table xprep NO-UNDO
        field    frm        like job-prep.frm
        field    blank-no   like job-prep.blank-no
        field    code       like job-prep.code
        field    ml         like job-prep.ml
        field    cost-m     like job-prep.cost-m format ">>>>>9.99"
        field    std-cost   like job-prep.std-cost
        field    qty        like job-prep.qty
        field    simon      like job-prep.simon
        FIELD amount AS DEC
        FIELD mat AS DEC
        FIELD lab AS DEC
        .

DEFINE {1} TEMP-TABLE tt-prep-sep NO-UNDO
   FIELD CODE AS CHAR
   FIELD form-no AS INT
   FIELD blank-no AS INT
   FIELD item-name AS CHAR
   FIELD sep-cost AS DEC
   FIELD misc AS LOG
   INDEX form-no form-no blank-no.

def buffer bmclean for mclean.
DEF BUFFER recalc-mr FOR reftable.
DEF BUFFER op-lock FOR reftable.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
