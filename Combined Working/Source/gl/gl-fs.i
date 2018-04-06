
def {1} shared var c-t-dscr   as ch init
["Curr Per,Curr QTD,Curr YTD,Budg Per,Budg QTD,Budg YTD,L.Y. Per,L.Y. QTD,L.Y. YTD,Variance"].

def {1} shared var consolidate as log no-undo init no.
def {1} shared var company-list as char format "x(40)".
def {1} shared var prt-detail as ch format "x(200)".
def {1} shared var v-rpt       like gl-rpt.rpt.
def {1} shared var tot as dec format "->>>,>>>,>>>,>>9.99" extent 14.
def {1} shared var tot1 LIKE tot.
def {1} shared var tot2 LIKE tot.
def {1} shared var tot3 LIKE tot.  /* to hold total sales for pct */
def {1} shared var pp as int.
def {1} shared var pre-close as log label "Pre Close period".
def {1} shared var tr-per like period.pnum.
def {1} shared var v-vcol     as int format "9"    extent 6.
def {1} shared var v-ct       as ch format "x(13)" extent 14.
def {1} shared var v-per      as log format "Y/N" extent 14 initial false.
def {1} shared var jj as int.
def {1} shared var stot as dec extent 168 format "->>>,>>>,>>9.99".
def {1} shared var v-d-wid    as int format "99".
def {1} shared var v-no-col   as int format "9".
def {1} shared var subac as int format ">>9".
def {1} shared var subac-lvl as int format "9".
def {1} shared var fsubac as int format ">>9" init 0.
def {1} shared var tsubac as int format ">>9" init 999.
def {1} shared var aclevel as int.
def {1} shared var skip_zero as logical no-undo init yes.
def {1} shared var supp_zero as logical no-undo init yes.
def {1} shared var v-onevar as int init 0 no-undo.
def {1} shared var v-twovar as int init 0 no-undo.
def {1} shared VAR all-per AS LOG NO-UNDO.
def {1} shared VAR all-per-tot AS DEC EXTENT 14 NO-UNDO.
def {1} shared VAR all-per-tot3 LIKE all-per-tot NO-UNDO.
def {1} shared VAR tot-format AS CHAR INIT "->>>,>>>,>>9.99" NO-UNDO.
def {1} shared VAR pct-format AS CHAR INIT "->>,>>>,>>9.99%" NO-UNDO.
def {1} shared VAR pct-formats AS CHAR INIT "->>>9.99%" NO-UNDO.
def {1} shared VAR sul-format AS CHAR INIT " --------------" NO-UNDO.
def {1} shared VAR sul-formats AS CHAR INIT " --------" NO-UNDO.
def {1} shared VAR dul-format AS CHAR INIT " ==============" NO-UNDO.
def {1} shared VAR dul-formats AS CHAR INIT " ========" NO-UNDO.
def {1} shared VAR udate AS DATE NO-UNDO.
def {1} shared VAR uperiod AS INT NO-UNDO.

def {1} shared var r-top1 as char format "x(200)".
def {1} shared var r-top2 as char format "x(200)".
def {1} shared var r-top3 as char format "x(200)".
def {1} shared var r-top4 as char format "x(200)".
def {1} shared var v-hdr      as char format "x(50)" extent 5.
def {1} shared var v-c-bs     as logical format "Y/N" initial false.
def {1} shared var v-p-w      as integer format "999" initial 80.
def {1} shared var v-page-length as integer format "z9".
def {1} shared var v-sub as ch format "x(13)" extent 12.
def {1} shared var v-col-used as int format "999".
def {1} shared var v-ch       as char format "X(13)" extent 14.

def {1} shared var c-t-no     as int format "99".
def {1} shared var v-rpt-name as ch format "x(50)".
def {1} shared var v-sper as log format "Y/N" extent 12.
def {1} shared var v-shun as log format "Y/N" extent 12.
def {1} shared var v-sdol as log format "Y/N" extent 12.
def {1} shared var rtype as ch init "".
def {1} shared var save_id as recid.

DEF {1} shared VAR fil_id AS RECID NO-UNDO.
DEF {1} shared VAR col-warn AS cha NO-UNDO.
DEF {1} shared VAR col-bg AS cha NO-UNDO.
DEF {1} shared VAR col-input AS cha NO-UNDO.
DEF {1} SHARED VAR ll-acct# as LOG NO-UNDO.
