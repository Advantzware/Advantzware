
def {1} shared var v-prt-box        as   log                        NO-UNDO.
def {1} shared var fcust            as   char init ""               NO-UNDO.
def {1} shared var tcust            like fcust init "zzzzzzzz"      NO-UNDO.
def {1} shared var fdept            as   char format "x(2)" init "" NO-UNDO.
def {1} shared var tdept            like fdept init "zz"            NO-UNDO.
def {1} shared var fquote           as   int format ">>>>9" init 0  NO-UNDO.
def {1} shared var tquote           like fquote init 99999          NO-UNDO.
def {1} shared var ch-inst          as   log init no                NO-UNDO.
def {1} shared var ch-note          as   log init yes               NO-UNDO.
def {1} shared var ch-multi         as   log init no                NO-UNDO.
def {1} shared var ch-sort          as   char format "!" init "E"   NO-UNDO.
def {1} shared var v-log            like sys-ctrl.log-fld           NO-UNDO.
def {1} shared var v-comm           as   log init no                NO-UNDO.
def {1} shared var v-term-id        as   CHAR                       NO-UNDO.
def {1} shared var v-print-fmt      as   CHAR                       NO-UNDO.
def {1} shared var v-lines-per-page as   INT                        NO-UNDO.
DEF {1} SHARED VAR v-notesPageSpan  AS   LOG                        NO-UNDO.
DEF {1} SHARED VAR v-boardDescription AS CHAR                       NO-UNDO.
DEF {1} SHARED VAR s-print-2nd-dscr AS   LOG                        NO-UNDO.
DEF {1} SHARED VAR s-print-comp     AS   LOG                        NO-UNDO.
DEF {1} SHARED VAR s-prt-quoimage   AS   LOG                        NO-UNDO.
def {1} shared var s-sep-page       as   log                        NO-UNDO.
def {1} shared var s-note-mode      as   CHAR                       NO-UNDO.
/* gdm - 04300907 */
DEF {1} SHARED VAR v-prt-shp2       AS   LOG                        NO-UNDO.
DEF {1} SHARED VAR v-terms          AS   LOG                        NO-UNDO.
DEF {1} SHARED VAR v-termfile       AS   CHAR FORMAT "x(35)"        NO-UNDO.
