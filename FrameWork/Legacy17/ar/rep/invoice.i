
def {1} shared var fcust like inv-head.cust-no.
def {1} shared var tcust like fcust init "zzzzzzzz".
def {1} shared var finv like inv-head.inv-no format ">>>>>>".
def {1} shared var tinv like finv init 999999.
def {1} shared var v-print     like ar-inv.printed.
def {1} shared var v-posted    as   log format "Y/N" init NO no-undo.
def {1} SHARED var v-print-fmt  as char no-undo format 'x'.
def {1} SHARED var v-print-head as log no-undo.
DEF {1} SHARED VAR v-lines-per-page AS INT NO-UNDO.
def {1} shared var v-term-id as CHAR NO-UNDO.
DEF {1} SHARED VAR v-print-dept AS LOG NO-UNDO.
DEF {1} SHARED VAR v-depts AS CHAR NO-UNDO.
def {1} shared var v-prntinst as log init yes.

def var v-last-page         as   int.
def var v-page-tot          as   int.
