/* ------------------------------------------------     jc/jcgl-sh.i 8/94 gb */
/* Job Costing - G/L Transaction Shared Variables                          */
/* -------------------------------------------------------------------------- */

def var wip-amt as dec no-undo.
def var raw-mat as dec no-undo.
def var ppv-amt as dec no-undo.
def var credits as dec no-undo.
def var debits as dec no-undo.
def var pct as dec no-undo.
def var std-run-hrs as dec no-undo.
def var std-mr-hrs as dec no-undo.
def var wip-lab as dec no-undo.
def var wip-voh as dec no-undo.
def var wip-foh as dec no-undo.
def var app-lab as dec no-undo.
def var app-voh as dec no-undo.
def var app-foh as dec no-undo.
def var var-lab as dec no-undo.
def var var-voh as dec no-undo.
def var var-foh as dec no-undo.
def var variance as dec no-undo.
def var v-err-desc as CHAR NO-UNDO.

def {1} shared var v-job-no  like job.job-no extent 2
                             initial [" ", "ZZZZZZ"] no-undo.
def {1} shared var v-job-no2 like job.job-no2 extent 2
                             initial [00, 99] no-undo.

def {1} shared TEMP-TABLE work-gl NO-UNDO
   field actnum like account.actnum
   field job like job.job
   field job-no like job.job-no
   field job-no2 like job.job-no2
   field err-desc as char format "x(100)"
   field debits as dec
   field credits as dec
   INDEX actnum actnum.

def {1} shared TEMP-TABLE work-gl-c NO-UNDO like work-gl.

def TEMP-TABLE w-work-gl NO-UNDO
   field job like job.job
   field job-no like job.job-no
   field job-no2 like job.job-no2.

def {1} shared TEMP-TABLE mch-srt-c NO-UNDO like mch-srt.
