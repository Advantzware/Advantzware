
def {1} shared var v-types     as char format "x(10)".
def {1} shared var b-post-date as date init today no-undo.
def {1} shared var e-post-date as date init today no-undo.
def {1} shared var v-pr-tots   as log format "Y/N"  init no no-undo.
def {1} shared var v-cost-sell as log format "Cost/Sell Value"
                                  init yes no-undo.
def {1} shared var v-showfggl  as log format "Y/N"  init no no-undo.

{oe/invwork.i {1}}

def {1} shared workfile w-job
  field job-no  like job.job-no
  field rec-id  as recid.
  
