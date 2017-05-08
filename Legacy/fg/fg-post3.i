
DEF {1} SHARED VAR v-types     AS CHAR FORMAT "x(10)".
DEF {1} SHARED VAR b-post-date AS DATE INIT TODAY NO-UNDO.
DEF {1} SHARED VAR e-post-date AS DATE INIT TODAY NO-UNDO.
DEF {1} SHARED VAR v-pr-tots   AS LOG FORMAT "Y/N"  INIT NO NO-UNDO.
DEF {1} SHARED VAR v-cost-sell AS LOG FORMAT "Cost/Sell Value"
                                  INIT YES NO-UNDO.
DEF {1} SHARED VAR v-showfggl  AS LOG FORMAT "Y/N"  INIT NO NO-UNDO.
DEFINE VARIABLE  rFgBinRow AS ROWID NO-UNDO.
{oe/invwork.i {1}}

DEF {1} SHARED WORKFILE w-job
  FIELD job-no  LIKE job.job-no
  FIELD rec-id  AS RECID.
  
