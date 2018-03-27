/* ******************************************************* cecrep/jobpqpH2.i  */
/*  N-K = JOBCARDC - FACTORY TICKET FOR PQP                                   */
/* ************************************************************************** */

DEF {1} VAR save_id AS RECID NO-UNDO.

DEF {1} BUFFER xest     FOR est.
DEF {1} BUFFER xef      FOR ef.
DEF {1} BUFFER xeb      FOR eb.
DEF {1} BUFFER xoe-ord  FOR oe-ord.
DEF {1} BUFFER xoe-ordl FOR oe-ordl.
DEF {1} BUFFER xoe-rel  FOR oe-rel.
DEF {1} BUFFER xstyle   FOR style.
DEF {1} BUFFER xxprep   FOR prep.

DEF {1} VAR v-break         AS   LOG                                    NO-UNDO.
DEF {1} VAR v-job-prt       AS   CHAR FORMAT "x(9)"                     NO-UNDO.
DEF {1} VAR v-ord-no        AS   CHAR FORMAT "x(8)"                     NO-UNDO.
DEF {1} VAR v-ord-date      AS   CHAR FORMAT "x(8)"                     NO-UNDO.
DEF {1} VAR v-est-no        AS   CHAR FORMAT "x(6)"                     NO-UNDO.
DEF {1} VAR v-fg            AS   CHAR FORMAT "x(37)"                    NO-UNDO.
DEF {1} VAR v-due-date      AS   CHAR FORMAT "x(13)"                    NO-UNDO.

DEF {1} VAR v-joint-dscr    LIKE item.i-name                            NO-UNDO.
DEF {1} VAR v-cus           AS   CHAR FORMAT "x(39)" EXTENT 4           NO-UNDO.
DEF {1} VAR v-shp           LIKE v-cus                                  NO-UNDO.
DEF {1} VAR v-standards     AS   LOG                                    NO-UNDO.
DEF {1} VAR v-adders        AS   CHAR FORMAT "x(31)"                    NO-UNDO.
DEF {1} VAR v-sht-qty       AS   INT FORMAT ">,>>>,>>9"                 NO-UNDO.
DEF {1} VAR v-dc-qty        AS   INT FORMAT ">,>>>,>>>,>>9"             NO-UNDO.
DEF {1} VAR v-1st-dc        AS   log                                    NO-UNDO.
DEF {1} VAR v-out           AS   INT FORMAT ">9"                        NO-UNDO.
DEF {1} VAR v-outw          AS   INT FORMAT ">9"                        NO-UNDO.
DEF {1} VAR v-outl          AS   INT FORMAT ">9"                        NO-UNDO.
DEF {1} VAR v-upw           AS   INT FORMAT ">9"                        NO-UNDO.
DEF {1} VAR v-upl           AS   INT FORMAT ">9"                        NO-UNDO.
DEF {1} VAR v-letter        AS   CHAR FORMAT "x"                        NO-UNDO.
DEF {1} VAR v-form-code     AS   CHAR FORMAT "x(32)"                    NO-UNDO.
DEF {1} VAR v-form-dscr     AS   CHAR FORMAT "x(32)"                    NO-UNDO.
DEF {1} VAR v-form-len      LIKE job-mat.len                            NO-UNDO.
DEF {1} VAR v-form-wid      LIKE job-mat.wid                            NO-UNDO.
DEF {1} VAR v-form-sqft     AS   DEC DECIMALS 3 FORMAT ">>9.9<<"        NO-UNDO.
DEF {1} VAR v-len-score     AS   CHAR FORMAT "x(27)"                    NO-UNDO.
DEF {1} VAR v-inst          AS   CHAR                                   NO-UNDO.
DEF {1} VAR v-ship          AS   CHAR                                   NO-UNDO.
DEF {1} VAR v-cas-pal       LIKE eb.cas-pal                             NO-UNDO.
DEF {1} VAR v-tr-cnt        LIKE eb.tr-cnt                              NO-UNDO.
DEF {1} VAR v-numstacks     AS   INT                                    NO-UNDO.
DEF {1} VAR v-stackcode     AS   CHAR                                   NO-UNDO.
DEF {1} VAR v-error         AS   LOG                                    NO-UNDO.
DEF {1} VAR v-line          AS   CHAR FORMAT "x(130)" EXTENT 8          NO-UNDO.
DEF {1} VAR v-form-hdr      AS   CHAR FORMAT "x(16)"                    NO-UNDO.
DEF {1} VAR v-net-shts      AS   LOG                                    NO-UNDO.
DEF {1} VAR v-iso           AS   CHAR FORMAT "x(14)"                    NO-UNDO.
DEF {1} VAR v-loc           AS   CHAR FORMAT "x(14)"                    NO-UNDO.
DEF {1} VAR v-loc-bin       AS   CHAR FORMAT "x(14)"                    NO-UNDO.
DEF {1} VAR v-set-hdr       AS   CHAR FORMAT "x(25)"                    NO-UNDO.
DEF {1} VAR v-board-code    AS   CHAR FORMAT "x(15)"                    NO-UNDO.
DEF {1} VAR v-board-dscr    AS   CHAR FORMAT "x(32)"                    NO-UNDO.
DEF {1} VAR v-form1         AS   CHAR FORMAT "x(3)"                     NO-UNDO.
DEF {1} VAR v-form2         AS   CHAR FORMAT "x(3)"                     NO-UNDO.

DEF {1} WORK-TABLE w-m NO-UNDo
  FIELD m-code LIKE mach.m-code
  FIELD dseq LIKE mach.d-seq
  FIELD dscr LIKE mach.m-dscr
  FIELD s-hr LIKE job-mch.mr-hr
  FIELD r-sp LIKE job-mch.speed
  FIELD r-hr LIKE job-mch.run-hr FORMAT ">>>9.99"
  .

DEF {1} WORK-TABLE w-i NO-UNDO
  FIELD i-code  AS CHAR FORMAT "x(10)"
  FIELD i-dscr  AS CHAR FORMAT "x(19)"
  FIELD i-qty   AS DEC  FORMAT ">>>9.99"
  FIELD i-code2 AS CHAR FORMAT "x(10)"
  FIELD i-dscr2 AS CHAR FORMAT "x(19)"
  FIELD i-qty2  AS DEC  FORMAT ">>>9.99".

DEF {1} WORK-TABLE w-ef NO-UNDO
  FIELD frm  LIKE ef.form-no.

FORMAT HEADER 
    ""
  WITH NO-BOX FRAME head NO-LABELS WIDTH 132  STREAM-IO.



/* end ---------------------------------- copr. 1997  advanced software, inc. */
