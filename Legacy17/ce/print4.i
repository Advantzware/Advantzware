/* ---------------------------------------------------- ce/print4.i 05/96 JLF */

DEFINE {1} VARIABLE sh-wid                  AS DECIMAL   NO-UNDO.
DEFINE {1} VARIABLE sh-len                  AS DECIMAL   NO-UNDO.
DEFINE {1} VARIABLE fac-tot                 AS DECIMAL   FORMAT ">>>>>>9.99" NO-UNDO.
DEFINE {1} VARIABLE fac-tot2                AS DECIMAL   FORMAT ">>>>>>9.99" NO-UNDO.
DEFINE {1} VARIABLE ord-cost                AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {1} VARIABLE tt-tot                  AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {1} VARIABLE vmclean                 AS LOG       NO-UNDO.
DEFINE {1} VARIABLE vmcl                    AS INTEGER   NO-UNDO.
DEFINE {1} VARIABLE vprint                  AS LOG       INIT NO NO-UNDO.

DEFINE {2} VARIABLE qtty                    AS INTEGER   EXTENT 28 FORMAT ">>,>>>,>>>" NO-UNDO.
DEFINE {2} VARIABLE rels                    AS INTEGER   EXTENT 28 FORMAT ">>" NO-UNDO.
DEFINE {2} VARIABLE save-qty                AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE xcal                    AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE lctr                    AS INTEGER   NO-UNDO.
DEFINE {2} VARIABLE lin-count               AS INTEGER   NO-UNDO.
DEFINE {2} VARIABLE gsa-mat                 AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE gsa-lab                 AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE gsa-com                 AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE gsa-war                 AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE gsa-fm                  AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE outfile1                AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE outfile2                AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE outfile3                AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE outfile4                AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE brd-l                   LIKE eb.len EXTENT 4 NO-UNDO.
DEFINE {2} VARIABLE brd-w                   LIKE brd-l NO-UNDO.
DEFINE {2} VARIABLE brd-d                   LIKE brd-l NO-UNDO.
DEFINE {2} VARIABLE brd-sq                  AS DECIMAL   FORMAT ">>>>9.9<<<<" EXTENT 4 NO-UNDO.
DEFINE {2} VARIABLE brd-sf                  AS DECIMAL   FORMAT ">>>>>9.9<<" EXTENT 4 NO-UNDO.
DEFINE {2} VARIABLE brd-wu                  LIKE brd-sq NO-UNDO.
DEFINE {2} VARIABLE b-wt                    AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE b-qty                   AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE b-cost                  AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE b-waste                 AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE b-tot                   AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE b-totw                  AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE b-msh                   AS DECIMAL   FORMAT ">>>9.99" NO-UNDO.
DEFINE {2} VARIABLE b-uom                   AS CHARACTER FORMAT "xxx" NO-UNDO.
DEFINE {2} VARIABLE i-qty                   AS DECIMAL   EXTENT 10 NO-UNDO.
DEFINE {2} VARIABLE i-cost                  AS DECIMAL   EXTENT 10 NO-UNDO.
DEFINE {2} VARIABLE adh-qty                 AS INTEGER   EXTENT 3 NO-UNDO.
DEFINE {2} VARIABLE dr-qty                  AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE dr-cost                 AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE dm-qty                  AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE dm-cost                 AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE fg-qty                  AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE fg-wt                   AS DECIMAL   NO-UNDO. /* tally fg weight MFG */
DEFINE {2} VARIABLE f-qty                   AS DECIMAL   EXTENT 4 NO-UNDO.
DEFINE {2} VARIABLE f-cost                  AS DECIMAL   EXTENT 4 NO-UNDO.
DEFINE {2} VARIABLE t-qty                   AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE t-cost                  AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE c-qty                   AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE c-cost                  AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE p-qty                   AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE p-cost                  AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE {2} VARIABLE s-qty                   AS DECIMAL   EXTENT 8 NO-UNDO.
DEFINE {2} VARIABLE s-cost                  AS DECIMAL   EXTENT 8 NO-UNDO.
DEFINE {2} VARIABLE dm-tot                  AS DECIMAL   EXTENT 5 NO-UNDO.
DEFINE {2} VARIABLE hand-tot                AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE prep-lab                AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE prep-mat                AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE prep-add                AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE prep-atz                AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE prep-sim                AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE prep-tot                AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE tprep-lab               AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE tprep-mat               AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE tprep-add               AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE tprep-atz               AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE tprep-sim               AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE tprep-tot               AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE op-tot                  AS DECIMAL   EXTENT 10 NO-UNDO.
DEFINE {2} VARIABLE mis-tot                 AS DECIMAL   EXTENT 6 NO-UNDO.
DEFINE {2} VARIABLE tt-blk                  AS DECIMAL   NO-UNDO.           /* total blank qty all sheets */
DEFINE {2} VARIABLE r-spo$                  AS DECIMAL   EXTENT 99 NO-UNDO.
DEFINE {2} VARIABLE r-spo                   AS DECIMAL   EXTENT 99 NO-UNDO.
DEFINE {2} VARIABLE spo                     AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE ctrl                    AS DECIMAL   EXTENT 20 NO-UNDO.
DEFINE {2} VARIABLE ctrl2                   AS DECIMAL   EXTENT 20 NO-UNDO.
DEFINE {2} VARIABLE fr-tot                  AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE {2} VARIABLE v-2                     AS LOG       INIT NO NO-UNDO.
DEFINE {2} VARIABLE t-shtfrm                AS DECIMAL   EXTENT 99 NO-UNDO.  /* total # sheets per form    */
DEFINE {2} VARIABLE t-blksht                AS INTEGER   EXTENT 99 NO-UNDO. /* total # blanks per sheets  */
DEFINE {2} VARIABLE t-blkqty                AS DECIMAL   EXTENT 99 NO-UNDO.  /* total blank qty per sheet  */
DEFINE {2} VARIABLE vbsf                    AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE opsplit$                AS DECIMAL   EXTENT 3 NO-UNDO.
DEFINE {2} VARIABLE v-op-qty                AS INTEGER   NO-UNDO.
DEFINE {2} VARIABLE fg-rate-f               AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE rm-rate-f               AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE hand-pct-f              AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE v-do-all-forms-ink      AS LOG       NO-UNDO.
DEFINE {2} VARIABLE v-board-cost-from-blank AS LOG       NO-UNDO.
DEFINE {2} VARIABLE v-print-cm              AS LOG       NO-UNDO INIT TRUE.
DEFINE {2} VARIABLE cm-disp                 AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE cmah-disp               AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE cmoh-disp               AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE cm%-disp                AS CHARACTER NO-UNDO.
DEFINE {2} VARIABLE dTotalManHrs            AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE dMCostToExcludeMisc     AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE dMPriceToAddMisc        AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE dMCostToExcludePrep     AS DECIMAL   NO-UNDO.
DEFINE {2} VARIABLE dMPriceToAddPrep        AS DECIMAL   NO-UNDO.

DEFINE     VARIABLE save-lock               AS LOG       NO-UNDO.
DEFINE     VARIABLE do-gsa                  AS LOG       INIT NO NO-UNDO.
DEFINE     VARIABLE do-speed                AS LOG       INIT YES NO-UNDO.
DEFINE     VARIABLE do-mr                   AS LOG       INIT YES NO-UNDO.
DEFINE     VARIABLE do-freight              AS LOG       INIT NO NO-UNDO.
DEFINE     VARIABLE do-split                AS LOG       INIT YES NO-UNDO.
DEFINE     VARIABLE cust-ad                 AS CHARACTER FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE     VARIABLE ship-ad                 LIKE cust-ad NO-UNDO.
DEFINE     VARIABLE dsc                     AS CHARACTER FORMAT "x(22)" EXTENT 2 NO-UNDO.
DEFINE     VARIABLE sizcol                  AS CHARACTER FORMAT "x(20)" EXTENT 2 NO-UNDO.
DEFINE     VARIABLE stypart                 AS CHARACTER FORMAT "x(20)" EXTENT 2 NO-UNDO.
DEFINE     VARIABLE oprun                   AS DECIMAL   NO-UNDO.
DEFINE     VARIABLE opmr$                   LIKE oprun NO-UNDO.
DEFINE     VARIABLE oprun$                  LIKE oprun NO-UNDO.
DEFINE     VARIABLE optot$                  AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE     VARIABLE vhld                    AS INTEGER   NO-UNDO.
DEFINE     VARIABLE vmcl-desc               AS CHARACTER NO-UNDO.
DEFINE     VARIABLE vmcl-cost               AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.
DEFINE     VARIABLE v-cnt                   AS INTEGER   NO-UNDO.
DEFINE     VARIABLE r-spoil                 AS DECIMAL   NO-UNDO.
DEFINE     VARIABLE tr-tot                  AS DECIMAL   FORMAT ">>>,>>9.99" NO-UNDO.

DEFINE TEMP-TABLE tt-all-forms-ink NO-UNDO
    FIELD i-code AS CHARACTER
    FIELD i-qty  AS DECIMAL
    INDEX i-code i-code.

DEFINE {1} TEMP-TABLE mclean NO-UNDO
    FIELD rec-type    AS CHARACTER
    FIELD form-no     LIKE ef.form-no
    FIELD descr       AS CHARACTER FORMAT "x(30)"
    FIELD cost        AS DECIMAL   FORMAT "->>,>>9.99" EXTENT 28
    FIELD total-field AS INTEGER.

DEFINE {1} TEMP-TABLE wprobe NO-UNDO
    FIELD line LIKE probe.line
    FIELD bsf  AS DECIMAL.

{ce/print4a.i "{1}"}

DEFINE {2} TEMP-TABLE kli NO-UNDO
    FIELD cust-no  LIKE cust.cust-no
    FIELD cust-add AS CHARACTER FORMAT "x(30)" EXTENT 4
    FIELD ship-add AS CHARACTER FORMAT "x(30)" EXTENT 4
    FIELD sman     AS CHARACTER FORMAT "x(3)"
    FIELD sname    AS CHARACTER FORMAT "x(20)"
    FIELD rec-type AS CHARACTER
    FIELD form-no  LIKE ef.form-no
    FIELD descr    AS CHARACTER FORMAT "x(30)"
    FIELD cost     AS DECIMAL   FORMAT "->>,>>9.99" EXTENT 28.
 
DEFINE {2} TEMP-TABLE ink NO-UNDO
    FIELD id     AS ch
    FIELD snum   AS INTEGER
    FIELD bnum   AS INTEGER
    FIELD i-code AS CHARACTER FORMAT "x(10)"
    FIELD i-dscr AS CHARACTER FORMAT "x(19)"
    FIELD i-%    AS INTEGER   FORMAT ">>9"
    FIELD i-qty  AS DECIMAL   FORMAT ">>>9.99"
    FIELD i-cost AS DECIMAL   FORMAT ">>>,>>9.99".

DEFINE {2} TEMP-TABLE flm NO-UNDO
    FIELD id   AS CHARACTER
    FIELD snum AS INTEGER   FORMAT "9"
    FIELD bnum AS INTEGER   FORMAT "99"
    FIELD i-no LIKE item.i-no
    FIELD dscr LIKE item.est-dscr
    FIELD qty  AS DECIMAL
    FIELD uom  AS CHARACTER FORMAT "x(3)"
    FIELD cosm AS DECIMAL   FORMAT ">>>9.99"
    FIELD cost AS DECIMAL   FORMAT ">>>,>>9.99".

DEFINE {2} TEMP-TABLE cas NO-UNDO
    FIELD typ   AS INTEGER
    FIELD id    AS CHARACTER
    FIELD snum  AS INTEGER
    FIELD bnum  AS INTEGER
    FIELD ino   LIKE item.i-no
    FIELD dscr  LIKE item.est-dscr
    FIELD t-qty AS INTEGER
    FIELD qty   AS INTEGER
    FIELD cosm  AS DECIMAL   FORMAT ">>>9.99"
    FIELD cost  AS DECIMAL   FORMAT ">>>,>>9.99".

DEFINE {2} TEMP-TABLE car NO-UNDO
    FIELD id      AS CHARACTER
    FIELD snum    AS INTEGER
    FIELD bnum    AS INTEGER
    FIELD carrier LIKE carrier.carrier
    FIELD dscr    LIKE item.est-dscr
    FIELD qty     AS INTEGER
    FIELD rate    AS DECIMAL   FORMAT ">>>9.99"
    FIELD cost    AS DECIMAL   FORMAT ">>>,>>9.99"
    FIELD msf     AS DECIMAL.

DEFINE {1} TEMP-TABLE brd NO-UNDO
    FIELD form-no  LIKE ef.form-no
    FIELD blank-no LIKE eb.blank-no
    FIELD i-no     LIKE e-item.i-no
    FIELD qty      AS DECIMAL
    FIELD qty-mr   LIKE job-mat.qty-mr
    FIELD qty-wst  LIKE job-mat.qty-wst
    FIELD qty-uom  AS CHARACTER FORMAT "x(3)"
    FIELD sc-uom   AS CHARACTER FORMAT "x(3)"
    FIELD cost     AS DECIMAL
    FIELD cost-m   AS DECIMAL   FORMAT ">>>>>9.99"
    FIELD dscr     AS CHARACTER FORMAT "x(20)"
    FIELD basis-w  LIKE job-mat.basis-w
    FIELD len      LIKE job-mat.len
    FIELD wid      LIKE job-mat.wid
    FIELD amount   AS DECIMAL .

DEFINE {1} TEMP-TABLE op NO-UNDO
    FIELD form-no   LIKE ef.form-no
    FIELD blank-no  LIKE eb.blank-no
    FIELD i-no      LIKE e-item.i-no
    FIELD i-name    AS CHARACTER FORMAT "x(30)"
    FIELD dept      LIKE job-mch.dept
    FIELD line      LIKE job-mch.line
    FIELD m-code    LIKE job-mch.m-code
    FIELD mr-fixoh  LIKE job-mch.mr-fixoh
    FIELD mr-hr     LIKE job-mch.mr-hr
    FIELD mr-rate   LIKE job-mch.mr-rate
    FIELD mr-varoh  LIKE job-mch.mr-varoh
    FIELD mr-waste  LIKE job-mch.mr-waste
    FIELD pass      LIKE est-op.op-pass
    FIELD run-hr    AS DECIMAL
    FIELD run-fixoh LIKE job-mch.run-fixoh
    FIELD run-rate  LIKE job-mch.run-rate
    FIELD run-varoh LIKE job-mch.run-varoh
    FIELD speed     LIKE est-op.op-speed
    FIELD wst-prct  AS DECIMAL   FORMAT ">>9.99"
    FIELD run-qty   LIKE job-mch.run-qty
    FIELD opmr      AS DECIMAL
    FIELD oprun     AS DECIMAL
    FIELD optot     AS DECIMAL
    FIELD rec_key   AS CHARACTER .

DEFINE {1} TEMP-TABLE xprep NO-UNDO
    FIELD frm      LIKE job-prep.frm
    FIELD blank-no LIKE job-prep.blank-no
    FIELD code     LIKE job-prep.code
    FIELD ml       LIKE job-prep.ml
    FIELD cost-m   LIKE job-prep.cost-m FORMAT ">>>>>9.99"
    FIELD std-cost LIKE job-prep.std-cost
    FIELD qty      LIKE job-prep.qty
    FIELD simon    LIKE job-prep.simon
    FIELD amount   AS DECIMAL
    FIELD mat      AS DECIMAL
    FIELD lab      AS DECIMAL
    .

DEFINE {1} TEMP-TABLE tt-prep-sep NO-UNDO
    FIELD CODE      AS CHARACTER
    FIELD form-no   AS INTEGER
    FIELD blank-no  AS INTEGER
    FIELD item-name AS CHARACTER
    FIELD sep-cost  AS DECIMAL
    FIELD misc      AS LOG
    INDEX form-no form-no blank-no.

DEFINE BUFFER bmclean   FOR mclean.
DEFINE BUFFER recalc-mr FOR reftable.
DEFINE BUFFER op-lock   FOR reftable.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
