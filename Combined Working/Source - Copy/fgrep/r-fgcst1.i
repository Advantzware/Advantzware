/* -------------------------------------------------------------------------- */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG".                          */
/* -------------------------------------------------------------------------- */

DEF {1} SHARED VAR vdat AS DATE INIT TODAY FORMAT "99/99/9999" NO-UNDO.
DEF {1} SHARED VAR fcus LIKE itemfg.cust-no INIT "" NO-UNDO.
DEF {1} SHARED VAR tcus LIKE fcus INIT "zzzzzzzzzzzzzzz" NO-UNDO.
DEF {1} SHARED VAR v-loc LIKE fg-bin.loc EXTENT 2 INIT ["","zzzzz"] NO-UNDO.
DEF {1} SHARED VAR fino LIKE fg-bin.i-no INIT " " NO-UNDO.
DEF {1} SHARED VAR tino LIKE fg-bin.i-no INIT "zzzzzzzzzzzzzzz" NO-UNDO.
DEF {1} SHARED VAR fcat AS CHAR INIT " " NO-UNDO.
DEF {1} SHARED VAR tcat LIKE fcat INIT "zzzzzzzz" NO-UNDO.
DEF {1} SHARED VAR v-type AS ch FORMAT "!" INIT "A" NO-UNDO.
DEF {1} SHARED VAR v-sort-by-cust AS CHAR FORMAT "!" INIT "C" NO-UNDO.
DEF {1} SHARED VAR v-excel AS LOG NO-UNDO.

DEF {1} SHARED VAR v-tot-qty AS DEC FORMAT "->>>,>>>,>>9"  EXTENT 3 NO-UNDO.
DEF {1} SHARED VAR v-tot-msf AS DEC FORMAT "->>>,>>9.999"  EXTENT 3 NO-UNDO.
DEF {1} SHARED VAR v-tot-cst AS DEC FORMAT "->>>>>,>>9.99" EXTENT 3 NO-UNDO.
DEF {1} SHARED VAR v-tot-ext AS DEC FORMAT "->>>>>,>>9.99" EXTENT 3 NO-UNDO.
DEFINE {1} SHARED VARIABLE security-flag AS LOG NO-UNDO.

DEF {1} SHARED STREAM excel.

DEF VAR v-tot-per AS DEC FORMAT "->>>,>>9.99<<<" NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR v-cst AS DEC NO-UNDO.
DEF VAR v-msf AS DEC NO-UNDO.
DEF VAR v-ext AS DEC NO-UNDO.
DEF VAR v-password AS char FORMAT "x(16)" NO-UNDO.
DEF VAR lv-sell-price LIKE itemfg.sell-price NO-UNDO.
DEF VAR lv-sell-uom LIKE itemfg.sell-uom NO-UNDO.
DEF VAR lv-case-count LIKE itemfg.case-count NO-UNDO.

{fg/rep/tt-fgbin.i {1} SHARED}

FORM itemfg.cust-no                 COLUMN-LABEL "Customer"
     itemfg.i-no                    COLUMN-LABEL "FG Item#"
     itemfg.part-no                 COLUMN-LABEL "Cust Part#"
                                    FORMAT "x(15)"
     tt-fg-bin.loc                  COLUMN-LABEL "Whse"
     v-tot-qty[1]                   COLUMN-LABEL "Total Qty"
     v-tot-msf[1]                   COLUMN-LABEL "Total MSF"
     v-tot-cst[1]                   COLUMN-LABEL "Total Cost"
     v-tot-ext[1]                   COLUMN-LABEL "Total Sell Value"
     v-tot-per                      COLUMN-LABEL "$$$/MSF"
   WITH FRAME itemx NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 200.

DEF TEMP-TABLE tt-rdtlh NO-UNDO LIKE fg-rdtlh
    INDEX tt-rdtlh job-no job-no2 loc loc-bin trans-date r-no rec_key.

&SCOPED-DEFINE itemfg-index i-no job-no job-no2 loc loc-bin tag bin-cust-no
DEF {1} SHARED TEMP-TABLE tt-itemfg NO-UNDO
    FIELD row-id      AS   ROWID
    FIELD i-no        LIKE itemfg.i-no
    FIELD cust-no     LIKE itemfg.cust-no
    FIELD part-no     LIKE itemfg.part-no
    FIELD part-cust   AS   CHAR
    FIELD procat      LIKE itemfg.procat
    FIELD job-no      LIKE fg-rcpth.job-no
    FIELD job-no2     LIKE fg-rcpth.job-no2
    FIELD loc         LIKE fg-rdtlh.loc
    FIELD loc-bin     LIKE fg-rdtlh.loc-bin
    FIELD tag         LIKE fg-rdtlh.tag
    FIELD bin-cust-no LIKE fg-rdtlh.cust-no
    FIELD loc-bin-tag AS   CHAR
    INDEX i-no {&itemfg-index}
    INDEX cust-no cust-no {&itemfg-index}
    INDEX part-no part-cust {&itemfg-index}
    INDEX procat procat {&itemfg-index}
    INDEX loc-bin-tag loc-bin-tag {&itemfg-index}.

FIND fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

