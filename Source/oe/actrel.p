/* ---------------------------------------------------- oe/actrel.p  7/94 rd  */
/* order entry - Create actual releases from planned release line            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ip-recid AS RECID.
DEFINE INPUT-OUTPUT PARAMETER iocPrompt AS CHARACTER NO-UNDO.
{sys/inc/var.i shared}

DEFINE NEW SHARED VARIABLE out-recid      AS RECID NO-UNDO.
DEFINE SHARED     VARIABLE relh-recid     AS RECID NO-UNDO.
DEFINE SHARED     VARIABLE v-auto         AS LOG   NO-UNDO.

DEFINE            VARIABLE rOeRelhRow        AS ROWID NO-UNDO.


DEFINE BUFFER bf-rel FOR oe-rel.

{oe/chkordl.i}

{oe/relemail.i}

FIND oe-rel WHERE RECID(oe-rel) EQ ip-recid NO-LOCK NO-ERROR.

IF AVAILABLE oe-rel THEN 
DO:

    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ oe-rel.ord-no
          AND oe-ordl.i-no    EQ oe-rel.i-no
          AND oe-ordl.line    EQ oe-rel.line
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE oe-ordl THEN
        RETURN.

    RUN oe/chkordl.p (ROWID(oe-ordl)).

    FIND FIRST w-ordl WHERE w-rowid EQ ROWID(oe-ordl) AND w-ok NO-ERROR.

    IF AVAILABLE w-ordl THEN 
    DO:
        IF w-auto THEN v-auto = YES.

        RUN oe/actrelmerg.p (INPUT ROWID(oe-rel), INPUT "CREATE", INPUT-OUTPUT iocPrompt, OUTPUT rOeRelhRow).

        PAUSE 1 MESSAGE " RELEASED ".

    END.
END.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
