
DEF {1} SHARED VAR framechk-i-changed AS LOG NO-UNDO.

DEF {1} SHARED TEMP-TABLE tt-frame FIELD tt-frame-seq AS INT
                                   FIELD tt-frame-val AS CHAR
                                   INDEX tt-frame-seq tt-frame-seq.
