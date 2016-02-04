DEFINE {1} SHARED TEMP-TABLE tt-est-dir NO-UNDO
       FIELD dir-name AS CHAR FORMAT "X(100)"
       FIELD dir-count AS INT
       INDEX tt-est-dir-idx IS PRIMARY dir-count ASC.
