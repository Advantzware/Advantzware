
DEF {1} SHARED WORKFILE w-except LIKE oe-boll
       FIELD dOnhQty like fg-bin.qty       
       FIELD lAvailOnhQty AS LOGICAL
       FIELD cLocBin AS CHARACTER.

DEF {1} SHARED TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.
