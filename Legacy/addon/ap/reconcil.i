
DEF BUFFER ar-mcash-ref FOR reftable.

DEF {1} SHARED TEMP-TABLE reconcile NO-UNDO
                    FIELD tt-type       AS   INT
                    FIELD tt-rowid      AS   ROWID
                    FIELD tt-number     AS   CHAR FORMAT "x(10)"
                    FIELD tt-date       AS   DATE
                    FIELD tt-amt        AS   DEC  FORMAT "->>,>>>,>>9.99"
                    FIELD tt-bank       LIKE ap-pay.bank-code
                    FIELD tt-vend       AS   CHAR
                    FIELD tt-name       LIKE vend.name
                    FIELD tt-cleared    LIKE ap-pay.cleared.

DEF {1} SHARED TEMP-TABLE tt-cash NO-UNDO
                    FIELD tt-trnum      LIKE ar-ledger.tr-num
                    FIELD row-id        AS   ROWID
                    INDEX tt-trnum tt-trnum.
