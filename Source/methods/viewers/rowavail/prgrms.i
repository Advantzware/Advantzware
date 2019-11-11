/* prgrms.i - rstark - 11.5.2019 */

DEFINE VARIABLE hField AS HANDLE  NO-UNDO.
DEFINE VARIABLE idx    AS INTEGER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE prgrms THEN
    DO idx = 1 TO EXTENT(hSubjectID):
        IF VALID-HANDLE(hSubjectID[idx]) THEN
        hSubjectID[idx]:BGCOLOR = IF CAN-FIND(FIRST dynPrgrmsPage
                                              WHERE dynPrgrmsPage.prgmName EQ prgrms.prgmName
                                                AND dynPrgrmsPage.pageTab  EQ idx - 1) THEN 11 ELSE 15.
    END. /* if avail */
END. /* with frame */
