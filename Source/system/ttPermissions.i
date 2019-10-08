/* ttPermissions.i */

DEFINE TEMP-TABLE ttPermissions NO-UNDO
    FIELD sortMnemonic LIKE prgrms.mnemonic LABEL "Sort"
    FIELD sortOrder      AS INTEGER         LABEL "Order" FORMAT ">>>>9"
    FIELD mnemonic     LIKE prgrms.mnemonic LABEL "Hotkey"
    FIELD parentPrgm     AS CHARACTER       LABEL "Parent"
    FIELD prgmName     LIKE prgrms.prgmname
    FIELD prgTitle     LIKE prgrms.prgTitle
    FIELD can_run      LIKE prgrms.can_run
    FIELD can_create   LIKE prgrms.can_create
    FIELD can_delete   LIKE prgrms.can_delete
    FIELD can_update   LIKE prgrms.can_update
    FIELD groups         AS CHARACTER       LABEL "Groups" FORMAT "x(60)"
        INDEX ttPermissions IS PRIMARY
            sortMnemonic parentPrgm sortOrder prgmName
            .
