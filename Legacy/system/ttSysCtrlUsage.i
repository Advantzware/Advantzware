/* ttSysCtrlUsage.i */

DEFINE TEMP-TABLE ttSysCtrlUsage NO-UNDO
    FIELD company      LIKE sys-ctrl.company             
    FIELD module       LIKE sys-ctrl.module              LABEL "Module"
    FIELD name         LIKE sys-ctrl.name                                        FORMAT "x(20)"
    FIELD char-fld     LIKE sys-ctrl.char-fld            LABEL "Character Value" FORMAT "x(30)"
    FIELD date-fld     LIKE sys-ctrl.date-fld            LABEL "Date"
    FIELD dec-fld      LIKE sys-ctrl.dec-fld             LABEL "Decimal"
    FIELD int-fld      LIKE sys-ctrl.int-fld             LABEL "Integer"
    FIELD log-fld      LIKE sys-ctrl.log-fld             LABEL "Logical"
    FIELD descrip      LIKE sys-ctrl.descrip                                     FORMAT "x(80)"
    FIELD usageNow       AS DATETIME                     LABEL "Date/Time"
    FIELD category     LIKE sys-ctrl-shipto.category
    FIELD cust-vend    LIKE sys-ctrl-shipto.cust-vend
    FIELD cust-vend-no LIKE sys-ctrl-shipto.cust-vend-no
    FIELD seqNo        LIKE sys-ctrl-shipto.seqNo
    FIELD ship-id      LIKE sys-ctrl-shipto.ship-id
    FIELD subCategory  LIKE sys-ctrl-shipto.subCategory
    FIELD sysCtrlID    LIKE sys-ctrl-shipto.sysCtrlID
    FIELD typeCode     LIKE sys-ctrl-shipto.typeCode
    FIELD stackTrace     AS CHARACTER                    LABEL "Stack"
        INDEX ttSysCtrlUsage IS PRIMARY
            company module name
            .
