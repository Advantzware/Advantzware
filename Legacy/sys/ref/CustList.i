DEFINE {1} SHARED TEMP-TABLE ttCustList
    FIELD cust-no AS CHARACTER
    FIELD char-fld AS CHARACTER
    FIELD log-fld AS LOGICAL
    FIELD int-fld AS INTEGER
    FIELD dec-fld AS DECIMAL
    FIELD date-fld AS DATE
    FIELD cSource AS CHARACTER
    INDEX cust-no cust-no      .
DEFINE {1} SHARED VAR custcount AS CHAR NO-UNDO .