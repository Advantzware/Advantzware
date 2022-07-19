DEFINE TEMP-TABLE ttVariable NO-UNDO
    FIELD varFull      AS CHARACTER FORMAT "X(100)"
    FIELD varName      AS CHARACTER FORMAT "X(50)"  LABEL "Name"
    FIELD varPosition  AS INTEGER
    FIELD varLength    AS INTEGER
    FIELD varFormat    AS CHARACTER FORMAT "X(30)"
    FIELD varDataType  AS CHARACTER FORMAT "X(30)"
    FIELD varAlign     AS CHARACTER
    FIELD varFunction1 AS CHARACTER FORMAT "X(50)"
    FIELD varFunction2 AS CHARACTER FORMAT "X(50)"
    FIELD varFunction3 AS CHARACTER FORMAT "X(50)"
    FIELD varFunction4 AS CHARACTER FORMAT "X(50)"
    FIELD varFunction5 AS CHARACTER FORMAT "X(50)"
    FIELD sequenceID   AS INTEGER                   LABEL "#"
    .