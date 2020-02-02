    /* Temp-table to save the input data */
    DEFINE TEMP-TABLE ttInput NO-UNDO
        FIELD fieldSeq      AS INTEGER
        FIELD fieldKey      AS CHARACTER
        FIELD fieldValue    AS CHARACTER
        FIELD fieldDB       AS CHARACTER     
        FIELD fieldDataType AS CHARACTER
        FIELD fieldProcess  AS LOGICAL
        FIELD success       AS LOGICAL
        .
