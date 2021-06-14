DEFINE TEMP-TABLE ttRawMaterialsGLTransToPost NO-UNDO
    FIELD accountNo     AS CHARACTER
    FIELD job           AS INTEGER
    FIELD jobNo         AS CHARACTER
    FIELD jobNo2        AS INTEGER
    FIELD errorDesc     AS CHARACTER
    FIELD debitsAmount  AS DECIMAL
    FIELD creditsAmount AS DECIMAL
    INDEX accountNo accountNo
    .