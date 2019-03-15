/* ttDynQueryParam.i - rstark - 12.20/2018 */

DEFINE TEMP-TABLE ttDynQueryParam NO-UNDO
    FIELD ParamCharacter AS CHARACTER EXTENT 10
    FIELD ParamDecimal   AS DECIMAL   EXTENT 10
    FIELD ParamDate      AS DATE      EXTENT 10
    FIELD ParamDateTime  AS DATETIME  EXTENT 10
    FIELD ParamInteger   AS INTEGER   EXTENT 10
    FIELD ParamLogical   AS LOGICAL   EXTENT 10
    .
