/* cQueryStr.i - rstark - 4.6.2019 */

/* logic to remove [[parameter]] with parameter value */

cQueryStr = {1}.
IF INDEX({1},"[[") NE 0 THEN
FOR EACH dynValueParam EXCLUSIVE-LOCK
    WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
      AND dynValueParam.user-id      EQ dynParamValue.user-id
      AND dynValueParam.prgmName     EQ dynParamValue.prgmName
      AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
    :
    cParam = "[[" + dynValueParam.paramName + "]]".
    IF INDEX(cQueryStr,cParam) NE 0 THEN
    CASE dynValueParam.dataType:
        WHEN "Character" THEN DO:
        cQueryStr = REPLACE(cQueryStr,cParam,"~"" + REPLACE(dynValueParam.paramValue,"~"","~~~"") + "~"").
        END.
        WHEN "Date" THEN DO:
            dtDate = DATE(dynValueParam.paramValue) NO-ERROR.
            ASSIGN
                cDate     = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynValueParam.paramFormat)
                cQueryStr = REPLACE(cQueryStr,cParam,cDate)
                .
        END. /* date */
        WHEN "DateTime" THEN DO:
            dtDate = DATE(dynValueParam.paramValue) NO-ERROR.
            ASSIGN
                cDate     = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynValueParam.paramFormat)
                cQueryStr = REPLACE(cQueryStr,cParam,cDate)
                cQueryStr = REPLACE(cQueryStr,cParam,dynValueParam.paramValue)
                .
        END. /* date */
        WHEN "Decimal" OR WHEN "Integer" OR WHEN "Logical" THEN
        cQueryStr = REPLACE(cQueryStr,cParam,dynValueParam.paramValue).
    END CASE.
END. /* each dynvalueparam */

/*/* rstark - remove when depricated */                                                                          */
/*cQueryStr = {1}.                                                                                               */
/*IF INDEX({1},"[[") NE 0 THEN                                                                                   */
/*DO idx = 1 TO EXTENT(dynParamValue.paramName):                                                                 */
/*    IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.                                                          */
/*    cParam = "[[" + dynParamValue.paramName[idx] + "]]".                                                       */
/*    IF INDEX(cQueryStr,cParam) NE 0 THEN                                                                       */
/*    CASE dynParamValue.paramDataType[idx]:                                                                     */
/*        WHEN "Character" THEN DO:                                                                              */
/*        cQueryStr = REPLACE(cQueryStr,cParam,"~"" + REPLACE(dynParamValue.paramValue[idx],"~"","~~~"") + "~"").*/
/*        END.                                                                                                   */
/*        WHEN "Date" THEN DO:                                                                                   */
/*            dtDate = DATE(dynParamValue.paramValue[idx]) NO-ERROR.                                             */
/*            ASSIGN                                                                                             */
/*                cDate     = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynParamValue.paramFormat[idx])        */
/*                cQueryStr = REPLACE(cQueryStr,cParam,cDate)                                                    */
/*                .                                                                                              */
/*        END. /* date */                                                                                        */
/*        WHEN "DateTime" THEN DO:                                                                               */
/*            dtDate = DATE(dynParamValue.paramValue[idx]) NO-ERROR.                                             */
/*            ASSIGN                                                                                             */
/*                cDate     = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynParamValue.paramFormat[idx])        */
/*                cQueryStr = REPLACE(cQueryStr,cParam,cDate)                                                    */
/*                cQueryStr = REPLACE(cQueryStr,cParam,dynParamValue.paramValue[idx])                            */
/*                .                                                                                              */
/*        END. /* date */                                                                                        */
/*        WHEN "Decimal" OR WHEN "Integer" OR WHEN "Logical" THEN                                                */
/*        cQueryStr = REPLACE(cQueryStr,cParam,dynParamValue.paramValue[idx]).                                   */
/*    END CASE.                                                                                                  */
/*END. /* do idx */                                                                                              */
