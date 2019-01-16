/* pSetOrder.i - rstark - 12.20.2018 - used in queryBldr.w */

FOR EACH {1}
    WHERE {1}.subjectID EQ ttSubject.subjectID
    &IF "{1}" EQ "ttSubjectWhere" &THEN
      AND {1}.whereTable EQ tableList
    &ENDIF
       BY {1}.sortOrder
    :
    ASSIGN
        idx = idx + 1
        {1}.sortOrder = idx
        .
END. /* for each */
{&OPEN-QUERY-{2}}
fShowQuery().
