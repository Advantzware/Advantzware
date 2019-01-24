/* pSetOrder.i - rstark - 12.20.2018 - used in queryBldr.w */

FOR EACH b{1}
    WHERE b{1}.subjectID EQ ttSubject.subjectID
    &IF "{1}" EQ "ttSubjectWhere" &THEN
      AND b{1}.whereTable EQ tableList
    &ENDIF
       BY b{1}.sortOrder
    :
    ASSIGN
        idx = idx + 1
        b{1}.sortOrder = idx
        .
END. /* for each */
{&OPEN-QUERY-{2}}
fShowQuery().
