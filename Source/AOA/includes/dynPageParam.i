/* dynPageParam.i - rstark - 11.5.2019 */

IF AVAILABLE prgrms THEN DO:
    ASSIGN
        iPageNo    = INTEGER(ENTRY(2,SELF:NAME,"-"))
        iSubjectID = INTEGER(hSubjectID[iPageNo + 1]:SCREEN-VALUE)
        .
    /* if current page has subject id use it, otherwise use page 0 */
    IF iSubjectID EQ 0 THEN
    iSubjectID = prgrms.pageSubjectID[1]. /* page 0 */
    IF iSubjectID NE 0 AND
       CAN-FIND(FIRST dynSubject WHERE dynSubject.subjectID EQ iSubjectID) THEN
    RUN AOA/dynPageParam.w (prgrms.prgmName, iPageNo, iSubjectID).
    ELSE
    MESSAGE
        "Valid Subject ID NOT Found!"
    VIEW-AS ALERT-BOX.
END. /* if avail */
