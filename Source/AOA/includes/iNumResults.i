/* iNumResults.i - rstark - 2.11.2021 */

iNumResults = iphQuery:NUM-RESULTS.
IF iNumResults EQ 0 THEN DO:
    iphQuery:GET-FIRST().
    IF NOT iphQuery:QUERY-OFF-END THEN
    REPEAT:
        iNumResults = iNumResults + 1.
        iphQuery:GET-NEXT().
        IF iphQuery:QUERY-OFF-END THEN LEAVE.
    END. /* repeat */
END. /* if inumresults */
