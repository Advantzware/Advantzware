/* Obtain next available r-no for release creation */
      
    DEFINE INPUT PARAMETER ipMode AS CHARACTER NO-UNDO. /* oe-rel or oe-relh */    
    DEFINE OUTPUT PARAMETER opNextRelNo AS INTEGER NO-UNDO.

    DEFINE VARIABLE iNextRelNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLastRelNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
    
    CASE ipMode:
        WHEN "oe-rel" THEN DO:
            ASSIGN 
                opNextRelNo = NEXT-VALUE(oerel_rno_seq).
            /* This SHOULD take care of situations where an out-of-sequence r-no has been added, */
            /* such that the sequence generator is updated to the latest (correct) r-no.         */
            /* Since this is not a consistent error, there is some sequence of events/processes  */
            /* that create this condition, but all reported cases have been resolved within 1-2  */
            /* sequence numbers                                                                  */
            DO WHILE CAN-FIND(FIRST oe-rel WHERE
                                    oe-rel.r-no EQ opNextRelNo):
                ASSIGN 
                    opNextRelNo = NEXT-VALUE(oerel_rno_seq).
            END.
        END.
        WHEN "release#" THEN DO:
            RUN spGetSessionParam ("Company", OUTPUT cCompany).
            /* If this is run outside a session (e.g. via a monitor program) get the user's default company */
            IF cCompany EQ "" THEN DO:
                FIND FIRST users NO-LOCK WHERE 
                    users.user_id EQ USERID("ASI")
                    NO-ERROR.
                IF AVAIL users THEN FIND FIRST usercomp NO-LOCK WHERE  
                    usercomp.user_id EQ users.user_id AND 
                    usercomp.company_default EQ TRUE 
                    NO-ERROR.
                IF AVAIL usercomp THEN ASSIGN 
                    cCompany = usercomp.company.
            END.
            /* The caller program here (oe/release#.p) contains additional logic, so don't want to change what works */
            RUN sys/ref/asiseq.p (cCompany,
                                  "oerel_release_seq",
                                  OUTPUT opNextRelNo).
        END.
        WHEN "oe-relh" THEN DO:
            ASSIGN
                opNextRelNo = NEXT-VALUE(oerel_release_seq).
            /* This SHOULD take care of situations where an out-of-sequence r-no has been added, */
            /* such that the sequence generator is updated to the latest (correct) r-no.         */
            /* Since this is not a consistent error, there is some sequence of events/processes  */
            /* that create this condition, but all reported cases have been resolved within 1-2  */
            /* sequence numbers                                                                  */
            DO WHILE CAN-FIND(FIRST oe-relh WHERE
                                    oe-relh.r-no EQ opNextRelNo):
                ASSIGN 
                    opNextRelNo = NEXT-VALUE(oerel_release_seq).
            END.
        END.
        OTHERWISE DO:
            MESSAGE
                "Entered mode (" + ipMode + ") not supported" SKIP
                "in program oe/getNextRelNo.p. Please" SKIP
                "contact your System Administrator."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END CASE.

