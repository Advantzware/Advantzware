    IF ENTRY(NUM-ENTRIES({&param1}, "."), {&param1}, ".") EQ "json" THEN DO:
        EMPTY TEMP-TABLE tt{&param2}.
        EMPTY TEMP-TABLE ttUnmatchedData.

        TEMP-TABLE tt{&param2}:READ-JSON("file", {&param1}).
                
        FOR EACH tt{&param2}:
            FIND FIRST {&param2} NO-LOCK
                 WHERE {&param2}.rec_Key = tt{&param2}.rec_Key 
                 NO-ERROR.
            IF AVAILABLE {&param2} THEN DO: 
                BUFFER-COMPARE {&param2} TO tt{&param2} SAVE RESULT IN cCompareResult.
                IF cCompareResult GT "" THEN 
                    ASSIGN 
                        tt{&param2}.differentFields = cCompareResult 
                    .
                ELSE 
                    ASSIGN
                        tt{&param2}.differentFields = "Exact Match" 
                        .
            END.   
            IF NOT AVAILABLE {&param2} THEN  
                tt{&param2}.differentFields = "New Record". 
        END.
    END.
    ELSE DO:
        INPUT FROM VALUE({&param1}). 
        EMPTY TEMP-TABLE tt{&param2}.
        EMPTY TEMP-TABLE ttUnmatchedData.
        REPEAT:     
            CREATE tt{&param2}.
            IMPORT tt{&param2} EXCEPT tt{&param2}.differentFields. 
            FIND FIRST {&param2} NO-LOCK
                 WHERE {&param2}.rec_Key = tt{&param2}.rec_Key 
                 NO-ERROR.
            IF AVAILABLE {&param2} THEN DO: 
                BUFFER-COMPARE {&param2} TO tt{&param2} SAVE RESULT IN cCompareResult.
                IF cCompareResult GT "" THEN 
                    ASSIGN 
                        tt{&param2}.differentFields = cCompareResult 
                    .
                ELSE 
                    ASSIGN
                        tt{&param2}.differentFields = "Exact Match" 
                        .
            END.   
            IF NOT AVAILABLE {&param2} THEN  
                tt{&param2}.differentFields = "New Record". 
    
        END.
        INPUT CLOSE.
    END.
    
    FOR EACH tt{&param2} 
        WHERE tt{&param2}.DifferentFields NE "" : 
        CREATE ttUnmatchedData.
        ASSIGN
            ttUnmatchedData.ttReckey          = tt{&param2}.rec_key
            ttUnmatchedData.ttTableName       = cbTableName
            ttUnmatchedData.ttUnmatchedFields = tt{&param2}.differentFields
            ttUnmatchedData.ttPrimaryKey      = tt{&param2}.{&param3}
            ttUnmatchedData.ttCompany         = tt{&param2}.company
            .
            
        &IF DEFINED(param4) NE 0 &THEN 
            ttUnmatchedData.ttPrimaryKey = ttUnmatchedData.ttPrimaryKey + "," + tt{&param2}.{&param4}.
        &ENDIF   

        &IF DEFINED(param5) NE 0 &THEN 
           ttUnmatchedData.ttPrimaryKey = ttUnmatchedData.ttPrimaryKey + "," + tt{&param2}.{&param5}.
        &ENDIF   
 
    END.