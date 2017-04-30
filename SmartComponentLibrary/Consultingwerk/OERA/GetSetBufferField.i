    /*------------------------------------------------------------------------------
        Purpose: Returns the value of a buffer field
        Notes:   
        @param pcFieldName The name of the field
        @return The field value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED {1} Get{2}FieldValue (pcFieldName AS CHARACTER):
        
        IF NOT THIS-OBJECT:BufferHandle:Available THEN 
            UNDO, THROW NEW RecordNotAvailableException (SUBSTITUTE ("No &1 record is available."{&TRAN},
                                                                     THIS-OBJECT:BufferName), 
                                                         0) . 

        RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName):BUFFER-VALUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Assigns the value of a buffer field
        Notes:   
        @param pcFieldName The name of the field
        @param pxFieldValue The value for the field
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Set{2}FieldValue (pcFieldName AS CHARACTER,
                                            pxFieldValue AS {1}):
        
        IF NOT THIS-OBJECT:BufferHandle:Available THEN 
            UNDO, THROW NEW RecordNotAvailableException (SUBSTITUTE ("No &1 record is available."{&TRAN},
                                                                     THIS-OBJECT:BufferName), 
                                                         0) . 

        THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName):BUFFER-VALUE = pxFieldValue .

    END METHOD .
