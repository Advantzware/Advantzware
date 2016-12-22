 
    DEFINE VARIABLE iPointer  AS INTEGER   NO-UNDO .
    DEFINE VARIABLE cListHash AS CHARACTER NO-UNDO .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the current item in the Dictionary
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Current AS {2} NO-UNDO 
    GET:
        IF iPointer > 0 AND iPointer <= THIS-OBJECT:Dictionary:Count THEN 
            RETURN CAST (THIS-OBJECT:Dictionary, {1}Dictionary):GetValue
                    (ENTRY (iPointer, THIS-OBJECT:Dictionary:Keys, THIS-OBJECT:Dictionary:KeyDelimiter)) .
        
    END GET . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Dictionary enumerated by this ListEnumerator instance 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Dictionary AS AblPrimitiveDictionary NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Dictionary has changed and the Enumerator needs to be Reset() 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListChanged AS LOGICAL NO-UNDO INIT FALSE 
    GET.
    PROTECTED SET.     
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DictionaryEnumerator class                                                                       
        Notes:                                                  
        @param poDictionary The AblPrimitiveDictionary to Enumerate                   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC {1}DictionaryEnumerator (poDictionary AS AblPrimitiveDictionary):
        SUPER ().
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poDictionary """AblPrimitiveDictionary"":U"} .
        
        ASSIGN THIS-OBJECT:Dictionary = poDictionary .

        THIS-OBJECT:Reset () .
               
        IF NOT TYPE-OF (poDictionary, ISupportsListHash) AND TYPE-OF (poDictionary, ISupportsListChanged) THEN 
            CAST (poDictionary, ISupportsListChanged):ListChanged:Subscribe (ListChangedHandler) .                
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ListChanged event of the IEnumerable instance                                                                      
        Notes:   Requires the ISupportsListChanged interface to by implemented by the List                                  
        @param sender The sender of the event
        @param e The ListChangedEventArgs object instance with the data of the ListChanged event                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ListChangedHandler (sender AS Progress.Lang.Object, 
                                              e AS ListChangedEventArgs):
        
        THIS-OBJECT:ListChanged = TRUE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Moves the enumerator to the next item                                                                        
        Notes:                      
        @return True when the next item is available, false when not.                                                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL MoveNext ():
    
        IF TYPE-OF (THIS-OBJECT:Dictionary, ISupportsListHash) AND 
           CAST (THIS-OBJECT:Dictionary, ISupportsListHash):ListHash <> cListHash THEN 
            UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("MoveNext":U,
                                                                             THIS-OBJECT:GetClass():TypeName) . 
        
        IF THIS-OBJECT:ListChanged THEN 
            UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("MoveNext":U,
                                                                             THIS-OBJECT:GetClass():TypeName) . 

        IF THIS-OBJECT:Dictionary:Keys = "":U THEN 
            RETURN FALSE .  

        ASSIGN iPointer = iPointer + 1 . 

        IF iPointer <= THIS-OBJECT:Dictionary:Count THEN 
            RETURN TRUE . 
        ELSE 
            RETURN FALSE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Resets the Enumerator (starts enumerating from the first item on)                                                                    
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Reset ():
        
        ASSIGN iPointer                = 0 
               THIS-OBJECT:ListChanged = FALSE  . 

        IF TYPE-OF (THIS-OBJECT:Dictionary, ISupportsListHash) THEN 
            ASSIGN cListHash = CAST (THIS-OBJECT:Dictionary, ISupportsListHash):ListHash .

    END METHOD.
