/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/  
/*------------------------------------------------------------------------
    File        : List
    Purpose     : Am abstract generic dictionary foundation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 27 15:01:32 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Framework.Base.GenericDictionary
    IMPLEMENTS IEnumerable, ISupportsListChanged, ISupportsListHash
    ABSTRACT
    {&SERIALIZABLE}: 

    DEFINE PRIVATE STATIC TEMP-TABLE ttDictionary NO-UNDO
        FIELD RecordOwner AS CHARACTER
        FIELD Key     AS CHARACTER 
        FIELD Item    AS Progress.Lang.Object
        INDEX Key IS UNIQUE PRIMARY RecordOwner Key 
        INDEX RecordOwner RecordOwner Item 
        .

    DEFINE VARIABLE cInternalId AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Raised when the Dictionary is changed by adding or removing items or 
                 clearing the dictionary
        Notes:      
        @param sender The sender of the event
        @param e The ListChangedEventArgs object instance with the data of the ListChanged event                                                                  
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC EVENT ListChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS ListChangedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Returns the Dictionaries item count                                                                  
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Count AS INTEGER NO-UNDO 
    GET():
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        DEFINE BUFFER ttDictionary FOR ttDictionary . 
        
        FOR EACH ttDictionary WHERE ttDictionary.RecordOwner = cInternalId:
            ASSIGN i = i + 1.
        END.
        
        RETURN i . 
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns a hash value representing the current list contents                                                                   
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListHash AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Dictionary class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED GenericDictionary ():
        SUPER ().
        
        ASSIGN cInternalId          = GUID 
               THIS-OBJECT:ListHash = GUID . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Removes all items from the Dictionary                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Clear ():

        DEFINE BUFFER ttDictionary FOR ttDictionary . 
        
        FOR EACH ttDictionary WHERE ttDictionary.RecordOwner = cInternalId:
            ASSIGN ttDictionary.Item = ? . 
            DELETE ttDictionary . 
        END.

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListCleared)) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Determines if an object is a member of the Dictionary                                       
        Notes:         
        @param poItem The object to locate in the Dictionary
        @return Logical value indicating if the object is contained in the Dictionary                                                            
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ContainsItem (poItem AS Progress.Lang.Object):
        
        DEFINE BUFFER ttDictionary FOR ttDictionary . 
                        
        RETURN CAN-FIND (FIRST ttDictionary WHERE ttDictionary.RecordOwner = cInternalId
                                              AND ttDictionary.Item    = poItem) . 
                                  
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Determines if an key is a member of the Dictionary
        Notes:         
        @param pcKey The key to locate in the Dictionary
        @return Logical value indicating if the object is contained in the Dictionary                                                            
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ContainsKey (pcKey AS CHARACTER):
        
        DEFINE BUFFER ttDictionary FOR ttDictionary . 
                        
        RETURN CAN-FIND (FIRST ttDictionary WHERE ttDictionary.RecordOwner = cInternalId
                                              AND ttDictionary.Key         = pcKey) . 
                                  
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a new IEnumerator instance for this object instance                                                                      
        Notes:                                                                        
        @return The IEnumerator instance for this object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IEnumerator GetEnumerator ():

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO .
        DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO .
        
        CREATE BUFFER hBuffer FOR TABLE TEMP-TABLE ttDictionary:HANDLE . 
        CREATE QUERY hQuery .
        
        hQuery:SET-BUFFERS (hBuffer) . 
        hQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH ttDictionary WHERE ttDictionary.RecordOwner = &1":U,
                                          QUOTER (cInternalId))) .
        
        RETURN NEW DictionaryEnumerator (THIS-OBJECT, 
                                         hQuery, 
                                         hBuffer) . 
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds an Item to the Dictionary                                                                     
        Notes:   
        @param pcKey The key value
        @param poItem The Item to add to the Dictionary   
        @return The item that was added to the dictionary                                                                   
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED Progress.Lang.Object InternalAdd (pcKey AS CHARACTER, 
                                                       poItem AS Progress.Lang.Object):
        
        DEFINE BUFFER ttDictionary FOR ttDictionary . 
        
        IF THIS-OBJECT:ContainsKey (pcKey) THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE ("The specified key is already member of the Dictionary."{&TRAN},
                                                  pcKey), 0) .
        
        CREATE ttDictionary.
        ASSIGN ttDictionary.RecordOwner = cInternalId 
               ttDictionary.Key         = pcKey
               ttDictionary.Item        = poItem . 

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListItemAdded)) .

        RETURN poItem . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Item at the specified Index                                                                       
        Notes:      
        @param pcKey The Key of the Item to be returned                                                                    
        @return The object at the specified dictionary position                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED Progress.Lang.Object InternalGetItem (pcKey AS CHARACTER):
        
        DEFINE BUFFER ttDictionary FOR ttDictionary . 
                        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        FIND ttDictionary WHERE ttDictionary.RecordOwner = cInternalId 
                            AND ttDictionary.Key         = pcKey NO-ERROR .
             
        IF NOT AVAILABLE ttDictionary THEN 
            UNDO, THROW NEW AppError ("The specified key is not part of the Dictionary."{&TRAN}, 0) . 
        
        IF AVAILABLE ttDictionary THEN 
            RETURN ttDictionary.Item .
            
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ListChanged event
        Notes:                              
        @param e The ListChangedEventArgs object instance with the data of the ListChanged event                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnListChanged (e AS ListChangedEventArgs):
        
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "ListChanged":U) .
    
        THIS-OBJECT:ListHash = GUID . 
        
        THIS-OBJECT:ListChanged:Publish (THIS-OBJECT, e) .        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes the first occurrence of a specific object from the Dictionary                                                                    
        Notes: 
        @param poDictionaryItem The member to remove from the Dictionary                                                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveItem (poDictionaryItem AS Progress.Lang.Object):

        DEFINE BUFFER ttDictionary FOR ttDictionary . 

        FIND FIRST ttDictionary WHERE ttDictionary.RecordOwner = cInternalId
                                  AND ttDictionary.Item        = poDictionaryItem NO-ERROR . 
                            
        IF NOT AVAILABLE ttDictionary THEN 
            RETURN . 
            
        DELETE ttDictionary . 

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListItemRemoved)) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Removes the Dictionary item at the specified index.                                                              
        Notes:                                                                        
        @param pcKey The Index of the Item to remove from the Dictionary                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveKey (pcKey AS CHARACTER):

        DEFINE BUFFER ttDictionary FOR ttDictionary . 

        FIND ttDictionary WHERE ttDictionary.RecordOwner = cInternalId 
                            AND ttDictionary.Key         = pcKey NO-ERROR .
            
        IF NOT AVAILABLE ttDictionary THEN 
            UNDO, THROW NEW AppError ("The specified key is not part of the Dictionary."{&TRAN}, 0) . 
            
        DELETE ttDictionary . 

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListItemRemoved)) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the Dictionary class                                                              
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC GenericDictionary ():
        
        THIS-OBJECT:Clear () .
        
    END DESTRUCTOR.

END CLASS.
