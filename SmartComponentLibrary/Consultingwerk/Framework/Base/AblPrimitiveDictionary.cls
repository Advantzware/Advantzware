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
    File        : AblPrimitiveDictionary
    Purpose     : Abstract foundation for dictionaries of ABL primitive
                  values
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consulingwerk Ltd.
    Created     : Sun Jan 22 14:36:08 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*     FROM PROPATH .
USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Framework.Base.AblPrimitiveDictionary
    IMPLEMENTS ISupportsListChanged, ISupportsListHash
    ABSTRACT:

    /**
     * Purpose: Raised when the List is changed by adding or removing items or
     *          clearing the list
     * Notes:
     * @param sender The sender of the event
     * @param e The ListChangedEventArgs object instance with the data of the ListChanged event
     */
    DEFINE PUBLIC EVENT ListChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS ListChangedEventArgs).

    /**
     * Purpose: Gets the number of key/value pairs contained in the Dictionary
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Count AS INTEGER NO-UNDO
    GET:
        IF THIS-OBJECT:Keys > "":U THEN
            RETURN NUM-ENTRIES (THIS-OBJECT:Keys,
                                THIS-OBJECT:KeyDelimiter) .
        ELSE
            RETURN 0 .

    END GET .

    /**
     * Purpose: Returns the Delimiter used for the Keys list
     * Notes:
     */
    DEFINE PUBLIC PROPERTY KeyDelimiter AS CHARACTER NO-UNDO INITIAL ",":U
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns the list of Keys in the Dictionary
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Keys AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns a hash value representing the current list contents
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ListHash AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Returns the Delimiter used for the Values list
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ValueDelimiter AS CHARACTER NO-UNDO INITIAL ",":U
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns the list of Values in the Dictionary
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Values AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Constructor for the ABLPrimitityDictionary class
     * Notes:
     */
    CONSTRUCTOR PUBLIC AblPrimitiveDictionary ():

        SUPER () .

        THIS-OBJECT:ListHash = GUID .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the AblPrimitiveDictionary class
     * Notes:   Defaults to the comma (",") as the delimiter for Keys and Values
     * @param pcKeys The initial list of Keys
     * @param pcValues The initial list of Values
     */
    CONSTRUCTOR PUBLIC AblPrimitiveDictionary (pcKeys AS CHARACTER,
                                               pcValues AS CHARACTER):

        THIS-OBJECT (pcKeys, ",":U, pcValues, ",":U) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the AblPrimitiveDictionary class
     * Notes:
     * @param pcKeys The initial list of Keys
     * @param pcKeyDelimiter The delimiter for the Keys
     * @param pcValues The initial list of Values
     * @param pcValueDelimiter The delimiter for the Values
     */
    CONSTRUCTOR PUBLIC AblPrimitiveDictionary (pcKeys AS CHARACTER,
                                               pcKeyDelimiter AS CHARACTER,
                                               pcValues AS CHARACTER,
                                               pcValueDelimiter AS CHARACTER):
        SUPER ().

        THIS-OBJECT:ListHash = GUID .

        IF NUM-ENTRIES (pcKeys, pcKeyDelimiter) <> NUM-ENTRIES (pcValues, pcValueDelimiter) THEN
            UNDO, THROW NEW AppError ("The list of Keys and the list of values needs to have the same number of initial entries."{&TRAN}, 0) .

        IF LENGTH (pcKeyDelimiter, "CHARACTER":U) <> 1 THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcKeyDelimiter":U,
                                                            pcKeyDelimiter,
                                                            THIS-OBJECT:GetClass():TypeName) .

        IF LENGTH (pcValueDelimiter, "CHARACTER":U) <> 1 THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcValueDelimiter":U,
                                                            pcValueDelimiter,
                                                            THIS-OBJECT:GetClass():TypeName) .

        ASSIGN THIS-OBJECT:Keys           = pcKeys
               THIS-OBJECT:KeyDelimiter   = pcKeyDelimiter
               THIS-OBJECT:Values         = pcValues
               THIS-OBJECT:ValueDelimiter = pcValueDelimiter .

    END CONSTRUCTOR.

    /**
     * Purpose: Removes all keys and values from the Dictionary
     * Notes:   Raisees the ListChanged event
     */
    METHOD PUBLIC VOID Clear ():

        ASSIGN THIS-OBJECT:Keys   = "":U
               THIS-OBJECT:Values = "":U .

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListCleared)) .

    END METHOD.

    /**
     * Purpose: Determines whether the Dictionary contains the specified key.
     * Notes:
     * @param pcKey The key to locate in the Dictionary
     * @return Logical value indicating if the dictionary contains the specified key
     */
    METHOD PUBLIC LOGICAL ContainsKey (pcKey AS CHARACTER):

        THIS-OBJECT:ValidateKey (pcKey) .

        IF THIS-OBJECT:Keys > "":U THEN .
        ELSE
            RETURN FALSE .

        IF LOOKUP (pcKey,
                   THIS-OBJECT:Keys,
                   THIS-OBJECT:KeyDelimiter) > 0 THEN
            RETURN TRUE .
        ELSE
            RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Determines the posiiton of the specified key in the list of keys
     * Notes:
     * @param pcKey The Key to locate in the Keys
     * @return The position of the specified in the list of keys
     */
    METHOD PROTECTED INTEGER EntryOfKey (pcKey AS CHARACTER):

        THIS-OBJECT:ValidateKey (pcKey) .

        RETURN LOOKUP (pcKey,
                       THIS-OBJECT:Keys,
                       THIS-OBJECT:KeyDelimiter) .

    END METHOD.

    /**
     * Purpose: Adds the specified key and value to the dictionary.
     * Notes:   Internal method, used by derived class (which converts from the
     *          original value type to the CHARACTER value expected by this method)
     * @param pcKey The key for the key/value pair to add to the dictionary
     * @param pcValue The value for the key/value pair to add to the dictionary
     */
    METHOD PROTECTED VOID InternalAdd (pcKey AS CHARACTER,
                                       pcValue AS CHARACTER):

        DEFINE VARIABLE cKeys   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cValues AS CHARACTER NO-UNDO.

        THIS-OBJECT:ValidateKey (pcKey) .
        THIS-OBJECT:ValidateValue (pcValue) .

        IF THIS-OBJECT:ContainsKey (pcKey) THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcKey":U,
                                                            pcKey,
                                                            THIS-OBJECT:GetClass():TypeName) .

        IF THIS-OBJECT:Count > 0 THEN DO:
            ASSIGN cKeys   = SUBSTITUTE ("&1&2&3":U,
                                         THIS-OBJECT:Keys,
                                         THIS-OBJECT:KeyDelimiter,
                                         pcKey)
                   cValues = SUBSTITUTE ("&1&2&3":U,
                                         THIS-OBJECT:Values,
                                         THIS-OBJECT:ValueDelimiter,
                                         pcValue) .
        END.
        ELSE
            ASSIGN cKeys   = pcKey
                   cValues = pcValue .

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListItemAdded)) .

        ASSIGN THIS-OBJECT:Keys   = cKeys
               THIS-OBJECT:Values = cValues .

    END METHOD.

    /**
     * Purpose: Determines whether the Dictionary contains the specified value.
     * Notes:   Internal method, used by derived class (which converts from the
     *          original value type to the CHARACTER value expected by this method)
     * @param pcValue The value to locate in the Dictionary
     * @return Logical value indicating if the Dictionary contains the specified value
     */
    METHOD PROTECTED LOGICAL InternalContainsValue (pcValue AS CHARACTER):

        THIS-OBJECT:ValidateValue (pcValue) .

        IF THIS-OBJECT:Values > "":U THEN .
        ELSE
            RETURN FALSE .

        IF LOOKUP (pcValue,
                   THIS-OBJECT:Values,
                   THIS-OBJECT:ValueDelimiter) > 0 THEN
            RETURN TRUE .
        ELSE
            RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Returns the Value for the specified Key
     * Notes:   Internal method, used by derived class (which converts from the
     *          original value type to the CHARACTER value expected by this method)
     * @param pcKey The value to locate in the Dictionary
     * @return The value for the specified key
     */
    METHOD PROTECTED CHARACTER InternalGetValue (pcKey AS CHARACTER):

        THIS-OBJECT:ValidateKey (pcKey) .

        IF NOT THIS-OBJECT:ContainsKey (pcKey) THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcKey":U,
                                                            pcKey,
                                                            THIS-OBJECT:GetClass():TypeName) .

        RETURN ENTRY (THIS-OBJECT:EntryOfKey (pcKey),
                      THIS-OBJECT:Values,
                      THIS-OBJECT:ValueDelimiter) .

    END METHOD.

    /**
     * Purpose: Assigns the Value for the specified Key
     * Notes:   Internal method, used by derived class (which converts from the
     *          original value type to the CHARACTER value expected by this method)
     * @param pcKey The value to locate in the Dictionary
     * @param pcValue The value to assign for the Key in the Dictionary
     */
    METHOD PROTECTED VOID InternalSetValue (pcKey AS CHARACTER,
                                            pcValue AS CHARACTER):

        DEFINE VARIABLE cValues AS CHARACTER NO-UNDO.

        THIS-OBJECT:ValidateKey (pcKey) .
        THIS-OBJECT:ValidateValue (pcValue) .

        IF NOT THIS-OBJECT:ContainsKey (pcKey) THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcKey":U,
                                                            pcKey,
                                                            THIS-OBJECT:GetClass():TypeName) .

        ASSIGN cValues = THIS-OBJECT:Values
               ENTRY (THIS-OBJECT:EntryOfKey (pcKey),
                      cValues,
                      THIS-OBJECT:ValueDelimiter) = pcValue
               THIS-OBJECT:Values = cValues.

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListItemChanged)) .

    END METHOD.

    /**
     * Purpose: Raises the ListChanged event
     * Notes:
     * @param e The ListChangedEventArgs object instance with the data of the ListChanged event
     */
    METHOD PROTECTED VOID OnListChanged (e AS ListChangedEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "ListChanged":U) .

        THIS-OBJECT:ListHash = GUID .

        THIS-OBJECT:ListChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Removes the Key/Value for the specified Key
     * Notes:   Internal method, used by derived class (which converts from the
     *          original value type to the CHARACTER value expected by this method)
     * @param pcKey The value to remove from the Dictionary
     */
    METHOD PUBLIC VOID Remove (pcKey AS CHARACTER):

        DEFINE VARIABLE iEntry AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cKeys  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cValues AS CHARACTER NO-UNDO.

        THIS-OBJECT:ValidateKey (pcKey) .

        IF NOT THIS-OBJECT:ContainsKey (pcKey) THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcKey":U,
                                                            pcKey,
                                                            THIS-OBJECT:GetClass():TypeName) .

        ASSIGN iEntry = THIS-OBJECT:EntryOfKey (pcKey)

               cKeys   = Consultingwerk.Util.ListHelper:RemoveEntry (THIS-OBJECT:Keys,
                                                                     iEntry,
                                                                     THIS-OBJECT:KeyDelimiter)
               cValues = Consultingwerk.Util.ListHelper:RemoveEntry (THIS-OBJECT:Values,
                                                                     iEntry,
                                                                     THIS-OBJECT:ValueDelimiter)
               THIS-OBJECT:Keys   = cKeys
               THIS-OBJECT:Values = cValues .

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListItemRemoved)) .

    END METHOD.

    /**
     * Purpose: Validates a Key
     * Notes:
     * @param pcKey The key to validate
     */
    METHOD PROTECTED VOID ValidateKey (pcKey AS CHARACTER):

        IF pcKey > "":U THEN .
        ELSE
            UNDO, THROW NEW ParameterNullValueException ("pcKey":U,
                                                         THIS-OBJECT:GetClass():TypeName) .

        IF pcKey = THIS-OBJECT:KeyDelimiter THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcKey":U,
                                                            pcKey,
                                                            THIS-OBJECT:GetClass():TypeName) .

    END METHOD.

    /**
     * Purpose: Validates a Value
     * Notes:
     * @param pcValue The value to validate
     */
    METHOD PROTECTED VOID ValidateValue (pcValue AS CHARACTER):

        IF pcValue = THIS-OBJECT:ValueDelimiter THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcValue":U,
                                                            pcValue,
                                                            THIS-OBJECT:GetClass():TypeName) .

    END METHOD.

END CLASS.