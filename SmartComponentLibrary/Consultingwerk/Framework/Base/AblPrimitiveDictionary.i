    /**
     * Purpose: Constructor for the Dictionary class
     * Notes:
     */
    CONSTRUCTOR PUBLIC {1} ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Dictionary class
     * Notes:   Defaults to the comma (",") as the delimiter for Keys and Values
     * @param pcKeys The initial list of Keys
     * @param pcValues The initial list of Values
     */
    CONSTRUCTOR PUBLIC {1} (pcKeys AS CHARACTER,
                                            pcValues AS CHARACTER):

        SUPER (pcKeys, pcValues).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Dictionary class
     * Notes:
     * @param pcKeys The initial list of Keys
     * @param pcKeyDelimiter The delimiter for the Keys
     * @param pcValues The initial list of Values
     * @param pcValueDelimiter The delimiter for the Values
     */
    CONSTRUCTOR PUBLIC {1} (pcKeys AS CHARACTER,
                                            pcKeyDelimiter AS CHARACTER,
                                            pcValues AS CHARACTER,
                                            pcValueDelimiter AS CHARACTER):

        SUPER (pcKeys, pcKeyDelimiter, pcValues, pcValueDelimiter).

    END CONSTRUCTOR.

    /**
     * Purpose: Adds the specified key and value to the dictionary.
     * Notes:   Throws an InvalidParameterValueException when the dictionary
     *          alredy contains the key (ContainsKey (pcKey) = TRUE)
     * @param pcKey The key for the key/value pair to add to the dictionary
     * @param pValue The value for the key/value pair to add to the dictionary
     */
    METHOD PUBLIC VOID Add (pcKey AS CHARACTER, pValue AS {2}):

        THIS-OBJECT:InternalAdd (pcKey, STRING (pValue)) .

    END METHOD.

    /**
     * Purpose: Adds the specified key and value to the dictionary only
     *          when it is not yet contained - silently ignores keys that
     *          are already contained
     * Notes:   Throws an InvalidParameterValueException when the dictionary
     *          alredy contains the key (ContainsKey (pcKey) = TRUE)
     * @param pcKey The key for the key/value pair to add to the dictionary
     * @param pValue The value for the key/value pair to add to the dictionary
     */
    METHOD PUBLIC VOID AddWhenNotContained (pcKey AS CHARACTER, pValue AS {2}):

        IF NOT THIS-OBJECT:ContainsKey (pcKey) THEN
            THIS-OBJECT:InternalAdd (pcKey, STRING (pValue)) .

    END METHOD.

    /**
     * Purpose: Determines whether the Dictionary contains the specified value.
     * Notes:
     * @param pValue The value to locate in the Dictionary
     * @return Logical value indicating if the value is contained in the Dictionary
     */
    METHOD PUBLIC LOGICAL ContainsValue (pValue AS {2}):

        RETURN THIS-OBJECT:InternalContainsValue (STRING (pValue)).

    END METHOD.

    /**
     * Purpose: Returns the Enumerator for the Dictionary
     * Notes:
     * @return The Enumerator instance
     */
    METHOD PUBLIC {1}Enumerator GetEnumerator ():

        RETURN NEW {1}Enumerator (THIS-OBJECT) .

    END METHOD.

    /**
     * Purpose: Returns the Value for the specified Key
     * Notes:
     * @param pcKey The value to locate in the Dictionary
     * @return The value for the specified key
     */
    METHOD PUBLIC {2} GetValue (pcKey AS CHARACTER):

        RETURN {3} (THIS-OBJECT:InternalGetValue (pcKey)) .

    END METHOD.

    /**
     * Purpose: Assigns the Value for the specified Key
     * Notes:
     * @param pcKey The value to locate in the Dictionary
     * @param pValue The value to assign for the Key in the Dictionary
     */
    METHOD PUBLIC VOID SetValue (pcKey AS CHARACTER,
                                 pValue AS {2}):

        THIS-OBJECT:InternalSetValue (pcKey, STRING (pValue)) .

    END METHOD.