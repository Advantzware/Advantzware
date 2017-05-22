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
    File        : ListNameValuePair
    Purpose     : List of NameValuePair implementations
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 31 20:39:40 CEST 2011
    Notes       : Simplified dictionary implementation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.ListNameValuePair
    INHERITS GenericList
    {&SERIALIZABLE}:

    { Consultingwerk/Framework/Base/GenericList.i NameValuePair }

    /**
     * Purpose: Returns if the ListNameValuePair contains a pair this the given Name
     * Notes:
     * @param pcName The Name value to search
     * @return True, when the ListNameValuePair contains a pair this the given Name
     */
    METHOD PUBLIC LOGICAL ContainsName (pcName AS CHARACTER):

        {Consultingwerk/foreachABL.i NameValuePair oPair in THIS-OBJECT}

             IF oPair:Name = pcName THEN
                RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Returns a ListNameValuePair from a single delimited list
     * Notes:   name1,name2,name3
     * @param pcList The simple delimited list
     * @return The new ListNameValuePair
     */
    METHOD PUBLIC STATIC ListNameValuePair FromSingleDelimitedList (pcList AS CHARACTER):

        DEFINE VARIABLE i        AS INTEGER           NO-UNDO .
        DEFINE VARIABLE iEntries AS INTEGER           NO-UNDO .
        DEFINE VARIABLE oList    AS ListNameValuePair NO-UNDO .

        ASSIGN iEntries = NUM-ENTRIES (pcList)
               oList    = NEW ListNameValuePair().

        DO i = 1 TO iEntries:

            oList:Add (ENTRY (i, pcList), ENTRY (i, pcList)) .

        END.

        RETURN oList .

    END METHOD .

    /**
     * Purpose: Returns a ListNameValuePair from a single delimited list
     * Notes:   name1,name2,name3
     * @param pcList The simple delimited list
     * @param pcDelimiter The list delimiter
     * @return The new ListNameValuePair
     */
    METHOD PUBLIC STATIC ListNameValuePair FromSingleDelimitedList (pcList AS CHARACTER,
                                                                    pcDelimiter AS CHARACTER):

        DEFINE VARIABLE i        AS INTEGER           NO-UNDO .
        DEFINE VARIABLE iEntries AS INTEGER           NO-UNDO .
        DEFINE VARIABLE oList    AS ListNameValuePair NO-UNDO .

        ASSIGN iEntries = NUM-ENTRIES (pcList, pcDelimiter)
               oList    = NEW ListNameValuePair().

        DO i = 1 TO iEntries:

            oList:Add (ENTRY (i, pcList, pcDelimiter), ENTRY (i, pcList, pcDelimiter)) .

        END.

        RETURN oList .

    END METHOD .

    /**
     * Purpose: Finds a NameValuePair by it's Name
     * Notes:   Performs a FIND-FIRST, does not verify if there is a second pair
     *          with the same name
     * @param pcName The Name value to search
     * @return The matching NameValuePair or ? when no Pair has been found
     */
    METHOD PUBLIC NameValuePair FindByName (pcName AS CHARACTER):

        {Consultingwerk/foreachABL.i NameValuePair oPair in THIS-OBJECT}

             IF oPair:Name = pcName THEN
                RETURN oPair .
        END.

        RETURN ? .

    END METHOD .

    /**
     * Purpose: Adds a NameValuePair to the List
     * Notes:
     * @param pcName The Name value
     * @param pcValue The value
     * @return The new NameValuePair added to the List
     */
    METHOD PUBLIC NameValuePair Add (pcName AS CHARACTER, pcValue AS CHARACTER):

        DEFINE VARIABLE oItem AS NameValuePair NO-UNDO .

        oItem = NEW NameValuePair (pcName, pcValue) .

        THIS-OBJECT:InternalAdd (oItem).

        RETURN oItem .

    END METHOD.

    /**
     * Purpose: Returns the value for the given Name
     * Notes:   When the Name is used in the List more than once, the first occurence
     *          is returned
     * @param pcName The Name value to search
     * @return The Value of the Pair with the given Name or ? when no pair was found
     */
    METHOD PUBLIC CHARACTER GetValueByPair (pcName AS CHARACTER):

        DEFINE VARIABLE oPair AS NameValuePair NO-UNDO.

        ASSIGN oPair = THIS-OBJECT:FindByName (pcName) .

        IF VALID-OBJECT (oPair) THEN
            RETURN oPair:Value .
        ELSE
            RETURN ? .

    END METHOD .

END CLASS.
