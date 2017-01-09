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
    File        : DictionaryEnumerator
    Purpose     : Provides Enumaration for Dictionary members
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 20 20:52:23 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Framework.Base.DictionaryEnumerator
    IMPLEMENTS IEnumerator:

    DEFINE VARIABLE hBuffer   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQuery    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cListHash AS CHARACTER NO-UNDO.

    /**
     * Purpose: Returns the current item in the Dictionary
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Current AS Progress.Lang.Object NO-UNDO
    GET:
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hBuffer """Enumeration"":U"} .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i hBuffer} .

        RETURN hBuffer::Item .

    END GET .

    /**
     * Purpose: Returns the Dictionary enumerated by this DictionaryEnumerator instance
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Dictionary AS IEnumerable NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns if the List has changed and the Enumerator needs to be Reset()
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ListChanged AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    PROTECTED SET.

    /**
     * Purpose: Constructor for the DictionaryEnumerator class
     * Notes:
     * @param poDictionary The reference to an implementation of the IEnumerable instance that is enumerated by this object instance
     * @param phQuery The query handle create by the IEnumerable instance for this DictionaryEnumerator instance
     * @param phBuffer The buffer handle created by the IEnumerable instance for this DictionaryEnumerator instance
     */
    CONSTRUCTOR PUBLIC DictionaryEnumerator (poDictionary AS IEnumerable,
                                             phQuery AS HANDLE,
                                             phBuffer AS HANDLE):
        SUPER ().

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poDictionary """IEnumerable"":U"} .
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phQuery Consultingwerk.WidgetTypeEnum:Query} .
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer Consultingwerk.WidgetTypeEnum:Buffer} .
        Consultingwerk.Assertion.BufferAssert:HasField (phBuffer, "Item":U) .

        ASSIGN THIS-OBJECT:Dictionary = poDictionary
               hQuery                 = phQuery
               hBuffer                = phBuffer .

        THIS-OBJECT:Reset () .

        IF NOT TYPE-OF (poDictionary, ISupportsListHash) AND TYPE-OF (poDictionary, ISupportsListChanged) THEN
            CAST (poDictionary, ISupportsListChanged):ListChanged:Subscribe (ListChangedHandler) .

    END CONSTRUCTOR.

    /**
     * Purpose: Event handler for the ListChanged event of the IEnumerable instance
     * Notes:   Requires the ISupportsListChanged interface to by implemented by the List
     * @param sender The sender of the event
     * @param e The ListChangedEventArgs object instance with the data of the ListChanged event
     */
    METHOD PROTECTED VOID ListChangedHandler (sender AS Progress.Lang.Object,
                                              e AS ListChangedEventArgs):

        THIS-OBJECT:ListChanged = TRUE .

    END METHOD.

    /**
     * Purpose: Moves the enumerator to the next item
     * Notes:
     * @return True when the next item is available, false when not.
     */
    METHOD PUBLIC LOGICAL MoveNext ():

        IF TYPE-OF (THIS-OBJECT:Dictionary, ISupportsListHash) AND
           CAST (THIS-OBJECT:Dictionary, ISupportsListHash):ListHash <> cListHash THEN
            UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("MoveNext":U,
                                                                             THIS-OBJECT:GetClass():TypeName) .

        IF THIS-OBJECT:ListChanged THEN
            UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("MoveNext":U,
                                                                             THIS-OBJECT:GetClass():TypeName) .

        hQuery:GET-NEXT () .

        IF hQuery:QUERY-OFF-END THEN
            RETURN FALSE .
        ELSE
            RETURN TRUE .

    END METHOD.

    /**
     * Purpose: Resets the Enumerator (starts enumerating from the first item on)
     * Notes:
     */
    METHOD PUBLIC VOID Reset ():

        hQuery:QUERY-OPEN () .

        THIS-OBJECT:ListChanged = FALSE .

        IF TYPE-OF (THIS-OBJECT:Dictionary, ISupportsListHash) THEN
            ASSIGN cListHash = CAST (THIS-OBJECT:Dictionary, ISupportsListHash):ListHash .

    END METHOD.

    /**
     * Purpose: Destructor for the DictionaryEnumerator class
     * Notes:
     */
    DESTRUCTOR PUBLIC DictionaryEnumerator ():
        IF VALID-HANDLE (hQuery) THEN DO:
            hQuery:QUERY-CLOSE () .
            DELETE OBJECT hQuery .
        END.

        IF VALID-HANDLE (hBuffer) THEN
            DELETE OBJECT hBuffer .

    END DESTRUCTOR.

END CLASS.
