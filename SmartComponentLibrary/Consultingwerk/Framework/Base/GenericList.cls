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
    File        : GenericList
    Purpose     : Abstract base class for generic lists
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 20 20:48:40 CET 2011
    Notes       : Uses a single static temp-table for all GenericList instances
                  On OpenEdge 10 to avoid too many temp-tables issues
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Framework.Base.GenericList
    IMPLEMENTS IEnumerable, ISupportsListChanged, ISupportsListHash
    ABSTRACT
    {&SERIALIZABLE}:

    DEFINE PRIVATE TEMP-TABLE ttList NO-UNDO
        FIELD RecordOwner AS CHARACTER
        FIELD ListItem    AS Progress.Lang.Object
        INDEX RecordOwner RecordOwner ListItem
        .

    DEFINE VARIABLE cInternalId AS CHARACTER NO-UNDO.

    /**
     * Purpose: Raised when the List is changed by adding or removing items or
     *          clearing the list
     * Notes:
     * @param sender The sender of the event
     * @param e The ListChangedEventArgs object instance with the data of the ListChanged event
     */
    DEFINE PUBLIC EVENT ListChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS ListChangedEventArgs).

    /**
     * Purpose: Returns the Lists item count
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Count AS INTEGER NO-UNDO
    GET():
        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DEFINE BUFFER ttList FOR ttList .

        FOR EACH ttList WHERE ttList.RecordOwner = cInternalId:
            ASSIGN i = i + 1.
        END.

        RETURN i .
    END GET.

    /**
     * Purpose: Returns a hash value representing the current list contents
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ListHash AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Constructor for the GenericList class
     * Notes:
     */
    CONSTRUCTOR PROTECTED GenericList ():
        SUPER ().

        ASSIGN cInternalId          = GUID
               THIS-OBJECT:ListHash = GUID  .

    END CONSTRUCTOR.

    /**
     * Purpose: Removes all items from the List
     * Notes:
     */
    METHOD PUBLIC VOID Clear ():

        DEFINE BUFFER ttList FOR ttList .

        FOR EACH ttList WHERE ttList.RecordOwner = cInternalId:
            ASSIGN ttList.ListItem = ? .
            DELETE ttList .
        END.

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListCleared)) .

    END METHOD.

    /**
     * Purpose: Determines if an object is a member of the List
     * Notes:
     * @param poListItem The object to locate in the List
     * @return Logical value indicating if the object is contained in the list
     */
    METHOD PUBLIC LOGICAL Contains (poListItem AS Progress.Lang.Object):

        DEFINE BUFFER ttList FOR ttList .

        RETURN CAN-FIND (FIRST ttList WHERE ttList.RecordOwner = cInternalId
                                        AND ttList.ListItem    = poListItem) .

    END METHOD.

    /**
     * Purpose: Returns a new IEnumerator instance for this object instance
     * Notes:
     * @return The IEnumerator instance for this object
     */
    METHOD PUBLIC IEnumerator GetEnumerator ():

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO .
        DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO .

        CREATE BUFFER hBuffer FOR TABLE TEMP-TABLE ttList:HANDLE .
        CREATE QUERY hQuery .

        hQuery:SET-BUFFERS (hBuffer) .
        hQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH ttList WHERE ttList.RecordOwner = &1":U,
                                          QUOTER (cInternalId))) .

        RETURN NEW ListEnumerator (THIS-OBJECT,
                                   hQuery,
                                   hBuffer) .
    END METHOD.

    /**
     * Purpose: Determines the index of a specific item in the List
     * Notes:
     * @param poListItem The object to locate in the List
     * @return Integer value indicating if position of the object in the list
     */
    METHOD PUBLIC INTEGER IndexOf (poListItem AS Progress.Lang.Object):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DEFINE BUFFER ttList FOR ttList .

        FOR EACH ttList WHERE ttList.RecordOwner = cInternalId:
            ASSIGN i = i + 1.

            IF ttList.ListItem = poListItem THEN
                RETURN i .
        END.

        RETURN -1 .

    END METHOD.

    /**
     * Purpose: Adds an Item to the List
     * Notes:   Protected method to be overridden in the actual generic List class
     * @param poItem The Item to add to the List
     */
    METHOD PROTECTED VOID InternalAdd (poItem AS Progress.Lang.Object):

        DEFINE BUFFER ttList FOR ttList .

        CREATE ttList.
        ASSIGN ttList.RecordOwner = cInternalId
               ttList.ListItem    = poItem .

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListItemAdded)) .

    END METHOD.

    /**
     * Purpose: Adds the Items from an Array to the List
     * Notes:   Protected method to be overridden in the actual generic List class
     * @param poItem The Array of Items to add to the List
     */
    METHOD PROTECTED VOID InternalAdd (poItem AS Progress.Lang.Object EXTENT):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (poItem):
            THIS-OBJECT:InternalAdd (poItem[i]) .
        END.

    END METHOD.

    /**
     * Purpose: Returns the Item at the specified Index
     * Notes:   Protected method to be overridden in the actual generic List class
     * @param piIndex The 1 based index of the Item to be returned
     * @return The object at the specified list position
     */
    METHOD PROTECTED Progress.Lang.Object InternalGetItem (piIndex AS INTEGER):

        DEFINE BUFFER ttList FOR ttList .

        DEFINE QUERY qReposition FOR ttList SCROLLING .

        Consultingwerk.Assertion.Assert:GreaterThanZero (piIndex) .

        OPEN QUERY qReposition FOR EACH ttList WHERE ttList.RecordOwner = cInternalId .

        REPOSITION qReposition TO ROW piIndex .

        GET NEXT qReposition .

        IF AVAILABLE ttList THEN
            RETURN ttList.ListItem .

        UNDO, THROW NEW AppError ("The specified index is not part of the List"{&TRAN}, 0) .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            UNDO, THROW NEW AppError ("The specified index is not part of the List"{&TRAN}, 0) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Raises the ListChanged event
     * Notes:
     * @param e The ListChangedEventArgs object instance with the data of the ListChanged event
     */
    METHOD PROTECTED VOID OnListChanged (e AS ListChangedEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "ListChanged":U) .

        THIS-OBJECT:ListHash = GUID  .

        THIS-OBJECT:ListChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Removes the first occurrence of a specific object from the List
     * Notes:
     * @param poListItem The member to remove from the List
     */
    METHOD PUBLIC VOID Remove (poListItem AS Progress.Lang.Object):

        DEFINE BUFFER ttList FOR ttList .

        FIND FIRST ttList WHERE ttList.RecordOwner = cInternalId
                            AND ttList.ListItem    = poListItem NO-ERROR .

        IF NOT AVAILABLE ttList THEN
            RETURN .

        DELETE ttList .

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListItemRemoved)) .

    END METHOD .

    /**
     * Purpose: Removes the List item at the specified index.
     * Notes:
     * @param piIndex The Index of the Item to remove from the List
     */
    METHOD PUBLIC VOID RemoveAt (piIndex AS INTEGER):

        DEFINE BUFFER ttList FOR ttList .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO piIndex:

            FIND NEXT ttList WHERE ttList.RecordOwner = cInternalId NO-ERROR .

            IF NOT AVAILABLE ttList THEN
                UNDO, THROW NEW AppError ("The specified index is not part of the List."{&TRAN}, 0) .
        END.

        IF NOT AVAILABLE ttList THEN
            RETURN .

        DELETE ttList .

        THIS-OBJECT:OnListChanged (NEW ListChangedEventArgs (ListChangedTypeEnum:ListItemRemoved)) .

    END METHOD .

    /**
     * Purpose: Destructor of the Generic List class
     * Notes:
     */
    DESTRUCTOR PUBLIC GenericList ():

        THIS-OBJECT:Clear () .

    END DESTRUCTOR.

END CLASS.
