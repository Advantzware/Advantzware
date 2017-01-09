&IF 1=0 &THEN
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
    File        : GenericList.i
    Purpose     : Generic List template for type safe Add and GetItem
                  methods

    Syntax      : { Consultingwerk/Framework/Base/GenericList ItemType }

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 20 20:52:06 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/
&ENDIF

    /**
     * Purpose: Adds an item to the generic List
     * Notes:
     * @param poItem And item of the Lists member type
     * @return The new Item added to the List
     */
    METHOD PUBLIC {1} Add (poItem AS {1}):

        SUPER:InternalAdd (poItem).

        RETURN poItem .

    END METHOD.

    /**
     * Purpose: Adds an array of items to the generic List
     * Notes:
     * @param poItem An array of items of the Lists member type
     */
    METHOD PUBLIC VOID Add (poItem AS {1} EXTENT):

        SUPER:InternalAdd (poItem).

    END METHOD.

    /**
     * Purpose: Adds an item to the generic List only when it is not
     *          yet contained - silently ignores keys that are already
     *          contained
     * Notes:
     * @param poItem And item of the Lists member type
     * @return The new Item added to the List
     */
    METHOD PUBLIC {1} AddWhenNotContained (poItem AS {1}):

        IF NOT THIS-OBJECT:Contains (poItem) THEN
            SUPER:InternalAdd (poItem).

        RETURN poItem .

    END METHOD.

    /**
     * Purpose: Retrieves an item from the generic List
     * Notes:   CAST's the element from the underlying Progress.Lang.Object based
     *          list
     * @param piIndex The 1 based index of the item to retrieve
     * @return The item of the Lists member type
     */
    METHOD PUBLIC {1} GetItem (INPUT piIndex AS INTEGER ):

        RETURN CAST (SUPER:InternalGetItem (piIndex), {1}) .

    END METHOD.

    /**
     * Purpose: Returns an Array with the elements of the List
     * Notes:
     * @return The array of elements of the Lists member type
     */
    METHOD PUBLIC {1} EXTENT ToArray ():

        DEFINE VARIABLE oArray AS {1}     NO-UNDO EXTENT .
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO .
        DEFINE VARIABLE i      AS INTEGER NO-UNDO .

        ASSIGN iCount = THIS-OBJECT:Count .

        IF iCount = 0 THEN DO:
            EXTENT (oArray) = ? .
            RETURN oArray .
        END.

        EXTENT (oArray) = iCount .

        DO i = 1 TO iCount:
            oArray[i] = CAST (THIS-OBJECT:InternalGetItem (i), {1}).
        END.

        RETURN oArray .

    END METHOD .
