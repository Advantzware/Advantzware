/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Compare
    Purpose     : Provides simplified comparision routines for ABL primitive
                  values
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Mar 14 22:40:05 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Util.CompareHelper:

    /**
     * Purpose: Constructor for the Compare class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED CompareHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Compares if the passed in character string is not unknown value
     *          or empty
     * Notes:
     * @param pcCharacter The character to compare
     * @return True when the passed in character value is not ? or Empty
     */
    METHOD PUBLIC STATIC LOGICAL IsNotNullOrEmpty (pcCharacter AS CHARACTER):

        RETURN pcCharacter > "":U.

    END METHOD .

    /**
     * Purpose: Compares if the passed in decimal value is not the unknown value
     *          or zero
     * Notes:
     * @param pdeValue The decimal to compare
     * @return True when the passed in decimal value is ? or Empty
     */
    METHOD PUBLIC STATIC LOGICAL IsNotZeroOrNull (pdeValue AS DECIMAL):

        RETURN pdeValue <> ? AND pdeValue <> 0 .

    END METHOD.

    /**
     * Purpose: Compares if the passed in integer value is not the unknown value
     *          or empty
     * Notes:
     * @param piValue The integer to compare
     * @return True when the passed in integer value is ? or Empty
     */
    METHOD PUBLIC STATIC LOGICAL IsNotZeroOrNull (piValue AS INTEGER):

        RETURN piValue <> ? AND piValue <> 0 .

    END METHOD.

    /**
     * Purpose: Compares if the passed in int64 value is not the unknown value
     *          or empty
     * Notes:
     * @param piValue The int64 to compare
     * @return True when the passed in int64 value is ? or Empty
     */
    METHOD PUBLIC STATIC LOGICAL IsNotZeroOrNull (piValue AS INT64):

        RETURN piValue <> ? AND piValue <> 0 .

    END METHOD.

    /**
     * Purpose: Compares if the passed in character string is the unknown value
     *          or empty
     * Notes:
     * @param pcCharacter The character to compare
     * @return True when the passed in character value is ? or Empty
     */
    METHOD PUBLIC STATIC LOGICAL IsNullOrEmpty (pcCharacter AS CHARACTER):

        RETURN pcCharacter = ? OR pcCharacter = "":U.

    END METHOD .

    /**
     * Purpose: Compares if the passed in decimal value is the unknown value
     *          or empty
     * Notes:
     * @param pdeValue The decimal to compare
     * @return True when the passed in decimal value is ? or Empty
     */
    METHOD PUBLIC STATIC LOGICAL IsZeroOrNull (pdeValue AS DECIMAL):

        RETURN pdeValue = ? OR pdeValue = 0 .

    END METHOD.

    /**
     * Purpose: Compares if the passed in integer value is the unknown value
     *          or empty
     * Notes:
     * @param piValue The integer to compare
     * @return True when the passed in integer value is ? or Empty
     */
    METHOD PUBLIC STATIC LOGICAL IsZeroOrNull (piValue AS INTEGER):

        RETURN piValue = ? OR piValue = 0 .

    END METHOD.

    /**
     * Purpose: Compares if the passed in int64 value is the unknown value
     *          or empty
     * Notes:
     * @param piValue The int64 to compare
     * @return True when the passed in int64 value is ? or Empty
     */
    METHOD PUBLIC STATIC LOGICAL IsZeroOrNull (piValue AS INT64):

        RETURN piValue = ? OR piValue = 0 .

    END METHOD.

END CLASS.
