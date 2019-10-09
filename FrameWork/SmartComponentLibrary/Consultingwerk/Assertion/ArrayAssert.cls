/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ArrayAssert
    Purpose     : Assertion methods for ABL Arrays
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 04 12:35:59 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Assertion.ArrayAssert:

    /**
     * Purpose: Validates that the array does have valid entries only (not ?)
     * Notes:
     * @param pcValue The array to verify
     * @param pcDescription The description of the array
     */
    METHOD PUBLIC STATIC VOID ValidEntriesOnly (pcValue AS CHARACTER EXTENT,
                                                pcDescription AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (pcValue):
            IF pcValue[i] = ? THEN
                UNDO, THROW NEW AssertException(SUBSTITUTE ("Value for &1[&2] is invalid."{&TRAN}, pcDescription, i), 0) .
        END.

    END METHOD.

    /**
     * Purpose: Validates that the array does have valid entries only (not ?)
     * Notes:
     * @param pdtValue The array to verify
     * @param pcDescription The description of the array
     */
    METHOD PUBLIC STATIC VOID ValidEntriesOnly (pdtValue AS DATE EXTENT,
                                                pcDescription AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (pdtValue):
            IF pdtValue[i] = ? THEN
                UNDO, THROW NEW AssertException(SUBSTITUTE ("Value for &1[&2] is invalid."{&TRAN}, pcDescription, i), 0) .
        END.

    END METHOD.

    /**
     * Purpose: Validates that the array does have valid entries only (not ?)
     * Notes:
     * @param pdtValue The array to verify
     * @param pcDescription The description of the array
     */
    METHOD PUBLIC STATIC VOID ValidEntriesOnly (pdtValue AS DATETIME EXTENT,
                                                pcDescription AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (pdtValue):
            IF pdtValue[i] = ? THEN
                UNDO, THROW NEW AssertException(SUBSTITUTE ("Value for &1[&2] is invalid."{&TRAN}, pcDescription, i), 0) .
        END.

    END METHOD.

    /**
     * Purpose: Validates that the array does have valid entries only (not ?)
     * Notes:
     * @param pdtValue The array to verify
     * @param pcDescription The description of the array
     */
    METHOD PUBLIC STATIC VOID ValidEntriesOnly (pdtValue AS DATETIME-TZ EXTENT,
                                                pcDescription AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (pdtValue):
            IF pdtValue[i] = ? THEN
                UNDO, THROW NEW AssertException(SUBSTITUTE ("Value for &1[&2] is invalid."{&TRAN}, pcDescription, i), 0) .
        END.

    END METHOD.

    /**
     * Purpose: Validates that the array does have valid entries only (not ?)
     * Notes:
     * @param pdeValue The array to verify
     * @param pcDescription The description of the array
     */
    METHOD PUBLIC STATIC VOID ValidEntriesOnly (pdeValue AS DECIMAL EXTENT,
                                                pcDescription AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (pdeValue):
            IF pdeValue[i] = ? THEN
                UNDO, THROW NEW AssertException(SUBSTITUTE ("Value for &1[&2] is invalid."{&TRAN}, pcDescription, i), 0) .
        END.

    END METHOD.

    /**
     * Purpose: Validates that the array does have valid entries only
     * Notes:
     * @param phHandle The array of Handle references to verify
     * @param pcDescription The description of the array
     */
    METHOD PUBLIC STATIC VOID ValidEntriesOnly (phHandle AS HANDLE EXTENT,
                                                pcDescription AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (phHandle):
            IF NOT VALID-HANDLE (phHandle[i]) THEN
                UNDO, THROW NEW AssertException(SUBSTITUTE ("Value for &1[&2] is invalid."{&TRAN}, pcDescription, i), 0) .
        END.

    END METHOD.

    /**
     * Purpose: Validates that the array does have valid entries only (not ?)
     * Notes:
     * @param piValue The array to verify
     * @param pcDescription The description of the array
     */
    METHOD PUBLIC STATIC VOID ValidEntriesOnly (piValue AS INTEGER EXTENT,
                                                pcDescription AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (piValue):
            IF piValue[i] = ? THEN
                UNDO, THROW NEW AssertException(SUBSTITUTE ("Value for &1[&2] is invalid."{&TRAN}, pcDescription, i), 0) .
        END.

    END METHOD.

    /**
     * Purpose: Validates that the array does have valid entries only (not ?)
     * Notes:
     * @param plValue The array to verify
     * @param pcDescription The description of the array
     */
    METHOD PUBLIC STATIC VOID ValidEntriesOnly (plValue AS LOGICAL EXTENT,
                                                pcDescription AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (plValue):
            IF plValue[i] = ? THEN
                UNDO, THROW NEW AssertException(SUBSTITUTE ("Value for &1[&2] is invalid."{&TRAN}, pcDescription, i), 0) .
        END.

    END METHOD.

    /**
     * Purpose: Validates that the array does have valid entries only
     * Notes:
     * @param poObject The array of Progress.Lang.Object references to verify
     * @param pcDescription The description of the array
     */
    METHOD PUBLIC STATIC VOID ValidEntriesOnly (poObject AS Progress.Lang.Object EXTENT,
                                                pcDescription AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (poObject):
            IF NOT VALID-OBJECT (poObject[i]) THEN
                UNDO, THROW NEW AssertException(SUBSTITUTE ("Value for &1[&2] is invalid."{&TRAN}, pcDescription, i), 0) .
        END.

    END METHOD.

    /**
     * Purpose: Protected default constructor for the ArrayAssert class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED ArrayAssert ():

    END CONSTRUCTOR.

END CLASS.
