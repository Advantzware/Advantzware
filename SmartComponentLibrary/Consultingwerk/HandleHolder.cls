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
    File        : HandleHolder
    Purpose     : A holder class for ABL primitive values
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 10 01:15:43 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.HandleHolder
    IMPLEMENTS IQueryExpression, IPrimitiveHolder &IF DEFINED (CommonComponentSpecification) NE 0 &THEN , Ccs.Common.Support.IHandleHolder &ENDIF
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Value associated with this holder class
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Value AS HANDLE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the HandleHolder class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC HandleHolder ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the IntegerHolder class
        Notes:
        @param phValue The initial Value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC HandleHolder (phValue AS HANDLE):

        THIS-OBJECT () .

        THIS-OBJECT:Value = phValue .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs by-value comparison with another holder class
        Notes:   The provided object for comparison needs to be of the same type
        @param poObject The object to compare with the current instance
        @return Logical value indicating if this object and the passed in object instance have the same value
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC LOGICAL Equals (poObject AS Progress.Lang.Object):

        Consultingwerk.Assertion.ObjectAssert:IsValid (poObject, "poObject":U) .

        IF TYPE-OF (poObject, HandleHolder) AND CAST (poObject, HandleHolder):Value = THIS-OBJECT:Value THEN
            RETURN TRUE .

        RETURN FALSE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Expression to be used in a Query String
        Notes:   Quoted value
        @return The CHARACTER Expression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetExpression ():

        RETURN QUOTER (THIS-OBJECT:Value) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Value of the Holder instance
        Notes:   Unquoted value
        @return The value as a CHARACTER
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetValue ():

        RETURN STRING (THIS-OBJECT:Value) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a Character representation of the value
        Notes:
        @return The Character representation of the value
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER ToString ():

        RETURN SUBSTITUTE ("HandleHolder (&1 &2)":U,
                           THIS-OBJECT:Value,
                           (IF VALID-HANDLE (THIS-OBJECT:VALUE) THEN THIS-OBJECT:Value:Type ELSE "<invalid handle>"{&TRAN})) .

    END METHOD .

END CLASS.
