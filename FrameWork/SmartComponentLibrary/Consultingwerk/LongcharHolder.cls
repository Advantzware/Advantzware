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
    File        : LongcharHolder
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

CLASS Consultingwerk.LongcharHolder
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS IQueryExpression, IPrimitiveHolder &IF DEFINED (CommonComponentSpecification) NE 0 &THEN , Ccs.Common.Support.ILongcharHolder &ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i Value LONGCHAR} .

    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the LONGHAR unknown value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UnknownValue AS LongcharHolder NO-UNDO
    GET:
        RETURN NEW LongcharHolder (?) .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LongcharHolder class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LongcharHolder ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        FIX-CODEPAGE (THIS-OBJECT:Value) = Codepages:UTF-8 .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LongcharHolder class
        Notes:
        @param pcValue The initial Value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LongcharHolder (pcValue AS LONGCHAR):

        THIS-OBJECT () .

        THIS-OBJECT:Value = pcValue .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs by-value comparison with another holder class
        Notes:   The provided object for comparison needs to be of the same type
        @param poObject The object to compare with the current instance
        @return Logical value indicating if this object and the passed in object instance have the same value
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC LOGICAL Equals (poObject AS Progress.Lang.Object):

        Consultingwerk.Assertion.ObjectAssert:IsValid (poObject, "poObject":U) .

        IF TYPE-OF (poObject, LongcharHolder) AND CAST (poObject, LongcharHolder):Value = THIS-OBJECT:Value THEN
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

        RETURN SUBSTITUTE ("LongcharHolder (&1)":U, SUBSTRING (THIS-OBJECT:Value, 1, 80, "CHARACTER":U)) .

    END METHOD .

END CLASS.
