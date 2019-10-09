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
    File        : LogicalHolder
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

CLASS Consultingwerk.LogicalHolder
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS IQueryExpression, IPrimitiveHolder &IF DEFINED (CommonComponentSpecification) NE 0 &THEN , Ccs.Common.Support.ILogicalHolder &ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i Value LOGICAL} .

    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the LOGICAL False value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY FalseValue AS LogicalHolder NO-UNDO
    GET:
        RETURN NEW LogicalHolder (FALSE) .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the LOGICAL True value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TrueValue AS LogicalHolder NO-UNDO
    GET:
        RETURN NEW LogicalHolder (TRUE) .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the LOGICAL unknown value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UnknownValue AS LogicalHolder NO-UNDO
    GET:
        RETURN NEW LogicalHolder (?) .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LogicalHolder class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LogicalHolder ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LogicalHolder class
        Notes:
        @param plValue The initial Value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LogicalHolder (plValue AS LOGICAL):

        THIS-OBJECT () .

        THIS-OBJECT:Value = plValue .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs by-value comparison with another holder class
        Notes:   The provided object for comparison needs to be of the same type
        @param poObject The object to compare with the current instance
        @return Logical value indicating if this object and the passed in object instance have the same value
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC LOGICAL Equals (poObject AS Progress.Lang.Object):

        Consultingwerk.Assertion.ObjectAssert:IsValid (poObject, "poObject":U) .

        IF TYPE-OF (poObject, LogicalHolder) AND CAST (poObject, LogicalHolder):Value = THIS-OBJECT:Value THEN
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

        RETURN SUBSTITUTE ("LogicalHolder (&1)":U, THIS-OBJECT:Value) .

    END METHOD .

END CLASS.
