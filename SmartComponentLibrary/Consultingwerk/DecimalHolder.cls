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
    File        : DecimalHolder
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

CLASS Consultingwerk.DecimalHolder 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    IMPLEMENTS IQueryExpression, IPrimitiveHolder 
    {&SERIALIZABLE}:
        
    {Consultingwerk/JsonSerializableProperty.i Value DECIMAL} .

    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the DECIMAL unknown value 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UnknownValue AS DecimalHolder NO-UNDO 
    GET:
        RETURN NEW DecimalHolder (?) .
    END GET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the DECIMAL zero value 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Zero AS DecimalHolder NO-UNDO 
    GET:
        RETURN NEW DecimalHolder (0) .
    END GET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DecimalHolder class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DecimalHolder ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DecimalHolder class                                                                     
        Notes:        
        @param pdeValue The initial Value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DecimalHolder (pdeValue AS DECIMAL):
        
        THIS-OBJECT () .
        
        THIS-OBJECT:Value = pdeValue . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs by-value comparison with another holder class
        Notes:   The provided object for comparison needs to be of the same type
        @param poObject The object to compare with the current instance
        @return Logical value indicating if this object and the passed in object instance have the same value  
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC LOGICAL Equals (poObject AS Progress.Lang.Object):
        
        Consultingwerk.Assertion.ObjectAssert:IsNotValid (poObject, "poObject":U) .
        
        IF TYPE-OF (poObject, DecimalHolder) AND CAST (poObject, DecimalHolder):Value = THIS-OBJECT:Value THEN 
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
         
    END.

    /*------------------------------------------------------------------------------
        Purpose: Returns a Character representation of the value
        Notes:   
        @return The Character representation of the value
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER ToString ():
        
        RETURN SUBSTITUTE ("DecimalrHolder (&1)":U, THIS-OBJECT:Value) .

    END METHOD . 
           
END CLASS.
