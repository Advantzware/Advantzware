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
    File        : CharacterHolder
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

CLASS Consultingwerk.DateTimeHolder 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    IMPLEMENTS IQueryExpression, IPrimitiveHolder
    {&SERIALIZABLE}: 

    {Consultingwerk/JsonSerializableProperty.i Value DATETIME} .

    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the DATETIME NOW value 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Now AS DateTimeHolder NO-UNDO 
    GET:
        DEFINE VARIABLE dtNow AS DATETIME NO-UNDO INIT NOW .
        RETURN NEW DateTimeHolder (dtNow) .
    END GET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the DATETIME TODAY value 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Today AS DateTimeHolder NO-UNDO 
    GET:
        RETURN NEW DateTimeHolder (TODAY) .
    END GET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the DATETIME unknown value 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UnknownValue AS DateTimeHolder NO-UNDO 
    GET:
        RETURN NEW DateTimeHolder (?) .
    END GET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CharacterHolder class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DateTimeHolder ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DateTimeHolder class                                                                     
        Notes:        
        @param pdtValue The initial Value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DateTimeHolder (pdtValue AS DATETIME):
        
        THIS-OBJECT () .
        
        THIS-OBJECT:Value = pdtValue . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs by-value comparison with another holder class
        Notes:   The provided object for comparison needs to be of the same type
        @param poObject The object to compare with the current instance
        @return Logical value indicating if this object and the passed in object instance have the same value  
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC LOGICAL Equals (poObject AS Progress.Lang.Object):
        
        Consultingwerk.Assertion.ObjectAssert:IsNotValid (poObject, "poObject":U) .
        
        IF TYPE-OF (poObject, DateTimeHolder) AND CAST (poObject, DateTimeHolder):Value = THIS-OBJECT:Value THEN 
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
        
        RETURN SUBSTITUTE ("DateTimeHolder (&1)":U, THIS-OBJECT:Value) .

    END METHOD .    
    
END CLASS.
