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
    File        : DateHolder
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

CLASS Consultingwerk.DateHolder 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    IMPLEMENTS IQueryExpression, IPrimitiveHolder
    {&SERIALIZABLE}: 

    {Consultingwerk/JsonSerializableProperty.i Value DATE} .

    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the DATE unknown value 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Today AS DateHolder NO-UNDO 
    GET:
        RETURN NEW DateHolder (TODAY) .
    END GET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a holder for the DATE unknown value 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UnknownValue AS DateHolder NO-UNDO 
    GET:
        DEFINE VARIABLE oDateHolder AS DateHolder NO-UNDO . 
        
        oDateHolder = NEW DateHolder () .
        oDateHolder:Value = ?.
        
        RETURN oDateHolder .
    END GET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DateHolder class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DateHolder ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DateHolder class                                                                     
        Notes:        
        @param pdtValue The initial Value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DateHolder (pdtValue AS DATE):
        
        THIS-OBJECT () .
        
        THIS-OBJECT:Value = pdtValue . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DateHolder class                                                                     
        Notes:        
        @param pdtValue The initial Value (DATETIME)
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DateHolder (pdtValue AS DATETIME):
        
        THIS-OBJECT () .
        
        THIS-OBJECT:Value = pdtValue . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DateHolder class                                                                     
        Notes:        
        @param pdtValue The initial Value (DATETIME-TZ)
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DateHolder (pdtValue AS DATETIME-TZ):
        
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
        
        IF TYPE-OF (poObject, DateHolder) AND CAST (poObject, DateHolder):Value = THIS-OBJECT:Value THEN 
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
        
        RETURN SUBSTITUTE ("DateHolder (&1)":U, THIS-OBJECT:Value) .

    END METHOD .    
        
END CLASS.
