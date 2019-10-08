/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : CanDoQueryExpression
    Purpose     : Query expression for CAN-DO style expressions, returning 
                  records where the value of the given field is contained 
                  in the given list using the CAN-DO operator
    Syntax      : 
    Description : Evaluated to CAN-DO ("value", field) 
                  Warning: Results in non-indexed query execution!
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 09 16:51:43 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*           FROM PROPATH .  
USING Consultingwerk.Assertion.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.CanDoQueryExpression 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    IMPLEMENTS IQueryExpression: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets fieldname of the QueryExpression
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i FieldName Consultingwerk.BufferFieldName}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets value of the QueryExpression
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Value CHARACTER}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CanDoQueryExpression class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CanDoQueryExpression ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CanDoQueryExpression class
        Notes:   
        @param pcValue The value to use for the CAN-DO Expression
        @param pcFieldName The field name for the CAN-DO Expression 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CanDoQueryExpression (pcValue AS CHARACTER,
                                             pcFieldName AS CHARACTER):
        
        THIS-OBJECT (pcValue, NEW BufferFieldName (pcFieldName)).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CanDoQueryExpression class
        Notes:   
        @param pcValue The calue to use for the CAN-DO Expression
        @param poFieldName The BufferFieldName reference for the CAN-DO Expression 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CanDoQueryExpression (pcValue AS CHARACTER,
                                             poFieldName AS BufferFieldName):
        SUPER ().
                
        ASSIGN THIS-OBJECT:FieldName = poFieldName 
               THIS-OBJECT:Value     = pcValue  .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Expression to be used in a Query String
        Notes:   
        @return The CHARACTER Expression  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetExpression ():
        
        RETURN SUBSTITUTE ("CAN-DO (&1, &2)":U, 
                           QUOTER (THIS-OBJECT:Value),
                           THIS-OBJECT:FieldName:GetExpression()) .
 
    END METHOD . 

END CLASS.
