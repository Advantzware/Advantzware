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
    File        : InQueryExpression
    Purpose     : Query expression for IN style expressions, returning 
                  records where the value of the given field is contained 
                  in the given list
    Syntax      : 
    Description : Evaluated to LOOKUP (field, "list1,list2,list3") > 0
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

CLASS Consultingwerk.InQueryExpression 
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
        Purpose: Gets and sets list of the QueryExpression
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i List CHARACTER}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InQueryExpression class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC InQueryExpression ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InQueryExpression class
        Notes:   
        @param pcFieldName The field name for the IN Expression 
        @param pcList The list to use for the IN Expression
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InQueryExpression (pcFieldName AS CHARACTER, 
                                          pcList AS CHARACTER):
        
        THIS-OBJECT (NEW BufferFieldName (pcFieldName), pcList).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InQueryExpression class
        Notes:   
        @param poFieldName The BufferFieldName reference for the IN Expression 
        @param pcList The list to use for the IN Expression
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InQueryExpression (poFieldName AS BufferFieldName, 
                                          pcList AS CHARACTER):
        SUPER ().
                
        ASSIGN THIS-OBJECT:FieldName = poFieldName 
               THIS-OBJECT:List      = pcList  .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Expression to be used in a Query String
        Notes:   
        @return The CHARACTER Expression  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetExpression ():
        
        /* Mike Fechner, Consultingwerk Ltd. 09.01.2015
           As long as SCL-136 is not resolved, we cannot have space
           characters in this expression*/
        RETURN SUBSTITUTE ("LOOKUP (&1, &2)>0":U, 
                           THIS-OBJECT:FieldName:GetExpression(),
                           QUOTER (THIS-OBJECT:List)) .
 
    END METHOD . 

END CLASS.
