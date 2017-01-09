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
    File        : Filter
    Purpose     : Represents a single filter option of the Kendo UI grid 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 07:19:56 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Data.Filter 
    INHERITS JsonSerializable:

    {Consultingwerk/JsonSerializableProperty.i field CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i operator Consultingwerk.Web.Kendo.Data.FilterOperatorEnum}.
    {Consultingwerk/JsonSerializableProperty.i value CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Filter class
        Notes:   
        @param pcField The value for the field
        @param poOperator The value for the operator
        @param pcValue The value for the value
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC Filter (pcField AS CHARACTER,
	                           poOperator AS FilterOperatorEnum,
	                           pcValue AS CHARACTER):
		THIS-OBJECT ().
		
		ASSIGN THIS-OBJECT:field    = pcField
		       THIS-OBJECT:operator = poOperator 
		       THIS-OBJECT:value    = pcValue  . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Filter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Filter ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

END CLASS.
