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
    File        : ILogicalValueProvider
    Purpose     : Interface for classes that can provide a logical value 
                  (TRUE/FALSE) to the LogicalExpressionParser
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 21 10:26:18 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.ExpressionParser.* FROM PROPATH .  
USING Progress.Lang.*                             FROM PROPATH .

INTERFACE Consultingwerk.Framework.ExpressionParser.ILogicalValueProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Parses a CHARACTER Expression and returns the logical result
        Notes:   
        @param pcExpression The Character expression to parse
        @return The logical result of the expressioln
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL GetLogicalValue (pcExpression AS CHARACTER) .

END INTERFACE.
