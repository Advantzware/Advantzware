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
    File        : ListQueryExpressionByTable
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 11 16:50:47 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                FROM PROPATH . 
USING Consultingwerk.Framework.Base.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.ListQueryExpressionByTable 
    INHERITS GenericDictionary: 

    { Consultingwerk/Framework/Base/GenericDictionary.i ListQueryExpression}

    /*------------------------------------------------------------------------------
        Purpose: Returns if the ListQueryExpressionByTable is empty
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IsEmpty AS LOGICAL NO-UNDO 
    GET():
        
        {Consultingwerk/foreachABL.i ListQueryExpression oExpression in this-object}
        
            IF oExpression:IsEmpty = FALSE THEN 
                RETURN FALSE . 
        END.
        
        RETURN TRUE . 
        
    END GET.

END CLASS.
