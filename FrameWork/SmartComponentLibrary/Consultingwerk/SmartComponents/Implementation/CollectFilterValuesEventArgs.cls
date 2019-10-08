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
    File        : CollectFilterValuesEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 24 15:46:31 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                FROM PROPATH . 
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH . 
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH . 
USING Progress.Lang.*                                 FROM PROPATH .
USING System.EventArgs                                FROM ASSEMBLY . 

CLASS Consultingwerk.SmartComponents.Implementation.CollectFilterValuesEventArgs
    INHERITS Consultingwerk.EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Cancel attribute                                                                     
        Notes:   Allows cancelling the Collection of Filter values                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Cancel AS LOGICAL NO-UNDO INIT FALSE  
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Query Expressions for this event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QueryExpressions AS ListQueryExpressionByTable NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CollectFilterValuesEventArgs class
        Notes:   
        @param poListQueryExpression This Dictionary of Query Expressions for the event
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CollectFilterValuesEventArgs (poListQueryExpression AS ListQueryExpressionByTable):
        SUPER  ().

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poListQueryExpression """ListQueryExpressionByTable"":U"} .

        THIS-OBJECT:QueryExpressions = poListQueryExpression . 

    END CONSTRUCTOR.

END CLASS.
