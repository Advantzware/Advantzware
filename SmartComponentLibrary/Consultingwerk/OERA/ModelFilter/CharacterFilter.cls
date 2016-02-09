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
    File        : CharacterFilter
    Purpose     : Provides Filter methods for CHARACTER Buffer Fields
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 15 02:19:36 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                  FROM PROPATH . 
USING Consultingwerk.Assertion.*        FROM PROPATH . 
USING Consultingwerk.OERA.*             FROM PROPATH . 
USING Consultingwerk.OERA.ModelFilter.* FROM PROPATH .  
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.OERA.ModelFilter.CharacterFilter: 

    DEFINE PRIVATE VARIABLE hBufferField     AS HANDLE             NO-UNDO . 
    DEFINE PRIVATE VARIABLE oFilterTarget    AS IModelFilterTarget NO-UNDO . 
    DEFINE PRIVATE VARIABLE oFilter          AS TableModelFilter   NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CharacterFilter class
        Notes:   
        @param phBufferField The BufferField this Filter works on
        @param poFilterTarget The IModelFilterTarget to add the QueryExpression to 
        @param poFilter The reference to the TableModelFilter instance
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CharacterFilter (phBufferField AS HANDLE,
	                                    poFilterTarget AS IModelFilterTarget,
	                                    poFilter AS TableModelFilter):
		SUPER ().
		
		{Consultingwerk/Assertion/HandleAssert/WidgetType.i  phBufferField WidgetTypeEnum:BufferField} .
		
		{Consultingwerk/Assertion/ObjectAssert/IsValid.i poFilterTarget """IModelFilterTarget"":U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poFilter """TableModelFilter"":U"} .
		
		ASSIGN hBufferField     = phBufferField 
		       oFilterTarget    = poFilterTarget
		       oFilter          = poFilter .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Retuns a BEGINS Query Expression
        Notes:   
        @param pcValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC TableModelFilter Begins (pcValue AS CHARACTER):
		
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
		
		oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:Begins,
                                          pcValue)) .   
		
		RETURN oFilter . 
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an CAN-DO Query Expression
        Notes:   
        @param pcList The list to use for the InQueryExpression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter CanDo (pcList AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW CanDoQueryExpression 
                                                (pcList, 
                                                 NEW BufferFieldName (hBufferField))) .   

        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns a CONTAINS Query Expression
        Notes:   
        @param pcValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter Contains (pcValue AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:Contains,
                                          pcValue)) .   
        
        RETURN oFilter. 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an EQ Query Expression
        Notes:   
        @param pcValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter EQ (pcValue AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:EQ,
                                          pcValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an GE Query Expression
        Notes:   
        @param pcValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter GE (pcValue AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:GE,
                                          pcValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an GT Query Expression
        Notes:   
        @param pcValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter GT (pcValue AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:GT,
                                          pcValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an IN Query Expression
        Notes:   
        @param pcList The list to use for the InQueryExpression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter In (pcList AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW InQueryExpression 
                                                (NEW BufferFieldName (hBufferField),
                                                 pcList)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns a Query Expression
        Notes:   
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter IsNotUnknownValue ():
        
        DEFINE VARIABLE cUnknown AS CHARACTER NO-UNDO INIT ?.
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:NE,
                                          cUnknown)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an LE Query Expression
        Notes:   
        @param pcValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter LE (pcValue AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:LE,
                                          pcValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an LT Query Expression
        Notes:   
        @param pcValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter LT (pcValue AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:LT,
                                          pcValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an Matches Query Expression
        Notes:   Uses Begins operator when the pattern is only using an * as the last character
                 Uses EQ when the pattern does not use any wildcard   
        @param pcValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter Matches (pcValue AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        /* Mike Fechner, Consultingwerk Ltd. 26.03.2014
           When there are no wild cards, use EQ */
        IF INDEX (pcValue, ".":U) = 0 AND INDEX (pcValue, "*":U) = 0 THEN 
            RETURN THIS-OBJECT:EQ (pcValue) .

        /* Mike Fechner, Consultingwerk Ltd. 26.03.2014
           When the only wildcard is an * at the end, use Begins */
        IF INDEX (pcValue, ".":U) = 0 AND INDEX (pcValue, "*":U) = LENGTH (pcValue) THEN 
            RETURN THIS-OBJECT:Begins (pcValue) .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:Matches,
                                          pcValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an NE Query Expression
        Notes:   
        @param pcValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter NE (pcValue AS CHARACTER):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:NE,
                                          pcValue)) .   
        
        RETURN oFilter . 

    END METHOD .

END CLASS.
