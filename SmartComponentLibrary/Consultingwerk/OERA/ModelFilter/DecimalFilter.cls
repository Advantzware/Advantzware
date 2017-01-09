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
    File        : DecimalFilter
    Purpose     : Provides Filter methods for DECIMAL Buffer Fields
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

CLASS Consultingwerk.OERA.ModelFilter.DecimalFilter: 

    DEFINE PRIVATE VARIABLE hBufferField     AS HANDLE             NO-UNDO . 
    DEFINE PRIVATE VARIABLE oFilterTarget    AS IModelFilterTarget NO-UNDO . 
    DEFINE PRIVATE VARIABLE oFilter          AS TableModelFilter   NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DecimalFilter class
        Notes:   
        @param phBufferField The BufferField this Filter works on
        @param poFilterTarget The IModelFilterTarget to add the QueryExpression to 
        @param poFilter The reference to the TableModelFilter instance
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DecimalFilter (phBufferField AS HANDLE,
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
        Purpose: Retuns an EQ Query Expression
        Notes:   
        @param pdeValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter EQ (pdeValue AS DECIMAL):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:EQ,
                                          pdeValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an GE Query Expression
        Notes:   
        @param pdeValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter GE (pdeValue AS DECIMAL):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:GE,
                                          pdeValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an GT Query Expression
        Notes:   
        @param pdeValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter GT (pdeValue AS DECIMAL):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:GT,
                                          pdeValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns a Query Expression
        Notes:   
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter IsNotUnknownValue ():
        
        DEFINE VARIABLE deUnknown AS DECIMAL NO-UNDO INIT ?.
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:NE,
                                          deUnknown)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an LE Query Expression
        Notes:   
        @param pdeValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter LE (pdeValue AS DECIMAL):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:LE,
                                          pdeValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an LT Query Expression
        Notes:   
        @param pdeValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter LT (pdeValue AS DECIMAL):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:LT,
                                          pdeValue)) .   
        
        RETURN oFilter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retuns an NE Query Expression
        Notes:   
        @param pdeValue The value for the Expression
        @return The reference to the TableModel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModelFilter NE (pdeValue AS DECIMAL):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .
        
        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:NE,
                                          pdeValue)) .   
        
        RETURN oFilter . 

    END METHOD .

END CLASS.
