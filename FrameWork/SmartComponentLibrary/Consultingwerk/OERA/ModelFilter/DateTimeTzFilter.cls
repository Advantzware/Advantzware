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
    File        : DateTimeTzFilter
    Purpose     : Provides Filter methods for DATETIME-TZ Buffer Fields
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

CLASS Consultingwerk.OERA.ModelFilter.DateTimeTzFilter:

    DEFINE PRIVATE VARIABLE hBufferField     AS HANDLE             NO-UNDO .
    DEFINE PRIVATE VARIABLE oFilterTarget    AS IModelFilterTarget NO-UNDO .
    DEFINE PRIVATE VARIABLE oFilter          AS TableModelFilter   NO-UNDO .

    /**
     * Purpose: Constructor for the DateTimeTzFilter class
     * Notes:
     * @param phBufferField The BufferField this Filter works on
     * @param poFilterTarget The IModelFilterTarget to add the QueryExpression to
     * @param poFilter The reference to the TableModelFilter instance
     */
    CONSTRUCTOR PUBLIC DateTimeTzFilter (phBufferField AS HANDLE,
                                         poFilterTarget AS IModelFilterTarget,
                                         poFilter AS TableModelFilter):
        SUPER ().

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBufferField WidgetTypeEnum:BufferField} .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poFilterTarget """IModelFilterTarget"":U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poFilter """TableModelFilter"":U"} .

        ASSIGN hBufferField     = phBufferField
               oFilterTarget      = poFilterTarget
               oFilter          = poFilter .

    END CONSTRUCTOR.

    /**
     * Purpose: Retuns an EQ Query Expression
     * Notes:
     * @param pdtValue The value for the Expression
     * @return The reference to the TableModel
     */
    METHOD PUBLIC TableModelFilter EQ (pdtValue AS DATETIME-TZ):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .

        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:EQ,
                                          pdtValue)) .

        RETURN oFilter .

    END METHOD .

    /**
     * Purpose: Retuns an GE Query Expression
     * Notes:
     * @param pdtValue The value for the Expression
     * @return The reference to the TableModel
     */
    METHOD PUBLIC TableModelFilter GE (pdtValue AS DATETIME-TZ):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .

        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:GE,
                                          pdtValue)) .

        RETURN oFilter .

    END METHOD .

    /**
     * Purpose: Retuns an GT Query Expression
     * Notes:
     * @param pdtValue The value for the Expression
     * @return The reference to the TableModel
     */
    METHOD PUBLIC TableModelFilter GT (pdtValue AS DATETIME-TZ):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .

        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:GT,
                                          pdtValue)) .

        RETURN oFilter .

    END METHOD .

    /**
     * Purpose: Retuns a Query Expression
     * Notes:
     * @return The reference to the TableModel
     */
    METHOD PUBLIC TableModelFilter IsNotUnknownValue ():

        DEFINE VARIABLE dtUnknown AS DATETIME-TZ NO-UNDO INITIAL ?.

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .

        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:NE,
                                          dtUnknown)) .

        RETURN oFilter .

    END METHOD .

    /**
     * Purpose: Retuns an LE Query Expression
     * Notes:
     * @param pdtValue The value for the Expression
     * @return The reference to the TableModel
     */
    METHOD PUBLIC TableModelFilter LE (pdtValue AS DATETIME-TZ):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .

        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:LE,
                                          pdtValue)) .

        RETURN oFilter .

    END METHOD .

    /**
     * Purpose: Retuns an LT Query Expression
     * Notes:
     * @param pdtValue The value for the Expression
     * @return The reference to the TableModel
     */
    METHOD PUBLIC TableModelFilter LT (pdtValue AS DATETIME-TZ):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .

        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:LT,
                                          pdtValue)) .

        RETURN oFilter .

    END METHOD .

    /**
     * Purpose: Retuns an NE Query Expression
     * Notes:
     * @param pdtValue The value for the Expression
     * @return The reference to the TableModel
     */
    METHOD PUBLIC TableModelFilter NE (pdtValue AS DATETIME-TZ):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oFilterTarget """TableModel"":U"} .

        oFilterTarget:AddQueryExpression (NEW QueryExpression (NEW BufferFieldName (hBufferField),
                                          OperatorEnum:NE,
                                          pdtValue)) .

        RETURN oFilter .

    END METHOD .

END CLASS.
