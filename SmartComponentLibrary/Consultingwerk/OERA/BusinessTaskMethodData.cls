/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : BusinessTaskMethodData
    Purpose     : Describes a Business Task Method for REST/JSDO Catalog
                  generation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 08 11:33:58 CEST 2016
    Notes       : Not serializable, as the Dataset Handle Parameters would
                  not be serializable
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* FROM PROPATH.
USING Consultingwerk.Exceptions.* FROM PROPATH.
USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.BusinessTaskMethodData
    IMPLEMENTS IBusinessTaskMethodData:

    /**
     * Purpose: Gets and sets the array of the dataset handles (empty datasets
     *          to describe the interface)
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DatasetHandles AS HANDLE EXTENT NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the mode of the dataset parameter (input, output,
     *          input-output)
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DatasetParameterMode AS DatasetParameterModeEnum EXTENT NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the name of the Business Task Method
     * Notes:
     */
    DEFINE PUBLIC PROPERTY MethodName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the  name of the class for the parameter object
     *          (non abstract, non interface) or ?
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ParameterClass AS Progress.Lang.Class NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the BusinessTaskMethodData class
     * Notes:
     */
    CONSTRUCTOR PUBLIC BusinessTaskMethodData ():

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskMethodData class
     * Notes:
     * @param pcMethodName The name of the Business Task Method
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     */
    CONSTRUCTOR PUBLIC BusinessTaskMethodData (pcMethodName AS CHARACTER,
                                               poParameterClass AS Progress.Lang.Class):

        Assert:NotNullOrEmpty (pcMethodName, "pcMethodName":U) .

        ObjectAssert:IsValid (poParameterClass, "poParameterClass":U) .

        ASSIGN THIS-OBJECT:MethodName           = pcMethodName
               THIS-OBJECT:ParameterClass       = poParameterClass .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskMethodData class
     * Notes:
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandles The array of the dataset handles (empty datasets to describe the interface)
     * @param poDatasetParameterMode The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     */
    CONSTRUCTOR PUBLIC BusinessTaskMethodData (pcMethodName AS CHARACTER,
                                               phDatasetHandles AS HANDLE EXTENT,
                                               poDatasetParameterMode AS DatasetParameterModeEnum EXTENT,
                                               poParameterClass AS Progress.Lang.Class):

        Assert:NotNullOrEmpty (pcMethodName, "pcMethodName":U) .

        IF EXTENT (phDatasetHandles) <> EXTENT (poDatasetParameterMode) THEN
            UNDO, THROW NEW InvalidParameterValueException ("poDatasetParameterMode":U,
                                                            THIS-OBJECT:GetClass():TypeName,
                                                            "The extent size of phDatasetHandles and poDatasetParameterMode must match."{&TRAN},
                                                            0) .
        ArrayAssert:ValidEntriesOnly (phDatasetHandles, "phDatasetHandles":U) .
        ArrayAssert:ValidEntriesOnly (poDatasetParameterMode, "poDatasetParameterMode":U) .
        ObjectAssert:IsValid (poParameterClass, "poParameterClass":U) .

        ASSIGN THIS-OBJECT:MethodName           = pcMethodName
               THIS-OBJECT:DatasetHandles       = phDatasetHandles
               THIS-OBJECT:DatasetParameterMode = poDatasetParameterMode
               THIS-OBJECT:ParameterClass       = poParameterClass .

    END CONSTRUCTOR.

END CLASS.
