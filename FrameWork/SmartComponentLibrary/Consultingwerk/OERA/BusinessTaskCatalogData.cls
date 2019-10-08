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
    File        : BusinessTaskCatalogData
    Purpose     : Describes the REST/JSDO Interface to a Business Task
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 08 11:54:08 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.OERA.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.OERA.BusinessTaskCatalogData
    IMPLEMENTS IBusinessTaskCatalogData:

    /**
     * Purpose: Returns the name of the Business Task
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BusinessTaskName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the List of IBusinessTaskMethodData instances
     *          for the Business Task
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Methods AS ListBusinessTaskMethodData NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData ():

        THIS-OBJECT:Methods = NEW ListBusinessTaskMethodData () .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:
     * @param pcBusinessTaskName The name of the Business Task
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData (pcBusinessTaskName AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:   Initializes a single Method Info instance
     * @param pcBusinessTaskName The name of the Business Task
     * @param pcMethodName The name of the Business Task Method
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData (pcBusinessTaskName AS CHARACTER,
                                                pcMethodName AS CHARACTER,
                                                poParameterClass AS Progress.Lang.Class):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

        THIS-OBJECT:Methods:Add (pcMethodName,
                                 poParameterClass) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:   Initializes a single Method Info instance
     * @param pcBusinessTaskName The name of the Business Task
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandles The array of the dataset handles (empty datasets to describe the interface)
     * @param poDatasetParameterMode The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData (pcBusinessTaskName AS CHARACTER,
                                                pcMethodName AS CHARACTER,
                                                phDatasetHandles AS HANDLE EXTENT,
                                                poDatasetParameterMode AS DatasetParameterModeEnum EXTENT,
                                                poParameterClass AS Progress.Lang.Class):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

        THIS-OBJECT:Methods:Add (pcMethodName,
                                 phDatasetHandles,
                                 poDatasetParameterMode,
                                 poParameterClass) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:   Initializes a single Method Info instance, with only a single dataset parameter
     * @param pcBusinessTaskName The name of the Business Task
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData (pcBusinessTaskName AS CHARACTER,
                                                pcMethodName AS CHARACTER,
                                                phDatasetHandle AS HANDLE,
                                                poDatasetParameterMode AS DatasetParameterModeEnum,
                                                poParameterClass AS Progress.Lang.Class):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

        THIS-OBJECT:Methods:Add (pcMethodName,
                                 phDatasetHandle,
                                 poDatasetParameterMode,
                                 poParameterClass) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:   Initializes a single Method Info instance, with only a single dataset parameter
     * @param pcBusinessTaskName The name of the Business Task
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle1 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode1 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle2 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode2 The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData (pcBusinessTaskName AS CHARACTER,
                                                pcMethodName AS CHARACTER,
                                                phDatasetHandle1 AS HANDLE,
                                                poDatasetParameterMode1 AS DatasetParameterModeEnum,
                                                phDatasetHandle2 AS HANDLE,
                                                poDatasetParameterMode2 AS DatasetParameterModeEnum,
                                                poParameterClass AS Progress.Lang.Class):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

        THIS-OBJECT:Methods:Add (pcMethodName,
                                 phDatasetHandle1,
                                 poDatasetParameterMode1,
                                 phDatasetHandle2,
                                 poDatasetParameterMode2,
                                 poParameterClass) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:   Initializes a single Method Info instance, with only a single dataset parameter
     * @param pcBusinessTaskName The name of the Business Task
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle1 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode1 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle2 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode2 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle3 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode3 The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData (pcBusinessTaskName AS CHARACTER,
                                                pcMethodName AS CHARACTER,
                                                phDatasetHandle1 AS HANDLE,
                                                poDatasetParameterMode1 AS DatasetParameterModeEnum,
                                                phDatasetHandle2 AS HANDLE,
                                                poDatasetParameterMode2 AS DatasetParameterModeEnum,
                                                phDatasetHandle3 AS HANDLE,
                                                poDatasetParameterMode3 AS DatasetParameterModeEnum,
                                                poParameterClass AS Progress.Lang.Class):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

        THIS-OBJECT:Methods:Add (pcMethodName,
                                 phDatasetHandle1,
                                 poDatasetParameterMode1,
                                 phDatasetHandle2,
                                 poDatasetParameterMode2,
                                 phDatasetHandle3,
                                 poDatasetParameterMode3,
                                 poParameterClass) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:   Initializes a single Method Info instance, with only a single dataset parameter
     * @param pcBusinessTaskName The name of the Business Task
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle1 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode1 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle2 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode2 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle3 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode3 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle4 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode4 The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData (pcBusinessTaskName AS CHARACTER,
                                                pcMethodName AS CHARACTER,
                                                phDatasetHandle1 AS HANDLE,
                                                poDatasetParameterMode1 AS DatasetParameterModeEnum,
                                                phDatasetHandle2 AS HANDLE,
                                                poDatasetParameterMode2 AS DatasetParameterModeEnum,
                                                phDatasetHandle3 AS HANDLE,
                                                poDatasetParameterMode3 AS DatasetParameterModeEnum,
                                                phDatasetHandle4 AS HANDLE,
                                                poDatasetParameterMode4 AS DatasetParameterModeEnum,
                                                poParameterClass AS Progress.Lang.Class):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

        THIS-OBJECT:Methods:Add (pcMethodName,
                                 phDatasetHandle1,
                                 poDatasetParameterMode1,
                                 phDatasetHandle2,
                                 poDatasetParameterMode2,
                                 phDatasetHandle3,
                                 poDatasetParameterMode3,
                                 phDatasetHandle4,
                                 poDatasetParameterMode4,
                                 poParameterClass) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:   Initializes a single Method Info instance, with only a single dataset parameter
     * @param pcBusinessTaskName The name of the Business Task
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle1 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode1 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle2 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode2 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle3 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode3 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle4 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode4 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle5 The dataset handle (empty datasets to describe the interface)
     * @param poDatasetParameterMode5 The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData (pcBusinessTaskName AS CHARACTER,
                                                pcMethodName AS CHARACTER,
                                                phDatasetHandle1 AS HANDLE,
                                                poDatasetParameterMode1 AS DatasetParameterModeEnum,
                                                phDatasetHandle2 AS HANDLE,
                                                poDatasetParameterMode2 AS DatasetParameterModeEnum,
                                                phDatasetHandle3 AS HANDLE,
                                                poDatasetParameterMode3 AS DatasetParameterModeEnum,
                                                phDatasetHandle4 AS HANDLE,
                                                poDatasetParameterMode4 AS DatasetParameterModeEnum,
                                                phDatasetHandle5 AS HANDLE,
                                                poDatasetParameterMode5 AS DatasetParameterModeEnum,
                                                poParameterClass AS Progress.Lang.Class):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

        THIS-OBJECT:Methods:Add (pcMethodName,
                                 phDatasetHandle1,
                                 poDatasetParameterMode1,
                                 phDatasetHandle2,
                                 poDatasetParameterMode2,
                                 phDatasetHandle3,
                                 poDatasetParameterMode3,
                                 phDatasetHandle4,
                                 poDatasetParameterMode4,
                                 phDatasetHandle5,
                                 poDatasetParameterMode5,
                                 poParameterClass) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BusinessTaskCatalogData class
     * Notes:   Adds an array of Method Info instances
     * @param pcBusinessTaskName The name of the Business Task
     * @param poMethods The array of Business Task Method Data instances
     */
    CONSTRUCTOR PUBLIC BusinessTaskCatalogData (pcBusinessTaskName AS CHARACTER,
                                                poMethods AS IBusinessTaskMethodData EXTENT):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

        THIS-OBJECT:Methods:Add (poMethods) .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the IBusinessTaskMethodData instance for the given
     *          Method Name
     * Notes:   Throwns an error when the Method name is not known
     * @param pcMethodName The name of the method to return
     * @return The IBusinessTaskMethodData instance
     */
    METHOD PUBLIC IBusinessTaskMethodData GetMethod (pcMethodName AS CHARACTER):

        {Consultingwerk/linqABL.i IBusinessTaskMethodData oMethod in THIS-OBJECT:Methods
                    where MethodName = pcMethodName}
            RETURN oMethod .
        END.

        UNDO, THROW NEW InvalidParameterValueException ("pcMethodName":U, pcMethodName, THIS-OBJECT:GetClass():TypeName) .

    END METHOD.

END CLASS.
