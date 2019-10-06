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
    File        : ListBusinessTaskMethodData
    Purpose     : List of IBusinessTaskMethodData instances
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 08 11:21:18 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Consultingwerk.OERA.*           FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.OERA.ListBusinessTaskMethodData
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i Consultingwerk.OERA.IBusinessTaskMethodData}


    /**
     * Purpose: Adds a new Item to the list
     * Notes:
     * @param pcMethodName The name of the Business Task Method
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     * @return The new BusinessTaskMethodData instance
     */
    METHOD PUBLIC BusinessTaskMethodData Add (pcMethodName AS CHARACTER,
                                              poParameterClass AS Progress.Lang.Class):

        RETURN CAST (THIS-OBJECT:Add (NEW BusinessTaskMethodData (pcMethodName,
                                                                  poParameterClass)),
                     BusinessTaskMethodData) .

    END METHOD.

    /**
     * Purpose: Adds a new Item to the list
     * Notes:
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandles The array of the dataset handles (empty datasets to describe the interface)
     * @param poDatasetParameterMode The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     * @return The new BusinessTaskMethodData instance
     */
    METHOD PUBLIC BusinessTaskMethodData Add (pcMethodName AS CHARACTER,
                                              phDatasetHandles AS HANDLE EXTENT,
                                              poDatasetParameterMode AS DatasetParameterModeEnum EXTENT,
                                              poParameterClass AS Progress.Lang.Class):

        RETURN CAST (THIS-OBJECT:Add (NEW BusinessTaskMethodData (pcMethodName,
                                                                  phDatasetHandles,
                                                                  poDatasetParameterMode,
                                                                  poParameterClass)),
                     BusinessTaskMethodData) .

    END METHOD.

    /**
     * Purpose: Adds a new Item to the list
     * Notes:   Variant of the method for only a single Dataset Parameter (no array)
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     * @return The new BusinessTaskMethodData instance
     */
    METHOD PUBLIC BusinessTaskMethodData Add (pcMethodName AS CHARACTER,
                                              phDatasetHandle AS HANDLE,
                                              poDatasetParameterMode AS DatasetParameterModeEnum,
                                              poParameterClass AS Progress.Lang.Class):

        DEFINE VARIABLE hDatasetHandles       AS HANDLE                   EXTENT 1 NO-UNDO.
        DEFINE VARIABLE oDatasetParameterMode AS DatasetParameterModeEnum EXTENT 1 NO-UNDO .

        ASSIGN hDatasetHandles[1]       = phDatasetHandle
               oDatasetParameterMode[1] = poDatasetParameterMode .

        RETURN CAST (THIS-OBJECT:Add (NEW BusinessTaskMethodData (pcMethodName,
                                                                  hDatasetHandles,
                                                                  oDatasetParameterMode,
                                                                  poParameterClass)),
                     BusinessTaskMethodData) .

    END METHOD.

    /**
     * Purpose: Adds a new Item to the list
     * Notes:   Variant of the method for only a single Dataset Parameter (no array)
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle1 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode1 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle2 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode2 The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     * @return The new BusinessTaskMethodData instance
     */
    METHOD PUBLIC BusinessTaskMethodData Add (pcMethodName AS CHARACTER,
                                              phDatasetHandle1 AS HANDLE,
                                              poDatasetParameterMode1 AS DatasetParameterModeEnum,
                                              phDatasetHandle2 AS HANDLE,
                                              poDatasetParameterMode2 AS DatasetParameterModeEnum,
                                              poParameterClass AS Progress.Lang.Class):

        DEFINE VARIABLE hDatasetHandles       AS HANDLE                   EXTENT 2 NO-UNDO.
        DEFINE VARIABLE oDatasetParameterMode AS DatasetParameterModeEnum EXTENT 2 NO-UNDO .

        ASSIGN hDatasetHandles[1]       = phDatasetHandle1
               oDatasetParameterMode[1] = poDatasetParameterMode1
               hDatasetHandles[2]       = phDatasetHandle2
               oDatasetParameterMode[2] = poDatasetParameterMode2
               .

        RETURN CAST (THIS-OBJECT:Add (NEW BusinessTaskMethodData (pcMethodName,
                                                                  hDatasetHandles,
                                                                  oDatasetParameterMode,
                                                                  poParameterClass)),
                     BusinessTaskMethodData) .

    END METHOD.

    /**
     * Purpose: Adds a new Item to the list
     * Notes:   Variant of the method for only a single Dataset Parameter (no array)
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle1 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode1 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle2 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode2 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle3 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode3 The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     * @return The new BusinessTaskMethodData instance
     */
    METHOD PUBLIC BusinessTaskMethodData Add (pcMethodName AS CHARACTER,
                                              phDatasetHandle1 AS HANDLE,
                                              poDatasetParameterMode1 AS DatasetParameterModeEnum,
                                              phDatasetHandle2 AS HANDLE,
                                              poDatasetParameterMode2 AS DatasetParameterModeEnum,
                                              phDatasetHandle3 AS HANDLE,
                                              poDatasetParameterMode3 AS DatasetParameterModeEnum,
                                              poParameterClass AS Progress.Lang.Class):

        DEFINE VARIABLE hDatasetHandles       AS HANDLE                   EXTENT 3 NO-UNDO.
        DEFINE VARIABLE oDatasetParameterMode AS DatasetParameterModeEnum EXTENT 3 NO-UNDO .

        ASSIGN hDatasetHandles[1]       = phDatasetHandle1
               oDatasetParameterMode[1] = poDatasetParameterMode1
               hDatasetHandles[2]       = phDatasetHandle2
               oDatasetParameterMode[2] = poDatasetParameterMode2
               hDatasetHandles[3]       = phDatasetHandle3
               oDatasetParameterMode[3] = poDatasetParameterMode3
               .

        RETURN CAST (THIS-OBJECT:Add (NEW BusinessTaskMethodData (pcMethodName,
                                                                  hDatasetHandles,
                                                                  oDatasetParameterMode,
                                                                  poParameterClass)),
                     BusinessTaskMethodData) .

    END METHOD.

    /**
     * Purpose: Adds a new Item to the list
     * Notes:   Variant of the method for only a single Dataset Parameter (no array)
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle1 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode1 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle2 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode2 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle3 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode3 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle4 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode4 The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     * @return The new BusinessTaskMethodData instance
     */
    METHOD PUBLIC BusinessTaskMethodData Add (pcMethodName AS CHARACTER,
                                              phDatasetHandle1 AS HANDLE,
                                              poDatasetParameterMode1 AS DatasetParameterModeEnum,
                                              phDatasetHandle2 AS HANDLE,
                                              poDatasetParameterMode2 AS DatasetParameterModeEnum,
                                              phDatasetHandle3 AS HANDLE,
                                              poDatasetParameterMode3 AS DatasetParameterModeEnum,
                                              phDatasetHandle4 AS HANDLE,
                                              poDatasetParameterMode4 AS DatasetParameterModeEnum,
                                              poParameterClass AS Progress.Lang.Class):

        DEFINE VARIABLE hDatasetHandles       AS HANDLE                   EXTENT 4 NO-UNDO.
        DEFINE VARIABLE oDatasetParameterMode AS DatasetParameterModeEnum EXTENT 4 NO-UNDO .

        ASSIGN hDatasetHandles[1]       = phDatasetHandle1
               oDatasetParameterMode[1] = poDatasetParameterMode1
               hDatasetHandles[2]       = phDatasetHandle2
               oDatasetParameterMode[2] = poDatasetParameterMode2
               hDatasetHandles[3]       = phDatasetHandle3
               oDatasetParameterMode[3] = poDatasetParameterMode3
               hDatasetHandles[4]       = phDatasetHandle4
               oDatasetParameterMode[4] = poDatasetParameterMode4
               .

        RETURN CAST (THIS-OBJECT:Add (NEW BusinessTaskMethodData (pcMethodName,
                                                                  hDatasetHandles,
                                                                  oDatasetParameterMode,
                                                                  poParameterClass)),
                     BusinessTaskMethodData) .

    END METHOD.

    /**
     * Purpose: Adds a new Item to the list
     * Notes:   Variant of the method for only a single Dataset Parameter (no array)
     * @param pcMethodName The name of the Business Task Method
     * @param phDatasetHandle1 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode1 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle2 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode2 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle3 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode3 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle4 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode4 The mode of the dataset parameter (input, output, input-output)
     * @param phDatasetHandle5 The handle of the dataset (empty datasets to describe the interface)
     * @param poDatasetParameterMode5 The mode of the dataset parameter (input, output, input-output)
     * @param poParameterClass The name of the class for the parameter object (non abstract, non interface) or ?
     * @return The new BusinessTaskMethodData instance
     */
    METHOD PUBLIC BusinessTaskMethodData Add (pcMethodName AS CHARACTER,
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

        DEFINE VARIABLE hDatasetHandles       AS HANDLE                   EXTENT 5 NO-UNDO.
        DEFINE VARIABLE oDatasetParameterMode AS DatasetParameterModeEnum EXTENT 5 NO-UNDO .

        ASSIGN hDatasetHandles[1]       = phDatasetHandle1
               oDatasetParameterMode[1] = poDatasetParameterMode1
               hDatasetHandles[2]       = phDatasetHandle2
               oDatasetParameterMode[2] = poDatasetParameterMode2
               hDatasetHandles[3]       = phDatasetHandle3
               oDatasetParameterMode[3] = poDatasetParameterMode3
               hDatasetHandles[4]       = phDatasetHandle4
               oDatasetParameterMode[4] = poDatasetParameterMode4
               hDatasetHandles[5]       = phDatasetHandle5
               oDatasetParameterMode[5] = poDatasetParameterMode5
               .

        RETURN CAST (THIS-OBJECT:Add (NEW BusinessTaskMethodData (pcMethodName,
                                                                  hDatasetHandles,
                                                                  oDatasetParameterMode,
                                                                  poParameterClass)),
                     BusinessTaskMethodData) .

    END METHOD.

END CLASS.
