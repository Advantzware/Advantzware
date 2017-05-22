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
    File        : IBusinessTaskMethodData
    Purpose     : Describes a Business Task Method for REST/JSDO Catalog
                  generation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 08 11:20:02 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IBusinessTaskMethodData:

    /**
     * Purpose: Returns the array of the dataset handles (empty datasets
     *          to describe the interface)
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DatasetHandles AS HANDLE EXTENT NO-UNDO
    GET.

    /**
     * Purpose: Gets and sets the mode of the dataset parameter (input, output,
     *          input-output)
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DatasetParameterMode AS DatasetParameterModeEnum EXTENT NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the name of the Business Task Method
     * Notes:
     */
    DEFINE PUBLIC PROPERTY MethodName AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the  name of the class for the parameter object
     *          (non abstract, non interface) or ?
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ParameterClass AS Progress.Lang.Class NO-UNDO
    GET.

END INTERFACE.
