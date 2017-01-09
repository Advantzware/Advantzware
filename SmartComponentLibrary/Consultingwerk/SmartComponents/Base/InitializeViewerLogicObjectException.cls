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
    File        : InitializeViewerLogicObjectException
    Purpose     : Exception thrown when there was an error initializing
                  a SmartViewerControls Logic Object
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 30 14:08:22 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.InitializeViewerLogicObjectException
    INHERITS Exception
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Viewer Logic Class that was attempted to be
                 initialized
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ViewerLogicClass AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InitializeViewerLogicObjectException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcViewerLogicClass The value for the ViewerLogicClass property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InitializeViewerLogicObjectException (poInnerException AS Progress.Lang.Error,
                                                             pcViewerLogicClass AS CHARACTER):
        SUPER (INPUT poInnerException).

        THIS-OBJECT:AddMessage ("Error initializing the Viewer Logic Class"{&TRAN}, 0) .

        IF poInnerException:NumMessages > 0 THEN
            THIS-OBJECT:AddMessage (poInnerException:GetMessage (1), poInnerException:GetMessageNum (0)) .

        ASSIGN THIS-OBJECT:ViewerLogicClass = pcViewerLogicClass .

    END CONSTRUCTOR.

END CLASS.
