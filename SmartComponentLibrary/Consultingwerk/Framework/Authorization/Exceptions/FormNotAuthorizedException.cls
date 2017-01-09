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
    File        : FormNotAuthorizedException
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 08 22:59:37 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Authorization.Exceptions.* FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Framework.Authorization.Exceptions.FormNotAuthorizedException
    INHERITS NotAuthorizedException
    {&SERIALIZABLE}:

    /**
     * Purpose: Returns the Name of the Form Class
     * Notes:
     */
    DEFINE PUBLIC PROPERTY FormClassName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Constructor for the FormNotAuthorizedException Class
     * Notes:
     * @param pcFormClassName The name of the Form Class that cannot be started
     */
    CONSTRUCTOR PUBLIC FormNotAuthorizedException (pcFormClassName AS CHARACTER):
        &IF DEFINED (SmartFramework) NE 0 &THEN
        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO .
        DEFINE VARIABLE cMessage         AS CHARACTER                                      NO-UNDO .

        SUPER () .

        oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        IF VALID-OBJECT (oMessageProvider) THEN
            cMessage = oMessageProvider:GetMessageText ("SFR":U, 102, pcFormClassName) .

        IF cMessage > "":U THEN
            THIS-OBJECT:AddMessage (cMessage, 0) .
        ELSE
            THIS-OBJECT:AddMessage (SUBSTITUTE ("You are not authorized to launch &1."{&TRAN}, pcFormClassName), 0) .
        &ELSE
        SUPER (SUBSTITUTE ("You are not authorized to launch &1."{&TRAN}, pcFormClassName),
               0).
        &ENDIF

        THIS-OBJECT:FormClassName = pcFormClassName .

    END CONSTRUCTOR.

END CLASS.