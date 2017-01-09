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
    File        : LaunchWebViewCallParameter
    Purpose     : Function call parameter implementation that represents
                  a menu item in the SmartComponents.Web2 menu
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 31 18:07:52 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                   FROM PROPATH .
USING Consultingwerk.Framework.*         FROM PROPATH .
USING Consultingwerk.SmartFramework.*    FROM PROPATH .
USING Consultingwerk.Util.*              FROM PROPATH .
USING Consultingwerk.Windows.Framework.* FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.Web2.SmartFramework.Menu.LaunchWebViewCallParameter
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS IFunctionCallParameter:

    {Consultingwerk/JsonSerializableProperty11.i ViewUri CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i Parameter CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i EntityName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i EntityTable CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i EntityViewTables CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i EntityView CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i ViewTitle CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Editor Control used by the Function maintenance
                 Viewer
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY EditorControl AS CHARACTER NO-UNDO INIT "Consultingwerk.Windows.Framework.Menu.LaunchWebViewCallParameterControl":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the image representing this Function Call Parameter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageKey AS CHARACTER INITIAL "Consultingwerk/Windows/Framework/Images/FunctionCallParameter/window_earth.png":U NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvokeStaticMethodCallParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LaunchWebViewCallParameter ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invokes/Starts the function associated with the IFunctionCallParameter
                 object
        Notes:
        @param plForceNewInstance Logical value indicating if a new Instance should be started (True/False/?)
        @param poContext An optional reference to a context (could be the Control that launches the function)
        @return The reference to the Form that has been launched
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Lang.Object Invoke (plForceNewInstance AS LOGICAL,
                                               poContext AS Progress.Lang.Object):

        MessageFormHelper:ShowMessage ("Unable to Launch. This call type is only supposed to be executed on the web."{&TRAN}) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns true when the function is currently active (runninng)
        Notes:
        @return True when the function is currently active (runninng)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsActive ():

        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: ReActivates the function
        Notes:
        @return True when the function was successfully reactivated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ReActivate ():

        THIS-OBJECT:Invoke (?, ?) .

        RETURN FALSE .

    END METHOD.

END CLASS.
