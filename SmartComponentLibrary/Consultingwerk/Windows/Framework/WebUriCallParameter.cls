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
    File        : WebUriCallParameter
    Purpose     : Function call parameter implementation that can launch 
                  a Web Site via URI 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 02 18:07:52 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                   FROM PROPATH .
USING Consultingwerk.Framework.*         FROM PROPATH . 
USING Consultingwerk.SmartFramework.*    FROM PROPATH .
USING Consultingwerk.Util.*              FROM PROPATH .
USING Consultingwerk.Windows.Framework.* FROM PROPATH .  
USING Progress.Lang.*                    FROM PROPATH .
USING System.Windows.Forms.*             FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Framework.WebUriCallParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    IMPLEMENTS IFunctionCallParameter: 
    
    {Consultingwerk/JsonSerializableProperty.i WebSiteLocation CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i UseExternalWebBrowser LOGICAL} .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Editor Control used by the Function maintenance
                 Viewer 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY EditorControl AS CHARACTER NO-UNDO INIT "Consultingwerk.Windows.Framework.Menu.WebUriCallParameterControl":U 
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the image representing this Function Call Parameter
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageKey AS CHARACTER INITIAL "Consultingwerk/Windows/Framework/Images/FunctionCallParameter/earth.png":U NO-UNDO 
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvokeStaticMethodCallParameter class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC WebUriCallParameter ():
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

        DEFINE VARIABLE oForm AS Consultingwerk.Windows.Forms.WebBrowserForm NO-UNDO .  

        IF THIS-OBJECT:UseExternalWebBrowser THEN 
            System.Diagnostics.Process:Start (THIS-OBJECT:WebSiteLocation) .
        ELSE DO:
            oForm = NEW Consultingwerk.Windows.Forms.WebBrowserForm () .
            oForm:MdiParent = FrameworkSettings:MdiContainer .
            oForm:Show ().
            
            oForm:Navigate (THIS-OBJECT:WebSiteLocation) . 
        
            RETURN oForm . 
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns true when the function is currently active (runninng) 
        Notes:   
        @return True when the function is currently active (runninng)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsActive ():
        
        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: ReActivates the function 
        Notes:   
        @return True when the function was successfully reactivated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ReActivate ():
        
 
    END METHOD.

END CLASS.
