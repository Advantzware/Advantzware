/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Layout
    Purpose     : Service Interface to return Layouts to the web browser
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechenr / Consultingwerk Ltd.
    Created     : Sat Jul 18 14:15:42 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                FROM PROPATH .
USING Consultingwerk.Framework.*                FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.*           FROM PROPATH .
USING Consultingwerk.Web2.Services.Exceptions.* FROM PROPATH .
USING Consultingwerk.Web2.Views.*               FROM PROPATH .  
USING Consultingwerk.Web2.Services.SmartViews.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.Services.SmartViews.Layout: 

    DEFINE VARIABLE oConfig AS IConfigurationProvider NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Layout class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Layout ():
        
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        SUPER ().

        FileAssert:Exists (".restapplicationsettings":U) .
        
        ASSIGN cFileName = FileHelper:FindFile (".restapplicationsettings":U) .
        
        ASSIGN oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider
                                                       "NEW ConfigurationProvider (cFileName)"} .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a Layout to the web browser 
        Notes:   
        @param pcLayoutId The Id of the layout to return
        @param pcJSON The JSON structure of the layout 
    ------------------------------------------------------------------------------*/
	@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
	METHOD PUBLIC VOID GetLayout (pcLayoutId AS CHARACTER,
	                              OUTPUT pcJSON AS LONGCHAR):
		
		DEFINE VARIABLE oLayoutProvider AS ILayoutProvider NO-UNDO . 
		
		/* Mike Fechner, Consultingwerk Ltd. 19.07.2015
           Prototype for now! */
		oLayoutProvider = DYNAMIC-NEW ("Consultingwerk.SmartComponentsDemo.Web2.DemoLayoutProvider":U) () . 
		
		oLayoutProvider:GetLayout (pcLayoutId, OUTPUT pcJSON) .
				
        CATCH err AS Progress.Lang.Error:
        	LogManager:WriteError (err) .
        	
        	UNDO, THROW NEW Web2ServiceInterfaceException (err, err:GetMessage(1), 0) .
        		
        END CATCH.

	END METHOD .

END CLASS.
