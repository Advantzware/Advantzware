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
    File        : KeepAliveService
    Purpose     : Service that performs keep alive AppServer calls
    Syntax      : 
    Description : Service can be loaded via service.xml, keep-alive timer 
                  can be set in number of seconds using the .applicationsettings 
                  file (Consultingwerk.Framework.IConfigurationProvider) 
                  using the AppServerKeepAlivePeriod setting 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Sat Feb 22 23:38:21 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                FROM PROPATH . 
USING Consultingwerk.Assertion.*                      FROM PROPATH . 
USING Consultingwerk.Framework.*                      FROM PROPATH . 
USING Consultingwerk.OERA.*                           FROM PROPATH . 
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Consultingwerk.Util.*                           FROM PROPATH . 
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.KeepAliveService: 

    DEFINE VARIABLE iKeepAlivePeriod AS INTEGER NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Raised when ever an AppServer Keep Alive ping was successful 
        Notes:   
        @param sender The object that raised the AppServerKeepAlive event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AppServerKeepAlive SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                           e AS Consultingwerk.EventArgs).
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the last response received from the AppServer 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY LastResponse AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Timer AS System.Windows.Forms.Timer NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the KeepAliveService class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC KeepAliveService ():
	    
        DEFINE VARIABLE oConfiguration AS Consultingwerk.Framework.IConfigurationProvider NO-UNDO . 
    
		SUPER ().

        oConfiguration = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .
		
		{Consultingwerk/Assertion/ObjectAssert/IsValid.i oConfiguration """Configuration Provider"":U"} .
		
		DO ON ERROR UNDO, THROW:
		    iKeepAlivePeriod = INTEGER (oConfiguration:GetValue ("AppServerKeepAlivePeriod":U)) .
    		
    		CATCH err AS Progress.Lang.Error :
            	/* ignore */	
            END CATCH.
    	END.	
    		
        Assert:NotNullOrZero (iKeepAlivePeriod, "AppServerKeepAlivePeriod":U) .
    		
        THIS-OBJECT:Timer = NEW System.Windows.Forms.Timer () .
         
        LogManager:WriteMessage (SUBSTITUTE ("Starting AppServer keep alive service, pinging AppServer every &1 seconds."{&TRAN},
                                             iKeepAlivePeriod),
                                 "KeepAliveService":U) . 
        
        THIS-OBJECT:Timer:Tick:Subscribe (TimerTickHandler) .  
        THIS-OBJECT:Timer:Interval = iKeepAlivePeriod * 1000 .
    	THIS-OBJECT:Timer:Enabled = TRUE . 
    		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AppServerKeepAlive
        Notes:   
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAppServerKeepAlive (e AS Consultingwerk.EventArgs):
            
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 
    
        THIS-OBJECT:AppServerKeepAlive:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the tick event of the Timer component
        Notes:   
        @param sender The reference to the object that raised the Tick event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID TimerTickHandler (sender AS System.Object, e AS System.EventArgs):
		
		DEFINE VARIABLE oMessage AS CharacterHolder NO-UNDO . 
		
        LogManager:WriteMessage ("Begin AppServer keep alive call"{&TRAN},
                                 "KeepAliveService":U) . 

        oMessage = NEW CharacterHolder () .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U, 
                                                     "Consultingwerk.Framework.Server.ServerHelperTask":U,
                                                     "Ping":U,
                                                     oMessage,
                                                     Consultingwerk.Framework.Session.SessionManager:ContextDataset) . 
        
        LogManager:WriteMessage (SUBSTITUTE ("Keep alive repsonse received: &1"{&TRAN},
                                             oMessage:Value),
                                 "KeepAliveService":U) . 

        THIS-OBJECT:LastResponse = oMessage:Value .

        THIS-OBJECT:OnAppServerKeepAlive (Consultingwerk.EventArgs:Empty) .

        CATCH err AS Progress.Lang.Error:
            LogManager:WriteError (err) .
            
            ErrorHelper:ShowErrorMessage (err) .        		
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the KeepAliveService class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC KeepAliveService ():

        IF VALID-OBJECT (THIS-OBJECT:Timer) THEN DO: 
            THIS-OBJECT:Timer:Enabled = FALSE .

            GarbageCollectorHelper:DeleteObject (THIS-OBJECT:Timer) .
        END.

    END DESTRUCTOR .

END CLASS.
