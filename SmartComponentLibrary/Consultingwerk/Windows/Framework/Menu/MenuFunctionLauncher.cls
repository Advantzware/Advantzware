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
    File        : MenuFunctionLauncher
    Purpose     : SmartFramework implementation of the IMenuFunctionLauncher 
                  service interface 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Dec 08 00:58:11 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*              FROM PROPATH .
USING Consultingwerk.OERA.*                   FROM PROPATH . 
USING Consultingwerk.SmartFramework.*         FROM PROPATH . 
USING Consultingwerk.Util.*                   FROM PROPATH . 
USING Consultingwerk.Windows.Framework.*      FROM PROPATH .  
USING Consultingwerk.Windows.Framework.Menu.* FROM PROPATH .
USING Progress.Lang.*                         FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.MenuFunctionLauncher 
    IMPLEMENTS IMenuFunctionLauncher: 

    {Consultingwerk/SmartFramework/Menu/dsMenu.i}

    /*------------------------------------------------------------------------------
        Purpose: Event launched whenever a menu function is launched  
        Notes:   This can be used for logging or LRU tracking purposes etc.
        @param sender The reference to the object that raised the event
        @param e The MenuFunctionLAunchedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT MenuFunctionLaunched SIGNATURE VOID (sender AS Progress.Lang.Object,   
                                                             e AS MenuFunctionLAunchedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Launches a Menu Function from the Menu  
        Notes:   
        @param pcMenuFunctionKey The key of the Menu Function
        @param poContext An optional reference to a context (could be the Control that launches the function) 
        @return The reference to the launched menu function
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC Progress.Lang.Object LaunchMenuFunction (pcMenuFunctionKey AS CHARACTER,
	                                                       poContext AS Progress.Lang.Object):
		
		DEFINE VARIABLE oCallParameter   AS IFunctionCallParameter                         NO-UNDO . 
        DEFINE VARIABLE oRequest         AS FetchDataRequest                               NO-UNDO .
        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 
        DEFINE VARIABLE oMessage         AS Consultingwerk.SmartFramework.Message          NO-UNDO . 
        DEFINE VARIABLE lcCallParameter  AS LONGCHAR                                       NO-UNDO .   
        DEFINE VARIABLE oReturn          AS Progress.Lang.Object                           NO-UNDO . 
        
        oRequest = NEW FetchDataRequest ("eSmartFunction":U,
                                         SUBSTITUTE ("FOR EACH eSmartFunction WHERE eSmartFunction.FunctionGuid = ~"&1~"":U, pcMenuFunctionKey),
                                         1) .

        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U,
                                                       oRequest,
                                                       OUTPUT DATASET dsMenu) .        

        FIND FIRST eSmartFunction NO-ERROR .
        
        IF NOT AVAILABLE (eSmartFunction) THEN DO:
            oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .
            oMessage = oMessageProvider:GetMessage ("SFR":U, 100) .
            
            UNDO, THROW NEW MenuFunctionLauncherException (oMessage:Text, 100) .
        END.

        COPY-LOB FROM eSmartFunction.FunctionCallParameter 
                 TO lcCallParameter 
                 CONVERT TARGET CODEPAGE "utf-8":U .

        oCallParameter = CAST (Consultingwerk.Serializable:DeserializeInstance (lcCallParameter),
                               IFunctionCallParameter) .

        oReturn = oCallParameter:Invoke (TRUE, poContext) . 

        IF VALID-OBJECT (oReturn) THEN 
            THIS-OBJECT:OnMenuFunctionLaunched (NEW MenuFunctionLaunchedEventArgs (pcMenuFunctionKey,
                                                                                   eSmartFunction.FunctionName,
                                                                                   poContext,
                                                                                   oReturn)) .

        RETURN oReturn . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MenuFunctionLaunched
        Notes:   
        @param e The MenuFunctionLaunchedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnMenuFunctionLaunched (e AS MenuFunctionLaunchedEventArgs):
    		
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "MenuFunctionLaunched":U) .
     
        THIS-OBJECT:MenuFunctionLaunched:Publish (THIS-OBJECT, e) .
    
    END METHOD .

END CLASS.
