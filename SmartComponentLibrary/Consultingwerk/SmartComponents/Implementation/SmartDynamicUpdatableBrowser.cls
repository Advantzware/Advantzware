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
    File        : SmartDynamicUpdatableBrowser
    Purpose     : Implementation of a SmartUpdatableBrowser (UltraGrid derived
                  Control) that receives it layout at runtime from a 
                  repository service
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 15 14:58:27 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.RepositoryServices.*   FROM PROPATH . 
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Infragistics.Win.UltraWinGrid.*                 FROM ASSEMBLY . 
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartDynamicUpdatableBrowser 
    INHERITS SmartUpdatableBrowser: 

    DEFINE VARIABLE oRepository AS IDynamicBrowserRepository NO-UNDO . 
    DEFINE VARIABLE oRenderer   AS IDynamicBrowserRenderer   NO-UNDO . 

    {Consultingwerk/Framework/RepositoryServices/dsDynamicBrowser.i}

    /*------------------------------------------------------------------------------
        Purpose: The key of the Browser's layout in the repository 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BrowserKey AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDynamicBrowser class                                                                       
        Notes:                                                                              
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDynamicUpdatableBrowser ():
        SUPER ().

        IF THIS-OBJECT:DesignTime = FALSE THEN DO: 
            oRepository = {Consultingwerk/get-service.i Consultingwerk.Framework.RepositoryServices.IDynamicBrowserRepository} .
            
            {Consultingwerk/Assertion/ObjectAssert/IsValid.i oRepository """DynamicBrowserRepository"":U"} .
            
            oRenderer = {Consultingwerk/get-service.i Consultingwerk.SmartComponents.Implementation.IDynamicBrowserRenderer
                                                      "NEW Consultingwerk.SmartComponents.Implementation.DynamicBrowserRenderer ()"} . 
        END.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Raises the InitializeLayout event
        Notes:   
        @param e The InitializeLayoutEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnInitializeLayout (e AS InitializeLayoutEventArgs):
        
        IF THIS-OBJECT:DesignTime = FALSE THEN DO: 
            oRepository:LoadSettings (THIS-OBJECT:BrowserKey,
                                      OUTPUT DATASET dsDynamicBrowser BY-REFERENCE) .
            oRenderer:RenderDynamicBrowser (THIS-OBJECT:BrowserKey,
                                            THIS-OBJECT,
                                            DATASET dsDynamicBrowser BY-REFERENCE) .
        END.
                
        SUPER:OnInitializeLayout(INPUT e).

    END METHOD .

END CLASS.
