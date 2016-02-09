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
    File        : DesignerEventService
    Purpose     : Provides event notifications when root designers are added 
                  and removed, when a selected component changes, and when 
                  the current root designer changes.
    Syntax      : 
    Description : Part of the .NET VisualDesigner infrastructure
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 13 00:32:08 CEST 2010
    Notes       : http://msdn.microsoft.com/en-us/library/system.componentmodel.design.idesignereventservice.aspx
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING System.ComponentModel.Design.* .

CLASS Consultingwerk.Design.DesignerEventService INHERITS System.Object 
    IMPLEMENTS IDesignerEventService: 

    DEFINE PRIVATE VARIABLE oDesignerHost AS IDesignerHost NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Occurs when the current root designer changes.                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ActiveDesignerChanged DELEGATE System.ComponentModel.Design.ActiveDesignerEventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Occurs when a root designer is created.                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DesignerCreated DELEGATE System.ComponentModel.Design.DesignerEventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Occurs when a root designer for a document is disposed.                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DesignerDisposed DELEGATE System.ComponentModel.Design.DesignerEventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Occurs when the current design-view selection changes.                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SelectionChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Gets the root designer for the currently active document.                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ActiveDesigner AS System.ComponentModel.Design.IDesignerHost 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets a collection of root designers for design documents that are 
                 currently active in the development environment.                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Designers AS System.ComponentModel.Design.DesignerCollection 
    GET.
    PRIVATE SET.
     
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DesignerEventService class                                                                       
        Notes:                                                  
        @param poDesignerHost The Visual Designer Host                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DesignerEventService (poDesignerHost AS IDesignerHost):
        DEFINE VARIABLE oDesignerHosts AS "System.ComponentModel.Design.IDesignerHost[]" NO-UNDO .
        
        SUPER ().
        
        ASSIGN oDesignerHost = poDesignerHost 
               THIS-OBJECT:ActiveDesigner = poDesignerHost  
               oDesignerHosts = {Consultingwerk/new-array.i System.ComponentModel.Design.IDesignerHost 1} .  
        
        oDesignerHosts:SetValue(poDesignerHost, 0) .
        
        ASSIGN THIS-OBJECT:Designers = NEW System.ComponentModel.Design.DesignerCollection (oDesignerHosts) .
        
    END CONSTRUCTOR.

END CLASS.
