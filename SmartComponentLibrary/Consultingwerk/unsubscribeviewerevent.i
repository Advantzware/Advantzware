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
    File        : unsubscribeviewerevent.i
    Purpose     : Unsubscribes a viewer controls events from event handlers 
                  in the Viewer Logic Object

    Syntax      : unsubscribeviewerevent.i
                    BindingPropertyName
                    EventName
                    EventHandlerMethodName

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 26 10:07:38 CEST 2013
    Notes       : The BindingPropertyName can be retrieved from the 
                  Consultingwerk.Util.BindingSourceHelper:BindingColumnName
                  method
  ----------------------------------------------------------------------*/

        {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in "THIS-OBJECT:GetBoundControls(""{1}"":U)" }
            oControl:{2}:Unsubscribe ({3}) .      
        END.
