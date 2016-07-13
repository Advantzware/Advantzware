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
    File        : BuildUrlEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Oct 19 13:28:35 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs          FROM PROPATH .
USING Consultingwerk.Web.Widgets.Grid.* FROM PROPATH .  
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.Grid.BuildUrlEventArgs INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: The Url to be used with the Grid Column
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Url AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BuildUrlEventArgs class
        Notes:   
        @param pcUrl The base Url for the Image Link
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BuildUrlEventArgs (pcUrl AS CHARACTER):
        SUPER ().
        
        THIS-OBJECT:Url = pcUrl . 
        
    END CONSTRUCTOR.

END CLASS.
