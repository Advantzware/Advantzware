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
    File        : IDataBindable
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 10 23:50:26 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Web.Widgets.IDataBindable:  

    /*------------------------------------------------------------------------------
        Purpose: Assigns the current ScreenValue to the BufferField                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Assign () .  
    
    /*------------------------------------------------------------------------------
        Purpose: Performs the DataBinding, that is typically populating the 
                 ScreenValue with the current value of the BufferField                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformBinding () .  
  
END INTERFACE.
