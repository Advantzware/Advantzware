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
    File        : IColorTableConverter
    Purpose     : Service Interface for services translating the 
                  Progress Color table numbers to .NET System.Drawing.Color
                  values 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingewrk Ltd.
    Created     : Tue Aug 09 12:42:15 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Windows.IColorTableConverter:  
  
    /*------------------------------------------------------------------------------
        Purpose: Converts the given color number (typically from the Progress Color 
                 table) into a System.Drawing.Color value.                                                                         
        Notes:     
        @param piColorNumber The number of the color from the Progress Color table
        @return The System.Drawing.Color matching the color                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Color ConvertColor (piColorNumber AS INTEGER).

END INTERFACE.