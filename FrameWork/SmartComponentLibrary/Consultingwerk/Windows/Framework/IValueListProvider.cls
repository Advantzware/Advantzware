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
    File        : IValueListProvider
    Purpose     : Service Interface to provide an Infragistics ValueList by
                  a list key
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 15 20:40:24 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Framework.* FROM PROPATH .  
USING Progress.Lang.*                    FROM PROPATH .

INTERFACE Consultingwerk.Windows.Framework.IValueListProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the ValueList for the given key
        Notes:   
        @param pcValueListKey The key to return a value list for 
        @return The ValueList for the given key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Infragistics.Win.ValueList ValueListFromKey (pcValueListKey AS CHARACTER).

END INTERFACE.