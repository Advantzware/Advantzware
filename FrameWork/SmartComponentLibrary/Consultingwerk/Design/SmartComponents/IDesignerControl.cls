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
    File        : IDesignerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 14 21:21:03 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Design.SmartComponents.IDesignerControl:  

    /*------------------------------------------------------------------------------
        Purpose: Prepares adding a Control                                                                        
        Notes:                            
        @param pcFieldName The name of the field to add
        @param pcFieldDataType The DataType of the field to add
        @param pcFieldLabel The Label of the field to add 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PrepareAddingControl (pcFieldName AS CHARACTER, 
                                             pcFieldDataType AS CHARACTER,
                                             pcFieldLabel AS CHARACTER) .  
  
END INTERFACE.