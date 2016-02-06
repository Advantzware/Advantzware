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
    File        : OpenSaveParameter
    Purpose     : Parameter object for the Open and Save methods of the 
                  Business Entity Designer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 16 23:18:55 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.Serializable.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.BusinessEntityDesigner.Services.OpenSaveParameter INHERITS Serializable: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the file name for the current operation
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i FileName CHARACTER} .

END CLASS.