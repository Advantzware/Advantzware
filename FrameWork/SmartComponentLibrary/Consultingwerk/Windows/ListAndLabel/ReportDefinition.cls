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
    File        : ReportDefinition
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 14 20:06:27 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                      FROM PROPATH . 
USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.ReportDefinition 
    INHERITS JsonSerializable: 

    {Consultingwerk/JsonSerializableProperty.i ParentBufferName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ReportBuffers ListReportBuffer} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ReportDefinition class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ReportDefinition ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        THIS-OBJECT:ReportBuffers = NEW ListReportBuffer () .
        
    END CONSTRUCTOR.

END CLASS.
