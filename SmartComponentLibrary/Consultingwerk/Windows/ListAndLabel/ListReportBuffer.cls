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
    File        : ListReportBuffer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 14 20:34:11 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.GenericList FROM PROPATH .
USING Consultingwerk.Windows.ListAndLabel.*     FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.ListReportBuffer 
    INHERITS GenericList: 

    { Consultingwerk/Framework/Base/GenericList.i ReportBuffer }

    /*------------------------------------------------------------------------------
        Purpose: Creates a ReportBuffer with the given values and adds it to the List
        Notes:   
        @param pcBufferName The name of the record buffer 
        @param poDefineBufferAs An DefineBufferAsEnum value
        @param plMultiple Logical value indicating if the buffer will be iterated using a Query
        @return The new ReportBuffer
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ReportBuffer Add (pcBufferName AS CHARACTER,
                                    poDefineBufferAs AS DefineBufferAsEnum,
                                    plMultiple AS LOGICAL):
        
        DEFINE VARIABLE oReportBuffer AS ReportBuffer NO-UNDO . 
        
        oReportBuffer = NEW ReportBuffer () .
        
        oReportBuffer:BufferName = pcBufferName .
        oReportBuffer:DefineBufferAs = poDefineBufferAs .
        oReportBuffer:Multiple = plMultiple .

        RETURN THIS-OBJECT:Add (oReportBuffer) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets a ReportBuffer definition by the Buffer Name 
        Notes:   
        @param pcBufferName The Buffer Name
        @return The ReportBuffer or ?
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ReportBuffer GetItemByBufferName (pcBufferName AS CHARACTER):
        
        {Consultingwerk/foreachABL.i ReportBuffer poBuffer in THIS-OBJECT}
        
             IF poBuffer:BufferName = pcBufferName THEN  
                 RETURN poBuffer . 
        END.

        RETURN ? . 
    
    END METHOD .

END CLASS.
