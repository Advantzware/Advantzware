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
    File        : ContextWrapperSessionProperty.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 20 01:09:21 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

    /*------------------------------------------------------------------------------
        Purpose: Wrapps access to eSessionContext.{1}                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY {1} AS {2} NO-UNDO 
    GET ():
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        
        DatasetAssert:HasBuffer (SessionManager:ContextDataset,
                                 "eSessionContext":U) . 
                                 
        ASSIGN hBuffer = SessionManager:ContextDataset::eSessionContext .
        
        hBuffer:FIND-FIRST () NO-ERROR . 
        
        IF NOT hBuffer:AVAILABLE THEN 
            RETURN ?  .
        
        RETURN GetSessionContextField ("{1}":U):BUFFER-VALUE . 
    END GET.
    SET (INPUT arg AS {2}):
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        
        DatasetAssert:HasBuffer (SessionManager:ContextDataset,
                                 "eSessionContext":U) . 
                                 
        ASSIGN hBuffer = SessionManager:ContextDataset::eSessionContext .
        
        hBuffer:FIND-FIRST () NO-ERROR . 
        
        IF NOT hBuffer:AVAILABLE THEN 
            hBuffer:BUFFER-CREATE () .
        
        GetSessionContextField ("{1}":U):BUFFER-VALUE = arg .               
    END SET.
