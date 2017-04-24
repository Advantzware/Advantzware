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
    File        : SaveRowChangesEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 12 14:56:05 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.* .
USING Consultingwerk.EventArgs .
USING Consultingwerk.OERA.Enum.* .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.OERA.SaveRowChangesEventArgs INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the buffers BEFORE-BUFFER                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BeforeBufferHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the actual buffers that is validated (the 
                 AFTER-BUFFER)
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the actual buffer that is validated (AFTER-BUFFER)                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RowState AS RowStateEnum NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SaveRowChangesEventArgs class                                                                      
        Notes:                                                                        
        @param pcBufferName The name of the buffer
        @param phBufferHandle The handle of the buffer
        @param phBeforeBufferHandle The handle of the records before buffer
        @param poRowState The RowStateEnum value mathing value of the ROW-STATE property of the before buffer
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SaveRowChangesEventArgs (pcBufferName AS CHARACTER,
                                                phBufferHandle AS HANDLE,
                                                phBeforeBufferHandle AS HANDLE,
                                                poRowState AS RowStateEnum):
        
        ASSIGN THIS-OBJECT:BufferName = pcBufferName 
               THIS-OBJECT:BufferHandle = phBufferHandle 
               THIS-OBJECT:BeforeBufferHandle = phBeforeBufferHandle 
               THIS-OBJECT:RowState = poRowState . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SaveRowChangesEventArgs class                                                                       
        Notes:
        @param pcBufferName The name of the buffer
        @param phBufferHandle The handle of the buffer
        @param phBeforeBufferHandle The handle of the records before buffer
        @param piRowState The INTEGER ROW-STATE value as returned by the ROW-STATE property of the before buffer                                                           
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SaveRowChangesEventArgs (pcBufferName AS CHARACTER,
                                                phBufferHandle AS HANDLE,
                                                phBeforeBufferHandle AS HANDLE,
                                                piRowState AS INTEGER):
        
        THIS-OBJECT (pcBufferName,
                     phBufferHandle,
                     phBeforeBufferHandle,
                     RowStateEnum:FromInteger(piRowState)) . 
        
    END CONSTRUCTOR.

END CLASS.
