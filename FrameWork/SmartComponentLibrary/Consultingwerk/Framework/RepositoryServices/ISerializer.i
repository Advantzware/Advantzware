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
    File        : ISerializer.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 26 21:31:44 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

    /*------------------------------------------------------------------------------
        Purpose: Serializes the component to the buffer                                                                       
        Notes:   
        @param poComponent The reference of the object to serialize to the Buffer
        @param phBuffer The handle of the Buffer to serialize to                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Serialize (poComponent AS Progress.Lang.Object,
                                  phBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Serializes the component from the buffer                                                                       
        Notes:                                                                        
        @param phBuffer The handle of the Buffer to deserialize from                                                                      
        @param poComponent The reference of the object to deserialize to 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Deserialize (phBuffer AS HANDLE,
                                    poComponent AS Progress.Lang.Object).
