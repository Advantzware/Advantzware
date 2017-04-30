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
    File        : DefaultColorTableConverter
    Purpose     : Default converter for Progress color table values into
                  .NET System.Drawing Color values 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 09 13:14:41 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.Windows.IColorTableConverter.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.DefaultColorTableConverter IMPLEMENTS IColorTableConverter: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the .NET Color equivalent for the default Progress color 
                 table entries                                                                     
        Notes:     
        @param piColorNumber The number of the color from the Progress Color table
        @return The System.Drawing.Color matching the color                                                                   
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC System.Drawing.Color ConvertColor (piColorNumber AS INTEGER):
        
        CASE piColorNumber:
            WHEN ? THEN 
                 RETURN System.Drawing.Color:Empty .
            WHEN 0 THEN  
                 RETURN System.Drawing.Color:Black .         
            WHEN 1 THEN  
                 RETURN System.Drawing.Color:DarkBlue .         
            WHEN 2 THEN  
                 RETURN System.Drawing.Color:DarkGreen .         
            WHEN 3 THEN  
                 RETURN System.Drawing.Color:DarkCyan .         
            WHEN 4 THEN  
                 RETURN System.Drawing.Color:DarkRed .         
            WHEN 5 THEN  
                 RETURN System.Drawing.Color:Violet .         
            WHEN 6 THEN  
                 RETURN System.Drawing.Color:Olive .         
            WHEN 7 THEN  
                 RETURN System.Drawing.Color:DarkGray .         
            WHEN 8 THEN  
                 RETURN System.Drawing.Color:Gray .         
            WHEN 9 THEN  
                 RETURN System.Drawing.Color:Blue .         
            WHEN 10 THEN  
                 RETURN System.Drawing.Color:Green .         
            WHEN 11 THEN  
                 RETURN System.Drawing.Color:Cyan .         
            WHEN 12 THEN  
                 RETURN System.Drawing.Color:Red .         
            WHEN 13 THEN  
                 RETURN System.Drawing.Color:Fuchsia .         
            WHEN 14 THEN  
                 RETURN System.Drawing.Color:Yellow .         
            WHEN 15 THEN  
                 RETURN System.Drawing.Color:White .         
        END CASE .              

    END METHOD.

END CLASS.