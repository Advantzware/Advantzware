/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ProparseTempFileWriteProtectedException
    Purpose     : Exception thrown by ProparseHelper when the Proparse 
                  Temp-File Progress-Temp\prorefactor\temp\listingfile.txt
                  is write protected
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 16 22:35:33 CEST 2014
    Notes       : When this error is thrown, the Java Exception from SCL-456
                  would alternatively be shown
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Studio.Proparse.ProparseTempFileWriteProtectedException 
    INHERITS ExceptionWithTitle
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the file name of the temp-file that caused the issue                                                                         
        Notes:                  
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TempFileName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProparseTempFileWriteProtectedException class                                                                         
        Notes:                  
        @param pcTempFileName The Name of the temp-file that caused the issue
        @param pcTitle The tile for the error message
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProparseTempFileWriteProtectedException (pcTempFileName AS CHARACTER, 
                                                                pcTitle AS CHARACTER ):
        
        SUPER (SUBSTITUTE ("Unable to invoke Proparse as the listing file ~"&1~" exists and is write protected."{&TRAN},
                           pcTempFileName), 
               0, 
               pcTitle).
        
        ASSIGN THIS-OBJECT:TempFileName = pcTitle . 
        
    END CONSTRUCTOR.

END CLASS.
