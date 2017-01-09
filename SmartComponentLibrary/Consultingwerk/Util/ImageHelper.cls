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
    File        : ImageHelper
    Purpose     : 
    Syntax      : Static methods only, Protected constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 29 16:58:41 CEST 2011
    Notes       : This class is intented for GUI for .NET use only 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Util.* FROM PROPATH . 
USING Progress.Lang.*       FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN    
USING System.Drawing.*      FROM ASSEMBLY .  
USING System.Drawing.Text.* FROM ASSEMBLY .  
&ENDIF

CLASS Consultingwerk.Util.ImageHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED ImageHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN    
    /*------------------------------------------------------------------------------
        Purpose: Returns a System.Drawing.Bitmap that has been rendered from the 
                 passed in text 
        Notes:   Useful for creating text-images for a Ribbon etc.
        @param piImageWidth The width of the Image in pixels
        @param piImageHeight The height of the Image in pixels
        @param pcFontName The name of the font to use for the Text
        @param piFontSize The font size in Point
        @param pcText The text to render as a Bitmap
        @return The Bitmap that has been rendered from the Text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Bitmap BuildTextImage (piImageWidth AS INTEGER,
                                                piImageHeight AS INTEGER,
                                                pcFontName AS CHARACTER,
                                                piFontSize AS INTEGER,
                                                pcText AS CHARACTER):
        
        DEFINE VARIABLE oBitmap       AS Bitmap       NO-UNDO . 
        DEFINE VARIABLE oGraphics     AS Graphics     NO-UNDO . 
        DEFINE VARIABLE oStringFormat AS StringFormat NO-UNDO .
        
        ASSIGN oBitmap = NEW Bitmap (piImageWidth, piImageHeight) 
               oGraphics = Graphics:FromImage (oBitmap) 
               
               oGraphics:TextRenderingHint = TextRenderingHint:AntiAlias .
               
        oStringFormat = NEW StringFormat ().
        
        oStringFormat:Trimming = StringTrimming:EllipsisWord .                 
        
        oGraphics:DrawString (pcText, 
                              NEW Font (pcFontName, piFontSize), 
                              Brushes:Black,
                              NEW RectangleF (1, 1, piImageWidth - 2, piImageHeight - 2),
                              oStringFormat) .
        
        RETURN oBitmap .

        FINALLY:
            IF VALID-OBJECT (oGraphics) THEN 
                oGraphics:Dispose () .
                
            IF VALID-OBJECT (oGraphics) THEN 
                DELETE OBJECT oGraphics .                       
        END FINALLY.

    END METHOD .
   
    /*------------------------------------------------------------------------------
        Purpose: Converts a BLOB field from a Buffer handle into a System.Drawing.Bitmap                                                                         
        Notes:   Creates a temporary Progress.Data.BindingSource to do the conversion
                 Verifies that a record is available in the buffer first                                                                        
        @param phBufferHandle The Buffer handle that contains the BLOB field
        @param pcBufferField The name of the BLOB field that contains the image data 
        @return The System.Drawing.Bitmap created from the BLOB data 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Drawing.Bitmap FromBLOB (phBufferHandle AS HANDLE, 
                                                         pcBufferField AS CHARACTER):
        
        DEFINE VARIABLE oMemoryStream  AS System.IO.MemoryStream      NO-UNDO.
        DEFINE VARIABLE oBindingSource AS Progress.Data.BindingSource NO-UNDO.    

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBufferHandle} .
         
        oBindingSource = NEW Progress.Data.BindingSource (phBufferHandle).
        
        oBindingSource:Refresh().
        
        oMemoryStream = NEW System.IO.MemoryStream (CAST(oBindingSource:InputValue [pcBufferField],
                                                         "System.Byte[]")) .
        
        RETURN NEW System.Drawing.Bitmap (oMemoryStream).
    
        FINALLY:
          IF VALID-OBJECT (oBindingSource) THEN 
              oBindingSource:Dispose() .
              
          IF VALID-OBJECT (oMemoryStream) THEN 
              oMemoryStream:Close() .
    
          IF VALID-OBJECT (oBindingSource) THEN 
              DELETE OBJECT oBindingSource .
              
          IF VALID-OBJECT (oMemoryStream) THEN 
              DELETE OBJECT oMemoryStream .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Bitmap stored in the Byte[]
        Notes:   
        @param poImageData The Image data
        @return The System.Drawing.Bitmap representing the Image
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC System.Drawing.Bitmap FromByteArray (poImageData AS "System.Byte[]"):
		
        DEFINE VARIABLE oMemoryStream    AS System.IO.MemoryStream NO-UNDO.

        DEFINE VARIABLE iOleHeaderLength AS INTEGER                INIT 78 . 
		
        oMemoryStream = NEW System.IO.MemoryStream () . 
        
        IF ImageHelper:HasOleContainerHeader (poImageData) THEN 
            oMemoryStream:Write (poImageData, iOleHeaderLength, poImageData:Length - iOleHeaderLength) .
        ELSE 
            oMemoryStream:Write (poImageData, 0, poImageData:Length) .
            
        RETURN NEW System.Drawing.Bitmap (oMemoryStream).
    
        FINALLY:
          IF VALID-OBJECT (oMemoryStream) THEN 
              oMemoryStream:Close() .
    
          IF VALID-OBJECT (oMemoryStream) THEN 
              DELETE OBJECT oMemoryStream .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns true when the Byte[] contains an OLE Image Header 
        Notes:   
        @param poImageData The Image data
        @return Logical value indicating if the Byte[] contains an OLE Image Header 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC LOGICAL HasOleContainerHeader (poImageData AS "System.Byte[]"):
		
        DEFINE VARIABLE iOleByte0 AS INTEGER INIT 21 . 
        DEFINE VARIABLE iOleByte1 AS INTEGER INIT 28 . 
        
        RETURN UNBOX (poImageData:GetValue(0)) = iOleByte0 AND UNBOX (poImageData:GetValue(1)) = iOleByte1 . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Loads an Image                                                                       
        Notes:   Performs Propath lookup and loads the image without locking it
                 When the image name is not qualified with a file extension, we attempt
                 to load a .ico and .bmp (similar to the ABL behaviour). 
                 This routine cannot load images from procedure libraries!     
        @param pcFileName The image file name
        @return The System.Drawing.Bitmap  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Drawing.Bitmap Load (pcFileName AS CHARACTER):
        
        DEFINE VARIABLE oStream AS System.IO.FileStream NO-UNDO . 
        
        FILE-INFO:FILE-NAME = pcFileName . 
        IF FILE-INFO:FULL-PATHNAME = ? OR NOT FILE-INFO:FILE-TYPE MATCHES "*F*":U THEN FILE-INFO:FILE-NAME = pcFileName + ".ico":U . 
    
        /* Mike Fechner, Consultingwerk Ltd. 15.03.2013 
           When the .ico is also not found, thr .bmp */
        IF FILE-INFO:FULL-PATHNAME = ? OR NOT FILE-INFO:FILE-TYPE MATCHES "*F*":U THEN FILE-INFO:FILE-NAME = pcFileName + ".bmp":U . 
        
        IF FILE-INFO:FULL-PATHNAME > "":U AND FILE-INFO:FILE-TYPE MATCHES "*F*":U AND
           /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
              .NET routines cannot load images from procedure library */
           NOT FILE-INFO:FULL-PATHNAME MATCHES "*<<*":U THEN DO:

            oStream = NEW System.IO.FileStream (FILE-INFORMATION:FULL-PATHNAME,
                                                System.IO.FileMode:Open,
                                                System.IO.FileAccess:Read,
                                                System.IO.FileShare:ReadWrite) .
                                                
            RETURN CAST (System.Drawing.Bitmap:FromStream (oStream),
                         System.Drawing.Bitmap) .
        END.

        IF FILE-INFO:FILE-TYPE MATCHES "*M*":U THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to load image &1 from procedure library: &2!"{&TRAN},    
                                                 pcFileName,
                                                 FILE-INFO:FULL-PATHNAME), 0) .    

        UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to load image &1!"{&TRAN}, pcFileName), 0) .    
        
        FINALLY:
            IF VALID-OBJECT (oStream) THEN 
                oStream:Close () .      
        END FINALLY.

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Loads an Icon                                                                       
        Notes:   Performs Propath lookup and loads the icon without locking it
                 When the icon name is not qualified with a file extension, we attempt
                 to load a .ico (similar to the ABL behaviour).
                 When no Size or an invalid size is given, we load the first Icon 
        @param pcFileName The icon file name
        @return The System.Drawing.Icon created  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Drawing.Icon LoadIcon (pcFileName AS CHARACTER):

        RETURN ImageHelper:LoadIcon (pcFileName, ?) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Loads an Icon                                                                       
        Notes:   Performs Propath lookup and loads the icon without locking it
                 When the icon name is not qualified with a file extension, we attempt
                 to load a .ico (similar to the ABL behaviour).
                 When no Size or an invalid size is given, we load the first Icon 
        @param pcFileName The icon file name
        @param poSize The preferred icon size to be returned 
        @return The System.Drawing.Icon created  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Drawing.Icon LoadIcon (pcFileName AS CHARACTER,
                                                       poSize AS System.Drawing.Size):
        
        DEFINE VARIABLE oIcon   AS System.Drawing.Icon  NO-UNDO . 
        DEFINE VARIABLE oStream AS System.IO.FileStream NO-UNDO . 
        
        FILE-INFO:FILE-NAME = pcFileName . 
        IF FILE-INFO:FULL-PATHNAME = ? OR NOT FILE-INFO:FILE-TYPE MATCHES "*F*":U THEN FILE-INFO:FILE-NAME = pcFileName + ".ico":U . 
    
        IF FILE-INFO:FULL-PATHNAME > "":U AND FILE-INFO:FILE-TYPE MATCHES "*F*":U AND
           /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
              .NET routines cannot load images from procedure library */
           NOT FILE-INFO:FULL-PATHNAME MATCHES "*<<*":U THEN DO:

            oStream = NEW System.IO.FileStream (FILE-INFORMATION:FULL-PATHNAME,
                                                System.IO.FileMode:Open,
                                                System.IO.FileAccess:Read,
                                                System.IO.FileShare:ReadWrite) .
            
            IF VALID-OBJECT (poSize) THEN       
                RETURN NEW System.Drawing.Icon (oStream, poSize) .    
            ELSE                                                 
                RETURN NEW System.Drawing.Icon (oStream) .
        END.

        IF FILE-INFO:FILE-TYPE MATCHES "*M*":U THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to load icon &1 from procedure library: &2!"{&TRAN},    
                                                 pcFileName,
                                                 FILE-INFO:FULL-PATHNAME), 0) .    

        UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to load icon &1!"{&TRAN}, pcFileName), 0) .    
        
        FINALLY:
            IF VALID-OBJECT (oStream) THEN 
                oStream:Close () .      
        END FINALLY.

    END METHOD.
        
    /*------------------------------------------------------------------------------
        Purpose: Adds transparency to a non transparent Bitmap                                                                     
        Notes:   http://www.dotnet247.com/247reference/msgs/40/202528.aspx
        @param poImage The System.Drawing.Bitmap to make transparent
        @return The reference to the generated transparent System.Drawing.Bitmap                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Drawing.Bitmap MakeTransparent (poImage AS System.Drawing.Bitmap):
        
        DEFINE VARIABLE oBitmap   AS System.Drawing.Bitmap   NO-UNDO .
        DEFINE VARIABLE oGraphics AS System.Drawing.Graphics NO-UNDO .
        
        /* Get the current desktop screen graphics. */
        oGraphics = System.Drawing.Graphics:FromHwnd (System.IntPtr:Zero) .
        
        /* Make a bitmap using that color depth. */
        oBitmap = NEW System.Drawing.Bitmap (poImage:Width, poImage:Height, oGraphics) .
        
        /* Clean up. */
        oGraphics:Dispose() .
        
        /* Start drawing on our new bitmap. */
        oGraphics = System.Drawing.Graphics:FromImage (oBitmap) .
        
        /* Copy the loaded bitmap to the bitmap with proper screen depth. */
        oGraphics:DrawImage (poImage, 0, 0, oBitmap:Width, oBitmap:Height) .
        
        /* Clean up */
        oGraphics:Dispose () .           
    
        /* Now we can successfully make the bitmap transparent! */  
        oBitmap:MakeTransparent (oBitmap:GetPixel (0,0)) .
        
        RETURN oBitmap . 
        
    END METHOD.
&ENDIF

END CLASS.