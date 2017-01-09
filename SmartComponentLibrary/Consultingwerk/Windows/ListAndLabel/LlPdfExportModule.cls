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
    File        : LlPdfExportModule
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 29 22:05:39 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.LlPdfExportModule 
    INHERITS LlExportModule
    IMPLEMENTS ILlExportModule: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the key of the Export Module
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC OVERRIDE PROPERTY ExportModule AS CHARACTER NO-UNDO INIT "PDF":U
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Specifies the title of the generated PDF document. 
        Notes:   Default: title passed to LlPrintWithBoxStart().
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PDF_Title AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Specifies the subject of the generated PDF document. 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_Subject AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Specifies the keywords of the generated PDF document.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_Keywords AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: If this parameter is set, the result file will be encrypted. 
        Notes:   Other different options are then enabled
                 0 Do not encrypt FILE (default)
                 1 Encrypt file
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_Encryption_EncryptFile AS CHARACTER NO-UNDO INIT "0":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: If this parameter is set, the file can be changed even if it is encrypted. 
        Notes:   Only effective if PDF.Encryption.EncryptFile is set to "1"
                 0 Changing is not enabled (default)
                 1 Changing is enabled
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_Encryption_EnableChanging AS CHARACTER NO-UNDO INIT "0":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: If this parameter is set, the file can be copied to the clipboard 
                 even if it is encrypted. 
        Notes:   Only effective if PDF.Encryption.EncryptFile is set to "1"
                 0 Copying is not enabled (default)
                 1 Copying is enabled        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_Encryption_EnableCopying AS CHARACTER NO-UNDO INIT "0":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Sets the encryption strength. Only effective if PDF.Encryption.EncryptFile is set to "1".
        Notes:   0 40 bit encryption (default)
                 1 128 bit encryption (needs Acrobat Reader version 5 and higher) 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_Encryption_Level AS CHARACTER NO-UNDO INIT "0":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The owner password for the encrypted file. 
        Notes:   This password is needed to edit the file. If no password is given, 
                 a random password will be assigned. We recommend that you always 
                 explicitly choose a suitable password.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_OwnerPassword AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The user password for the encrypted file.
        Notes:   This password is needed to access the encrypted file. If no password 
                 is given, access is possible without a password, but may be limited (see above).
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_UserPassword AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Sets the TrueType embedding mode.
        Notes:   0 No embedding. The TrueType fonts on the target machine will be used (if available), or the font mapper will choose substitutes.
                 1 All TrueType fonts are embedded.
                 2 Only symbol fonts are embedded.
                 3 No embedding. All fonts will be replaced by PostScript standard fonts.
                 4 Subset embedding. Only the characters from the selected code page are embedded.
                 5 Subset embedding. Only the used characters are embedded. (default)
                 6 CID fonts will be used. Suggested for unicode text.
                 7 Draw fonts as curves.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_FontMode AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Allows you to pass a semicolon-separated list of fonts that will 
                 never be embedded, regardless of the PDF font mode setting. 
        Notes:   Default: "Arial".
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_ExcludedFonts AS CHARACTER NO-UNDO INIT "Arial":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Sets the compression method for the PDF stream.
        Notes:   0 No compression
                 1 Flate compression (default)
                 2 Run-Length compression
                 3 FastFlate compression
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_CompressStreamMethod AS CHARACTER NO-UNDO INIT "1":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Specifies the quality and the corresponding compression factor of 
                 the generated JPEG graphic. The value lies between 0..100, with 100 
                 representing the highest quality (least compression). 
        Notes:   Default: 75
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Picture_JPEGQuality AS CHARACTER NO-UNDO INIT "75":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Influences the way that coordinate transforming records are handled 
                 and may help to avoid rounding differences or objects getting lost 
                 when dealing with very complex custom EMF records. 
        Notes:   Set to "1" to enable (default: "0").
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_DontStackWorldModifications AS CHARACTER NO-UNDO INIT "0":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Use to create a PDF of type PDF/A.
        Notes:   0 no PDF/A is created (default)
                 1 create a PDF/A-1a
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_PDFAMode AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Set the Author tag of the PDF file. 
        Notes:   Default: empty.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PDF_Author AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LlPdfExportModule class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC LlPdfExportModule ():
		SUPER ().
		
		THIS-OBJECT:ExportProperties = THIS-OBJECT:ExportProperties + ",":U + 
            "PDF_Title,PDF_Subject,PDF_Keywords,PDF_Encryption_EncryptFile,PDF_Encryption_EnableChanging,PDF_Encryption_EnableCopying,":U +
            "PDF_Encryption_Level,PDF_OwnerPassword,PDF_UserPassword,PDF_FontMode,PDF_ExcludedFonts,PDF_CompressStreamMethod,":U +
            "Picture_JPEGQuality,PDF_DontStackWorldModifications,PDF_PDFAMode,PDF_Author":U .		
		
		THIS-OBJECT:ExtensionType = LlConst:LL_LLX_EXTENSIONTYPE_EXPORT . 
		
	END CONSTRUCTOR.

END CLASS.
