
/*------------------------------------------------------------------------
    File        : system/EncryptionProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Fri Apr 29 22:49:45 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE EncryptString:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcData AS LONGCHAR  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcKey   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcData AS LONGCHAR  NO-UNDO.

    RUN pEncrypt (iplcData, ipcKey, OUTPUT oplcData).
END PROCEDURE.

PROCEDURE DecryptString:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcData AS LONGCHAR  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcKey   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcData AS LONGCHAR  NO-UNDO.

    RUN pDecrypt (iplcData, ipcKey, OUTPUT oplcData).
END PROCEDURE.

PROCEDURE EncryptFile:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcKey      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcData    AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValid     AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lcFileData AS LONGCHAR NO-UNDO.
    
    RUN FileSys_ValidateFile (ipcFileName, OUTPUT lValid, OUTPUT opcMessage).
    
    IF NOT lValid THEN DO:
        oplError = TRUE.
        RETURN.
    END.
    
    COPY-LOB FROM FILE ipcFileName TO lcFileData.
    
    RUN pEncrypt (lcFileData, ipcKey, OUTPUT oplcData).
END PROCEDURE.

PROCEDURE DecryptFile:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcKey      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcData    AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValid     AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lcFileData AS LONGCHAR NO-UNDO.
    
    RUN FileSys_ValidateFile (ipcFileName, OUTPUT lValid, OUTPUT opcMessage).
    
    IF NOT lValid THEN DO:
        oplError = TRUE.
        RETURN.
    END.
    
    COPY-LOB FROM FILE ipcFileName TO lcFileData.
    
    RUN pDecrypt (lcFileData, ipcKey, OUTPUT oplcData).
END PROCEDURE.

PROCEDURE pEncrypt PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcData AS LONGCHAR  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcKey   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcData AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE rKey      AS RAW    NO-UNDO.
    DEFINE VARIABLE mmptrData AS MEMPTR NO-UNDO.
    
    LENGTH(rKey) = LENGTH(ipcKey).
    
    PUT-STRING(rKey, 1, LENGTH(ipcKey)) = ipcKey.
    
    /* For AES_OFB_128 the key needs to be 15 characters. GENERATE-PBE-KEY converts a variable length key to 128 bit key */
    rKey = GENERATE-PBE-KEY(rKey).
    
    mmptrData = ENCRYPT(iplcData, rKey, ?, "AES_OFB_128").
    
    oplcData = BASE64-ENCODE(mmptrData).
END PROCEDURE.

PROCEDURE pDecrypt PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcData AS LONGCHAR  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcKey   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcData AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE rKey      AS RAW    NO-UNDO.
    DEFINE VARIABLE mmptrData AS MEMPTR NO-UNDO.
    
    LENGTH(rKey) = LENGTH(ipcKey).
    
    PUT-STRING(rKey, 1, LENGTH(ipcKey)) = ipcKey.
    
    mmptrData = BASE64-DECODE(iplcData).
    /* For AES_OFB_128 the key needs to be 15 characters. GENERATE-PBE-KEY converts a variable length key to 128 bit key */
    rKey = GENERATE-PBE-KEY(rKey).

    oplcData = GET-STRING(DECRYPT(mmptrData, rKey, ?, "AES_OFB_128"),1).
END PROCEDURE.

