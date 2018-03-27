
{custom/windows.i}
DEF OUTPUT PARAMETER op-printer-ports AS cha NO-UNDO.
DEF OUTPUT PARAMETER op-printer-names AS cha NO-UNDO.

DEFINE VARIABLE pPrinterEnum  AS MEMPTR NO-UNDO.
DEFINE VARIABLE pcbNeeded     AS INTEGER NO-UNDO.
DEFINE VARIABLE pcReturned    AS INTEGER NO-UNDO.
DEFINE VARIABLE RetValue      AS INTEGER NO-UNDO.
 
DEFINE VARIABLE pPrinterInfo  AS MEMPTR NO-UNDO.
DEFINE VARIABLE StructSize    AS INTEGER INITIAL 84.
 
DEFINE VARIABLE i             AS INTEGER NO-UNDO.
DEFINE VARIABLE lpPrinterName AS MEMPTR  NO-UNDO.
DEFINE VARIABLE lpPortName    AS MEMPTR  NO-UNDO.

  /* The first call to EnumPrinters is only to 
     get the required memory size */
   SET-SIZE(pPrinterEnum) = 30.  /* A default bobo value */
   RUN EnumPrinters{&A} IN hpApi(2, /* = PRINTER_ENUM_LOCAL */
                                 "", 
                                 2, 
                                 GET-POINTER-VALUE(pPrinterEnum),
                                 GET-SIZE(pPrinterEnum), 
                                 OUTPUT pcbNeeded, 
                                 OUTPUT pcReturned, 
                                 OUTPUT RetValue).
 
   /* RetValue will now be FALSE (=error) because we did not
      supply enough memory. But at least we know now how much
      memory was required (pcbNeeded) and also how many printers
      were found (pcReturned) */

   /* no printers installed, then return (rare) */
   /*
   IF pcbNeeded=0 THEN DO:
      MESSAGE "No printers found".
      RUN DeAlloc.
      RETURN.
   END.
   */

   /* Reset the size of pPrinterEnum to the correct size */
   SET-SIZE(pPrinterEnum) = 0.
   SET-SIZE(pPrinterEnum) = pcbNeeded.

   /* The second call actually fills the pPrinterEnum structure */
   RUN EnumPrinters{&A} IN hpApi(2,  /* = PRINTER_ENUM_LOCAL */
                                 "", 
                                 2,
                                 GET-POINTER-VALUE (pPrinterEnum),
                                 GET-SIZE(pPrinterEnum), 
                                 OUTPUT pcbNeeded,
                                 OUTPUT pcReturned, 
                                 OUTPUT RetValue).
 
   /* pPrinterEnum holds a couple of PRINTER_INFO_2 records.
      the number of records is pcReturned.
      the number of bytes copied to pPrinterEnum is pcbNeeded.
      size of one PRINTER_INFO_2 record is 84 bytes.
   */
 /*  IF PCRETURNED > 1 THEN */

   DO i=0 TO pcReturned - 1 :       
 
      SET-POINTER-VALUE(pPrinterInfo) = GET-POINTER-VALUE(pPrinterEnum) + (i * StructSize).
 
      /* the second LONG field in the PRINTER_INFO_2 structure is 
         a pointer to a string holding the printer name */
      SET-POINTER-VALUE(lpPrinterName) = GET-LONG(pPrinterInfo, 5).
 
      /* the 4th LONG field in the PRINTER_INFO_2 structure is 
         a pointer to a string holding the port name */
      SET-POINTER-VALUE(lpPortName)    = GET-LONG(pPrinterInfo,13).
 
       op-printer-ports = op-printer-ports + GET-STRING(lpPortName,1) + "," .
       op-printer-names = op-printer-names + GET-STRING(lpPrinterName,1) + "," .
 
   END.
   
   /* === this is for window xp - does not get all printer ports. 
          you may get duplicated printer ports - need to do something */
      /* Reset the size of pPrinterEnum to the correct size */
      /* no network printers defined on XP - hang here */

   SET-SIZE(pPrinterEnum) = 0.
   SET-SIZE(pPrinterEnum) = pcbNeeded.
   /* The third call actually fills the pPrinterEnum structure */ 
   RUN EnumPrinters{&A} IN hpApi(4,  /* = PRINTER_ENUM_network */
                                 "", 
                                 2,
                                 GET-POINTER-VALUE (pPrinterEnum),
                                 GET-SIZE(pPrinterEnum), 
                                 OUTPUT pcbNeeded,
                                 OUTPUT pcReturned, 
                                 OUTPUT RetValue).
 
   IF PCRETURNED > 1 THEN
   DO i=0 TO pcReturned - 1 :       
      SET-POINTER-VALUE(pPrinterInfo) = GET-POINTER-VALUE(pPrinterEnum) + (i * StructSize).
      SET-POINTER-VALUE(lpPrinterName) = GET-LONG(pPrinterInfo, 5).
      SET-POINTER-VALUE(lpPortName)    = GET-LONG(pPrinterInfo,13).

      op-printer-ports = op-printer-ports + GET-STRING(lpPortName,1) + "," .
      op-printer-names = op-printer-names + GET-STRING(lpPrinterName,1) + "," .      
   END.

 /* end of third running -========*/

   /* === this is for window 2000 Prof. - does not get all printer ports. 
          you may get duplicated printer ports - need to do something */
      /* Reset the size of pPrinterEnum to the correct size */
   SET-SIZE(pPrinterEnum) = 0.
   SET-SIZE(pPrinterEnum) = pcbNeeded.
   /* The third call actually fills the pPrinterEnum structure */ 
   RUN EnumPrinters{&A} IN hpApi(5,  /* = PRINTER_ENUM_NET */
                                 "", 
                                 2,
                                 GET-POINTER-VALUE (pPrinterEnum),
                                 GET-SIZE(pPrinterEnum), 
                                 OUTPUT pcbNeeded,
                                 OUTPUT pcReturned, 
                                 OUTPUT RetValue).


   IF retvalue <> 0 THEN
   DO i=0 TO pcReturned - 1 :       
      SET-POINTER-VALUE(pPrinterInfo) = GET-POINTER-VALUE(pPrinterEnum) + (i * StructSize).
      SET-POINTER-VALUE(lpPrinterName) = GET-LONG(pPrinterInfo, 5).
      SET-POINTER-VALUE(lpPortName)    = GET-LONG(pPrinterInfo,13).

      op-printer-ports = op-printer-ports + GET-STRING(lpPortName,1) + "," .
      op-printer-names = op-printer-names + GET-STRING(lpPrinterName,1) + "," .      
 
   END.
   
   /* Clean Up  */
   RUN DeAlloc.


PROCEDURE DeAlloc:
   SET-SIZE(pPrinterEnum) = 0.
END.

 

