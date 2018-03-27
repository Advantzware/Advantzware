/********************************************************
  File: jcrep\wiptg.i
********************************************************/

DEFINE {1} SHARED TEMP-TABLE w-job NO-UNDO
  FIELD tag-no AS CHAR FORMAT "X(20)"
  FIELD company LIKE job-mat.company
  FIELD location AS CHAR
  FIELD cust-no LIKE job-hdr.cust-no
  FIELD blank-no LIKE job-mat.blank-no
  FIELD rm-i-name LIKE ITEM.i-name
  FIELD rm-i-no LIKE ITEM.i-no
  FIELD fg-i-name LIKE itemfg.i-name
  FIELD fg-i-no LIKE itemfg.i-no
  FIELD job-no LIKE job-mat.job-no
  FIELD job-no2 LIKE job-mat.job-no2
  FIELD qty LIKE job-mat.qty
  FIELD qty-uom LIKE job-mat.qty-uom
  FIELD cons-uom AS CHAR FORMAT "X(4)"
  FIELD frm LIKE job-mat.frm
  FIELD wid LIKE job-mat.wid
  FIELD len LIKE job-mat.len
  FIELD partial-tags AS INT FORMAT '>>>,>>9' /* btr 02151105 */
  FIELD partial-tag-qty AS INT FORMAT '>>,>>>,>>9' /* btr 02151105 */
  FIELD expected-sheet-qty AS INT FORMAT '>>,>>>,>>9' /* btr 02151105 */
  FIELD partial AS INT FORMAT '>>>,>>9'
  FIELD tag-qty LIKE rm-rctd.qty
  FIELD total-tags AS INT
  FIELD upd-date AS DATE
  FIELD upd-time AS INT
  FIELD tag-date AS DATE
  FIELD tag-time AS INT
  FIELD num-out AS INT
  FIELD num-up AS INT
  FIELD rm-whs AS CHAR
  FIELD rm-bin AS CHAR
  FIELD job-due-date AS DATE
  FIELD last-mach-code AS CHAR
  FIELD last-prod-qty AS DEC DECIMALS 6
  FIELD sheets-tag AS INT
  FIELD sht-wid AS DEC DECIMALS 4
  FIELD sht-len AS DEC DECIMALS 4
  FIELD wip-whs AS CHAR
  FIELD wip-bin AS CHAR
  FIELD first-mach-code AS CHAR
  FIELD tag-timex AS CHAR
  FIELD processed AS LOG
  FIELD rmtag2 AS CHAR FORMAT "X(20)". /* btr */
  
