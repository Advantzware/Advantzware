/* --------------------------------------------------- sys/ref/convptom.p 5/12*/
/* Convert Price per UOM to Price per M.  BV-5/2012             */
/* -------------------------------------------------------------------------- */

def shared var cocode as ch no-undo.

DEF INPUT PARAMETER ip-uom  AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER ip-price LIKE quoteit.price NO-UNDO.
DEF INPUT PARAMETER ip-qty LIKE quoteit.qty NO-UNDO.
DEF INPUT PARAMETER ip-case-count LIKE eb.cas-cnt  NO-UNDO.

DEFINE OUTPUT PARAMETER op-converted-price AS DECIMAL NO-UNDO. 

CASE ip-uom:
    WHEN "M" THEN
        op-converted-price = ip-price.
    WHEN "EA" THEN
        op-converted-price = ip-price * 1000.
    WHEN "C" THEN
        op-converted-price = ip-price * 10.
    WHEN "L" THEN
        IF ip-qty <> 0 THEN
            op-converted-price = ip-price / ip-qty * 1000.
        ELSE
            op-converted-price = 0. /*error return value*/
    WHEN "CS" THEN
        IF ip-case-count <> 0 THEN
            op-converted-price = ip-price / ip-case-count * 1000.
        ELSE
            op-converted-price = 0. /*error return value*/
    OTHERWISE
        op-converted-price = 0. /*error return value*/

 END CASE.
