/*oe/tt-item-qty-price.i */

def temp-table tt-item-qty-price
    FIELD tt-selected AS LOG
    FIELD LINE AS INT
    field qty as int form ">>>,>>>,>>9" 
    FIELD q-no LIKE quotehd.q-no FORM ">>>>>>9"
    FIELD part-no LIKE quoteitm.part-no
    FIELD quote-date LIKE quoteqty.quote-date
    FIELD price LIKE quoteqty.price FORM ">>>,>>9.99<<<"
    FIELD uom LIKE quoteqty.uom
    FIELD rels LIKE quoteqty.rels
    FIELD quote-user LIKE quoteqty.quote-user
    INDEX primary-index AS PRIMARY LINE
    INDEX tt-selected tt-selected part-no
    INDEX q-no q-no.
