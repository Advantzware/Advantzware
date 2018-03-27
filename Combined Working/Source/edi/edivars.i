/*  Software Associates standard variable fields */
def {1} var cancel_date#            as date NO-UNDO.
def {1} var delivery_date#          as date no-undo.
def {1} var discount_due_date#      as date NO-UNDO.
def {1} var extra_date#             as date NO-UNDO.
def {1} var invoice_date#           as date NO-UNDO.
def {1} var manifest_create_date#   as date NO-UNDO.
def {1} var net_date#               as date NO-UNDO.
def {1} var purchase_order_date#    as date NO-UNDO.
def {1} var sched_delivery_date#    as date NO-UNDO.
def {1} var send_date#              as date NO-UNDO.
def {1} var ship_date#              as date NO-UNDO.
def {1} var vendor_order_date#      as date NO-UNDO.
def {1} var terms_deferred_due#     as date no-undo.

def {1} var address1                            AS CHAR NO-UNDO FORMAT 'x(35)'.
def {1} var address2                            AS CHAR NO-UNDO FORMAT 'x(35)'.
def {1} var address3                            AS CHAR NO-UNDO FORMAT 'x(35)'.
def {1} var address4                            AS CHAR NO-UNDO FORMAT 'x(35)'.
def {1} var AFE_number                          AS CHAR NO-UNDO FORMAT 'x(15)'.
def {1} var allow_charge_description            AS CHAR NO-UNDO FORMAT 'x(80)'.
def {1} var allow_charge_indicator              AS CHAR NO-UNDO FORMAT 'x(01)'.
def {1} var allow_charge_number                 AS CHAR NO-UNDO FORMAT 'x(16)'.
def {1} var allow_charge_percent        AS DEC NO-UNDO FORMAT '-99999999'.
def {1} var allow_charge_quantity       AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var allow_charge_total_amount   AS DEC NO-UNDO FORMAT '-99999999'.
def {1} var allow_charge_unit_measure           AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var back_order_code                     AS CHAR NO-UNDO FORMAT "x(01)".
def {1} var back_order_handling_number          AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var backorder_flag                      AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var bill_of_lading_number               AS CHAR NO-UNDO FORMAT 'x(15)'.
def {1} var bill_to_ship_to_code                AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var cancel_date                         as char NO-UNDO.
def {1} var cancel_date_qualifier               AS CHAR NO-UNDO FORMAT "x(03)".
def {1} var carrier                             AS CHAR NO-UNDO FORMAT 'x(30)'.
def {1} var carrier_code                        AS CHAR NO-UNDO FORMAT 'x(06)'.
def {1} var carrier_scac_code                   AS CHAR NO-UNDO FORMAT "x(04)".
def {1} var carton_unit_measure                 AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var case_height                 AS dec NO-UNDO FORMAT "99999999".
def {1} var case_length                 AS dec NO-UNDO FORMAT "99999999".
def {1} var case_weight                 AS dec NO-UNDO FORMAT "999999999".
def {1} var case_width                  AS dec NO-UNDO FORMAT "99999999".
def {1} var catalog_number                      AS CHAR NO-UNDO FORMAT "x(06)".
def {1} var change_indicator                    AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var city                                AS CHAR NO-UNDO FORMAT 'x(19)'.
def {1} var company_code                        AS CHAR NO-UNDO FORMAT 'x(05)'.
def {1} var company_name                        AS CHAR NO-UNDO FORMAT 'x(35)'.
def {1} var company_name_2                      AS CHAR NO-UNDO FORMAT 'x(35)'.
def {1} var configuration_code                  AS CHAR NO-UNDO FORMAT "x(01)".
def {1} var contact_function_code               AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var contact_name                        AS CHAR NO-UNDO FORMAT 'x(35)'.
def {1} var contact_number_qualifier            AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var contact_phone_number                AS CHAR NO-UNDO FORMAT 'x(21)'.
def {1} var contract_or_release_number          AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var contract_number                     AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var country                             AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var cube_this_order             AS dec NO-UNDO FORMAT "999999999".
def {1} var currency_denom_buyer                AS CHAR NO-UNDO FORMAT "x(03)".
def {1} var currency_denom_seller               AS CHAR NO-UNDO FORMAT "x(03)".
def {1} var customer_edi_id                     AS CHAR NO-UNDO FORMAT 'x(15)'.
def {1} var customer_item_number                AS CHAR NO-UNDO FORMAT 'x(30)'.
def {1} var customer_line_number            AS INT NO-UNDO FORMAT "9999999999".
def {1} var customer_number                     AS CHAR NO-UNDO FORMAT 'x(12)'.
def {1} var customer_sku_number                 AS CHAR NO-UNDO FORMAT 'x(40)'.
def {1} var day_of_month                    AS INT NO-UNDO FORMAT '99'.
def {1} var debit_credit_flag                   AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var delivery_date                       AS CHAR FORMAT "x(06)".
def {1} var delivery_date_qualifier             AS CHAR FORMAT "x(02)".
def {1} var department_number                   AS CHAR NO-UNDO FORMAT 'x(06)'.
def {1} var discount_amount             AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var discount_due_date                   as char NO-UNDO FORMAT '999999'.
def {1} var discount_due_days               AS INT NO-UNDO FORMAT '999'.
def {1} var discount_percent                    AS dec NO-UNDO FORMAT '999999'.
def {1} var division_number                     AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var dtm_century                         AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var duns_or_location_number             AS CHAR NO-UNDO FORMAT 'x(13)'.
def {1} var duns_qualifier                      AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var edi_funct_group_control_number      AS CHAR NO-UNDO FORMAT "x(09)".
def {1} var edi_receiver_id                     AS CHAR NO-UNDO FORMAT "x(15)".
def {1} var edi_sender_id                       AS CHAR NO-UNDO FORMAT "x(15)".
def {1} var edi_set_control_number              AS CHAR NO-UNDO FORMAT "x(09)".
def {1} var edi_standard                        AS CHAR NO-UNDO FORMAT "x(01)".
def {1} var edi_transmission_control_number     AS CHAR NO-UNDO FORMAT "x(09)".
def {1} var edi_version                         AS CHAR NO-UNDO FORMAT "x(03)".
def {1} var entity_id                           AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var equipment_code                      AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var equipment_initial                   AS CHAR NO-UNDO FORMAT "x(04)".
def {1} var exchange_rate_buyer         AS DEC  NO-UNDO FORMAT "99999999".
def {1} var exchange_rate_seller        AS DEC  NO-UNDO FORMAT "99999999".
def {1} var extra_date                          as char NO-UNDO.
def {1} var extra_date_qualifier                AS CHAR NO-UNDO FORMAT "x(03)".
def {1} var fob_code                            AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var fob_location_qualifier              AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var fob_text                            AS CHAR NO-UNDO FORMAT 'x(80)'.
def {1} var hl_structure_code                   AS CHAR NO-UNDO FORMAT 'x(04)'. /* 9803 CAH */
def {1} var hl_id                               AS CHAR NO-UNDO FORMAT 'x(12)'. /* 9803 CAH */
def {1} var hl_parent_id                        AS CHAR NO-UNDO FORMAT 'x(12)'. /* 9803 CAH */
def {1} var hl_child_id                         AS CHAR NO-UNDO FORMAT 'x(12)'. /* 9803 CAH */
def {1} var hl_level_code                       AS CHAR NO-UNDO FORMAT 'x(12)'. /* 9803 CAH */
def {1} var invoice_date                        as char NO-UNDO FORMAT '999999'.
def {1} var invoice_number                      AS CHAR NO-UNDO FORMAT 'x(22)'.
def {1} var id_code_qualifier                   AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var id_code                             AS CHAR NO-UNDO FORMAT "x(20)".
def {1} var item_assigned_id /* sequential # */ AS CHAR NO-UNDO FORMAT 'x(20)'. 
def {1} var item_assigned_id2                   AS CHAR NO-UNDO FORMAT 'x(20)'. 
def {1} var item_color                          AS CHAR NO-UNDO FORMAT 'x(15)'.
def {1} var item_description                    AS CHAR NO-UNDO FORMAT 'x(80)'.
def {1} var item_discount_dollars       AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var item_each_cube              AS DEC NO-UNDO FORMAT '999999'.
def {1} var item_each_weight            AS DEC NO-UNDO FORMAT '999999'.
def {1} var item_gross_dollars          AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var item_master_carton_cube     AS DEC NO-UNDO FORMAT '999999'.
def {1} var item_master_carton_weight   AS DEC NO-UNDO FORMAT '999999'.
def {1} var item_net_dollars                    AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var item_product_qualifier              AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var item_shipment_status                AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var item_size                           AS CHAR NO-UNDO FORMAT 'x(15)'.
def {1} var item_special_services_code          AS CHAR NO-UNDO FORMAT 'x(04)'.
def {1} var item_status_code                    AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var line_sequence_number            AS INT NO-UNDO FORMAT '9999'.
def {1} var location_number                     AS CHAR NO-UNDO FORMAT 'x(10)'.
def {1} var manifest_create_date                AS CHAR NO-UNDO FORMAT "x(06)".
def {1} var manifest_create_time                AS CHAR NO-UNDO FORMAT "x(04)".
def {1} var mark_for_store_number               AS CHAR NO-UNDO FORMAT "x(04)".
def {1} var measurement_1                       AS CHAR NO-UNDO FORMAT "x(10)".
def {1} var measurement_2                       AS CHAR NO-UNDO FORMAT "x(10)".
def {1} var measurement_3                       AS CHAR NO-UNDO FORMAT "x(10)".
def {1} var measurement_qualifier_1             AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var measurement_qualifier_2             AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var measurement_qualifier_3             AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var method_of_handling                  AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var name2                               AS CHAR NO-UNDO FORMAT 'x(35)'.
def {1} var name3                               AS CHAR NO-UNDO FORMAT 'x(35)'.
def {1} var net_date                            as char NO-UNDO FORMAT '999999'.
def {1} var net_days                        AS INT NO-UNDO FORMAT '999'.
def {1} var note                                AS CHAR NO-UNDO FORMAT 'x(80)'.
def {1} var note_reference_code                 AS CHAR NO-UNDO FORMAT 'x(3)'.
def {1} var note_sequence_number            AS INT NO-UNDO FORMAT '9999'.
def {1} var nrma_color_code                     AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var number_of_detail_lines          AS INT NO-UNDO FORMAT '999999'.
def {1} var number_of_eaches_per_inner      AS INT NO-UNDO FORMAT "999999".
def {1} var number_of_inner_packs           AS INT NO-UNDO FORMAT "999999".
def {1} var number_of_line_items            AS INT NO-UNDO FORMAT "999999".
def {1} var number_of_lines_this_order      AS INT NO-UNDO FORMAT "999999".
def {1} var order_change_quantity               AS DEC NO-UNDO FORMAT "9999999999".
def {1} var order_package_code                  AS CHAR NO-UNDO FORMAT "x(05)".
def {1} var order_sequence_number           AS INT NO-UNDO FORMAT '999999'.
def {1} var order_status                        AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var ordering_store_number               AS CHAR NO-UNDO FORMAT "x(10)".
def {1} var pack_size                   AS dec NO-UNDO FORMAT '999999'.
def {1} var packaging_code                      AS CHAR NO-UNDO FORMAT "x(15)".
def {1} var pallet_mark                         AS CHAR NO-UNDO FORMAT "x(20)".
def {1} var partner_duns_number                 AS CHAR NO-UNDO FORMAT 'x(13)'.
def {1} var price_basis                         AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var pro_number                          AS CHAR NO-UNDO FORMAT 'x(15)'.
def {1} var product_type                        AS CHAR NO-UNDO FORMAT 'x(03)'.
def {1} var promotion_event_code                AS CHAR NO-UNDO FORMAT "x(15)".
def {1} var purchase_order_date                 AS char NO-UNDO FORMAT '999999'.
def {1} var purchase_order_line_number      AS INT NO-UNDO FORMAT "999999".
def {1} var purchase_order_number               AS CHAR NO-UNDO FORMAT 'x(22)'.
def {1} var purchase_order_type                 AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var quantity_ordered            AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var quantity_shipped            AS DEC NO-UNDO FORMAT "-999999999".
def {1} var quantity_variance           AS DEC NO-UNDO FORMAT "-999999999".
def {1} var record_type                     AS INT NO-UNDO FORMAT '99'.
def {1} var reference_1                         AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var reference_1_qualifier               AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var reference_2                         AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var reference_2_qualifier               AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var reference_3                         AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var reference_3_qualifier               AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var release_number                      AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var retailers_vendor_number             AS CHAR NO-UNDO FORMAT "x(15)".
def {1} var routing                             AS CHAR NO-UNDO FORMAT 'x(50)'.
def {1} var routing_instructions                AS CHAR NO-UNDO FORMAT "x(50)".
def {1} var routing_sequence_code               AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var sac_agency_qualifier                AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var sac_agency_code                     AS CHAR NO-UNDO FORMAT "x(10)".
def {1} var sac_reference_id                    AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var sac_description                     AS CHAR NO-UNDO FORMAT "x(80)".
def {1} var sales_division                      AS CHAR NO-UNDO FORMAT "x(15)".
def {1} var sales_division_number               AS CHAR NO-UNDO FORMAT 'x(03)'.
def {1} var sales_region                        AS CHAR NO-UNDO FORMAT "x(15)".
def {1} var sched_delivery_date                 AS CHAR NO-UNDO FORMAT "x(06)".
def {1} var scheduled_code_1                    AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var scheduled_code_2                    AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var scheduled_code_3                    AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var second_description                  AS CHAR NO-UNDO FORMAT "x(80)".
def {1} var second_product_type                 AS CHAR NO-UNDO FORMAT "x(03)".
def {1} var selling_price               AS DEC NO-UNDO FORMAT "99999999999999".
def {1} var send_date                           as char NO-UNDO FORMAT '999999'.
def {1} var send_time                       AS INT  NO-UNDO FORMAT '999999'.
def {1} var ship_complete_code                  AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var ship_date                           as char NO-UNDO FORMAT '999999'.
def {1} var ship_date_qual_scheduled            AS CHAR NO-UNDO FORMAT "x(03)".
def {1} var ship_date_qualifier                 AS CHAR NO-UNDO FORMAT "x(03)".
def {1} var ship_from_address                   AS CHAR NO-UNDO FORMAT "x(35)".
def {1} var ship_from_city                      AS CHAR NO-UNDO FORMAT "x(19)".
def {1} var ship_from_state                     AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var ship_from_warehouse                 AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var ship_from_zip                       AS CHAR NO-UNDO FORMAT "x(09)".
def {1} var ship_location_qualifier             AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var ship_origin_destination_code        AS CHAR FORMAT "x(02)".
def {1} var ship_time                           AS CHAR NO-UNDO FORMAT "x(04)".
def {1} var ship_time_zone                      AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var shipment_id                         AS CHAR NO-UNDO FORMAT "x(15)".
def {1} var shipment_method_payment_code        AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var shipment_package_code               AS CHAR NO-UNDO FORMAT "x(05)".
def {1} var shipment_status                     AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var shipto_name                         AS CHAR NO-UNDO FORMAT "x(35)".
def {1} var shipto_address1                     AS CHAR NO-UNDO FORMAT "x(35)".
def {1} var shipto_address2                     AS CHAR NO-UNDO FORMAT "x(35)".
def {1} var shipto_city                         AS CHAR NO-UNDO FORMAT "x(19)".
def {1} var shipto_duns_suffix                  AS CHAR NO-UNDO FORMAT "x(13)".
def {1} var shipto_state                        AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var shipto_store_number                 AS CHAR NO-UNDO FORMAT "x(10)".
def {1} var shipto_zip                          AS CHAR NO-UNDO FORMAT "x(09)".
def {1} var special_service_code                AS CHAR NO-UNDO FORMAT 'x(10)'.
def {1} var state                               AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var sub_division_number                 AS CHAR NO-UNDO FORMAT "x(06)". 
                                                            /* 9609 was 03 */
def {1} var supplier_order_number               AS CHAR NO-UNDO FORMAT "x(15)".
def {1} var tare_package_code                   AS CHAR FORMAT "x(05)" NO-UNDO.
def {1} var tax_id                              AS CHAR NO-UNDO FORMAT "x(20)".
def {1} var tax_location                        AS CHAR NO-UNDO FORMAT "x(25)".
def {1} var tax_location_qualifier              AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var taxable_flag                        AS CHAR NO-UNDO FORMAT "x(01)".
def {1} var terms_basis                         AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var terms_description                   AS CHAR NO-UNDO FORMAT 'x(80)'.
def {1} var terms_type                          AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var total_cartons                   AS INT NO-UNDO FORMAT '99999999'.
def {1} var total_disc_dollars          AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var total_frgt_dollars          AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var total_gross_dollars         AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var total_net_dollars           AS DEC NO-UNDO FORMAT '-999999999'.
def {1} var total_order_amount          AS DEC  NO-UNDO FORMAT "-9999999999999".
def {1} var total_order_packages            AS INT NO-UNDO FORMAT "9999999".
def {1} var total_tare_cube             AS dec FORMAT "999999999" NO-UNDO.
def {1} var total_tare_packages             AS INT FORMAT "9999999" NO-UNDO.
def {1} var total_tare_weight           AS dec FORMAT "99999999" NO-UNDO.
def {1} var total_unit_quantity         AS dec NO-UNDO FORMAT '-999999999'.
def {1} var total_volume                AS dec NO-UNDO FORMAT '99999999'.
def {1} var total_weight                AS DEC NO-UNDO FORMAT '99999999'.
def {1} var trailer_number                      AS CHAR NO-UNDO FORMAT 'x(10)'.
def {1} var transaction_purpose_code            AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var transit_time                        AS CHAR NO-UNDO FORMAT "x(04)".
def {1} var transit_time_qualifier              AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var transportation_method_code          AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var ucc128_mark                         AS CHAR NO-UNDO FORMAT "x(20)".
def {1} var unique_order_number                 AS INT NO-UNDO FORMAT '999999'.
def {1} var unit_of_measure                     AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var unit_price                  AS DEC NO-UNDO FORMAT '-999999999.9999'.
def {1} var upc_case_code                       AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var upc_code                            AS CHAR NO-UNDO FORMAT 'x(12)'.
def {1} var vendor_duns_suffix                  AS CHAR NO-UNDO FORMAT "x(13)".
def {1} var vendor_item_number                  AS CHAR NO-UNDO FORMAT 'x(15)'.
def {1} var vendor_name                         AS CHAR NO-UNDO FORMAT "x(35)".
def {1} var vendor_number                       AS CHAR NO-UNDO FORMAT 'x(15)'.
def {1} var vendor_order_date                   AS char NO-UNDO FORMAT "999999".
def {1} var volume_unit_measure                 AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var weight_unit_measure                 AS CHAR NO-UNDO FORMAT 'x(02)'.
def {1} var ws_contact_function_code            AS CHAR NO-UNDO.
def {1} var ws_contact_name                     AS CHAR NO-UNDO.
def {1} var ws_contact_phone_number             AS CHAR NO-UNDO.
def {1} var zip                                 AS CHAR NO-UNDO FORMAT 'x(09)'.

/* 9807 CAH: Additional VARS Added to set up sterling TDF */
def {1} var transaction_description         AS char NO-UNDO FORMAT "x(80)".
def {1} var transaction_type_code           AS char NO-UNDO FORMAT "x(02)".
def {1} var date_qual                       AS char NO-UNDO FORMAT "x(02)".
def {1} var char_date                       AS char NO-UNDO FORMAT "x(06)".
def {1} var char_time                       AS char NO-UNDO FORMAT "x(06)".
def {1} var time_code                       AS char NO-UNDO FORMAT "x(02)".
def {1} var n_dec                           AS int  NO-UNDO FORMAT "9".
def {1} var ws_century                      AS int  NO-UNDO FORMAT "9999".
def {1} var location_qual                   AS char NO-UNDO FORMAT "x(02)".
def {1} var location_code                   AS char NO-UNDO FORMAT "x(25)".
def {1} var ref_qual                        AS char NO-UNDO FORMAT "x(02)".
def {1} var ref_number                      AS char NO-UNDO FORMAT "x(30)".
def {1} var ref_desc                        AS char NO-UNDO FORMAT "x(80)".
def {1} var message_text                    AS char NO-UNDO FORMAT "x(264)".
def {1} var printer_cc_code                 AS char NO-UNDO FORMAT "x(02)".
def {1} var contact_fax_number              AS char NO-UNDO FORMAT "x(25)".
def {1} var delivery_zone                   AS char NO-UNDO format "x(15)".
def {1} var date_time_period                as char no-undo format "x(35)".
def {1} var date_time_period_qual           as char no-undo format "x(03)".
def {1} var entity_relationship_code        as char no-undo format "x(02)".
def {1} var entity_identifier_code          as char no-undo format "x(02)".
def {1} var merchandise_type                as char no-undo format "x(30)".
def {1} var product_id                      as char no-undo format "x(40)".
def {1} var hash_total                      as decimal no-undo.
def {1} var hash_weight                     as decimal no-undo.
def {1} var hash_volume                     as decimal no-undo.
def {1} var ean_code                        as char no-undo format "x(13)".
def {1} var change_order_seq_number         as char no-undo format "x(22)".
def {1} var action_code                     as char no-undo format "x(02)".
def {1} var remit_number                    AS char NO-UNDO format "x(15)".
def {1} var n_dec1                          AS int  NO-UNDO FORMAT "9".
def {1} var n_dec2                          AS int  NO-UNDO FORMAT "9".
def {1} var n_dec3                          AS int  NO-UNDO FORMAT "9".
def {1} var n_dec4                          AS int  NO-UNDO FORMAT "9".
def {1} var total_amount1                   as dec  no-undo format "-9999999999999.99".
def {1} var total_amount2                   as dec  no-undo format "-9999999999999.99".
def {1} var total_amount3                   as dec  no-undo format "-9999999999999.99".
def {1} var total_amount4                   as dec  no-undo format "-9999999999999.99".
def {1} var quantity_invoiced               as dec no-undo format '-999999999'.
def {1} var class_of_trade_code             as char no-undo format 'x(02)'.
def {1} var price_id_code                   as char no-undo format 'x(03)'.
def {1} var item_description_type           as char no-undo format 'x(01)'.
def {1} var product_description_code        as char no-undo format 'x(12)'.
def {1} var alt_invoice_number              as char no-undo format 'x(22)'.
DEF {1} var product_characteristic_code AS CHAR FORMAT 'X(03)' NO-UNDO.
DEF {1} var note_code         AS CHAR FORMAT 'X(03)' NO-UNDO
  LABEL 'Note Cd' INITIAL 'NTE'.
DEF {1} var status_reason_code              AS CHAR FORMAT 'X(03)' NO-UNDO.
DEF {1} var unit_of_measure2                AS CHAR FORMAT 'X(02)' NO-UNDO.
DEF {1} var returnable_container_code       AS CHAR FORMAT 'X(02)' NO-UNDO.
def {1} var quantity_cumulative             AS DEC NO-UNDO FORMAT "-999999999".
def {1} var weight_qual                     AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var size_unit_measure               AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var case_volume                     AS dec no-undo format "-99999999".
def {1} var slp_code                        AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var misc_number1                    as dec no-undo format "-99999999.99999999".
def {1} var misc_number2                    as dec no-undo format "-99999999.99999999".
def {1} var inner_pack                      as int  no-undo format "-9999999999".
def {1} var lading_quantity                 AS INT NO-UNDO FORMAT '99999999'.
def {1} var commodity_code_qual             AS CHAR NO-UNDO FORMAT "x(01)".
def {1} var commodity_code                  AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var lading_desc                     AS CHAR NO-UNDO FORMAT "x(50)".
def {1} var booking_number                  AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var seal_number                     AS CHAR NO-UNDO FORMAT "x(30)".
def {1} var man_qual                        AS CHAR NO-UNDO FORMAT "x(02)".
def {1} var man_number                      AS CHAR NO-UNDO FORMAT "x(48)".
def {1} var man_number2                     AS CHAR NO-UNDO FORMAT "x(48)".
def {1} var maint_type_code                 AS CHAR NO-UNDO FORMAT "x(03)".

def {1} var misc_elem                       as char no-undo extent 100.
