*&---------------------------------------------------------------------*
*& Report ZOT_29_P_BAPI_SALESORDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsy_006_p_bapi_salesorder.

TABLES: vbak, vbap, likp, vbkd.

DATA: v_vbeln                TYPE vbak-vbeln,
      order_header_in        TYPE bapisdhd1,
      order_header_inx       TYPE bapisdhd1x,
      gt_order_items_in      TYPE TABLE OF bapisditm,
      gt_order_items_inx     TYPE TABLE OF bapisditmx,
      gt_order_partners      TYPE TABLE OF bapiparnr,
      gt_order_schedules_in  TYPE TABLE OF bapischdl,
      gt_order_schedules_inx TYPE TABLE OF bapischdlx,
      gt_return              TYPE TABLE OF bapiret2,
      gs_order_items_in      TYPE bapisditm,
      gs_order_items_inx     TYPE bapisditmx,
      gs_order_partners      TYPE bapiparnr,
      gs_order_schedules_in  TYPE bapischdl,
      gs_order_schedules_inx TYPE bapischdlx.

START-OF-SELECTION.

  SELECT vkorg, vtweg, spart ,augru ,audat, bstnk ,auart
    FROM vbak
    WHERE auart <> 'ZI01' OR auart <> 'ZI02'
    INTO TABLE @DATA(lt_vbak).

  SELECT posnr, matnr, kwmeng, werks, kunnr_ana , kunwe_ana
    FROM vbap
    INTO TABLE @DATA(lt_vbap).

  READ TABLE lt_vbak INTO DATA(ls_vbak) INDEX 1.
  IF sy-subrc = 0.

    order_header_in = VALUE #( doc_type   = ls_vbak-auart
                               sales_org  = ls_vbak-vkorg
                               distr_chan = ls_vbak-vtweg
                               division   = ls_vbak-spart
                               doc_date   = ls_vbak-audat
                               purch_no_c = ls_vbak-bstnk  ).

    order_header_inx = VALUE #( updateflag = 'I'
                                doc_type   = 'X'
                                sales_org  = 'X'
                                distr_chan = 'X'
                                division   = 'X'
                                doc_date   = 'X'
                                purch_no_c = 'X' ).

  ENDIF.

  READ TABLE lt_vbap INTO DATA(ls_vbap) INDEX 1.
  IF sy-subrc = 0.

    gt_order_items_in = VALUE #( BASE gt_order_items_in ( itm_number = ls_vbap-posnr
                                                          material   = ls_vbap-matnr
                                                          plant      = ls_vbap-werks
                                                          target_qty = ls_vbap-kwmeng ) ).

    gt_order_items_inx = VALUE #( BASE gt_order_items_inx ( itm_number = ls_vbap-posnr
                                                            material   = 'X'
                                                            plant      = 'X'
                                                            target_qty = 'X' ) ).

    gt_order_partners = VALUE #( BASE gt_order_partners ( partn_role = 'AG' partn_numb = ls_vbap-kunnr_ana )
                                                        ( partn_role = 'WE' partn_numb = ls_vbap-kunwe_ana )
                                                        ( partn_role = 'RG' partn_numb = ls_vbap-kunnr_ana )
                                                        ( partn_role = 'RE' partn_numb = ls_vbap-kunnr_ana ) ).

    gt_order_schedules_in = VALUE #( BASE gt_order_schedules_in ( itm_number = ls_vbap-posnr
                                                                  sched_line = '01'
                                                                  req_qty    = ls_vbap-kwmeng ) ).

    gt_order_schedules_inx = VALUE #( BASE gt_order_schedules_inx ( itm_number = ls_vbap-posnr
                                                                    sched_line = '01'
                                                                    updateflag = 'I'
                                                                    req_qty    = 'X' ) ).

  ENDIF.

  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
    EXPORTING
      order_header_in     = order_header_in
      order_header_inx    = order_header_inx
    IMPORTING
      salesdocument       = v_vbeln
    TABLES
      return              = gt_return
      order_items_in      = gt_order_items_in
      order_items_inx     = gt_order_items_inx
      order_partners      = gt_order_partners
      order_schedules_in  = gt_order_schedules_in
      order_schedules_inx = gt_order_schedules_inx.

  LOOP AT gt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
    EXIT.

  ENDLOOP.

  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    WRITE :/ v_vbeln.
  ENDIF.
