*&---------------------------------------------------------------------*
*& Include          ZFI_LIQUIDACION_VENTAS_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM get_data.

*  BREAK consulabap05.
  SELECT * FROM i_operationalacctgdocitem INTO TABLE @DATA(lt_docitem)
    WHERE "companycode IN @s_bukrs
          companycode EQ @p_bukrs
      AND financialaccounttype EQ 'D'
      AND issalesrelated EQ 'X'
      AND documentdate IN @s_bldat
      AND clearingjournalentry EQ ''
      AND debitcreditcode EQ 'S'.

  IF lt_docitem[] IS NOT INITIAL.

    SELECT * FROM vbrk INTO TABLE @DATA(lt_vbrk)
      FOR ALL ENTRIES IN @lt_docitem
    WHERE vbeln EQ @lt_docitem-originalreferencedocument(10).

    IF lt_vbrk[] IS NOT INITIAL.
      SELECT * FROM vbfa INTO TABLE @DATA(lt_vbfa)
        FOR ALL ENTRIES IN @lt_vbrk
        WHERE vbeln EQ @lt_vbrk-vbeln
          AND vbtyp_v EQ 'C'.
      IF lt_vbfa[] IS NOT INITIAL.
        SELECT * FROM vbap INTO TABLE @DATA(lt_vbap)
          FOR ALL ENTRIES IN @lt_vbfa
          WHERE vbeln EQ @lt_vbfa-vbelv
            AND route IN @s_route.
      ENDIF.
    ENDIF.

    SELECT * FROM bkpf INTO TABLE @DATA(lt_bkpf)
      FOR ALL ENTRIES IN @lt_docitem
      WHERE belnr EQ @lt_docitem-accountingdocument
        AND bukrs EQ @lt_docitem-companycode
        AND gjahr EQ @lt_docitem-fiscalyear
        AND xref1_hd IN @s_route.

    SELECT * FROM bkpf INTO TABLE @DATA(lt_bkpf_fact)
      FOR ALL ENTRIES IN @lt_docitem
      WHERE belnr EQ @lt_docitem-accountingdocument
        AND bukrs EQ @lt_docitem-companycode
        AND gjahr EQ @lt_docitem-fiscalyear.

    SELECT * FROM kna1 INTO TABLE @DATA(lt_kna1)
      FOR ALL ENTRIES IN @lt_docitem
      WHERE kunnr EQ @lt_docitem-customer.
    IF lt_kna1[] IS NOT INITIAL.
      SELECT * FROM adrc INTO TABLE @DATA(lt_adrc)
        FOR ALL ENTRIES IN @lt_kna1
        WHERE addrnumber EQ @lt_kna1-adrnr.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&           G E T   H E A D E R
*&---------------------------------------------------------------------*

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      input  = 'ES'
    IMPORTING
      output = gv_spras.

  CONCATENATE s_bldat-low+6(2) s_bldat-low+4(2) s_bldat-low(4) INTO gv_date SEPARATED BY '/'.

  SELECT SINGLE adrnr FROM t001 INTO @DATA(lv_adrnr)
    WHERE "bukrs IN @s_bukrs.
           bukrs EQ @p_bukrs.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM adrc INTO @DATA(ls_adrc)
      WHERE addrnumber EQ @lv_adrnr.

    DATA(lv_names) = |{ ls_adrc-name1 }{ ls_adrc-name2 }{ ls_adrc-name3 }{ ls_adrc-name4 }|.
  ENDIF.

  SELECT SINGLE bezei FROM tvrot INTO @DATA(lv_bezei)
    WHERE route IN @s_route
      AND spras EQ @gv_spras.

  gs_header-nombre = lv_names.
  gs_header-titulo = 'LIQUIDACIÓN DE VENTAS'.
  CONCATENATE 'FECHA:' gv_date INTO gs_header-fecha SEPARATED BY space.
  CONCATENATE 'RUTA:' s_route-low INTO gs_header-ruta SEPARATED BY space.

  CLEAR: gv_date, lv_adrnr, ls_adrc, lv_names, lv_bezei.

*&---------------------------------------------------------------------*
*&           G E T   D A T A
*&---------------------------------------------------------------------*
  LOOP AT lt_docitem INTO DATA(ls_docitem).

*    Fecha Documento
    MOVE ls_docitem-documentdate TO gs_data-bldat.
    MOVE p_budat TO gs_data-budat.
*    MOVE p_budat TO gs_data-budat.
*    Documento SAP
    MOVE ls_docitem-accountingdocument TO gs_data-sapdo.
*    Texto
    MOVE p_sgtxt TO gs_data-sgtxt.
*    Business Partner
    MOVE ls_docitem-customer TO gs_data-kunnr.
*    Importe de Factura
    MOVE ls_docitem-amountintransactioncurrency TO gs_data-dmbtr.
*    WRITE ls_docitem-amountintransactioncurrency TO gs_data-dmbtr.
*    Importe Cobrado (Editable)
    MOVE ls_docitem-amountintransactioncurrency TO gs_data-zimpc.
*    WRITE ls_docitem-amountintransactioncurrency TO gs_data-zimpc.

*    Factura no Cobrada (Editable)
*    Nro. Cobro         (Editable)

*    Sociedad
    MOVE ls_docitem-companycode TO gs_data-bukrs.
*    Asignacion
    MOVE ls_docitem-assignmentreference TO gs_data-assig.
*    Referencia de pago
    MOVE ls_docitem-originalreferencedocument TO gs_data-payre.
*    Moneda
    MOVE ls_docitem-transactioncurrency TO gs_data-waers.

*    Ruta
    CASE ls_docitem-referencedocumenttype.
      WHEN 'VBRK'.
        READ TABLE lt_vbrk INTO DATA(ls_vbrk) WITH KEY vbeln = ls_docitem-originalreferencedocument(10).
        IF sy-subrc EQ 0.
          READ TABLE lt_vbfa INTO DATA(ls_vbfa) WITH KEY vbeln = ls_vbrk-vbeln.
          IF sy-subrc EQ 0.
            READ TABLE lt_vbap INTO DATA(ls_vbap) WITH KEY vbeln = ls_vbfa-vbelv.
            IF sy-subrc EQ 0.
              MOVE ls_vbap-route TO gs_data-route.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: ls_vbrk, ls_vbap.
      WHEN 'BKPF'.
        READ TABLE lt_bkpf INTO DATA(ls_bkpf) WITH KEY belnr = ls_docitem-accountingdocument
                                                       gjahr = ls_docitem-fiscalyear
                                                       bukrs = ls_docitem-companycode.
        IF sy-subrc EQ 0.
          MOVE ls_bkpf-xref1_hd TO gs_data-route.
        ENDIF.
        CLEAR: ls_bkpf.
    ENDCASE.

*    Nro Factura
    READ TABLE lt_bkpf_fact INTO ls_bkpf WITH KEY belnr = ls_docitem-accountingdocument
                                                  gjahr = ls_docitem-fiscalyear
                                                  bukrs = ls_docitem-companycode.
    IF sy-subrc EQ 0.
      MOVE ls_bkpf-xblnr TO gs_data-xblnr.
      "MOVE ls_bkpf-bldat TO gs_data-bldat.
    ENDIF.


*    Nombre Business Partner
    READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_docitem-customer.
    IF sy-subrc EQ 0.
      READ TABLE lt_adrc INTO ls_adrc WITH KEY addrnumber = ls_kna1-adrnr.
      IF sy-subrc EQ 0.
        gs_data-nombs = |{ ls_adrc-name1 } { ls_adrc-name2 } { ls_adrc-name3 } { ls_adrc-name4 }|.
      ENDIF.

    ENDIF.

    APPEND gs_data TO gt_data.
    CLEAR: gs_data, ls_vbrk, ls_vbfa, ls_vbap, ls_bkpf, ls_kna1, ls_adrc.

  ENDLOOP.

  DELETE gt_data WHERE route NOT IN s_route.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form show_alv
*&---------------------------------------------------------------------*
FORM show_alv.
  PERFORM alv_group.
  PERFORM alv_fieldcat.
  PERFORM alv_layout.
  PERFORM alv_display.
ENDFORM.

FORM alv_group.
  APPEND VALUE #( sp_group = 'A'
                  text     = TEXT-001 )
                  TO gt_slis_group.
ENDFORM.



FORM alv_fieldcat.
  REFRESH gt_slis_fieldcat.

*  APPEND VALUE #( outputlen = 5
*                  fieldname = 'FLAG'
*                  checkbox  = abap_true
*                  edit      = abap_true
*                  seltext_s = 'Sel.')
*                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 15
                  fieldname = 'BLDAT'
                  seltext_s = 'Fe.Doc'
                  seltext_l = 'Fecha Documento')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 10
                  fieldname = 'ROUTE'
                  seltext_l = 'Ruta')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 16
                  fieldname = 'XBLNR'
                  seltext_l = 'N° Factura')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 20
                  fieldname = 'SAPDO'
                  seltext_l = 'Documento SAP')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 20
                  fieldname = 'SGTXT'
                  seltext_l = 'Texto'
                  edit      = COND #( WHEN p_chkbx EQ abap_true THEN abap_false ELSE abap_true ) )
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 15
                  fieldname = 'KUNNR'
                  seltext_l = 'BP')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 30
                  fieldname = 'NOMBS'
                  seltext_l = 'Nombre BP')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 15
                  fieldname = 'DMBTR'
                  just      = 'R'
                  seltext_l = 'Importe Factura')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 23
                  just      = 'R'
                  fieldname = 'ZIMPC'
                  seltext_l = 'Importe Cobrado'
                  edit      = COND #( WHEN p_chkbx EQ abap_true THEN abap_false ELSE abap_true ) )
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 16
                  fieldname = 'ZFNCB'
                  seltext_l = 'Factura No Cobrada'
                  checkbox  = abap_true
                  edit      = COND #( WHEN p_chkbx EQ abap_true THEN abap_false ELSE abap_true ) )
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 16
                  just      = 'R'
                  fieldname = 'ZNCOB'
                  seltext_l = 'N° de Cobro'
                  edit      = COND #( WHEN p_chkbx EQ abap_true THEN abap_false ELSE abap_true ) )
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 20
                  fieldname = 'CONTA'
                  seltext_s = 'Doc.Contab'
                  seltext_l = 'Doc. Contabilizado')
                  TO gt_slis_fieldcat.
ENDFORM.

FORM alv_layout.
  CLEAR gs_slis_layout.
*  gs_slis_layout-colwidth_optimize   = 'X'.
  gs_slis_layout-zebra               = 'X'.
*  gs_slis_layout-box_fieldname       = 'CHECK'.
*  gs_slis_layout-get_selinfos        = 'X'.
*  gs_slis_layout-f2code              = 'BEAN' .
*  gs_slis_layout-confirmation_prompt = 'X'.
*  gs_slis_layout-key_hotspot         = 'X'.
*  gs_slis_layout-info_fieldname      = 'COL'.
ENDFORM.

FORM alv_display.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
*     i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = gs_slis_layout
      it_fieldcat              = gt_slis_fieldcat
      it_special_groups        = gt_slis_group
      i_save                   = 'X'
    TABLES
      t_outtab                 = gt_data.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM pf_status USING extab TYPE slis_t_extab.
  IF p_chkbx IS INITIAL.
    SET PF-STATUS 'STANDARD'.
  ELSE.
    SET PF-STATUS 'STANDARD_CHKBX'.
  ENDIF.
ENDFORM.


FORM user_command USING p_ucomm TYPE sy-ucomm
                        p_selfield TYPE slis_selfield.
*  BREAK consulabap05.

  IF ls_refalv IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ls_refalv.
  ENDIF.

  IF NOT ls_refalv IS INITIAL.
    CALL METHOD ls_refalv->check_changed_data.
  ENDIF.

  CASE p_ucomm.
    WHEN '&CONTA'.
      PERFORM map_bapi.
      p_selfield-refresh    = abap_true.
      p_selfield-col_stable = abap_true.
      p_selfield-row_stable = abap_true.
    WHEN '&PRINT'.
      READ TABLE gt_data TRANSPORTING NO FIELDS WITH KEY pass = abap_true.
      IF sy-subrc EQ 0.
        PERFORM print_form.
      ELSE.
        MESSAGE 'Debe seleccionar al menos una fila con Doc. Contabilizado' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

*    WHEN '&SEL_ALL'.
*      LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<gfs_data>).
*        <gfs_data>-flag = 'X'.
*        MODIFY gt_data FROM <gfs_data>.
*      ENDLOOP.
*      p_selfield-refresh    = abap_true.
*    WHEN '&DES_ALL'.
*      LOOP AT gt_data ASSIGNING <gfs_data>.
*        <gfs_data>-flag = ''.
*        MODIFY gt_data FROM <gfs_data>.
*      ENDLOOP.
*      p_selfield-refresh    = abap_true.
  ENDCASE.

  CLEAR p_ucomm.
ENDFORM.


*FORM top_of_page.
*
*  REFRESH gt_slis_header.
*
*  DATA: lv_date TYPE char10.
*
*  CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO lv_date SEPARATED BY '/'.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
*    EXPORTING
*      input  = 'ES'
*    IMPORTING
*      output = gv_spras.
*
*  SELECT SINGLE adrnr FROM t001 INTO @DATA(lv_adrnr)
*    WHERE bukrs IN @s_bukrs.
*  IF sy-subrc EQ 0.
*    SELECT SINGLE * FROM adrc INTO @DATA(ls_adrc)
*      WHERE addrnumber EQ @lv_adrnr.
*
*    DATA(lv_names) = |{ ls_adrc-name1 }{ ls_adrc-name2 }{ ls_adrc-name3 }{ ls_adrc-name4 }|.
*  ENDIF.
*
*  SELECT SINGLE bezei FROM tvrot INTO @DATA(lv_bezei)
*    WHERE route IN @s_route
*      AND spras EQ @gv_spras.
*
*
*  APPEND VALUE #( typ = 'H'
*                  info = lv_names )
*                  TO gt_slis_header.
*
*  APPEND VALUE #( typ = 'S'
*                  key = 'FECHA:'
*                  info = lv_date )
*                  TO gt_slis_header.
*
*  APPEND VALUE #( typ = 'S'
*                  key = 'RUTA:'
*                  info = lv_bezei )
*                  TO gt_slis_header.
*
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      it_list_commentary = gt_slis_header.
*
*
*  gs_header-nombre = lv_names.
*  gs_header-titulo = 'LIQUIDACIÓN DE VENTAS'.
*  CONCATENATE 'FECHA:' lv_date INTO gs_header-fecha SEPARATED BY space.
*  CONCATENATE 'RUTA:' s_route INTO gs_header-ruta SEPARATED BY space.
*
*ENDFORM.
