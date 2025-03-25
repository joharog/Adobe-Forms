*&---------------------------------------------------------------------*
*& DETERMINACION DE ISR MENSUAL
*&---------------------------------------------------------------------*
REPORT zfi_isr_trimestral.

TABLES: acdoca, t001, adrc, vbrk, fagl_011zc, sscrfields.

*&---------------------------------------------------------------------*
*& D E C L A R A T I O N  -  C O N S T A N T S / D A T A S / R A N G E S
*&---------------------------------------------------------------------*
CONSTANTS: i_isr TYPE char4 VALUE '@SF@',
           v_isr TYPE tabname VALUE 'ZFI_NODOS_ISR'.

DATA: smp_dyntxt TYPE smp_dyntxt.

DATA: ls_header TYPE zst_trimestral_header,
      ls_data   TYPE zst_trimestral_data,
      ls_decs   TYPE zst_trimestral_decs,
      ls_texto  TYPE zst_trimestral_texto.

DATA: lt_financial_node TYPE TABLE OF i_financialstatementhiernodet,
      ls_financial_node TYPE i_financialstatementhiernodet,
      lt_financial      TYPE TABLE OF i_financialstatementhiernode,
      ls_financial      TYPE i_financialstatementhiernode,
      lt_acdoct         TYPE TABLE OF acdoct,
      ls_acdoct         TYPE acdoct.

DATA: date_first  TYPE sy-datum,
      date_last   TYPE sy-datum,
      month_names TYPE TABLE OF t247,
      lv_balance  TYPE acdoca-hsl,
      lv_field    TYPE char5,
      lv_dec      TYPE p DECIMALS 2,
      lv_dec4     TYPE p DECIMALS 4.

RANGES: r_fkdat FOR vbrk-fkdat.


*&---------------------------------------------------------------------*
*&   P A R A M E T R O S   E N T R A D A
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE t01.
  PARAMETERS:
    p_ledger LIKE acdoca-rldnr OBLIGATORY DEFAULT '0L',
    p_bukrs  LIKE acdoca-rbukrs OBLIGATORY,
    p_trime  TYPE ztrimestral OBLIGATORY,
    p_ryear  LIKE acdoca-ryear OBLIGATORY,
    p_versn  LIKE fagl_011zc-versn OBLIGATORY DEFAULT 'Z009'.
SELECTION-SCREEN END OF BLOCK  b01.

SELECTION-SCREEN FUNCTION KEY 1.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      zcl_abap_tools_2=>show_updateview( tabname = v_isr ).
  ENDCASE.

INITIALIZATION.
  CLEAR smp_dyntxt.
  smp_dyntxt-text       = 'Nodos ISR'.
  smp_dyntxt-icon_id    = i_isr.
  smp_dyntxt-icon_text  = 'Nodos ISR'.
  smp_dyntxt-quickinfo  = 'Nodos ISR'.
  sscrfields-functxt_01 = smp_dyntxt.

START-OF-SELECTION.

  PERFORM get_data.
  PERFORM print_recibo.

END-OF-SELECTION.


FORM get_data.
*&---------------------------------------------------------------------*
*&   T H I N G S
*&---------------------------------------------------------------------*
  CASE p_trime.
    WHEN 1.
      CONCATENATE p_ryear '03' '01' INTO date_first.
    WHEN 2.
      CONCATENATE p_ryear '06' '01' INTO date_first.
    WHEN 3.
      CONCATENATE p_ryear '09' '01' INTO date_first.
    WHEN 4.
      CONCATENATE p_ryear '12' '01' INTO date_first.
  ENDCASE.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = date_first
    IMPORTING
      last_day_of_month = date_last.

  APPEND VALUE #( sign   = 'I'
                  option = 'BT'
                  low    = date_first
                  high   = date_last )
                  TO  r_fkdat.
  BREAK consulabap05.
*&---------------------------------------------------------------------*
*&  G E T   H E A D E R
*&---------------------------------------------------------------------*
  SELECT SINGLE * FROM t001 INTO @DATA(ls_t001)
   WHERE bukrs EQ @p_bukrs.

  SELECT SINGLE * FROM adrc INTO @DATA(ls_adrc)
   WHERE addrnumber = @ls_t001-adrnr.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language    = sy-langu
    TABLES
      month_names = month_names.

  READ TABLE month_names INTO DATA(ls_month) WITH KEY mnr = date_first+4(2).
  IF sy-subrc EQ 0.
    CONCATENATE 'Al' date_last+6(2) 'de' ls_month-ltx 'de' p_ryear INTO ls_header-periodo SEPARATED BY space.
  ENDIF.

  ls_header-sociedad = |{ ls_adrc-name1 } { ls_adrc-name2 } { ls_adrc-name3 } { ls_adrc-name4 }|.
  ls_header-titulo = 'Determinación de Renta Imponible e ISR'.
  ls_header-cifras = '(Cifras en Quetzales)'.


*&---------------------------------------------------------------------*
*&  G E T   D A T A
*&---------------------------------------------------------------------*
  SELECT * FROM i_financialstatementhiernodet INTO TABLE @lt_financial_node
  WHERE financialstatementhierarchy EQ @p_versn. "@versn.

  DELETE lt_financial_node WHERE hierarchynode EQ '00ASSETS'.
  DELETE lt_financial_node WHERE hierarchynode EQ '00LIABILITS'.
  DELETE lt_financial_node WHERE hierarchynode EQ '0Z009'.

  SELECT * FROM i_financialstatementhiernode INTO TABLE @lt_financial
    FOR ALL ENTRIES IN @lt_financial_node
    WHERE financialstatementhierarchy EQ @p_versn
      AND parentnode EQ @lt_financial_node-hierarchynode.


  DELETE lt_financial WHERE parentnode EQ '00NOTASSGND'.

  SELECT * FROM acdoct INTO TABLE lt_acdoct
    FOR ALL ENTRIES IN lt_financial
    WHERE rldnr  EQ p_ledger
      AND rbukrs EQ p_bukrs
      AND ryear  EQ p_ryear
      AND racct  EQ lt_financial-hierarchynodeval(10).

  SELECT * FROM zfi_nodos_isr INTO TABLE @DATA(lt_nodos_isr)
    WHERE bukrs EQ @p_bukrs
      AND gjahr EQ @p_ryear
      AND versn EQ @p_versn.
*      AND poper EQ @p_trime.

*  BREAK consulabap05.
  LOOP AT lt_financial INTO ls_financial.
    PERFORM hsl_nodes.
  ENDLOOP.


  LOOP AT lt_nodos_isr INTO DATA(ls_nodos_isr).
    CASE p_trime.
      WHEN 1.
        CASE ls_nodos_isr-poper.
          WHEN 001 OR 002 OR 003.
            IF ls_nodos_isr-hierarchynode EQ 032.
              ls_decs-nodo_tab = ls_decs-nodo_tab + ls_nodos_isr-monto.
            ELSEIF ls_nodos_isr-hierarchynode EQ 045. "Nodo 045 Obtener Total del Trimestre Anterior
              DATA(lv_year) = p_ryear - 1.
              SELECT SUM( monto )  INTO ls_decs-nodo_045 FROM zfi_nodos_isr
              WHERE bukrs EQ p_bukrs
                AND poper IN ( 010, 011, 012 )
                AND gjahr EQ lv_year
                AND versn EQ p_versn.
            ENDIF.
        ENDCASE.
      WHEN 2.
        CASE ls_nodos_isr-poper.
          WHEN 004 OR 005 OR 006.
            IF ls_nodos_isr-hierarchynode EQ 032.
              ls_decs-nodo_tab = ls_decs-nodo_tab + ls_nodos_isr-monto.
            ELSEIF ls_nodos_isr-hierarchynode EQ 045. "Nodo 045 Obtener Total del Trimestre Anterior
              SELECT SUM( monto )  INTO ls_decs-nodo_045 FROM zfi_nodos_isr
              WHERE bukrs EQ p_bukrs
                AND poper IN ( 001, 002, 003 )
                AND gjahr EQ p_ryear
                AND versn EQ p_versn.
            ENDIF.
        ENDCASE.
      WHEN 3.
        CASE ls_nodos_isr-poper.
          WHEN 007 OR 008 OR 009.
            IF ls_nodos_isr-hierarchynode EQ 032.
              ls_decs-nodo_tab = ls_decs-nodo_tab + ls_nodos_isr-monto.
            ELSEIF ls_nodos_isr-hierarchynode EQ 045. "Nodo 045 Obtener Total del Trimestre Anterior
              SELECT SUM( monto )  INTO ls_decs-nodo_045 FROM zfi_nodos_isr
              WHERE bukrs EQ p_bukrs
                AND poper IN ( 004, 005, 006 )
                AND gjahr EQ p_ryear
                AND versn EQ p_versn.
            ENDIF.
        ENDCASE.
      WHEN 4.
        CASE ls_nodos_isr-poper.
          WHEN 010 OR 011 OR 012.
            IF ls_nodos_isr-hierarchynode EQ 032.
              ls_decs-nodo_tab = ls_decs-nodo_tab + ls_nodos_isr-monto.
            ELSEIF ls_nodos_isr-hierarchynode EQ 045. "Nodo 045 Obtener Total del Trimestre Anterior
              SELECT SUM( monto )  INTO ls_decs-nodo_045 FROM zfi_nodos_isr
              WHERE bukrs EQ p_bukrs
                AND poper IN ( 007, 008, 009 )
                AND gjahr EQ p_ryear
                AND versn EQ p_versn.
            ENDIF.
        ENDCASE.
    ENDCASE.
  ENDLOOP.


  LOOP AT lt_financial_node  INTO DATA(ls_financial_node).
    CASE ls_financial_node-hierarchynode.
      WHEN 08.
        ls_texto-nodo_08 = ls_financial_node-hierarchynodetext.
      WHEN 09.
        ls_texto-nodo_09 = ls_financial_node-hierarchynodetext.
      WHEN 010.
        ls_texto-nodo_010 = ls_financial_node-hierarchynodetext.
      WHEN 011.
        ls_texto-nodo_011 = ls_financial_node-hierarchynodetext.
      WHEN 012.
        ls_texto-nodo_012 = ls_financial_node-hierarchynodetext.
      WHEN 013.
        ls_texto-nodo_013 = ls_financial_node-hierarchynodetext.
      WHEN 014.
        ls_texto-nodo_014 = ls_financial_node-hierarchynodetext.
      WHEN 015.
        ls_texto-nodo_015 = ls_financial_node-hierarchynodetext.
      WHEN 016.
        ls_texto-nodo_016 = ls_financial_node-hierarchynodetext.
      WHEN 017.
        ls_texto-nodo_017 = ls_financial_node-hierarchynodetext.
      WHEN 018.
        ls_texto-nodo_018 = ls_financial_node-hierarchynodetext.
      WHEN 019.
        ls_texto-nodo_019 = ls_financial_node-hierarchynodetext.
      WHEN 020.
        ls_texto-nodo_020 = ls_financial_node-hierarchynodetext.
      WHEN 021.
        ls_texto-nodo_021 = ls_financial_node-hierarchynodetext.
      WHEN 022.
        ls_texto-nodo_022 = ls_financial_node-hierarchynodetext.
      WHEN 023.
        ls_texto-nodo_023 = ls_financial_node-hierarchynodetext.
      WHEN 024.
        ls_texto-nodo_024 = ls_financial_node-hierarchynodetext.
      WHEN 026.
        ls_texto-nodo_026 = ls_financial_node-hierarchynodetext.
      WHEN 027.
        ls_texto-nodo_027 = ls_financial_node-hierarchynodetext.
      WHEN 028.
        ls_texto-nodo_028 = ls_financial_node-hierarchynodetext.
      WHEN 029.
        ls_texto-nodo_029 = ls_financial_node-hierarchynodetext.
      WHEN 031.
        ls_texto-nodo_031 = ls_financial_node-hierarchynodetext.
      WHEN '00ASSETS'.
        ls_texto-nodo_00assets = ls_financial_node-hierarchynodetext.     "Gran Total Ingresos
      WHEN '00LIABILITS'.
        ls_texto-nodo_00liabilits = ls_financial_node-hierarchynodetext.  "Base Imponible * 25%
      WHEN 025.
        ls_texto-nodo_025 = ls_financial_node-hierarchynodetext.          "Rentas Exentas y No Afectas
      WHEN 030.
        ls_texto-nodo_030 = ls_financial_node-hierarchynodetext.          "Costos y Gastos Acumulados
      WHEN 034.
        ls_texto-nodo_034 = ls_financial_node-hierarchynodetext.          "Gastos No Deducibles
      WHEN 043.
        ls_texto-nodo_043 = ls_financial_node-hierarchynodetext.          "Base Imponible
      WHEN 044.
        ls_texto-nodo_044 = ls_financial_node-hierarchynodetext.          "ISR S/ Ingresos totales   1.25%
      WHEN 045.
        ls_texto-nodo_045 = ls_financial_node-hierarchynodetext.          "ISR acumulada del trimestre inmediato anterio
      WHEN 046.
        ls_texto-nodo_046 = ls_financial_node-hierarchynodetext.          "ISR a pagar neto    644,373
      WHEN 047.
        ls_texto-nodo_047 = ls_financial_node-hierarchynodetext.          "ISR S/ Ingresos totales   1.10%
    ENDCASE.

  ENDLOOP.

*  Completar textos
  ls_texto-nodo_00assets = 'GRAN TOTAL INGRESOS'.    "Gran Total Ingresos
  ls_texto-nodo_00liabilits = 'Base Imponible * 25%'.
  ls_texto-nodo_tab = 'Excedente de costos y gastos del 97% de los ingresos del período 2012'.
  ls_texto-nodo_031 = 'Perdida En Diferencial Cambiario (Valuación De Moneda)'.
  ls_texto-nodo_045 = 'ISR acumulada del trimestre inmediato anterior'.
  ls_texto-nodo_p25 = 'Base Imponible * 25%'.
  TRANSLATE ls_texto-nodo_030 TO UPPER CASE.
  TRANSLATE ls_texto-nodo_043 TO UPPER CASE.




  ls_decs-nodo_00assets = ls_decs-nodo_08 + ls_decs-nodo_09 + ls_decs-nodo_010 + ls_decs-nodo_011 + ls_decs-nodo_014.
  ls_decs-nodo_00assets = ls_decs-nodo_00assets - ( ls_decs-nodo_012 + ls_decs-nodo_013 ).

  ls_decs-nodo_025 = ls_decs-nodo_015 + ls_decs-nodo_016 + ls_decs-nodo_018.
  ls_decs-nodo_025 = ls_decs-nodo_025 - ls_decs-nodo_017.

  ls_decs-nodo_030 = ls_decs-nodo_019 + ls_decs-nodo_020 + ls_decs-nodo_021 + ls_decs-nodo_tab.

  ls_decs-nodo_034 = ls_decs-nodo_022 + ls_decs-nodo_023 + ls_decs-nodo_024 + ls_decs-nodo_026
                     + ls_decs-nodo_027 + ls_decs-nodo_028 + ls_decs-nodo_029 + ls_decs-nodo_031.

*  ls_decs-nodo_043 = ( ls_decs-nodo_00assets * -1 ) - ls_decs-nodo_025 - ls_decs-nodo_030 + ls_decs-nodo_034.
   ls_decs-nodo_043 = ( ls_decs-nodo_00assets * -1 ) -  ls_decs-nodo_030.


  CLEAR: lv_dec.
  ls_decs-nodo_p25 = lv_dec = ls_decs-nodo_043 * ( 25 / 100 ).

  CLEAR: lv_dec4.
  lv_dec4 = ls_decs-nodo_p25 /  ls_decs-nodo_00assets.
  ls_decs-nodo_044 = lv_dec4 * 100.

  ls_decs-nodo_046 = ls_decs-nodo_p25 - ls_decs-nodo_045.

*  CLEAR: lv_dec.
*  lv_dec = ls_decs-nodo_046 / ls_decs-nodo_00assets.
*  ls_decs-nodo_044 = lv_dec * 100.

  CLEAR: lv_dec.
  ls_decs-nodo_blank = lv_dec = ls_decs-nodo_030 / ls_decs-nodo_00assets.

  CLEAR: lv_dec4.
  lv_dec4 = ls_decs-nodo_046 / ( ls_decs-nodo_00assets * -1 ).
  ls_decs-nodo_047 = lv_dec4 * 100.

  PERFORM write_decimals.

  PERFORM condense_all.

ENDFORM.

FORM print_recibo.

  DATA lv_name  TYPE char30.
  DATA lv_function_name   TYPE rs38l_fnam.
  DATA lv_interface_type  TYPE fpinterfacetype.
  DATA lv_subrc           TYPE sy-subrc.
  DATA ls_outputparams    TYPE sfpoutputparams.
  DATA ls_docparams       TYPE sfpdocparams.
  DATA ls_formoutput      TYPE fpformoutput.
  DATA ls_result        TYPE sfpjoboutput.

  lv_name = 'ZAF_FI_ISR_TRIMESTRAL'.

  TRY.
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name           = lv_name
        IMPORTING
          e_funcname       = lv_function_name
          e_interface_type = lv_interface_type.

    CATCH cx_fp_api_internal
          cx_fp_api_repository
          cx_fp_api_usage.
      MOVE sy-subrc TO lv_subrc.
  ENDTRY.

  ls_outputparams = VALUE #(
             dest = 'PDF1'
         nodialog = abap_false
          preview = abap_false ).

* Set parameters (print/send/output device etc)
  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = ls_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    MOVE sy-subrc TO lv_subrc.
  ENDIF.

* Actually print Adobe form
  ls_docparams-langu     = sy-langu.
*  ls_docparams-fillable  = p_edita.
*  ls_docparams-langu     = 'S'.
*  ls_docparams-replangu1 = 'S'.
*  ls_docparams-replangu2 = 'E'.
*  ls_docparams-country   = 'GT'.

  CALL FUNCTION lv_function_name
    EXPORTING
      /1bcdwb/docparams  = ls_docparams
      header             = ls_header
      pos_data           = ls_data
      txt_data           = ls_texto
    IMPORTING
      /1bcdwb/formoutput = ls_formoutput
    EXCEPTIONS
      usage_error        = 1
      system_error       = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    MOVE sy-subrc TO lv_subrc.
  ENDIF.

* Finish off printing
  CALL FUNCTION 'FP_JOB_CLOSE'
    IMPORTING
      e_result       = ls_result            " What has been done? 2030195
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3.
  IF sy-subrc <> 0.
    MOVE sy-subrc TO lv_subrc.
  ENDIF.



ENDFORM.

FORM hsl_nodes.

  CASE ls_financial-parentnode.
    WHEN 08.
      LOOP AT lt_acdoct INTO DATA(ls_acdoct) WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_08 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_08 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                               + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_08 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                               + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                               + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_08 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                               + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                               + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                               + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 09.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_09 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_09 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                               + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_09 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                               + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                               + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_09 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                               + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                               + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                               + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 010.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_010 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_010 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_010 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_010 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.

      ENDLOOP.
    WHEN 011.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_011 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_011 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_011 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_011 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 012.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_012 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_012 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_012 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_012 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 013.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_013 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_013 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_013 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_013 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 014.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_014 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_014 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_014 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_014 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 015.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_015 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_015 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_015 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_015 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 016.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_016 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_016 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_016 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_016 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 017.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_017 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_017 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_017 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_017 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 018.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_018 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_018 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_018 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_018 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 019.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_019 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_019 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_019 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_019 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 020.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_020 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_020 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_020 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_020 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 021.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_021 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_021 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_021 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_021 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 022.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_022 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_022 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_022 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_022 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 023.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_023 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_023 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_023 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_023 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 024.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_024 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_024 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_024 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_024 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 026.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_026 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_026 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_026 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_026 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 027.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_027 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_027 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_027 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_027 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 028.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_028 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_028 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_028 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_028 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 029.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_029 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_029 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_029 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_029 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.
    WHEN 031.
      LOOP AT lt_acdoct INTO ls_acdoct WHERE racct EQ ls_financial-hierarchynodeval.
        CASE p_trime.
          WHEN 1.
            ls_decs-nodo_031 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03.
          WHEN 2.
            ls_decs-nodo_031 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06.
          WHEN 3.
            ls_decs-nodo_031 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09.
          WHEN 4.
            ls_decs-nodo_031 += ls_acdoct-hslvt + ls_acdoct-hsl01 + ls_acdoct-hsl02 + ls_acdoct-hsl03
                                                + ls_acdoct-hsl04 + ls_acdoct-hsl05 + ls_acdoct-hsl06
                                                + ls_acdoct-hsl07 + ls_acdoct-hsl08 + ls_acdoct-hsl09
                                                + ls_acdoct-hsl10 + ls_acdoct-hsl11 + ls_acdoct-hsl12.
        ENDCASE.
      ENDLOOP.

  ENDCASE.

ENDFORM.


FORM condense_all.

  CONDENSE:
  ls_data-nodo_08,
  ls_data-nodo_09,
  ls_data-nodo_010,
  ls_data-nodo_011,
  ls_data-nodo_012,
  ls_data-nodo_013,
  ls_data-nodo_014,
  ls_data-nodo_015,
  ls_data-nodo_016,
  ls_data-nodo_017,
  ls_data-nodo_018,
  ls_data-nodo_019,
  ls_data-nodo_020,
  ls_data-nodo_021,
  ls_data-nodo_022,
  ls_data-nodo_023,
  ls_data-nodo_024,
  ls_data-nodo_026,
  ls_data-nodo_027,
  ls_data-nodo_028,
  ls_data-nodo_029,
  ls_data-nodo_031,
  ls_data-nodo_025,
  ls_data-nodo_030,
  ls_data-nodo_034,
  ls_data-nodo_043,
  ls_data-nodo_044,
  ls_data-nodo_045,
  ls_data-nodo_046,
  ls_data-nodo_047,
  ls_data-nodo_tab,
  ls_data-nodo_p25,
  ls_data-nodo_blank,
  ls_data-nodo_00assets,
  ls_data-nodo_00liabilits.

ENDFORM.

FORM write_decimals.


  IF ls_decs-nodo_08 < 0.
    WRITE ls_decs-nodo_08 TO ls_data-nodo_08 NO-SIGN.
    CONDENSE ls_data-nodo_08.
*    ls_data-nodo_08 = |({ ls_data-nodo_08 })|.
  ELSE.
    WRITE ls_decs-nodo_08 TO ls_data-nodo_08 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_09 < 0.
    WRITE ls_decs-nodo_09 TO ls_data-nodo_09 NO-SIGN.
    CONDENSE ls_data-nodo_09.
*    ls_data-nodo_09 = |({ ls_data-nodo_09 })|.
  ELSE.
    WRITE ls_decs-nodo_09 TO ls_data-nodo_09 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_010 < 0.
    WRITE ls_decs-nodo_010 TO ls_data-nodo_010 NO-SIGN.
    CONDENSE ls_data-nodo_010.
*    ls_data-nodo_010 = |({ ls_data-nodo_010 })|.
  ELSE.
    WRITE ls_decs-nodo_010 TO ls_data-nodo_010 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_011 < 0.
    WRITE ls_decs-nodo_011 TO ls_data-nodo_011 NO-SIGN.
    CONDENSE ls_data-nodo_011.
*    ls_data-nodo_011 = |({ ls_data-nodo_011 })|.
  ELSE.
    WRITE ls_decs-nodo_011 TO ls_data-nodo_011 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_012 < 0.
    WRITE ls_decs-nodo_012 TO ls_data-nodo_012 NO-SIGN.
    CONDENSE ls_data-nodo_012.
*    ls_data-nodo_012 = |({ ls_data-nodo_012 })|.
  ELSE.
    WRITE ls_decs-nodo_012 TO ls_data-nodo_012 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_013 < 0.
    WRITE ls_decs-nodo_013 TO ls_data-nodo_013 NO-SIGN.
    CONDENSE ls_data-nodo_013.
*    ls_data-nodo_013 = |({ ls_data-nodo_013 })|.
  ELSE.
    WRITE ls_decs-nodo_013 TO ls_data-nodo_013 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_014 < 0.
    WRITE ls_decs-nodo_014 TO ls_data-nodo_014 NO-SIGN.
    CONDENSE ls_data-nodo_014.
*    ls_data-nodo_014 = |({ ls_data-nodo_014 })|.
  ELSE.
    WRITE ls_decs-nodo_014 TO ls_data-nodo_014 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_015 < 0.
    WRITE ls_decs-nodo_015 TO ls_data-nodo_015 NO-SIGN.
    CONDENSE ls_data-nodo_015.
*    ls_data-nodo_015 = |({ ls_data-nodo_015 })|.
  ELSE.
    WRITE ls_decs-nodo_015 TO ls_data-nodo_015 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_016 < 0.
    WRITE ls_decs-nodo_016 TO ls_data-nodo_016 NO-SIGN.
    CONDENSE ls_data-nodo_016.
*    ls_data-nodo_016 = |({ ls_data-nodo_016 })|.
  ELSE.
    WRITE ls_decs-nodo_016 TO ls_data-nodo_016 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_017 < 0.
    WRITE ls_decs-nodo_017 TO ls_data-nodo_017 NO-SIGN.
    CONDENSE ls_data-nodo_017.
*    ls_data-nodo_017 = |({ ls_data-nodo_017 })|.
  ELSE.
    WRITE ls_decs-nodo_017 TO ls_data-nodo_017 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_018 < 0.
    WRITE ls_decs-nodo_018 TO ls_data-nodo_018 NO-SIGN.
    CONDENSE ls_data-nodo_018.
*    ls_data-nodo_018 = |({ ls_data-nodo_018 })|.
  ELSE.
    WRITE ls_decs-nodo_018 TO ls_data-nodo_018 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_019 < 0.
    WRITE ls_decs-nodo_019 TO ls_data-nodo_019 NO-SIGN.
    CONDENSE ls_data-nodo_019.
*    ls_data-nodo_019 = |({ ls_data-nodo_019 })|.
  ELSE.
    WRITE ls_decs-nodo_019 TO ls_data-nodo_019 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_020 < 0.
    WRITE ls_decs-nodo_020 TO ls_data-nodo_020. " NO-SIGN.  "Dejar signo incidencias 26.12.2024
    CONDENSE ls_data-nodo_020.
    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
      CHANGING
        value = ls_data-nodo_020.


*    ls_data-nodo_020 = |({ ls_data-nodo_020 })|.
  ELSE.
    WRITE ls_decs-nodo_020 TO ls_data-nodo_020. " NO-SIGN.  "Dejar signo incidencias 26.12.2024
  ENDIF.

  IF ls_decs-nodo_021 < 0.
    WRITE ls_decs-nodo_021 TO ls_data-nodo_021 NO-SIGN.
    CONDENSE ls_data-nodo_021.
*    ls_data-nodo_021 = |({ ls_data-nodo_021 })|.
  ELSE.
    WRITE ls_decs-nodo_021 TO ls_data-nodo_021 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_022 < 0.
    WRITE ls_decs-nodo_022 TO ls_data-nodo_022 NO-SIGN.
    CONDENSE ls_data-nodo_022.
*    ls_data-nodo_022 = |({ ls_data-nodo_022 })|.
  ELSE.
    WRITE ls_decs-nodo_022 TO ls_data-nodo_022 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_023 < 0.
    WRITE ls_decs-nodo_023 TO ls_data-nodo_023 NO-SIGN.
    CONDENSE ls_data-nodo_023.
*    ls_data-nodo_023 = |({ ls_data-nodo_023 })|.
  ELSE.
    WRITE ls_decs-nodo_023 TO ls_data-nodo_023 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_024 < 0.
    WRITE ls_decs-nodo_024 TO ls_data-nodo_024 NO-SIGN.
    CONDENSE ls_data-nodo_024.
*    ls_data-nodo_024 = |({ ls_data-nodo_024 })|.
  ELSE.
    WRITE ls_decs-nodo_024 TO ls_data-nodo_024 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_026 < 0.
    WRITE ls_decs-nodo_026 TO ls_data-nodo_026 NO-SIGN.
    CONDENSE ls_data-nodo_026.
*    ls_data-nodo_026 = |({ ls_data-nodo_026 })|.
  ELSE.
    WRITE ls_decs-nodo_026 TO ls_data-nodo_026 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_027 < 0.
    WRITE ls_decs-nodo_027 TO ls_data-nodo_027 NO-SIGN.
    CONDENSE ls_data-nodo_027.
*    ls_data-nodo_027 = |({ ls_data-nodo_027 })|.
  ELSE.
    WRITE ls_decs-nodo_027 TO ls_data-nodo_027 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_028 < 0.
    WRITE ls_decs-nodo_028 TO ls_data-nodo_028 NO-SIGN.
    CONDENSE ls_data-nodo_028.
*    ls_data-nodo_028 = |({ ls_data-nodo_028 })|.
  ELSE.
    WRITE ls_decs-nodo_028 TO ls_data-nodo_028 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_029 < 0.
    WRITE ls_decs-nodo_029 TO ls_data-nodo_029 NO-SIGN.
    CONDENSE ls_data-nodo_029.
*    ls_data-nodo_029 = |({ ls_data-nodo_029 })|.
  ELSE.
    WRITE ls_decs-nodo_029 TO ls_data-nodo_029 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_031 < 0.
    WRITE ls_decs-nodo_031 TO ls_data-nodo_031 NO-SIGN.
    CONDENSE ls_data-nodo_031.
*    ls_data-nodo_031 = |({ ls_data-nodo_031 })|.
  ELSE.
    WRITE ls_decs-nodo_031 TO ls_data-nodo_031 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_00assets < 0.
    WRITE ls_decs-nodo_00assets TO ls_data-nodo_00assets NO-SIGN.
    CONDENSE ls_data-nodo_00assets.
*    ls_data-nodo_00assets = |({ ls_data-nodo_00assets })|.
  ELSE.
    WRITE ls_decs-nodo_00assets TO ls_data-nodo_00assets NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_00liabilits < 0.
    WRITE ls_decs-nodo_00liabilits TO ls_data-nodo_00liabilits NO-SIGN.
    CONDENSE ls_data-nodo_00liabilits.
*    ls_data-nodo_00liabilits = |({ ls_data-nodo_00liabilits })|.
  ELSE.
    WRITE ls_decs-nodo_00liabilits TO ls_data-nodo_00liabilits NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_025 < 0.
    WRITE ls_decs-nodo_025 TO ls_data-nodo_025 NO-SIGN.
    CONDENSE ls_data-nodo_025.
*    ls_data-nodo_025 = |({ ls_data-nodo_025 })|.
  ELSE.
    WRITE ls_decs-nodo_025 TO ls_data-nodo_025 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_030 < 0.
    WRITE ls_decs-nodo_030 TO ls_data-nodo_030 NO-SIGN.
    CONDENSE ls_data-nodo_030.
*    ls_data-nodo_030 = |({ ls_data-nodo_030 })|.
  ELSE.
    WRITE ls_decs-nodo_030 TO ls_data-nodo_030 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_034 < 0.
    WRITE ls_decs-nodo_034 TO ls_data-nodo_034 NO-SIGN.
    CONDENSE ls_data-nodo_034.
*    ls_data-nodo_034 = |({ ls_data-nodo_034 })|.
  ELSE.
    WRITE ls_decs-nodo_034 TO ls_data-nodo_034 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_043 < 0.
    WRITE ls_decs-nodo_043 TO ls_data-nodo_043 NO-SIGN.
    CONDENSE ls_data-nodo_043.
*    ls_data-nodo_043 = |({ ls_data-nodo_043 })|.
  ELSE.
    WRITE ls_decs-nodo_043 TO ls_data-nodo_043 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_045 < 0.
    WRITE ls_decs-nodo_045 TO ls_data-nodo_045 NO-SIGN.
    CONDENSE ls_data-nodo_045.
*    ls_data-nodo_045 = |({ ls_data-nodo_045 })|.
  ELSE.
    WRITE ls_decs-nodo_045 TO ls_data-nodo_045 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_046 < 0.
    WRITE ls_decs-nodo_046 TO ls_data-nodo_046 NO-SIGN.
    CONDENSE ls_data-nodo_046.
*    ls_data-nodo_046 = |({ ls_data-nodo_046 })|.
  ELSE.
    WRITE ls_decs-nodo_046 TO ls_data-nodo_046 NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_tab < 0.
    WRITE ls_decs-nodo_tab TO ls_data-nodo_tab NO-SIGN.
    CONDENSE ls_data-nodo_tab.
*    ls_data-nodo_tab = |({ ls_data-nodo_tab })|.
  ELSE.
    WRITE ls_decs-nodo_tab TO ls_data-nodo_tab NO-SIGN.
  ENDIF.

  IF ls_decs-nodo_p25 < 0.
    WRITE ls_decs-nodo_p25 TO ls_data-nodo_p25 NO-SIGN.
    CONDENSE ls_data-nodo_p25.
*    ls_data-nodo_p25 = |({ ls_data-nodo_p25 })|.
  ELSE.
    WRITE ls_decs-nodo_p25 TO ls_data-nodo_p25 NO-SIGN.
  ENDIF.


***  Nodos en %
  IF ls_decs-nodo_blank < 0.
    WRITE ls_decs-nodo_blank TO ls_data-nodo_blank NO-SIGN.
    CONDENSE ls_data-nodo_blank.
    ls_data-nodo_blank = |{ ls_data-nodo_blank }%|.
  ELSE.
    WRITE ls_decs-nodo_blank TO ls_data-nodo_blank NO-SIGN.
    CONDENSE ls_data-nodo_blank.
    ls_data-nodo_blank = |{ ls_data-nodo_blank }%|.
  ENDIF.

  IF ls_decs-nodo_044 < 0.
    WRITE ls_decs-nodo_044 TO ls_data-nodo_044 NO-SIGN.
    CONDENSE ls_data-nodo_044.
    ls_data-nodo_044 = |{ ls_data-nodo_044 }%|.
  ELSE.
    WRITE ls_decs-nodo_044 TO ls_data-nodo_044 NO-SIGN.
    CONDENSE ls_data-nodo_044.
    ls_data-nodo_044 = |{ ls_data-nodo_044 }%|.
  ENDIF.

  IF ls_decs-nodo_047 < 0.
    WRITE ls_decs-nodo_047 TO ls_data-nodo_047 NO-SIGN.
    CONDENSE ls_data-nodo_047.
    ls_data-nodo_047 = |{ ls_data-nodo_047 }%|.
  ELSE.
    WRITE ls_decs-nodo_047 TO ls_data-nodo_047 NO-SIGN.
    CONDENSE ls_data-nodo_047.
    ls_data-nodo_047 = |{ ls_data-nodo_047 }%|.
  ENDIF.

ENDFORM.
