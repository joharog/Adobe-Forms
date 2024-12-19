*&---------------------------------------------------------------------*
*& Include          ZFI_LIQUIDACION_VENTAS_TOP
*&---------------------------------------------------------------------*

TABLES: vbrk, vbfa, vbap, bkpf, kna1, adrc, t001, tvrot, tvro, bseg, skb1.


*&---------------------------------------------------------------------*
*&           T Y P E  -  P O O L S
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.
TYPE-POOLS: icon.

*&---------------------------------------------------------------------*
*&           T Y P E  -  D E F I N I T I O N
*&---------------------------------------------------------------------*
*TYPES:
*  BEGIN OF ty_data,
*    flag  TYPE char1,           "Check Box
*    conta   TYPE char255,       "Documento Contabilizado / Log (BAPI)
*    budat TYPE fis_bldat,       "Fecha de Documento CDS
*    route   TYPE bkpf-xref1_hd, "Ruta
*    xblnr   TYPE bkpf-xblnr,    "Numero Factura
*    sapdo   TYPE string,        "Documento SAP
*    sgtxt   TYPE bseg-sgtxt,    "Texto
*    kunnr   TYPE bseg-kunnr,    "Business Partner
*    nombs   TYPE char200,       "Nombre BP
*    dmbtr   TYPE char20,        "Importe de Factura
*    zimpc   TYPE char20,        "Importe Cobrado
*    zfncb   TYPE char1,         "Factura No Cobrada
*    zncob   TYPE cbo_numeric15, "Numero de Cobro

*    BAPI Fields
*    bldat   TYPE bkpf-bldat,
*    bukrs   TYPE bkpf-bukrs,
*    assig   TYPE fis_zuonr,
*    payre   TYPE fis_awkey,
*    waers   TYPE fis_rwcur,
*  END OF ty_data.


*&---------------------------------------------------------------------*
*&           G L O B A L  -  V A R I A B L E S
*&---------------------------------------------------------------------*
DATA:
  gv_date  TYPE char10,
  gv_spras TYPE tvrot-spras.


*&---------------------------------------------------------------------*
*&           G L O B A L  -  S T R U C T U R E S / T A B L E S
*&---------------------------------------------------------------------*
DATA:
  gs_header     TYPE zst_liquidacion_header,
  gs_data       TYPE zst_liquidacion_data,
  gt_data       TYPE TABLE OF zst_liquidacion_data,
  gt_liq_ventas TYPE TABLE OF zfi_liq_ventas.


*&---------------------------------------------------------------------*
*&           F I E L D  S Y M B O L S
*&---------------------------------------------------------------------*
FIELD-SYMBOLS:
               <gfs_data> TYPE zst_liquidacion_data.


*&---------------------------------------------------------------------*
*&           A L V  -  D E F I N I T I O N
*&---------------------------------------------------------------------*
DATA:
  gt_slis_group    TYPE slis_t_sp_group_alv,
  gt_slis_fieldcat TYPE slis_t_fieldcat_alv,
  gs_slis_layout   TYPE slis_layout_alv,
  gt_slis_header   TYPE slis_t_listheader,

  ls_refalv        TYPE REF TO cl_gui_alv_grid.


*&---------------------------------------------------------------------*
*&           S T A R T  -  O F  -  S E L E C T I O N
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  PARAMETERS:     p_bukrs LIKE t001-bukrs OBLIGATORY.
  SELECT-OPTIONS: "s_bukrs FOR t001-bukrs, "OBLIGATORY,
                  s_bldat FOR bkpf-bldat, "OBLIGATORY,
                  s_route FOR tvro-route, "OBLIGATORY,
                  s_saknr FOR skb1-saknr. "OBLIGATORY,
  PARAMETERS:     p_budat LIKE bkpf-budat, "OBLIGATORY.
                  p_sgtxt LIKE bseg-sgtxt, "OBLIGATORY.
                  p_chkbx AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN END OF BLOCK b01.
