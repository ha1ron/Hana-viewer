*&---------------------------------------------------------------------*
*& Report Z92_HANA_VIEWER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z92_HANA_VIEWER.

data: container    type ref to cl_gui_docking_container,
      grid         type ref to cl_gui_alv_grid,
      fieldcatalog type lvc_t_fcat.

data: timepassed_seconds type i,
      catalog_name       type string,
      cube_name          type string.

*--------------------------------------------------------------------*

data: editor_container type ref to cl_gui_custom_container,
      text_editor      type ref to cl_gui_textedit.

*--------------------------------------------------------------------*

field-symbols: <tab> type any table.

data: sql          type string,
      rows         type i value 1000,
      in_screen(1).

start-of-selection.
  call screen 200.

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_QUERY
*&---------------------------------------------------------------------*
FORM EXECUTE_QUERY.

  data: input_parameters type string,
        data_tab         type ref to data,
        schema           type string.

  free data_tab.
  if <tab> is assigned.
    free: <tab>.
  endif.

*удаляем лишине символы
  call function 'SCP_REPLACE_STRANGE_CHARS'
    exporting
      intext      = sql
      replacement = 32
    importing
      outtext     = sql.

  data comm type string.
  get run time field data(ts_start).

  data_tab = zcl_sql_executor=>result_hana_model( exporting i_query = sql
                                                            i_top = conv string( rows )
                                                  importing e_catalog = fieldcatalog
                                                            e_patch = catalog_name
                                                            e_model_name = cube_name
                                                            e_comm = comm ).

  get run time field data(ts_end).
  timepassed_seconds = ts_end - ts_start.

  assign data_tab->* to <tab>.

  if lines( <tab> ) < 2000.
    perform creaty_numbering changing fieldcatalog <tab>.
  endif.

  if comm is initial.
    if in_screen = abap_true.
      call screen 200.
    else.
      call screen 100.
    endif.
  else.
    message comm type 'I'.
  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*&      МОДУЛИ
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
module USER_COMMAND_0100 input.
  case sy-ucomm.
    when 'OUT'.
      leave program.
    when 'BACK'.
      set screen 0.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  GRID_INIT  OUTPUT
*&---------------------------------------------------------------------*
module GRID_INIT output.

  if container is not initial.
    grid->free( ).
    container->free( ).
  endif.

  free: container, grid.

  container = new #( side      = cl_gui_docking_container=>dock_at_top
                     extension = cl_gui_docking_container=>ws_maximizebox ).

  grid = new #( container )." cl_gui_container=>screen0 ).

  data: layout type lvc_s_layo.
  layout-cwidth_opt = abap_true.
  layout-grid_title = |Время исполнения { timepassed_seconds } ms|.

  grid->set_table_for_first_display( exporting is_layout            = layout
                                      changing it_outtab            = <tab>
                                               it_fieldcatalog      = fieldcatalog ).
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module STATUS_0100 output.
  set pf-status 'STATUS100'.
  set titlebar 'BAR100' with catalog_name cube_name.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'STATUS200'.
  SET TITLEBAR 'BAR200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  case sy-ucomm.
    when 'OUT'.
      leave program.
    when 'BACK'.
      leave to screen 0.
    when 'EXECUTE'.
      clear sql.
      text_editor->get_textstream( importing text = sql ).
      cl_gui_cfw=>flush( ).

      perform execute_query.
  endcase.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TEXT_INIT  OUTPUT
*&---------------------------------------------------------------------*
MODULE TEXT_INIT OUTPUT.

  if text_editor is initial.

    editor_container = new #( container_name = 'TEXTEDITOR' ).
    text_editor = new #( parent                     = editor_container
                         wordwrap_mode              = cl_gui_textedit=>wordwrap_off
                         wordwrap_position          = 254
                         wordwrap_to_linebreak_mode = cl_gui_textedit=>false ).
    text_editor->set_toolbar_mode( toolbar_mode = cl_gui_textedit=>false ).
    text_editor->set_statusbar_mode( statusbar_mode = cl_gui_textedit=>false ).

  endif.

* экран справа
  if in_screen = abap_true.

    if container is not initial.
      grid->free( ).
      container->free( ).
    endif.

    free: container, grid.

    container = new #( side  = cl_gui_docking_container=>dock_at_right
                       ratio =  50 ).

    grid = new #( container )." cl_gui_container=>screen0 ).

    layout-cwidth_opt = abap_true.
    layout-grid_title = |Время исполнения { timepassed_seconds } ms|.

    if <tab> is assigned.
      grid->set_table_for_first_display( exporting is_layout            = layout
                                          changing it_outtab            = <tab>
                                                   it_fieldcatalog      = fieldcatalog ).
    endif.


  endif.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CREATY_NUMBERING
*&---------------------------------------------------------------------*
FORM CREATY_NUMBERING changing f_catalog type lvc_t_fcat
                               p_tab type standard table.

  loop at f_catalog assigning field-symbol(<catalog>).
    <catalog>-col_pos = <catalog>-col_pos + 1.
  endloop.

  data: fcat_property type lvc_s_fcat.
  fcat_property-col_pos = 1.
  fcat_property-fieldname = 'ROWS_NUMBER'.
  fcat_property-outputlen = 3.
  fcat_property-datatype = 'INT4'.
  fcat_property-intlen = 3.
  fcat_property-just = abap_true.
  fcat_property-lowercase = abap_true.
  append fcat_property to f_catalog.

  data: num_data type ref to data.

  call method cl_alv_table_create=>create_dynamic_table
    exporting
      it_fieldcatalog           = f_catalog
    importing
      ep_table                  = num_data
    exceptions
      generate_subpool_dir_full = 1
      others                    = 2.
  if sy-subrc <> 0.

  endif.

  field-symbols: <num_data> type standard table.
  assign num_data->* to <num_data>.

  move-corresponding p_tab to <num_data>.

  data(num_filed) = '<line>-ROWS_NUMBER'.
  loop at <num_data> assigning field-symbol(<line>).
    assign (num_filed) to field-symbol(<value>).
    <value> = sy-tabix.
  endloop.

  assign num_data->* to <tab>.

ENDFORM.