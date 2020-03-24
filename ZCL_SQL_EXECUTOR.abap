class ZCL_SQL_EXECUTOR definition
  public
  final
  create public .

public section.

  class-methods RESULT
    importing
      value(I_CON_NAME) type STRING default 'DEFAULT'
      value(I_QUERY) type STRING
    exporting
      value(E_COMM) type STRING
      value(E_STATUS) type CHAR1
    changing
      value(C_DATA_TABLE) type ref to DATA .
  class-methods EXECUTE
    importing
      value(I_QUERY) type STRING
    exporting
      value(E_COMM) type STRING
      value(E_STATUS) type CHAR1 .
  class-methods RESULT_HANA_MODEL
    importing
      value(I_CON_NAME) type STRING default 'DEFAULT'
      value(I_QUERY) type STRING
      value(I_EXECUTE) type CHAR1 default ABAP_TRUE
    exporting
      value(E_CATALOG) type LVC_T_FCAT
      value(E_COMM) type STRING
      value(E_STATUS) type CHAR1
      value(E_SCHEMA) type STRING
      value(E_PATCH) type STRING
      value(E_MODEL_NAME) type STRING
    returning
      value(R_TABLE) type ref to DATA .
protected section.
private section.

  class-methods GET_CATALOG_FROM_HANA_MODEL
    importing
      !I_SQL type STRING
    exporting
      !E_CATALOG type LVC_T_FCAT
      !E_SQL type STRING
      !E_SCHEMA type STRING
      !E_PATCH type STRING
      !E_MODEL_NAME type STRING .
  class-methods GET_MODEL_META
    importing
      !I_SQL type STRING
    exporting
      !E_CATALOG_NAME type STRING
      !E_CUBE_NAME type STRING
      !E_REQUEST_TAIL type STRING .
  class-methods APPEND_FIELD
    importing
      !I_META_LINE type CL_RSDD_HANA_MODEL=>TN_S_METADATA
    changing
      !C_CATALOG type LVC_T_FCAT .
ENDCLASS.



CLASS ZCL_SQL_EXECUTOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SQL_EXECUTOR=>APPEND_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_META_LINE                    TYPE        CL_RSDD_HANA_MODEL=>TN_S_METADATA
* | [<-->] C_CATALOG                      TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method APPEND_FIELD.

    data: fcat_property type lvc_s_fcat.
    fcat_property-col_pos = sy-tabix.
    fcat_property-fieldname = i_meta_line-column_name.
    fcat_property-outputlen = i_meta_line-intlen.
    fcat_property-datatype = i_meta_line-datatp.
    fcat_property-intlen = i_meta_line-intlen.
    fcat_property-reptext = i_meta_line-column_name.
    fcat_property-scrtext_l = i_meta_line-column_name.
    fcat_property-scrtext_m = i_meta_line-column_name.
    fcat_property-scrtext_s = i_meta_line-column_name.
    fcat_property-decimals = i_meta_line-decimals.
    fcat_property-just = abap_true.
    fcat_property-lowercase = abap_true.
    append fcat_property to c_catalog.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SQL_EXECUTOR=>EXECUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_QUERY                        TYPE        STRING
* | [<---] E_COMM                         TYPE        STRING
* | [<---] E_STATUS                       TYPE        CHAR1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method EXECUTE.

    data: connection type ref to cl_sql_connection,
          statement  type ref to cl_sql_statement.

    try.
        connection = cl_sql_connection=>get_connection( ).
        statement = connection->create_statement( ).
        statement->execute_ddl( I_QUERY ).
        connection->close( ).

        e_status = 'S'.
      catch cx_sql_exception into data(ex_obj).
        e_status = 'E'.
        e_comm = ex_obj->get_text( ).
    endtry.

    free: connection, statement.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SQL_EXECUTOR=>GET_CATALOG_FROM_HANA_MODEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SQL                          TYPE        STRING
* | [<---] E_CATALOG                      TYPE        LVC_T_FCAT
* | [<---] E_SQL                          TYPE        STRING
* | [<---] E_SCHEMA                       TYPE        STRING
* | [<---] E_PATCH                        TYPE        STRING
* | [<---] E_MODEL_NAME                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_CATALOG_FROM_HANA_MODEL.

    data(sql) = i_sql.

    replace all occurrences of 'from' in sql with 'FROM'.
    condense sql.

    zcl_sql_executor=>get_model_meta( exporting i_sql = sql
                                      importing e_catalog_name = e_patch
                                                e_cube_name = e_model_name
                                                e_request_tail = data(request_tail) ).

    data: hana_model type ref to cl_rsdd_hana_model.
    try.
        hana_model = cl_rsdd_hana_model=>factory( i_catalog_name = e_patch
                                                  i_cube_name    = e_model_name ).
      catch cx_root.
        exit.
    endtry.

    hana_model->get_metadata( importing e_t_metadata = data(metadata)
                                        e_s_cube = data(s_cube) ).
    e_schema = s_cube-schema_name.
    sort metadata by key_column descending column_name.

*************************************
    if sql cs '*'.

      loop at metadata into data(metadata_line).
        if metadata_line-aggrgen is not initial.
          e_sql = |{ e_sql } { metadata_line-aggrgen }("{ metadata_line-column_name }") as "{ metadata_line-column_name }"|.
        else.
          e_sql = |{ e_sql } "{ metadata_line-column_name }"|.
        endif.
        if sy-tabix <> lines( metadata ).
          e_sql = e_sql && ','.
        endif.

        zcl_sql_executor=>append_field( exporting i_meta_line = metadata_line changing c_catalog = e_catalog ).
      endloop.

    else.

      split sql at 'FROM' into data(select_feild) data(trash).
      select_feild = select_feild+6.
      condense select_feild.

      split select_feild at ',' into table data(field_list).

      loop at field_list into data(field).
        translate field to upper case.

        if field cp '*SUM(*)*' or field cp '*MIN(*)*' or field cp '*MAX(*)*'.

          split field at 'AS' into trash select_feild.

          translate select_feild using '" '.
          condense select_feild.
          metadata_line = metadata[ column_name = select_feild ].
          e_sql = |{ e_sql } { field }|.
        else.
          translate field using '" '.
          condense field.
          metadata_line = metadata[ column_name = field ].
          e_sql = |{ e_sql } "{ metadata_line-column_name }"|.
        endif.

        if sy-tabix <> lines( metadata ).
          e_sql = e_sql && ','.
        endif.

        zcl_sql_executor=>append_field( exporting i_meta_line = metadata_line changing c_catalog = e_catalog ).
      endloop.

    endif.

    e_sql = |select { e_sql } from "{ e_schema }"."{ e_patch }/{ e_model_name }" { request_tail }|.
*    if strlen( request_tail ) > 0.
*      e_sql = |{ e_sql } { request_tail }|.
*    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SQL_EXECUTOR=>GET_MODEL_META
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SQL                          TYPE        STRING
* | [<---] E_CATALOG_NAME                 TYPE        STRING
* | [<---] E_CUBE_NAME                    TYPE        STRING
* | [<---] E_REQUEST_TAIL                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_MODEL_META.

    split i_sql at '.' into data(param1) data(param2).

    find first occurrence of regex '\"\S+\"' in param2 match offset data(moff) match length data(mlen).

    param1 = param2+moff(mlen).
    e_request_tail = param2+mlen.

    translate param1 using '" '.
    condense param1.
    split param1 at '/' into e_catalog_name e_cube_name.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SQL_EXECUTOR=>RESULT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CON_NAME                     TYPE        STRING (default ='DEFAULT')
* | [--->] I_QUERY                        TYPE        STRING
* | [<---] E_COMM                         TYPE        STRING
* | [<---] E_STATUS                       TYPE        CHAR1
* | [<-->] C_DATA_TABLE                   TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method RESULT.

    data: connection type ref to cl_sql_connection,
          statement  type ref to cl_sql_statement,
          result_set type ref to cl_sql_result_set.

    try.

        connection = cl_sql_connection=>get_connection( con_name = conv dbcon_name( i_con_name ) ).
        statement = new #( con_ref = connection ).
        result_set = statement->execute_query( i_query ).

        result_set->set_param_table( c_data_table ).
        result_set->next_package( ).
        result_set->close( ).

        e_status = 'S'.
      catch cx_sql_exception into data(ex).
        e_status = 'E'.
        e_comm = ex->get_text( ).
    endtry.

    free: connection, statement, result_set.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SQL_EXECUTOR=>RESULT_HANA_MODEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CON_NAME                     TYPE        STRING (default ='DEFAULT')
* | [--->] I_QUERY                        TYPE        STRING
* | [--->] I_EXECUTE                      TYPE        CHAR1 (default =ABAP_TRUE)
* | [<---] E_CATALOG                      TYPE        LVC_T_FCAT
* | [<---] E_COMM                         TYPE        STRING
* | [<---] E_STATUS                       TYPE        CHAR1
* | [<---] E_SCHEMA                       TYPE        STRING
* | [<---] E_PATCH                        TYPE        STRING
* | [<---] E_MODEL_NAME                   TYPE        STRING
* | [<-()] R_TABLE                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method RESULT_HANA_MODEL.

    try .
        zcl_sql_executor=>get_catalog_from_hana_model( exporting i_sql = i_query
                                                       importing e_catalog = e_catalog
                                                                 e_schema = e_schema
                                                                 e_patch = e_patch
                                                                 e_model_name = e_model_name
                                                                 e_sql = data(query) ).
        if lines( e_catalog ) = 0.
          e_status = 'E'.
          e_comm = 'Хана модель не найдена'.
          return.
        endif.
      catch cx_root.
        e_status = 'E'.
        e_comm = 'Ошибка при парсинге запроса'.
        return.
    endtry.

    check i_execute = abap_true.
    cl_alv_table_create=>create_dynamic_table( exporting it_fieldcatalog = e_catalog
                                               importing ep_table        = r_table ).

    zcl_sql_executor=>result( exporting i_query = query
                                        i_con_name = i_con_name
                              importing e_comm = e_comm
                                        e_status = e_status
                               changing c_data_table = r_table ).

  endmethod.
ENDCLASS.