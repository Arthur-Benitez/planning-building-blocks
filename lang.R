#' Depends: global.R
#' 


lang <- list(
  app_version_text = sprintf('Versión %s (%s)', app_version, app_version_date),
  ## Autores
  owner = 'Replenishment Data Science MX',
  
  ## functions.R
  ventas = 'Ventas',
  # forecast_base = 'Forecast Base', # Duplicado en view_collaboration.R
  forecast_promocional = 'Promociones',
  no_resurtible = 'No Resurtible',
  fulfillment = 'Pres. + Disp.',
  fulfillment_ref = 'Pres. + Disp. (referencia)',
  gap = 'Gap',
  bp = 'Business Plan (BP)',
  ventas_ly = 'Ventas AA',
  # fecha_calculo = 'Fecha de Cálculo', #dup filters.R
  # ym = 'Año-Mes', #dup filters.R
  apuestas = 'Simulador',
  proyeccion = 'Proyección',
  potencial = 'Potencial',
  forecast_base_piezas = 'Forecast Base (piezas)',
  forecast_promocional_piezas = 'Promociones (piezas)',
  ventas_piezas = 'Ventas (piezas)',
  fulfillment_ref_piezas = 'Pres. + Disp. (piezas, referencia)',
  ventas_ly_piezas = 'Ventas AA (piezas)',
  
  # overview.R
  sales_ly = 'Ventas AA',
  ytd_ytg = 'YTD + YTG',
  ibp_last_iteration = 'IBP Last Iteration',
  sales_day = 'Venta AA por día',
  bp_day = 'Business Plan (BP) por día',
  ytd_ytg_day = 'YTD + YTG', 
  ytd_ytg_vs_sales_ly_abs = 'YTD + YTG vs Venta AA (abs)', 
  ytd_ytg_vs_sales_ly_p = 'YTD + YTG vs Venta AA (%)',
  ytd_ytg_vs_bp_abs = 'YTD + YTG vs BP (abs)',
  ytd_ytg_vs_bp_p = 'YTD + YTG vs BP (%)',
  price_sales_ly = 'Precio AA',
  price_bp = 'Precio BP',
  price_ibp = 'Precio YTD + YTG',
  
  month_current_ibp_sales_cost = 'Pesos ($)',
  month_current_ibp_sales_cost_percent = 'Pesos (%)',
  month_current_ibp_sales_eaches = 'Volumen (piezas)',
  month_current_ibp_sales_eaches_p = 'Volumen (%)',
  
  # demand-plan.R
  sales_ytd = 'Venta YTD',
  ibp_ytg = 'IBP YTG',
  cat_name = "Categoría",
  ytd_ibp_ytg = 'FY (YTD + IBP YTG)', 
  fy_vs_sales_ly = 'FY vs AA', 
  fy_vs_sales_ly_p = '%FY vs AA',
  fy_vs_bp = 'FY vs BP',
  fy_vs_bp_p = '%FY vs BP',
  
  # view_collaboration.R
  forecast_base = 'Forecast Base',
  forecast_promo = 'Building Blocks SMART',
  pretty_bb = 'Building Blocks',
  
  ## server.R
  app_name = 'BB-4',
  app_description = 'Building Blocks Forecasting',
  ibp = 'Integrated Business Planning',
  planning = 'Planeación y seguimiento',
  user_management = 'Administración de usuarios',
  password_update = 'Actualización de contraseña',
  # login = 'Iniciar sesión', #dup login.R
  reset = 'Reset',
  planning_tabset = 'Proyecciones',
  usage_stats = 'Utilización',
  faq = 'FAQ - Preguntas frecuentes',
  
  ## filters.R
  apply_filters = 'Aplicar',
  ym = 'Año-Mes',
  fecha_calculo = 'Fecha de cálculo',
  vicepresidencia = 'Vicepresidencia',
  departamento = "Departamento",
  categoria = "Categoría",
  formato = "Formato",
  tribu = "Tribu",
  squad = "Squad",
  
  # filters-ibp.R
  canal = 'Canal de Venta',
  marcas_estrategicas = 'Marcas Propias',
  vendor_nbr = 'No. Proveedor',
  dept_nbr = 'No. Dept.',
  dept_name = 'Departamento',
  clan = 'Clan',
  cat_nbr = 'No. Categoría',
  cat_name = 'Categoría',
  estado = 'Estado',
  nodo = 'Nodo',
  reg_nielsen = 'Región Nielsen',
  
  ## login.R
  user = 'Usuario',
  password = 'Contraseña',
  role = 'Permisos',
  goto_faq = 'Recuperar contraseña',
  login = 'Iniciar sesión',
  logout = 'Cerrar sesión',
  wrong_user_or_password = 'Usuario o contraseña incorrectos',
  old_password = 'Contraseña anterior',
  new_password_1 = 'Contraseña nueva',
  new_password_2 = 'Repetir contraseña nueva',
  button = 'Aplicar',
  passwords_must_match = 'La contraseña nueva debe ser igual en ambos recuadros',
  password_updated_successfully = 'Contraseña actualizada',
  wrong_current_password = 'Contraseña actual incorrecta',
  add = 'Crear',
  update_password = 'Actualizar contraseña',
  update_role = 'Actualizar permisos',
  delete = 'Eliminar',
  add_success = 'Usuario creado',
  update_password_success = 'Contraseña actualizada',
  update_role_success = 'Permisos actualizados',
  delete_success = 'Usuario eliminado',
  unknown_action = 'Acción desconocida',
  add_error = 'Error al crear usuario. Verifique que el usuario no exista y que tenga permisos menores a los de usted',
  update_password_error = 'Error al actualizar contraseña. Verifique que el usuario no exista y que tenga permisos menores a los de usted',
  update_role_error = 'Error al actualizar permisos. Verifique que el usuario exista y que sus nuevos permisos sean válidos e inferiores a los de usted',
  delete_error = 'Error al eliminar usuario. El usuario no existe o usted no tiene permiso para eliminarlo',
  manage_users = 'Administrar usuarios',
  action = 'Acción',
  selected_user = 'Usuarios disponibles',
  
  ## waterfall.R
  cumulative = 'Acumulado',
  title_error = 'No hay información disponible',
  waterfall_title = 'Building Blocks de %s, Calculado en %s',
  waterfall_footnote = '* (Montos en pesos)',
  selected_sim = 'Simulaciones guardadas',
  load_sim = 'Cargar',
  save_sim = 'Guardar',
  save_tree = 'Carpeta',
  share = 'Enviar',
  get_children = 'Bajar',
  get_parent = 'Subir',
  share_tree = 'Editar permisos',
  add_blob = 'Copiar / Compartir',
  delete_sim = 'Eliminar',
  visible = 'Visible',
  locked = 'Bloqueado',
  unlock_deadline = 'Fecha límite',
  choose_tree = 'Seleccionar carpeta',
  created_on = 'Creado en',
  created_by = 'Creado por',
  
  ## resumen.R
  
  ## Compartido
  download_data = 'Descargar',
  ok = 'Aceptar',
  confirm = 'Confirmar',
  cancel = 'Cancelar',
  refresh = 'Refrescar',
  money_units = '* Montos en pesos a precio de venta, sin impuestos.',
  days = 'Días',
  weeks = 'Semanas',
  months = 'Meses',
  years = 'Años',
  
  ## Usage stats
  kpi = 'KPI',
  split_by_clearance = 'Separar por nivel de permisos',
  unit = 'Eje temporal',
  
  ## faq.R
  download_bp_template = 'Descargar formato'
)
