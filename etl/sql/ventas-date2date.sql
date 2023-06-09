-- ESTA CONSULTA EXTRAE LAS VENTAS DE CIERTO PERIODO A UN NIVEL DE DEPARTAMENTO-CATEGOR�A-PROVEEDOR-MARCA - ESTADO-CEDIS-FORMATO - MES
WITH CALE AS (
	--CALENDARIO PARA FILTRAR EL RANGO DE TIEMPO A DESCARGAR, POR DIA
	SELECT
	  GREGORIAN_DATE,
		YEAR(GREGORIAN_DATE) AS ANUAL,
		MONTH(GREGORIAN_DATE) AS MES,
		(YEAR(GREGORIAN_DATE) * 100 +  MONTH(GREGORIAN_DATE)) AS YM,
		WM_YR_WK,
		SUN_MULT,
		MON_MULT,
		TUE_MULT,
		WED_MULT,
		THU_MULT,
		FRI_MULT,
		SAT_MULT
	FROM 
	  MX_CF_VM.CALENDAR_DAY
	WHERE
		GREGORIAN_DATE BETWEEN (?SALES_INI_DATE) AND (?SALES_END_DATE)
),  

--VENTAS CRUDAS CON UN RANGO DE TIEMPO A NIVEL MES, ITEM, TIENDA
VENTAS AS (
	SELECT
	  CALE.ANUAL,
	  CALE.MES,
	   CALE.YM,
	  SKD.STORE_NBR,
	  CASE
			WHEN FORMATO.TRAIT_NBR = 9 THEN 'BODEGA'
			WHEN FORMATO.TRAIT_NBR = 11 THEN 'SUPERAMA'
			WHEN FORMATO.TRAIT_NBR = 297 THEN 'SUPERCENTER'
			WHEN FORMATO.TRAIT_NBR = 1312 THEN 'MIBODEGA' 
			WHEN FORMATO.TRAIT_NBR IN (136, 969) THEN 'BAE'
			ELSE 'OTRO'
		END	AS FORMATO,
	  SKD.ITEM_NBR,
	  ITEM_DESC.WHPK_QTY,
	  ITEM_DESC.VNPK_QTY,
	  CAST(SUM(
			SKD.SAT_QTY * CALE.SAT_MULT +
	    SKD.SUN_QTY * CALE.SUN_MULT + 
	  	SKD.MON_QTY * CALE.MON_MULT + 
	  	SKD.TUE_QTY * CALE.TUE_MULT + 
	  	SKD.WED_QTY * CALE.WED_MULT + 
	  	SKD.THU_QTY * CALE.THU_MULT + 
	  	SKD.FRI_QTY * CALE.FRI_MULT
		) AS DECIMAL(32,2))AS SALES_QTY,
		CAST(SUM(
			SKD.SAT_SALES_AMT * CALE.SAT_MULT +
	  	SKD.SUN_SALES_AMT * CALE.SUN_MULT +
	  	SKD.MON_SALES_AMT * CALE.MON_MULT + 
	  	SKD.TUE_SALES_AMT * CALE.TUE_MULT + 
	  	SKD.WED_SALES_AMT * CALE.WED_MULT + 
	  	SKD.THU_SALES_AMT * CALE.THU_MULT + 
	  	SKD.FRI_SALES_AMT * CALE.FRI_MULT
		) AS DECIMAL(32,2)) AS SALES,	
		CAST(SALES_QTY AS DECIMAL(30,4)) / ITEM_DESC.WHPK_QTY AS SALES_WHPK,
		CAST(SALES_QTY AS DECIMAL(30,4)) / ITEM_DESC.VNPK_QTY AS SALES_VNPK
	FROM
	  MX_CF_VM.SKU_DLY_POS AS SKD
	  INNER JOIN CALE
			ON SKD.WM_YR_WK = CALE.WM_YR_WK
	  INNER JOIN MX_CF_VM.ITEM_DESC AS ITEM_DESC
			ON SKD.ITEM_NBR = ITEM_DESC.ITEM_NBR
		INNER JOIN MX_CF_VM.TRAIT_STORE AS FORMATO
			ON SKD.STORE_NBR = FORMATO.STORE_NBR AND FORMATO.TRAIT_NBR IN (9, 11, 136, 297, 969, 1312)
	GROUP BY 1, 2, 3, 4, 5, 6, 7, 8
),

--DETALLE ITEM, PROVEDOR, MARCAS ESTRATEGICAS, DEPARTAMENTO, DAPARTAMENTO_NAME, CATEGORIA, CATEGORIA NAME POR ITEM
INFO_ITEM AS (
	SELECT
  	ITEM_NBR,
		OLD_NBR,
		REPL_GROUP_NBR,
		UPC_NBR,
		ITEM1_DESC,
		ITEM_TYPE_CODE AS TIPO_ART,
		VENDOR_NBR,
		ACCTG_DEPT_NBR AS DEPT_NBR, 
  	SUBSTR (FINELINE_NBR,3,2) AS CAT_NBR, 
  	VNPK_COST_AMT/NULLIF(VNPK_QTY,0) AS COSTO, 
  	BASE_UNIT_RTL_AMT AS PRECIO, 
  	CUST_BASE_RTL_AMT AS PRECIO_IVA, 
  	CASE WHEN 
			BRAND_ID IN (
				312624,312625,312626,312627,312628,312629,312630,312631,312648,314614,315263,316405,317731,321588,328774,332303,335021,335945,361672
			) THEN 'NUESTRAS MARCAS'
      WHEN BRAND_ID IN (
				324092,324501,335023,353473,355413,355414,355415,355416,355417,355418,355419,355420,355421,355422,355423,355424,355425,355426,355427,
				355428,355429,355430,355431,355432,355433,355434,355435,355436,355437,355962,355963,355964,355965,355966,355967,355968,355969,355970,
				355971,355972,355973,355975,355976,355977,355978,355979,355981,355982,355983,355984,355985,355986,355987,355988,355989,355990,355991,
				356001,356002,356018,357117,357973,361429,361430,361517,361612,361686,361693,363811,361769
			) THEN 'MARCA PROPIA' 
      ELSE 'REGULAR' 
		END	AS MARCAS_ESTRATEGICAS
	FROM
		MX_CF_VM.ITEM
	WHERE VENDOR_NBR NOT IN (18)
),

TAB AS (
	-- SHIPS POR MES, ITEM, TIENDA, CEDIS
	SELECT
		CALE.ANUAL,
		--CALE.MES,
		SDS.ITEM_NBR,
		SDS.STORE_NBR,
		(SDS.SOURCE_ID) - 70000 AS CEDIS,
		SUM(
			SDS.SAT_SHIP_QTY * CALE.SAT_MULT +
			SDS.SUN_SHIP_QTY * CALE.SUN_MULT + 
			SDS.MON_SHIP_QTY * CALE.MON_MULT + 
			SDS.TUE_SHIP_QTY * CALE.TUE_MULT + 
			SDS.WED_SHIP_QTY * CALE.WED_MULT + 
			SDS.THU_SHIP_QTY * CALE.THU_MULT + 
			SDS.FRI_SHIP_QTY * CALE.FRI_MULT
		) AS SHIP_QTY
	FROM 
		MX_CF_VM.SKU_DLY_SHIP AS SDS
		INNER JOIN CALE
			ON SDS.WM_YR_WK = CALE.WM_YR_WK
	HAVING SHIP_QTY <> 0 AND CEDIS <> 600
	GROUP BY 1, 2, 3, 4
),

--CEDIS A TIENDA (BUSCAMOS EL CEDIS QUE MAS MERCANCIA LE DISTRIBUYO A UNA TIENDA EN UN PERIODO)
CEDIS AS (
	SELECT 
	  ANUAL,
		ITEM_NBR,
		STORE_NBR,
		CEDIS
	FROM (
		SELECT
			TAB.*,
			ROW_NUMBER() OVER(
				PARTITION BY ITEM_NBR, STORE_NBR,	ANUAL 
				ORDER BY SHIP_QTY DESC
			) AS RNK
		FROM TAB 
	) AS TAB_2
	WHERE RNK = 1
),

--REGIONES NIELSEN (REGION NIELSEN)
REGI_NIELSEN AS (
	SELECT
  	CAST(DET AS NUMBER) AS STORE_NBR,
		CAST(FORMATO AS CHAR(20)) AS FORMATO,
		CAST(STATE AS CHAR(20)) AS ESTADO,
		CAST(REG AS NUMBER) AS REG_NIELSEN
	FROM 
		WM_AD_HOC.CATALOG_STORE_2S AS CS2
	WHERE 
		FORMATO IN ('SUPERCENTER', 'SUPERAMA')
	
	UNION 
	
	SELECT
  	CAST(WM_TIENDA AS NUMBER) AS STORE_NBR,
		CAST(WM_NOM_NEG AS CHAR(20)) AS FORMATO,
		CAST(WM_EDO AS CHAR(20)) AS ESTADO,
		CAST(REGIONAL  AS NUMBER) AS REG_NIELSEN
	FROM
  	WM_AD_HOC.TABLA_TIENDAS AS TT
	WHERE
		FORMATO IN ('BOD', 'MIB', 'BAE')
),

--RELACION TIENDA-ESTADO UNICOS
TDA_EDO AS (
SELECT DISTINCT
	WM_TIENDA AS STORE_NBR,
	WM_EDO AS ESTADO,
	CASE 
		WHEN WM_EDO = 'Baja California Sur' THEN 'BCS'
		ELSE EDO3D
	END AS STATE_CODE
FROM
			WM_AD_HOC.T_TDAS
)

SELECT  
  	VENTAS.MES,
	  VENTAS.YM,
	CEDIS.CEDIS,
	TDA_EDO.ESTADO,
	REGI_NIELSEN.REG_NIELSEN,
	VENTAS.FORMATO,
	INFO_ITEM.VENDOR_NBR,
	INFO_ITEM.DEPT_NBR,
	INFO_ITEM.CAT_NBR,
	INFO_ITEM.MARCAS_ESTRATEGICAS,
	SUM(SALES_WHPK) AS SALES_WHPK,
	SUM(SALES_VNPK) AS SALES_VNPK,
	SUM(SALES_QTY) AS SALES_QTY,
	SUM(SALES) AS SALES
FROM  
	VENTAS
	LEFT JOIN INFO_ITEM
		ON VENTAS.ITEM_NBR = INFO_ITEM.ITEM_NBR
	LEFT JOIN CEDIS
		ON VENTAS.ANUAL = CEDIS.ANUAL AND VENTAS.STORE_NBR = CEDIS.STORE_NBR AND VENTAS.ITEM_NBR = CEDIS.ITEM_NBR
	LEFT JOIN REGI_NIELSEN
	 	ON VENTAS.STORE_NBR = REGI_NIELSEN.STORE_NBR
	LEFT JOIN TDA_EDO
	 	ON VENTAS.STORE_NBR = TDA_EDO.STORE_NBR
	WHERE INFO_ITEM.VENDOR_NBR NOT IN (18)
GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
