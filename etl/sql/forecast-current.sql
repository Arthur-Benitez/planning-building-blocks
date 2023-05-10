/*
#'' Consulta que obtiene datos de diferentes tablas
#'  mostrando los forecast e informacion general de cada articulo-formato 
#' 
#' Autor: Eduardo Castro <eduardo.castro0@walmart.com>
#' Colaboracion: Robert Paniagua <mrpania@walmart.com>
#'               Felipe Gerard <felipe.gerard@walmart.com>
#' 
*/

-- Consulta Dias Del Mes
WITH DDM AS
(
	SELECT 
		1 AS LLAVE,
		GREGORIAN_DATE AS FECHA,
		EXTRACT (DAY FROM GREGORIAN_DATE) AS DIA
	FROM 
		CALENDAR_DAY
	WHERE
		CALENDAR_DAY.WM_MONTH = (SELECT calendar_day.WM_MONTH FROM calendar_day WHERE calendar_day.GREGORIAN_DATE = DATE) 
		AND CALENDAR_DAY.FISCAL_YEAR = (SELECT calendar_day.FISCAL_YEAR  FROM calendar_day WHERE calendar_day.GREGORIAN_DATE = DATE)
		AND GREGORIAN_DATE >= DATE
),
-- Consulta Gregorian Date
GD AS
( 
	SELECT
		GREGORIAN_DATE
	FROM 
		CALENDAR_DAY AS C
	WHERE
		C.WM_MONTH =  
		CASE
			WHEN (SELECT C.WM_MONTH + ?NUM_FUTMONTHS  FROM CALENDAR_DAY AS C WHERE C.GREGORIAN_DATE = DATE) > 12 
			THEN (SELECT C.WM_MONTH + ?NUM_FUTMONTHS  FROM CALENDAR_DAY AS C WHERE C.GREGORIAN_DATE = DATE) -12
			ELSE (SELECT C.WM_MONTH + ?NUM_FUTMONTHS  FROM CALENDAR_DAY AS C WHERE C.GREGORIAN_DATE = DATE) END 
		AND C.FISCAL_YEAR = 
		CASE
			WHEN (SELECT C.WM_MONTH + ?NUM_FUTMONTHS  FROM CALENDAR_DAY AS C WHERE C.GREGORIAN_DATE = DATE) > 12 
				THEN  (SELECT C.FISCAL_YEAR +1 FROM CALENDAR_DAY AS C WHERE C.GREGORIAN_DATE = DATE)
				ELSE  (SELECT C.FISCAL_YEAR  FROM CALENDAR_DAY AS C WHERE C.GREGORIAN_DATE = DATE) 
			END
	
),

--Consulta Promo Fulfillment
PF AS
(
	SELECT
		1 AS LLAVE,
		T1.GRS_PROMOTION_ID,
		T1.PROMOTION_ID,
		T1.PROMO_START_DATE,
		(T1.PROMO_START_DATE + (T1.PROMO_DURATION_NBR/1440)) AS PROMO_END_DATE,
		T1.PROMO_PRIORITY_NBR,
		T1.ADDITIVE_SWITCH_CNT,
		T2.REPL_GROUP_NBR,
		T2.GRS_LOC_ID,
		SUBSTR(T2.GRS_LOC_ID,12,4) AS STORE_NBR,
		T2.OVRD_PRESENTATION_QTY,
		T2.OVRD_DISPLAY_QTY

	FROM
		MX_CF_REPL_VM.GRS_PROMOTION T1,
		MX_CF_REPL_VM.GRS_DMDUNIT_PROMO T2

	WHERE
		T1.GRS_PROMOTION_ID = T2.GRS_PROMOTION_ID
		AND T1.PROMOTION_ID NOT LIKE 'DEFAULT'
		AND T1.PROMOTION_ID NOT LIKE 'ARR-PLAN'
		AND T1.PROMOTION_ID NOT LIKE 'Discontinuation PROFILE'
		AND T1.PROMOTION_ID NOT LIKE 'APER%'
		AND (T2.OVRD_PRESENTATION_QTY > 0 OR T2.OVRD_DISPLAY_QTY>0)
		AND PROMO_END_DATE >= (SELECT MIN(GREGORIAN_DATE) FROM calendar_day WHERE WM_MONTH =(SELECT WM_MONTH FROM calendar_day WHERE GREGORIAN_DATE = DATE) AND FISCAL_YEAR =(SELECT FISCAL_YEAR FROM calendar_day WHERE GREGORIAN_DATE = DATE)) 
		AND T1.PROMO_START_DATE <= (SELECT  MAX(GREGORIAN_DATE)  FROM calendar_day WHERE  WM_MONTH =
			(SELECT WM_MONTH FROM calendar_day WHERE GREGORIAN_DATE = DATE)  
			AND FISCAL_YEAR =(SELECT FISCAL_YEAR FROM calendar_day WHERE GREGORIAN_DATE = DATE))
),

-- JOIN consultas Dias Del Mes y Promo Fufillment
DDM_PF AS
(
	SELECT 
		P.LLAVE,
		P.GRS_PROMOTION_ID,
		P.PROMOTION_ID,
		P.PROMO_START_DATE,
		P.PROMO_END_DATE,
		P.PROMO_PRIORITY_NBR,
		P.ADDITIVE_SWITCH_CNT,
		P.REPL_GROUP_NBR,
		P.GRS_LOC_ID,
		P.STORE_NBR,
		1.00000 * P.OVRD_PRESENTATION_QTY AS OVRD_PRESENTATION_QTY,
		P.OVRD_DISPLAY_QTY,
		D.FECHA,
		D.DIA
	FROM PF AS P
	INNER JOIN DDM AS D
		ON D.LLAVE = P.LLAVE

),
-- Sumariza la consulta anterior obteniendo la minima prioridad cuando ADDITIVE_SWITCH_CNT = 0
MIN_PTY_DDM_PF AS
(
	SELECT
		D.LLAVE,
		D.GRS_PROMOTION_ID,
		D.PROMOTION_ID,
		D.PROMO_START_DATE,
		D.PROMO_END_DATE,
		D.PROMO_PRIORITY_NBR, 
		D.ADDITIVE_SWITCH_CNT,
		D.REPL_GROUP_NBR,
		D.GRS_LOC_ID,
		D.STORE_NBR,
		D.OVRD_PRESENTATION_QTY,
		D.OVRD_DISPLAY_QTY,
		D.FECHA,
		D.DIA
	FROM  DDM_PF AS D
		INNER JOIN
		(
			SELECT 
				REPL_GROUP_NBR,
				STORE_NBR, 
				FECHA, 
				DIA, 
				PROMO_END_DATE,
				MIN(PROMO_PRIORITY_NBR) AS PROMO_PRIORITY_NBR
			FROM DDM_PF
			GROUP BY 1, 2, 3, 4, 5
			WHERE ADDITIVE_SWITCH_CNT = 0
		) AS S
		ON D.REPL_GROUP_NBR = S.REPL_GROUP_NBR 
			AND D.STORE_NBR = S.STORE_NBR 
			AND D.FECHA = S.FECHA 
			AND D.PROMO_PRIORITY_NBR = S.PROMO_PRIORITY_NBR
		WHERE D.ADDITIVE_SWITCH_CNT = 0

),

-- Sumariza la consulta anterior obteniendo la maxima fecha de finalizacion de la Promo cuando ADDITIVE_SWITCH_CNT = 0
MAX_END_DDM_PF AS
(
	SELECT 
		M.LLAVE,
		M.GRS_PROMOTION_ID,
		M.PROMOTION_ID,
		M.PROMO_START_DATE,
		M.PROMO_END_DATE,
		M.PROMO_PRIORITY_NBR,
		M.ADDITIVE_SWITCH_CNT,
		M.REPL_GROUP_NBR,
		M.GRS_LOC_ID,
		M.STORE_NBR,
		M.OVRD_PRESENTATION_QTY,
		M.OVRD_DISPLAY_QTY,
		M.FECHA, M.DIA
	FROM MIN_PTY_DDM_PF AS M
	INNER JOIN 
	(
		SELECT 
			REPL_GROUP_NBR,
			STORE_NBR, 
			FECHA,
			DIA, 
			PROMO_PRIORITY_NBR, 
			MAX(PROMO_END_DATE) AS PROMO_END_DATE
		FROM MIN_PTY_DDM_PF
		GROUP BY 1, 2, 3, 4, 5
		WHERE ADDITIVE_SWITCH_CNT = 0
	) AS S
	ON M.REPL_GROUP_NBR = S.REPL_GROUP_NBR
		AND M.STORE_NBR = S.STORE_NBR 
		AND M.FECHA = S.FECHA 
		AND M.PROMO_PRIORITY_NBR = S.PROMO_PRIORITY_NBR
		
),
-- Une la consulta anterior (DDM_PF) con la sumarizacion previa
DDM_PF_U AS
(
	SELECT *
	FROM MAX_END_DDM_PF AS M
	UNION
		(SELECT * FROM DDM_PF WHERE ADDITIVE_SWITCH_CNT = 1) 

),

-- Calcula OVRD_PRESENTATION_QTY y OVRD_DISPLAY_QTY
TRNS_DDM_PF AS
(
	SELECT 
		LLAVE, 
		GRS_PROMOTION_ID,
		PROMOTION_ID,
		PROMO_START_DATE,
		PROMO_END_DATE,
		PROMO_PRIORITY_NBR,
		ADDITIVE_SWITCH_CNT,
		REPL_GROUP_NBR,
		GRS_LOC_ID,
		STORE_NBR,
		FECHA,
		DIA,
		OVRD_PRESENTATION_QTY,
		OVRD_DISPLAY_QTY
	FROM DDM_PF_U

	WHERE
		FECHA >= PROMO_START_DATE AND FECHA < PROMO_END_DATE
),

-- Agrupa la consulta anterior obteniendo los maximos de OVRD_PRESENTATION_QTY y OVRD_DISPLAY_QTY
GROUP_DDM_PF AS
(
	SELECT
		REPL_GROUP_NBR,
		STORE_NBR,
		MAX(OVRD_PRESENTATION_QTY) AS OVRD_PRESENTATION_QTY,
		MAX(OVRD_DISPLAY_QTY) AS OVRD_DISPLAY_QTY
	FROM TRNS_DDM_PF
	GROUP BY 1, 2

),

-- Consulta Tiendas Formato
TIENDAS_FORMATO AS 
(
	SELECT  DISTINCT
		T1.Store_nbr,
		T2.Store_name,
		T2.OPEN_STATUS,
		T1.Trait_Nbr,
		CASE 
			WHEN T1.Trait_Nbr = 297 THEN 'SUPERCENTER' 
			WHEN T1.Trait_Nbr = 9 THEN 'BODEGA'
			WHEN T1.Trait_Nbr = 11 THEN 'SUPERAMA'
			WHEN T1.Trait_Nbr = 1312 THEN 'MIBODEGA'
			WHEN T1.Trait_Nbr IN (136,969) THEN 'BAE' 
			ELSE 'OTRO' 
		END AS NEGOCIO,
		T2.region_Nbr,
		T2.OPEN_DATE

	FROM
		MX_CF_VM.TRAIT_STORE T1,
		MX_CF_VM.Store_info T2

	WHERE
		T1.Store_Nbr = T2.Store_Nbr
		AND T1.Trait_Nbr IN (11,297,1312,969,136,9)

),
-- JOIN de las consultas (DDM_PF) y Tiendas Formato
DDM_PF_TF AS
(
	SELECT
		REPL_GROUP_NBR,
		NEGOCIO,
		SUM(OVRD_PRESENTATION_QTY) AS OVRD_PRESENTATION_QTY,
		SUM(OVRD_DISPLAY_QTY) AS OVRD_DISPLAY_QTY
	FROM
	(
		SELECT
			G.REPL_GROUP_NBR,
			G.OVRD_PRESENTATION_QTY,
			G.OVRD_DISPLAY_QTY,
			T.NEGOCIO
		FROM
			GROUP_DDM_PF AS G
			INNER JOIN
			TIENDAS_FORMATO AS T
			ON 
			G.STORE_NBR = T.STORE_NBR
			
	) AS J
	GROUP BY 1, 2
	
),

-- Consutla CTE_CALENDAR
CTE_CALENDAR AS 
(
	SELECT  CALENDAR_DAY.WM_YR_WK,
		COUNT(CALENDAR_DAY.WM_YR_WK) AS DIAS,
		SUM(SAT_MULT) AS SAT_MULT,
		SUM(SUN_MULT) AS SUN_MULT,
		SUM(MON_MULT) AS MON_MULT,
		SUM(TUE_MULT) AS TUE_MULT,
		SUM(WED_MULT) AS WED_MULT,
		SUM(THU_MULT) AS THU_MULT,
		SUM(FRI_MULT) AS FRI_MULT
	FROM   CALENDAR_DAY 
	WHERE  CALENDAR_DAY.WM_MONTH IN 
		(
			SELECT CALENDAR_DAY.WM_MONTH  
			FROM     CALENDAR_DAY 
			WHERE  CALENDAR_DAY.GREGORIAN_DATE = DATE) 
	AND CALENDAR_DAY.FISCAL_YEAR IN
		(
			SELECT FISCAL_YEAR  
			FROM   CALENDAR_DAY 
			WHERE  GREGORIAN_DATE = DATE) 
	GROUP BY 1
	),

--Consulta CTE_FCST
CTE_FCST AS 
(
	SELECT
		FB.DEPT_NBR,
		FB.CAT_NBR,
		FB.ITEM_NBR,
		FB.OLD_NBR,
		FB.REPL_GROUP_NBR,
		FB.UPC_NBR,
		FB.ITEM1_DESC,
		FB.TIPO_ART,
		FB.COSTO,
		FB.PRECIO,
		FB.PRECIO_IVA,
		FB.FORMATO,
		FB.FCST_WM_YR_WK,
		FB.WM_YR_WK,
		FB.WHSE_ALIGN,
		CAST(SUM(FB.FCST_BASE) AS DECIMAL (15,5)) AS FCST_BASE, 
		ZEROIFNULL (SUM(FP.FCST_PROM)) AS FCST_PROM 

	FROM 
		(
			SELECT 
				T1.DEPT_NBR,
				SUBSTR (T1.FINELINE_NBR,3,2) AS CAT_NBR,
				T1.ITEM_NBR,
				T1.OLD_NBR,
				T1.REPL_GROUP_NBR,
				T1.UPC_NBR,
				T1.ITEM1_DESC,
				T1.ITEM_TYPE_CODE AS TIPO_ART,
				(T1.VNPK_COST_AMT*1.00 / T1.VNPK_QTY*1.00) AS COSTO,
				T1.BASE_UNIT_RTL_AMT AS PRECIO,
				T1.CUST_BASE_RTL_AMT AS PRECIO_IVA,
				CASE
					WHEN T4.TRAIT_NBR=297 THEN 'SUPERCENTER' 
					WHEN T4.TRAIT_NBR=9 THEN 'BODEGA'
					WHEN T4.TRAIT_NBR=11 THEN 'SUPERAMA'
					WHEN T4.TRAIT_NBR=1312 THEN 'MIBODEGA'
					WHEN T4.TRAIT_NBR IN (969,136) THEN 'BAE' 
					ELSE 'OTRO' 
				END AS FORMATO,
				T2.STORE_NBR,
				T2.STORE_NAME,
				RIGHT (RTRIM (T2.CITY),3) AS ESTADO,
				T3.FCST_WM_YR_WK,
				T3.WM_YR_WK,
				WHSE_ALIGN_TYPE_CD AS WHSE_ALIGN,
				ZEROIFNULL 
					(	
						CASE 
							WHEN FCST_WM_YR_WK = WM_YR_WK THEN SUM (SALES_FCST_EACH_QTY) 
							WHEN 
								FCST_WM_YR_WK = 
								(
									SELECT WM_YR_WK 
									FROM 	 CALENDAR_DAY 
									WHERE  GREGORIAN_DATE = DATE
								) 
							AND 
								WM_YR_WK > 
								(
									SELECT WM_YR_WK 
									 FROM 	CALENDAR_DAY 
									 WHERE GREGORIAN_DATE = DATE
								)
						THEN SUM (SALES_FCST_EACH_QTY) 
						END 
					) AS FCST_BASE
			FROM	MX_CF_VM.ITEM T1
			
			INNER JOIN 
					MX_CF_VM.STORE_ITEM_FCST_WK_CONV T3
					ON 	    T1.ITEM_NBR =T3.ITEM_NBR
			INNER JOIN 
					MX_CF_VM.STORE_INFO T2
					ON 		T2.STORE_NBR = T3.STORE_NBR
			INNER JOIN 
					MX_CF_VM.TRAIT_STORE T4
					ON 	 	T2.STORE_NBR = T4.STORE_NBR
			WHERE   T4.TRAIT_NBR IN (11,297,1312,969,136,9)
					AND 
					T3.JDA_FCST_TYPE_CD NOT IN (5)
					AND 
					T3.FCST_WM_YR_WK IN 
					(
						SELECT WM_YR_WK 
						 FROM CALENDAR_DAY 
						 WHERE GREGORIAN_DATE = DATE
					)
					AND 
						T3.WM_YR_WK IN (SELECT WM_YR_WK FROM CTE_CALENDAR)
			GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18
		) AS FB
	LEFT JOIN 	
	(
		SELECT 
			STORE_NBR,
			ITEM_NBR,
			FCST_WM_YR_WK,
			WM_YR_WK,
			ZEROIFNULL 
			(
				CASE 
					WHEN FCST_WM_YR_WK = WM_YR_WK THEN SUM (SALES_FCST_EACH_QTY) 
					WHEN FCST_WM_YR_WK = (SELECT WM_YR_WK FROM CALENDAR_DAY WHERE GREGORIAN_DATE = DATE) AND WM_YR_WK > (SELECT WM_YR_WK FROM CALENDAR_DAY WHERE GREGORIAN_DATE = DATE) THEN SUM (SALES_FCST_EACH_QTY) 
				END ) AS FCST_PROM

		FROM
			MX_CF_VM.STORE_ITEM_FCST_WK_CONV 
		WHERE
			JDA_FCST_TYPE_CD = 5
			AND FCST_WM_YR_WK IN (SELECT WM_YR_WK FROM    CALENDAR_DAY WHERE GREGORIAN_DATE = DATE)
			AND WM_YR_WK IN (SELECT WM_YR_WK FROM CTE_CALENDAR)
			GROUP BY
			1,2,3,4
	) AS FP 
		ON (FB.STORE_NBR = FP.STORE_NBR 
			AND 
			FB.ITEM_NBR = FP.ITEM_NBR 
			AND 
			FB.FCST_WM_YR_WK=FP.FCST_WM_YR_WK AND FB.WM_YR_WK=FP.WM_YR_WK )
	GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
),

-- Consulta CTE_ALLOC
CTE_ALLOC AS
(
	SELECT
		ALLOC.ITEM_NBR,
		ALLOC.FORMATO,
		ALLOC.ALLOC_CAL_NAME,
		SUM(ALLOC.SAT) AS SAT,
		SUM(ALLOC.SUN) AS SUN,
		SUM(ALLOC.MON) AS MON,
		SUM(ALLOC.TUE) AS TUE,
		SUM(ALLOC.WED) AS WED,
		SUM(ALLOC.THU) AS THU,
		SUM(ALLOC.FRI) AS FRI

	FROM
	(
		SELECT
			T1.ITEM_NBR,
			T1.ALLOC_CAL_NAME AS ALLOC_CAL_NAME,
			CASE
			WHEN T4.TRAIT_NBR=297 THEN 'SUPERCENTER' 
			WHEN T4.TRAIT_NBR=9 THEN 'BODEGA'
			WHEN T4.TRAIT_NBR=11 THEN 'SUPERAMA'
			WHEN T4.TRAIT_NBR=1312 THEN 'MIBODEGA'
			WHEN T4.TRAIT_NBR IN (969,136) THEN 'BAE' ELSE 'OTRO' END AS FORMATO,
			CASE WHEN T3.SAT_MULT = 1 THEN  T2.DAILY_ALLOC_PCT ELSE 0 END AS SAT,
			CASE WHEN T3.SUN_MULT = 1 THEN  T2.DAILY_ALLOC_PCT ELSE 0 END AS SUN,
			CASE WHEN T3.MON_MULT = 1 THEN  T2.DAILY_ALLOC_PCT ELSE 0 END AS MON,
			CASE WHEN T3.TUE_MULT = 1 THEN  T2.DAILY_ALLOC_PCT ELSE 0 END AS TUE,
			CASE WHEN T3.WED_MULT = 1 THEN  T2.DAILY_ALLOC_PCT ELSE 0 END AS WED,
			CASE WHEN T3.THU_MULT = 1 THEN  T2.DAILY_ALLOC_PCT ELSE 0 END AS THU,
			CASE WHEN T3.FRI_MULT = 1 THEN  T2.DAILY_ALLOC_PCT ELSE 0 END AS FRI
		FROM
			MX_CF_REPL_VM.GRS_FULFILLMENT_PARM T1,
			MX_CF_REPL_VM.GRS_CAL T2,
			MX_CF_VM.CALENDAR_DAY T3,
			MX_CF_VM.TRAIT_STORE T4
		WHERE
			T1.ALLOC_CAL_NAME = T2.CAL_NAME
			AND T2.CAL_EFF_DATE = T3.GREGORIAN_DATE
			AND T1.STORE_NBR = T4.STORE_NBR
			AND T4.TRAIT_NBR IN (9,11,136,297,969,1312)
			AND T2.DAILY_ALLOC_PCT > 0 
		
		GROUP BY 1,2,3,4,5,6,7,8,9,10 ) AS ALLOC
	GROUP BY 1,2,3),

-- Consulta Forecast Base y Promo
FCST_B_P AS
(
	SELECT 
		L.DEPT_NBR,
		L.CAT_NBR, 
		L.ITEM_NBR,
		L.OLD_NBR,
		L.REPL_GROUP_NBR,
		L.UPC_NBR,
		L.ITEM1_DESC,
		L.TIPO_ART,
		L.COSTO,
		L.PRECIO,
		L.PRECIO_IVA,
		L.FCST_BASE,
		L.FCST_PROM,
		L.FORMATO,
		L.FCST_WM_YR_WK,
		L.WM_YR_WK,
		L.WHSE_ALIGN,
		L.ALLOC_CAL_NAME,
		L.SAT,
		L.SUN,
		L.MON,
		L.TUE,
		L.WED,
		L.THU,
		L.FRI,
		CASE WHEN R.DIAS IS NULL THEN L.DIAS ELSE R.DIAS END AS DIAS,
		CASE WHEN R.SAT_MULT IS NULL THEN L.SAT_MULT ELSE R.SAT_MULT END AS SAT_MULT,
		CASE WHEN R.SUN_MULT IS NULL THEN L.SUN_MULT ELSE R.SUN_MULT END AS SUN_MULT,
		CASE WHEN R.MON_MULT IS NULL THEN L.MON_MULT ELSE R.MON_MULT END AS MON_MULT,
		CASE WHEN R.TUE_MULT IS NULL THEN L.TUE_MULT ELSE R.TUE_MULT END AS TUE_MULT,
		CASE WHEN R.WED_MULT IS NULL THEN L.WED_MULT ELSE R.WED_MULT END AS WED_MULT,
		CASE WHEN R.THU_MULT IS NULL THEN L.THU_MULT ELSE R.THU_MULT END THU_MULT,
		CASE WHEN R.FRI_MULT IS NULL THEN L.FRI_MULT ELSE R.FRI_MULT END AS FRI_MULT
	FROM 
	(
		SELECT 
			CTE1.DEPT_NBR,
			CTE1.CAT_NBR, 
			CTE1.ITEM_NBR,
			CTE1.OLD_NBR,
			CTE1.REPL_GROUP_NBR,
			CTE1.UPC_NBR,
			CTE1.ITEM1_DESC,
			CTE1.TIPO_ART,
			CTE1.COSTO,
			CTE1.PRECIO,
			CTE1.PRECIO_IVA,
			CTE3.FORMATO,
			CTE1.FCST_WM_YR_WK,
			CTE1.WM_YR_WK,
			CTE1.WHSE_ALIGN,
			CTE1.FCST_BASE,
			CTE1.FCST_PROM,
			CTE2.DIAS,
			CTE2.SAT_MULT,
			CTE2.SUN_MULT,
			CTE2.MON_MULT,
			CTE2.TUE_MULT,
			CTE2.WED_MULT,
			CTE2.THU_MULT,
			CTE2.FRI_MULT,
			CTE3.ALLOC_CAL_NAME,
			CTE3.SAT,
			CTE3.SUN,
			CTE3.MON,
			CTE3.TUE,
			CTE3.WED,
			CTE3.THU,
			CTE3.FRI
		FROM CTE_FCST CTE1  
		JOIN CTE_CALENDAR CTE2 
			ON CTE1.WM_YR_WK = CTE2.WM_YR_WK
		JOIN CTE_ALLOC  CTE3 
			ON (CTE1.ITEM_NBR = CTE3.ITEM_NBR 
			AND CTE1.FORMATO = CTE3.FORMATO)
	)
	AS L
	LEFT JOIN 
	(
		SELECT 
			CALENDAR_DAY.WM_YR_WK, 
			COUNT(CALENDAR_DAY.WM_YR_WK) AS DIAS,
			SUM(CALENDAR_DAY.SAT_MULT) AS SAT_MULT,
			SUM(CALENDAR_DAY.SUN_MULT) AS SUN_MULT,
			SUM(CALENDAR_DAY.MON_MULT) AS MON_MULT,
			SUM(CALENDAR_DAY.TUE_MULT) AS TUE_MULT,
			SUM(CALENDAR_DAY.WED_MULT) AS WED_MULT,
			SUM(CALENDAR_DAY.THU_MULT) AS THU_MULT,
			SUM(CALENDAR_DAY.FRI_MULT) AS FRI_MULT
		FROM CALENDAR_DAY
		WHERE CALENDAR_DAY.WM_YR_WK = 
			(
				SELECT calendar_day.WM_YR_WK 
				FROM calendar_day 
				WHERE calendar_day.GREGORIAN_DATE = DATE
			) 
			AND CALENDAR_DAY.WM_MONTH = 
			(
				SELECT calendar_day.WM_MONTH
				FROM calendar_day
				WHERE calendar_day.GREGORIAN_DATE = DATE
			)
			AND CALENDAR_DAY.GREGORIAN_DATE >= DATE
		GROUP BY 1
	) AS R
	ON L.WM_YR_WK = R.WM_YR_WK
),

-- Se calculan los campos FCST_BASE_AJD y FCST_PROM_AJD para los casos en los que ALLOC_CAL_NAME = NULL
FCST_B_P_U AS
(
	SELECT * FROM
	(
		SELECT
			DEPT_NBR,
			CAT_NBR, 
			ITEM_NBR,
			OLD_NBR,
			REPL_GROUP_NBR,
			UPC_NBR,
			ITEM1_DESC,
			TIPO_ART,
			COSTO,
			PRECIO,
			PRECIO_IVA,
			FORMATO,
			FCST_WM_YR_WK,
			WM_YR_WK,
			WHSE_ALIGN,
			FCST_BASE,
			FCST_PROM,
			DIAS,
			SAT_MULT, SUN_MULT, MON_MULT, TUE_MULT, WED_MULT, THU_MULT, FRI_MULT,
			ALLOC_CAL_NAME,
			SAT, SUN, MON, TUE, WED, THU, FRI,
			0 AS PCT_WK,
			FCST_BASE * (DIAS/7.0) AS FCST_BASE_AJD,
			FCST_PROM * (DIAS/7.0) AS FCST_PROM_AJD 
		FROM FCST_B_P
		WHERE ALLOC_CAL_NAME IS NULL
	) AS A
	UNION 
	(
		SELECT
			DEPT_NBR,
			CAT_NBR, 
			ITEM_NBR,
			OLD_NBR,
			REPL_GROUP_NBR,
			UPC_NBR,
			ITEM1_DESC,
			TIPO_ART,
			COSTO,
			PRECIO,
			PRECIO_IVA,
			FORMATO,
			FCST_WM_YR_WK,
			WM_YR_WK,
			WHSE_ALIGN,
			FCST_BASE,
			FCST_PROM,
			DIAS,
			SAT_MULT, SUN_MULT, MON_MULT, TUE_MULT, WED_MULT, THU_MULT, FRI_MULT,
			ALLOC_CAL_NAME,
			SAT, SUN, MON, TUE, WED, THU, FRI,
			((SAT_MULT * SAT + SUN_MULT * SUN + MON_MULT * MON + TUE_MULT * TUE + WED_MULT * WED + THU_MULT * THU + FRI_MULT * FRI) /100) AS PCT_WK,
			FCST_BASE * PCT_WK AS FCST_BASE_AJD,
			FCST_PROM * PCT_WK AS FCST_PROM_AJD 
		FROM FCST_B_P
		WHERE ALLOC_CAL_NAME IS NOT NULL
	)
),

-- Sumarizacion de la consulta anterior sumando todos los registros de FCST_BASE_AJD y FCST_PROM_AJD respectivamente 
FCST_B_P_SUM AS 
(	
	SELECT
		DEPT_NBR,
		CAT_NBR, 
		ITEM_NBR,
		OLD_NBR,
		REPL_GROUP_NBR,
		UPC_NBR,
		ITEM1_DESC,
		TIPO_ART,
		COSTO,
		PRECIO,
		PRECIO_IVA,
		FORMATO,
		CAST (SUM(FCST_BASE_AJD) AS DECIMAL(15,5)) AS  FCST_BASE,
		CAST (SUM(FCST_PROM_AJD) AS DECIMAL (15,5)) AS FCST_PROM
	FROM 
		FCST_B_P_U
	GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
),

-- Sumarizacion de la consulta anterior contando todos los registros de ITEM_NBR
FCST_B_P_S2 AS
(
	SELECT
		REPL_GROUP_NBR,
		FORMATO,
		COUNT(ITEM_NBR) AS COUNT_ITEM
	FROM 
	(
		(
			SELECT
				DEPT_NBR,
				CAT_NBR, 
				ITEM_NBR,
				OLD_NBR,
				REPL_GROUP_NBR,
				UPC_NBR,
				ITEM1_DESC,
				TIPO_ART,
				COSTO,
				PRECIO,
				PRECIO_IVA,
				FORMATO,
				FCST_WM_YR_WK,
				WM_YR_WK,
				WHSE_ALIGN,
				FCST_BASE,
				FCST_PROM,
				DIAS,
				SAT_MULT, SUN_MULT, MON_MULT, TUE_MULT, WED_MULT, THU_MULT, FRI_MULT,
				ALLOC_CAL_NAME,
				SAT, SUN, MON, TUE, WED, THU, FRI,
				0 AS PCT_WK,
				FCST_BASE * (DIAS/7.0) AS FCST_BASE_AJD,
				FCST_PROM * (DIAS/7.0) AS FCST_PROM_AJD 
			FROM FCST_B_P
			WHERE ALLOC_CAL_NAME IS NULL
		)
		UNION 
			(
				SELECT
					DEPT_NBR,
					CAT_NBR, 
					ITEM_NBR,
					OLD_NBR,
					REPL_GROUP_NBR,
					UPC_NBR,
					ITEM1_DESC,
					TIPO_ART,
					COSTO,
					PRECIO,
					PRECIO_IVA,
					FORMATO,
					FCST_WM_YR_WK,
					WM_YR_WK,
					WHSE_ALIGN,
					FCST_BASE,
					FCST_PROM,
					DIAS,
					SAT_MULT, SUN_MULT, MON_MULT, TUE_MULT, WED_MULT, THU_MULT, FRI_MULT,
					ALLOC_CAL_NAME,
					SAT, SUN, MON, TUE, WED, THU, FRI,
					((SAT_MULT * SAT + SUN_MULT * SUN + MON_MULT * MON + TUE_MULT * TUE + WED_MULT * WED + THU_MULT * THU + FRI_MULT * FRI) /100) AS PCT_WK,
					FCST_BASE * PCT_WK AS FCST_BASE_AJD,
					FCST_PROM * PCT_WK AS FCST_PROM_AJD 
				FROM FCST_B_P
				WHERE ALLOC_CAL_NAME IS NOT NULL
			)
	)  AS T
	GROUP BY 1, 2
),

-- JOIN de las consultas (FCST_B_P_S2) y (DDM_PF_TF)
FCST_DDM_PF_J AS
(
	SELECT
		L.REPL_GROUP_NBR,
		L.NEGOCIO,
		R.COUNT_ITEM,
		CASE WHEN R.COUNT_ITEM IS NOT NULL THEN L.OVRD_PRESENTATION_QTY/R.COUNT_ITEM ELSE L.OVRD_PRESENTATION_QTY END AS PRESS_QTY,
		CASE WHEN R.COUNT_ITEM IS NOT NULL THEN L.OVRD_DISPLAY_QTY/R.COUNT_ITEM ELSE L.OVRD_DISPLAY_QTY END AS DISPLAY_QTY
	FROM DDM_PF_TF AS L
	LEFT JOIN FCST_B_P_S2 AS R
		ON L.REPL_GROUP_NBR = R.REPL_GROUP_NBR
		AND L.NEGOCIO = R.FORMATO
)

SELECT
	L.DEPT_NBR,
	L.CAT_NBR,
	L.ITEM_NBR,
	L.OLD_NBR,
	L.REPL_GROUP_NBR,
	L.UPC_NBR,
	L.ITEM1_DESC,
	L.TIPO_ART,
	L.COSTO,
	L.PRECIO,
	L.PRECIO_IVA,
	L.FORMATO,
	L.FCST_BASE,
	L.FCST_PROM,
	R.COUNT_ITEM,
	R.PRESS_QTY,
	R.DISPLAY_QTY
FROM
	FCST_B_P_SUM AS L
	LEFT JOIN
	FCST_DDM_PF_J R
	ON 
	L.REPL_GROUP_NBR = R.REPL_GROUP_NBR
	AND
	L.FORMATO = R.NEGOCIO


	
