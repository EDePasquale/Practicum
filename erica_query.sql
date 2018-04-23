
--
-- step 0
--
SET NOCOUNT ON
go

USE [StudentPracticum]
GO

SELECT [RANDOM_PAT_ID]
      ,[RANDOM_PAT_ENC_CSN_ID]
  FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
  where dx_type like '%FEV%' and code = 'FEV1/FVC' and value <> ''
union
SELECT [RANDOM_PAT_ID]
      ,[RANDOM_PAT_ENC_CSN_ID]
  FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
  where code like '%490%' or code like '%491%' or code like '%492%' or code like '%493%' or code like '%494%' or code like '%495%' or code like '%496%' or 
  code like 'J45%' or code like 'J44%'
  go 

--
-- step 0
--

SET NOCOUNT ON
go

USE [StudentPracticum]
GO

SELECT *
  FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
  where dx_type like '%FEV%' and code = 'FEV1/FVC' and value <> ''
union
SELECT *
  FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
  where code like '%490%' or code like '%491%' or code like '%492%' or code like '%493%' or code like '%494%' or code like '%495%' or code like '%496%' or 
  code like 'J45%' or code like 'J44%'
  go 

--
-- step 1
--

SET NOCOUNT ON
go

USE [StudentPracticum]
GO

SELECT a.RANDOM_PAT_ID
      ,a.RANDOM_PAT_ENC_CSN_ID
      ,[AGE]
      ,[START_DATE]
      ,[NAME]
      ,[CODE]
      ,[DX_TYPE]
  FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_All_Diagnosis] a
  inner join ( SELECT distinct [RANDOM_PAT_ID]
               FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
               where dx_type like '%FEV%' and code = 'FEV1/FVC' and value <> ''
               union
               SELECT distinct [RANDOM_PAT_ID]
               FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
               where code like '%490%' or code like '%491%' or code like '%492%' or code like '%493%' or code like '%494%' or code like '%495%' or code like '%496%' or 
               code like 'J45%' or code like 'J44%' ) t  on 
				t.RANDOM_PAT_ID = a.RANDOM_PAT_ID
GO

--
-- step 2
--

SET NOCOUNT ON
go

USE [StudentPracticum]
GO

SELECT a.RANDOM_PAT_ID
      ,a.RANDOM_PAT_ENC_CSN_ID
      ,[CONTACT_DATE]
      ,[AGE]
      ,[HEIGHT]
      ,[WEIGHT]
      ,[DISP_ENC_TYPE]
      ,[IP_EPISODE_ID]
      ,[ED_EPISODE_ID]
      ,[ADMIT_CONF_STAT]
      ,[APPT_STATUS]
      ,[ED_DISPOSITION]
      ,[DISCH_DISPOSITION]
      ,[HOSPITAL_AREA]
      ,[PRIMARY_LOC]
      ,[DEPARTMENT_NAME]
  FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_PAT_ENC] a
  inner join ( SELECT distinct [RANDOM_PAT_ID]
               FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
               where dx_type like '%FEV%' and code = 'FEV1/FVC' and value <> ''
               union
               SELECT distinct [RANDOM_PAT_ID]
               FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
               where code like '%490%' or code like '%491%' or code like '%492%' or code like '%493%' or code like '%494%' or code like '%495%' or code like '%496%' or 
               code like 'J45%' or code like 'J44%' ) t  on 
				t.RANDOM_PAT_ID = a.RANDOM_PAT_ID
GO

--
-- step 3
--

SET NOCOUNT ON
go

USE [StudentPracticum]
GO

SELECT a.RANDOM_PAT_ID
      ,a.RANDOM_PAT_ENC_CSN_ID
      ,RANDOM_ORDER_PROC_ID
      ,[AGE]
      ,[PROC_NAME]
      ,[PROC_ID]
      ,[PROC_CODE]
      ,[ORDERING_DATE]
      ,[RESULT_TIME]
      ,[ORDER_DESCRIPTION]
      ,[RESULT_LINE_NUMBER]
      ,[COMPONENT_ID]
      ,[COMPONENT_NAME]
      ,[COMPONENT_ABBREV]
      ,[RESULT_VALUE]
      ,[LAB_STATUS]
      ,[RESULT_STATUS]
  FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Order_Results] a
   inner join ( SELECT [RANDOM_PAT_ID]
               FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
               where dx_type like '%FEV%' and code = 'FEV1/FVC' and value <> ''
               union
               SELECT [RANDOM_PAT_ID]
               FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
               where code like '%490%' or code like '%491%' or code like '%492%' or code like '%493%' or code like '%494%' or code like '%495%' or code like '%496%' or 
               code like 'J45%' or code like 'J44%' ) t  on 
				t.RANDOM_PAT_ID = a.RANDOM_PAT_ID
GO


--
-- step 4
--

SET NOCOUNT ON
go

USE [StudentPracticum]
GO

SELECT a.RANDOM_PAT_ID
      ,a.RANDOM_PAT_ENC_CSN_ID
      ,[AGE]
      ,[TOBACCO_USER]
      ,[TOBACCO_PAK_PER_DY]
      ,[TOBACCO_USED_YEARS]
      ,[SMOKING_QUIT_DATE]
      ,[CIGARETTES_YN]
      ,[PIPES_YN]
      ,[CIGARS_YN]
      ,[SNUFF_YN]
      ,[CHEW_YN]
      ,[SMOKELESS_TOB_USE]
      ,[SMOKING_TOB_USE]
      ,[SMOKELESS_QUIT_DATE]
  FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Social] a
   inner join ( SELECT [RANDOM_PAT_ID]
               FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
               where dx_type like '%FEV%' and code = 'FEV1/FVC' and value <> ''
               union
               SELECT [RANDOM_PAT_ID]
               FROM [dbo].[Asthma_Exacerbation_Study_For_Student_Practicum_Diagnosis]
               where code like '%490%' or code like '%491%' or code like '%492%' or code like '%493%' or code like '%494%' or code like '%495%' or code like '%496%' or 
               code like 'J45%' or code like 'J44%' ) t  on 
				t.RANDOM_PAT_ID = a.RANDOM_PAT_ID
GO
