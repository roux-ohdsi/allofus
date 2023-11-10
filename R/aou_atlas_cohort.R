
#' Retrieve a cohort from ATLAS for use in AllofUs
#'
#' @param cohort_id The ID of the cohort to retrieve
#' @param base_url The URL of the ATLAS instance to use ending in /WebAPI. Defaults to demo atlas at https://atlas-demo.ohdsi.org
#'
#' @return a dataframe or list with the resulting cohort. see include_query
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  con <- allofus::aou_connect()
#'  cohort <- aou_atlas_cohort(1788061, baseUrl = "https://atlas-demo.ohdsi.org/WebAPI")
#' }
#'
aou_atlas_cohort <-function(cohort_id, base_url='http://api.ohdsi.org/WebAPI'){

  message("Querying ATLAS...generating a cohort can take a few minutes.")
  # Credit to https://github.com/cmayer2/r4aou with a few tweaks

  #billing=Sys.getenv('GOOGLE_PROJECT')
  cdmDatabaseSchema=Sys.getenv('WORKSPACE_CDR')

  #grabs sql from cohort_id

  version <- ROhdsiWebApi:::getWebApiVersion(baseUrl = base_url)
  d=ROhdsiWebApi:::getDefinitionsMetadata(base_url,category = 'cohort')
  aa=ROhdsiWebApi:::getCohortDefinition(cohort_id,base_url)
  sql=ROhdsiWebApi:::getCohortSql(aa,base_url)

  #prep the temp table (compensate for lack of result schema here    - ^ step
  #cohort sensor table, target_cohort_table (always call
  #@target_database_schema
  #@target_cohort_table

  #create pseudoresult table targetCohortTable
  TargetCohortTable  = cohort_id

  #modify slighly the SQL
  sql2= gsub("@cdm_database_schema", "@cdmDatabaseSchema", sql)
  #Remove result schema since not present
  sql2= gsub("@results_database_schema.", "", sql)
  #declare vocabulary schema as same as database schema
  sql2= gsub("@vocabulary_database_schema", "@cdm_database_schema", sql2)
  sql2= gsub("@target_database_schema.", "", sql2)
  sql2=gsub("delete from cohort_censor_stats where cohort_definition_id = @target_cohort_id;","",sql2)
  sql2=gsub("@cdm_database_schema.observation_period","#observation_period2",sql2)

  # Add parentheses at end of table creation
  #generate output since no result schema exist
  sql2= paste(sql2, "
        select * from #target_cohort_table;")
  #create final table with select statment
  sql2=paste("CREATE temp TABLE #target_cohort_table (
  cohort_definition_id INT64 not null, subject_id INT64 not null, cohort_start_date DATE, cohort_end_date DATE)
;", sql2)

  # generic obs period table from before

  #   sql2= paste( "
  # CREATE temp TABLE #observation_period2 as (select
  #   person_id, cast('1900-01-01' as DATE) as observation_period_start_date , cast('2100-01-01' as DATE) as observation_period_end_date
  # from @cdm_database_schema.person);
  # ",sql2)

  # Addition to at least account for the first and last visit + 60 days for each person
  # so that the cohort can be somewhat limited by observation period
  # This doesn't exactly following EHR obs period guidelines yet b/c there may be laspses
  # of > 548 days.

  # Then I tried this to make sure we at least donn't go past the EHR cutoff date but its not working...
  # used glue() instead of paste() but not sure if something is off...
  #     ehr_cutoff = aou_sql('SELECT * FROM {CDR}._cdr_metadata') %>% pull(ehr_cutoff_date)
  #
  #     CREATE temp TABLE #observation_period2 as (
  #         SELECT
  #           person_id,
  #           MIN(visit_start_date) AS observation_period_start_date,
  #           MAX(
  #             CASE
  #               WHEN DATE_ADD(visit_end_date, INTERVAL 60 DAY) > DATE({ehr_cutoff}) THEN DATE({ehr_cutoff})
  #               ELSE DATE_ADD(visit_end_date, INTERVAL 60 DAY)
  #             END
  #           ) AS observation_period_end_date
  #         FROM
  #           @cdm_database_schema.visit_occurrence
  #         GROUP BY
  #           person_id);

  sql2= paste( "
CREATE temp TABLE #observation_period2 as (
        SELECT
          person_id,
          MIN(visit_start_date) AS observation_period_start_date,
          DATE_ADD(MAX(visit_end_date), INTERVAL 60 DAY) AS observation_period_end_date
        FROM
          @cdm_database_schema.visit_occurrence
        GROUP BY
          person_id);
",sql2)



  sql3rendered <- SqlRender::render(sql2,cdm_database_schema=cdmDatabaseSchema,target_cohort_id=cohort_id, target_cohort_table= '#target_cohort_table')

  #switching to not a calling aou_run at all here
  #step TWO - translating
  sql4translated <- SqlRender::translate(sql3rendered,targetDialect = 'bigquery')

  sql5= gsub("create table", "CREATE TEMP TABLE", sql4translated)
  sql5= gsub("CREATE TABLE", "CREATE TEMP TABLE", sql5)
  sql5=gsub("and e.end_date >= i.start_date","",sql5)

  sql5=stringr::str_replace_all(sql5,tolower(cdmDatabaseSchema),cdmDatabaseSchema)
  #run and export results

  r = aou_sql(sql5)


  r = r %>% rename(person_id = subject_id)

  return(r)

}
