#' @title Fetches data about all Câmara's órgãos
#' @description Fetches a dataframe containing information about all Câmara's órgãos
#' @return A dataframe
#' @export
fetch_orgaos_camara <- function() {
  .camara_api(.ORGAOS_FILE_CAMARA_PATH) %>%
    .assert_dataframe_completo(.COLNAMES_ORGAOS) %>%
    .coerce_types(.COLNAMES_ORGAOS)
}

#' @title Fetches Câmara órgão
#' @description Fetches a dataframe containing information about the órgão requested
#' @param sigla Órgão's id
#' @return Returns a dataframe containing information about the órgão requested
#' @rdname fetch_orgao_camara
#' @export
fetch_orgao_camara <- function(sigla = NULL) {
    parametros <- as.list(environment(), all = TRUE)
    .camara_api(.ORGAOS_CAMARA_PATH, parametros) %>%
        .assert_dataframe_completo(.COLNAMES_ORGAO) %>%
        .coerce_types(.COLNAMES_ORGAO)
}

#' @title Retorna a composição da comissão da camara
#' @description Retorna um dataframe contendo os membros da comissão
#' @param sigla_comissao Sigla da comissão da Camara
#' @return dataframe
#' @examples 
#' fetch_composicao_comissoes_camara('cmads')
fetch_composicao_comissoes_camara <- function(sigla_comissao) {
  orgaos_camara <- 
    fetch_orgaos_camara() %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::filter(trimws(sigla) == toupper(sigla_comissao)) %>%
    dplyr::select(orgao_id) %>% head(1)
  
  if (nrow(orgaos_camara) == 0) {
    warning("Comissão não encontrada")
    n <- tibble::frame_data(~ cargo, ~ id, ~ partido, ~ uf, ~ situacao, ~ nome, ~ sigla, ~ casa)
    return(n)
  }
  
  url <- paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterMembrosOrgao?IDOrgao=', orgaos_camara[[1]])
  
  eventos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()
  
  df <-
    eventos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    magrittr::extract2('membros') %>%
    tibble::as.tibble() %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("VALUE")
  
  if (nrow(df) == 0) {
    return(tibble::frame_data(~ cargo, ~ id, ~ nome, ~ partido, ~ uf, ~ situacao))
  }
  
  new_names <- c('cargo', 'id', 'nome', 'partido', 'uf', 'situacao')
  
  names(df) <- new_names
  df %>% 
    rowwise() %>% 
    dplyr::mutate(partido = ifelse(length(partido) == 0, "", partido)) %>% 
    dplyr::mutate(uf = ifelse(length(uf) == 0, "", uf)) %>% 
    dplyr::mutate(id = ifelse(length(id) == 0, "", id)) %>% 
    tidyr::unnest() %>%
    dplyr::arrange(nome)
}

#' @title Baixa os órgãos na câmara
#' @description Retorna um dataframe contendo os órgãos da câmara
#' @return Dataframe contendo os órgãos da Câmara
#' @importFrom RCurl getURL
fetch_orgaos_camara <- function(){
  url <- RCurl::getURL('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterOrgaos')
  
  orgaos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()
  
  df <-
    orgaos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    tibble::as.tibble() %>%
    t() %>%
    as.data.frame()
  
  names(df) <- c("orgao_id", "tipo_orgao_id", "sigla", "descricao")
  
  return(df)
}
