#' @title Fetches data about all Câmara's órgãos
#' @description Fetches a dataframe containing information about all Câmara's órgãos
#' @return A dataframe
#' @export
fetch_orgaos_camara <- function() {
  .camara_api(.ORGAOS_FILE_CAMARA_PATH) %>%
    dplyr::mutate(idOrgao = stringr::str_extract(uri, "([0-9]+)$")) %>%
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

#' @title Fetchs the commission composition of the camera
#' @description Returns a dataframe containing the comission's members
#' @param sigla_comissao Acronym for commission Camara
#' @return dataframe
#' @examples 
#' fetch_composicao_comissoes_camara('cmads')
fetch_composicao_comissoes_camara <- function(sigla_comissao) {
  orgaos_camara <- 
    fetch_orgaos_camara() %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::filter(trimws(sigla) == toupper(sigla_comissao)) %>%
    dplyr::select(idOrgao) %>% head(1)
  
  df <- stats::setNames(
      data.frame(
        matrix(ncol = length(.COLNAMES_COMPOSICAO_COMISSOES_CAMARA), nrow = 0)),
      names(.COLNAMES_COMPOSICAO_COMISSOES_CAMARA)
    )
  if (nrow(orgaos_camara) == 0) {
    warning("Comissao nao encontrada")
  } else {
    url <- paste0(.CAMARA_WEBSITE_LINK, .COMPOSICAO_COMISSOES_CAMARA_PATH, orgaos_camara[[1]])
    
    membros <-
      .get_from_url(url)%>%
      .get_xml()%>%
      jsonlite::toJSON() %>%
      jsonlite::fromJSON() %>%
      magrittr::extract2('membros') %>%
      tibble::as.tibble() %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("VALUE")
    
    if (nrow(membros) != 0) {
      df <- membros
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
  }
  
  df %>% .assert_dataframe_completo(.COLNAMES_ORGAOS) %>%
  .coerce_types(.COLNAMES_ORGAOS)
  
  df
}
