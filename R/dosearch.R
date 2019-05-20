# Wrapper function to call different variations of the search
# Renamed from 'get_derivation'

dosearch <- function(
    data, query, graph, 
    transportability = NULL, selection_bias = NULL, missing_data = NULL,
    control = list()) {

    if ( !is.character(data) ) stop("Invalid data.")
    if ( !is.character(query) ) stop("Invalid query.")
    if ( !is.character(graph) ) stop("Invalid graph.")

    if ( !is.null(transportability) || !is.null(selection_bias) || !is.null(missing_data) ) {
        return(get_derivation_dag(match.call()))
    }
    if ( grepl(":", graph[1]) ) {
        return(get_derivation_ldag(match.call()))
    }
    return(get_derivation_dag(match.call()))
}

get_derivation <- dosearch